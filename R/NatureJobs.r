NatureJobs <- function(keywords,outputfileprefix="Jobs",savedescriptions=TRUE,days=0,BadJobs="",WriteDirectory="NJ",celllimit=30000,hyper="HYPERLINK",xldiv=";",quiet=FALSE,showurl=FALSE,LocalLink=TRUE)
   {
   #not flexible about structure.
   #split into sub functions perhaps, and wrap things in a try command
   BJFlag<-FALSE
   if(!file.exists(BadJobs) & BadJobs!="")
      print("The file you specified for the unwanted jobs was not found")
   if(file.exists(BadJobs) & BadJobs!="") 
      {
      print("The file you specified for the unwanted jobs was located and will be used to clean jobs")
      BJFlag<-TRUE
      }
   savedescriptions<-TRUE
   keywords<-RemoveSpaces(keywords)
   keywords<-paste(keywords, collapse="+")   
   keywords<-gsub(" ","\\+",keywords)
   nextpageaddress <- paste("http://www.nature.com/naturejobs/science/jobs/search?[is_advanced]=0&query=",keywords, sep = "")
   if(showurl==TRUE)
      print(paste("Now querying",nextpageaddress))
   webget <- get.file(nextpageaddress)
   if(length(grep("No jobs found which matched your search for",webget))==1)
      stop("No jobs were found for this query")
   PageLine<-webget[grep("which matched your search for",webget)]
     if(length(grep(" of ",PageLine))==1)
       TotalJobs<-as.numeric(strsplitdbl(PageLine,"of <b>","</b> which matched"))
     if(length(grep(" of ",PageLine))!=1)
       TotalJobs<-as.numeric(strsplitdbl(PageLine,"<p>Displaying <b>","</b> job[s]? which matched"))         
   TotalPages<-ceiling(TotalJobs/25)
   AddThis<-"http://www.nature.com"
   writeLines(paste(TotalPages, " pages of jobs identified. Now saving brief descriptions",sep=""))
   pb <- txtProgressBar(min = 0, max = TotalPages, style = 3)
   for(ThisPageLoop in 1:TotalPages)
      {
      setTxtProgressBar(pb,ThisPageLoop)
      if(ThisPageLoop!=1)
          webget <- get.file(nextpageaddress)
      ThisPageJobs<-as.data.frame(cbind( Title=rep("",length(grep("<span>Employer:</span>",webget))),employer="",location="",posted="",description="",description.xlweb=""),stringsAsFactors=FALSE)
      ThisPageJobs$employer<-strsplitdbl(webget[grep("<span>Employer:</span>",webget)],"</span> ","</p>")
      ThisPageJobs$location<-strsplitdbl(webget[grep("<span>Location:</span>",webget)],"</span> ","</p>")
      ThisPageJobs$posted<-strsplitdbl(webget[grep("<span>Posted:</span>",webget)],"</span> ","</p>")
      format(Sys.time,"YYYY")
      ThisPageJobs$posted<-gsub("&nbsp;"," ",ThisPageJobs$posted)
      ThisPageJobs$posted<-gsub("Just posted",format(Sys.time(),"%B %d, %Y"),ThisPageJobs$posted)
      DescriptionFull<-do.call(cbind,strsplit(webget[grep("<p class=\"job-desc\">",webget)],"job-desc\">"))[2,]
      JD_start<-grep("<p class=\"job-desc\">",webget)
      JD_end<-grep("Read more",webget)

      for(clean in 1:length(JD_start))
         {
         webget[JD_start[clean]]<-paste(webget[JD_start[clean]:JD_end[clean]],collapse=" ")
         webget[(JD_start[clean]:JD_end[clean])[JD_start[clean]:JD_end[clean]!=JD_start[clean]]]<-""
         }
       ThisPageJobs$Title<-do.call(cbind,strsplit(webget[grep("<h3><a href=",webget)],"\">"))[2,]
       ThisPageJobs$Title<-do.call(cbind,strsplit(ThisPageJobs$Title,"</a>"))[1,]

       DescriptionFull<-do.call(cbind,strsplit(webget[grep("<p class=\"job-desc\">",webget)],"job-desc\">"))[2,]
       ThisPageJobs$description<-do.call(cbind,strsplit(webget[grep("<a href=",DescriptionFull)],"job-desc\">"))[1,]
       ThisPageJobs$description<-do.call(cbind,strsplit(DescriptionFull[grep("<a href=\"",DescriptionFull)],"<a href=\""))[1,]
       ThisPageJobs$description<-gsub(" &#8211; ","_",ThisPageJobs$description)
       ThisPageJobs$description.xlweb<-do.call(cbind,strsplit(DescriptionFull[grep("<a href=\"",DescriptionFull)],"<a href=\""))[2,]
       ThisPageJobs$description.xlweb<-paste(AddThis,gsub("\">Read more</a></p>","",ThisPageJobs$description.xlweb),sep="")

       if(ThisPageLoop==1)
          AllJobs<-ThisPageJobs
       else
          AllJobs<-as.data.frame(rbind(AllJobs,ThisPageJobs),stringsAsFactors=FALSE)

       if(ThisPageLoop!=TotalPages)
          nextpageaddress<-paste(AddThis,strsplitdbl(webget[grep(paste("page=",ThisPageLoop+1,sep=""),webget)][1],"<a href=\"",paste("page=",ThisPageLoop+1,sep=""),include="right"),sep="")
       flush.console()

      }
   close(pb)
#   print("stage B")
   AllJobs$Title<-gsub(",",";",AllJobs$Title)  
   AllJobs$location<-gsub(",",";",AllJobs$location)
   AllJobs$employer<-gsub(",",";",AllJobs$employer)
   AllJobs$description<-gsub(",",";",AllJobs$description)
   AllJobs$DaysAgo<-floor(as.numeric(Sys.time()-strptime(AllJobs$posted, format="%b %d, %Y")))
   AllJobs$posted<-as.character(AllJobs$posted)
   if(days!=0){
      oldjobcount<-nrow(AllJobs)
      AllJobs<-AllJobs[AllJobs$DaysAgo<=as.numeric(days),]
      print(paste("NatureJobs Filter: days(",days,") Old Job Count:",oldjobcount," New Job Count:",nrow(AllJobs),sep=""))
      }       
#   print("stage C")
   if(nrow(AllJobs)==0)
      {
      stop("No jobs left with after applying the days filter. Try the same search without the filter, or increase the number of days")
      } else {
      print("stage C1a")
      if(BJFlag==TRUE)
         {
         print("stage C1")
         BadJobTitles <- get.file(BadJobs, showurl = showurl, clean = FALSE)
         print("NatureJobs Filter: Jobs that will be cut - perhaps you should look through manually")
         print(unique(AllJobs[AllJobs$Title %in% BadJobTitles,"Title"]))
         print("----")
         oldcount<-nrow(AllJobs)
         AllJobs<-AllJobs[!(AllJobs$Title %in% BadJobTitles),]
         print(paste("Old Job Count:",oldcount,"  New Job Count:",nrow(AllJobs)))
         }
   #   print("stage C2a")
      if (savedescriptions == TRUE)
         {
         print("stage C2")
         print(paste("Will save descriptions inside folder:",WriteDirectory))
         pb <- txtProgressBar(min = 0, max = nrow(AllJobs), style = 3)
         for (i in 1:nrow(AllJobs))
            {
            setTxtProgressBar(pb, i)
            writeLines(paste("Trying to process AllJobs",i,"of",nrow(AllJobs)))
            webget <- get.file(AllJobs$description.xlweb[i])
            a <- try(make.local.HTML(AllJobs$description.xlweb[i], WriteDirectory,30000))
            if(class(a)!="try-error")  #but work out WHY
               {
               AllJobs$BigDescription[i] <- unlist(strsplit(a[[2]],"txt-ed-job-desc\">"))[2]
               AllJobs$ExpDate[i] <- paste("'", as.character(strptime(a[[1]],format = "%B %d, %Y")))
               }
            if(class(a)=="try-error")
              {
              print("NCBI2R ERROR. NatureJobs. unable to parse HTML from this job")
              print(AllJobs$description.xlweb[i])  #should do something more here about informing
              stop("Program has stopped") #what if it returns a value instead of an error?
              }
               
            }
         close(pb)
         }
    #  print("stage C3")
      AllJobs$BigDescription<-RemoveSpaces(AllJobs$BigDescription)
      AllJobs$BigDescription<-gsub("\t","<p>",AllJobs$BigDescription)
      print("stage C4")
      if("outputfileprefix" %in% ls())
         {
         if(LocalLink==TRUE & savedescriptions==TRUE)
            {
     #       print("stage C5")
            olddirectory<-getwd()
            setwd(WriteDirectory) #is it worth changing directory each time - why not just make the full description.
            thisdir<-getwd()
            setwd(olddirectory)
            AllJobs$LocalLink<-ConvertURLToExcel(paste(thisdir,"/",substr(AllJobs$description.xlweb,47,nchar(AllJobs$description.xlweb)),".html",sep=""),xldiv=xldiv,hyper=hyper)
            }
   #     print("stage C6")
        AllJobs<-AllJobs[!duplicated(AllJobs$LocalLink),]
        AllJobs$description.xlweb<-ConvertURLToExcel(AllJobs$description.xlweb,xldiv=xldiv,hyper=hyper)
        filename<-paste(outputfileprefix,format(Sys.time(),"%d_%H%M%S"),".tab",sep="")
        write.table(AllJobs,file=filename,quote=FALSE,row.names=FALSE,sep="\t")
        print(paste("saved the output as",filename)) 
        }
      print("Warning - sometimes the expiry dates in the description is different to the heading - that's because employers often don't fill in the form properly")
      return(AllJobs)
      }
   }

#keywords<-c("scientist","genetic") #this is one search, composed of two keywords
#nj<-NatureJobs(keywords,"MYFILE_A_",days=81,xldiv=",")  #should describe that xldiv is better? is it clear. i.e. not CSV