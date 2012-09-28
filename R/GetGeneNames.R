GetGeneNames <- function(locusID,showurl=FALSE)
   {
   batchsize<-50
   BatchCount<-0
   URLdef<-ncbi2r.options()
   remainingObjects<-unique(locusID)
   while(length(remainingObjects)>0)
      {
      BatchCount<-BatchCount+1
      this.batch<-min(batchsize,length(remainingObjects))
      these.objects<-remainingObjects[1:this.batch]
      url_piece<-paste("&id=",paste(these.objects,collapse=","),sep="")
      remainingObjects<-remainingObjects[!(remainingObjects %in% remainingObjects[1:this.batch])]
      getURL<-paste(URLdef$front,"efetch.fcgi?db=gene",url_piece,"&rettype=gene_Table&retmode=text",URLdef$back,sep="")
      webget<-get.file(getURL,showurl=showurl,clean=FALSE)
      a<-grep("^\t",webget)[1]-1
      b<-grep("^Reference ",webget)[1]
      b2<-grep("^Alternate ",webget)[1]
      ab<-c(a,b,b2)
      ab<-ab[!is.na(ab)]

      if(length(ab)==0)
        {
        refLines<-NA
        } else {
        refLines<-min(ab)
        }
      if(is.na(refLines))
         refLines<-grep("There is no table for this gene",webget)[1]


      if(is.na(refLines))
        {
        errorHandler("GGN-001")
        writeLines("GetGeneNames cannot parse records-no refLines found")
        stop("NCBI2R",call.=FALSE)
        }
      if((length(webget)==1 & length(refLines)==1))
        {
        writeLines("Warn: Appears to be a bad parse on the NCBI website. Will try partial extraction.")
        writeLines("Warn: Extraction will not contain CurrentRecord and LastUpdate information for this batch")
        writeLines("Warn: If extraction fails, try breaking query into smaller groups and trying again.")
        a5t<-get.gene.docset(these.objects)
        minigenedf<-a5t[,c("genename","genesymbol","NewlocusID","locusID","species")]
        minigenedf$CurrentRecord<-"";  minigenedf$LastUpdate<-""
        minigenedf<-minigenedf[,c("genename","genesymbol","NewlocusID","CurrentRecord","LastUpdate","locusID","species")]
        }  else {

        if(length(refLines)==0)
          {
          mt.df<-as.data.frame(cbind(genename="",genesymbol="",NewlocusID="",CurrentRecord="",LastUpdate="",locusID="",species=""),stringsAsFactors=FALSE)
          return(mt.df[0,])
          }
        if(refLines[1]==2)
          refLines<-3
        top<-try(webget[(1:refLines[1]-1)])
        if(class(top)=="try-error")
           {
           writeLines("Unable to parse the top of the file")
           writeLines(paste("Length of webget:",length(webget)))
           if(length(webget)<10)
             print(webget)
           writeLines(paste("Length of refLines:",length(refLines)))
           if(length(refLines)<10)
             print(refLines)
           stop("NCBI2R Error GGN-009")
           }
        top<-gsub("This record was discontinued.","",top)
         if(length(grep("not found",top[2]))==1)
          {
          writeLines("NCBI record query error")
          writeLines("Try excluding the following locusID from the query and repeating")
          writeLines(gsub("GeneId ([[:digit:]]*) not found.","\\1",top[2]))
          writeLines("Will try to salvage query.")
          minigenedf<-get.gene.docset(these.objects)
          minigenedf<-GetGeneNames(minigenedf$locusID[minigenedf$genename!=""])
          } else {
          minigenedf<-extract.genenames.from.genetabletxt(top)
          }
      }
      if(BatchCount==1)
        {
        tgf<-minigenedf
        } else {
        tgf<-as.data.frame(rbind(tgf,minigenedf),stringsAsFactors=FALSE)
        }
      }

   tgf<-try(order.to.original.list(enteredlist=locusID,df1=tgf,keycol="locusID"))
   if(class(tgf)=="try-error")
        {
        errorHandler("GGN-002")
        writeLines("GetGeneNames worked but data could not be sorted")
        stop("NCBI2R",call.=FALSE)
      }
   return(tgf)
   }
