GetPubMed <-function(searchterm,file="",download=FALSE,showurl=FALSE,xldiv=";",hyper="HYPERLINK",MaxRet=30000,sme=FALSE,smt=FALSE,quiet=TRUE,batchsize=500)
   {
   URLdef<-URLdefinitions()
   searchterm<-gsub(" ","+",searchterm)
   getURL<-paste(URLdef$front,"esearch.fcgi?db=pubmed&term=",searchterm,"&retmax=",MaxRet,"&rettype=FASTA",URLdef$back,sep="")
   webget<-get.file(getURL,showurl=showurl,clean=FALSE)  
   ListItems<-GetListFromXML(webget,sme=sme,smt=smt)
   url_piece<-""
   
   print(paste("Number of papers found in PubMed was:",length(ListItems)))
   flush.console()
   Counter<-0

   NumberOfBatchesInTotal<-ceiling(length(ListItems)/batchsize)
   print(paste("Batches to run is",NumberOfBatchesInTotal))
   remainingPMIDs<-ListItems

 for(batch in 1:NumberOfBatchesInTotal)
   {
   thisbatchPMIDs<-remainingPMIDs[1:batchsize]                       
   thisbatchPMIDs<-thisbatchPMIDs[!(is.na(thisbatchPMIDs))]
   remainingPMIDs<-remainingPMIDs[!(remainingPMIDs %in% thisbatchPMIDs)]
   remainingPMIDs<-remainingPMIDs[!(is.na(remainingPMIDs))]
   url_piece<-paste(thisbatchPMIDs,collapse="&id=")

   print(paste("Now processing batch",batch,"of",NumberOfBatchesInTotal))
   papers<-data.frame(PMID=rep("0",length(thisbatchPMIDs)),TI="",AB="",stringsAsFactors=FALSE)
   
   getURL<-paste(URLdef$front,"efetch.fcgi?db=pubmed&id=",url_piece,"&report=medline",URLdef$back,sep="") 
   webget<-get.file(getURL,showurl,clean=FALSE)
   webget<-webget[1:(length(webget)-1)]
   webget<-gsub("&gt;",">",webget)
   papercounter<-0
   LC<-3 
   while(LC<=length(webget))
      {
      if(substr(webget[LC],5,5)=="-")    {
         medline_heading<-RemoveSpaces(substr(webget[LC],1,4))
         if(medline_heading=="PMID")
            papercounter<-papercounter+1
         if(length(which(medline_heading==names(papers)))==0)  
            papers<-add.column(papers,medline_heading)
         thisentry<-substr(webget[LC],7,nchar(webget[LC]))   
      } else {
         thisentry<-paste(thisentry,substr(webget[LC],7,nchar(webget[LC])),sep=" ") 
         if(quiet==FALSE & medline_heading=="TI"){print(paste("[TI]",thisentry))}
      }
      if(substr(webget[LC+1],5,5)=="-" | LC==length(webget))  {
          prev_cell_entry<-papers[papercounter,which(medline_heading==names(papers))] 
         if(prev_cell_entry=="" | prev_cell_entry==0) {  papers[papercounter,which(medline_heading==names(papers))]<-thisentry      
            } else {
            papers[papercounter,which(medline_heading==names(papers))]<-paste(prev_cell_entry,", ",thisentry,sep="") 
            }
         }
      LC<-LC+1
      }

 if(!exists("AllPapers"))
   { AllPapers<-papers
   } else {
    AllPapers<-as.data.frame(rbind.with.diffnames(AllPapers,papers),stringsAsFactors=FALSE)
   } 
   }
  papers<-AllPapers

   FindThesePapers<-papers[substr(papers$PMC,1,3)=="PMC",]
   if(download==TRUE)  
      {
      print(paste("Number of papers that appear to be obtainable through PubMedCentral (PMC) are:",nrow(FindThesePapers)))
      print("This number does not include those under embargo")
      papers$localcopy<-""
      directoryname<-gsub("\\+","_",searchterm)
      print(paste("Saving PDFs to the following directory:",directoryname))  
      set.create.dir(directoryname,setdir=TRUE)
      pdfurl<-paste("http://www.pubmedcentral.nih.gov/picrender.fcgi?artid=",FindThesePapers$PMC,"&blobtype=pdf",sep="")
      pb<-txtProgressBar(min=0,max=nrow(FindThesePapers),style=3)
      for(i in 1:nrow(FindThesePapers))
         { 
         setTxtProgressBar(pb,i)               
        ThisTitle<-gsub("[^A-za-z0-9 \\(\\)\\-]","",substr(FindThesePapers$TI[i],1,70)) 
        if(Excel.FIND(",",FindThesePapers$AU[i])!=(-1))
          pdffilefront<-clean(paste(splitfirst(FindThesePapers$AU[i],",")[1]," (",substr(FindThesePapers$DP[i],1,4),") ",ThisTitle,"_",FindThesePapers$TA[i],sep=""))
        if(Excel.FIND(",",FindThesePapers$AU[i])==(-1))
          pdffilefront<-clean(paste(FindThesePapers$AU[i]," (",substr(FindThesePapers$DP[i],1,4),") ",ThisTitle,"_",FindThesePapers$TA[i],sep=""))
        pdffilename<-paste(pdffilefront,".pdf",sep="")
        pdffilename<-gsub("\\.\\.","\\.",pdffilename)
        pdffilename<-gsub(" \\.pdf","\\.pdf",pdffilename)
        if(!file.exists(pdffilename))
           a<-try(download.file(pdfurl[i],pdffilename,mode="wb",quiet=TRUE))
        FindThesePapers$localcopy[i]<-paste(getwd(),"/",pdffilename,sep="")
            }
      setwd("..") 
      close(pb)
      } 
         
   TheOtherOnes<-papers[substr(papers$PMC,1,3)!="PMC",]   
   TheOtherOnes$localcopy<-" " 
   papers<-as.data.frame(rbind.with.diffnames(FindThesePapers,TheOtherOnes),stringsAsFactors=FALSE)   
   papers$link<-paste("http://www.ncbi.nlm.nih.gov/pubmed/",papers$PMID,sep="")
   
   if(file!="") 
      MakeExcel(papers,file,xldiv=xldiv,hyper=hyper)
   return(papers=papers)
   } 

