GetSNPCurrent <-
function(multipleIDs,batchsize=200,showurl=FALSE)
   {      
   URLdef<-ncbi2r.options()
   CountOfThisBatch<-0
   url_piece<-""
   BatchOffset<-0 
   while((CountOfThisBatch<batchsize) & ((CountOfThisBatch+BatchOffset)<length(multipleIDs)))
      {
      CountOfThisBatch<-CountOfThisBatch+1
      url_piece<-paste(url_piece,"&id=",multipleIDs[(CountOfThisBatch+BatchOffset)],sep="")
      }
   getURL<-paste(URLdef$front,"efetch.fcgi?db=snp",url_piece,"&rettype=sgml",URLdef$back,sep="")
   webget<-get.file(getURL,showurl,clean=FALSE)
   Counter<-0;     LC<-1
   SNPthings<-data.frame(rep("",10),rep("a",10),stringsAsFactors=FALSE)
   colnames(SNPthings)<-c("rsId","leftover")  
   Counter<-Counter+1
   LC<-skimUntil("<Rs rsId=&quot;",webget,LC)
   while(!is.na(webget[LC]))
      {
      int<-substring(webget[LC],16,nchar(webget[LC]))
      SNPthings$rsId[Counter]<-splitfirst(int,"&quot;")[1]
      LC<-LC+1
      LC<-skimUntil("<Rs rsId=&quot",webget,LC)
      }
   if(length(unique(SNPthings$rsId[1:Counter]))!=1)
      stop("NCBI2R error: Not only one unique value (may be zero or more than one)")
   return(unique(SNPthings$rsId[SNPthings$rsId!=""]))
   }

