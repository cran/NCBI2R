GetTableWithComments<-function(myfile,columnnames,sep=" ",filter="")
   {
   errorcount<-0
   mytext<-scan(myfile,sep="\n",what="character",quiet=TRUE)
   NCBI2R.TimeStampA<-Sys.time()
   while(mytext[1]=="<!DOCTYPE html")
       {
       errorcount<-errorcount+1
       print(paste("hapmap download gave unexpected file structure. NCBI2R will retry. Error:",errorcount))
       flush.console()
       mytext<-scan(myfile,sep="\n",what="character",quiet=TRUE)
       }
   mystuff<-mytext[grep("#",mytext,invert=TRUE)] 
  colnum<-length(unlist(strsplit(mystuff[1],sep)))
  newdf<-data.frame(rbind(1:colnum))

  if(filter!="")
     {
     mystuff<-mystuff[grep(filter,mystuff)]
     if(length(mystuff)==0)
        stop("The filter term, ",filter,", was not found inside the downloaded LD information. Perhaps there is no LD information for the specified SNP")
     }

  newdf<-as.data.frame(1:length(mystuff))    
  for(i in 1:colnum)
     newdf[,i]<-do.call(cbind,strsplit(mystuff,sep))[i,]
  colnames(newdf)<-columnnames
  return(newdf)
   }
