GetTableWithComments<-function(myfile,columnnames,sep=" ",filter="")
   {
   errorcount<-0
   mytext <- get.file(myfile, showurl = FALSE, clean = FALSE)
   .ncbi2r.options$TimeStampA<<-Sys.time()
   while(mytext[1]=="<!DOCTYPE html")
       {
       errorcount<-errorcount+1
       retrying<-"problem"
       cat(paste("\r NCBI2R: retrying hapmap. RetryCount:",errorcount))
       flush.console()
       mytext <- get.file(myfile, showurl = FALSE, clean = FALSE)
       }
   if(exists("retrying"))
      cat(paste("\r NCBI2R: retrying hapmap. RetryCount:",errorcount,"....CHECKED: OK."))
   cat("\n")   
   my_table_text<-mytext[grep("#",mytext,invert=TRUE)] 
   if(length(my_table_text)==0)
      return("No information available")
   else   
     {
     colnum<-length(unlist(strsplit(my_table_text[1],sep)))
     newdf<-data.frame(rbind(1:colnum))
     if(filter!="")
         {
         my_table_text<-my_table_text[grep(filter,my_table_text)]
         if(length(my_table_text)==0)
            stop("The filter term, ",filter,", was not found inside the downloaded LD information. Perhaps there is no LD information for the specified SNP")
         }
     newdf<-as.data.frame(1:length(my_table_text))    
     for(i in 1:colnum)
         newdf[,i]<-do.call(cbind,strsplit(my_table_text,sep))[i,]
     colnames(newdf)<-columnnames
     return(newdf)
     }
   }