get.file <-
function(getURL,showurl=FALSE,sep="\n",quiet=TRUE,clean=FALSE,verbose=FALSE)
   {  
   if(showurl){writeLines(getURL)}
   if(exists(".ncbi2r.options")==TRUE)
     {
     if("TimeStampA" %in% names(.ncbi2r.options))
       {
       delayInc(.ncbi2r.options$TimeStampA,.ncbi2r.options$dS)
       }
     } else {
     ncbi2r.options()
     }
   webget<-suppressWarnings(tryScan(getURL,sep = sep,quiet=quiet,verbose=verbose))
   .ncbi2r.options$TimeStampA<<-Sys.time()
   if(clean)
      webget<-clean.xml(webget)
   return(webget)
   }  

