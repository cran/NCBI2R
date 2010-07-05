get.file <-
function(getURL,showurl=FALSE,sep="\n",quiet=TRUE,clean=FALSE)
   {            
   if(showurl){print(getURL)}
   if(exists("NCBI2R.TimeStampA")==TRUE)
     DelayInc(NCBI2R.TimeStampA,0.333334) 
   webget<-suppressWarnings(TryScan(getURL,sep = sep,quiet=quiet))
   NCBI2R.TimeStampA<<-Sys.time()
   if(clean)
      webget<-clean.xml(webget)
   return(webget)
   }  

