TryScan <-
function(getURL,sep="\n",quiet=TRUE)
   {
   thispage<-try(scan(getURL, what="character", sep=sep, quiet=quiet),silent=TRUE)
   retrycount<-0
   while(class(thispage)=="try-error" & retrycount<20) {
      Delay(0.5) 
      retrycount<-retrycount+1
      thispage<-try(scan(getURL, what="character", sep=sep, quiet=quiet),silent=TRUE)
      }
   if(class(thispage)=="try-error") 
      stop("NCBI2R error: The website was not found.")
   return(thispage)
   }
   
