tryScan <-
function(getURL,sep="\n",quiet=TRUE,retry=20,error=TRUE)
   {
   thispage<-try(scan(getURL, what="character", sep=sep, quiet=quiet),silent=TRUE)
   retrycount<-0
   while(class(thispage)=="try-error" & retrycount<retry) {
      Delay(0.5) 
      retrycount<-retrycount+1
      thispage<-try(scan(getURL, what="character", sep=sep, quiet=quiet),silent=TRUE)
      }
   if(class(thispage)=="try-error" & error==TRUE) 
      stop("NCBI2R error: The website was not found.")
   if(class(thispage)=="try-error" & error==FALSE) 
      thispage<-"Page not found"
   return(thispage)
   }
   