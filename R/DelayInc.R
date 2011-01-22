delayInc <-function(timestamp,seconds)
   {
   if(.ncbi2r.options$vS=="No update check yet")
     t1<-updateCheckWrapper(cache=TRUE,display=TRUE)
   while(Sys.time()<(timestamp+seconds))
     OnlyForDelay<-1    
   }

