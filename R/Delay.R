Delay <-
function(seconds)  
   {
   NCBI2R.TimeStampA <<- Sys.time()
   while(Sys.time()<NCBI2R.TimeStampA+seconds)
      OnlyForDelay<-1  
   }

