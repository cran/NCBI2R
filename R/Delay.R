Delay <-
function(seconds)  
   {
   ncbi2r.options()
   .ncbi2r.options$TimeStampA <<- Sys.time()
   while(Sys.time()<.ncbi2r.options$TimeStampA+seconds)
      OnlyForDelay<-1  
   }

