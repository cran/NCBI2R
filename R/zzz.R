.onLoad <- function(lib, pkg)
   {
   options(timeout = 180)
   NCBI2R.TimeStampA <<- Sys.time()-3
   PrintNCBI2RInfo()
   }


.onUnload <- function(lib)
   {
   options(timeout = NULL)
   }
