.onLoad <- function(lib, pkg)
   {
   options(timeout = 180)
   NCBI2R.TimeStampA <<- Sys.time()-3
   #buildStartupMessage(pkg="NCBI2R",msg="NCBI2R loaded. type PrintNCBI2RInfo() for more details")
   #UpdateCheck()
   }


.onUnload <- function(lib)
   {
   options(timeout = NULL)
   }
