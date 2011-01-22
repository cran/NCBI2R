.onLoad <- function(lib, pkg)
   {
   options(timeout = 180)
   .ncbi2r.options<<-ncbi2r.options.default()
   }


.onUnload <- function(lib)
   {
   options(timeout = NULL)
   }
