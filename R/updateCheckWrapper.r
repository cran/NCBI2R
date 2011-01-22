updateCheckWrapper<-function(cache=FALSE,display=FALSE)
  {
  ncbi2r.options()
  if(cache==FALSE | (cache==TRUE & .ncbi2r.options$vS=="No update check yet"))
     {
     t1<-updateCheck.int()
     } else {
     t1<-ncbi2r.options()$vS
     }
 if(display)
   writeLines(t1)
 return(t1)
 }