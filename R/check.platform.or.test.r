
check.platform.or.test<-function()
  {
  a2<-.Platform$OS.type
  t1<-as.data.frame(installed.packages())
  t1<-t1[t1$Package=="NCBI2R",]
  t1<-as.character(t1$LibPath)
  if(length(grep("tmp|check",t1))>0)
       a2<-"test"
  return(a2)
  }
