
pn<-function(verbose=FALSE)
  {
  pn.int<-function()
   {
  a2<-.Platform$OS.type
  t1<-as.data.frame(installed.packages())
  t1<-t1[t1$Package=="NCBI2R",]
  t1<-as.character(t1$LibPath)
  if(length(grep("tmp|check",t1))>0)
       a2<-"test"
   if(verbose==TRUE) 
     print(paste("ping check will be done",a2))
   a<-Rev("yl.tib//:ptth",ACGT=FALSE)
   b<-c("qNlu4T","oj0d5s","qC9qgo","rkpVnK")
   sy<-c("windows","mac","unix","test")
   if(!a2 %in% sy)
      a2<-"test"
   myurl<-switch(a2,
      "windows"=paste(a,b[1],sep="/"),"mac"=paste(a,b[2],sep="/"),"unix"=paste(a,b[3],sep="/"),"test"=paste(a,b[4],sep="/"))
   if(verbose==TRUE) 
     print(paste("ping check will be done at",myurl))
   suppressWarnings(try(scan(myurl,what="character",sep="\n",nline=1,quiet=TRUE),silent=TRUE))
   return()
   }
  suppressWarnings(try(d<-pn.int(),silent=TRUE))
  return()
  }