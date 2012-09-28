AnnotateSNPList <-
function(snplist,filename="",hyper="HYPERLINK",xldiv=";",smt=FALSE,sme=FALSE,div="---",keeplocusIDs=FALSE,keepNS=FALSE,FlankingDistance=100000,kp=TRUE,quiet=TRUE,neigh=TRUE,showurl=FALSE)
   {
   a<-as.data.frame(snplist,stringsAsFactors=FALSE)
   names(a)<-"marker"
   x<-AnnotateDataframe(a,selections="marker",filename=filename,hyper=hyper,xldiv=xldiv,smt=smt,sme=sme,div=div,keeplocusIDs=keeplocusIDs,keepNS=keepNS,FlankingDistance=FlankingDistance,kp=kp,quiet=quiet,neigh=neigh,showurl=showurl,suppressColCheck=TRUE)
   return(x)
   }

