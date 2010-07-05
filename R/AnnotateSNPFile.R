AnnotateSNPFile <-
function(snpfile,filename="output.html",hyper="HYPERLINK",xldiv=",",quiet=TRUE,smt=FALSE,sme=FALSE,keeplocusIDs=FALSE,keepNS=FALSE,kp=TRUE,div="---",neigh=TRUE,showurl=FALSE)  
   {
   snplist<-scan(snpfile, what="character", sep="\n")
   if(substr(snplist[1],1,2)!="rs")
      {
      print("first item in snplist was not an rs identifier so it was removed. Assumed to be a header. Will continue")
      snplist<-snplist[2:length(snplist)]
      }
   newsnps<-AnnotateSNPList(snplist,filename,hyper=hyper,xldiv=xldiv,quiet=quiet,smt=smt,sme=sme,div="---",keeplocusIDs=keeplocusIDs,keepNS=keepNS,showurl=showurl,neigh=neigh,kp=kp) 
   return(newsnps)  
   }

