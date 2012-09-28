AnnotateSNPFile <-
function(snpfile,filename="output.html",hyper="HYPERLINK",xldiv=",",quiet=TRUE,smt=FALSE,sme=FALSE,keeplocusIDs=FALSE,keepNS=FALSE,FlankingDistance=100000,kp=TRUE,div="---",neigh=TRUE,showurl=FALSE)
   {     
   snplist<-get.file(snpfile, showurl = showurl, clean = FALSE)
   if(substr(snplist[1],1,2)!="rs")
      {
      if(substr(snplist[2],1,2)!="rs")
         stop("NCBI2R error: This was not a list of rs identifiers.")
      print("first item in snplist was not an rs identifier so it was removed. Assumed to be a header. Will continue")
      snplist<-snplist[2:length(snplist)]
      }
   newsnps<-AnnotateSNPList(snplist,filename,hyper=hyper,xldiv=xldiv,quiet=quiet,smt=smt,sme=sme,div=div,keeplocusIDs=keeplocusIDs,keepNS=keepNS,showurl=showurl,neigh=neigh,kp=kp,FlankingDistance=FlankingDistance)
   return(newsnps)  
   }

