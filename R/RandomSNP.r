RandomSNP <-
function(number,org="")
   {
   snplist<-""
   for(i in 1:number)
      {
      thisvalue<-paste("rs",as.character(floor(runif(1)*1000000)),sep="")
      snplist<-c(snplist,thisvalue)
      }
   snplist<-snplist[2:(number+1)]
   return(snplist)
   }
