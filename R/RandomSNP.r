randomSNP <-
function(number,org="",replace=FALSE)
   {
   snplist<-paste("rs",as.character(sample(1:1000000,number,replace=replace)),sep="")
   return(snplist)
   }
