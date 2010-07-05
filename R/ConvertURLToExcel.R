ConvertURLToExcel <-
function(link,linktext="Link",xldiv=";",hyper="HYPERLINK") 
   {
   link<-paste("=",hyper,"(\"",link,"\"",xldiv,"\"",linktext,"\")",sep="")
   return(link)
   }  

