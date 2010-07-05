GetGOs <-
function(locusID,showurl=FALSE)  
   {
   URLdef<-URLdefinitions()
   getURL<-paste(URLdef$front,"efetch.fcgi?db=gene&id=",locusID,"&rettype=XML",URLdef$back,sep="")
   webget<-get.file(getURL,showurl,clean=TRUE)
   V<-get.go.int(webget)  
   GO<-V$GO
   return(GO) 
   }
