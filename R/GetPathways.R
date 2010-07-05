GetPathways <-
function(idnumber,showurl=FALSE)  
   {  
   URLdef<-URLdefinitions()
   getURL<-paste(URLdef$front,"efetch.fcgi?db=gene&id=",idnumber,"&rettype=XML",URLdef$back,sep="")
   webget<-get.file(getURL,showurl,clean=TRUE)
   V<-get.pathways.int(webget)  
   pathways<-V$Pathways
   return(pathways) 
   }
