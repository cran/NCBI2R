GetPhenotypes <-
function(idnumber,showurl=FALSE)
   {
   URLdef<-URLdefinitions()
   getURL<-paste(URLdef$front,"efetch.fcgi?db=gene&id=",idnumber,"&rettype=XML",URLdef$back,sep="")
   webget<-get.file(getURL,showurl=showurl,clean=TRUE)
   webget<-gsub("&amp;apos;s","",webget)
   V<-get.phenotypes.int(webget) 
   phenotypes<-V$Phenotypes
   return(phenotypes)
   } 
