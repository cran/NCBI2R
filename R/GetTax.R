GetTax <-
function(org,showurl=FALSE,smt=FALSE,sme=FALSE)   
   {
   if(missing(org))
      stop("Please specify a taxonomy string to find, eg (human, homo sapiens, mouse)")
   org<-gsub(" ","+",org) 
   URLdef<-URLdefinitions()
   getURL<-paste(URLdef$front,"esearch.fcgi?db=taxonomy&term=",org,URLdef$back,sep="")
   webget<-get.file(getURL,showurl,clean=FALSE)
   taxid<-as.numeric(GetListFromXML(webget,sme=sme,smt=smt)) 
   return(taxid=taxid)
   }    
