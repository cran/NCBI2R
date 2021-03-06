GetTax <-
function(org,showurl=FALSE,smt=FALSE,sme=FALSE)   
   {
   if(missing(org))
      stop("Please specify a taxonomy string to find, eg (human, homo sapiens, mouse)")
   org<-gsub(" ","+",org) 
   URLdef<-ncbi2r.options()
   getURL<-paste(URLdef$front,"esearch.fcgi?db=taxonomy&term=",org,URLdef$back,sep="")
   webget<-get.file(getURL,showurl,clean=FALSE)
   taxid<-try(as.numeric(getListFromXML(webget,sme=sme,smt=smt)))
   if(class(taxid)=="try-error")
     stop("NCBI2R Error GT-001. GetTax function failed to identify organism.")
   return(taxid=taxid)
   }    
