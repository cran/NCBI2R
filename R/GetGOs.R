GetGOs <-
function(locusID,showurl=FALSE)  
   {
   if(length(locusID)>1)
      stop("NCBI2R GetGOs error: Only one locus at a time for this function in this release of software.")
   URLdef<-ncbi2r.options()
   getURL<-paste(URLdef$front,"efetch.fcgi?db=gene&id=",locusID,"&rettype=XML",URLdef$back,sep="")
   webget<-get.file(getURL,showurl,clean=TRUE)
   V<-get.go.int(webget)  
   GO<-V$GO
   return(GO) 
   }
