parseSNPIDLine<-function(textvector)
   {
   queryID<-do.call(rbind,strsplit(textvector,":"))[,1]
   rsID<-strsplitdbl(textvector,": "," \\[")  
   species<-strsplitdbl(textvector,"\\[","\\]")
   u<-as.data.frame(cbind(queryID,rsID,species),stringsAsFactors=FALSE)
   u$queryID<-as.numeric(u$queryID)
   return(u)
   }

