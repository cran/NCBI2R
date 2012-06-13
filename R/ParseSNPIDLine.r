parseSNPIDLine<-function(textvector)
   {
   queryID<-do.call(rbind,strsplit(textvector,":"))[,1]
   rsID<-strsplitdbl(textvector,": "," \\[")  
   species<-strsplitdbl(textvector,"\\[","\\]")
   u<-as.data.frame(cbind(queryID,rsID,species),stringsAsFactors=FALSE)
   u$queryID<-as.numeric(u$queryID)
   return(u)
   }
   
parseSNPIDLine.v2<-function(textvector)
   {
   tmp<-unlist(strsplit(textvector,"\\|"))
   tmp<-tmp[grep("=",tmp)]
   hd<-gsub("([[:print:]]*[[:blank:]])*([[:print:]]*)=(\")*([[:print:]]*)+(\")*","\\2",tmp)
   dt<-gsub("[[:print:]]*=([[:print:]]*)","\\1",tmp)
   dt<-gsub("\"","",dt)
   return(as.data.frame(cbind(hd,dt),stringsAsFactors=FALSE))
   }

