get.uni.sts.info.map<-function(webget,StartLine,stsid)
   {
   MappingError<-"None"
   MappingDF<-"No MappingDF available" 
   StartMapping<-grep("<!--- Mapping data --->",webget)
   StartMapping<-StartMapping[StartMapping>StartLine][1]
   if(substr(webget[StartMapping+1],1,5)=="<!---")    
      {
      MappingError<-paste("unists ",stsid,":No mapping info available for this id number",sep="")
      } else {
      MappingDF<-get.uni.sts.info.map.wrap(webget,StartMapping)
      }
     return(list(MappingError=MappingError,MappingDF=MappingDF))
   }
