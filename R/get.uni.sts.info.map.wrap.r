get.uni.sts.info.map.wrap<-function(webget,StartMapping)
   {
   chunk<-GetTable(webget,StartMapping)
   chunk<-parse.chunk(chunk)
   MappingDF<-get.uni.sts.create.mappingdf.from.chunk(chunk)
   sup_maps<-c("MARC","deCODE","Marshfield","Whitehead-YAC","RH", "Stanford-G3", "Whitehead-RH", "GeneMap99-GB4","Genethon","NCBI RH","TNG","GeneMap99-G3")
   MappingDF[MappingDF$map %in% sup_maps,"PosLow"]<-do.call(rbind,strsplit(MappingDF[MappingDF$map %in% sup_maps,"Position"]," \\("))[,1]
   MappingDF[MappingDF$map %in% sup_maps,"PosHigh"]<-do.call(rbind,strsplit(MappingDF[MappingDF$map %in% sup_maps,"Position"]," \\("))[,1]
   MappingDF[MappingDF$map %in% sup_maps,"Position"]<-"processed"

  if(length(grep("\\|",MappingDF$chr_source[MappingDF$map=="Sequence"],invert=TRUE))>0)
     MappingDF$chr_source[MappingDF$map=="Sequence" & grep("\\|",MappingDF$chr_source,invert=TRUE)]<-paste(MappingDF$chr_source[MappingDF$map=="Sequence" & grep("\\|",MappingDF$chr_source,invert=TRUE)],"|NCBI")
  
   tmp<-grep("\\|",MappingDF$chr_source)
   nontmp<-grep("\\|",MappingDF$chr_source,invert=TRUE)
  MappingDF$chr_source[tmp]<-gsub("^([[:print:]]*\\|[[:print:]]*)\\|([[:print:]]*)$","\\1_\\2",MappingDF$chr_source[tmp])
   if(length(tmp)>0)
     {
      MappingDF[tmp,"source"]<-do.call(rbind,strsplit(MappingDF$chr_source[tmp],"\\|"))[,2]
      MappingDF$chr[tmp]<-do.call(rbind,strsplit(MappingDF$chr_source[tmp],"\\|"))[,1]
     }
   MappingDF$chr[nontmp]<-MappingDF$chr_source[nontmp]
   MappingDF$chr[grep(" ",MappingDF$chr)]<-do.call(rbind,strsplit(MappingDF$chr[grep(" ",MappingDF$chr)]," "))[,2]
   MappingDF$chr_source<-NULL;   MappingDF$Position<-NULL 
   MappingDF$units<-gsub("\\(|\\)","",MappingDF$units) 
   if(!is.na(pmatch("source",colnames(MappingDF))))
      {
      MappingDF$source<-gsub("^[[:blank:]]*([[:print:]]*)[[:blank:]]*$","\\1",MappingDF$source)
      MappingDF$map[!is.na(MappingDF$source)]<-paste(MappingDF$source[!is.na(MappingDF$source)],"-",MappingDF$map[!is.na(MappingDF$source)],sep="")
      MappingDF$source<-NULL
      } 
   colnames(MappingDF)<-gsub(" ","_",colnames(MappingDF))
   return(MappingDF)
   }
   