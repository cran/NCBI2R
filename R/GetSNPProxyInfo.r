GetSNPProxyInfo<-function(singlesnpname,include="",exclude="",FlankingDistance=100000,showurl=FALSE,method="r2",keepmode=1)
   {
   if(method!="r2" & method!="Dprime" & method!="LOD")
     stop("NCBI2R GetSNPProxyInfo Error: This method of choosing a best marker not found. Needs to be one of LOD or r2 or Dprime")
   if(FlankingDistance>1000000)
     stop("NCBI2R GetSNPProxyInfo Error: The maximum flanking distance available is 1 million nucleotides")  
   pos.req.SNP<-GetSNPPosHapmap(singlesnpname,showurl=showurl)
   lowpos<-pos.req.SNP$chrpos-FlankingDistance
   if(lowpos<0)
      lowpos<-0
   highpos<-pos.req.SNP$chrpos+FlankingDistance
   DataChunk<-GetLDInfo(pos.req.SNP$chr,lowpos,highpos,showurl=showurl,filter=singlesnpname)  
  
   if(include[1]!="")
      DataChunk<-DataChunk[(DataChunk$SNPA %in% include | DataChunk$SNPB %in% include),]
   if(exclude[1]!="")
      DataChunk<-DataChunk[!(DataChunk$SNPA %in% exclude) & !(DataChunk$SNPB %in% exclude),]
   if(keepmode==1)
      {
      BestValue<-max(DataChunk[pmatch(method,names(DataChunk))])
      BestMarkers<-DataChunk[DataChunk[pmatch(method,names(DataChunk))]==BestValue,]
      answer<-BestMarkers[BestMarkers$distance==min(BestMarkers$distance),]
      }
   if(keepmode==2)
      {
      BestValue<-max(DataChunk[pmatch(method,names(DataChunk))])
      answer<-DataChunk[DataChunk[pmatch(method,names(DataChunk))]==BestValue,]
      }
   if(keepmode==3)
      answer<-DataChunk
   return(answer)
   }
