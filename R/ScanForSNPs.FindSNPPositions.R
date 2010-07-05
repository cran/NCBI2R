ScanForSNPs.FindSNPPositions <-
function(snplist,AssociationResults,markercolumn="name",chromosomecolumn="chr",positioncolumn="position")
   {   
   print("Finding locations of SNPs")
   RequestedSNPInfo<-GetSNPInfo(snplist)
   Results<-data.frame(snplist,RequestedSNPInfo$chr,RequestedSNPInfo$chrpos,distance=0,matchedSNP="",stringsAsFactors=FALSE)
   colnames(Results)<-c("ReqSNP","chr","chrpos","distance","matchedSNP")
   for(i in 1:nrow(RequestedSNPInfo)) 
      {
      if(Results$ReqSNP[i] %in% AssociationResults[,markercolumn]) {

         for(j in 1:ncol(AssociationResults))  
            Results[i,j+4]<-AssociationResults[AssociationResults[,markercolumn]==Results[i,1],j]
        } else {
          print(paste("ScanForSNPs - without position - no exact match found for",Results$ReqSNP[i]))
         lowboundary<-(Results$chrpos[i]-500000)
         highboundary<-(Results$chrpos[i]+500000)
         RegionalSNPs<-GetRegion("snp",Results$chr[i],lowboundary,highboundary)
         RegionalSNPs<-RegionalSNPs[(RegionalSNPs %in% AssociationResults[,markercolumn])]
         RegionalSNPInfo<-GetSNPInfo(RegionalSNPs) 
         RegionalSNPInfo$distance<-abs(as.numeric(RegionalSNPInfo$chrpos-RequestedSNPInfo$chrpos[i]))
         closestmarker<-RegionalSNPInfo[RegionalSNPInfo$distance==min(RegionalSNPInfo$distance),]
         if(nrow(closestmarker)>0)
            closestmarker<-closestmarker[1,] 
         Results$distance[i]<-closestmarker$distance
         ourmatch<-AssociationResults[AssociationResults[,markercolumn]==closestmarker$marker,]
        for(k in 1:13)  
           Results[i,k+4]<-ourmatch[1,k] 
           }
      }
      colnames(Results)[6:(5-1+ncol(AssociationResults))]<-colnames(AssociationResults)[2:ncol(AssociationResults)] 
   return(Results)
   } 

