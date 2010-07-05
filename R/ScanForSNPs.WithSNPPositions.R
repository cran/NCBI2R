ScanForSNPs.WithSNPPositions <-
function(snplist,AssociationResults,markercolumn="name",chromosomecolumn="chr",positioncolumn="position")
   {                                                        
   tempDF<-data.frame(rep(list(char = character(length(snplist))), each = length(AssociationResults)),stringsAsFactors=FALSE)
   for(i in 1:length(AssociationResults))
      colnames(tempDF)[i] <- colnames(AssociationResults)[i]
   myshortframe<-data.frame(ReqSNP=rep("",length(snplist)),ReqChr="",ReqPos=0,distance=0,stringsAsFactors=FALSE)
   RequestedResults<-cbind(myshortframe,tempDF)
   NumberOfColumnsMyStuff<-4  
   if(unique(substr(snplist,1,2))=="rs")         
      RequestedResults$ReqSNP<-snplist   
   print("now see if any of the SNPs are appear within my Association Results")
   for(r in 1:length(snplist))
      {                                                                         
      MatchedData<-data.frame(AssociationResults[(AssociationResults[,markercolumn])==RequestedResults$ReqSNP[r],],stringsAsFactors=FALSE)
      print(MatchedData)
      if(nrow(MatchedData)>0)
         {
         for(c in 1:ncol(AssociationResults))
            RequestedResults[r,c+NumberOfColumnsMyStuff]<-as.character((MatchedData[r,c]))
         RequestedResults$ReqChr[r]<-MatchedData[,chromosomecolumn]  
         RequestedResults$ReqPos[r]<-MatchedData[,positioncolumn] 
         }
      }
   print("All SNPs in *your* dataset have been identified. Now looking for closest matches")
   LookUpThese<-as.character(rep("",100000)) 
   counter<-0
   for(i in 1:length(snplist))
      {
      if(RequestedResults$ReqChr[i]=="" & RequestedResults$ReqPos[i]==0)
         {
         counter<-counter+1
         LookUpThese[counter]<-RequestedResults$ReqSNP[i]
         }
      }
   LookUpThese<-unique(LookUpThese[LookUpThese!=""]) 
   if(length(LookUpThese)!=0)
      {
      SNPinfo<-GetSNPInfo(LookUpThese)
      for(i in 1:nrow(RequestedResults))
         {
         if(RequestedResults$ReqChr[i]=="" & RequestedResults$ReqPos[i]==0)
            {
            countertwo<-1
            while(countertwo<=length(SNPinfo$chr))
               {
                if(SNPinfo$marker[countertwo]==RequestedResults$ReqSNP[i])   
                  {
                  RequestedResults$ReqChr[i]<-as.character(SNPinfo$chr[countertwo])
                  RequestedResults$ReqPos[i]<-as.numeric(SNPinfo$chrpos[countertwo])
                  }
               countertwo<-countertwo+1
               }
            }
         }
      }
   for(i in 1:nrow(RequestedResults))
      {
      print(paste("now checking",i,"of the",nrow(RequestedResults),"results that you want to compare to"))
      if(RequestedResults[i,NumberOfColumnsMyStuff+1]=="")
         {
         windowsize<-1000000 
         rangelow<-as.numeric(RequestedResults$ReqPos[i])-(windowsize/2)
         if(rangelow<0)
            rangelow<-0
         rangehigh<-as.numeric(RequestedResults$ReqPos[i])+(windowsize/2)
         Object<-(AssociationResults[AssociationResults[,"chromosomecolumn"]==RequestedResults$ReqChr[i] & AssociationResults$pos>rangelow & AssociationResults$pos<rangehigh,])
         Object$distance<-abs(as.numeric(Object$position-RequestedResults$ReqPos[i]))
         closestmarker<-Object[Object$distance==min(Object$distance),]
         if(nrow(closestmarker)>0)
            closestmarker<-closestmarker[1,] 
         RequestedResults$distance[i]<-closestmarker$distance
         RequestedResults[i,5:(5-1+ncol(closestmarker)-1)]<-(closestmarker[1:(ncol(closestmarker)-1)])
         }
      }
   return(RequestedResults)
   }

