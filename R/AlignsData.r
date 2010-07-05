
AlignsData<-function(LineObserved,DataObserved,RecordStartLines,condense=TRUE)
   {
   ActualResults<-as.data.frame(cbind(LineObserved,DataObserved),stringsAsFactors=FALSE)
   ActualResults$LineObserved<-as.numeric(ActualResults$LineObserved)
   for(i in 1:nrow(ActualResults))
      ActualResults$Record[i]<-max(RecordStartLines[RecordStartLines<=ActualResults$LineObserved[i]])
   dummy<-as.data.frame(cbind(RecordStartLines))  
   ft<-merge(ActualResults,dummy,by.x="Record",by.y="RecordStartLines",all=TRUE)
   return(ft$DataObserved)
   }
