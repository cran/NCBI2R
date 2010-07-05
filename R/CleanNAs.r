CleanNAs<-function(datatoclean)
   {
   if(class(datatoclean)=="character")
     datatoclean[is.na(datatoclean)]<-""
   if(class(datatoclean)=="numeric")
     datatoclean[is.na(datatoclean)]<-0
  return(datatoclean)
   }
