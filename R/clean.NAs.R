clean.NAs <-
function(dataobj)
   {
   if(class(dataobj)=="data.frame")
      {
      for(c in 1:ncol(dataobj))
         {
         if(class(dataobj[,c])=="character")
             dataobj[,c][is.na(dataobj[,c])]<-""
         if(class(dataobj[,c])=="numeric")
             dataobj[,c][is.na(dataobj[,c])]<-0
         }
      }
   if(class(dataobj)=="character")
     dataobj[is.na(dataobj)]<-""
   if(class(dataobj)=="numeric")
     dataobj[is.na(dataobj)]<-0
  return(dataobj=dataobj)   
  }  

