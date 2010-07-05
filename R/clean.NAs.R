clean.NAs <-
function(anydf)
  {
  for(c in 1:ncol(anydf))
         {
         if(class(anydf[,c])=="character")
             anydf[,c][is.na(anydf[,c])]<-""
         if(class(anydf[,c])=="numeric")
             anydf[,c][is.na(anydf[,c])]<-0
         }
   return(anydf=anydf)   
  }  

