swap.columns <-
function(anydf,colA,colB)
   {
   for(i in 1:ncol(anydf))
     {
     if(colnames(anydf)[i]==colA)
        {colApos<-i}
     if(colnames(anydf)[i]==colB)
        {colBpos<-i}   
     }
   temp<-anydf[colA]
   anydf[colA]<-anydf[colB]
   anydf[colB]<-temp
   colnames(anydf)[colBpos]<-colA
   colnames(anydf)[colApos]<-colB
   return(anydf)
   }

