swap.rows <-
function(anydf,rowApos,rowBpos)
   {
   temp<-anydf[rowApos,]
   anydf[rowApos,]<-anydf[rowBpos,]
   anydf[rowBpos,]<-temp
   return(anydf)
   }

