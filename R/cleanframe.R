cleanframe <-
function(anydf)
  {
   anydf<-anydf[anydf[,1]!="",]
   for(a in ncol(anydf):1) 
      if(length(unique(anydf[,a]))==1)
         if(unique(anydf[,a])=="")
            anydf[,a]<-NULL
  return(anydf)
  }
