change.class.data.frame <-
function(dframe)
   {
   for(i in 1:ncol(dframe))
      {
      if(class(dframe[,i])=="factor")
        {
        dframe[i]<-as.character(dframe[,i])
        }
      }
   return(dframe)
   }

