move.column <-
function(anydf,colname,newposition,quiet=TRUE) 
   {
   if(missing(anydf))
     stop("No dataframe provided")
   if(class(anydf)!="data.frame")
      stop("You have not provided a data.frame")
   if(newposition<0||newposition>ncol(anydf))
      stop(paste("The new position of this column must be between 1 and the number of columns in this table (",ncol(anydf),")",sep=""))
   currentcolumn<-0
   for(i in 1:ncol(anydf))
     {   
     if(colnames(anydf)[i]==colname)
        currentcolumn<-i
     }   
   if(currentcolumn==0)
      stop("The column name you have provided could not be found in this dataframe")  
   if(currentcolumn==newposition & quiet==FALSE)
      print("This column is already in that position - move.column was not used") 
   if(currentcolumn>newposition) 
      {
      temp.column<-anydf[currentcolumn]
      temp.colname<-colnames(anydf)[currentcolumn]
      for(j in (currentcolumn-1):(newposition)) 
         {
         anydf[j+1]<-anydf[j]
         colnames(anydf)[j+1]<-colnames(anydf)[j]
         }
      anydf[newposition]<-temp.column
      colnames(anydf)[newposition]<-temp.colname
      }
   if(currentcolumn<newposition)
      {
      temp.column<-anydf[currentcolumn]
      temp.colname<-colnames(anydf)[currentcolumn]
      for(j in (currentcolumn):(newposition-1)) 
         {
         anydf[j]<-anydf[j+1]
         colnames(anydf)[j]<-colnames(anydf)[j+1]
         }
      anydf[newposition]<-temp.column
      colnames(anydf)[newposition]<-temp.colname
      }
   return(anydf=anydf)  
   }      