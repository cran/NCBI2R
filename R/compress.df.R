compress.df <-
function(anydf,colname,refcol="genesymbol")
   {
   if(missing(anydf))
      stop("NCBI2R error: no data frame provided")
   if(class(anydf)!="data.frame")
      stop("NCBI2R error: no data frame provided")
   if(colname %in% names(anydf))
      {
      for(i in nrow(anydf):2)
         {
         if((anydf[i,colname]==anydf[i-1,colname]) & (anydf[i-1,colname]!="") & (anydf[i,refcol]==anydf[i-1,refcol]) & (anydf[i,colname]!=""))  
            {
            anydf[i,colname]<-"as above" 
            }
         }
      }   
   return(anydf=anydf)
   }

