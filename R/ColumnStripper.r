columnStripper <-
function(anydf,Text)
   {
    if(missing(anydf))
      stop("NCBI2R error: no data.frame provided")
   if(class(anydf)!="data.frame")
      stop("NCBI2R error: no data.frame provided")
   columns<-grep(Text,colnames(anydf))
   if(length(columns)!=0)
      {
      for(i in length(columns):1)
         anydf[columns[i]]<-NULL
      }
   return(anydf)   
   }
       
