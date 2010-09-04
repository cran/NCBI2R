
ConvertPubMedHeadings<-function(paper.df,reverse=FALSE)
   {
   med<-MedlineAbbreviations()
   if(reverse==FALSE)
     {
     for(i in 1:ncol(paper.df))
      {
      if(names(paper.df)[i] %in% med$abrv)
        names(paper.df)[i]<-med[med$abrv==names(paper.df)[i],"desc"]
      }
     }
   if(reverse==TRUE)
     {
     for(i in 1:ncol(paper.df))
      {
      if(names(paper.df)[i] %in% med$desc)
        names(paper.df)[i]<-med[med$desc==names(paper.df)[i],"abrv"]
      }
     }
   return(paper.df)
   }
