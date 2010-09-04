
rbind.with.diffnames<-function(df1,df2,commonname="PMID")
   {
   if(nrow(df1)>0 & nrow(df2)>0)
      {
       for(i in 1:length(colnames(df1)))
          {
          if(!(names(df1)[i] %in% names(df2)))
             {
             if(class(df1[,i])=="character")
                df2[,names(df1)[i]]<-""
             if(class(df1[,i])=="numeric")
                df2[,names(df1)[i]]<-0
             }
          }

        for(i in 1:length(colnames(df2)))
          {
          if(!(names(df2)[i] %in% names(df1)))
             {
             if(class(df2[,i])=="character")
                df1[,names(df2)[i]]<-""
             if(class(df2[,i])=="numeric")
                df1[,names(df2)[i]]<-0
             }
          }
       df3<-as.data.frame(rbind(df1,df2),stringsAsFactors=FALSE)
       return(df3)
    } else {
    if(nrow(df1)==0)
       return(df2)
    if(nrow(df2)==0)
       return(df1)
    }
   }
