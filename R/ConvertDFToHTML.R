ConvertDFToHTML <-
function(anydf,div="---")
   {
   if(substr(div,nchar(div),nchar(div))!=" ")
      div<-paste(div," ",sep="")
   twt_names<-""
   A.h<-""
   for(i in 1:nrow(anydf))
     {
     twt_names<-paste(twt_names,div,anydf[i,1],sep="")
     A.h<-paste(A.h,div,"<a href=\"",anydf[i,2],"\">",anydf[i,1],"</a>",sep="")
     }
   names<-substr(twt_names,nchar(div)+1,nchar(twt_names))
   A.h<-substr(A.h,nchar(div)+1,nchar(A.h))
   return(list(names.html=A.h,names=names))
   }

