add.column <-
function(anydf,newword)
   { 
   dummycol<-rep("",nrow(anydf))
   anydf<-data.frame(anydf,dummycol,stringsAsFactors=FALSE)
   colnames(anydf)[ncol(anydf)]<-newword
   return(anydf)
   } 

