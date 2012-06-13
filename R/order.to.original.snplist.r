

order.to.original.snplist<-function(listofSNPs,df1,keycol="marker")
   {
   req.snps<-as.data.frame(cbind(mylist=listofSNPs,osdf=1:length(listofSNPs)),stringsAsFactors=FALSE)
   df2<-merge(df1,req.snps,by.x=keycol,by.y="mylist",all=TRUE)
   df2<-df2[order(df2$osdf),]
   df2$osdf<-NULL
   row.names(df2)<-1:nrow(df2)
   return(df2)
   }
   
