order.to.original.list<-function(enteredlist,df1,keycol="locusID")
   {
   if(class(enteredlist)=="data.frame")
      {
      req.items<-as.data.frame(cbind(enteredlist,n2rinternalosdf=1:nrow(enteredlist)),stringsAsFactors=FALSE)
      df2<-merge(req.items,df1,by.x=keycol[1],by.y=keycol[2],all=TRUE)
      } else {
      req.items<-as.data.frame(cbind(mylist=enteredlist,n2rinternalosdf=1:length(enteredlist)),stringsAsFactors=FALSE)
      df2<-merge(df1,req.items,by.x=keycol,by.y="mylist",all=TRUE)
      }
   df2<-df2[order(as.numeric(df2$n2rinternalosdf)),]
   df2$n2rinternalosdf<-NULL
   row.names(df2)<-1:nrow(df2)
   return(df2)
   }
