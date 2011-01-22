getListFromXML <-
function(webget,smt=FALSE,sme=FALSE,MaxRet=30000)
   {
   webget<-gsub("\t","",webget)
   RowCounter<-1
   ListCounter<-0
   ListItems<-rep("",10000)
   while(substr(webget[RowCounter],nchar(webget[RowCounter])-7,nchar(webget[RowCounter]))!="<IdList>")
      RowCounter<-RowCounter+1
   RowCounter<-RowCounter+1
   while(substr(webget[RowCounter],1,9)!="</IdList>")
      {                                                                              
      ListCounter<-ListCounter+1
      ListItems[ListCounter]<-substr(webget[RowCounter],5,nchar(webget[RowCounter])-5)
      RowCounter<-RowCounter+1
      }
   if(smt==TRUE | sme==TRUE)
      showMessages(webget,smt=smt,sme=sme)
   if(length(ListItems[ListItems!=""])==MaxRet)
      print("Warning: Number of items was greater than expected. PARTIAL RESULTS USED [MaxRet need increasing]")
   ListItems<-ListItems[ListItems!=""]
   }  
                           