GetUniSTSFromName <-
function(searchterm,taxid=9606,showurl=FALSE)
   {
   a<-GetIDs(searchterm,db="unists")
   if(length(a)==0)
     {
     return(paste("The search term",searchterm,"was not found in the UniSTS database",sep=" "))
     } 
  if(length(a)>1)  
     {
     print("more than one ID was returned. You'll have to run GetUniSTSInfo individually on each ID")
     return(a)
     }
  if(length(a)==1)
     {
     b<-GetUniSTSInfo(a,taxid=taxid,showurl=showurl)
     return(b) 
     }         
  }
