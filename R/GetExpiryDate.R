GetExpiryDate <-
function(sourcefile)
   {
   TD_Row<-1
   findthis<-"      <dt>Expires:</dt>"
   while(substr(sourcefile[TD_Row+1],1,nchar(findthis))!=findthis)
      {
      TD_Row<-TD_Row+1
      }
   ExpiryDateRow<-sourcefile[TD_Row+2]
   textrow<-gsub("&nbsp;"," ",ExpiryDateRow)
   startchar<-">"
   stopchar<-"<"
   currentpos<-1
   while(substr(textrow,currentpos,currentpos)!=startchar)
      {
      currentpos<-currentpos+1
      }
   thispoint<-currentpos
   while(substr(textrow,currentpos,currentpos)!=stopchar)
      {
      currentpos<-currentpos+1
      }
   return(substr(textrow,thispoint+1,currentpos-1))
   }  
