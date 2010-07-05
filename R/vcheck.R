vcheck <-
function(InThisText,ThisHTMLtag,between1=">",between2="<")
   {
   if(substr(InThisText,1,nchar(ThisHTMLtag))==ThisHTMLtag)
      {
      startp<-1 
      while(substr(InThisText,startp,(startp+nchar(between1)-1))!=between1)
         startp<-startp+1
      stopp<-startp
      while(substr(InThisText,stopp,(stopp+nchar(between2)-1))!=between2)
         stopp<-stopp+1
      result<-substr(InThisText,startp+1,stopp-1)
      }
   }

