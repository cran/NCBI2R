ConvertURLToHTML <-
function(link,linktext="Link",m=FALSE,div="**")
   {
   if(m)  
      {
      tz<-""
      for(i in 1:length(link))   
        tz<-paste(tz,div,"<a href=\"",link[i],"\">",linktext,"</a>",sep="")
      tz<-substr(tz,3,nchar(tz))
      return(tz)
      }  
   if(!m)
      { 
      link<-paste("<a href=\"",link,"\">",linktext,"</a>",sep="")     
      return(link)
      }
   } 

