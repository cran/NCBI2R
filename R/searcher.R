searcher <-
function(string,findthis,before="http:",after="\"")
   {
   startp<-1 
   while(substr(string,startp,(startp+nchar(findthis)-1))!=findthis & startp<=nchar(string)) 
      startp<-startp+1
   if(startp==length(string))
     {
     print(string)
     stop("object not found on this line - so we should keep looping around (if there are more than one line in the object")
     }
   startpos<-0
   i<-startp
   FlagStop<-FALSE
   while(i>1 & FlagStop==FALSE)
      {
      if(substr(string,i,i+nchar(before)-1)==before)  
         {
         startpos<-i
         FlagStop<-TRUE
         }
      i<-i-1   
      } 
   if(startpos==0)
     stop("bug-object not foundA")
   FlagStop<-FALSE  
   i<-startp
   stoppos<-1 
   while(i<(nchar(string)-nchar(after)+1) & FlagStop==FALSE) 
      {
      if(substr(string,i,i+nchar(after)-1)==after)  
         {
         stoppos<-i
         FlagStop<-TRUE
         }
      i<-i+1   
      }
   if(stoppos==1)
      {
      return("not found")
      }  else  {  result<-substr(string,startpos,stoppos-1)
      }
   }

