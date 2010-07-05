Excel.FIND<-function(find_text,within_text,startnum=1)
      {
      i<-startnum
      answer<-(-1)
      while(i<=nchar(within_text) & answer==(-1))
        {
       if(substr(within_text,i,i+nchar(find_text)-1)==find_text)
            answer<-i
        i<-i+1
        }
     return(answer)
      }
