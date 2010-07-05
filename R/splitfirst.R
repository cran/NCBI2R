splitfirst <-
function(lineoftext,searchfor=" ",characterposition=1)
   {
   splitnow<-FALSE
   content<-rep("",1000)
   word<-1
   while(characterposition<=nchar(lineoftext) & splitnow==FALSE)
      {
      if(substr(lineoftext,characterposition,characterposition+nchar(searchfor)-1)==searchfor)
         {
         splitnow<-TRUE
         word<-word+1
         content[word-1]<-substr(lineoftext,1,characterposition-1)
         content[word]<-substr(lineoftext,characterposition+nchar(searchfor),nchar(lineoftext))
         }
      characterposition<-characterposition+1
      }
   content[content!=""]
   }

