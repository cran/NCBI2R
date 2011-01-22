cutter <-
function(Text,HTMLtag) 
   {
   counter<-1
   while(substr(Text,counter,(counter+nchar(HTMLtag)-1))!=HTMLtag)
      counter<-counter+1
   counter<-counter+nchar(HTMLtag)+1
   endcounter<-counter
   while(substr(Text,endcounter,(endcounter+nchar(HTMLtag)))!=paste("/",HTMLtag,sep="") & endcounter<=nchar(Text))
      endcounter<-endcounter+1
  answer<-"******No Query Translation available******"
   if(endcounter<nchar(Text))
      answer<-substr(Text,counter,endcounter-2)
   leftover<-substr(Text,endcounter+nchar(HTMLtag)+2,nchar(Text))
   return(list(answer=answer,rem=leftover))
   }

