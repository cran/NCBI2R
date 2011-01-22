parse.chunk<-function(chunk)
   {
   i<-length(chunk)
   while(i>0)
      {
      while(length(splitfirst(chunk[i],">"))!=0 & !is.na(splitfirst(chunk[i],">")[2]))
         {
         partA<-paste(splitfirst(chunk[i],">")[1],">",sep="")
         partB<-splitfirst(chunk[i],">")[2]
         chunk[i]<-partA
         for(tt in (length(chunk)):i)
            chunk[tt+1]<-chunk[tt]
         chunk[i+1]<-partB
         i<-i+1
         }
      i<-i-1
      }
  return(chunk)
  }
  