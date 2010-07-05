parse.items <-
function(dataset)
   {
   while(length(splitfirst(dataset[length(dataset)],"  "))==2)
      {
      a<-splitfirst(dataset[length(dataset)],"  ")
      counter<-length(dataset)
      while(substr(a[2],1,1)==" ")
         a[2]<-substr(a[2],2,nchar(a[2]))
      dataset[counter+1]<-a[2]
      dataset[counter]<-a[1]
      counter<-counter+1
      }
   return(dataset)
   }