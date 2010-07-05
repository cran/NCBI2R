add.lead.zero <-
function(number,digits=2)
   {
   number<-as.character(number)
   for(i in 1: length(number))
      {
      if(nchar(number[i])<digits) 
        {
        print(paste("changing",number[i]))
        number[i]<-paste("0",number[i],sep="")
        print(paste("to",number[i]))
        }
      } 
   return(number)   
   } 

