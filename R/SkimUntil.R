SkimUntil <-
function(texttofind,dataset,ln) 
   {
   while(substr(dataset[ln],1,nchar(texttofind))!=texttofind & ln<=length(dataset))
      ln<-ln+1
   if(ln==length(dataset) & substr(dataset[ln],1,nchar(texttofind))!=texttofind)
     ln<-(-1)
   return(ln)
   }

