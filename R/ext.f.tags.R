ext.f.tags <-
function(Text,t1=">",t2="</")
   {
   return(splitfirst(splitfirst(Text,t1)[2],t2)[1])
   }

