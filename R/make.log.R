make.log <-
function(Object,Text,clean=FALSE)
   {
   if(clean){
      return(Text)
      } else {
   return(c(Object,Text))
      }
   }

