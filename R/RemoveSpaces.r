RemoveSpaces<-function(anytext)
   {
   anytext<-gsub(" *$", "", anytext)
   anytext<-gsub("^ *", "", anytext)
   return(anytext)
  }