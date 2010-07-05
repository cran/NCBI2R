strsplitdbl<-function(mystring,starttext,stoptext,include="")
   {
   testvalue<-grep(paste(starttext,"[[:print:]]*",stoptext,sep=""),mystring)
   partB<-do.call(rbind,strsplit(mystring,starttext))[,2] 
   partC<-do.call(rbind,strsplit(partB,stoptext))[,1]
   if(include=="left")
       partC<-paste(starttext,partC,sep="")
   if(include=="right")
       partC<-paste(partC,stoptext,sep="")
   if(include=="both")
       partC<-paste(starttext,partC,stoptext,sep="")
   return(partC)
  }
