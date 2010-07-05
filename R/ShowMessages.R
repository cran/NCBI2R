ShowMessages <-
function(webget,smt=FALSE,sme=FALSE)
   {
   cleanedset<-parse.items(webget)
   QT<-Cutter(cleanedset[length(cleanedset)],"QueryTranslation")
   if(smt==TRUE)
      print(paste("QueryTranslation was:",QT$answer))
   if(substr(QT$rem,1,11)=="<ErrorList>")
      {                                   
      ErrorList<-splitfirst(splitfirst(QT$rem,"<ErrorList>"),"</ErrorList>")[1]
      tagtofind<-substr(splitfirst(ErrorList,">")[1],2,nchar(splitfirst(ErrorList,">")[1]))
      tti<-ErrorList
      while(nchar(tti)>0)
         {
         f<-Cutter(tti,tagtofind)
         if(sme==TRUE)
            print(paste("Error on Query (",tagtofind,"): ",f$answer,sep=""))
         tti<-f$rem
         }
      }
   }

