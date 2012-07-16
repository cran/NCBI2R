showMessages <-
function(webget,smt=FALSE,sme=FALSE)
   {
   cleanedset<-parse.items(webget)

 if(length(grep("<QueryTranslation>",webget))==0)
    {
     answer<-"******No Query Translation available******"
     rem<-webget
     } else {
     answer<-gsub("^[[:print:]]*<QueryTranslation>([[:print:]]*)</QueryTranslation>[[:print:]]*","\\1",webget[grep("<QueryTranslation>",webget)])
     rem<-gsub("^[[:print:]]*<QueryTranslation>([[:print:]]*)</QueryTranslation>([[:print:]]*)","\\2",webget[grep("<QueryTranslation>",webget)])
     }
   s1<-"No errors"
   s2<-"No warnings"
   if(smt==TRUE)
      print(paste("QueryTranslation was:",answer))
   if(length(grep("<ErrorList>",rem))==1 | length(grep("<WarningList>",rem))==1)
      {
      Y<-unlist(strsplit(rem,"<"))
      if(length(grep("<ErrorList>",rem))==1)
         {
         subtxt<-Y[(grep("ErrorList>",Y)[1]+1):(grep("ErrorList>",Y)[2]-1)]
         s1<-as.data.frame(matrix(subtxt,ncol=2,byrow=TRUE),stringsAsFactors=FALSE)
         s1$tag<-gsub("/|>","",s1$V2)
         s1$text<-gsub("^[[:print:]]*>([[:print:]]*)$","\\1",s1$V1)
         s1<-s1[,c("tag","text")]
         if(sme)
           writeLines(paste("Error On Query (",s1$tag,"): ",s1$text,sep="")) 
         }
      if(length(grep("<WarningList>",rem))==1)
         {
         subtxt<-Y[(grep("WarningList>",Y)[1]+1):(grep("WarningList>",Y)[2]-1)]
         s2<-as.data.frame(matrix(subtxt,ncol=2,byrow=TRUE),stringsAsFactors=FALSE)
         s2$tag<-gsub("/|>","",s2$V2)
         s2$text<-gsub("^[[:print:]]*>([[:print:]]*)$","\\1",s2$V1)
         s2<-s2[,c("tag","text")]
         if(sme)
           writeLines(paste("Warning On Query (",s2$tag,"): ",s2$text,sep="")) 
        }
    }
   return(list(errors=s1,warnings=s2))
   }

