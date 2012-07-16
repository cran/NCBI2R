getListFromXML <-
function(webget,smt=FALSE,sme=FALSE,MaxRet=30000,return.data=FALSE)
   {
   webget<-gsub("\t","",webget)
   webget<-gsub("<IdList>","\t<IdList>",webget)
   webget<-gsub("</IdList>","\t</IdList>",webget)
   webget<-unlist(strsplit(webget,"\t"))
   s1<-grep("^<IdList>$",webget)[1]
   s2<-grep("^</IdList>",webget)[1]
   if(is.na(s1) | is.na(s2))
     stop("NCBI2R error: parsing XML problem")
   ListItems<-gsub("[[:print:]]*<Id>([[:print:]]*)</Id>$","\\1",webget[(s1+1):(s2-1)])
   ListItems<-ListItems[ListItems!=""]
   dummy<-showMessages(webget,smt=smt,sme=sme)
   if(class(dummy$errors)=="data.frame")
      {
      writeLines("NCBI2R Fatal error using esearch query")
      print(dummy$errors)
      if(class(dummy$warnings)=="data.frame")
        {
        writeLines("Additional warnings were also present")
        print(dummy$warnings)
        }
      stop("")
      }
   if(length(ListItems[ListItems!=""])==MaxRet)
      print("Warning: Number of items was greater than expected. PARTIAL RESULTS USED [MaxRet need increasing]")
   if(return.data==TRUE)
     {
      return(list(ListItems=ListItems,errors=dummy$errors,warnings=dummy$warnings))
      } else {
         return(ListItems)
       }
   }

   