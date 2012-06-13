
checkSNPsAllrs<-function(listofSNPs)
  {
   if(class(listofSNPs)!="character")
      stop("Incorrect input. Need at least one SNP rsId")
   if(length(listofSNPs)==0)
      stop("Incorrect input. Need at least one SNP rsId")
   test<-unique(substr(listofSNPs,1,2))
   if(length(test)!=1 | test[1]!="rs")
      {
      writeLines("Incorrect input. Each item must begin with rs")
      writeLines("The following was found in the first two characters of the snp names and may help you find the problem")
      print(test)
      stop()
      }
   trimmedSNPnames<-substr(as.character(listofSNPs),3,nchar(as.character(listofSNPs)))
   return(trimmedSNPnames)
  }
