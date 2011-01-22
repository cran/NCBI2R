  
  check.IUPAC<-function(variants,input="code")
     {
     ans<-""
     if(input=="code")
       {
       set<-unlist(strsplit("EFIJLOPQUXZ",""))
       if(length(variants[(variants %in% set)])>0)
          ans<-paste(variants[(variants %in% set)],collapse=",")
       } else {
       set<-IUPAC(duplicates=TRUE)$alleles
       if(length(variants[!(variants %in% set)])>0)
          ans<-paste(variants[!(variants %in% set)],collapse=",")
        }
     return(ans)
     }
     