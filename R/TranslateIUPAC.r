TranslateIUPAC<-function(variants)
   {
   variants<-paste(",",paste(variants,collapse=","),",",sep="")
   if(length(grep("/",variants)!=0)) {
      if(length(grep("[BDEFHIJKLMNOPQRSUVWXYZ]",variants))!=0)
         stop("NCBI2R Translate IUPAC error: Abnormal alleles found in given sequence") 
      dataset<-IUPAC(duplicates=TRUE)  
      for(i in 1:nrow(dataset))
        variants<-gsub(paste(",",dataset$alleles[i],",",sep=""),paste(",",dataset$code[i],",",sep=""),variants)
    } else  {
       if(length(grep("[EFIJLOPQUXZ]",variants))!=0)
          stop("NCBI2R Translate IUPAC error: No slash detected and protein letters found not in IUPAC code") 
       dataset<-IUPAC(duplicates=FALSE)
       for(i in 1:nrow(dataset)) 
          variants<-gsub(paste(",",dataset$code[i],",",sep=""),paste(",",dataset$alleles[i],",",sep=""),variants)
   }
   variants<-substr(variants,2,nchar(variants))
   variants<-unlist(strsplit(variants,","))
   return(variants)
   }

   