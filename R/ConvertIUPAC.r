ConvertIUPAC<-function(variants)
   {
   variants<-toupper(variants) 
   if(length(grep("/",variants)!=0)) {
      if(length(grep("[BDEFHIJKLMNOPQRSUVWXYZ]",unlist(strsplit(variants,""))))!=0)
        stop("NCBI2R Translate IUPAC error: Abnormal alleles found in given sequence\n. This only accepts combinations of A C G T")  
     dataset<-IUPAC(duplicates=TRUE)  
     fakes<-as.data.frame(cbind(code=c("C","A","T","G"),alleles=c("C/C","A/A","T/T","G/G")),stringsAsFactors=FALSE)
     dataset<-as.data.frame(rbind(dataset,fakes),stringsAsFactors=FALSE)
     if(length(variants[!(variants %in% dataset$alleles)])>0)
        {
         writeLines("")
          writeLines("NCBI2R Translate IUPAC error: Unable to determine input.")
          writeLines("Expecting input like A/T C/G etc *or* Y R W etc")
          writeLines("Because a slash was detected at least once in the input, complete input was believed to be alleles.")
          bad<-check.IUPAC(variants,input="alleles")  
          if(bad!="")
            writeLines(paste("Bad input was:",bad))
          stop("Please check the input and try again.")  
        stop("NCBI2R Translate IUPAC error: Abnormal alleles found in given sequence. Expecting input like A/T C/G etc or Y R W etc") 
        }
     variants<-merge(as.data.frame(variants),dataset,by.x="variants",by.y="alleles",all=FALSE)$code   
    } else  {
      if(length(grep("[EFIJLOPQUXZ]",variants))!=0)
          {
          writeLines("")
          writeLines("NCBI2R Translate IUPAC error: Unable to determine input.")
          writeLines("Expecting input like A/T C/G etc *or* Y R W etc")
          writeLines("Because a slash was not detected in the input, entire input was believed to be IUPAC codes.")
          bad<-check.IUPAC(variants) 
          if(bad!="")
            writeLines(paste("Bad input was:",bad))
          stop("Please check the input and try again.")  
          }
       dataset<-IUPAC(duplicates=FALSE)
     fakes<-as.data.frame(cbind(code=c("C","A","T","G"),alleles=c("C/C","A/A","T/T","G/G")),stringsAsFactors=FALSE)
     dataset<-as.data.frame(rbind(dataset,fakes),stringsAsFactors=FALSE)       
     if(length(variants[!(variants %in% dataset$code)])>0)
        stop("NCBI2R Translate IUPAC error: Abnormal alleles found in given sequence. Expecting input like A/T C/G etc or Y R W etc") 
     variants<-merge(as.data.frame(variants),dataset,by.x="variants",by.y="code",all=FALSE)$alleles
     }
  return(variants)
  }

