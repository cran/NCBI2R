Rev<-function(DNAseq,ACGT=TRUE)
   {
   if(ACGT==TRUE)
      DNAseq<-CleanACGT(DNAseq)
   answer<-paste(rev(as.character(do.call(cbind,strsplit(DNAseq,"")))),collapse="")
   return(answer)
   }
