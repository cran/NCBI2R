CleanACGT<-function(DNAseq)
   {
   answer<-gsub("[^ACGT]","",DNAseq)
   return(answer)
   }
