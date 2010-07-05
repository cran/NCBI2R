Comp<-function(DNAseq,ACGT=TRUE)
   {
   if(ACGT==TRUE)
      DNAseq<-CleanACGT(DNAseq)
  DNAseq<-gsub("A","1",DNAseq)
  DNAseq<-gsub("C","2",DNAseq)
  DNAseq<-gsub("G","3",DNAseq)
  DNAseq<-gsub("T","4",DNAseq)
  DNAseq<-gsub("1","T",DNAseq)
  DNAseq<-gsub("2","G",DNAseq)
  DNAseq<-gsub("3","C",DNAseq)
  DNAseq<-gsub("4","A",DNAseq)
  return(DNAseq)
   }

