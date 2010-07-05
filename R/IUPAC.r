IUPAC<-function()
  {
  code<-c("A","C","G","T","M","R","W","S","Y","K","V","H","D","B","N")
  alleles<-c("A","C","G","T","A/C","A/G","A/T","C/G","C/T","G/T","A/C/G","A/C/T","A/G/T","C/G/T","A/C/G/T")
  IUPAC<-as.data.frame(cbind(code,alleles),stringsAsFactors=FALSE)
  return(IUPAC)
  }
