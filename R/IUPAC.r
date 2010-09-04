IUPAC<-function(duplicates=FALSE)
  {
  if(duplicates)
     {
     code<-c("A","B","B","B","B","B","B","C","D","D","D","D","D","D","G","H","H","H","H","H","H","K","K","M","M","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","N","R","R","S","S","T","V","V","V","V","V","V","W","W","Y","Y")
     alleles<-c("A","C/G/T","C/T/G","G/C/T","G/T/C","T/C/G","T/G/C","C","A/G/T","A/T/G","G/A/T","G/T/A","T/A/G","T/G/A","G","A/C/T","A/T/C","C/A/T","C/T/A","T/A/C","T/C/A","G/T","T/G","A/C","C/A","A/C/G/T","A/C/T/G","A/G/C/T","A/G/T/C","A/T/C/G","A/T/G/C","C/A/G/T","C/A/T/G","C/G/A/T","C/G/T/A","C/T/A/G","C/T/G/A","G/A/C/T","G/A/T/C","G/C/A/T","G/C/T/A","G/T/A/C","G/T/C/A","T/A/C/G","T/A/G/C","T/C/A/G","T/C/G/A","T/G/A/C","T/G/C/A","A/G","G/A","C/G","G/C","T","A/C/G","A/G/C","C/A/G","C/G/A","G/A/C","G/C/A","A/T","T/A","C/T","T/C")
  } else {
  code<-c("A","C","G","T","M","R","W","S","Y","K","V","H","D","B","N")
  alleles<-c("A","C","G","T","A/C","A/G","A/T","C/G","C/T","G/T","A/C/G","A/C/T","A/G/T","C/G/T","A/C/G/T")
    }
  IUPAC.data<-as.data.frame(cbind(code,alleles),stringsAsFactors=FALSE)    
  return(IUPAC.data)
  }


