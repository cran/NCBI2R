RevComp<-function(anytext,ACGT=TRUE)
  {
  anytext<-Comp(Rev(anytext,ACGT=ACGT),ACGT=ACGT)
  return(anytext)
  }
