GenomeSort<-function(df1,keycols=c("chr","chrpos"),remove.leading.zeroes=TRUE)
  {
  if(class(df1[,keycols[1]])=="factor")
    stop("Cannot accept a factor as the chromosome column")
  df1[,keycols[2]]<-as.numeric(df1[,keycols[2]])
  if(checkAllNumerics(df1[,keycols[1]]))
     {
     df1<-df1[with(df1,order(eval(parse(text=keycols[1])), eval(parse(text=keycols[2])))),]
     } else {
    lead.zeroes<-1
    df1[,keycols[1]]<-as.character(  df1[,keycols[1]])
    df1[grep("^[[:digit:]]$",df1[,keycols[1]]),keycols[1]]<-paste(rep("0",lead.zeroes),df1[grep("^[[:digit:]]$",df1[,keycols[1]]),keycols[1]],sep="")
    df1<-df1[with(df1,order(eval(parse(text=keycols[1])), eval(parse(text=keycols[2])))),]
    if(remove.leading.zeroes)
      df1[,keycols[1]]<-gsub("^0*","",df1[,keycols[1]])
    }
  return(df1)
  }
