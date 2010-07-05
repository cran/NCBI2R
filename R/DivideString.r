DivideString<-function(anytext,sep=",")
    {
    answer<-unlist(strsplit(anytext,sep))
    return(answer)
    }