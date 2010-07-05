 Excel.RIGHT<-function(text,num_chars=1)
    {
    answer<-substr(text,nchar(text)-num_chars+1,nchar(text))
    return(answer)
    }
