
checkAllNumerics<-function(vec)
  {
  if(class(vec)=="factor")
    stop("Cannot use values that are factors")
  if(class(vec)=="numeric")
    {
    return(TRUE)
    } else {
    tmp<-suppressWarnings(as.numeric(vec))
    ifelse(length(tmp[!is.na(tmp)])==length(tmp),return(TRUE),return(FALSE))
    }
  }
