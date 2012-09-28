
checkAllNumerics<-function(vec)
  {
  if(class(vec)=="data.frame")
    stop("NCBI2R Error E61: Expecting a vector of values")
  if(class(vec)=="factor")
    stop("NCBI2R Error E62: Cannot use values that are factors")
  if(class(vec)=="numeric")
    {
    return(TRUE)
    } else {
    tmp<-suppressWarnings(as.numeric(vec))
    ifelse(length(tmp[!is.na(tmp)])==length(tmp),return(TRUE),return(FALSE))
    }
  }
