randomGene <- function(number=1,limits=c(1,5000),replace=FALSE)
    {
    a<-as.character(sample(limits[1]:limits[2],number,replace=replace))
    return(a)
    }
