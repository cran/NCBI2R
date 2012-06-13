\name{GenomeSort}
\Rdversion{1.1}
\alias{GenomeSort}
\title{
Sorts the genome by chromosome and position
}
\description{
Sorts the genome by chromosome and chromosomal position
}
\usage{
GenomeSort(df1, keycols=c("chr","chrpos"),remove.leading.zeroes=TRUE)
}

\arguments{
  \item{df1}{
     The dataframe of your data which must contain columns with the names of the
     two keycols.
}
  \item{keycols}{
     The names of the chromosome and chromosome columns in the dataframe supplied.
     Both keycols must be specified.
}
  \item{remove.leading.zeroes}{
    If the chromosome consists of characters then a leading zero will be used for single digit
    chromosomes. This argument will allow you to keep those leading zeroes within the output.
}

}
\details{

The function sorts all of the data.frame by the two columns specified and the complete data set will be returned.

This function will force the chrpos column to be a numeric value, whereas the chromosome column can
be either character or numeric class.

Even if the chromosome is of character class (eg because of the prescence of X and Y), it will
temporarily add a leading zero to the single digit chromosomes and then sort alphabetically.
Normally this leading zero will be removed but you can keep it by using the remove.leading.zeroes
argument of this function.

}
\value{
data.frame
}
\author{
Scott Melville
}
\section{Warning}{
The chromosome column is sorted alphabetically (although with leading zeroes as discussed above). So
depending on the notation used for some species, the order in the output may not be what you expect.
}
\examples{
 a<-as.data.frame(cbind(chr=c("6","X","12"),chrpos=c(123,515,563),value=c("A","C","D"),data=c(19,-51,52.2)),stringsAsFactors=FALSE)
 GenomeSort(a)
}
