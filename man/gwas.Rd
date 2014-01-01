\name{gwas}
\alias{gwas}
\docType{data}
\title{
Sample gwas data from a fictional project
}
\description{
This data set of fictional GWAS results can demonstrate NCBI2R.

}
\usage{data(gwas)}
\format{
  A data frame with some fictional data in four columns
  \describe{
    \item{\code{marker}}{a character vector}
    \item{\code{chr}}{a numeric vector}
    \item{\code{chrpos}}{a numeric vector}
    \item{\code{p}}{a numeric vector}    
  }
}
\details{
Fictional data used to demonstrate the functions of the NCBI2R package.
This data may be different in each release of the package.
}

\examples{
\donttest{
data(gwas)
GenomeSort(gwas)
}
}
\author{
Scott Melville
}
\keyword{datasets}
