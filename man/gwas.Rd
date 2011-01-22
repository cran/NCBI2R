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
Fictional data used to demonstrate the functions of NCBI2R. This data will
probably be different in each release of the package as I change it to
illustrate some new features of NCBI2R.
}

\examples{
data(gwas)
## maybe str(gwas) ; plot(gwas) ...
}
\author{
Scott Melville
}
\keyword{datasets}
