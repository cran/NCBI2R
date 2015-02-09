\name{CompareTaxInfo}
\Rdversion{1.1}
\alias{CompareTaxInfo}

\title{
Identify common lineages of taxonomy terms
}
\description{
Identify common lineages of two taxonomy terms
}
\usage{
CompareTaxInfo(id1=9606, id2=10090)
}

\arguments{
  \item{id1}{
The identifier for NCBI's taxonomy database for the first term.
}
  \item{id2}{
The identifier for NCBI's taxonomy database for the second term.
}
}
\details{
Using the function GetTaxInfo, the lineage information is obtained for both
supplied taxonomy terms.

}
\value{
Returns a list of three dataframes. The first will be the lineages that the two
terms have in common. The second and third dataframes are the lineages where
the items diverge.
}

\author{
Scott Melville
}

\section{Warning}{

These functions use NCBI's eutils, and come with the same user requirements - if
performing many queries, you must run the scripts during certain hours when the
NCBI servers are not in high demand. Please see the package website for more details

http://NCBI2R.wordpress.com

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group. But remember, the average light
user should have no problems at all. NCBI2R inserts small delays into most of
the code - it's should only be if you are performing many queries in a short
space of time.
}


\seealso{
\code{\link{GetTax}}, \code{\link{GetTaxInfo}}.
}
\examples{
\donttest{
g1<-GetTax("puffinus")
g2<-GetTax("penguins")
g12<-CompareTaxInfo(g1,g2)
print(g12)
}
}
