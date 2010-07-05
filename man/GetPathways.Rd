\name{GetPathways}
\Rdversion{1.1}
\alias{GetPathways}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Obtains pathways from NCBI for a given gene
}
\description{
 Obtains pathways for any given EntrezID number using NCBI.
}
\usage{
GetPathways(idnumber, showurl = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{idnumber}{
A single Entrez locus ID.
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request. 
}
}
\details{
This function internally works just the same as GetGeneInfo, and will take a similar amount of time, and return only the pathway information. You're probably better off obtaining all of the information by using GetGeneInfo, however the function was included here for those that want to customize workflow.
}
\value{
A dataframe
}

\author{
Scott Melville
}


\section{Warning}{
These functions use NCBI's eutils, and come with the same user requirements - if
performing many queries, you must run the scripts during certain hours when the
NCBI servers are not in high demand. Please see the package website for more details

http://drop.io/NCBI2R_package

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group.
}
\seealso{
\code{\link{GetPhenotypes}}, \code{\link{GetGOs}}, \code{\link{GetInteractions}}, \code{\link{GetGeneInfo}}.
}
\examples{
s<-GetPhenotypes(348)
t<-GetPathways(348)
}

