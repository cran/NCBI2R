\name{GetPathways}
\Rdversion{1.1}
\alias{GetPathways}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Obtains pathways from NCBI for a given set of genes
}
\description{
 Obtains pathways for any multiple Entrez locusIDs using NCBI.
}
\usage{
GetPathways(locusIDs, showurl = FALSE,silent=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locusIDs}{
A single or multiple Entrez locus ID numbers.
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request. 
}
  \item{silent}{
Display a message on screen if a locusID has no information. Default is no.
}
}
\details{
This function works internally just like as GetGeneInfo, and takes a similar amount of time, but it returns only the pathway information. It may be faster obtaining all of the gene information by using GetGeneInfo, however the function was included here for those that want to customize workflow.

For each gene listed, there will be one query of NCBI.
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

http://NCBI2R.wordpress.com

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group.
}
\seealso{
\code{\link{GetPhenotypes}}, \code{\link{GetGOs}}, \code{\link{GetInteractions}}, \code{\link{GetGeneInfo}}.
}
\examples{
s<-GetPhenotypes(c(348,1203,1204,777)) #some information may be blank for some genes
t<-GetPathways(c(348,1203,1204,777))     #some information may be blank for some genes
}

