\name{GetGeneNames}
\Rdversion{1.1}
\alias{GetGeneNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains information about user defined genes from NCBI
}
\description{
Obtains NCBI information about user defined genes from NCBI's Entrez Gene database using eutilities.
}
\usage{
GetGeneNames(locusID, batchsize = 200, showurl = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locusID}{
One or more Entrez locus ID numbers to obtain information for.
}
  \item{batchsize}{
The number of ids that are sent to NCBI for processing in each batch. Faster results may be achieved by changing this number depending on internet/CPU speeds.
}
  \item{showurl}{
Displays the URL of the requests. Useful for learning how the functions work, and how you might wish to write your own functions for querying NCBI.
}
}
\details{
This function is much faster than GetGeneInfo and returns the gene name, gene symbol, species and importantly, if the gene record is current, or has been replaced.

It returns less information, but also some items can not be obtained from GetGeneInfo. These function is really useful when you just want a basic amount of information, such as the gene symbol and full gene name. Also, you can find out if the locusID has been replaced by a more recent gene identifier.
Will work with multiple IDs, just like GetGeneInfo.

}
\value{
A dataframe with genename, genesymbol, NewlocusID,CurrentRecord,LastUpdate,locusID, species.
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
\code{\link{GetGeneInfo}}, \code{\link{GetSNPInfo}}.
}
\examples{
GetGeneNames(1203)
a<-GetGeneNames(348)
a$genesymbol
}
