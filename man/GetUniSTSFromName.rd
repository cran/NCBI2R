\name{GetUniSTSFromName}
\Rdversion{1.1}
\alias{GetUniSTSFromName}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find information about UniSTS from NCBI
}
\description{
Find information about UniSTS from NCBI by entering a search term.
}

\usage{
GetUniSTSFromName(searchterm, taxid = 9606, showurl = FALSE)
}

\arguments{
  \item{searchterm}{
   The searchterm to find UniSTS records.
}
  \item{taxid}{
   The taxonomy ID number of the species to return.
   Default is human (9606). Can be found using the function
   called GetTax.
}
  \item{showurl}{
   Enables the user to see the URL of the NCBI request.
}
}
\details{
Obtains information from a single UniSTS ID number.
}
\value{
Three items are returned

  \item{CrossRefs}{Any genes or UniGenes that the marker is located with}
  \item{Maps}{The mapping information about the UniSTS record}
  \item{WarningString}{normally "None" otherwise a description of the error/warning will be displayed}

}
\references{
UniSTS website

http://www.ncbi.nlm.nih.gov/sites/entrez?db=unists

This function, like most of the NCBI2R tools uses the eutils feature of NCBI

http://www.ncbi.nlm.nih.gov/books/NBK25500
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
\code{\link{GetIDs}}, \code{\link{GetUniSTSSpecies}}, \code{\link{GetUniSTSInfo}}, \code{\link{GetTax}}.
}


\examples{

w<-GetUniSTSFromName("D3S1234") #this does the same as x and y together

x<-GetIDs("D3S1234",db="UniSTS")
y<-GetUniSTSInfo(x)     #x and y do the same as step w above

z<-GetUniSTSSpecies(x)

}
