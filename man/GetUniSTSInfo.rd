\name{GetUniSTSInfo}
\Rdversion{1.1}
\alias{GetUniSTSInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains information about a given UniSTS from NCBI
}
\description{
Obtains information about a given UniSTS from NCBI. Use this to find physical or linkage map position of microsatellite markers.
}
\usage{
GetUniSTSInfo(stsid, taxid = 9606, showurl = FALSE,quiet=TRUE)
}
\arguments{
  \item{stsid}{
   The ID number of the UniSTS record. Found by using GetIDs.
}
  \item{taxid}{
   The taxonomy ID number of the species to return.
   Default is human (9606). Can be found using the function
   called GetTax.
}
  \item{showurl}{
   Enables the user to see the URL of the NCBI request.
}

  \item{quiet}{
  If set to false, will show the number of lines in the HTML file upon loading.
}


}
\details{
Obtains information from a single UniSTS ID number which is probably not
what you might now a microsatellite marker by. If you don't know the ID number,
use GetUniSTSFromName instead, which will get the ID number and then process.

This is the main function to use to obtain information about a microsatellite marker through the UniSTS records.
All mapping data is returned, from both linkage and physical maps.


}
\value{
Three items are returned

  \item{CrossRefs}{Any genes or UniGenes that the marker is located with}
  \item{Maps}{The mapping information about the UniSTS record}
  \item{WarningString}{Normally "None" otherwise a description of the error/warning will be displayed}

}
\references{
UniSTS website:

http://www.ncbi.nlm.nih.gov/sites/entrez?db=unists

This function, like most of the NCBI2R tools, uses the eutils feature of NCBI

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
\code{\link{GetIDs}}, \code{\link{GetUniSTSSpecies}}, \code{\link{GetUniSTSFromName}}, \code{\link{GetTax}}
}


\examples{

w<-GetUniSTSFromName("D3S1234") #this does the same as x and y together

x<-GetIDs("D3S1234",db="UniSTS")
y<-GetUniSTSInfo(x)     #x and y do the same as step w above

z<-GetUniSTSSpecies(x)

}
