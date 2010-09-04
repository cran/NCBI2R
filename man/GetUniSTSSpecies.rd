\name{GetUniSTSSpecies}
\Rdversion{1.1}
\alias{GetUniSTSSpecies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains a list of species with data for a given UniSTS
}
\description{
Obtains a list of species with available NCBI data for a given UniSTS.
}
\usage{
GetUniSTSSpecies(stsid, showurl = FALSE)
}
\arguments{
  \item{stsid}{
   The id number of the UniSTS record. Found by using GetIDs.
}
  \item{showurl}{
   Enables the user to see the URL of the NCBI request.
}
}
\details{
For a single UniSTS ID number, obtains the information about
what information is available for each species. You'll need
to have used GetIDs to obtain the ID number first.


}
\value{
A dataframe.
}
\references{
http://www.ncbi.nlm.nih.gov/sites/entrez?db=unists

This function, like most of the NCBI tools uses the eutils feature of NCBI

http://eutils.ncbi.nlm.nih.gov/entrez/query/static/eutils_help.html
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
\code{\link{GetIDs}}, \code{\link{GetUniSTSInfo}}, \code{\link{GetUniSTSFromName}}, \code{\link{GetTax}}.
}


\examples{
w<-GetUniSTSFromName("D3S1234") #this does the same as x and y together

x<-GetIDs("D3S1234",db="UniSTS")
y<-GetUniSTSInfo(x)     #x and y do the same as step w above

z<-GetUniSTSSpecies(x)


}
