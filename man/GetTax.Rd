\name{GetTax}
\Rdversion{1.1}
\alias{GetTax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the NCBI Taxonomy ID number using a search term
}
\description{
Find the NCBI Taxonomy ID number using a search term
}
\usage{
GetTax(org, showurl = FALSE, smt = FALSE, sme = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{org}{
The organism you are searching for, e.g., "human", "homo sapiens".
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request.
}
  \item{smt}{
These two arguments are for checking that your requests are being
understood correctly by NCBI. If set to true, these will show all
translations of your request (smt), and any errors that were obtained in
the query (sme).
}
  \item{sme}{
These two arguments are for checking that your requests are being
understood correctly by NCBI. If set to true, these will show all
translations of your request (smt), and any errors that were obtained in
the query (sme).
}
}
\details{
Returns the ID number of a species. Required for some NCBI2R functions.

Also used internally by some NCBI2R functions.
}
\value{
A vector of ID numbers.
}
\references{
%% ~put references to the literature/web site here ~
Taxonomy website:

http://www.ncbi.nlm.nih.gov/sites/entrez?db=Taxonomy

This function, like most of the NCBI2R tools, uses the eutils feature of NCBI

http://www.ncbi.nlm.nih.gov/books/NBK25500
}
\author{
Scott Melville
}

\section{Warning}{
These functions use NCBI's eutils, and come with the same user requirements. If
performing many queries, you must run the scripts during certain hours when the
NCBI servers are not in high demand. Please see the package website for more
details.

http://NCBI2R.wordpress.com

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group.
}

\examples{
GetTax("dog")
GetTax("Mus musculus")
GetTax("dog | human")  #The symbol between the two species means OR
}
