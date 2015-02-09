\name{GetIDs}
\Rdversion{1.1}
\alias{GetIDs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtain Entrez ID numbers
}
\description{
Obtain Entrez ID numbers from a search string.
}
\usage{
GetIDs(term, org = "human", strict = TRUE, cg = TRUE, db = "gene",
    MaxRet = 30000, showurl = FALSE, sme = FALSE, smt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{term}{
The search term that you'd do at NCBI website for EntrezGenes.
}
  \item{org}{
The organism that you want the IDs for. The default is human.
}
  \item{strict}{
Full match only with searches on genesymbols.
}
  \item{cg}{
If set to true, data will return only current genes, otherwise discontinued genes will also be returned.
}
  \item{db}{
The database to use "snp" or "gene".
}
  \item{MaxRet}{
The maximum number of items to return from the query.
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request. 
}
  \item{sme}{
These two arguments are for checking that your requests are being understood correctly by NCBI. If set to true, these will show all translations of your request (smt), and any errors that were obtained in the query (sme).
}
  \item{smt}{
These two arguments are for checking that your requests are being understood correctly by NCBI. If set to true, these will show all translations of your request (smt), and any errors that were obtained in the query (sme).
}
}
\details{
This list of LocusIDs can then be used to obtain more information about the genes, or used as a list of candidates for ScanForGenes.
You can use this function within other queries as shown below in the example.

This query is best used for one gene at a time but there are ways to do more than one gene at a time.

Each object within NCBI gets a unique identifier - different for each database.

Each use of this function represents a single query of NCBI. By including more than one search term
the number of times this function needs to be run will be reduced. However, please see the warnings
below.

}
\value{
A vector of ID numbers specific to the database you queried.
}
\references{
For more Information about filters and limits that can be used on the website and this NCBI2R package (not affiliated with NCBI) please see the following page for NCBI's Gene database page
http://www.ncbi.nlm.nih.gov/books/NBK3841/table/

On that webpage there are two useful tables that may be consulted to design your own queries for some NCBI2R functions.

Table 4 - Filter sets
Table 5. - Fields sets

This links were checked 24 September 2012. If they are not there, please let me know via the package website.
http://ncbi2r.wordpress.com
}
\author{
Scott Melville
}

\section{Warning}{

If you query more than one search term at a time (separated by a pipe symbol as shown below),
the IDs returned may not be in the order that you expect so check carefully.

These functions use NCBI's eutils, and come with the same user requirements - if
performing many queries, you must run the scripts during certain hours when the
NCBI servers are not in high demand. Please see the package website for more details

http://NCBI2R.wordpress.com

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group. But remember, the average user should have no problems at all. NCBI2R inserts delays most of the times - it's only going to be if you are performing large numbers of queries in a short space of time.
}

\seealso{
\code{\link{PrintFilters}}.
}

\examples{
\donttest{
s<-GetIDs("Cdc42[go]")
GetIDs("bone")  #Get all genes that have anything to do with bone (#def=?)
GetIDs("bone[DIS]")  #Get all genes that have anything to do with bone disease
GetIDs("CLN5[sym]", org="dog")
GetIDs("CLN5[SYM]|APOE[SYM]") #this performs queries for two genes at the same time.
s<-GetIDs("Cdc42 protein signal transduction[go]")
t<-GetGeneInfo(s)
}
}
