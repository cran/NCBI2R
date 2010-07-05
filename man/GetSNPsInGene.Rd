\name{GetSNPsInGene}
\Rdversion{1.1}
\alias{GetSNPsInGene}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This function returns a vector of all the SNPs within the boundary of
the specified gene.
}
\description{
This function returns a vector of all the SNPs within the boundary of
the user specified gene.
}
\usage{
GetSNPsInGene(locusID, MaxRet = 30000, showurl = FALSE, quiet = TRUE,
             smt = FALSE, sme = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locusID}{
An Entrez locus ID.
}
  \item{MaxRet}{
The maximum number of items to return.
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request.
}
  \item{quiet}{
Suppresses some of the output from being displayed.
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
This is quite literal - it defines the edges of the genes from NCBI, and
obtains all the SNPs from within that boundary. It does not obtain
nearby SNPs. To do this, you could use GetRegion (with db set to "set")
and use the regions as defined by AdjustRangeAroundGene.
}
\value{
vector
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
\code{\link{GetRegion}}, \code{\link{AdjustRangeAroundGene}}.
}
\examples{
SingleGene<-GetIDs("CLN5[sym]")    #returns a value of 1203
                                   #default of GetIDs is human
listSNPs<-GetSNPsInGene(SingleGene) #obtains the list
SomeGenes<-GetIDs("CLN5")  #any gene (human as default species)
                           #that contains CLN5 within the gene report.
LotsOfSNPs<-GetSNPsInGene(SomeGenes) #obtains the list
}
