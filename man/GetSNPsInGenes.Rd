\name{GetSNPsInGenes}
\Rdversion{1.1}
\alias{GetSNPsInGenes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This function returns a vector of all the SNPs within the boundary of
a gene or genes.
}
\description{
This function returns a vector of all the SNPs within the boundary of
a gene or genes.}
\usage{
GetSNPsInGenes(locusIDs, batchsize=200, MaxRet = 30000, showurl = FALSE,
             quiet = TRUE, smt = FALSE, sme = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locusIDs}{
One or more Entrez locus IDs.
}
  \item{batchsize}{
The number of ids that are sent to NCBI for processing in each batch. Faster
results may be achieved by changing this number depending on internet/CPU speeds.  
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
This function uses the edges of the genes from NCBI, and obtains all the SNPs
from within those boundaries. It also includes nearby SNPs that NCBI considers
upstream and downstream SNPs. I recommend using GetSNPInfo to check if your list
of SNPs includes upstream and downstream SNPs. 

If you wanted to include SNPs from outside the gene, you could use the function
AdjustRangeAroundGene to find the new genome region and then supply that as
input for GetSNPsInRegion.

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

http://NCBI2R.wordpress.com

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group.
}
\seealso{
\code{\link{GetRegion}}, \code{\link{AdjustRangeAroundGene}}.
}
\examples{
\donttest{
SingleGene<-GetIDs("CLN5[sym]")    #returns a value of 1203
                                   #default of GetIDs is human
listSNPs<-GetSNPsInGenes(SingleGene) #obtains the list
SomeGenes<-GetIDs("CLN5")  #any gene (human as default species)
                           #that contains CLN5 within the gene report.
LotsOfSNPs<-GetSNPsInGenes(SomeGenes) #obtains the list
}
}
