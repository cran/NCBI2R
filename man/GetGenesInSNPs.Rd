\name{GetGenesInSNPs}
\Rdversion{1.1}
\alias{GetGenesInSNPs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns a vector of genes that the provided SNPs are located within.
}
\description{
Returns a vector of genes that the SNPs are located within.
}
\usage{
GetGenesInSNPs(listofSNPs, output="locusID",batchsize=200)
}

\arguments{
  \item{listofSNPs}{
The list of SNPs that you wish to find information from.
}
  \item{batchsize}{
The number of ids that are sent to NCBI for processing in each batch. Faster
results may be achieved by changing this number depending on internet/CPU speeds.  
}  
  \item{output}{
The column from GetSNPInfo that you'd like returned: Either locusID or genesymbol must be provided.
}
}
\details{
Input a list of SNPs, the function will run GetSNPInfo to find if a snp is in a gene or not.

Only the list of genes is returned. Duplicates will be removed.

The order may NOT correspond to the order as the snps provided.

Any warnings from the GetSNPInfo will be displayed on screen only.

If no genes are found, it will return an empty vector.

If this function does not do what you want, please use GetSNPInfo and filter appropriately.

}
\value{
vector
}
\author{
Scott Melville
}


\section{Warning}{

This tool relies on the NCBI2R function GetSNPInfo. The definition of if a SNP is in a gene may not
be exactly what you expect. Please see the warnings for that function.

Any warnings from the GetSNPInfo will be displayed on screen only. There is no flag column in the
final list of genes.

These functions use NCBI's eutils, and come with the same user requirements - if
performing many queries, you must run the scripts during certain hours when the
NCBI servers are not in high demand. Please see the package website for more details

http://NCBI2R.wordpress.com

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group.
}
\seealso{
\code{\link{GetSNPsInGenes}}, \code{\link{GetSNPInfo}}.
}
\examples{
snplist<-c("rs1234567","rs61363","rs632163","rs7477823")
genelist<-GetGenesInSNPs(snplist)
}
