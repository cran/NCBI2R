\name{GetSNPInfo}
\Rdversion{1.1}
\alias{GetSNPInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains NCBI information about one or more SNPs
}
\description{
Obtains NCBI information about one or more SNPs.
}
\usage{
GetSNPInfo(listofSNPs, batchsize = 200, showurl = FALSE, pbar=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{listofSNPs}{
The list of SNPs that you wish to find information from.
}
  \item{batchsize}{
Number of SNPs that will get grouped together into one URL request.
Depending on internet connection speed and processing power, changing
this value might lead to faster analysis.
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request.
}
  \item{pbar}{
Show progress bar. Default is TRUE.
}


}
\details{
Obtains NCBI information about one or more SNPs, including genome
position, and if the SNP is located inside any genes. If it is within
the boundaries of a gene, the gene symbol and NCBI Entrez ID will
also be returned. The Entrez ID (also known as locus ID) can then be
used in commands such as GetGeneInfo to obtain information about
the gene.

If all items do not start with "rs", the program will stop and a
warning message will be generated.

Each batch of this function represents a single query of NCBI.
}
\value{
  dataframe
}

\author{
Scott Melville
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning}{

The definition of a SNP being within a gene is very literal. If the
SNP is one base pair outside the NCBI defined gene location, it will
not be found. Consider using GetRegion or GetNeighbours.

The locusID column that is returned, may not be only numerical.
If a SNP is found within multiple genes, the locusID column will contain
an ID number from each of the genes, separated by a column. Similarly,
the column with the gene symbols will contain multiple gene symbols.

These functions use NCBI's eutils, and come with the same user requirements - if
performing many queries, you must run the scripts during certain hours when the
NCBI servers are not in high demand. Please see the package website for more details

http://NCBI2R.wordpress.com

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group.
}

\seealso{
\code{\link{GetGeneInfo}}, \code{\link{GetRegion}}, \code{\link{GetNeighGenes}}, \code{\link{SplitGenes}}.
}
\examples{
snplist<-c("rs12345","rs333","rs624662")
mySNPs<-GetSNPInfo(snplist)
mySNPs<-SplitGenes(mySNPs)
}