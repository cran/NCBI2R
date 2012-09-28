\name{GetClosestGeneInfo}
\Rdversion{1.1}
\alias{GetClosestGeneInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains information about the closest genes to a list of SNPs.
}
\description{
Obtains information about the closest genes to a list of SNPs from NCBI's Entrez database.
}
\usage{
GetClosestGeneInfo(listofSNPs,FlankingDistance=100000,lb=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{listofSNPs}{
The list of SNPs that you wish to find information from. Note: This is not a filename.
}
  \item{FlankingDistance}{
The distance on either side of the specified position to include.
}
  \item{lb}{
Should the genes only be from the Latest Build of the genome.
}
}
\details{

Returns a data frame of information for the genes that are closest to each SNP
listed. Gene information comes from NCBI's Entrez Gene database. If no gene is
found within the distance specified, no information will be returned for this
SNP within the gene information data.frame, however this SNP will be included
in the list of SNPs where no match was found.

This function performs the task by doing the following:

Runs GetSNPInfo to obtain the chromosomal positions, and to see if the SNPs are also within genes.
Obtains gene information by using GetGeneInfo for any SNPs that are within genes.
  NB - May return more than one line per snp if given SNP is inside more than one gene.
  NB - A SNP that is defined by NCBI (aka GetSNPInfo) as upstream or downstream is included as a SNP that is inside a gene.
  
For any SNP where a gene was not found above,
    GetGenesInRegion is performed for a region around each of these positions. 
    GetGeneInfo is used to obtain gene information

}
\value{
a list of two objects

  data

     A dataframe for the gene information and a list of any object

  snps.no.genes

     A vector of any snps that were not found to be near a gene.
     
 snps.unmapped
    A vector of any snps that did not have a genomic position.
}

\author{
Scott Melville
}
\section{Warning}{
If there is uncertainty about the mapping of any SNP, please pay careful attention to the column
named flag. A non-zero value represents a mapping issue that you should investigate.

Similarly, any genes that have a non-zero value for the column named approx require investigation.

}

\seealso{
\code{\link{GetSNPInfo}, \link{GetNeighGenes}}, \code{\link{GetGeneInfo}}.
}
\examples{
\donttest{
t1<-GetClosestGeneInfo(c("rs2972556","rs1081103"),50)
}
}
