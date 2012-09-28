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
GetSNPInfo(listofSNPs, batchsize = 200, showurl = FALSE, pbar=TRUE,
           style="docset.xml")
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
  \item{style}{
Which type of NCBI file will be used. Only one value available at present.
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

At present time, only one value for the style argument is available.

Each batch of this function normally represents a single query of NCBI. However,
since early 2012, after the usual parsing of files in the fast manner, any snps
that are not mapped properly will undergo an additional check using a more
comprehensive manner. The additional queries will only be performed on a small
number of SNPs for a typical user so any delays will be minimal. The extra check
was added in response to NCBI no longer listing map positions of markers with
more than one mapping location.

Check the flag column carefully as a value other than zero indicates
a likely problem with the SNP record. This could point to a warning
such as the SNP mapping may be uncertain etc, and should be checked
with the web version of NCBI for more information.

There are two different cases when a SNP will have a position returned as 0 and both related to
them not being found. In some cases, the species and current.rsid will be complete but there will be
no mapping position because this SNP is not found on the latest map. You may notice that this
function will try a second method to confirm this. For SNPs that are not found at all in the NCBI
database, there will be empty strings for the species or current.rsid in the information returned.

Please note. It is not possible to a SNPs older position. Only the position (if available) from
the current genome build will be returned.

}
\value{
A dataframe
}

\author{
Scott Melville
}

\section{Warning}{

The definition of a SNP being within a gene is precisely determined by NCBI.
If the SNP is one base pair outside the defined gene location, it will
not be found. Consider using GetRegion or GetNeighbours. In some cases, a SNP will appear to be
within the gene but is literally located outside the gene but closeby eg (within 5 kb).
This reflects NCBI's classification and not a NCBI2R issue. I have no affiliation with NCBI.

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