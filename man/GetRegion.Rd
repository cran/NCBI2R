\name{GetRegion}
\Rdversion{1.1}
\alias{GetRegion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Use this to find out a list of snps or genes in a particular region of
the genome
}
\description{
Use this to find out a list of snps or genes in a particular region of
the genome.
}
\usage{
GetRegion(db, chr, LowPoint, HighPoint, MaxRet = 30000, showurl = FALSE,
          cg = TRUE, org = "human", quiet = TRUE, smt = FALSE, sme = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{db}{
The NCBI database to use. "snp" or "gene".
}
  \item{chr}{
The chromosome (specified as a character) of the genome position.
}
  \item{LowPoint}{
The boundaries of the gene. The lowpoint is the boundary at the smallest
nucleotide position. Use in conjunction with the orientation to
determine the gene start and stop position.
}
  \item{HighPoint}{
The boundaries of the gene. The lowpoint is the boundary at the smallest
nucleotide position. Use in conjunction with the orientation to
determine the gene start and stop position.
}
  \item{MaxRet}{
The maximum number of items to return.
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request.
}
  \item{cg}{
If set to true, data will return only current genes, otherwise
discontinued genes will also be returned.
}
  \item{org}{
The name of the organism to use.
}
  \item{quiet}{
Suppresses some of the output on screen.
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
For snps ... using GetRegion is better than another function GetSNPsInGene
which obtains the SNPs strictly for the gene boundary as defined by
NCBI, but can include flanking regions (use AdjustRangeAroundGene).

eg step A use GetGeneInfo on a gene   GetGeneInfo(1203)

eg step B AdjustRangeAroundGene on those start and stop positions

eg step C GetRegion(from step B)

gene filters may be applied. More information available soon.
}
\value{
A list of IDs or SNP names.
}

\author{
Scott Melville
}

\section{Warning}{
chr is defined as a character, because of X, Y, XY, mt values.

If both boundaries are fully inside a gene, that locusID will not be returned. The code for genes looks for gene boundaries within these two points, not if a gene is situated there.

These functions use NCBI's eutils, and come with the same user requirements - if
performing many queries, you must run the scripts during certain hours when the
NCBI servers are not in high demand. Please see the package website for more details

http://NCBI2R.wordpress.com

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group. But remember, the average light user should have no problems at all. NCBI2R inserts small delays into most of the code - it's should only be if you are performing many queries in a short space of time.
}


\seealso{
\code{\link{GetGenesInRegion}}, \code{\link{GetSNPsInRegion}}.
}
\examples{
GetRegion(db="gene","mt",1,100000) #for the mitochondrial genome
GetRegion(db="snp","13",123456789,200222333) #chromosome must be a character
}
