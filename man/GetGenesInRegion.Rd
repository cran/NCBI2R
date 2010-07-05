\name{GetGenesInRegion}
\Rdversion{1.1}
\alias{GetGenesInRegion}

\title{
Use this to find out a list of genes in a particular region of
the genome.
}
\description{
Use this to find out a list of genes in a particular region of
the genome.
}
\usage{
GetGenesInRegion(chr, LowPoint, HighPoint, MaxRet = 30000, showurl = FALSE,
                cg= TRUE, org = "human", quiet = TRUE, sme = FALSE, smt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chr}{
Specify the chromosome as a character.
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
The maximum number of items to return from the query/request.
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
GetGenesInRegion does exactly what you think it might :)  Specify the
chromosome and the region of the genome and the function will return
the NCBI ID numbers for the genes in that region. You can then use
another function such as GetGeneInfo to return information about them.

I've noticed some cases that if you specify a region, and the gene starts and stops outside that region, it will not be included.
}
\value{
All the NCBI Entrez ID numbers of the genes that have gene boundaries in the region.
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
\code{\link{GetRegion}}, \code{\link{GetSNPsInRegion}}
}
\examples{
GetGenesInRegion("3",1,1000000) 

GetGenesInRegion("mt",1,100000) #for the mitochondrial genome
GetRegion(db="gene","mt",1,100000) #works the same as the above line
}