\name{GetSNPsInRegion}
\Rdversion{1.1}
\alias{GetSNPsInRegion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains a list of SNPs in a particular region of the genome.
}
\description{
Obtains a list of SNPs in a particular region of the genome.
}
\usage{
GetSNPsInRegion(chr, LowPoint, HighPoint, MaxRet = 30000, showurl = FALSE,
                org = "human", quiet = TRUE, sme = FALSE, smt = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chr}{
Chromosome as a character.
}
  \item{LowPoint}{
The boundaries of the gene. The lowpoint is the boundary at the smallest
nucleotide position. Use in conjunction with the orientation to
determine the gene start and stop position.
}
  \item{HighPoint}{
The boundaries of the gene. The highpoint is the boundary at the smallest
nucleotide position. Use in conjunction with the orientation to
determine the gene start and stop position.
}
  \item{MaxRet}{
The maximum number of items to return in the request.
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request.
}
  \item{org}{
The name of the organism to use.
}
  \item{quiet}{
Suppresses some of the output on screen.
}
  \item{sme}{
These two arguments are for checking that your requests are being
understood correctly by NCBI. If set to true, these will show all
translations of your request (smt), and any errors that were obtained in
the query (sme).
}
  \item{smt}{
These two arguments are for checking that your requests are being
understood correctly by NCBI. If set to true, these will show all
translations of your request (smt), and any errors that were obtained in
the query (sme).
}
}
\details{

Works the same as GetRegion(db="snp") or GetRegion (db="SNP").

Unlike the function GetSNPsInGene, GetSNPsInRegion can consider SNPs within the
flanking regions. Consider using AdjustRangeAroundGene prior to use of
GetSNPsInRegion to do this.

e.g., step A use GetGeneInfo on a gene, eg GetGeneInfo(1203) - one query of NCBI

e.g., step B AdjustRangeAroundGene on those start and stop positions - no query

e.g., step C GetRegion(adjusted boundaries from step B) - one query of NCBI

}
\value{
vector of the matching SNPs (if any).
}
\references{
%% ~put references to the literature/web site here ~
This function, like most of the NCBI2R tools, uses the eutils feature of NCBI

http://www.ncbi.nlm.nih.gov/books/NBK25500

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
\code{\link{GetRegion}}, \code{\link{GetGenesInRegion}}.
}
\examples{
\donttest{
GetSNPsInRegion("13",123456789,167222333) #same as following line
GetRegion(db="snp","13",123456789,167222333)#same as previous line
}
}
