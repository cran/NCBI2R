\name{GetGeneTable}
\Rdversion{1.1}
\alias{GetGeneTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get intron/exon data from NCBI
}
\description{
Obtains the introns, exons, and coding exons of a specified gene using NCBI.
}
\usage{
GetGeneTable(locusIDs, batchsize=200, showurl = FALSE, pbar=TRUE, TrueBP=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locusIDs}{
A single Entrez locusID.
}
  \item{batchsize}{
The number of ids that are sent to NCBI for processing in each batch. Faster results may be achieved by changing this number depending on internet/CPU speeds. 
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request. 
}
  \item{pbar}{
 Show a progress bar or not. The default is TRUE.
}
  \item{TrueBP}{
 Adjusts the exon info to generate the genomic positions of each exon, rather than the relative positions.
}



}
\details{
Input one or more locusIDs and retrieve the sizes of the introns and exons of all transcripts available.  The function returns the relative positions of exons and introns in respect to the genes, and not an absolute value in respect to the chromosome. i.e. the first exon will start at position 1. A future release of the NCBI2R functions will contain an option to obtain the absolute position.

Three dataframes are returned. The first is all the positions with "Set" standing for each transcript. So you can easily compare different transcripts or select a particular transcript. Two other dataframes store the accession identifiers for DNA and Protein sequences.

Each batch of this function represents a single query of NCBI.

}
\value{

 \item{ExonInfo}{The positions of each of the exons.}
 \item{ACC.DNA}{The DNA accession numbers of the transcripts.}
 \item{ACC.Prot}{The Prot accession numbers of the transcripts.}

}
\references{
%% ~put references to the literature/web site here ~

http://www.ncbi.nlm.nih.gov/sites/entrez?db=gene

This function, like most of the NCBI2R tools uses the eutils feature of NCBI

http://eutils.ncbi.nlm.nih.gov/entrez/query/static/eutils_help.html
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
\code{\link{GetGeneInfo}}.
}
\examples{
a<-GetGeneTable(348)
b<-GetGeneTable(1201)
}
