\name{GetARFromLocation}
\Rdversion{1.1}
\alias{GetARFromLocation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Filter your association results.
}
\description{
Allows the user to extract easily a part of the annotated association results from a specified region of the genome, and all results (if any available) are returned. 
}
\usage{
GetARFromLocation(AssociationResults, chr, LowPoint, HighPoint,
                  chromosomecolumn = "chr", positioncolumn = "chrpos")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AssociationResults}{
A dataframe of whole genome association results.
}
  \item{chr}{
The chromosome (as a character), e.g., "4" or "14" or "X".
}
  \item{LowPoint}{
The lowest position of the gene, which might be the stop or the start of the gene depending on the orientation.
}
  \item{HighPoint}{
The highest position of the gene, which might be the stop or the start of the gene depending on the orientation.
}
  \item{chromosomecolumn}{
The name of the column containing the chromosome data.
}
  \item{positioncolumn}{
The name of the column containing the genome position data.
}
}
\details{
A method of selecting a subset of your association results (or someone elses) by a list of SNPs that you specify. These may be SNPs that the literature has already identified. Like ScanForGenes, this function is great for comparing your results to other people's results.
If a particular SNP is not found, then the closest SNP value is returned, along with the distance away from the requested SNP.

This function by itself does not use NCBI - it just queries your association results.

}
\value{
A dataframe.
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
\code{\link{ScanForGenes}}.
}
