\name{ScanForSNPs}
\Rdversion{1.1}
\alias{ScanForSNPs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Filters your association results for those that match a list of SNPs.
}
\description{
Filters your association results for those that match a list of
candidate SNPs.
}
\usage{
ScanForSNPs(snplist, AssociationResults, markercolumn = "name",
            chromosomecolumn = "chr", positioncolumn = "position")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{snplist}{
the list of candidate SNPs to find in your own results.
}
  \item{AssociationResults}{
The name of your association results (dataframe).
}
  \item{markercolumn}{
The column name of your SNPs in the association results.
}
  \item{chromosomecolumn}{
The name of the column in the dataframe containing the chromosome
eg "2","13","mt","X".
}
  \item{positioncolumn}{
The name of the column in the dataframe containing the genome position data.
}
}
\details{
Looks at a data frame of association results for a list of SNPs, eg what
the literature has already found. Like ScanForGenes, this function is
great for comparing your results to other people's results.

Returns the results of your own results for a list of SNPs. If a
particular SNP is not found, then the closest SNP value is returned,
along with the distance away from the requested SNP.
}
\value{
A dataframe.
}

\author{
Scott Melville
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ScanForGenes}}
}
