\name{GetSNPFlankSeq}
\Rdversion{1.1}
\alias{GetSNPFlankSeq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains flanking sequence of a set of SNPs from NCBI
}
\description{
Obtains flanking sequence of a set of SNPs from NCBI
}
\usage{
GetSNPFlankSeq(listofSNPs, batchsize = 200, showurl = FALSE)
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
}
\details{
Obtains the flanking position and the variation code of a set of SNPs
from NCBI. The variation code used is the IUPAC system eg where R is 
used to indicate A or G alleles. 

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

These functions use NCBI's eutils, and come with the same user requirements - if
performing many queries, you must run the scripts during certain hours when the
NCBI servers are not in high demand. Please see the package website for more details

http://NCBI2R.wordpress.com

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group.
}

\seealso{
\code{\link{IUPAC}}.
}
\examples{
snplist<-c("rs94","rs334","rs309")
mySNPs<-GetSNPFlankSeq(snplist)
}