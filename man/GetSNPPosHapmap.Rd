\name{GetSNPPosHapmap}
\Rdversion{1.1}
\alias{GetSNPPosHapmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the chromosomal position of any SNP on the Hapmap.
}
\description{
Find the position of any SNP on the Hapmap.
}
\usage{
GetSNPPosHapmap(singlesnpname, showurl = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{singlesnpname}{
The name of a single SNP to be found. Future builds of NCBI2R will allow multiple items to be queried.
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request. 
}
}
\details{
The position of any SNP on the Hapmap website may not be the latest NCBI build and as such, this function is included here for you to convert SNPs to different genome positions.

The version of the data is PhaseIII/Rel 2 Febuary 2009 on NCBI B36 assembly, dbSNP b126
}
\value{
A list of the chromosome and the chromosomal position is returned.
}
\references{
%% ~put references to the literature/web site here ~
http://hapmap.ncbi.nlm.nih.gov/
}
\author{
Scott Melville
}

\section{Warning}{
The position of any SNP on the Hapmap website may not be the latest NCBI build - The Hapmap data is usually compiled after the NCBI build.
}
\seealso{
\code{\link{GetGeneInfo}}.
}
\examples{
a<-GetSNPPosHapmap("rs1234567")
b<-GetSNPInfo("rs1234567")

}

