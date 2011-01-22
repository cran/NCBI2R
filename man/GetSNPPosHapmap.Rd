\name{GetSNPPosHapMap}
\Rdversion{1.1}
\alias{GetSNPPosHapMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the chromosomal position of any SNP on the HapMap.
}
\description{
Find the position of any SNP on the HapMap.
}
\usage{
GetSNPPosHapMap(singlesnpname, showurl = FALSE,build="27_B36",rsOnly=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{singlesnpname}{
The name of a single SNP to be found. Future builds of NCBI2R will allow multiple items to be queried.
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request. 
}
  \item{build}{
The build to query. Two options available that define the build/release.
3r2_B36 - PhaseIII/Rel 2 February 2009 on NCBI B36 assembly, dbSNP b126
27_B36 - Release 27 Phase II+III February 2009 on NCBI B36 assembly, dbSNP b126
}  
  \item{rsOnly}{
Should only rsOnly SNPs be queried? At the time this was written, HapMap can only search for SNPs with rs identifiers, but this feature was added as a checking system.  
}  

}
\details{
The position of a SNP on the HapMap website may not be the latest NCBI build and as such, this function is included here for you to convert SNPs to different genome positions.

The version of the data is PhaseIII/Rel 2 Febuary 2009 on NCBI B36 assembly, dbSNP b126

Each use of this function represents a single query of NCBI.
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
The position of any SNP on the HapMap website may not be the latest NCBI build - The Hapmap data is usually compiled after the NCBI build.
}
\seealso{
\code{\link{GetGeneInfo}}.
}
\examples{
a<-GetSNPPosHapMap("rs1234567")
b<-GetSNPInfo("rs1234567")

}

