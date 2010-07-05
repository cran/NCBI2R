\name{GetLDInfo}
\Rdversion{1.1}
\alias{GetLDInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtain Linkage Disequilibrium (LD) information from Hapmap for a given genomic region.
}
\description{
Obtain Linkage Disequilibrium (LD) information from Hapmap for a given genomic region.
}
\usage{
GetLDInfo(chr,pos1,pos2,showurl=FALSE,filter="",build="27_B36")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chr}{
The chromosome of the region you which to obtain LD information for
}
  \item{pos1}{
The lower genomic position (in Hapmap coordinates) of the region you wish to obtain LD information for.
}
  \item{pos2}{
The lower genomic position (in Hapmap coordinates) of the region you wish to obtain LD information for.
}
  \item{showurl}{
specify TRUE if you wish to see the query URL. Useful if you wish to see how NCBI2R works.
}
  \item{filter}{
Applying anything in this variable will allow you to remove all lines of results that do not contain this string anywhere on the line.
}
  \item{build}{
The build to query. Two options available that define the build/release.
3r2_B36 - PhaseIII/Rel 2 February 2009 on NCBI B36 assembly, dbSNP b126
27_B36 - Release 27 Phase II+III February 2009 on NCBI B36 assembly, dbSNP b126
}  

}
\details{
The use of this function will obtain the LD information for a given genomic region. Please understand that the position relates to the queried Hapmap build which may not match to the NCBI build.

At this time, only CEU population is supported, but other populations will be available shortly.

}
\value{
A dataframe.
}
\references{
%% ~put references to the literature/web site here ~
Main hapmap website:
http://hapmap.ncbi.nlm.nih.gov/
}
\author{
Scott Melville
}

\section{Warning}{
the Hapmap website does not always have the same positions as the NCBI builds. Therefore, taking a value from one function, and submitting it to another function must be performed carefully.
}
\seealso{
\code{\link{GetGeneInfo}}.
}
\examples{
ff<-GetLDInfo("4",123456789,123556789)
}

