\name{GetGOs}
\Rdversion{1.1}
\alias{GetGOs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains Gene Ontologies from NCBI.
}
\description{
Obtains Gene Ontologies from the NCBI's Entrez Gene database.
}
\usage{
GetGOs(locusID, showurl = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locusID}{
A single Entrez locus ID number.
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request. 
}
}
\details{
Enter a single locusID number, and obtain a data.frame of the gene ontologies.

Each use of this function represents a single query of NCBI.

}
\value{
A dataframe
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
\code{\link{GetIDs}}.
}
\examples{
\donttest{
GetGOs(1203)

w<-GetGOs(348)
x<-w[w$category=="Component",]
y<-w[grep("Golgi",w$name),]

d<-GetIDs("CLN5[sym] human")
e<-GetGOs(d)
}
}
