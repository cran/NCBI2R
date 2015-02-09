\name{GetInteractions}
\Rdversion{1.1}
\alias{GetInteractions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains interacting genes from NCBI
}
\description{
Obtains the genes known to interact with the specified gene, from the NCBI's Entrez Gene database.
}
\usage{
GetInteractions(locusID, showurl = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locusID}{
A single Entrez locus ID.
}

  \item{showurl}{
Enables the user to see the URL of the NCBI request.
}
}
\details{
Works on a single EntrezID (locusID) number and provides a dataframe of results of the sources of information, the other gene name, other gene locusID number and additional items, such as sequence accession identifiers (eg NP_000518.1).

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
\examples{
\donttest{
GetInteractions(777)
}
}
