\name{OpenPMID}
\Rdversion{1.1}
\alias{OpenPMID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Opens the PubMed webpage for a given PMID
}
\description{
Opens the PubMed webpage for a given PubMed ID (journal article). This allows a
user to select viewing options for journals that are not freely available.

}
\usage{
OpenPMID(PMID, safety=10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PMID}{
the PubMed ID number of the paper
}
  \item{safety}{
This is the upper limit of items that will be opened. Designed to stop
the user accidently opening many items at the same time.
}
}
\details{
Opens a given NCBI PubMed ID number in a webbrowser.
}
\value{
No returned value. Webbrowser opened up on corresponding page(s).
}
\references{
%% ~put references to the literature/web site here ~

PubMed website:
http://www.ncbi.nlm.nih.gov/sites/entrez?db=PubMed

Like most other NCBI2R tools, this function uses eutils:

http://eutils.ncbi.nlm.nih.gov/entrez/query/static/eutils_help.html
}
\author{
Scott Melville
}
\note{
The NCBI PubMed ID number is different to the PubMed Central ID number.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning}{
Only works on computers with a webbrowser set up.

}

\examples{
\donttest{
 OpenPMID(16033706)
 a<-GetPubMed("canine CLN5","My references.tab")
 OpenPMID(a$PMID[1])
 }
}
