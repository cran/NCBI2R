\name{MedlineAbbreviations}
\Rdversion{1.2}
\alias{MedlineAbbreviations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Produces a dataframe of the Medline abbreviations
}
\description{
  Produces a dataframe of the Medline abbreviations for querying.
}
\usage{
MedlineAbbreviations()
}
%- maybe also 'usage' for other objects documented here.
\details{
    The Medline abbreviations are used by PubMed.
    
    These are included as a reference and are also used to convert headings by the
    ConvertPubMedHeadings function included as part of NCBI2R.
}
\value{
dataframe
}
\author{
Scott Melville
}


\seealso{
\code{\link{ConvertPubMedHeadings}}.
}


\examples{
\donttest{
ma<-MedlineAbbreviations()
}
}
