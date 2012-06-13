\name{ConvertPubMedHeadings}
\Rdversion{1.1}
\alias{ConvertPubMedHeadings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert MEDLINE abbreviations to descriptions
}
\description{
Convert MEDLINE abbreviations to descriptions.
}
\usage{
ConvertPubMedHeadings(paper.df,reverse=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{paper.df}{
A dataframe of papers as obtained by GetPubMed
}
  \item{reverse}{
Default is FALSE, but you can use this argument to take the full headings and turn them back into MEDLINE abbreviations.
}



}
\details{
When GetPubMed performs a literature search for you, the default option is to
use the medline abbreviations as headings, which are often hard to understand.
Author(AU), Gene Symbol(GS) and Investigator Name(IR) are three examples.

There is an argument within the function GetPubMed that if specified will run
this function automatically. Alternatively, you can run this function by itself
later.

}
\value{
A dataframe of all of the papers you entered, just with more descriptive 
headings
}

\author{
Scott Melville
}

\seealso{
\code{\link{GetPubMed}}, \code{\link{MedlineAbbreviations}}.
}

\examples{
\donttest{
a <- GetPubMed("CLN5","My references.tab")
b <- ConvertPubMedHeadings(a)
c <- GetPubMed("CLN5",descHead=TRUE)
}
}