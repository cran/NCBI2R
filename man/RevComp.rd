\name{RevComp}
\Rdversion{1.2}
\alias{RevComp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Produces a reverse complement DNA sequence
}
\description{
  Produces a reverse complement DNA sequence from a sequence provided.
}
\usage{
RevComp(anytext,ACGT=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{anytext}{
A single line of DNA sequence that you wish to find the reverse complement sequence for.
}
 \item{ACGT}{
Set to TRUE (default) to remove all non ACGT characters. If FALSE, the will remain in the string unchanged.
}
}
\details{
Just a sentence or a DNA sequence and use this function to generate a reverse complement sequence. Just calls both the Rev and the Comp functions of the NCBI2R package in turn.
}
\value{
vector
}
\author{
Scott Melville
}

\seealso{
\code{\link{Comp}}, \code{\link{Rev}}.
}
\examples{
\donttest{
Comp("AAAAGGGGTTTTCCA")
Rev("AAAAGGGGTTTTCCA")
RevComp("AAAAGGGGTTTTCCA")
}
}
