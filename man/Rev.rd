\name{Rev}
\Rdversion{1.2}
\alias{Rev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Produces a reverse DNA sequence
}
\description{
  Produces a reverse DNA sequence from a sequence provided
}
\usage{
Rev(DNAseq,ACGT=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DNAseq}{
A single line of DNA sequence that you wish to find the reverse sequence for. Can be used for not just DNA sequences - it will just reverse the orders of the letters in a single string.
}
 \item{ACGT}{
Set to TRUE (default) to remove all non ACGT characters. 
}
}
\details{
Just a sentence or a DNA sequence and use this function to generate a reverse sequence.
}
\value{
vector
}
\author{
Scott Melville
}

\seealso{
\code{\link{Comp}}, \code{\link{RevComp}}.
}
\examples{
Comp("AAAAGGGGTTTTCCA")
Rev("AAAAGGGGTTTTCCA")
RevComp("AAAAGGGGTTTTCCA")
}
