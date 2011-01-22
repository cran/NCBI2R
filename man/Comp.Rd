\name{Comp}
\Rdversion{1.2}
\alias{Comp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Produces a complementary DNA sequence
}
\description{
  Produces a complementary DNA sequence.
}
\usage{
Comp(DNAseq,ACGT=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DNAseq}{
A single line of DNA sequence that you wish to find the complementary sequence for. A will be switched for T, C for G, G for C, T for A. All other characters will remain.
}
   \item{ACGT}{
Set to TRUE to remove all non ACGT characters.    
}   
}
\details{
Just input your sequence and use this function to generate a complementary sequence.
}
\value{
vector
}
\author{
Scott Melville
}

\section{Warning}{
If ACGT=FALSE, all characters that are not ACGT will remain in the string and be ignored.

}
\seealso{
\code{\link{Rev}}, \code{\link{RevComp}}.
}
\examples{
Comp("AAAAGGGGTTTTCCA")
Rev("AAAAGGGGTTTTCCA")
RevComp("AAAAGGGGTTTTCCA")
}
