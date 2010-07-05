\name{IUPAC}
\Rdversion{1.2}
\alias{IUPAC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Produces a dataframe of the IUPAC DNA allele ambiguity abbreviation codes
}
\description{
  Produces a dataframe of the IUPAC SNP variation allele ambiguity abbreviation codes for looking up
}
\usage{
IUPAC()
}
%- maybe also 'usage' for other objects documented here.
\details{
   The IUPAC codes are a way to show DNA SNP variation using only one letter. This function produces a dataframe to enable conversion between the two.
   For example, R represents that both A and G alleles (denoted as A/G) may occur in that position.
  
   
}
\value{
dataframe
}
\author{
Scott Melville
}

\section{Warning}{
 When using TranslateIUPAC and providing alleles (for example A/T) as input, the alleles must be in alphabetical order. In other words, do not enter T/A as that is not defined in the IUPAC() table.
 Also the allele code to represent any nucleotide base is A/C/G/T. The letter N is only used in the IUPAC code.
}

\seealso{
\code{\link{TranslateIUPAC}}.
}


\examples{
VariationChart<-IUPAC()
TranslateIUPAC(c("T","K","R","W"))
TranslateIUPAC(c("A/C","A/G","A/C/G/T"))
}
