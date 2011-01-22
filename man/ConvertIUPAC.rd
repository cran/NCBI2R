\name{ConvertIUPAC}
\Rdversion{1.2}
\alias{ConvertIUPAC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Convert IUPAC allele ambiguity abbreviation codes
}
\description{
    Convert IUPAC allele ambiguity abbreviation codes into DNA variation and vice versa.
}
\usage{
ConvertIUPAC(variants)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variants}{
One of more of the variants that you wish to translate. See description below.
}
}


\details{
   The IUPAC codes are a way to show DNA SNP variation using only one letter. This function produces a dataframe to enable conversion between the two.
   For example, R represents that both A and G alleles (denoted as A/G) may occur in that position.
   
   The function will automatically detect input (allelic variation codes or IUPAC codes) and output the other form. The function will look for a slash symbol and if it finds the symbol anywhere in the code it will assume all of them are variants. The string must be all alleles or all IUPAC codes.

   Use this to convert easily between single letter codes (eg R or W) and what you might have in other documents (A/G or A/T).
   The allele pairs (eg A/G or A/T) must be in alphabetical order. G/A or T/A will not be recognised. Future versions will fix this.
}
\value{
vector
}
\author{
Scott Melville
}

\section{Warning}{
 When using ConvertIUPAC and providing alleles (for example A/T) as input, the alleles must be in alphabetical order. In other words, do not enter T/A as that is not defined in the IUPAC() table.
 Also the allele code to represent any nucleotide base is A/C/G/T. The letter N is only used in the IUPAC code.
}

\seealso{
\code{\link{IUPAC}}.
}


\examples{
VariationChart<-IUPAC()
ConvertIUPAC(c("T","K","R","W"))
ConvertIUPAC(c("A/C","A/G","A/C/G/T"))
}
