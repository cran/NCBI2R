\name{AminoAcids}
\Rdversion{1.2}
\alias{AminoAcids}
\title{
  Reference table of amino acids
}
\description{
  Reference table of amino acids.
}
\usage{
AminoAcids()
}

\details{
This is a reference table of the amino acids. It can be used to convert between amino acid names and corresponding codons, and abbreviations.
}
\value{
dataframe
}
\author{
Scott Melville
}

\section{Note}{
This function does not use NCBI or require the internet. The reference table is stored within this function.
}


\seealso{
\code{\link{ConvertProtein}}.
}
\examples{

ref.data<-AminoAcids()
stop.codons<-ref.data[ref.data$aa1=="*","codon"]
lysine.abbreviation<-ref.data[ref.data$aalong=="Lysine","aa3"][1]
writeLines(lysine.abbreviation)
}
