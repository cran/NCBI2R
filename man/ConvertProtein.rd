\name{ConvertProtein}
\Rdversion{1.2}
\alias{ConvertProtein}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts between different types of amino acid codes.
}
\description{
Converts between different types of abbreviations of amino acid codes.
 
}
\usage{
ConvertProtein(aminoacids,abbrev="aa3",exactstop=FALSE)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{aminoacids}{
A list of the converted amino acids. Many types are accepted. See details below.
}
  \item{abbrev}{
The abbreviation of the output form. Can be aa3 (eg Lys Asn), aa1 (eg K N) or aalong (Lysine Asparagine).
}
  \item{exactstop}{
When FALSE (default) Amber, Ochre, Opal will be converted to a generic Stop.
}


}
\details{

This function will convert between different representations of the same amino acid. For example, Lysine can also be written as K, AAA and Lys.

Input type will be detected automatically. All input items must be the same type.

Valid inputs include:
   three letter codon (e.g., TAG or AAA)
   one letter protein code (e.g., K or M)
   three letter protein code (e.g., Lys or Arg)
   full protein name (e.g., Serine, Tryptophan) using a *capital* letter.

   any of the above with > in between two (e.g., K>M or R>T)
                                        or (e.g., Lysine>Arginine)

The argument exactstop will be fixed at FALSE if single letter codes are used, regardless of what the user enters. This is because a * could be either of the three stop codons.

This function will work with multiple values for the input.

This function does not use NCBI or any internet resources.

}
\value{
vector
}
\author{
Scott Melville
}

\seealso{
\code{\link{AminoAcids}}
}
\examples{

ConvertProtein(c("A","R","T","K","R"),abbrev="aa3")  #aa1 -> aa3
ConvertProtein(c("A","R","T","K","R"),abbrev="aa1")  #aa1 -> aa1 (no change)
ConvertProtein(c("A","R","T","K","R"),abbrev="aalong")  #aa1 -> aalong

ConvertProtein(c("Arg","Ala","Lys"),abbrev="aa1")
ConvertProtein(c("Arg","Ala","Lys"),abbrev="aalong")
ConvertProtein(c("A>T","T>K","M>T"))
ConvertProtein(c("Ala>Thr","Thr>Lys","Met>Thr"),abbrev="aalong")

}
