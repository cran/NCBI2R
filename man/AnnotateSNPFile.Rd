\name{AnnotateSNPFile}
\Rdversion{1.1}
\alias{AnnotateSNPFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Annotates SNPs from a file
}
\description{
  Performs annotation on a list of SNPs from a file.
}
\usage{
AnnotateSNPFile(snpfile, filename = "output.html", hyper = "HYPERLINK",
               xldiv = ",", quiet = TRUE, smt = FALSE, sme = FALSE,
               keeplocusIDs = FALSE, keepNS = FALSE, FlankingDistance=100000,
               kp = TRUE, div = "---",    neigh = TRUE, showurl = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{snpfile}{
A file containing snps identified by the "rs" at the start of each item. One SNP per line of a plain text document. With or without a heading.
}
  \item{filename}{
A filename to save the results. 
}
  \item{hyper}{
The formula required for Microsoft Excel to open a web address. In English/American versions, it is "HYPERLINK", and this is the default. In Italian, this should be set to "COLLEG.IPERTESTUALE".
}
  \item{xldiv}{
The divider inside a Microsoft Excel formula. Depends on the local settings of your computer.  A comma or semicolon.
}
  \item{quiet}{
Changes how much information is displayed on the screen. TRUE or FALSE.
}
  \item{smt}{
These two arguments are for checking that your requests are being understood correctly by NCBI. If set to true, these will show all translations of your request (smt), and any errors that were obtained in the query (sme).
}
  \item{sme}{
These two arguments are for checking that your requests are being understood correctly by NCBI. If set to true, these will show all translations of your request (smt), and any errors that were obtained in the query (sme).
}
  \item{keeplocusIDs}{
Do you want to keep a list of the locusIDs?
}
  \item{keepNS}{
Do you want to keep a string of the Neighbouring Genes.
}
  \item{FlankingDistance}{
The distance on either side of the specified position for finding NeighGenes.
}
  \item{kp}{
Replaces all mentions of "Kegg Pathway" with "KP" to save space inside the pathways.
}
  \item{div}{
The division symbol that will appear between items within the output.
}
  \item{neigh}{
If set to True, will obtain information about neighbouring genes.
}
  \item{showurl}{
Useful for learning how NCBI2R functions work. It will show the url of the requests using the eutils method.
}
}
\details{

The list of neighbouring genes will include all genes around the SNP in question, and will therefore include any gene that the SNP is located in.

}
\value{
A dataframe
}
\author{
Scott Melville
}

\section{Warning}{
These functions use NCBI's eutils, and come with the same user requirements - if
performing many queries, you must run the scripts during certain hours when the
NCBI servers are not in high demand. Please see the package website for more details

http://NCBI2R.wordpress.com

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group.
}
\seealso{
\code{\link{AnnotateSNPList}}, \code{\link{AnnotateDataframe}}.
}
\examples{
\donttest{
resultsdf<-AnnotateSNPFile("markers.txt","scottsoutput.html")
}
}
