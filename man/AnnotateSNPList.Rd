\name{AnnotateSNPList}
\Rdversion{1.1}
\alias{AnnotateSNPList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Annotates SNPs from NCBI
}
\description{
Annotates a list of SNPs using NCBI.
}
\usage{
AnnotateSNPList(snplist, filename = "", hyper = "HYPERLINK", xldiv = ";",
               smt = FALSE, sme = FALSE, div = "---", keeplocusIDs = FALSE,
               keepNS = FALSE,FlankingDistance=100000, kp = TRUE, quiet = TRUE,
               neigh = TRUE,showurl = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{snplist}{
A vector of snps identified by the "rs" at the start of each item.
}
  \item{filename}{
The filename to save the results.
}
  \item{hyper}{
The formula required for Microsoft Excel to open a web address. In English/American versions, it is "HYPERLINK", and this is the default. In Italian, this should be set to "COLLEG.IPERTESTUALE".
}
  \item{xldiv}{
The divider inside a Microsoft Excel formula. Depends on the local settings of your computer.  A comma or semicolon.
}
  \item{smt}{
These two arguments are for checking that your requests are being understood correctly by NCBI. If set to true, these will show all translations of your request (smt), and any errors that were obtained in the query (sme).
}
  \item{sme}{
These two arguments are for checking that your requests are being understood correctly by NCBI. If set to true, these will show all translations of your request (smt), and any errors that were obtained in the query (sme).
}
  \item{div}{
The division symbol that will appear between items within the output.
}
  \item{keeplocusIDs}{
Do you want to keep a list of the locusIDs?
}
  \item{keepNS}{
Do you want to keep the NeighbourString? This is the locusID numbers for the Neighbouring Genes.
}
  \item{FlankingDistance}{
The distance on either side of the specified position for finding NeighGenes.
}
  \item{kp}{
Replaces all mentions of "Kegg Pathway" with "KP" to save space inside the pathways column.
}
  \item{quiet}{
Changes how much information is displayed on the screen. TRUE or FALSE.
}
  \item{neigh}{
Do you want to find out the neighbouring genes.
}
  \item{showurl}{
Useful for learning how NCBI2R functions work.
It will show the url of the requests using the eutils method.
}
}
\details{

If you're looking to produce annotation tables with things like pvalues, I suggest you use AnnotateDataframe instead. You can still use AnnotateSNPFile to create the dataframe, and then manually merge p-values etc on to the created dataframe, and then use MakeHTML or MakeExcel to view the output.

The list of neighbouring genes will include all genes around the SNP in question, and will therefore include any gene that the SNP is located in.

It works the same as AnnotateSNPFile but this function works with an existing R object of SNPs.

The command used to open a URL from Microsoft Excel differs depending on which language the software is in. In English language, the command in Microsoft Excel is "HYPERLINK" and in Italian it is "COLLEG.IPERTESTUALE". This command should be specified at the hyper argument for this command

}
\value{
A dataframe
}

\author{
Scott Melville
}


\section{Warning}{

Please pay careful attention to the column named flag. Any non-zero values indicate a
mapping issue with the marker and this result should be examined carefully.

These functions use NCBI's eutils, and come with the same user requirements - if
performing many queries, you must run the scripts during certain hours when the
NCBI servers are not in high demand. Violation of the terms described there, and the terms on the
eutils website may result in losing access to NCBI for your group.

Please see the package website for more details: http://NCBI2R.wordpress.com

}

\seealso{
\code{\link{AnnotateSNPFile}}, \code{\link{AnnotateDataframe}}
}
\examples{
\donttest{
listofsnps<-c("rs12345","rs734747","rs334","rs735783","rs335")
resultsdf<-AnnotateSNPList(listofsnps,"scottsoutput.html")
}
}

