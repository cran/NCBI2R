\name{AnnotateDataframe}
\Rdversion{1.1}
\alias{AnnotateDataframe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Uses NCBI to annotate dataframe 
}
\description{
  Annotates a dataframe of association results with information from NCBI.
}
\usage{
AnnotateDataframe(anydf, selections = "", filename = "", hyper = "HYPERLINK",
                  xldiv = ";", smt = FALSE, sme = FALSE, div = "---",
                  keeplocusIDs = FALSE, keepNS = FALSE, kp = TRUE, quiet = TRUE,
                  neigh = TRUE, showurl = FALSE, FlankingDistance=100000, 
                  suppressColCheck=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{anydf}{
     A dataframe of results, containing one column of SNPs and other columns of data that will be included in the output. The other columns are not relevant for annotation.
}
  \item{selections}{
     Which items from the dataframe of results that you would like displayed with the annotation. The first item must be the list of SNPs.
}
  \item{filename}{
     A filename to save the results as HTML.
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
     Do you wish to keep a string of locusIDs for the neighbouring genes?
}
  \item{kp}{
     Replaces all mentions of "Kegg Pathway" with "KP" to save space inside the pathways.
}
  \item{quiet}{
     Changes how much information is displayed on the screen. TRUE or FALSE.
}
  \item{neigh}{
    Do you want to find out the neighbouring genes?
}
  \item{showurl}{
     	Displays the URL of the requests. Useful for learning how the functions work, and how you might wish to write your own functions for querying NCBI.
}
  \item{FlankingDistance}{
The distance on either side of the specified position for finding NeighGenes.
}
  \item{suppressColCheck}{
Used to turn off the message that informs the user when only one column is returned.
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

Please pay careful attention to the column named flag. This indicates there was a mapping issue
with the marker and this result should be examined carefully.

These functions use NCBI's eutils, and come with the same user requirements - if
performing many queries, you must run the scripts during certain hours when the
NCBI servers are not in high demand. Please see the package website for more details

http://NCBI2R.wordpress.com

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group.
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{GetGeneInfo}}, \code{\link{GetSNPInfo}}
}
\examples{
\donttest{
data(gwas) #load in sample data
MyResults<-gwas[gwas$chr==22 & gwas$chrpos>=2000000 & gwas$chrpos<=25001000,]
#The selections argument MUST contain the column with rsIDs FIRST
#Then you can add whatever columns you want that are found in your own data, eg p, OR, beta etc
#These columns (e.g. the p column in the sample data) are not used in any way for the annotation
#but are returned in the response.
j<-AnnotateDataframe(MyResults,selections = c("marker","p"),filename="Output.html")
}
}
