\name{GetGeneInfo}
\Rdversion{1.1}
\alias{GetGeneInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains information about genes from NCBI
}
\description{
Obtains information from user specified genes from NCBI's Entrez database.
}
\usage{
GetGeneInfo(locusID, batchsize = 200, xldiv = ";", int = FALSE, go = FALSE,
            showurl = FALSE, quiet = TRUE, sme = FALSE, smt = FALSE,
            div = "---",html=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locusID}{
vector of one of more locusIDs. These are numerical identifiers that NCBI calls EntrezID. Eg the human gene CLN5 is 1203.
}
  \item{batchsize}{
NCBI2R will break up the locusIDs into batch sizes of this specified amount. Each batch is requested and parsed, before a new request is made against the NCBI servers. Depending on the speed of your internet connection, and your processing power, this number may be changed to improve your results.
}
  \item{xldiv}{
What divider does Microsoft Excel use for formulas and depends on regional settings. Normally a semi colon or a comma.
}
  \item{int}{
If set to true, the function will also obtain a list of interacting genes.
}
  \item{go}{
If set to true, the function will also obtain gene ontology genes.
}
  \item{showurl}{
Useful for learning how NCBI2R functions work. It will show the url of the requests using the eutils method.
}
  \item{quiet}{
When set to true, data displayed on screen is at a minimum.
}
  \item{sme}{
These two arguments are for checking that your requests are being understood correctly by NCBI. If set to true, these will show all translations of your request (smt), and any errors that were obtained in the query (sme).
}
  \item{smt}{
These two arguments are for checking that your requests are being understood correctly by NCBI. If set to true, these will show all translations of your request (smt), and any errors that were obtained in the query (sme).
}
  \item{div}{
What visual divider do you wish to have inbetween each pathway/phenotype/neighbouring gene. An extra space may be added by NCBI2R automatically to increase readability.
}
  \item{html}{
Should the function return columns formatted for HTML pages. Useful when using functions such as MakeHTML. Default is TRUE.
}

}
\details{
Input a locusID or several locusIDs and retrieve information about the gene. Such as pathways, phenotypes, gene names, symbol.
Depending on what information you wish to obtain, it might be better to use GetGeneNames as it is faster and returns some similar items (eg genesymbol), but also returns different information.

The approx column that is included in the dataframe, details any approximations that were made while obtaining the results. A value of 1 means that the gene symbol is defined as interim; a value of 2 means that no gene symbol was available, and what is provided comes from one of the synonyms. A value of 3 is both of these messages.
}
\value{
A dataframe.
}

\author{
Scott Melville
}

\section{Warning}{

Some genes that lie on both X and Y chromosomes may give odd results. This is because the gene info can only return one item in the chr column, and it will choose. The cyto band will show both X and Y.

If yopu're manually exploring your data one or two lines of code at a time, you will not have any issue with the NCBI user limits. NCBI2R inserts appropriate pauses most of the time so you can only submit three queries per second. 
If you start running many scripts using NCBI2R, you'll have to pay attention to the limits carefully. These functions use NCBI's eutils, and come with the same user requirements - if
performing many queries, you must run the scripts during certain hours when the
NCBI servers are not in high demand. Please see the package website for more details

http://drop.io/NCBI2R_package

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group.
}
\seealso{
\code{\link{GetSNPInfo}}, \code{\link{GetIDs}}, \code{\link{GetGeneNames}}.

}
\examples{
dd<-GetGeneInfo(1203)  #uses the Entrez identifying number.
dd$OMIM   #contains OMIM record
dd$synonyms  #contains synonyms of the current gene
dd$phenotypes
dd$genesymbol
ss<- GetIDs("CLN5[sym] human")   #returns the Entrez ID number
GetGeneInfo(ss) #will display the information on screen
#or combine both lines together and display the information
GetGeneInfo(GetIDs("CLN5[sym] human"))
genelist<-c(1203,1207,348,777)   
genes<-GetGeneInfo(genelist)
}
