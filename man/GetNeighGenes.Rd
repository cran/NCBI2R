\name{GetNeighGenes}
\Rdversion{1.1}
\alias{GetNeighGenes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains neighbouring genes from NCBI
}
\description{
Obtains a list of nearby genes from a given genome position using NCBI.
}
\usage{
GetNeighGenes(chr, chrpos, FlankingDistance = 1e+05, showurl = FALSE,
              hyper = "HYPERLINK", full = FALSE, web=TRUE, html=FALSE, org = "human", cg = TRUE, div = "**",
              sme = FALSE, smt = FALSE, pbar=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chr}{
the chromosome of the center of the region that you wish to find out the neighbouring genes of.
}
  \item{chrpos}{
The position in the genome that you wish to find the nearby genes for.
}
  \item{FlankingDistance}{
The distance on either side of the specified position to include.
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request. 
}
  \item{hyper}{
The name of the Microsoft Excel command to link to a URL. Intended for future development of codes where XL is built as well (will require xldiv as well)
}
  \item{full}{
Will set the other arguments of web and html to TRUE. It will override the other settings.
}
  \item{web}{
default is true. will create a URL that can be used to show the region in question.
}
  \item{html}{
Will create a string for a html file of the names of the genes, with a dynamic link to the NCBI locus file
}
  \item{org}{
The name of the organism to use.
}
  \item{cg}{
If set to true, data will return only current genes, otherwise discontinued genes will also be returned.
}
  \item{div}{
The dividing symbol that will appear between items within the output. Used to make it easier to read.
}
  \item{sme}{
These two arguments are for checking that your requests are being understood correctly by NCBI. If set to true, these will show all translations of your request (smt), and any errors that were obtained in the query (sme).
}
  \item{smt}{
These two arguments are for checking that your requests are being understood correctly by NCBI. If set to true, these will show all translations of your request (smt), and any errors that were obtained in the query (sme).
}
  \item{pbar}{
Show progress bar or not. Default is TRUE.
}



}
\details{
Obtains a list of nearby genes from a given genome position.

The list of neighbouring genes will include all genes around the SNP in question, and will therefore include any gene that the SNP is located in.

Works on any species mapped on NCBI, like most of the functions in NCBI2R.
Instead of chromosome and position, you can now specify a dataframe and the function will look for columns called "chr" and "chrpos", and GetNeighbours will work on all items.
You can spell this function as GetNeighbours or GetNeighbors.
To open a URL from Microsoft Excel uses a specific command, depending on which language the software is in. In English language, the command in Microsoft Excel is "HYPERLINK" and in Italian it is "COLLEG.IPERTESTUALE". This command should be specified at the hyper argument for this command.

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
\code{\link{GetRegion}, \link{DivideString}.}
}
\examples{


a<-GetNeighGenes("4",123456789,100000)  #Looks at the genes within 100 kbp on either side of the location on chromosome four, a total of 200 kbp
b<-GetNeighGenes("4",123456789,100000,web=FALSE)
c<-GetNeighGenes("4",123456789,100000,full=TRUE)
ff<-as.data.frame(cbind(c("4","6","10"),c("123456789","222333444","100666777")))
g<-GetNeighGenes(ff,html=TRUE)
loci<-DivideString(g$locusID,sep=",") #divide the string of loci into a vector of each component

}
