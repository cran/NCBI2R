\name{SplitGenes}
\Rdversion{1.1}
\alias{SplitGenes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Checks SNP information for multiple genes.
}
\description{
  If a SNP is found in multiple genes, will divide the NCBI2R dataframe into individual entries for each SNP
}
\usage{
SplitGenes(snpdf,quiet=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{snpdf}{
     A dataframe of results from the function NCBI2R
}
  \item{quiet}{
     Controls if SNPs that are found in multiple genes will be displayed on screen.
}

}
\details{
After you run GetSNPInfo, particular SNPs may be found in multiple genes. Therefore a cell may contain more than one locusID or gene symbol and it should be split.
 
By running SplitGenes, the data frame will be modified to contain the SNP multiple times - one entry per gene. See the example.
}
\value{
A dataframe
}

\author{
  Scott Melville
}


\section{Warning}{
fxn_class will not be divided properly
}


\seealso{
\code{\link{GetSNPInfo}}.
}
\examples{
snplist<-c("rs12345","rs333","rs624662")
mySNPs<-GetSNPInfo(snplist)
splitSNPs<-SplitGenes(mySNPs)
}
