\name{SplitGenes}
\Rdversion{1.1}
\alias{SplitGenes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Checks and converts SNP information for multiple genes.
}
\description{
  If a SNP is found in multiple genes, will divide the NCBI2R dataframe into individual entries for each SNP.
}
\usage{
SplitGenes(snpdf,quiet=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{snpdf}{
     A dataframe of results from the function GetSNPInfo.
}
  \item{quiet}{
     Controls if SNPs that are found in multiple genes will be displayed on screen.
}

}
\details{

Please see the warnings below.

Some SNPs are found in multiple genes and this will result in more than one
genesymbol or locusID. Multiple entries will cause problems in some subsequent
functions and therefore they need to be "split". The split procedure will divide
the single entry for a SNP over multiple rows where each row contains only one
locusID and one genesymbol. However, the function class cannot be divided.

}
\value{
A dataframe
}

\author{
  Scott Melville
}


\section{Warning}{
fxn_class will not be divided properly. This is due to the way that NCBI encodes the data that is queried. Future methods of NCBI2R will rectify the situation.
}


\seealso{
\code{\link{GetSNPInfo}}.
}
\examples{
snplist<-c("rs12345","rs333","rs1003483")
mySNPs<-GetSNPInfo(snplist)
splitSNPs<-SplitGenes(mySNPs)
}
