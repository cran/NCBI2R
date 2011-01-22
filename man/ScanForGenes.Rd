\name{ScanForGenes}
\Rdversion{1.1}
\alias{ScanForGenes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Filters your association results for those found within a list of genes.
}
\description{
Filters your association results for those found within a list of genes.
}
\usage{
ScanForGenes(locusID, AssociationResults, fiveprime = 1e+05,
             threeprime = 2000, fileprefix = "", markercolumn = "name",
             pcolumn = "p",positioncolumn = "position", visualise = TRUE,
             xlab = "", filetype = "PDF", th1 = 0.001, th2 = 0.01,
             moveold = TRUE, org = "human")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locusID}{
one gene, or many genes, using the Entrez NCBI numbering system.
}
  \item{AssociationResults}{
your autosomal results (dataframe)
}
  \item{fiveprime}{
buffer regions on the SNPs
}
  \item{threeprime}{
buffer regions on the SNPs
}
  \item{fileprefix}{
%%     ~~Describe \code{fileprefix} here~~
}
  \item{markercolumn}{
the name of the column containing the SNP marker data
}
  \item{pcolumn}{
Enables user to sort by p or pgc.
}
  \item{positioncolumn}{
the name of the column containing the genome position data
}
  \item{visualise}{
creates graphical output of results with function VisualiseResults
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{filetype}{
type of plots to be created by VisualiseResults. options are PDF and JPEG.
}
  \item{th1}{
significance thresholds. See VisualiseResults for more information.
}
  \item{th2}{
significance thresholds. See VisualiseResults for more information.
}
  \item{moveold}{
move all plots in current "plots" directory into a subfolder called "old".
}
  \item{org}{
organism
}
}
\details{
Looks at a data frame of association results for all SNPs within the
list of candidate genes, eg what the literature has already found. Like
ScanForSNPs this function is great for comparing your results to other
people's results.

User inputs an object of Genes and an object of GWA results, to compare
your results against a list of candidate genes/SNPs from
literature/hypotheses. Using object instead of files, enables
repeats/similar repeats.
}
\references{
This function, like most of the NCBI tools, uses the eutils feature of NCBI

http://eutils.ncbi.nlm.nih.gov/entrez/query/static/eutils_help.html

}
\author{
Scott Melville
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
For a similar function for SNPs, please see \code{\link{ScanForSNPs}}

}
