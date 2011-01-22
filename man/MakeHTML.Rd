\name{MakeHTML}
\Rdversion{1.1}
\alias{MakeHTML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts genetic annotation dataframes into a HTML document.
}
\description{
Converts genetic annotation dataframes into a HTML document.
}
\usage{
MakeHTML(anydf, filename, compress = TRUE, markercolumn = "marker",
         keeplocusIDs = FALSE, keepNS = FALSE, kp = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{anydf}{
A dataframe of results (with or without annotation).
}
  \item{filename}{
The filename to save the HTML data.
}
  \item{compress}{
If similar data is found in adjacent rows, the output table will be
compressed.
}
  \item{markercolumn}{
The name of the column within the dataframe that contains the SNP markers.
}
  \item{keeplocusIDs}{
Do you want a column of the locusID or not for any genes that the SNP is
found in? Default is FALSE.
}
  \item{keepNS}{
Do you want to keep the NeighbourString? This is the locusID numbers for
the Neighbouring Genes.
}
  \item{kp}{
Replaces all mentions of "Kegg Pathway" with "KP" to save space inside
the pathways.
}
}
\details{
MakeHTML takes the rather large dataframe of SNP/Gene information and
merges it. For example, the weblink to the locusID is not displayed on
the HTML, but clickable links is created on the gene names. Output is
normally created as R objects, but this function will convert the object
into the output files that can be used by people not familiar with R.

This function will work on many different types of dataframes in
NCBI2R. This includes, the results of a PubMed search, the annotation
results, or the results of GetGeneInfo commands. See the examples below.

The default HTML file generated can be quite large. You should consider
which columns you really need for your output, and remove those you
will not use.

The compress argument for this function works best if the dataframe
is sorted by chromosome and chromosome position before generating
the page.

Depending on the webbrowser that you use, you can shrink the size
of the font displayed by pressing (Alt with -) and expand the text
by pressing (Alt with +). This may make things easier to read if the formatting
is not easy to read.

In some functions, such as AnnotateSNPList, AnnotateSNPFile and
AnnotateDataframe, the MakeHTML is automatically created when an output
file name is provided by the user.
}
\value{
A HTML file is created and saved for use in a webbrowser.
}

\author{
Scott Melville
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{MakeExcel}}.
}
\examples{
snplist<-c("rs1234567","rs333","rs848484")
d<-AnnotateSNPList(snplist)  #no HTML file is created
MakeHTML(d,"NCBI2R_AnnotatedSNPs.html")
\donttest{
#these are additional examples that can be used but won't be checked during package testing
snps<-GetSNPInfo(snplist)
MakeHTML(snps,"NCBI2R_JustSNPs.html")

bb<-GetGeneInfo(1200:1209)
MakeHTML(bb,"NCBI2R_GeneInformation.html")
}
}