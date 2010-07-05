\name{VisualiseRegion}
\Rdversion{1.1}
\alias{VisualiseRegion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Produces a graphical representation of the region, with the exons (optional).
}
\description{
Produces a graphical representation of the region, with the exons (optional).
}
\usage{
VisualiseRegion(snpnames, snppositions, pvalues, StartFlank, StopFlank,
                GeneLowPoint, title = paste(StartFlank, "", "StopFlank"),
                xlab = "",filetype = "PDF", th1 = 1e-06, th2 = 1e-04,
                locusID = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{snpnames}{
The data to be plotted.
}
  \item{snppositions}{
The data to be plotted.
}
  \item{pvalues}{
The data to be plotted.
}
  \item{StartFlank}{
The lowest position (in numerical terms) of the region to be plotted.
}
  \item{StopFlank}{
The highest position (in numerical terms) of the region to be plotted.
}
  \item{GeneLowPoint}{
The lowest nucleotide position of the edges of the two genes.
Either the start or the stop of the gene depending on the orientation.
}
  \item{title}{
The title for the plot.
}
  \item{xlab}{
The label for the x axis.
}
  \item{filetype}{
Type of plots to be created. Available options are PDF and JPEG.
You need to make sure you can write to the directory.
}
  \item{th1}{
High significance threshold. Each plot has a graphical bar, and it will be coloured depending on the significance values you specify here. Red for highly significant, yellow if the pvalue is between the two thresholds, or green if less than both.
}
  \item{th2}{
Low significance thresholds. Each plot has a graphical bar, and it will be coloured depending on the significance values you specify here. Red for highly significant, yellow if the pvalue is between the two thresholds, or green if less than both.
}
  \item{locusID}{
The NCBI Entrez locusID of the gene to plot.
}
}
\details{
This is mainly an internal function called by ScanForGenes, but it may be used by itself.

More details coming soon.
}
\value{
A plot is saved.
}

\author{
Scott Melville
}
