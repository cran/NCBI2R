\name{AdjustRangeAroundGene}
\Rdversion{1.1}
\alias{AdjustRangeAroundGene}
\title{
Adjusts gene position
}
\description{
Adjusts gene position to include flanking regions for use in other NCBI2R functions.
}
\usage{
AdjustRangeAroundGene(LowPoint, HighPoint, orientation, upstream = 10000,
                    downstream = 2000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LowPoint}{
The lowest position of the gene, which might be the stop or the start of the gene depending on the orientation.
}
  \item{HighPoint}{
The highest position of the gene, which might be the stop or the start of the gene depending on the orientation.
}
  \item{orientation}{
To specify the orientation of the gene. Use "+" or "-". 
}
  \item{upstream}{
The number of nucleotides you wish to include upstream (before the start of the gene).
}
  \item{downstream}{
The number of nucleotides you wish to include downstream (after the end of the gene).
}
}
\details{
Takes a physical location, orientation, and recalculates the region of interest by considering the flanking regions on either side - e.g. promoter/5'UTR/3'UTR.
It is used because looking at SNPs within a gene, may be quite limiting and the interesting SNPs might be those located outside the gene sequence, such as upstream or downstream regulatory elements. This function adjusts the region by allowing the user to specify how much on the upstream and downstream "ends" to consider the flanking regions, but defaults are included.

This function does not use NCBI at all - it's just does calculations.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{LowPoint }{The adjusted boundary position of the gene.}
  \item{HighPoint }{The adjusted boundary position of the gene.}
%% ...
}
\author{
Scott Melville
}
\note{
%%  ~~further notes~~
The gene positions in NCBI2R functions are usually known as LowPoint and
HighPoint and you'll need to consider the orientation of the gene (also available)
to determine which of the two returned positions is the gene start position, and 
which is the gene stop position.
}

\seealso{
\code{\link{GetSNPInfo}}.
}
\examples{
\donttest{
locusID<-GetIDs("AKR1C4[SYM] human")
ss<-GetGeneInfo(locusID)
dd<-AdjustRangeAroundGene(ss$GeneLowPoint,ss$GeneHighPoint,ss$ori,12000,0)
data(gwas)  #load sample gwas data and then filter based on the adjusted range around gene
xx<-GetARFromLocation(gwas,ss$chr,dd$LowPoint,dd$HighPoint)
}
}
