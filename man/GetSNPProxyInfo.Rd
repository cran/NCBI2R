\name{GetSNPProxyInfo}
\Rdversion{1.1}
\alias{GetSNPProxyInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtain proxy SNP information for a given SNP that is in close LD with your requested SNP.
}
\description{
Obtain proxy SNP information for a given SNP that is in close LD with your requested SNP.
}
\usage{
GetSNPProxyInfo(singlesnpname, include = "", exclude = "",
    FlankingDistance = 1e+05,  showurl = FALSE, method = "r2", keepmode = 1,
    build="27_B36", pop="CEU")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{singlesnpname}{
The SNP name that you wish to find proxies for.
}
  \item{include}{
A list of SNPs that you want to keep in the data. It's a filter that runs before the exclusion list. This might be a list of SNPs available to you in your SNP chip of GWA study. 
}
  \item{exclude}{
A list of SNPs that you want to remove from the data. The exclusion is applied after the inclusion list.
}
  \item{FlankingDistance}{
The number of nucleotides on either side that you wish to limit your search to. Maximum value is 1,000,000 because the maximum LD information block is 2,000,000.
}

  \item{showurl}{
Enables the user to see the URL of the NCBI request. 
}
  \item{method}{
Specify the method of choosing the best proxy. Dprime, distance or r2 are the available answers.
}
  \item{keepmode}{
Controls how many proxy SNPs to be returned. See more thorough description below.
}
  \item{build}{
The build that will be queried. Two options available that define the build/release.
3r2_B36 - PhaseIII/Rel 2 February 2009 on NCBI B36 assembly, dbSNP b126
27_B36 - Release 27 Phase II+III February 2009 on NCBI B36 assembly, dbSNP b126
}  

  \item{pop}{
The population to be queried. Default is CEU (Caucasian).
  }

}
\details{
You could use this function when someone has published a result for a particular SNP but you did not examine association in that same exact SNP but may have SNPs that are in linkage disequilibrium (LD) with the published SNP.

The function finds the physical position of the requested SNP, then creates a range to look up based on the FlankingDistance argument and obtains all the LD information.
Then various levels of filters are applied.

The include argument of this function is where you supply a list of SNPs that you wish to keep in your data.
There is no point returning LD information for a SNP that you don't have available to you. You can a list of SNPs on your GWA study or the SNP chip and supply it here.
If left blank (as is the default) no filtering at this step will take place.

The exclude argument of this function is where you supply a list of SNPs that you wish to remove from your data.
Perhaps you have a number of SNPs that failed a quality control step. You can create a list of SNPs on your GWA study or the SNP chip and supply it here.
If left blank (as is the default) no filtering at this step will take place.

All keepmode options are applied after the inclusion/exclusion lists.
Three options available.
1=The LD information will be filtered for all SNPs with the highest value of the method (Dprime, r2, LOD) selected. The SNP that is closest to the requested SNP will be returned.
2=The LD information will be filtered for all SNPs with the highest value of the method (Dprime, r2, LOD) selected. All SNPs that match that criteria will be returned.
3=All SNPs in the region are returned.

The code may take up to thirty seconds for an answer to be returned.
}
\value{
A dataframe
}

\author{
Scott Melville
}

\seealso{
\code{\link{GetLDInfo},\link{GetSNPPosHapMap}.}
}
\examples{
 #Next example returns one line. The proxy will be in column SNPA or
 #  SNPB depending on the linkage data.
a<-GetSNPProxyInfo("rs1234567")
# Next example returns a few lines - all SNPs in that region with
# r2 of 1.0 (r2 was the default method specified)
b<-GetSNPProxyInfo("rs1234567",keepmode=2)
 # Next example returns many many lines - no filtering at all other
 # than the inclusion and exclusion lists
c<-GetSNPProxyInfo("rs1234567",keepmode=3)
}

