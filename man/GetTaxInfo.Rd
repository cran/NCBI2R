\name{GetTaxInfo}
\Rdversion{1.1}
\alias{GetTaxInfo}

\title{
Get Taxonomy information
}
\description{
Get Taxonomy information for supplied id numbers
}
\usage{
GetTaxInfo(taxids,batchsize=200,showurl=FALSE)
}

\arguments{
  \item{taxids}{
One of more taxonomy id numbers.
}
 \item{batchsize}{
Number of SNPs that will get grouped together into one URL request.
Depending on internet connection speed and processing power, changing
this value might lead to faster analysis.
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request.
}
}
\details{

Obtains information about an item in the taxonomy database of NCBI. In each of
the three dataframes, data will include the requested taxids and an additional
column that contains the latest taxid for those requests.

#   return(list(namesVarious=df1,lineage=df3,names=df2))
The first dataframe is all the available names for each of the taxonomy ids,
including synonyms, scientific names and common names where available.

The second dataframe shows the lineage for each of the taxonomy ids. Each query
will produce a number of rows for kingdom, family, order, suborder, genus, etc,
where available.

The third and final dataframe contains one row of name information per taxid
request. Also included is one column with compressed lineage information which
is identifical to the lineage information of the second dataframe, except for
the format.


}
\value{
Returns a list of three dataframes. The first will be the lineages that the two
terms have in common. The second and third dataframes are the lineages where
the items diverge.
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
result in losing access to NCBI for your group. But remember, the average light
user should have no problems at all. NCBI2R inserts small delays into most of
the code - it's should only be if you are performing many queries in a short
space of time.
}


\seealso{
\code{\link{GetTax}}, \code{\link{GetTaxInfo}}.
}
\examples{
g1<-GetTax("puffinus")
g2<-GetTaxInfo(g1)
print(g2)

}
