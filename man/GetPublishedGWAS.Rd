\name{GetPublishedGWAS}
\Rdversion{1.1}
\alias{GetPublishedGWAS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains published GWAS association values
}
\description{
Obtains published GWAS association values from genome.gov
}
\usage{
GetPublishedGWAS(keycol="",term="")
}

\arguments{

  \item{keycol}{
  The name of the column to filter on.
  }
  
\item{term}{
  The term (matching only to the keycol) to filter the results on.
  }

}
\details{
Obtains the txt file from genome.gov of published association values.

Only if the keycol value is filled in, then the search term will be used
and the result will be filtered by the term.

For the full available results leave the both the term and keycol blank.

To find the names of columns that you could filter on follow the example below.

}
\value{
A dataframe.
}
\references{
%% ~put references to the literature/web site here ~
The file that is downloaded is
http://www.genome.gov/admin/gwascatalog.txt
}
\author{
Scott Melville
}

\section{Warning}{
Any values returned by this function (that are in the spreadsheet) should always
be checked in the literature. If a paper gets corrected, there may be some delay in secondary
sources of information such as this table.
}

\examples{
\donttest{
bb<-GetPublishedGWAS()
print(names(bb))
}
}

