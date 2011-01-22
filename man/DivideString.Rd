\name{DivideString}
\Rdversion{1.2}
\alias{DivideString}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Converts a string into fragment pieces based on a comma or other separator
}
\description{
  Converts a string into fragment pieces based on a comma or other separator.
}
\usage{
DivideString(anytext,sep=",")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{anytext}{
A line of locusIDs that contain a comma, or any other text that you wish to divide into vector components.
}
   \item{sep}{
The separator you wish to divide the string by.
}   
}
\details{
This makes it easy to convert a string of locusIDs into a vector of each individual item.

This function is extremely similar to the R command strsplit but outputs a vector instead of a list.
}
\value{
vector
}
\author{
Scott Melville
}

\examples{
DivideString("1234,5678,145251,6136",sep=",")
}
