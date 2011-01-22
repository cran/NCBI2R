\name{RemoveSpaces}
\Rdversion{1.2}
\alias{RemoveSpaces}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Removes spaces at the start of a string and the end of a string
}
\description{
  Removes spaces at the start of a string and the end of a string.
}
\usage{
RemoveSpaces(anytext)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{anytext}{
  The string you wish to have the spaces removed from. 
}
}
\details{
Removes only the spaces at the start and end of a string. All internal spaces will remain.
}
\value{
Vector
}
\author{
Scott Melville
}

\examples{
RemoveSpaces("   This is a line of text    ")
}
