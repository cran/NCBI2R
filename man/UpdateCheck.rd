\name{UpdateCheck}
\Rdversion{1.1}
\alias{UpdateCheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checks if there is a more recent version of NCBI2R
}
\description{
Checks if there is a more recent version of NCBI2R
}
\usage{
UpdateCheck()
}
%- maybe also 'usage' for other objects documented here.
\details{
Used to check if there is a more recent version. Like most NCBI2R functions,
the internet is required.

Will be called when the function PrintNCBI2RInfo() is used.

This function is no longer (since build 1.3.4) initiated on start up of the
package but it is highly recommended you use this function regularly or
view the package website http://ncbi2r.wordpress.com
}
\value{
Vector
}

\author{
Scott Melville
}
