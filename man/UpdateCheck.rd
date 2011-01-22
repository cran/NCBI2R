\name{UpdateCheck}
\Rdversion{1.1}
\alias{UpdateCheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checks for a more recent version of NCBI2R
}
\description{
Checks for a more recent version of NCBI2R.
}
\usage{
UpdateCheck()
}
%- maybe also 'usage' for other objects documented here.
\details{

Used to check if there is a more recent version. Like most NCBI2R functions,
the internet is required.

This function will be called at the start of a session only after an NCBI2R
function obtains information from NCBI. This check will be performed once per
session and can be turned off by setting .ncbi2r.options$vS to a value of
"no check". After an UpdateCheck, .ncbi2r.options$vS will contain a string of
characters that indicates if your version is up to date or not.

This function will also be called as part of the function PrintNCBI2RInfo().

This function is no longer (since build 1.3.4) initiated when the package is
first loaded into the R workspace, but will be performed once data is downloaded
from NCBI as described above.
}
\value{
Vector
}

\author{
Scott Melville
}
