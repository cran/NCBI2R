\name{OpenURL}
\Rdversion{1.1}
\alias{OpenURL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Opens a webbrowser to a given address.
}
\description{
Opens a webbrowser to a specified address or addresses
}
\usage{
OpenURL(urls, safety=10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{urls}{
One or more urls that will be opened.
}
  \item{safety}{
This is the upper limit of items that will be opened. The argument
is designed to stop the user accidently opening many items at the
same time.
}

}
\details{
Opens a webbrowser to a specified address or addresses. At this time will only work on Windows and not within linux. Untested on a mac.
}
\value{
No returned value. 
}

\author{
Scott Melville
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning}{
Will work only if you have a web browser set up in your configuration.
}


\seealso{
\code{\link{OpenPMID}}, \code{\link{OpenPDF}}
}
\examples{

## Not run:
## OpenURL("http://www.ncbi.nlm.nih.gov/")
}
