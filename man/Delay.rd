\name{Delay}
\Rdversion{1.1}
\alias{Delay}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pauses the code for N seconds
}
\description{
Pauses the code for N seconds.
}
\usage{
Delay(seconds)}

\arguments{
  \item{seconds}{
The number of seconds that you wish to have the code sleep for. A built in pause.
}
}
\details{
If you have to run multiple large analyses using NCBI2R, you can insert a delay
so you don't overload the system. The average user will not have any problems 
with the NCBI limits - the NCBI2R code takes into account most of the delays for
you.

}
\value{
No output.
}

\author{
Scott Melville
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
Delay(2) #sleeps for two seconds
}
