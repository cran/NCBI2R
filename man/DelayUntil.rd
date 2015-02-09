\name{DelayUntil}
\Rdversion{1.1}
\alias{DelayUntil}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pauses the code until a specified time
}
\description{
Pauses the code until a specified time.
}
\usage{
DelayUntil(hours, minutes=0,seconds=0,day=as.numeric(format(Sys.time(),"\%d")),
           month = as.numeric(format(Sys.time(),"\%m")),
           year = as.numeric(format(Sys.time(),"\%Y")))}

\arguments{
  \item{hours}{
   The time that you want the code to restart.
}
  \item{minutes}{
   The time that you want the code to restart.
}
  \item{seconds}{
   The time that you want the code to restart.
}
  \item{day}{
   The day that you want the code to restart. Not the day of the
   week, but the number, e.g., 1 for the first day of the month.
   Default is current day.
}
  \item{month}{
   The month that you want the code to restart, as a number, e.g.,
   8 for August. Default is current month.
}
  \item{year}{
   The year that you want the code to restart, with four digits.
   Default is current year.
}

}
\details{
These functions use the eutilities feature of the NCBI database.
Due to the time limits of running lots of requests from the NCBI
site, it might be better to delay the requests until the early
hours of the morning. But this depends on your timezone. 
}
\value{
One line of text displayed that specifies when the code will continue.
}

\author{
Scott Melville
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning}{
The settings are very literal. If you are working in the afternoon, and
set the DelayUntil time to start just after midnight, without specifying
tomorrow's date, the function will decide you meant today. Because this
time today has already happened, it will not place any delay.

}

\examples{
\donttest{
##   Delays until 1:05 AM of the current day - which may have already passed.
DelayUntil(hours=1,minutes=05)
##   Delays until 1:05 of January 1 this year - which may have already passed.
DelayUntil(1,05,day=1,month=1)
}
}
