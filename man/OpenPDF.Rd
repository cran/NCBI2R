\name{OpenPDF}
\Rdversion{1.1}
\alias{OpenPDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Opens an available PDF from the PudMed Central database
}
\description{
Opens an available PDF from PudMed Central within the webrowser.
}
\usage{
OpenPDF(PMID, safety=10, OpenURL=TRUE, download=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PMID}{
The PubMed ID number of the paper. If you've used GetPubMed,
you will have this in one column.
}
  \item{safety}{
This is the upper limit of items that will be opened. Designed to stop
the user accidently opening many items at the same time.
}

  \item{OpenURL}{
If set to TRUE, this will open up a window for each of the PMIDs. This function 
will only work if NCBI2R can detect a web browser. }

  \item{download}{
If set to TRUE, will download available papers on PMC(PubMed Central).
}
}
\details{
Input the PMID (ID number from PubMed). This number may be obtained from
the GetPubMed command, or by entering the number manually. If the
article has a PDF version in the PubMed Central database, a webbrowser
will open up with the PDF inside it. If the PDF is not available, then
the PubMed page will open up to allow the user to select from the
available login methods.
}
\value{
No returned value. A string returned if PDF is not available on PubMed Central.
}
\references{
%% ~put references to the literature/web site here ~
http://www.ncbi.nlm.nih.gov/sites/entrez?db=PubMed
                                 
http://www.pubmedcentral.nih.gov/
}
\author{
Scott Melville
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Warning}{
Will work only if you have a web browser set up.
}

\seealso{
\code{\link{OpenPMID}}, \code{\link{GetPubMed}}
}
\examples{

#Note: will run only if web browser set up on operating system
#examplepaper<-GetPubMed("South Tyrol MICROS")
#OpenPDF(examplepaper$PMID)

#mypaper<-GetPubMed("border collie CLN5")
# will not open PDF because item not available on PubMed Central
#OpenPDF(mypaper$PMID)

#OpenPDF(20436961)
}
