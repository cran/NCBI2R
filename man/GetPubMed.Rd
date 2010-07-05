\name{GetPubMed}
\Rdversion{1.1}
\alias{GetPubMed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains references from NCBI's PubMed database
}
\description{
Obtains references from NCBI's PubMed database
}
\usage{
GetPubMed(searchterm, file = "", download = FALSE, showurl = FALSE, xldiv =
          ";", hyper = "HYPERLINK", MaxRet = 30000, sme = FALSE, smt = FALSE,
          quiet = TRUE, batchsize=500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{searchterm}{
The search term for PubMed.
}
  \item{file}{
Name of the optional Microsoft Excel spreadsheet to be created. Leave
out if not required and no file will be created.
}
  \item{download}{
If set to True, it will download all papers available from PubMed Central.
}
  \item{showurl}{
Enables the user to see the URL of the NCBI request.
}
  \item{xldiv}{
The division symbol (comma or semicomma) that goes between items in a
Microsoft Excel formula.
}
  \item{hyper}{
The name of the Microsoft Excel command to link to a URL.
}
  \item{MaxRet}{
The maximum number of items to return.
}
  \item{sme}{
These two arguments are for checking that your requests are being
understood correctly by NCBI. If set to true, these will show all
translations of your request (smt), and any errors that were obtained in
the query (sme).
}
  \item{smt}{
These two arguments are for checking that your requests are being
understood correctly by NCBI. If set to true, these will show all
translations of your request (smt), and any errors that were obtained in
the query (sme).
}
  \item{quiet}{
If set to True, it will not display the title of the paper as it is
processed.
}
  \item{batchsize}{
Controls how many MEDLINE entries it obtains as one query of the NCBI database.
}


}
\details{
This function is a literature search in one simple R command. This function 
obtains the MEDLINE entries for the papers that match the search
term specified. The MEDLINE entry contains paper title, PubMedID (PMID), 
abstract, journal, date of publication etc. A dataframe of this information
is returned. 

You can, if you wish to, output this information as an tab delimited file, just
by entering a name in the file argument. It contains specially designed links
that will work in Microsoft Excel so that you can click on some cells and 
(depending on operating system) will open up links to webbrowser for a more
graphical repesentation on the NCBI website.

Any papers available in PubMedCentral will be downloaded if you want to. Simply
set the download argument to TRUE, and a directory will be created from the 
search term inside your working directory, and then all the papers will be saved
inside. If a filename already exists with that name, it will not be overwritten.
This allows you to rerun the same search later, and update your reference 
collection. Remember these are only papers in PubMedCentral that are freely 
available. Other free papers on journal websites will not be included. The 
titles of the papers will be abbreviated to about seventy characters and special
symbols removed.

Future plans will include similar options for OpenOffice.

To open a URL from Microsoft Excel uses a specific command, depending on
which language the software is in. In English language, the command in
Microsoft Excel is "HYPERLINK" and in Italian it is
"COLLEG.IPERTESTUALE". This command should be specified at the hyper
argument for this command.

Mechanism: This function works in three steps, first it obtains all the PMIDs for
the query term. It does this in one query of NCBI. The second step is to obtain
the MEDLINE information for all the papers. If you have one hundred papers, and
the batchsize is set to 500, only one query of NCBI will get you all of the 
MEDLINE data from those papers - all the data is joined together. But if you 
have 4000 papers, a batch size of 500, will mean, 8 queries will be performed.
The data is then parsed and a dataframe is created. The file step is if you set
download to be TRUE - each paper that is found on PubMedCentral will be 
downloaded. If it fails, it's likely the paper is embargoed at the present time.

}
\value{
A dataframe of all of the medline fields for all references, and a
specially formatted spreadsheet for Microsoft Excel will be created.
}

\author{
Scott Melville
}

\section{Warning}{
These functions use NCBI's eutils, and come with the same user requirements - if
performing many queries, you must run the scripts during certain hours when the
NCBI servers are not in high demand. Please see the package website for more details

http://drop.io/NCBI2R_package

Violation of the terms described there, and the terms on the eutils website may
result in losing access to NCBI for your group.
}


\examples{
a <- GetPubMed("CLN5","My references.tab")
}