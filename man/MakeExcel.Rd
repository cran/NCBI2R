\name{MakeExcel}
\Rdversion{1.1}
\alias{MakeExcel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates an Excel spreadsheet from genetic information.
}
\description{
Takes the dataframe (from GetPubMed or from any of the snp/gene items
and make a document for Microsoft Excel. This could be from
results/annotation or from a GetPubMed search.
}
\usage{
MakeExcel(anydf, filename, xldiv = ";", linktext = "link", hyper = "HYPERLINK",
         markercolumn = "marker", positioncolumn = "chrpos", absize= 32000,
         keeplocusIDs = FALSE, keepNS = FALSE, kp = TRUE, myColumns="short")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{anydf}{
The dataframe of results
}
  \item{filename}{
The output filename
}
  \item{xldiv}{
The division symbol (comma or semicomma) that goes between items in a
Microsoft Excel formula. You must specify what is already used in the
Regional settings of the computer/Excel program.
}
  \item{linktext}{
The text that will appear visibly in Microsoft Excel that you will click
on to open a URL
}
  \item{hyper}{
The name of the Microsoft Excel command to link to a URL
}
  \item{markercolumn}{
The name of the column that contains the markers
}
  \item{positioncolumn}{
The name of the column that contains the genome position
}
  \item{absize}{
The maximum size of an abstract that will be displayed in a single
cell. See the details section below for larger description.
}
  \item{keeplocusIDs}{
If set to TRUE, the locusIDs will be kept as a column in the output file.
}
  \item{keepNS}{
If set to TRUE, the NeighbouringString will be kept as a column in the 
output file. This is the locusIDs of the neighbouring genes in a single
string for each marker, separated by a comma.

}
  \item{kp}{
When kp is set to TRUE, the words "KEGG PATHWAY" will be shortened to "KP"
to create an easier to read output file.
}

  \item{myColumns}{
Values are short and long. If short, only a few selected columns ("DP","TI", "AU", "AB", "JT", "VI", "IP", "PG", "SO") will be kept.
If long is specified - all columns will be kept.         

}


}
\details{
It converts weblinks to Excel format (where possible) and it turns the
dataframe into a nice format for Excel. The file (with weblinks) has the
advantage that the user can sort by a particular column, but Microsoft
Excel will not allow multiple weblinks in a single cell and the weblinks
will not be available. Using the HTML version overcomes this, and the
user can sort the table before exporting.

To open a URL from Microsoft Excel uses a specific command, depending on
which language the software is in. In English language, the command in
Microsoft Excel is "HYPERLINK" and in Italian it is
"COLLEG.IPERTESTUALE". This command should be specified at the hyper
argument for this command.

Abstract Size - Used only for GetPubMed functions. You might not want the 
complete abstract to be saved, or the form of Microsoft Excel you have may 
not be able to store more than a certain number, about 32000 characters.


}
\value{
A spreadsheet for Microsoft Excel is produced.
}
\references{
%% ~put references to the literature/web site here ~
2003 cell size limits:

http://office.microsoft.com/en-us/excel/HP051992911033.aspx

2007 cell size limits:

http://office.microsoft.com/en-us/excel/HP100738491033.aspx?pid=CH101030621033

}
\author{
Scott Melville
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{MakeHTML}}.

}
\examples{
snplist<-c("rs1234567","rs333","rs848484")
d<-AnnotateSNPList(snplist)  #no HTML file is created
MakeExcel(d,"results.tab")
}
