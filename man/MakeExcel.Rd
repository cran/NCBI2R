\name{MakeExcel}
\Rdversion{1.1}
\alias{MakeExcel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates an Excel spreadsheet from genetic information.
}
\description{
Takes the dataframe (from most NCBI2R functions) and prepares a document for Microsoft Excel.
}
\usage{
MakeExcel(anydf, filename, xldiv = ";", linktext = "link", hyper = "HYPERLINK",
         markercolumn = "marker", positioncolumn = "chrpos", absize= 32000,
         keeplocusIDs = FALSE, keepNS = FALSE, kp = TRUE, myColumns="short")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{anydf}{
The dataframe of results.
}
  \item{filename}{
The output filename.
}
  \item{xldiv}{
The division symbol (comma or semicomma) that goes between items in a
Microsoft Excel formula. You must specify what is already used in the
Regional settings of the computer/Excel program.
}
  \item{linktext}{
The text that will appear visibly in Microsoft Excel that you will click
on to open a URL.
}
  \item{hyper}{
The name of the Microsoft Excel command to link to a URL.
}
  \item{markercolumn}{
The name of the column that contains the markers.
}
  \item{positioncolumn}{
The name of the column that contains the genome position.
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

The function converts the dataframe into something that can be read into Excel,
and will convert URL addresses into hyperlinks.

Microsoft Excel will not allow multiple weblinks in a single cell, and
hyperlinks will not be available. Using a different function MakeHTML will
overcome this, and the user can sort the table before exporting.

The command used to open a URL from Microsoft Excel differs by language. In
English, the command is "HYPERLINK" and in Italian, it is "COLLEG.IPERTESTUALE".
The command should be specified at the hyper argument of the MakeExcel command.

Abstract Size is used only for GetPubMed functions. This is helpful if you do
not want the complete abstract saved, or if your version of Microsoft Excel can
only store a limited number of characters.

}
\value{
A spreadsheet for Microsoft Excel is produced.
}
\references{

The number of characters in each cell of Microsoft Excel differs by version

2003

http://office.microsoft.com/en-us/excel/HP051992911033.aspx

2007

http://office.microsoft.com/en-us/excel/HP100738491033.aspx

2010

http://office.microsoft.com/en-us/excel/HP010342495.aspx
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
