\name{NatureJobs}
\Rdversion{1.1}
\alias{NatureJobs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtains a dataframe of jobs listed on the NatureJobs.com website
}
\description{
Obtains a dataframe of jobs listed on the NatureJobs.com website.
}
\usage{
NatureJobs(keywords, outputfileprefix = "Jobs", savedescriptions = TRUE,
           days = 0, BadJobs = "", WriteDirectory = "NJ", celllimit = 30000, 
           hyper = "HYPERLINK", xldiv =";", quiet = FALSE, showurl = FALSE, 
           LocalLink=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{keywords}{
an array of keywords to be used against NatureJobs. All keywords will be joined
together in a single request on the website.
}
  \item{outputfileprefix}{
Creates a folder with this name which will hold all the job
descriptions, and a tab delimited spreadsheet. More details are below.
}
  \item{savedescriptions}{
When set to TRUE, it will save the descriptions. More detail coming soon.
}
  \item{days}{
Filter new jobs that were only listed in the last n days. 
Note: May include the same job being reposted.
}
  \item{BadJobs}{
Specify the location of a file that contains a list of job titles you
want to automatically remove. If no file location is specified, or the
file is not found, then no filter will be applied, but the function will
continue.
}
  \item{WriteDirectory}{
The name of the subdirectory that will hold the local copies of the job description.  
}  
  \item{celllimit}{
How many characters do you want in each cell of the spreadsheet. The number of
characters within Microsoft Excel may be limited. See below for more details about cell limits in 2003 and 2007 version.
}
  \item{hyper}{
The formula required in Microsoft Excel for a hyperlink. Default is
english, and the command is specific to the language of the program. 
See notes below
}
  \item{xldiv}{
The division symbol (comma or semicomma) that goes between items in a Microsoft Excel formula. You must specify what is already used in the Regional settings of the computer/Excel program.
}
  \item{quiet}{
Suppresses some output on screen.
}

  \item{showurl}{
Displays the query URL.
}
  \item{LocalLink}{
Makes a URL link to a local copy of the job description.  
}  


}
\details{
This is a nifty little function I wrote to help me find my new
job. Personally, I find the NatureJobs website not as friendly as it
could be, so I wrote this to download the information into a
dataframe, and you can do with that what you want.

It will also make a spreadsheet suitable for Microsoft Excel,
which you can filter yourself.

This function is a little slow depending on what you're search for.

The BadJobList was designed because even though I was searching for
genetics or statistics I would get job titles like "Lecturer in
Neurology", which I did not want. So I added that into my BadJobList
file and I never saw it again. Obviously, if you use the days argument,
this will prevent you from seeing the same job again, unless it's been
reposted. The BadJobList was intended for general sort of titles, like
"lab technician","chair","director" etc which are not the sort of things
I was after.

The search method works the same as on the website, which in cases
will return some strange results based on keywords. If you get a strange
result from a query, perform the same query on the website and compare
the results - please email me (see http://NCBI2R.wordpress.com) if
there are differences.

Using the desired country name in a search may not help, and may exclude 
jobs of interest if the position has not been tagged by that country.

outputfileprefix - the output file is ready for Microsoft Excel (in a 
tab delimited file), and will have this name as it's prefix, and also
integers representing the date and time of the search to prevent
multiple searches from overwriting old searches. All the job
descriptions can go into the same folder each time.

A word of caution - The Expiry Date listed as a heading inside the
NatureJobs description is in some cases not the same as when
applications close. In some cases, I starting writing an application
and on closer reading of the description, I found that applications had
already closed.

To open a URL from Microsoft Excel uses a specific command, depending on
which language the software is in. In English language, the command in
Microsoft Excel is "HYPERLINK" and in Italian it is
"COLLEG.IPERTESTUALE". This command should be specified at the hyper
argument for this command.

This function, and it's author, have no affiliation with NatureJobs.com or
Nature.com
}
\value{
dataframe
}
\references{
%% ~put references to the literature/web site here ~
http://www.naturejobs.com

Information about cell limits in Microsoft Excel 2003

http://office.microsoft.com/en-us/excel/HP051992911033.aspx

Information about cell limits in Microsoft Excel 2007

http://office.microsoft.com/en-us/excel/HP100738491033.aspx?pid=CH101030621033


}
\author{
Scott Melville
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
\donttest{
keywords<-c("scientist","genetic") #this is one search, composed of two keywords
nj<-NatureJobs(keywords,"MYFILE_A_",days=81)
}
}
