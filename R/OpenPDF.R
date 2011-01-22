OpenPDF <-
function(PMID,safety=10,OpenURL=TRUE,download=FALSE)
   {
   if(length(PMID)>safety)
     stop(paste("NCBI2R error: There are more than ",safety," PMIDs! If you are sure, repeat the command, using the safety argument set to a larger number"))
  limit<-min(safety,length(PMID))
  availPDFcounter<-0
  pdfurl<-rep("",limit)
  for(i in 1:limit)
  {
    getURL<-paste("http://www.pubmedcentral.nih.gov/articlerender.fcgi?tool=pubmed&pubmedid=",PMID[i],sep="")
    webget<-try(suppressWarnings(tryScan(getURL)) )
    if(class(webget)=="try-error")
        {
        print("The PDF for the requested paper is not available")
        flush.console()
        } else {
        testing2<-webget[grep("PDF ",webget)]
        p1<- gsub("[[:print:]]+Full Text</li> \\| <li>","\\1",testing2)
        p2<-gsub("<a href=\"","",p1)
        p3<-gsub("\">PDF [[:print:]]+","",p2)
        availPDFcounter<-availPDFcounter+1
        pdfurl[availPDFcounter]<-paste("http://www.ncbi.nlm.nih.gov",p3,sep="")
        }
   }
   pdfurl<-pdfurl[pdfurl!=""]
   for(j in 1:(length(availPDFcounter)))
     {
     if(download==TRUE)
        {
        thisPMC_1<-substr(pdfurl[j],excel.FIND("articles",pdfurl[j])+9,nchar(pdfurl[j]))
        thisPMC_2<-substr(thisPMC_1,1,excel.FIND("/",thisPMC_1)-1)
        download.file(pdfurl[j],destfile=paste(thisPMC_2,".pdf",sep=""),mode="wb",quiet=TRUE) 
        }
   } 
    if(OpenURL==TRUE)
       OpenURL(pdfurl) 
    print(paste("PDFs were found in PMC PubMed central for",length(availPDFcounter),"papers",sep=" "))
   } 
