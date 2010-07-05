OpenPMID <-
function(PMID,safety=10)
   {
   thisURL<-paste("http://www.ncbi.nlm.nih.gov/pubmed/",PMID,sep="")
   OpenURL(thisURL,safety)
   }

