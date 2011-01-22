GetUniSTSSpecies <-function(stsid,showurl=FALSE)
   {
   if(length(stsid)>1)
     stop("Error: Please provide only one id number at a time for this function")
   for(i in 1:nchar(stsid))
     {
     if(substr(stsid,i,i) %in% LETTERS | substr(stsid,i,i) %in% letters)
        stop("Error: Please specify an ID number not a search term. Use GetIDs or GetUniSTSFromName")
     }
   getURL<-paste("http://www.ncbi.nlm.nih.gov/genome/sts/sts.cgi?uid=",stsid,sep="")
   qw<-get.file(getURL,showurl=showurl,sep="\n",quiet=TRUE,clean=FALSE)
   if(length(grep("Lookup failed",qw))>0)
     stop(writeLines(paste("Error: No information for unists ",stsid,". Lookup failed",sep="")))
   places<-grep("<td colspan =\"2\" class =\"H2\" bgcolor =CCCCFF>&nbsp;<a name = taxid",qw)
   placesmap<-grep("<!--- Mapping data --->",qw)
   ttt<-qw[places]
   species<-data.frame(taxid=rep(0,100),org="",map="No",stringsAsFactors=FALSE)
   for(i in 1:length(places))
     {
     temp<-splitfirst(ttt[i],"taxid")[2]
     temp<-splitfirst(temp,"</i>")[1]
     species[i,"taxid"]<-as.numeric(splitfirst(temp,">")[1])
     species[i,"org"]<-splitfirst(temp,"<i>")[2]  
     if(substr(qw[(placesmap[i]+1)],1,5)!="<!---")
        {
        species[i,"map"]<-"Yes"
        }
     }
   species<-species[species$taxid!=0,]
   return(species)
   }
