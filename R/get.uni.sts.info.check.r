get.uni.sts.info.check<-function(webget,StartLine,taxid,stsid)
   {
   if(length(StartLine)==0 & length(grep("Lookup failed",webget))>0)
     stop(writeLines(paste("Error: No information for unists ",stsid,". Lookup failed",sep="")))    
   if(length(StartLine)==0)                                                                                   
     stop(writeLines(paste("Error: No information for unists ",stsid," on requested species ",taxid,sep="")))
   warn<-grep("<font color=\"#FF0066\">Warning!",webget)
   warningstring<-"None"
   if(length(warn)!=0)   
      {
      print("NCBI2R - UniSTS - warnings have been indentified")
      places<-grep("<td colspan =\"2\" class =\"H2\" bgcolor =CCCCFF>&nbsp;<a name = taxid",webget)  
      if(length(warn)>1)
        warningstring<-"More than warning found. Check the NCBI webpage."
      if(length(warn==1))
         {
         if(warn<StartLine)
           {
           warningstring<-"Warning was found but for different species. Consider checking."
           } else {
           if(pmatch(StartLine,places)!=length(places)) 
              {
              NextSpeciesStart<-places[pmatch(StartLine,places)+1]
              if(warn>StartLine & warn<NextSpeciesStart)
                 warningstring<-splitfirst(webget[warn+1],"<BR>")[1]
              } else {
              warningstring<-splitfirst(webget[warn+1],"<BR>")[1]
              }
           }
        }
      }
   return(warningstring)
   }    