GetUniSTSInfo <-
function(stsid,taxid=9606,showurl=FALSE,quiet=TRUE)
   {
   if(quiet==FALSE)
      print(paste("running GetUniSTSInfo",stsid))
   if(length(stsid)==0)
      stop("require one id number at a time for the function GetUniSTSInfo")
   if(length(stsid)!=1)
      stop("only one id number at a time for the function GetUniSTSInfo. sorry.")
   getURL<-paste("http://www.ncbi.nlm.nih.gov/genome/sts/sts.cgi?uid=",stsid,sep="")
   webget<-get.file(getURL,quiet=quiet,showurl=showurl,clean=FALSE)
   StartLine<-grep(paste(";<a name = taxid",taxid,">",sep=""),webget) 
   warningstring<-get.uni.sts.info.check(webget,StartLine,taxid,stsid)
   tmp<-get.uni.sts.info.cross.ref(webget,StartLine)
   CrossRefError<-tmp$CrossRefError
   CrossRef<-tmp$CrossRef   
   tmp<-get.uni.sts.info.map(webget,StartLine,stsid)
   MappingError<-tmp$MappingError
   MappingDF<-tmp$MappingDF   
   if(CrossRefError=="None" & MappingError=="None")
      return(list(CrossRefs=CrossRef,Maps=MappingDF,WarningString=warningstring)) 
   if(CrossRefError!="None" & MappingError!="None")
      return(list(CrossRefs=CrossRefError,Maps=MappingError,WarningString=warningstring))
   if(CrossRefError=="None" & MappingError!="None")
     return(list(CrossRefs=CrossRef,Maps=MappingError,WarningString=warningstring))
   if(CrossRefError!="None" & MappingError=="None")
      return(list(CrossRefs=CrossRefError,Maps=MappingDF,WarningString=warningstring))
 }


