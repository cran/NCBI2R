GetSNPPosHapMap<-function(singlesnpname,showurl=FALSE,build="27_B36",rsOnly=TRUE)
   {
   if(build!="3r2_B36" & build!="27_B36")
      stop("NCBI2R GetSNPPosHapMap error: Unknown build specified. Function has failed")
   if(length(singlesnpname)>1)
     stop("NCBI2R GetSNPPosHapMap Error:  Only one SNP at a time please")   
   if(rsOnly==TRUE & substr(singlesnpname,1,2)!="rs")
     stop("NCBI2R GetSNPPosHapMap Error:  This is not a SNP identifier beginning with rs")
   getURL<-paste("http://hapmap.ncbi.nlm.nih.gov/cgi-perl/gbrowse/hapmap",build,"/?name=",singlesnpname,sep="")
   webget <- get.file(getURL, showurl = showurl, clean = FALSE)   
   titleline<-webget[grep("<title>HapMap Data",webget[1:10])]
   tempA<-excel.FIND(":",titleline)
   if(tempA==(-1))
      return("The requested SNP was not found in this build of hapmap")
   else
      {   
       a2<-gsub("</title>","",substr(titleline,tempA+2,nchar(titleline)))
       a3<-gsub("chr","",a2)
       a4<-gsub("\\.\\.",":",a3)
       a5<-unlist(strsplit(a4,":"))
       chr<-a5[1]
       chrpos<-as.numeric(a5[2])
       df1<-as.data.frame(cbind(chr,chrpos=as.numeric(chrpos)),stringsAsFactors=FALSE)
       df1$chrpos<-as.numeric(df1$chrpos)
       return(df1)
     } 
   }
