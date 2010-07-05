GetSNPPosHapmap<-function(singlesnpname,showurl=FALSE)
   {
   myurl<-paste("http://hapmap.ncbi.nlm.nih.gov/cgi-perl/gbrowse/hapmap3r2_B36/?name=",singlesnpname,sep="")
   if(showurl)
      print(myurl)
   a1<-scan(myurl,what="character",sep="\n",quiet=TRUE,nmax=10) 
   titleline<-a1[grep("<title>HapMap Data",a1)]
   tempA<-Excel.FIND(":",titleline)
   if(tempA==(-1))
      stop("The requested SNP was not found in this build of hapmap")
   a2<-gsub("</title>","",substr(titleline,tempA+2,nchar(titleline)))
   a3<-gsub("chr","",a2)
   a4<-gsub("\\.\\.",":",a3)
   a5<-unlist(strsplit(a4,":"))
   chr<-a5[1]
   chrpos<-as.numeric(a5[2])
   return(list(chr=chr,chrpos=chrpos))
   }
