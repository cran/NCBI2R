GetSNPsInGene <-
function(locusID,MaxRet=30000,showurl=FALSE,quiet=TRUE,smt=FALSE,sme=FALSE)
   {
   
   URLdef<-URLdefinitions()
   getURL<-paste(URLdef$front,"esearch.fcgi?db=snp&term=",locusID,"[LOCUS_ID]+AND&retmax=",MaxRet,"&rettype=FASTA",URLdef$back,sep="")
   listofSNPs<-rep("",MaxRet)
   webget<-get.file(getURL,showurl=showurl,quiet=quiet,clean=FALSE)
   listofSNPs<-GetListFromXML(webget,sme=sme,smt=smt)
   if(length(listofSNPs)!=0)
      listofSNPs<-paste("rs",listofSNPs,sep="")
   return(listofSNPs=listofSNPs)
   }
