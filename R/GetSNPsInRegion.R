GetSNPsInRegion <-
function(chr,LowPoint,HighPoint,MaxRet=30000,showurl=FALSE,org="human",quiet=TRUE,sme=FALSE,smt=FALSE)
   {
   URLdef<-URLdefinitions()
   LowPoint<-as.numeric(LowPoint)
   HighPoint<-as.numeric(HighPoint)
   if(LowPoint<0)
      LowPoint<-0
   LowPoint<-formatC(LowPoint,digits=9,width=1) 
   HighPoint<-formatC(HighPoint,digits=9,width=1)
   getURL<-paste(URLdef$front,"esearch.fcgi?db=snp&term=",chr,"[CHR]+AND+",LowPoint,":",HighPoint,"[CHRPOS]+AND+",org,"[ORGN]&retmax=",MaxRet,"&rettype=FASTA",URLdef$back,sep="")
   webget<-get.file(getURL,showurl=showurl,quiet=quiet,clean=FALSE)
   listofSNPs<-GetListFromXML(webget,sme=sme,smt=smt,MaxRet=MaxRet)
   if(length(listofSNPs)!=0)  
      listofSNPs<-paste("rs",listofSNPs,sep="")
   return(listofSNPs=listofSNPs) 
   }  
