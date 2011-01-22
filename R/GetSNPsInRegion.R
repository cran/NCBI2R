GetSNPsInRegion <-
function(chr,LowPoint,HighPoint,MaxRet=30000,showurl=FALSE,org="human",quiet=TRUE,sme=FALSE,smt=FALSE)
   {
   URLdef<-ncbi2r.options()
   org<-gsub(" ","+",org)
   LowPoint<-as.numeric(LowPoint)
   HighPoint<-as.numeric(HighPoint)
   if(LowPoint<0)
      LowPoint<-0
   LowPoint<-formatC(LowPoint,digits=9,width=1) 
   HighPoint<-formatC(HighPoint,digits=9,width=1)
   db<-"snp"
   getURL<-paste(URLdef$front,"esearch.fcgi?db=",db,"&term=",chr,"[CHR]+AND+",LowPoint,":",HighPoint,"[CHRPOS]+AND+",org,"[ORGN]&retmax=",MaxRet,"&rettype=FASTA",URLdef$back,sep="") 
   webget<-get.file(getURL,showurl=showurl,quiet=quiet,clean=FALSE)
   listofSNPs<-getListFromXML(webget,sme=sme,smt=smt,MaxRet=MaxRet)
   if(length(listofSNPs)!=0)  
      listofSNPs<-paste("rs",listofSNPs,sep="")
   return(listofSNPs=listofSNPs) 
   }  
