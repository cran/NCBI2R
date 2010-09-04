GetGenesInRegion <-
function(chr,LowPoint,HighPoint,MaxRet=30000,showurl=FALSE,cg=TRUE,org="human",quiet=TRUE,sme=FALSE,smt=FALSE)
   {
   URLdef<-URLdefinitions()
   org<-gsub(" ","+",org)
   LowPoint<-as.numeric(LowPoint)  
   HighPoint<-as.numeric(HighPoint)
   if(LowPoint<0)
      LowPoint<-0  
   LowPoint<-formatC(LowPoint,digits=9,width=1) 
   HighPoint<-formatC(HighPoint,digits=9,width=1) 
   if(cg) {
     currentgenestring<-"+gene+all[filter]"
     }  else { 
     currentgenestring<-""
     }
   getURL<-paste(URLdef$front,"esearch.fcgi?db=gene&term=",chr,"[CHR]+AND+",LowPoint,":",HighPoint,"[CHRPOS]+AND+",org,"[ORGN]",currentgenestring,"&retmax=",MaxRet,"&rettype=FASTA",URLdef$back,sep="")
   webget<-get.file(getURL,showurl=showurl,clean=FALSE)
   ListItems<-GetListFromXML(webget,sme=sme,smt=smt)
   return(ListItems=ListItems)  
   }
