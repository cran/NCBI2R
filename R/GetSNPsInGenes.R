GetSNPsInGenes<-
function(locusIDs,batchsize=200,MaxRet=30000,showurl=FALSE,quiet=TRUE,smt=FALSE,sme=FALSE)
   {
   if(length(locusIDs)==0)
      stop("NCBI2R: You must enter at least one locusID")
   if(length(locusIDs)>100)
      stop("NCBI2R: Too many locusIDs entered. Limited to 100 or less in this build")   
   locusIDs<-locusIDs[locusIDs!=""]  
   locusIDs<-paste(locusIDs,collapse=",")
   locusIDs<-sort(unique(unlist(strsplit(locusIDs,",")))) 
   locusIDs<-paste(locusIDs,collapse=",")                  
   URLdef<-ncbi2r.options()
   getURL<-paste(URLdef$front,"esearch.fcgi?db=snp&term=",locusIDs,"[LOCUS_ID]+AND&retmax=",MaxRet,"&rettype=FASTA",URLdef$back,sep="")
   listofSNPs<-rep("",MaxRet)
   webget<-get.file(getURL,showurl=showurl,quiet=quiet,clean=FALSE)
   listofSNPs<-getListFromXML(webget,sme=sme,smt=smt)
   if(length(listofSNPs)!=0)
      listofSNPs<-paste("rs",listofSNPs,sep="")
   return(listofSNPs=listofSNPs)
   }

