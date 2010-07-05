GetRegion <-
function(db,chr,LowPoint,HighPoint,MaxRet=30000,showurl=FALSE,cg=TRUE,org="human",quiet=TRUE,smt=FALSE,sme=FALSE)
   {                   
   db<-toupper(db)
   if(db=="SNP")
      RegionInfo<-GetSNPsInRegion(chr=chr,LowPoint=LowPoint,HighPoint=HighPoint,MaxRet=MaxRet,showurl=showurl,org=org,quiet=quiet,sme=sme,smt=smt)
   if(db=="GENE")
      RegionInfo<-GetGenesInRegion(chr=chr,LowPoint=LowPoint,HighPoint=HighPoint,MaxRet=MaxRet,showurl=showurl,cg=cg,org,quiet=quiet,sme=sme,smt=smt)
   if(db!="GENE" & db!="SNP")  
      stop(print("database not found. valid answers are 'snp' or 'gene'"))
   return(RegionInfo[RegionInfo!=""])
   }
