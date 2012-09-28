GetGenesInSNPs<-function(listofSNPs,output="locusID",batchsize=200)
   {
   if(!(output %in% c("locusID","genesymbol")))
      {
      writeLines("NCBI2R error: The output argument of function GetGenesInSNPs will only accept values of:")
      stop("NCBI2R error: locusID or genesymbol")
      }
   gsi<-GetSNPInfo(listofSNPs,batchsize=batchsize)
   a<-unique(gsi[gsi$locusID!="" & !is.na(gsi$locusID),output])
   return(a)
   }
   