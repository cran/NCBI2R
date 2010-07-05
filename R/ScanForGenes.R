ScanForGenes <-
function(locusID,AssociationResults,fiveprime = 100000,threeprime = 2000,fileprefix="",markercolumn="name",pcolumn="p",positioncolumn="position",visualise=TRUE,xlab="",filetype="PDF",th1=0.001,th2=0.01,moveold=TRUE,org="human")
   {
   if(missing(AssociationResults))
     stop("NCBI2R ScanForGenes error: No dataframe provided")
   if(class(AssociationResults)!="data.frame")
      stop("NCBI2R ScanForGenes error: You have not provided a data.frame for the association results")
   if(visualise==TRUE & file.exists("plots") & file.info("plots")$isdir & moveold==TRUE)
      {
      MainDir<-getwd()
      setwd("plots")
      fl<-dir()
      if((file.exists("old") & file.info("old")$isdir==TRUE))
         {
         for(i in 1:length(fl))
            {
            if(file.info(fl[i])$isdir==FALSE)
               file.rename(fl[i],paste("old//",fl[i],sep=""))
            }
         } else {
         dir.create("old")
         for(i in 1:length(fl))
            {
            if(file.info(fl[i])$isdir==FALSE)
               file.rename(fl[i],paste("old//",fl[i],sep=""))
            }
         }
      setwd(MainDir)
      }
    if(positioncolumn %in% colnames(AssociationResults)) {
       print("SFG will be run using positions already determined in the result file")
       answer<-ScanForGenes.WithSNPPositions(locusID,AssociationResults,fiveprime,threeprime,fileprefix,markercolumn,pcolumn,positioncolumn,visualise,xlab,filetype,th1,th2)
        } else {
        print("SFG will be run but no position column can be found so NCBI will be required more")
       answer<-ScanForGenes.FindSNPPositions(locusID,AssociationResults,fiveprime,threeprime,fileprefix,markercolumn,pcolumn,visualise,xlab,filetype,th1,th2)
       }
   return(list(best=answer$best,complete=answer$complete,GeneInfo=answer$dees))
   } 

