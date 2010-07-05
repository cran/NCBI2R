ScanForGenes.FindSNPPositions <-
function(locusID,AssociationResults,fiveprime = 100000,threeprime = 2000,fileprefix="",markercolumn="name",pcolumn="p",visualise=TRUE,xlab="",filetype="PDF",th1=0.001,th2=0.01,org="human")
   {
   largeresponse<-data.frame("","","","","","",stringsAsFactors=FALSE)
   dees<-GetGeneInfo(locusID)
   response<-data.frame(genename=dees$genename,genesymbol=dees$genesymbol,locusID=dees$locusID,chr=dees$chr,GeneLowPoint=dees$GeneLowPoint,GeneHighPoint=dees$GeneHighPoint,ori=dees$ori,StartFlank=rep(0,nrow(dees)),StopFlank=0,bestSNP="",bestP=0,nSNPs=0,stringsAsFactors=FALSE)
   print("<<SFG.Find")
   for(i in 1:nrow(response))
      {
      if(response$GeneLowPoint[i]!=0)
         {
         temp<-AdjustRangeAroundGene(response$GeneLowPoint[i],response$GeneHighPoint[i],response$ori[i], fiveprime, threeprime)
         response$StartFlank[i]<-as.numeric(temp$LowPoint) 
         response$StopFlank[i]<-as.numeric(temp$HighPoint)
         tempSNPs<-GetRegion("snp",response$chr[i],response$StartFlank[i],response$StopFlank[i],org=org,showurl=TRUE)
         tempdump<-AssociationResults[AssociationResults[,markercolumn] %in% tempSNPs,]
         response$bestP[i]<-as.numeric(tempdump[order(tempdump[,pcolumn], decreasing=FALSE),][1,pcolumn])
         response$bestSNP[i]<-tempdump[order(tempdump[,pcolumn], decreasing=FALSE),][1,markercolumn]
         response$nSNPs[i]<-nrow(tempdump) 
         if(visualise==TRUE & response$nSNPs[i]!=0)
            {
            tempdump$position<-GetSNPInfo(tempdump[,markercolumn])$chrpos
            VisualiseRegion(tempdump[,markercolumn],tempdump$position,tempdump[,pcolumn],response$StartFlank[i],response$StopFlank[i],dees$GeneLowPoint[i],title=paste(dees$fullname[i],"[",dees$locusID[i],"]",sep=""),xlab=paste("chr",response$Chr[i]),filetype=filetype,th1=th1,th2=th2,dees$locusID[i])
            }
         if(visualise==TRUE & response$nSNPs[i]==0)
            print("VisualiseRegion could not be performed as there were no SNPs to plot")
         if(largeresponse[1,2]!="" & (nrow(tempdump[,c(markercolumn,pcolumn)])!=0))   
            {
            stuff_to_add<-data.frame(response$genename[i],response$genesymbol[i],response$locusID[i],response$chr[i],response$GeneLowPoint[i],response$GeneHighPoint[i],response$ori[i],tempdump[,c(markercolumn,pcolumn)],stringsAsFactors=FALSE)
            largeresponse<-data.frame(rbind(largeresponse,stuff_to_add)) 
            }
         if(largeresponse[1,2]=="" & (nrow(tempdump[,c(markercolumn,pcolumn)])!=0))
            {
            largeresponse<-data.frame(response$genename[i],response$genesymbol[i],response$locusID[i],response$chr[i],response$GeneLowPoint[i],response$GeneHighPoint[i],response$ori[i],tempdump[,c(markercolumn,pcolumn)],stringsAsFactors=FALSE)    
            }
         }
      }
   colnames(largeresponse)<-c("genename","genesymbol","locusID","chr","GeneLowPoint","GeneHighPoint","ori",markercolumn,pcolumn) 
   return(list(best=response,complete=largeresponse,dees=dees))
   }

