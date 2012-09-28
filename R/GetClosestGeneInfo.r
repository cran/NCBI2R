GetClosestGeneInfo<-function(listofSNPs,FlankingDistance=100000,lb=TRUE)
   {
   if(class(listofSNPs)!="character")
     stop("NCBI2R error: vector of snp ids required as input and each start with rs")
   test <- unique(substr(listofSNPs, 1, 2))
   if (length(test) != 1 | test[1] != "rs")
        stop("NCBI2R error: Incorrect input. Each item must begin with rs")  
   gsi<-try(GetSNPInfo(listofSNPs))
   if(class(gsi)=="try-error")
     stop("GetClosestGene failed due to an error in GetSNPInfo")

   zeropos<-gsi[gsi$chrpos==0,]
   gsi<-gsi[gsi$chrpos!=0,]

   internalgenes<-unique(unlist(strsplit(gsi$locusID,","))) 
   missingsnps<-gsi$marker
   if(length(internalgenes)>0)
      {
      ggi<-try(GetGeneInfo(internalgenes))
      if(class(ggi)=="try-error")
        {
        errorHandler("GCGI-001")
        stop("")
        }
      if(lb)
        ggi<-ggi[grep("not current",ggi$build,invert=TRUE),]
      markers<-gsi$marker[gsi$locusID!=""]
      sg<-SplitGenes(gsi[gsi$locusID!="",c("marker","genesymbol","locusID","flag")],quiet=TRUE)
      all.internals<-merge(sg[,c("marker","locusID","flag")],ggi,by="locusID",all=TRUE)
      all.internals$distance<-0
      all.internals<-merge(all.internals,gsi[gsi$locusID!="",c("marker","chrpos")],by="marker",all=TRUE)
      missingsnps<-missingsnps[!(missingsnps %in% all.internals$marker)]
      gsi<-gsi[!(gsi$marker %in% all.internals$marker),] 
      }

   neargenes<-c()
   if(nrow(gsi)>0)
      {
      for(L in 1:nrow(gsi))
          gsi$locusID[L]<-paste(GetGenesInRegion(gsi$chr[L],gsi$chrpos[L]-FlankingDistance,gsi$chrpos[L]+FlankingDistance),collapse=",")
      neargenes<-unique(unlist(strsplit(gsi$locusID,",")))
      }

      if(length(neargenes)!=0)
         {
         ggi<-try(GetGeneInfo(neargenes))
         if(class(ggi)=="try-error")
           stop("NCBI2R error GCGI-005. Unable to use GetGeneInfo within GetClosestGeneInfo")
         if(lb)
            ggi<-ggi[grep("not current",ggi$build,invert=TRUE),]
         sg<-SplitGenes(gsi[,c("marker","genesymbol","locusID","chrpos","flag")],quiet=TRUE)
         sg<-sg[sg$locusID %in% ggi$locusID,]


         h1<-merge(sg[,c("marker","locusID","flag")],ggi,by="locusID",all=TRUE)
         h1$distance<-999999999
         h1<-merge(h1,sg[,c("marker","chrpos")],by="marker",all=TRUE)
         h1$distance[h1$chrpos<=h1$GeneLowPoint] <- h1$GeneLowPoint[h1$chrpos<=h1$GeneLowPoint] - h1$chrpos[h1$chrpos<=h1$GeneLowPoint]
         h1$distance[h1$chrpos>=h1$GeneHighPoint]<- h1$chrpos[h1$chrpos>=h1$GeneHighPoint]-h1$GeneHighPoint[h1$chrpos>=h1$GeneHighPoint]
         h1$keep<-FALSE;    allmarkers<-unique(h1$marker)
         for(K in 1:length(allmarkers))
            h1$keep[h1$marker==allmarkers[K] & h1$distance==min(h1$distance[h1$marker==allmarkers[K]])]<-TRUE
         all.externals<-unique(h1[h1$keep==TRUE,])
         all.externals$keep<-NULL
         missingsnps<-missingsnps[!(missingsnps %in% all.externals$marker)]
         }
     if(exists("all.internals") & exists("all.externals"))
       res<-as.data.frame(rbind(all.internals,all.externals),stringsAsFactors=FALSE)
     if(exists("all.internals") & !exists("all.externals"))
       res<-all.internals
     if(!exists("all.internals") & exists("all.externals"))
       res<-all.externals
     if(!exists("all.internals") & !exists("all.externals"))
       res<-"No snps near genes"
   if(class(res)=="data.frame")
      {
      pr.cols<-c("marker","chrpos","distance","genesymbol","locusID","GeneLowPoint","GeneHighPoint")

      other<-names(res)[!(names(res) %in% pr.cols)]
      res<-res[,c(pr.cols,other)]

      res<-try(order.to.original.list(enteredlist=listofSNPs[listofSNPs %in% res$marker],df1=res,keycol="marker"))

      }
    return(list(data=res,snps.no.genes=missingsnps,snps.unmapped=zeropos$marker))
   }
