AdjustGeneTable<-function(ExonInfoTables,optional_geneDB)
   {
    if(paste(names(ExonInfoTables),collapse="")!="ExonInfoACC.DNAACC.ProtRecordInfo")
       stop("NCBI2R message: You need to specify the name that is on all three tables")
    foundgenes<-unique(ExonInfoTables$ExonInfo$locusID)
    if(missing(optional_geneDB))
       {
       gi<-GetGeneInfo(foundgenes)
       } else  {
        gi<-optional_geneDB[optional_geneDB$locusID %in% foundgenes]
        requiredgenes<-foundgenes[!(foundgenes %in% gi$locusID)]  
        if(length(requiredgenes)!=0)
           {
           gi2<-GetGeneInfo(requiredgenes)
           gi<-as.data.frame(cbind(gi,gi2),stringsAsFactors=FALSE)
           }
         }
    newACC.DNA<-merge(gi,ExonInfoTables$ACC.DNA,by="locusID",all.x=FALSE,all.y=TRUE)
    newACC.DNA<-newACC.DNA[,c("locusID","Identifier","Length","Exons","GeneLowPoint","GeneHighPoint","ori","chr","genesymbol")]
    newExonInfo<-merge(gi,ExonInfoTables$ExonInfo,by="locusID",all.x=FALSE,all.y=TRUE)
    newExonInfo<-newExonInfo[,c("locusID","Where","Start","Stop","Size","Set","GeneLowPoint","GeneHighPoint","ori","chr","genesymbol")]
    newExonInfo$Start.BP<-newExonInfo$Start+newExonInfo$GeneLowPoint-1
    newExonInfo$Stop.BP<-newExonInfo$Start+newExonInfo$GeneHighPoint-1
    newExonInfo<-newExonInfo[,c("locusID","Where","Start","Stop","Start.BP","Stop.BP","Size","Set","ori","chr","genesymbol")]
    newACC.Prot<-merge(gi,ExonInfoTables$ACC.Prot,by="locusID",all.x=FALSE,all.y=TRUE)
    newACC.Prot<-newACC.Prot[,c("locusID","Identifier","Length","Exons","GeneLowPoint","GeneHighPoint","ori","chr","genesymbol")]
    total<-list(ExonInfo=newExonInfo,ACC.DNA=newACC.DNA,ACC.Prot=newACC.Prot)
    return(total)
   }
