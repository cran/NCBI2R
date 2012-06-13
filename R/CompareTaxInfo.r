
CompareTaxInfo<-function(id1=9606,id2=10090)
   {
   if(length(id1)!=1 | length(id2)!=1)
     stop("NCBI2R error: One item sep by columns please.")
   g1<-GetTaxInfo(id1)
   g2<-GetTaxInfo(id2)
   common<-g1$lineage[g1$lineage$line.sciName %in% g2$lineage$line.sciName,]
   common$sciName<-"shared"
   id1u<-g1$lineage[!g1$lineage$line.sciName %in% g2$lineage$line.sciName,]
   id2u<-g2$lineage[!g2$lineage$line.sciName %in% g1$lineage$line.sciName,]
   return(list(common=common,id1=id1u,id2=id2u))
   }
