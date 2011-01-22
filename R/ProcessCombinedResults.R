processCombinedResults <-
function(combinedresults,pcolumn="p")
   {
   if(missing(combinedresults))
      stop("no data.frame given")
   nonsynonymous<-combinedresults[combinedresults$Fxn_Class=="coding-nonsynonymous",]
   synonymous<-combinedresults[combinedresults$Fxn_Class=="coding-synonymous",]
   mrna_utr<-combinedresults[combinedresults$Fxn_Class=="mrna-utr",]
   locus_region<-combinedresults[combinedresults$Fxn_Class=="locus-region",]
   noteworthy<-combinedresults[(combinedresults$Fxn_Class=="coding-synonymous") | combinedresults$Fxn_Class=="coding-nonsynonymous" | combinedresults$Fxn_Class=="mrna-utr",]
   intron<-combinedresults[combinedresults$Fxn_Class=="intron",]
   return(list(nonsynonymous=nonsynonymous,synonymous=synonymous,mrna_utr=mrna_utr,locus_region=locus_region,noteworthy=noteworthy,intron=intron))
   }  

