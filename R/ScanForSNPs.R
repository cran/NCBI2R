ScanForSNPs <-
function(snplist,AssociationResults,markercolumn="name",chromosomecolumn="chr",positioncolumn="position")
   {
   if(positioncolumn %in% colnames(AssociationResults)) {      
      answer<-ScanForSNPs.WithSNPPositions(snplist,AssociationResults,markercolumn=markercolumn,chromosomecolumn=chromosomecolumn,positioncolumn=positioncolumn)
      } else {
      answer<-ScanForSNPs.FindSNPPositions(snplist,AssociationResults,markercolumn=markercolumn)   
      }
   }

