GetARFromLocation <-
function(AssociationResults,chr,LowPoint,HighPoint,chromosomecolumn="chr",positioncolumn="chrpos")
   {
   AssociationResults[AssociationResults[,chromosomecolumn]==chr & AssociationResults[,positioncolumn]>=LowPoint & AssociationResults[,positioncolumn]<=HighPoint,]
   }

