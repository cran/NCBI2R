plotExons <-
function(Exons,ExonsPos,color,ExonSize=3,IntronSize=0.5)
   { 
   for(boxloop in 1:nrow(Exons))
      {
      rect(Exons$Perc.Start[boxloop],ExonsPos-(ExonSize/2),Exons$Perc.Stop[boxloop],ExonsPos+(ExonSize/2),col=color,border=color)
      if(boxloop!=nrow(Exons))
         rect(Exons$Perc.Stop[boxloop],ExonsPos-(IntronSize/2),Exons$Perc.Start[boxloop+1],ExonsPos+(IntronSize/2),col=color,border=color)
      }
   } 