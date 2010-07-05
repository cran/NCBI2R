SortByChr <-
function(anydf)
   {
   anydf$chr<-add.lead.zero(anydf$chr)   
   anydf$chrpos<-formatC(anydf$chrpos,width=9)
   anydf<-anydf[order(anydf$chr,anydf$chrpos),]
   }

