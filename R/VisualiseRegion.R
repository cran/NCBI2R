VisualiseRegion <-
function(snpnames,snppositions,pvalues,StartFlank,StopFlank,GeneLowPoint,title=paste(StartFlank,"","StopFlank"),xlab="",filetype="PDF",th1=0.000001,th2=0.0001,locusID=0)
   {
   print("inside VR")
   URLdef<-URLdefinitions()
   subsetAR<-data.frame(cbind(snpnames,snppositions,pvalues,PercentagePositions=0),stringsAsFactors=FALSE) 
   subsetAR$snppositions<-as.numeric(subsetAR$snppositions)
   subsetAR$pvalues<-as.numeric(subsetAR$pvalues)                      
   subsetAR<-subsetAR[order(subsetAR$snppositions),][,] 
   range_low<-as.numeric(as.character(StartFlank))-50
   range_high<-as.numeric(as.character(StopFlank))+20  
   subsetAR$PercentagePositions<-100*(subsetAR$snppositions-range_low)/(range_high-range_low)
   if(filetype!="" & !(file.exists("plots") & file.info("plots")$isdir))
      dir.create("plots")
   if(nrow(subsetAR)==0) 
      print("VisualiseRegion could not be performed as there were no SNPs to plot-but which one was it?")   
   print("VR: after directory made")
   if(nrow(subsetAR)!=0)
      {
      off<-80  
      heightforSNPs<-off-2
      heightfordivisions<-off-20
      while(.Device!="null device") 
         dev.off()
      if(filetype=="PDF")
         {
         title<-gsub("/","[SLASH]",title) 
         pdf(file=paste("plots//",title,"__",format(Sys.time(),"%d_%H%M%S"),".pdf",sep=""))
         }
      if(filetype=="JPEG" | filetype=="JPG")
         {
         title<-gsub("/","[SLASH]",title) 
         jpeg(file=paste("plots//",title,"__",format(Sys.time(),"%d_%H%M%S"),".jpg",sep=""))
         }
      plot(c(0, 100), c(0, off+5), type= "n", xlab=xlab, ylab="",axes=FALSE) 
      title(main=title)
      print(locusID)
      if(locusID!=0)
         {
         asd<-GetGeneTable(locusID)
         if(class(asd)!="character") 
            {
            ExonPlotAt<-80
            NumberToPlot<-max(asd$ExonInfo$Set)
            if(max(asd$ExonInfo$Set)>3){NumberToPlot<-3}
            for(i in 1:NumberToPlot)
               {
               ExonPlotAt<-ExonPlotAt+4 
               ExonInfo<-asd$ExonInfo[asd$ExonInfo$Where=="Exon" & asd$ExonInfo$Set==i,]   
               ExonInfo$Bp.Start<-rep(0,nrow(ExonInfo)) 
               ExonInfo$Bp.Start<-as.numeric(ExonInfo$Bp.Start) 
               ExonInfo$Bp.Start<-as.numeric((GeneLowPoint)+(as.numeric(ExonInfo$Start))-1) 
               ExonInfo$Bp.Stop<-(GeneLowPoint)+(as.numeric(ExonInfo$Stop))-1 
               ExonInfo$Perc.Start<-100*(ExonInfo$Bp.Start-range_low)/(range_high-range_low)
               ExonInfo$Perc.Stop<-100*(ExonInfo$Bp.Stop-range_low)/(range_high-range_low)
               CodingExonInfo<-asd$ExonInfo[asd$ExonInfo$Where=="CodExon" & asd$ExonInfo$Set==i,]
               CodingExonInfo$Bp.Start<-as.numeric((GeneLowPoint)+(as.numeric(CodingExonInfo$Start))-1)
               CodingExonInfo$Bp.Stop<-(GeneLowPoint)+(as.numeric(CodingExonInfo$Stop))-1 
               CodingExonInfo$Perc.Start<-100*(CodingExonInfo$Bp.Start-range_low)/(range_high-range_low)
               CodingExonInfo$Perc.Stop<-100*(CodingExonInfo$Bp.Stop-range_low)/(range_high-range_low)
               PlotExons(ExonInfo,ExonPlotAt,"blue")
               PlotExons(CodingExonInfo,ExonPlotAt,"red")
               } 
            }
         }
     geneleft<-as.numeric(StartFlank) 
      generight<-as.numeric(StopFlank)
      GeneA<-100*(geneleft-range_low)/(range_high-range_low) 
      GeneB<-100*(generight-range_low)/(range_high-range_low)
      threshold.major<-th1  
      threshold.minor<-th2   
      if(min(subsetAR$pvalues)<=threshold.major)
         {
         rect(GeneA,off,GeneB,off+1,col="red",border="blue")
         print("P1")
         } else   {
         if(min(subsetAR$pvalues)<=threshold.minor)
            {
            rect(GeneA,off,GeneB,off+1,col="yellow",border="blue")
            } else {
            rect(GeneA,off,GeneB,off+1,col="green",border="blue")
            }
         }
      points(subsetAR$PercentagePositions,rep(heightforSNPs,length(subsetAR$PercentagePositions)))
      divisions<-NULL 
      for(i in 1:length(subsetAR$PercentagePositions))
         divisions<-c(divisions,(i-1)*(100/(length(subsetAR$PercentagePositions)-1)))
      points(divisions,rep(heightfordivisions,length(divisions)))
      par(srt=90) 
      for(plotsnpnames in 1:length(subsetAR$PercentagePositions))  
         {
         if(length(subsetAR$PercentagePositions)<=40) {
            displaytext<-paste(sprintf("%-12s", subsetAR$snppositions[plotsnpnames]),sprintf("%-14s",subsetAR$snpnames[plotsnpnames])," ",formatC(subsetAR$pvalues[plotsnpnames],format="E",digits=1)," ",sep="")
            text(divisions[plotsnpnames],heightfordivisions,displaytext,pos=2,offset=0, family="mono", cex=1)
            } else  {
            if(length(subsetAR$PercentagePositions)<=50)  {
               displaytext<-paste(sprintf("%-14s", subsetAR$snppositions[plotsnpnames]),sprintf("%-16s",subsetAR$snpnames[plotsnpnames])," ",formatC(subsetAR$pvalues[plotsnpnames],format="E",digits=1)," ",sep="")
               text(divisions[plotsnpnames],heightfordivisions,displaytext,pos=2,offset=0, family="mono", cex=0.9)
               } else {
               if(length(subsetAR$PercentagePositions)<=65)  {
                  displaytext<-paste(sprintf("%-17s", subsetAR$snppositions[plotsnpnames]),sprintf("%-16s",subsetAR$snpnames[plotsnpnames])," ",formatC(subsetAR$pvalues[plotsnpnames],format="E",digits=1)," ",sep="")
                  text(divisions[plotsnpnames],heightfordivisions,displaytext,pos=2,offset=0, family="mono", cex=0.8)
                  } else {
                  displaytext<-paste(sprintf("%-19s", subsetAR$snppositions[plotsnpnames]),sprintf("%-24s",subsetAR$snpnames[plotsnpnames])," ",formatC(subsetAR$pvalues[plotsnpnames],format="E",digits=1)," ",sep="")
                  text(divisions[plotsnpnames],heightfordivisions,displaytext,pos=2,offset=0, family="mono", cex=0.7)
                  }
               }
            }
         }
      mtext(paste("File created at",Sys.time()),side=4,font=3,family="serif")
      mtext("Position               SNPnames                 p-values                                   ",side=2,font=3,family="serif")
      for(i in 1:length(subsetAR$PercentagePositions)) 
         arrows(subsetAR$PercentagePositions[i],heightforSNPs,divisions[i],heightfordivisions,length=0)
      if(filetype!="")  
         {
         while(.Device!="null device") 
            dev.off()
         }
      } 
   print("leaving VR")
   }

