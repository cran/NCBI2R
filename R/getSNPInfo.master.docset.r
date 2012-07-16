getSNPInfo.master.docset<-function(trimmedSNPnames,batchsize=200,showurl=FALSE,pbar=TRUE)
   {
   if(length(trimmedSNPnames)==1)
      pbar<-FALSE
   TotalBatches<-ceiling(length(trimmedSNPnames)/batchsize)
   if(TotalBatches==1)
      pbar<-FALSE
   if(pbar)
      pb<-txtProgressBar(min=0,max=TotalBatches,style=3)
   remainingItems<-trimmedSNPnames
   for(BatchCounter in 1:TotalBatches)
      {
      if(pbar==TRUE)
         setTxtProgressBar(pb,BatchCounter)
      thisbatchItems<-remainingItems[1:batchsize]
      thisbatchItems<-thisbatchItems[!(is.na(thisbatchItems))]
      remainingItems<-remainingItems[!(remainingItems %in% thisbatchItems)]
      remainingItems<-remainingItems[!(is.na(remainingItems))]
      ThisPageOfData<-getSNPInfo.singlebatch.trimmednames.docset(thisbatchItems,showurl=showurl)
      if(BatchCounter==1)
         {
         TotalSNPData<-ThisPageOfData
         } else {
         TotalSNPData<-as.data.frame(rbind(TotalSNPData,ThisPageOfData))
         }
      }
   if(pbar==TRUE)
      close(pb)
    TotalSNPData<-getSNPInfo.cleaning(TotalSNPData)
   myvalue_lines<-grep(",",TotalSNPData$locusID)
   if(length(myvalue_lines)!=0)
      {
      myvalues<-grep(",",TotalSNPData$locusID,value=TRUE) 
      myvalues_string<-paste(myvalues,collapse=",")   
      myvalues<-unique(unlist(strsplit(myvalues_string,",")))
      mydata<-GetGeneNames(myvalues,showurl=showurl)[,c("locusID","genesymbol")]
      for(i in myvalue_lines)
         {
         locusIDsToChange<-unlist(strsplit(TotalSNPData$locusID[i],","))
         TotalSNPData$genesymbol[i]<-paste(mydata[mydata$locusID %in% locusIDsToChange,"genesymbol"],collapse=",")
         }
      }
   if(nrow(TotalSNPData[TotalSNPData$chrpos==0,])>0)
      writeLines("Note: Some SNPs were not found. They have a chrpos of zero.")
   if(nrow(TotalSNPData[TotalSNPData$dupl_loc!="",])>0)
      writeLines("Note: Some SNPs were found in more than one location.")
     TotalSNPData$current.rsid<-gsub("^[[:blank:]]*([[:print:]]*)[[:blank:]]has merged into[[:blank:]]*([[:print:]]*)[[:blank:]]*$","\\2",TotalSNPData$marker)
   TotalSNPData$marker<-gsub("^[[:blank:]]*([[:print:]]*)[[:blank:]]has merged into[[:blank:]]*([[:print:]]*)$[[:blank:]]*","\\1",TotalSNPData$marker)
   TotalSNPData$marker<-gsub(" ","",TotalSNPData$marker)
   TotalSNPData$current.rsid<-gsub(" ","",TotalSNPData$current.rsid)

   TotalSNPData$fxn_class<-gsub(" ","",TotalSNPData$fxn_class)
   TotalSNPData$genesymbol<-gsub(" ","",TotalSNPData$genesymbol)
   return(TotalSNPData)
   }

