
getSNPInfo.master.xml<-function(trimmedSNPnames,batchsize=200,showurl=FALSE)
   {
   NumBatches<-ceiling(length(trimmedSNPnames)/batchsize)
   BatchCount<-1
   gsi.all<-getSNPInfo.singlebatch.trimmednames.xml(trimmedSNPnames[1:min(batchsize,length(trimmedSNPnames))],showurl=showurl)
   while(BatchCount<NumBatches)
      {
      BatchCount<-BatchCount+1
      startnum<-((BatchCount-1)*batchsize)+1
      stopnum<-min(BatchCount*batchsize,length(trimmedSNPnames))
      j<-getSNPInfo.singlebatch.trimmednames.xml(trimmedSNPnames[startnum:stopnum],showurl=showurl)
      gsi.all<-as.data.frame(rbind(gsi.all,j),stringsAsFactors=FALSE)
      }

  newcols<-c("marker","genesymbol","locusID","chr","chrpos","fxn_class","taxId","snpClass","dupl_loc","current.rsid","freq","ma")
  absentcols<-newcols[!(newcols %in% names(gsi.all))]
  i<-0
  while(i<length(absentcols))
     {
     i<-i+1
     gsi.all[,absentcols[i]]<-""
     }

   ans4<-gsi.all[,newcols]
   ans4$flag<-0
   ans4$flag[ans4$dupl_loc!=""]<-1
   modcols<-c(2:4,6,7,8,9,11)
   ans4[,modcols][is.na(ans4[,modcols])]<-""
   ans4$freq<-as.numeric(ans4$freq)
   ans4$chrpos[is.na(ans4$chrpos)|ans4$chrpos==""]<-"0"
   ans4$chrpos<-as.numeric(ans4$chrpos)
   ans4$chrpos[ans4$chrpos!=0]<-ans4$chrpos[ans4$chrpos!=0]+1
   return(ans4)
   }
