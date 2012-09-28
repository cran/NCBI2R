GetSNPInfo<-function(listofSNPs,batchsize=200,showurl=FALSE,pbar=TRUE,style="docset.xml")
   {
   if(style!="docset.xml")
     stop("NCBI2R error: Only docset.xml is available for the style setting at this time.")
   check.batchsize(batchsize)
   trimmedSNPnames<-checkSNPsAllrs(listofSNPs)
   p1<-getSNPInfo.master.docset(trimmedSNPnames=trimmedSNPnames,batchsize=batchsize,showurl=showurl,pbar=pbar)
   j3<-p1[p1$chrpos==0 & p1$species!="",]
   j4<-p1[!(p1$chrpos==0 & p1$species!=""),]
   if(nrow(j3)>0 & style=="docset.xml")
      {
      writeLines("Some markers were not found. Will attempt a second method.")
      p2<-getSNPInfo.master.xml(trimmedSNPnames=gsub("^rs","",j3$marker),batchsize=batchsize,showurl=showurl)
      p2j3<-merge(j3[,c("marker","species")],p2[,c("marker","genesymbol","locusID","chr","chrpos","fxn_class","dupl_loc","current.rsid","flag")],by="marker",all=TRUE)
      rm(p2)
      p2j3<-p2j3[,c("marker","genesymbol","locusID","chr","chrpos","fxn_class","species","dupl_loc","current.rsid","flag")]
      j34<-as.data.frame(rbind(j4,p2j3),stringsAsFactors=FALSE)
      } else {
      j34<-j4; rm(j4)
      }
   ans<-order.to.original.list(listofSNPs,j34,"marker")
   rm(j3)
   return(ans)
   }