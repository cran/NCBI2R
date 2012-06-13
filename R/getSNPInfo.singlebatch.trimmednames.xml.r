
getSNPInfo.singlebatch.trimmednames.xml<-function(trimmedSNPnames,showurl=FALSE)
   {
   URLdef<-ncbi2r.options()
   url_piece<-paste(trimmedSNPnames,collapse=",")
   getURL<-paste(URLdef$front,"efetch.fcgi?db=snp&id=",url_piece,"&rettype=XML&retmode=text",URLdef$back,sep="")
   webget<-get.file(getURL,showurl,clean=FALSE)
   webget<-unlist(strsplit(webget,"<"))
   g1<-paste("<",webget[webget!=""],sep="")
   rm(webget)
  key.things.to.extract<-c("Rs","Sequence","Frequency","Assembly","FxnSet","MapLoc","Component","MergeHistory")
  g2<-parse.multiple.line.xml2(key.things.to.extract,g1)
  g2$entry<-cut(g2[,4],c(g2$LN[g2$V1=="Rs" & g2$V2=="rsId"],99999999999),labels=FALSE,right=FALSE)
  g2$snp<-g2$V3[g2$V1=="Rs" & g2$V2=="rsId"][cut(g2[,4],c(g2$LN[g2$V1=="Rs" & g2$V2=="rsId"],99999999999),labels=FALSE,right=FALSE)]
  y762<-unique(g2[,c("entry","snp")])
  g2<-g2[g2$entry %in%    y762$entry[!duplicated(y762$snp)],]
  g2$entry<-pmatch(g2$entry,unique(g2$entry),duplicates.ok=TRUE)

  g2$groupGenomeMADEUP<-""
  refLines<-c(g2$LN[g2$V1=="Assembly" & g2$V2=="groupLabel"],9999999999)
  key.things.to.assembly.stage2<-c("Assembly","FxnSet","MapLoc","Component")
  k17<-g2$V3[g2$V1=="Assembly" & g2$V2=="groupLabel"]
  if(length(k17)>0)
    {
    g2$groupGenomeMADEUP[g2$V1 %in% key.things.to.assembly.stage2]<-k17[cut(g2$LN[g2$V1 %in% key.things.to.assembly.stage2],refLines,labels=FALSE,right=FALSE)]
    groupLabels.ref<-get.reference.genome.groupLabels(g1)
    g3<-g2[g2$groupGenomeMADEUP %in% groupLabels.ref | g2$groupGenomeMADEUP=="",]
    } else {
    g3<-g2
    }

   keepTheseA<-c("rsId","snpClass","symbol","geneId","fxnClass","physMapInt","chromosome","taxId","freq","allele")
   g4<-g3[g3$V2 %in% keepTheseA,]
   ans<-as.data.frame(cbind(current.rsid=paste("rs",g4$V3[g4$V1=="Rs" & g4$V2=="rsId"],sep=""),genesymbol="",locusID="",fxn_class="",
        taxId=g4$V3[g4$V1=="Rs" & g4$V2=="taxId"],
        snpClass=g4$V3[g4$V1=="Rs" & g4$V2=="snpClass"]),stringsAsFactors=FALSE)

  t1<-g4[g4$V1=="Frequency" & g4$V2=="freq",c("snp","V3")]
  t2<-g4[g4$V1=="Frequency" & g4$V2=="allele",c("snp","V3")]
  if(nrow(t1)>0 | nrow(t2)>0)
    {
    t3<-merge(t1,t2,by="snp",suffixes=c("freq","allele"),all=TRUE)
    t3$snp<-paste("rs",t3$snp,sep="")
    names(t3)[2:3]<-c("freq","ma")
    ans<-merge(ans,t3,by.x="current.rsid",by.y="snp",all=TRUE)
    oso.df<-g4[g4$V1=="Rs" & g4$V2=="rsId",c("V3","LN"),]
    oso.df$V3<-paste("rs",oso.df$V3,sep="")

    tmp<-merge(ans,oso.df,by.x="current.rsid",by.y="V3",all=TRUE)
    ans<-tmp[order(tmp$LN),]
    rm(tmp)
    
    } else {
    ans$freq<-NA
    ans$ma<-""
    }
   uq.entries<-unique(g4$entry[g4$V1=="FxnSet" & g4$V2=="geneId"])
   for(i in uq.entries)
      {
      ans$genesymbol[i]<-paste(unique(g4$V3[g4$V1=="FxnSet" & g4$V2=="symbol" & g4$entry==i]),collapse=",")
      ans$locusID[i]<-paste(unique(g4$V3[g4$V1=="FxnSet" & g4$V2=="geneId" & g4$entry==i]),collapse=",")
      ans$fxn_class[i]<-paste(unique(g4$V3[g4$V1=="FxnSet" & g4$V2=="fxnClass" & g4$entry==i]),collapse=",")

      }

    g4$chr<-""
  if(nrow(g4[g4$V1=="MapLoc",])>0)
      {
    t7<-g4$V3[g4$V1=="Component" & g4$V2=="chromosome"][cut(g4$LN[g4$V1=="MapLoc"],c(g4$LN[g4$V1=="Component" & g4$V2=="chromosome"],99999999),labels=FALSE,right=TRUE)]
    g4$chr[g4$V1=="MapLoc"]<-t7
    mapping<-g4[g4$V1=="MapLoc" & g4$V2=="physMapInt" &g4$chr!="",c("snp","V3","chr")]
    mapping$snp<-paste("rs",mapping$snp,sep="")
    mapping<-unique(mapping)
    m2<-collapse.df(mapping,keycol="snp")
    ans<-unique(merge(ans,m2,by.x="current.rsid",by.y="snp",all=TRUE))
    } else {
    ans$chr<-""
    ans$chrpos<-""
    }

   merge.df<-unique(g4[g4$V1=="MergeHistory" & g4$V2=="rsId",c("V3","snp")])
  names(merge.df)<-c("marker","current.rsid")

  ans2<-ans
  ans2$marker<-ans2$current.rsid
  if(nrow(merge.df)>0)
     {
     merge.df$current.rsid<-paste("rs",merge.df$current.rsid,sep="")
     merge.df$marker<-paste("rs",merge.df$marker,sep="")
       cur<-unique(merge.df$current.rsid)
     merge.df<-as.data.frame(rbind(merge.df,as.data.frame(cbind(marker=cur,current.rsid=cur))))
     ans2<-merge(ans,merge.df,by="current.rsid",all=TRUE)
     }

   ans2$marker[is.na(ans2$marker)]<-ans2$current.rsid[is.na(ans2$marker)]
   ans3<-unique(ans2[ans2$marker %in% paste("rs",trimmedSNPnames,sep=""),])

   return(ans3)
   }
   
get.reference.genome.groupLabels<-function(dat)
   {
  y1.dt<-dat[grep("Assembly[[:print:]]*[[:blank:]]reference",dat)]
  reference.list<-gsub("[[:print:]]*groupLabel=\"([[:print:]]*?)\"[[:print:]]*$","\\1",y1.dt)
  return(reference.list)
  }
parse.multiple.line.xml2<-function(keywords,txt)
   {
   keywords<-paste("^<",keywords," ",sep="")
   xx.l<-grep(paste(keywords,collapse="|"),txt)
   xx<-txt[xx.l]
   h1<-as.data.frame(parse.multiple.line.internal(xx),stringsAsFactors=FALSE)
   h1$LN<-rep(xx.l,times=nchar(gsub("[^=]","",xx)))
   return(h1)
   }
parse.multiple.line.internal<-function(SL)
   {
   j5<-gsub("^<[[:print:]]*?([[:blank:]]([[:print:]]*=\"[[:print:]]*\"))(/)*>$", "\\1",SL)
   j6<-unlist(strsplit(j5,"\"| "))
   j6<-matrix(j6,ncol=3,byrow=TRUE)
   SL.first.word<-gsub("^<([[:print:]]*?)[[:blank:]][[:print:]]*$","\\1",SL)
   j6[,1]<-rep(SL.first.word,nchar(gsub("[^=]","",SL)))

   j6[,2]<-gsub("=","",j6[,2])
   return(j6)
   }

collapse.df<-function(df18,keycol="snp")
   {
   numOcc<-as.data.frame(table(df18$snp),stringsAsFactors=FALSE)
   single.df<-df18[df18$snp %in% numOcc$Var1[numOcc$Freq==1],]
   multi.df<-df18[df18$snp %in% numOcc$Var1[numOcc$Freq>1],]
   if(nrow(multi.df)>0)
      {
      multi.df$newchr<-""
      multi.df$newchrpos<-0
      multi.df$dupl_loc<-""
      for(i in (1:nrow(multi.df))[ !duplicated(multi.df$snp)])
         {
             multi.df$newchr[i]<-df18$chr[df18$snp==multi.df$snp[i]][1]
         multi.df$newchrpos[i]<-df18$V3[df18$snp==multi.df$snp[i]][1]

         a<-df18$chr[df18$snp==multi.df$snp[i]]
         b<-df18$V3[df18$snp==multi.df$snp[i]]
         a<-a[2:length(a)]
         b<-b[2:length(b)]
         multi.df$dupl_loc[i]<-paste(paste(a,b,sep=":"),collapse=",")
         }

      multi.df<-multi.df[!duplicated(multi.df$snp),]
      names(multi.df)<-c("snp","X1","X2","chr","chrpos","dupl_loc")
      }
   if(nrow(single.df)>0)
      {
      names(single.df)<-c("snp","chrpos","chr")
      single.df$dupl_loc<-""
      if(nrow(multi.df)>0)
         {
         mergedf<-rbind(multi.df[,c("snp","chr","chrpos","dupl_loc")],single.df[,c("snp","chr","chrpos","dupl_loc")])
         } else {
         mergedf<-single.df
         }
      } else {
      mergedf<-multi.df
      }
   return(mergedf)
   }
