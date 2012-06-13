get.taxa.major.records.from.tax.xml<-function(xml)
   {
   taxId<-gsub("[[:blank:]]*<[[:print:]]*>([[:print:]]*)</[[:print:]]*>$","\\1",xml[ grep("^    <TaxId>",xml)])
   sciName<-gsub("[[:blank:]]*<[[:print:]]*>([[:print:]]*)</[[:print:]]*>$","\\1",xml[grep("^    <ScientificName>",xml)])
   rank<-gsub("[[:blank:]]*<[[:print:]]*>([[:print:]]*)</[[:print:]]*>$","\\1",xml[grep("^    <Rank>",xml)])
   division<-gsub("[[:blank:]]*<[[:print:]]*>([[:print:]]*)</[[:print:]]*>$","\\1",xml[grep("^    <Division>",xml)])
   lineage<-gsub("[[:blank:]]*<[[:print:]]*>([[:print:]]*)</[[:print:]]*>$","\\1",xml[grep("^    <Lineage>",xml)])
   master.df<-as.data.frame(cbind(taxId,sciName,rank,division,lineage),stringsAsFactors=FALSE)
   return(master.df)
   }

get.lineage.records.from.tax.xml<-function(xml)
   {
   lineage.tax<-gsub("[[:blank:]]*<[[:print:]]*>([[:print:]]*)</[[:print:]]*>$","\\1",xml[ grep("^            <TaxId>",xml)])
   lineage.sciname<-gsub("[[:blank:]]*<[[:print:]]*>([[:print:]]*)</[[:print:]]*>$","\\1",xml[1+grep("^            <TaxId>",xml)])
   lineage.rank<-gsub("[[:blank:]]*<[[:print:]]*>([[:print:]]*)</[[:print:]]*>$","\\1",xml[2+grep("^            <TaxId>",xml)])
   sciName<-gsub("[[:blank:]]*<[[:print:]]*>([[:print:]]*)</[[:print:]]*>$","\\1",xml[grep(paste("^[[:blank:]]{",4,"}<ScientificName>",sep=""),xml)])
   taxId<-gsub("[[:blank:]]*<[[:print:]]*>([[:print:]]*)</[[:print:]]*>$","\\1",xml[grep(paste("^[[:blank:]]{4}<TaxId>",sep=""),xml)])
   LN<-as.numeric(grep("^            <TaxId>",xml))
   l.df<-as.data.frame(cbind(sciName=sciName[cut(LN,c(grep("^    <TaxId>",xml),99999999999),right=TRUE,labels=FALSE)],LN=LN,line.taxId=lineage.tax,line.sciName=lineage.sciname,line.rank=lineage.rank),stringsAsFactors=FALSE)
   l.df$LN<-as.numeric(l.df$LN)
   l.df$taxId<-taxId[cut(l.df$LN,c(grep("^    <TaxId>",xml),99999999999),right=TRUE,labels=FALSE)]
   return(l.df)
   }

get.names.from.tax.xml<-function(xml)
   {
   keys<-c("CommonName","DispName","Synonym","GenbankCommonName","ScientificName")
   baserow<-as.data.frame(cbind(name="",LN=0,nametype=""),stringsAsFactors=FALSE)
   leader<-8+c(0,4,0,0,-4)
   for(k in 1:length(keys))
     {
      LN<-grep(paste("^[[:blank:]]{",leader[k],"}<",keys[k],">",sep=""),xml)
      if(length(LN)>0)
        {
        namedata<-gsub("[[:blank:]]*<[[:print:]]*>([[:print:]]*)</[[:print:]]*>$","\\1",xml[grep(paste("^[[:blank:]]{",leader[k],"}<",keys[k],">",sep=""),xml)])
        g.this<-as.data.frame(cbind(name=namedata,LN=LN,nametype=keys[k]),stringsAsFactors=FALSE)
        baserow<-as.data.frame(rbind(baserow,g.this),stringsAsFactors=FALSE)
        }
      }
   m1<-baserow
   m1$LN<-as.numeric(m1$LN)
   m1<-m1[order(m1$LN),]
   m1$sciName<-m1$name[m1$nametype=="ScientificName"][cut(m1$LN,c(m1$LN[m1$nametype=="ScientificName"],9999999999),right=FALSE,labels=FALSE)]
   m1<-m1[m1$LN!=0,]
   sciName<-m1$name[m1$nametype=="ScientificName"]
   taxId<-gsub("[[:blank:]]*<[[:print:]]*>([[:print:]]*)</[[:print:]]*>$","\\1",xml[grep(paste("^[[:blank:]]{4}<TaxId>",sep=""),xml)])
   d6<-as.data.frame(cbind(taxId,sciName),stringsAsFactors=FALSE)
   m1<-merge(m1,d6,by="sciName",all=TRUE)
   m1<-m1[order(m1$LN),c("taxId","sciName","name","nametype","LN")]
   return(m1)
   }

get.akaTax.ids.from.tax.xml<-function(xml)
   {
   startlines<-grep("    <AkaTaxIds>$",xml)
   stoplines<-grep("    </AkaTaxIds>$",xml)
   alltaxids<- grep("<TaxId>",xml)
   AkaTaxIds.LN<-alltaxids[alltaxids %in% (startlines+1) | alltaxids %in% (stoplines-1) | alltaxids %in% (alltaxids+1) | alltaxids %in% (alltaxids-1)]
   record.start.ln<-grep("^    <TaxId>[[:digit:]]*</TaxId*>",xml)
   record.start<-gsub("[[:blank:]]*[[:print:]]*>([[:digit:]]*)</[[:print:]]*>$","\\1",xml[record.start.ln])
   if(length(AkaTaxIds.LN)==0)
      {
      x2<-as.data.frame(cbind(reqTaxId=record.start,taxId=record.start),stringsAsFactors=FALSE)
      } else {
      x<-record.start[cut(AkaTaxIds.LN,c(record.start.ln,9999999999),right=FALSE,labels=FALSE)]
      AkaTaxIds<-gsub("[[:blank:]]*[[:print:]]*>([[:digit:]]*)</[[:print:]]*>$","\\1",xml[AkaTaxIds.LN])
      x2<-as.data.frame(cbind(reqTaxId=c(AkaTaxIds,record.start[!(record.start %in% x)]),taxId=c(x,record.start[!(record.start %in% x)])),stringsAsFactors=FALSE)
        }
   return(x2)
   }


GetTaxInfo<-function(taxids,batchsize=200,showurl=FALSE)
   {

   if(length(taxids)<1 | !is.numeric(taxids))
     stop("NCBI2R error: Needs one or more taxonomy identifiers as numbers")
   if(!is.numeric(taxids) & !is.integer(taxids))
     stop("NCBI2R error: Needs one or more taxonomy identifiers as numbers")
    if(length(taxids)>200)
     {
     writeLines("NCBI2R error: At this time, GetTaxInfo only allows a max of 200 items")
     stop("This will be fixed in future versions of NCBI2R")
     }

   URLdef<-ncbi2r.options()
   getURL<-paste(URLdef$front,"efetch.fcgi?db=taxonomy&id=",paste(taxids,collapse=","),URLdef$back,sep="")
   webget<-get.file(getURL,showurl,clean=FALSE)
   if(length(grep("Error occurred",webget))>0)
      stop("NCBI2R error. Taxonomy search failed.")
   if(length(webget)<5)
      stop("NCBI2R error. Taxonomy search failed.")
   j1<-get.akaTax.ids.from.tax.xml(webget)
   j2<-get.names.from.tax.xml(webget)
   j3<-get.taxa.major.records.from.tax.xml(webget)
   j4<-get.lineage.records.from.tax.xml(webget)
   j12<-merge(j1,j2,by="taxId",all=TRUE)
   j12<-j12[order(j12$LN),-6]
   j13<-merge(j1,j3,by="taxId",all=TRUE)
   j14<-merge(j1,j4,by="taxId",all=TRUE)
   j14<-j14[order(j14$LN),-4]
   df1<-order.to.original.snplist(taxids,j12,"taxId")
   df1$taxId<-as.numeric(df1$taxId)
   df1$reqTaxId<-as.numeric(df1$reqTaxId)
   row.names(df1)<-1:nrow(df1)
   df2<-order.to.original.snplist(taxids,j13,"taxId")
   df2$taxId<-as.numeric(df2$taxId)
   df2$reqTaxId<-as.numeric(df2$reqTaxId)
   row.names(df2)<-1:nrow(df2)
   df3<-order.to.original.snplist(taxids,j14,"taxId")
   df3$taxId<-as.numeric(df3$taxId)
   df3$reqTaxId<-as.numeric(df3$reqTaxId)
   row.names(df3)<-1:nrow(df3)
   return(list(namesVarious=df1,lineage=df3,names=df2))
   }
