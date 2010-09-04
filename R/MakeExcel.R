MakeExcel <-
function(anydf,filename,xldiv=";",linktext="link",hyper="HYPERLINK",markercolumn="marker",positioncolumn="chrpos",absize=32000,keeplocusIDs=FALSE,keepNS=FALSE,kp=TRUE,myColumns="short") 
  {
   if(missing(anydf))
      stop("NCBI2R MakeExcel error: no data frame provided")  
   if(class(anydf)!="data.frame")
       stop("NCBI2R MakeExcel error: no data frame provided")
   convert<-FALSE
   if(names(anydf)[1]=="PubMed_Unique_Identifier")
      {
      anydf<-ConvertPubMedHeadings(anydf,reverse=TRUE)    
      convert<-TRUE
      }
   if(names(anydf)[1]=="PMID") {
      if(myColumns!="short" & myColumns!="long")
        stop("NCBI2R error: invalid value for myColumns")

      if(myColumns=="short")
         Sel.Cols<-c("DP","TI", "AU", "AB", "JT", "VI", "IP", "PG", "SO")
         
      if(myColumns=="long")
         Sel.Cols<-names(anydf)
      Sel.Cols<-unique(c("PMID",linktext,Sel.Cols)) 
      anydf$DP<-substr(anydf$DP,1,4)  
      anydf$JT<-gsub("&amp;","&",anydf$JT) 
      anydf$AB<-substr(anydf$AB,1,absize)
      anydf$link<-paste("http://www.ncbi.nlm.nih.gov/pubmed/",anydf$PMID,sep="")
      if("localcopy" %in% colnames(anydf)) {
          anydf<-anydf[,c(Sel.Cols,"localcopy")]
          anydf$localcopy[anydf$localcopy!=""]<-ConvertURLToExcel(anydf[anydf$localcopy!="","localcopy"],"Local",xldiv=xldiv,hyper=hyper) 
          } else {
          anydf<-anydf[,Sel.Cols]
          }
      anydf$link<-ConvertURLToExcel(link=anydf$link,linktext=linktext,xldiv=xldiv,hyper=hyper)
      anydf$PG<-paste("'",anydf$PG,sep="")
      if(convert)
         anydf<-ConvertPubMedHeadings(anydf)
      write.table(anydf,file=filename,sep="\t",row.names=FALSE,quote=FALSE)
      print("file was created")
      }  else {
      anydf$NeighHTMLlink<-NULL
      if(kp)
         anydf$pathways<-gsub("KEGG pathway","KP",anydf$pathways)
      if(keepNS==FALSE)
         anydf$NeighString<-NULL
      anydf[,positioncolumn]<-ConvertURLToExcel(paste(anydf$Neigh.web,"&query=",anydf[,markercolumn],sep=""),linktext=anydf[,positioncolumn],xldiv=xldiv,hyper=hyper)
      anydf[,markercolumn]<-ConvertURLToExcel(paste("http://www.ncbi.nlm.nih.gov/SNP/snp_ref.cgi?rs=",anydf[,markercolumn],sep=""),linktext=anydf[,markercolumn],xldiv=xldiv,hyper=hyper)
      anydf$genesymbol[anydf$locusID!=""]<-ConvertURLToExcel(paste("http://www.ncbi.nlm.nih.gov/sites/entrez?db=gene&term=",anydf$locusID[anydf$locusID!=""],sep=""),linktext=anydf$genesymbol[anydf$locusID!=""],xldiv=xldiv,hyper=hyper)
      anydf$genename[anydf$locusID!=""]<-ConvertURLToExcel(paste("http://www.ncbi.nlm.nih.gov/portal/query.fcgi?p$site=entrez&db=gene&cmd=Display&dopt=gene_pubmed&from_uid=",anydf$locusID[anydf$locusID!=""],sep=""),linktext=anydf$genename[anydf$locusID!=""],xldiv=xldiv,hyper=hyper)
      if(keeplocusIDs==FALSE)
         anydf$locusID<-NULL
      anydf$OMIM.xlweb<-as.character("")
      anydf$OMIM.xlweb[anydf$OMIM!=""]<-ConvertURLToExcel(paste("http://www.ncbi.nlm.nih.gov/entrez/dispomim.cgi?id=",anydf$OMIM[anydf$OMIM!=""],sep=""),linktext=anydf$OMIM[anydf$OMIM!=""],xldiv=xldiv,hyper=hyper)
      anydf$OMIM<-anydf$OMIM.xlweb
      anydf$OMIM.xlweb<-NULL
      anydf$Neigh<-ConvertURLToExcel(anydf$Neigh.web,linktext="NeighView",xldiv=xldiv,hyper=hyper)
      anydf$Neigh.web<-NULL
      anydf$pathways.HTML<-NULL
      anydf$phenotypes.HTML<-NULL
      anydf$locusID<-paste("'   ",anydf$locusID,sep="") 

      anydf$genesummary<-gsub(";","*,",anydf$genesummary)
      write.table(anydf,file=filename,sep="\t",row.names=FALSE, col.names=TRUE, quote=FALSE)
      print("file was created")
      }
   } 
