GetGeneTable <-
function(locusIDs,batchsize=200,showurl=FALSE,pbar=TRUE,TrueBP=FALSE)
   {
   URLdef<-URLdefinitions() 
   if(missing(locusIDs))                                       
      stop("no locusIDs provided")

   oldlocusIDs<-locusIDs
   locusIDs<-unique(oldlocusIDs)
   if(length(oldlocusIDs)!=length(locusIDs))
      print("NCBI2R GetGeneTable message - duplicate locusIDs were removed")
   TotalBatches<-ceiling(length(locusIDs)/batchsize)
   if(TotalBatches==1)
      pbar<-FALSE
   if(pbar==TRUE)
     pb<-txtProgressBar(min=0,max=TotalBatches,style=3)
  remainingItems<-locusIDs
  BatchCounter<-1
  while(BatchCounter <= TotalBatches)
    {
    if(pbar==TRUE)
       setTxtProgressBar(pb,BatchCounter)
    thisbatchItems<-remainingItems[1:batchsize]                       
    thisbatchItems<-thisbatchItems[!(is.na(thisbatchItems))]
    remainingItems<-remainingItems[!(remainingItems %in% thisbatchItems)]
    remainingItems<-remainingItems[!(is.na(remainingItems))]
    url_piece<-paste(thisbatchItems,collapse=",")
    getURL<-paste(URLdef$front,"efetch.fcgi?db=gene&id=",url_piece,"&rettype=gene_Table",URLdef$back,sep="")
    webget<-get.file(getURL,showurl,clean=FALSE) 
    a<-1                                          
   
    
    doublelines<-grep("There is no record in DB for GeneID = [:digit:]+ There is not",webget)

   
    startlines<-grep("^[[:digit:]]+:[[:space:]]",webget)
    if(length(startlines)>1)
       stoplines<-c(startlines[2:length(startlines)]-1,length(webget))
    if(length(startlines)==1)
       stoplines<-length(webget)
 
      
    norecords<-grep("There is no record in DB for GeneID = ",webget)
    additional_startlines<-0
    additional_stoplines<-0
    for(i in 1:length(norecords))
       {
       w1<-unlist(strsplit(webget[norecords[i]],"There is no record in DB for GeneID = ") )
       w1<-w1[w1!=""]
       additional_startlines<-c(additional_startlines,rep(norecords[i],length(w1)))
       additional_stoplines<-c(additional_stoplines,rep(norecords[i],length(w1)))
       }
    additional_startlines<-additional_startlines[additional_startlines!=0]
    additional_stoplines<-additional_stoplines[additional_stoplines!=0]
 
    if(length(additional_startlines)!=0)
      {
      startlines<-sort(c(startlines,additional_startlines))
      stoplines<-sort(c(stoplines,additional_stoplines))
      }

    if(length(startlines)!=length(thisbatchItems) | length(stoplines)!=length(startlines))
       {
       
       print(cat("\n"))
       print("NCBI2R error: variable dumps are:")
       print(thisbatchItems)
       print(length(thisbatchItems))
       print(startlines)
       print(stoplines)
       print("hh")
       print(length(webget))
       stop("NCBI2R GetGeneTable message - not matching")
       }
    
    for(this.item.in.batch in 1:length(thisbatchItems))
      {
      if(startlines[this.item.in.batch]==stoplines[this.item.in.batch])
         gt<-"No information available"
      else   
         gt<-process.gene.table.int(webget[startlines[this.item.in.batch]:stoplines[this.item.in.batch]])
      if(class(gt)!="character")
        {    
        gt$ExonInfo<-as.data.frame(cbind(locusID=thisbatchItems[this.item.in.batch],gt$ExonInfo),stringsAsFactors=FALSE)    
        gt$ACC.DNA<-as.data.frame(cbind(locusID=thisbatchItems[this.item.in.batch],gt$ACC.DNA),stringsAsFactors=FALSE)      
        gt$ACC.Prot<-as.data.frame(cbind(locusID=thisbatchItems[this.item.in.batch],gt$ACC.Prot),stringsAsFactors=FALSE)  

        if(exists("total"))
           {
           total$ExonInfo<-as.data.frame(rbind(total$ExonInfo,gt$ExonInfo),stringsAsFactors=FALSE)
           total$ACC.DNA<-as.data.frame(rbind(total$ACC.DNA,gt$ACC.DNA),stringsAsFactors=FALSE)
           total$ACC.Prot<-as.data.frame(rbind(total$ACC.Prot,gt$ACC.Prot),stringsAsFactors=FALSE)
           total$RecordInfo<-as.data.frame(rbind(total$RecordInfo,gt$RecordInfo),stringsAsFactors=FALSE)
           } else {
           total<-gt
           }
        }
     } 
   BatchCounter<-BatchCounter+1  
  } 
 if(pbar==TRUE)
    setTxtProgressBar(pb,BatchCounter)
  if(exists("total")) 
     {
     if(TrueBP==TRUE)
        total<-AdjustGeneTable(total)
     return(total)
      } else {
   return("No information available") 
      }
   }  
  