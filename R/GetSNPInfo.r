GetSNPInfo<-function(listofSNPs,batchsize=200,showurl=FALSE,pbar=TRUE)
  {
  if(length(listofSNPs)==1)
     pbar<-FALSE
  URLdef<-URLdefinitions()
  test<-unique(substr(listofSNPs,1,2)) 
  if(length(test)!=1 | test[1]!="rs")
     stop("Incorrect input. Each item must begin with rs")
  trimmedSNPnames<-substr(as.character(listofSNPs),3,nchar(as.character(listofSNPs)))
  NumberOfBatchesInTotal<-ceiling(length(trimmedSNPnames)/batchsize)
  if(pbar==TRUE)
    pb<-txtProgressBar(min=0,max=NumberOfBatchesInTotal,style=3)
  remainingSNPs<-trimmedSNPnames
   
  for(BatchCounter in 1:NumberOfBatchesInTotal)
    {
  if(pbar==TRUE)
    setTxtProgressBar(pb,BatchCounter)
  thisbatchSNPs<-remainingSNPs[1:batchsize]                       
  thisbatchSNPs<-thisbatchSNPs[!(is.na(thisbatchSNPs))]
  remainingSNPs<-remainingSNPs[!(remainingSNPs %in% thisbatchSNPs)]
  remainingSNPs<-remainingSNPs[!(is.na(remainingSNPs))]
  url_piece<-paste(thisbatchSNPs,collapse=",")
  getURL<-paste(URLdef$front,"efetch.fcgi?db=snp&id=",url_piece,"&report=DocSet",URLdef$back,sep="")
  webget<-get.file(getURL,showurl,clean=FALSE)
  LineRecords<-grep("^[[:digit:]]+:[[:space:]]",webget)
  ThisPageSNPs<-as.data.frame(cbind(LineRecords,marker="",genesymbol="",locusID="",chr="",chrpos="",fxn_class="",species="",dupl_loc=""),stringsAsFactors=FALSE)
  Species_lines<-grep(" \\[[[:alpha:]]+[[:space:]][[:alpha:]]+\\] ",webget)
  if(length(Species_lines!=0))
      {
      Species_text<-strsplitdbl(webget[Species_lines]," \\[","\\] ")
      ThisPageSNPs$species<-AlignsData(Species_lines,Species_text,LineRecords) 
      }
   
  SNPID_B_lines<-grep("cannot get document summary",webget)
  SNPID_A_lines<-LineRecords[!(LineRecords %in% SNPID_B_lines)]

  if(length(SNPID_A_lines)!=0 & length(SNPID_B_lines)!=0)
    {
    SNPID_A_text<-strsplitdbl(webget[SNPID_A_lines],": "," \\[")
    SNPID_B_text<-paste("rs",strsplitdbl(webget[SNPID_B_lines],"id: "," Error occurred"),sep="")   
    ThisPageSNPs$SNPID_A<-as.character(AlignsData(SNPID_A_lines,SNPID_A_text,LineRecords))            
    ThisPageSNPs$SNPID_B<-as.character(AlignsData(SNPID_B_lines,SNPID_B_text,LineRecords))
    ThisPageSNPs$marker[is.na(ThisPageSNPs$SNPID_A)]<-ThisPageSNPs$SNPID_B[is.na(ThisPageSNPs$SNPID_A)]
    ThisPageSNPs$marker[is.na(ThisPageSNPs$SNPID_B)]<-ThisPageSNPs$SNPID_A[is.na(ThisPageSNPs$SNPID_B)]
    ThisPageSNPs$SNPID_A<-NULL            
    ThisPageSNPs$SNPID_B<-NULL
    } 
    
  if(length(SNPID_A_lines)==0 & length(SNPID_B_lines)==0)
    {
    print("ITEMS: NONE FOUND")
    print("An error has occurred in NCBI2R. Please try again.")
    print("If a problem persists - please contact the author - details at the website")
    stop("No SNP data was available. Error code is E01")
    }
    
  if(length(SNPID_A_lines)==0 & length(SNPID_B_lines)!=0)
    {
    ThisPageSNPs$marker<-paste("rs",strsplitdbl(webget[SNPID_B_lines],"id: "," Error occurred"),sep="")        
    ThisPageSNPs$chrpos<-0
    }
  if(length(SNPID_A_lines)!=0 & length(SNPID_B_lines)==0)
    ThisPageSNPs$marker<-strsplitdbl(webget[SNPID_A_lines],": "," \\[")
    
    GENE_lines<-grep("GENE=",webget)
    if(length(GENE_lines)!=0)
       GENE_text<-substr(webget[GENE_lines],6,nchar(webget[GENE_lines]))
    LOCUSID_lines<-grep("^LOCUS_ID=",webget)
    LOCUSID_text<-substr(webget[LOCUSID_lines],10,nchar(webget[LOCUSID_lines])) 
    if(length(LOCUSID_lines)!=0)
      {
      ThisPageSNPs$genesymbol<-AlignsData(GENE_lines,GENE_text,LineRecords)
      ThisPageSNPs$locusID<-AlignsData(LOCUSID_lines,LOCUSID_text,LineRecords)           
      }
    FXNCLASS_lines<-grep("^FXN_CLASS=",webget)
    FXNCLASS_text<-substr(webget[FXNCLASS_lines],11,nchar(webget[FXNCLASS_lines]))
    if(length(FXNCLASS_lines)!=0)
      ThisPageSNPs$fxn_class<-AlignsData(FXNCLASS_lines,FXNCLASS_text,LineRecords)
    CHRPOS_lines<-grep("^CHROMOSOME BASE POSITION=",webget)
    dbl_lines<-grep("^CHROMOSOME BASE POSITION=+[[:alnum:]]+:[[:digit:]]+\\|",webget)
    if(length(dbl_lines)!=0)
      {              
      dbl_text<-substr(webget[dbl_lines],26,nchar(webget[dbl_lines]))   
      ThisPageSNPs$dupl_loc<-AlignsData(dbl_lines,dbl_text,LineRecords)

      webget[dbl_lines]<-gsub("(\\|[[:alnum:]]+:+[[:digit:]]+)*$","",webget[dbl_lines])
      }

    CHRPOS_text<-substr(webget[CHRPOS_lines],26,nchar(webget[CHRPOS_lines]))   
    CHR_text<-as.character(do.call(cbind,strsplit(CHRPOS_text,":"))[1,])

    if(length(CHRPOS_lines)!=0)
       {
       ThisPageSNPs$chr<-AlignsData(CHRPOS_lines,CHR_text,LineRecords)
       BP_text<-as.numeric(do.call(cbind,strsplit(CHRPOS_text,":"))[2,])
       ThisPageSNPs$chrpos<-AlignsData(CHRPOS_lines,BP_text,LineRecords)
       }

    ThisPageSNPs$LineRecords<-NULL 
    if(BatchCounter==1)   {
      TotalSNPData<-ThisPageSNPs
      } else {
      TotalSNPData<-as.data.frame(rbind(TotalSNPData,ThisPageSNPs))   
      }
    } 
  if(pbar==TRUE)    
   close(pb) 
    
  TotalSNPData$genesymbol<-CleanNAs(TotalSNPData$genesymbol)
  TotalSNPData$locusID<-CleanNAs(TotalSNPData$locusID)
  TotalSNPData$chr<-CleanNAs(TotalSNPData$chr)
  TotalSNPData$chrpos<-CleanNAs(TotalSNPData$chrpos)
  TotalSNPData$fxn_class<-CleanNAs(TotalSNPData$fxn_class)
  TotalSNPData$species<-CleanNAs(TotalSNPData$species)
  TotalSNPData$dupl_loc<-CleanNAs(TotalSNPData$dupl_loc)

   myvalue_lines<-grep(",",TotalSNPData$locusID)
  if(length(myvalue_lines)!=0)
     {
      myvalues<-grep(",",TotalSNPData$locusID,value=TRUE) 
      myvalues_string<-paste(myvalues,collapse=",")   
      myvalues<-unique(unlist(strsplit(myvalues_string,",")))
      mydata<-GetGeneName(myvalues)[,c("locusID","genesymbol")]
      for(i in myvalue_lines)
         {
         locusIDsToChange<-unlist(strsplit(TotalSNPData$locusID[i],","))
         TotalSNPData$genesymbol[i]<-paste(mydata[mydata$locusID %in% locusIDsToChange,"genesymbol"],collapse=",")
         }
     }    
  if(nrow(TotalSNPData[TotalSNPData$chrpos==0,])>0)
     print("Note: Some SNPs were not found. The have a chrpos of zero.")
  if(nrow(TotalSNPData[TotalSNPData$dupl_loc!="",])>0)
     print("Note: Some SNPs were found in more than one location.")
  return(TotalSNPData)
}   


