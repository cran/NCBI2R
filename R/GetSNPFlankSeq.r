GetSNPFlankSeq<-function(listofSNPs,batchsize=200,showurl=FALSE)
  {
  URLdef<-URLdefinitions()
  test<-unique(substr(listofSNPs,1,2))
  if(length(test)!=1 | test[1]!="rs")
     stop("Incorrect input. Each item must begin with rs")
  
  OneLineSequence<-function(manylines) {
     answer<-paste(manylines,collapse="")
     answer<-gsub(" ","",answer)
     return(answer)
     }
  
  trimmedSNPnames<-substr(as.character(listofSNPs),3,nchar(as.character(listofSNPs)))
  NumberOfBatchesInTotal<-ceiling(length(trimmedSNPnames)/batchsize)
  pb<-txtProgressBar(min=0,max=NumberOfBatchesInTotal,style=3)
  remainingSNPs<-trimmedSNPnames
  for(BatchCounter in 1:NumberOfBatchesInTotal)
    {
    setTxtProgressBar(pb,BatchCounter)
    thisbatchSNPs<-remainingSNPs[1:batchsize]                         
    thisbatchSNPs<-thisbatchSNPs[!(is.na(thisbatchSNPs))]
    remainingSNPs<-remainingSNPs[!(remainingSNPs %in% thisbatchSNPs)] 
    remainingSNPs<-remainingSNPs[!(is.na(remainingSNPs))]
    url_piece<-paste(thisbatchSNPs,collapse=",")

    getURL<-paste(URLdef$front,"efetch.fcgi?db=snp&id=",url_piece,"&report=FASTA",URLdef$back,sep="")
    webget<-get.file(getURL,showurl=showurl,clean=FALSE)
    LineRecords<-grep("^[[:digit:]]+:[[:space:]]",webget)

    ThisPageSNPs<-as.data.frame(cbind(LineRecords,marker="",ThreePrime="",Variation="",FivePrime="",species=""),stringsAsFactors=FALSE)
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
      stop("No SNP data was available within GetSNPFlankSeq. Error code is E02")
      }

    if(length(SNPID_A_lines)==0 & length(SNPID_B_lines)!=0)
      {
      ThisPageSNPs$marker<-paste("rs",strsplitdbl(webget[SNPID_B_lines],"id: "," Error occurred"),sep="")        
      ThisPageSNPs$chrpos<-0
      }
    if(length(SNPID_A_lines)!=0 & length(SNPID_B_lines)==0)
      ThisPageSNPs$marker<-strsplitdbl(webget[SNPID_A_lines],": "," \\[")


    nonHTMLlines<-grep("<",webget)
    keylines<-webget[!(1:length(webget) %in% nonHTMLlines)] 
    IDlines<-grep(":",keylines)

    Infolines<-grep("\\|",keylines) 
    
    startlines<-grep("&gt",keylines)+1
    tempA<-grep(": ",keylines)-1
    badlist<-grep("Error occurred",keylines) 
    
    tempA<-tempA[!(tempA %in% badlist)]
    
    if(!(length(keylines) %in% badlist))
       stoplines<-c(tempA[tempA!=0],length(keylines))
    if(length(keylines) %in% badlist)
       stoplines<-tempA[tempA!=0]
          
    fiveprime<-rep("",length(stoplines))
    threeprime<-rep("",length(stoplines))
    marker<-rep("",length(stoplines))
    variation<-rep(0,length(stoplines))

    i<-0
    while(i<length(stoplines)) {
      i<-i+1

      chunk<-keylines[startlines[i]:stoplines[i]]   
      markerline<-(startlines[i]-2)
      marker[i]<-ParseSNPIDLine(keylines[markerline])$rsID
      nums<-1:length(chunk)
      p1A<-nums[nchar(chunk)==1]
      singlematch<-p1A[p1A!=length(chunk)]   
      VarLineNumber<-max(singlematch) 

      
      if(length(VarLineNumber)>1)
         {
          rsline<-gsub(" rs=(.)*","",keylines[startlines[i]-1])
          badrsID<-gsub("(.)*dbSNP\\|","",rsline)
          print(paste("NCBI2R Error-GetSNPFlankSeq - unable to parse variation of SNP:",badrsID))
          stop("Please email the maintainer of the NCBI2R package. Error code E0312")
         }
      
      variation[i]<-chunk[VarLineNumber]
      five<-chunk[1:((VarLineNumber)-1)]
      three<-chunk[((VarLineNumber)+1):length(chunk)]
      fiveprime[i]<-OneLineSequence(five)
      threeprime[i]<-OneLineSequence(three)
      }
    marker<-as.character(marker)
    w<-as.data.frame(cbind(marker,variation,fiveprime,threeprime),stringsAsFactors=FALSE)
    adjustedTheseSNPs<-paste("rs",thisbatchSNPs,sep="")
    adjustedTheseSNPs<-as.data.frame(cbind(adjustedTheseSNPs,(1:length(adjustedTheseSNPs))),stringsAsFactors=FALSE)
    w$marker<-as.character(w$marker)
    colnames(adjustedTheseSNPs)<-c("marker","originalorder")
    ThisPageSNPs<-merge(adjustedTheseSNPs,w,by="marker",all=TRUE)
    ThisPageSNPs<-ThisPageSNPs[order(ThisPageSNPs$originalorder),]
    ThisPageSNPs$originalorder<-NULL
    ThisPageSNPs$LineRecords<-NULL
    if(BatchCounter==1)   {
        TotalSNPData<-ThisPageSNPs
      } else {
      TotalSNPData<-as.data.frame(rbind(TotalSNPData,ThisPageSNPs))
      }
    } 
   close(pb)
   TotalSNPData$variation<-CleanNAs(TotalSNPData$variation)
   TotalSNPData$fiveprime<-CleanNAs(TotalSNPData$fiveprime)
   TotalSNPData$threeprime<-CleanNAs(TotalSNPData$threeprime)
   if(nrow(TotalSNPData[TotalSNPData$variation=="",])>0)
      print("Note: Some SNPs were not found. They are included in the table with blank results")
   return(TotalSNPData)
   } 

 