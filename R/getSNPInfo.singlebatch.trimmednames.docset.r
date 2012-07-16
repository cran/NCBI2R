
getSNPInfo.singlebatch.trimmednames.docset<-function(trimmedSNPs,showurl=FALSE)
   {
   URLdef<-ncbi2r.options()
   url_piece<-paste(trimmedSNPs,collapse=",")
   getURL<-paste(URLdef$front,"efetch.fcgi?db=snp&id=",url_piece,"&report=DocSet",URLdef$back,sep="")
   webget<-get.file(getURL,showurl,clean=FALSE)
   new20120618LineRecords<-grep("^rs[[:digit:]]+",webget)
   old20120618LineRecords<-grep("^[[:digit:]]+:[[:space:]]",webget)
   LineRecords<-sort(c(old20120618LineRecords,new20120618LineRecords))
   ThisPageSNPs<-as.data.frame(cbind(LineRecords,marker="",genesymbol="",locusID="",chr="",chrpos=0,fxn_class="",species="",dupl_loc="",current.rsid="",flag=0),stringsAsFactors=FALSE)
   Species_lines<-grep(" \\[[[:alpha:]]+[[:space:]][[:alpha:]]+\\]",webget)
   if(length(Species_lines!=0))
      {
      Species_text<-strsplitdbl(webget[Species_lines]," \\[","\\]")
      ThisPageSNPs$species<-alignsData(Species_lines,Species_text,LineRecords)
      warnText<-gsub("[[:print:]]*\\]([[:print:]]*)?","\\1",webget[Species_lines])
      ThisPageSNPs$flag[alignsData(Species_lines,warnText,LineRecords)!=""]<-1
      }
   SNPID_B_lines<-grep("cannot get document summary",webget) 
   SNPID_A_lines<-LineRecords[!(LineRecords %in% SNPID_B_lines)]

   if(length(SNPID_A_lines)!=0 & length(SNPID_B_lines)!=0)
       {
       SNPID_A_text<-strsplitdbl(webget[SNPID_A_lines],": "," \\[")
       SNPID_B_text<-paste("rs",strsplitdbl(webget[SNPID_B_lines],"id: "," Error occurred"),sep="") 
       ThisPageSNPs$SNPID_A<-as.character(alignsData(SNPID_A_lines,SNPID_A_text,LineRecords))
       ThisPageSNPs$SNPID_B<-as.character(alignsData(SNPID_B_lines,SNPID_B_text,LineRecords))
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
       ThisPageSNPs$marker<-gsub("^(rs[[:digit:]]+)[[:blank:]][[:print:]]*","\\1",webget[SNPID_A_lines])
   if(length(SNPID_A_lines)!=0 & length(SNPID_B_lines)==0)
       ThisPageSNPs$marker<-gsub("^(rs[[:digit:]]+)[[:blank:]][[:print:]]*","\\1",webget[SNPID_A_lines])
   GENE_lines<-grep("GENE=",webget)
   if(length(GENE_lines)!=0)
      GENE_text<-substr(webget[GENE_lines],6,nchar(webget[GENE_lines]))
   LOCUSID_lines<-grep("^LOCUS_ID=",webget)
   LOCUSID_text<-substr(webget[LOCUSID_lines],10,nchar(webget[LOCUSID_lines])-1)
   if(length(LOCUSID_lines)!=0)
      {
      ThisPageSNPs$genesymbol<-alignsData(GENE_lines,GENE_text,LineRecords)
      ThisPageSNPs$locusID<-alignsData(LOCUSID_lines,LOCUSID_text,LineRecords)
      }
   FXNCLASS_lines<-grep("^FXN_CLASS=",webget)
   FXNCLASS_text<-substr(webget[FXNCLASS_lines],11,nchar(webget[FXNCLASS_lines]))
   if(length(FXNCLASS_lines)!=0)
      ThisPageSNPs$fxn_class<-alignsData(FXNCLASS_lines,FXNCLASS_text,LineRecords)
   CHRPOS_lines<-grep("^CHROMOSOME BASE POSITION=",webget)
   dbl_lines<-grep("^CHROMOSOME BASE POSITION=+[[:alnum:]]+:[[:digit:]]+\\|",webget)
   if(length(dbl_lines)!=0)
      {
      dbl_text<-substr(webget[dbl_lines],26,nchar(webget[dbl_lines]))
      ThisPageSNPs$dupl_loc<-alignsData(dbl_lines,dbl_text,LineRecords)
      webget[dbl_lines]<-gsub("(\\|[[:alnum:]]+:+[[:digit:]]+)*$","",webget[dbl_lines])
      }
   CHRPOS_text<-substr(webget[CHRPOS_lines],26,nchar(webget[CHRPOS_lines]))
   CHR_text<-as.character(do.call(cbind,strsplit(CHRPOS_text,":"))[1,])
   if(length(CHRPOS_lines)!=0)
       {
       ThisPageSNPs$chr<-alignsData(CHRPOS_lines,CHR_text,LineRecords)
       BP_text<-as.numeric(do.call(cbind,strsplit(CHRPOS_text,":"))[2,])
       ThisPageSNPs$chrpos<-alignsData(CHRPOS_lines,BP_text,LineRecords)
       }
   ThisPageSNPs$LineRecords<-NULL
   return(ThisPageSNPs)
   }
