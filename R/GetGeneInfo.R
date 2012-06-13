GetGeneInfo<-function(locusIDs,batchsize=200,xldiv=";",int=FALSE,go=FALSE,showurl=FALSE,quiet=TRUE,sme=FALSE,smt=FALSE,div="---",html=FALSE)
   {
   locusID<-locusIDs
   if(missing(locusID))                                       
      stop("no locusID provided")
   if(class(locusID)=="data.frame")
      stop("You have provided a data.frame instead of ID numbers")
   if(class(locusID)=="character")
      locusID<-locusID[locusID!=""]
   if(length(locusID)==0)
      return()
   if(class(locusID)=="numerical")
      locusID<-as.character(locusID[locusID!=0])
   if(length(locusID[substr(locusID,1,2)=="rs"])!=0)
     stop("NCBI2R error: You appear to have used a list of SNPs instead of Entrez locus identifiers.")
   if(length(locusID)>0)
     {
     if(length(grep(",",locusID))>0) 
       locusID<-unlist(strsplit(locusID,","))  
     }
   locusID<-unique(locusID)
   locusID<-locusID[locusID!=""] 
   if(quiet==FALSE)
      print(paste("Unique genes:",length(locusID)))
   URLdef<-ncbi2r.options()
   Num<-length(locusID)
   genedf<-data.frame(org_ref_taxname=rep("",length(locusID)),org_ref_commonname="",OMIM="",synonyms="", genesummary="", genename="",phenotypes="", phenotypes.html="", pathways="",pathways.html="", GeneLowPoint=0,GeneHighPoint=0,ori="",chr="",genesymbol="",Int.GeneIDs="",Int.genesymbols.html="",GOfunc="",GOcomp="",GOproc="",GOfunc.html="",GOcomp.html="",GOproc.html="", build=0, cyto="",approx=0,stringsAsFactors=FALSE)  
   genedf<-as.data.frame(cbind(locusID,genedf))     
   TotalBatches<-ceiling(length(locusID)/batchsize)
   for(BatchLoop in 1:TotalBatches)
      {
      BatchOffset<-((BatchLoop-1)*batchsize)
      CountOfThisBatch<-0
      url_piece<-"&id="
        while((CountOfThisBatch<batchsize) & ((CountOfThisBatch+BatchOffset)<length(locusID)))
         {
         CountOfThisBatch<-CountOfThisBatch+1
         url_piece<-paste(url_piece,locusID[(CountOfThisBatch+BatchOffset)],",",sep="")
         }
       url_piece<-substr(url_piece,1,nchar(url_piece)-1)
      getURL<-paste(URLdef$front,"efetch.fcgi?db=gene",url_piece,"+gene%20%all[filter]&rettype=XML",URLdef$back,sep="")
      webget<-get.file(getURL,quiet=quiet,showurl=showurl,clean=TRUE)
      BatchItemNum<-0
      LC<-1
      while(LC<=length(webget))
         {
         remain_pc<-floor(100*(length(webget)-LC)/length(webget))
         cat(paste("\r NCBI2R GetGeneInfo Batch ",BatchLoop," of ",TotalBatches," of max size ", batchsize," ",remain_pc,"%     ",sep=""))
         flush.console()
         if(substr(webget[LC],nchar(webget[LC])-11,nchar(webget[LC]))=="<Entrezgene>")
            BatchItemNum<-BatchItemNum+1
         if(substr(webget[LC],1,35)=="There is no record in DB for GeneID")
            {
            BatchItemNum<-BatchItemNum+1
            }   
         if(substr(webget[LC],1,9)=="<Org-ref>")
            {
            genedf$org_ref_taxname[BatchItemNum+BatchOffset]<-substr(webget[LC+1],18,nchar(webget[LC+1])-18)
            genedf$org_ref_commonname[BatchItemNum+BatchOffset]<-substr(webget[LC+2],17,nchar(webget[LC+2])-17)
            }
         if(substr(webget[LC],1,25)=="<SubSource_subtype value=")
            {
            tmp.chr<-substr(webget[LC+1],17,nchar(webget[LC+1])-17)
            if(genedf$chr[BatchItemNum+BatchOffset]=="")
               genedf$chr[BatchItemNum+BatchOffset]<-tmp.chr
            else
               genedf$chr[BatchItemNum+BatchOffset]<-paste(genedf$chr[BatchItemNum+BatchOffset],tmp.chr,sep="")
            }
         if(substr(webget[LC],1,13)=="<Dbtag_db>MIM")
            genedf$OMIM[BatchItemNum+BatchOffset]<-substr(webget[LC+3],15,nchar(webget[LC+3])-15)
         if(substr(webget[LC],1,17)=="<Gene-ref_maploc>")
            genedf$cyto[BatchItemNum+BatchOffset]<-substr(webget[LC],18,nchar(webget[LC])-18)
         if(substr(webget[LC],1,60)=="<Gene-commentary_heading>RefSeqs of Annotated Genomes: Build")
            genedf$build[BatchItemNum+BatchOffset]<-substr(webget[LC],62,nchar(webget[LC])-26)
         if(substr(webget[LC],1,38)=="<Gene-commentary_label>Official Symbol")
            genedf$genesymbol[BatchItemNum+BatchOffset]<-substr(webget[LC+1],23,nchar(webget[LC+1])-23)
         if(substr(webget[LC],1,37)=="<Gene-commentary_label>Interim Symbol")
            {
            genedf$genesymbol[BatchItemNum+BatchOffset]<-substr(webget[LC+1],23,nchar(webget[LC+1])-23)
            genedf$approx[BatchItemNum+BatchOffset]<-1
            }
         if(substr(webget[LC],1,41)=="<Gene-commentary_label>Official Full Name")
            genedf$genename[BatchItemNum+BatchOffset]<-substr(webget[LC+1],23,nchar(webget[LC+1])-23)
         if(substr(webget[LC],1,40)=="<Gene-commentary_label>Interim Full Name")
            {
            genedf$genename[BatchItemNum+BatchOffset]<-substr(webget[LC+1],23,nchar(webget[LC+1])-23)
            genedf$approx[BatchItemNum+BatchOffset]<-1
            }
         if(substr(webget[LC],1,14)=="<Gene-ref_syn>")
              {
              synonymstring<-""
              while(substr(webget[LC+1],1,16)=="<Gene-ref_syn_E>")
                 {
                 LC<-LC+1
                 if(synonymstring=="") {
                    synonymstring<-substr(webget[LC],17,nchar(webget[LC])-17)
                    } else  {  synonymstring<-paste(synonymstring,substr(webget[LC],17,nchar(webget[LC])-17),sep=" ")
                    }
                 }
              genedf$synonyms[BatchItemNum+BatchOffset]<-synonymstring
              }
           if(substr(webget[LC],1,19)=="<Entrezgene_summary")
               genedf$genesummary[BatchItemNum+BatchOffset]<-substr(webget[LC],21,nchar(webget[LC])-21)
           if(substr(webget[LC],1,37)=="<Gene-commentary_heading>Interactions" & int==TRUE)
              {
              Intstuff<-get.int.int(webget,LC)
              sqe12<-convert.int(Intstuff$Interactions)
              LC<-Intstuff$LC
              genedf$Int.genesymbols.html[BatchItemNum+BatchOffset]<-sqe12$int.html
              genedf$Int.GeneIDs[BatchItemNum+BatchOffset]<-sqe12$genelist
              }
            if(substr(webget[LC],1,33)=="<Gene-commentary_heading>Pathways")
               {
               Pathstuff<-get.pathways.int(webget,LC)
               sqe13<-ConvertDFToHTML(Pathstuff$Pathways,div=div)
               LC<-Pathstuff$LC

               genedf$pathways[BatchItemNum+BatchOffset]<-sqe13$names
               genedf$pathways.html[BatchItemNum+BatchOffset]<-sqe13$names.html
               }
            if(substr(webget[LC],1,35)=="<Gene-commentary_heading>Phenotypes")
               {
               Phenstuff<-get.phenotypes.int(webget,LC)
               sqe14<-ConvertDFToHTML(Phenstuff$Phenotypes,div=div)
               LC<-Phenstuff$LC
               genedf$phenotypes[BatchItemNum+BatchOffset]<-sqe14$names
               genedf$phenotypes.html[BatchItemNum+BatchOffset]<-sqe14$names.html
               }
             if(substr(webget[LC],1,38)=="<Gene-commentary_heading>GeneOntology<" & go==TRUE)
                 {
                 GOstuff<-get.go.int(webget,LC)
                 LC<-GOstuff$LC
                 sdqf16<-convert.go(GOstuff$GO)
                 genedf$GOfunc[BatchItemNum+BatchOffset]<-sdqf16$func
                 genedf$GOcomp[BatchItemNum+BatchOffset]<-sdqf16$comp
                 genedf$GOproc[BatchItemNum+BatchOffset]<-sdqf16$proc
                 genedf$GOfunc.html[BatchItemNum+BatchOffset]<-sdqf16$func.html
                 genedf$GOcomp.html[BatchItemNum+BatchOffset]<-sdqf16$comp.html
                 genedf$GOproc.html[BatchItemNum+BatchOffset]<-sdqf16$proc.html
                }
            if(substr(webget[LC],1,54)=="<Gene-commentary_heading>RefSeqs of Annotated Genomes:")
               {
               Checker<-finder("<Seq-interval_from>","</Gene-commentary_seqs>",webget,LC)
               if(Checker$Object==1)
                  {
                  genedf$GeneLowPoint[BatchItemNum+BatchOffset]<-as.numeric(substr(webget[Checker$RowNumber],20,nchar(webget[Checker$RowNumber])-20))+1
                  genedf$GeneHighPoint[BatchItemNum+BatchOffset]<-as.numeric(substr(webget[Checker$RowNumber+1],18,nchar(webget[Checker$RowNumber+1])-18))+1
                  genedf$ori[BatchItemNum+BatchOffset]<-substr(webget[Checker$RowNumber+3],24,nchar(webget[Checker$RowNumber+3])-8)


                  if(length(grep("plus",webget[Checker$RowNumber+3]))==1)
                    genedf$ori[BatchItemNum+BatchOffset]<-"+"
                  if(length(grep("minus",webget[Checker$RowNumber+3]))==1)
                    genedf$ori[BatchItemNum+BatchOffset]<-"-"
                  }
               }
            LC<-LC+1
         }
      }
  cat("\r                                                           ")
  cat("\n")
   if(go==FALSE)
      {
      genedf$GOfunc<-NULL;       genedf$GOcomp<-NULL
      genedf$GOproc<-NULL;       genedf$GOfunc.html<-NULL
      genedf$GOcomp.html<-NULL;  genedf$GOproc.html<-NULL
      }
   if(int==FALSE)
      {
      genedf$Int.genesymbols.html<-NULL
      genedf$Int.GeneIDs<-NULL
      }   
   if(html==FALSE)
      {
      genedf$pathways.html<-NULL
      genedf$phenotypes.html<-NULL
      }
   genedf$genesummary<-gsub("&amp;apos;","'",genedf$genesummary)
   for(i in 1:nrow(genedf))
      {
      if(genedf$genesymbol[i]=="")
         {
         if(length(grep(" ",genedf$synonyms[i]))==0)
            {
            genedf$genesymbol[i]<-genedf$synonyms[i]
            genedf$approx[i]<-(genedf$approx[i])+2
            }
         }
      }
   return(genedf)
   }
