AnnotateDataframe <-
function(anydf,selections="",filename="",hyper="HYPERLINK",xldiv=";",smt=FALSE,sme=FALSE,div="---",keeplocusIDs=FALSE,keepNS=FALSE,kp=TRUE,quiet=TRUE,neigh=TRUE,showurl=FALSE,FlankingDistance=100000,suppressColCheck=FALSE)
   {
   if(class(anydf)!="data.frame")
      stop("NCBI2R error: object was not a data frame")
   if(selections[1]=="")
      {
      writeLines("NCBI2R advisory: No columns were selected in the data frame using the selections argument.")
      writeLines("NCBI2R advisory: Function will work on the first column and assume these are the list of markers.")
      selections<-names(anydf)[1]
      print(selections)
      print(anydf[,selections])
      }
   if(length(selections)==1 & !suppressColCheck)
      writeLines("NCBI2R advisory: only one column was selected from the data frame")

   if(selections[1]!="")
      {
      nonmatches<-selections[!(selections %in% colnames(anydf))]
      if(length(nonmatches)>0)
         {
         writeLines("The following columns were not found in the loaded data frame")
         writeLines(paste(nonmatches,collapse=","))
         stop("NCBI2R error. Please try again.")
         }
      }

   snplist<-unique(anydf[,selections[1]])
   if(class(snplist)!="character")
      stop("NCBI2R error: the marker list must be of character class. Factors not allowed")

   snps<-GetSNPInfo(snplist,showurl=showurl)
   print("GetSNPInfo has been performed and information for identified genes will now be found.")
   if(length(unique(snps$species[snps$species!=""]))!=1) 
      stop("NCBI2R error - More than one species in the snps dataframe. Can only continue this step if there is only one species.")
   flush.console()   
   org<-unique(snps$species[snps$species!=""])
   snps$species<-NULL

  snps<-SplitGenes(snps)
  genes<-try(GetGeneInfo(snps$locusID,div=div,quiet=quiet,showurl=showurl))
   if(class(genes)=="try-error")
     stop("NCBI2R error ADf-001: Unable to GetGeneInfo within function AnnotateDataframe")
   genes$org_ref_taxname<-NULL
   genes$org_ref_commonname<-NULL
   if(is.null(genes))
      {
      newsnps<-snps          
      } else {
      if(length(unique(genes$approx))==1 & genes$approx[1]==0)
         {
         writeLines("Information for genes has been found - no genes listed have interim titles (i.e. temporary and unofficial names)")
          } else {
         writeLines("Information for genes has been found - AT LEAST one gene listed has  titles that are temporary or unofficial names)")
         }
      newsnps<-try(MergeSNPsGenes(snps,genes,quiet=TRUE))
      if(class(newsnps)=="try-error")
        stop("NCBI2R error ADf-002: Unable to MergeSNPsGenes within function AnnotateDataframe")
      }
   flush.console()
   if(neigh){
     Nei<-try(GetNeighGenes(newsnps$chr,newsnps$chrpos,org=org,sme=sme,smt=smt,div=div,showurl=showurl,html=TRUE,FlankingDistance=FlankingDistance))
     if(class(Nei)=="try-error")
        stop("NCBI2R error ADf-003: Unable to use GetNeighGenes within function AnnotateDataframe")
     Nei$chr<-NULL
     names(Nei)[1:5]<-paste("N.",names(Nei)[1:5],sep="")
     flush.console()
     snpsNei<-as.data.frame(cbind(newsnps,Nei),stringsAsFactors=FALSE)
     } else {
     snpsNei<-newsnps
     }
   snpsNei<-order.to.original.list(anydf[,selections],snpsNei,keycol=c(selections[1],"marker"))
   if(filename!="")
      MakeHTML(snpsNei,filename,keeplocusIDs=keeplocusIDs,keepNS=keepNS,kp=kp)
   return(snpsNei)
   } 

