AnnotateDataframe <-
function(anydf,selections="",filename="",hyper="HYPERLINK",xldiv=";",smt=FALSE,sme=FALSE,div="---",keeplocusIDs=FALSE,keepNS=FALSE,kp=TRUE,quiet=TRUE,neigh=TRUE,showurl=FALSE)
   {
   if(class(anydf)!="data.frame")
     stop("object was not a data frame")
   if(selections[1]=="")
     print("no columns were selected in the data frame. Program will proceed assuming these are the list of markers.")
   if(length(selections)==1)
     print("WARNING: only one column was selected from the data frame")
   if(nrow(anydf)>200)
      print(paste("Your data frame is",nrow(anydf),"lines long. Program will continue - this is just advice"))
   if(selections[1]!="")
      {
      for(i in length(selections))
         {
         if(!(selections[i] %in% colnames(anydf)))
            {
            print(paste("The column name you specified (",selections[i],") was not found in the loaded data frame"))
            print(paste("The column names were:"))
            print(colnames(anydf))
            stop(print("Please try again."))
            }
         }
      }
   snplist<-anydf[,selections[1]]
   snps<-GetSNPInfo(snplist,showurl=showurl)
   print("GetSNPInfo has been performed and now information for identified genes will now be found.")
   if(length(unique(snps$species[snps$species!=""]))!=1) 
      stop("NCBI2R error - More than one species in the snps dataframe. Can only continue this step if there is only one species.")
   flush.console()   
   org<-unique(snps$species[snps$species!=""])
   snps$species<-NULL
   snps<-as.data.frame(cbind(snps,anydf[,selections[2:length(selections)]]))
   genes<-GetGeneInfo(snps$locusID,div=div,quiet=quiet,showurl=showurl)
   genes$org_ref_taxname<-NULL
   genes$org_ref_commonname<-NULL
     if(is.null(genes))
      {
      newsnps<-snps          
      } else {
      if(length(unique(genes$approx))==1 & genes$approx[1]==0)  {
         
         print("Information for genes has been found - no genes listed have interim titles (i.e. temporary and unofficial names)")
         genes$approx<-NULL
         } else {
         print("Information for genes has been found - AT LEAST one gene listed has  titles that are temporary or unofficial names)")
        }
      newsnps<-MergeSNPsGenes(snps,genes,quiet=TRUE)  
      }
   flush.console()
   if(neigh){
     Nei<-GetNeighGenes(newsnps$chr,newsnps$chrpos,org=org,sme=sme,smt=smt,div=div,showurl=showurl,html=TRUE)
     snpsNei<-as.data.frame(cbind(newsnps,Nei))
     } else {
     snpsNei<-newsnps
     }
   if(filename!="") 
      MakeHTML(snpsNei,filename,keeplocusIDs=keeplocusIDs,keepNS=keepNS,kp=kp)
   return(snpsNei)
   } 

