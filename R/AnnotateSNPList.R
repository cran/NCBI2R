AnnotateSNPList <-
function(snplist,filename="",hyper="HYPERLINK",xldiv=";",smt=FALSE,sme=FALSE,div="---",keeplocusIDs=FALSE,keepNS=FALSE,kp=TRUE,quiet=TRUE,neigh=TRUE,showurl=FALSE)
   {                          
   snps<-GetSNPInfo(snplist,showurl=showurl)
   print("GetSNPInfo has been performed and now information for identified genes will now be found.")
   if(length(unique(snps$species[snps$species!=""]))!=1)  
      stop("more than one species was found in the SNPs dataframe.")
   flush.console()   
   org<-unique(snps$species[snps$species!=""])
   snps$species<-NULL
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

   if(neigh){
     Nei<-GetNeighGenes(newsnps$chr,newsnps$chrpos,org=org,sme=sme,smt=smt,div=div,showurl=showurl)
     flush.console()
     snpsNei<-as.data.frame(cbind(newsnps,Nei))
     } else {
     snpsNei<-newsnps
     }
   if(filename!="")
      MakeHTML(snpsNei,filename,keeplocusIDs=keeplocusIDs,keepNS=keepNS,kp=kp)  
   return(snpsNei) 
   }

