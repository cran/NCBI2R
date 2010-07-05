SplitGenes <-
function(snpdf,quiet=FALSE)
   {
   if(colnames(snpdf)[1]!="marker")
      stop("Could you please make sure the first column is called marker and contains the SNP identifiers")
   if(colnames(snpdf)[2]!="genesymbol")
      stop("Could you please make sure that the second column is called genesymbol and contains the gene symbols, eg CLN5")
   if(colnames(snpdf)[3]!="locusID")
      stop("Could you please make sure that the second column is called locusID, eg 1203")
   snpdf$ordering<-1:nrow(snpdf) 
   LC<-1
   firsttime<-0
   while(LC!=(nrow(snpdf)+1))     
      {
      while(length(splitfirst(snpdf$locusID[LC],","))>0)
         {
         if(quiet==FALSE)
            {
            if(firsttime==0)
               {
               firsttime<-1
               print("The following SNP/s are located within genes")
               }
            print(snpdf$marker[LC])
            }
         res<-splitfirst(snpdf$locusID[LC],",")
         snpdf$locusID[LC]<-res[1] 
         snpdf$genesymbol[LC]<-as.character(GetGeneNames(snpdf$locusID[LC])$genesymbol) 
         if(length(splitfirst(res[2],","))==0)
            thisrow<-data.frame(snpdf[LC,1],GetGeneNames(res[2])$genesymbol,res[2],snpdf[LC,c(4:ncol(snpdf))])
         if(length(splitfirst(res[2],","))!=0)
            thisrow<-data.frame(snpdf[LC,1],"tempSYMB",res[2],snpdf[LC,c(4:ncol(snpdf))])
         colnames(thisrow)<-colnames(snpdf)  
         snpdf<-rbind(snpdf,thisrow)
         }
      LC<-LC+1
      }
   snpdf<-snpdf[order(snpdf$ordering),]
   row.names(snpdf)<-1:nrow(snpdf)
   snpdf$ordering<-NULL   
   return(snpdf)
   }

