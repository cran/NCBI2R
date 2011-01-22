GetGeneNames <-
function(locusID,batchsize=200,showurl=FALSE)
   {
   NumberOfGenesFound<-length(locusID)
   if(NumberOfGenesFound!=0)
      minigenedf<-data.frame(genename=rep("",NumberOfGenesFound),genesymbol="",NewlocusID="",CurrentRecord="",LastUpdate="",locusID=0,species="",stringsAsFactors=FALSE)
   CurrentGeneforURL<-1
   BatchOffset<-0       
   OffsetCounter<-0
   URLdef<-ncbi2r.options()
   while(CurrentGeneforURL<=length(locusID))
      {
      url_piece<-""
      BatchCounter<-1
      while((CurrentGeneforURL<=length(locusID))&(BatchCounter<=batchsize))
         {
         url_piece<-paste(url_piece,"&id=",locusID[CurrentGeneforURL],sep="")
         BatchCounter<-BatchCounter+1   
         CurrentGeneforURL<-CurrentGeneforURL+1
         }
      getURL<-paste(URLdef$front,"efetch.fcgi?db=gene",url_piece,"&report=gene_table&mode=text",URLdef$back,sep="")
      webget<-get.file(getURL,showurl,clean=FALSE)
      OffsetCounter<-OffsetCounter+1
      BatchOffset<-(OffsetCounter*batchsize)-batchsize 
      CurrentGeneOfThisBatch<-0
      LineOfWebData<-1
      while(LineOfWebData<=length(webget))
         {
         Test1<-FALSE
         Test1<-substr(webget[LineOfWebData],1,(nchar(as.character(CurrentGeneOfThisBatch+1))+1))==paste(CurrentGeneOfThisBatch+1,":",sep="")
         Test2<-FALSE
         if(nchar(webget[LineOfWebData])>38)
            Test2<-substr(webget[LineOfWebData],1,38)=="There is no record in DB for GeneID = "
         if(Test1 | Test2)
            {
            CurrentGeneOfThisBatch<-CurrentGeneOfThisBatch+1
            if((substr(webget[LineOfWebData],1,38)=="There is no record in DB for GeneID = ") & (nchar(webget[LineOfWebData])>46))
                CurrentGeneOfThisBatch<-CurrentGeneOfThisBatch+1
            if(Test1)
               {
               temp<-substr(webget[LineOfWebData],nchar(webget[LineOfWebData])-1,nchar(webget[LineOfWebData]))
               LineValid<-FALSE
               if(temp=="] " | temp==" ]")
                  LineValid<-TRUE
               TitleLine<-webget[LineOfWebData]
               while(LineValid==FALSE)
                  {
                  TitleLine<-paste(TitleLine,webget[LineOfWebData+1],sep=" ") 
                  LineOfWebData<-LineOfWebData+1
                  temp<-substr(TitleLine,nchar(TitleLine)-1,nchar(TitleLine))
                  LineValid<-FALSE
                  if(temp=="] " | temp==" ]"){LineValid<-TRUE}
                  }
               LineRemainder<-splitfirst(TitleLine)[2]  
               minigenedf$genesymbol[CurrentGeneOfThisBatch+BatchOffset]<-splitfirst(LineRemainder)[1]
               GeneName_Species<-splitfirst(LineRemainder)[2]
               temp<-splitfirst(GeneName_Species,"[")
               if(length(temp)==1)
                  minigenedf$species[CurrentGeneOfThisBatch+BatchOffset]<-splitfirst(substr(temp[1],2,nchar(temp[1]))," ]")[1]
               if(length(temp)!=1) 
                  {   
                  minigenedf$genename[CurrentGeneOfThisBatch+BatchOffset]<-substring(temp[1],1,nchar(temp[1])-1)
                  minigenedf$species[CurrentGeneOfThisBatch+BatchOffset]<-splitfirst(substr(temp[2],2,nchar(temp[2])),"]")[1]
                  if(substr(minigenedf$species[CurrentGeneOfThisBatch+BatchOffset],nchar(minigenedf$species[CurrentGeneOfThisBatch+BatchOffset]),nchar(minigenedf$species[CurrentGeneOfThisBatch+BatchOffset]))==" ")
                     minigenedf$species[CurrentGeneOfThisBatch+BatchOffset]<-substr(minigenedf$species[CurrentGeneOfThisBatch+BatchOffset],1,nchar(minigenedf$species[CurrentGeneOfThisBatch+BatchOffset])-1)
                  }
               }
            } else {
            if(substr(webget[LineOfWebData],1,8)=="GeneID: ")
               {
               temp<-splitfirst(webget[LineOfWebData]," ",9)[1]  
               minigenedf$locusID[CurrentGeneOfThisBatch+BatchOffset]<-substr(temp,9,nchar(temp))  
               minigenedf$CurrentRecord[CurrentGeneOfThisBatch+BatchOffset]<-splitfirst(splitfirst(webget[LineOfWebData]," ",9)[2])[1]
               minigenedf$LastUpdate[CurrentGeneOfThisBatch+BatchOffset]<-splitfirst(splitfirst(webget[LineOfWebData]," ",9)[2])[2]
               }
            if(substr(webget[LineOfWebData],1,16)=="This record was ")
               {
               temp<-substr(webget[LineOfWebData],17,nchar(webget[LineOfWebData]))
               if(substring(temp,1,8)=="replaced")
                  minigenedf$NewlocusID[CurrentGeneOfThisBatch+BatchOffset]<-substring(temp,23,nchar(temp))
               if(substring(temp,1,12)=="discontinued")
                  minigenedf$NewlocusID[CurrentGeneOfThisBatch+BatchOffset]<-0
               } 
            } 
         LineOfWebData<-LineOfWebData+1
         }
      }
   if(NumberOfGenesFound!=0)
      return(minigenedf)
   }  
