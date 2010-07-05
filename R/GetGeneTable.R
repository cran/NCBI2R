GetGeneTable <-
function(locusID,showurl=FALSE)
   {
   URLdef<-URLdefinitions()
   getURL<-paste(URLdef$front,"efetch.fcgi?db=gene&id=",locusID,"&rettype=gene_Table",URLdef$back,sep="")
   webget<-get.file(getURL,showurl,clean=FALSE)   
   CurrentRow<-1
   dummy<-0
   ExonSet<-0
   ExonInfo<-data.frame(Where=rep("",30000),Start=0,Stop=0,Size=0,Set=0,stringsAsFactors=FALSE)
   ACC.DNA<-data.frame(Identifier=rep("",200),Length=0,Exons=0,stringsAsFactors=FALSE)
   ACC.Prot<-ACC.DNA   
   while(webget[CurrentRow]!="mRNA   bp   exons   Protein   aa   exons" & CurrentRow<length(webget))
      CurrentRow<-CurrentRow+1
   if(CurrentRow<length(webget)) {
      TotalExonSets<-0
      CurrentRow<-CurrentRow+1
      while(webget[CurrentRow]!="Exon information:")
         {
         CurrentRow<-CurrentRow+1
         TotalExonSets<-TotalExonSets+1
         }
      WriteHere<-0
      for(i in 1:TotalExonSets)
         {
         ACC.DNA$Identifier[i]<-splitfirst(webget[CurrentRow+1])[1]
         rem<-splitfirst(splitfirst(webget[CurrentRow+1])[2])[2]
         ACC.DNA$Length[i]<-as.numeric(splitfirst(rem)[1])
         ACC.DNA$Exons[i]<-as.numeric(splitfirst(rem,": ")[2])
         ACC.Prot$Identifier[i]<-splitfirst(webget[CurrentRow+2])[1]
         rem<-splitfirst(splitfirst(webget[CurrentRow+2])[2])[2]
         ACC.Prot$Length[i]<-as.numeric(splitfirst(rem)[1])
         ACC.Prot$Exons[i]<-as.numeric(splitfirst(rem,": ")[2])
         CurrentRow<-CurrentRow+4    
        while(substr(webget[CurrentRow+1],nchar(webget[CurrentRow+1])-1,nchar(webget[CurrentRow+1]))=="bp" & CurrentRow<length(webget))
            {
            startposition<-1
            currentposition<-1
            CurrentRow<-CurrentRow+1
            TL<-webget[CurrentRow]
            a<-0
            ExonSet<-ExonSet+1
            while(currentposition<nchar(TL))
               {
               a<-a+1
               WriteHere<-WriteHere+1
               while(substr(TL,currentposition,currentposition)!= "-")
                  currentposition<-currentposition+1
               ExonInfo$Start[WriteHere]<-(substr(TL,startposition,currentposition-2))
               startposition<-currentposition
               while(substr(TL,currentposition,currentposition+2)!= "   ")
                  currentposition<-currentposition+1
               ExonInfo$Stop[WriteHere]<-(substr(TL,startposition+2,currentposition-1))
               startposition<-currentposition
               while(substr(TL,currentposition,currentposition+2)!= " bp")
                  currentposition<-currentposition+1
               ExonInfo$Size[WriteHere]<-(substr(TL,startposition+3,currentposition-1))
               currentposition<-currentposition+5
               startposition<-currentposition
               if(a==1)
                  ExonInfo$Where[WriteHere]<-"Exon"
               if(a==2)
                  ExonInfo$Where[WriteHere]<-"CodExon"
               if(a==3)
                  ExonInfo$Where[WriteHere]<-"Intron"
               ExonInfo$Set[WriteHere]<-i
               } 
            } 
         } 
     return(list(ExonInfo=ExonInfo[ExonInfo$Where!="",],ACC.DNA=ACC.DNA[ACC.DNA$Identifier!="",],ACC.Prot=ACC.Prot[ACC.Prot$Identifier!="",]))
      } else {
      return("No information available") 
      }
   }  
