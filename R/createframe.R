createframe<-function(chunk)
  {
   i<-length(chunk)
   chunk<-gsub("<B>","",chunk)
   chunk<-gsub("</B>","",chunk)
   while(i>0)
      {
      while(length(splitfirst(chunk[i],">"))!=0 & !is.na(splitfirst(chunk[i],">")[2]))  
         {
         partA<-paste(splitfirst(chunk[i],">")[1],">",sep="")
         partB<-splitfirst(chunk[i],">")[2]
    
         chunk[i]<-partA
         for(tt in (length(chunk)):i) 
            chunk[tt+1]<-chunk[tt]
         chunk[i+1]<-partB
         i<-i+1
         }
      i<-i-1
      }
   rows<-0
   cells<-0

   anydf<-as.data.frame(cbind(rep("",100),rep("",100),rep("",100),rep("",100),rep("",100),rep("",100),rep("",100),rep("",100)),stringsAsFactors=FALSE)
   for(j in 1:length(chunk))
      {
      if(substr(chunk[j],1,3)=="<tr")  {
         rows<-rows+1 
         cells<-0 
       }
     if(substr(chunk[j],1,3)=="<td")
       cells<-cells+1
     if(substr(chunk[j],nchar(chunk[j])-4,nchar(chunk[j]))=="</td>")   {
       if(chunk[j]=="</td>")  {

         CellContent<-splitfirst(chunk[j-1],"</a>")[1]
         } else {
         if(chunk[j]==" Map:</td>") {
            CellContent<-paste(splitfirst(chunk[j-1],"</a>")[1],"Map:")
            } else {
            CellContent<-splitfirst(chunk[j],"</td>")[1]
            }
        }
       anydf[rows,cells]<-CellContent
       }
     }
   return(anydf)
  }
