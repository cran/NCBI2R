get.uni.sts.create.mappingdf.from.chunk<-function(chunk)
   {
   rows<-0   ;     cells<-0
   MappingDF<-as.data.frame(cbind(rep("",100),rep("",100),rep("",100),rep("",100),rep("",100),rep("",100),rep("",100),rep("",100)),stringsAsFactors=FALSE)
   for(j in 1:length(chunk))
     {
     if(substr(chunk[j],1,3)=="<tr")
       {
       rows<-rows+1;  cells<-0
       }
     if(substr(chunk[j],1,3)=="<td")
       {
       cells<-cells+1     
       }   
      if(substr(chunk[j],nchar(chunk[j])-4,nchar(chunk[j]))=="</td>")       {
       if(chunk[j]=="</td>")           {
         CellContent<-splitfirst(chunk[j-1],"</a>")[1]
         } else {
         if(chunk[j]==" Map:</td>")             {
            CellContent<-paste(splitfirst(chunk[j-1],"</a>")[1],"Map:")
            } else {
            CellContent<-splitfirst(chunk[j],"</td>")[1]
            }
        }
       MappingDF[rows,cells]<-CellContent
       }
     }
   
   for(r in ncol(MappingDF):1)
     {
     if(length(unique(MappingDF[,r]))==1)
        MappingDF[,r]<-NULL
     }

   MappingDF[,4]<-NULL
   colnames(MappingDF)<-c("name","map","chr_source")
   MappingDF<-as.data.frame(cbind(MappingDF,chr="",PosLow="",PosHigh="",units="",Reference_Interval="",Lod_score="",stringsAsFactors=FALSE))
   MappingDF<-MappingDF[MappingDF[,"map"]!="",]
   for(i in 1:(nrow(MappingDF)-1)) 
     {
     if(MappingDF[i,"name"]=="&nbsp;" & MappingDF[i+1,"name"]=="&nbsp;")
        {
        firstentry<-i
        while(MappingDF[firstentry,"name"]=="&nbsp;")
          firstentry<-firstentry-1
        MappingDF[firstentry,gsub(" ","_",splitfirst(MappingDF[i+1,"map"],":")[1])]<-MappingDF[i+1,"chr_source"]
        MappingDF[i+1,"map"]<-""
        }
    }

   MappingDF<-MappingDF[MappingDF[,"map"]!="",]
   rows<-0

   for(i in 1:nrow(MappingDF))     {
      if(MappingDF[i,1]=="&nbsp;")          {
         rows<-rows+1
         KeyWord<-splitfirst(MappingDF[i, "map"], ":")[1]
         MappingDF[i-1,KeyWord]<-MappingDF[i,3]
         MappingDF[i,1]<-"a"
         }
     }
   MappingDF$chr<-as.character("")
   MappingDF$map<-gsub(" Map:","",MappingDF$map)
   MappingDF$units[grep("\\([[:alpha:]]+\\)$",MappingDF$Position)]<-do.call(rbind,strsplit(grep("\\([[:alpha:]]+\\)$",MappingDF$Position,value=TRUE)," "))[,2]
   MappingDF<-MappingDF[MappingDF$map!="Position:",]
   MappingDF$Position<-do.call(rbind,strsplit(MappingDF$Position," \\("))[,1]
   MappingDF[MappingDF$map=="Sequence","PosLow"]<-do.call(rbind,strsplit(MappingDF[MappingDF$map=="Sequence","Position"],"-"))[,1]
   MappingDF[MappingDF$map=="Sequence","PosHigh"]<-do.call(rbind,strsplit(MappingDF[MappingDF$map=="Sequence","Position"],"-"))[,2]
   MappingDF[MappingDF$map=="Sequence","Position"]<-"processed"
   return(MappingDF)
  }
