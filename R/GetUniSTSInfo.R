GetUniSTSInfo <-
function(stsid,taxid=9606,showurl=FALSE,quiet=TRUE)
   {
   if(quiet==FALSE)
      print(paste("running GetUniSTSInfo",stsid))
   if(length(stsid)>1)
      stop("only one id number at a time for the function GetUniSTSInfo. sorry.")
   getURL<-paste("http://www.ncbi.nlm.nih.gov/genome/sts/sts.cgi?uid=",stsid,sep="")
   webget<-get.file(getURL,quiet=quiet,showurl=showurl,clean=FALSE)

   findthis<-paste(";<a name = taxid",taxid,">",sep="")
   StartLine<-grep(findthis,webget) 
   if(length(StartLine)==0) 
     stop(print(paste("unists ",stsid,":No information for this unists on the requested species(",taxid,")")))
   warn<-grep("<font color=\"#FF0066\">Warning!",webget)
   warningstring<-"None"
   if(length(warn)!=0)   {
     print("NCBI2R - UniSTS - warnings have been indentified")
   
     places<-grep("<td colspan =\"2\" class =\"H2\" bgcolor =CCCCFF>&nbsp;<a name = taxid",webget)  
     if(length(warn)>1)
        warningstring<-"More than warning found. Check the NCBI webpage."
     if(length(warn==1))
       {
       if(warn<StartLine)   {
         warningstring<-"Warning was found but for different species. Consider checking."
         } else {
            
             if(pmatch(StartLine,places)!=length(places))   {
                NextSpeciesStart<-places[pmatch(StartLine,places)+1]
                if(warn>StartLine & warn<NextSpeciesStart)
                  warningstring<-splitfirst(webget[warn+1],"<BR>")[1]
                 } else {
                 
                 warningstring<-splitfirst(webget[warn+1],"<BR>")[1]
                 
                 }
         }
      }
     } 
   findthis<-paste(";<a name = taxid",taxid,">",sep="")
   StartLine<-grep(findthis,webget)
      
   if(length(StartLine)==0) 
     stop(print(paste("unists ",stsid,":No information for this unists on the requested species(",taxid,")")))

  StartCrossReference<-grep("<!--- Cross reference --->",webget)
  StartCrossReference<-StartCrossReference[StartCrossReference>StartLine][1]
  CrossRefAvailable<-TRUE
  if(substr(webget[StartCrossReference+1],1,5)=="<!---")
      {
      CrossRefAvailable<-FALSE 
      } else {
      chunk1<-GetTable(webget,StartCrossReference)
      if(chunk1[2]== "</table><br>")
         CrossRefAvailable<-FALSE
      }
   CrossRefError<-"None"
   if(CrossRefAvailable==FALSE)
      {
      CrossRefError<-("No Cross Reference info available")
      } else {
      www<-createframe(chunk1) 
      CrossRef<-cleanframe(www)
      CrossRef<-as.data.frame(cbind(CrossRef,GeneID=rep(0),Symbol=rep(""),UniGene=rep(""),Description=rep(""),Position=rep(""),stringsAsFactors=FALSE))
      i<-0
      lastheading<-""
      while(i<nrow(CrossRef))
         {
         i<-i+1
         ExpectedResponses<-c("Gene","UniGene")

         if(CrossRef[i,1]=="&nbsp;" & lastheading=="UniGene")
            CrossRef[i,1]<-lastheading
         if(is.na(pmatch(CrossRef[i,1],ExpectedResponses))==TRUE)
            stop("NCBI2R error: GetUniSTSInfo error 4 - bad parse of record")
         if(CrossRef[i,1]=="Gene")
            {
            toprow<-i
            bottomrow<-toprow

            CrossRef[toprow,"GeneID"]<-CrossRef[i,3] 
                                       
            while(i<nrow(CrossRef) & CrossRef[i+1,1]=="&nbsp;")
               {
               i<-i+1
               CrossRef[toprow,splitfirst(CrossRef[i,2],":")]<-CrossRef[i,3]
               }
           lastheading<-"Gene"
            }
         if(CrossRef[i,1]=="UniGene")
            {
            CrossRef[i,"UniGene"]<-CrossRef[i,2]
            CrossRef[i,"Description"]<-CrossRef[i,3]
            lastheading<-"UniGene"
            } 
        }
     CrossRef<-CrossRef[CrossRef$V1!="&nbsp;",]
     CrossRef$V1<-NULL    
     CrossRef$V2<-NULL    
     CrossRef$V3<-NULL

     CrossRef<-CrossRef[order(CrossRef$Description,decreasing=TRUE),]     

     if((length(unique(CrossRef$Description))!=length(CrossRef$Description)) & (nrow(CrossRef)>1))
       {
       
        for(j in 1:(nrow(CrossRef)-1))
           {
           if((CrossRef[j,"Description"]!="") & (CrossRef[j,"Description"]==CrossRef[j+1,"Description"]) & (CrossRef[j,"UniGene"]=="") & CrossRef[j,"GeneID"]!=0)
             {
             CrossRef[j,"UniGene"]<-CrossRef[j+1,"UniGene"]
             CrossRef[j+1,"UniGene"]<-""
             CrossRef[j+1,"Description"]<-""
             }
           CrossRef<-CrossRef[CrossRef$GeneID!=0 | CrossRef$UniGene!="",]  
        }
       } 
     
     CrossRef$GeneID<-as.numeric(CrossRef$GeneID)
     CrossRef<-CrossRef[order(CrossRef$Symbol),]
     
     }

   MappingError<-"None"
   StartMapping<-grep("<!--- Mapping data --->",webget)
   StartMapping<-StartMapping[StartMapping>StartLine][1]
   if(substr(webget[StartMapping+1],1,5)=="<!---")    {
      MappingError<-paste("unists ",stsid,":No mapping info available for this id number",sep="")
      } else {
      chunk<-GetTable(webget,StartMapping)
      i<-length(chunk)
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

   MappingDF<-as.data.frame(cbind(rep("",100),rep("",100),rep("",100),rep("",100),rep("",100),rep("",100),rep("",100),rep("",100)),stringsAsFactors=FALSE)

   for(j in 1:length(chunk))
     {
     if(substr(chunk[j],1,3)=="<tr")
       {
       rows<-rows+1 
       cells<-0 
       }
     if(substr(chunk[j],1,3)=="<td")
       {         cells<-cells+1     }
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
   for(i in 1:(nrow(MappingDF)-1))  {

     if(MappingDF[i,"name"]=="&nbsp;" & MappingDF[i+1,"name"]=="&nbsp;")   {
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
   MappingDF[MappingDF$map=="Sequence","Position"<-"processed"]
 
  
   sup_maps<-c("MARC","deCODE","Marshfield","Whitehead-YAC","RH", "Stanford-G3", "Whitehead-RH", "GeneMap99-GB4","Genethon","NCBI RH","TNG","GeneMap99-G3")
   MappingDF[MappingDF$map %in% sup_maps,"PosLow"]<-do.call(rbind,strsplit(MappingDF[MappingDF$map %in% sup_maps,"Position"]," \\("))[,1]

   MappingDF[MappingDF$map %in% sup_maps,"PosHigh"]<-do.call(rbind,strsplit(MappingDF[MappingDF$map %in% sup_maps,"Position"]," \\("))[,1]

   MappingDF[MappingDF$map %in% sup_maps,"Position"]<-"processed" 

    MappingDF[MappingDF$map=="Sequence" & grep("\\|",MappingDF$chr_source,invert=TRUE),"chr_source"]<-paste(MappingDF[MappingDF$map=="Sequence" & grep("\\|",MappingDF$chr_source,invert=TRUE),"chr_source"],"|NCBI")
   
          
      MappingDF[grep("\\|",MappingDF$chr_source),"source"]<-do.call(rbind,strsplit(MappingDF[grep("\\|",MappingDF$chr_source),"chr_source"],"\\|"))[,2]
      MappingDF[grep("\\|",MappingDF$chr_source),"chr"]<-do.call(rbind,strsplit(MappingDF[grep("\\|",MappingDF$chr_source),"chr_source"],"\\|"))[,1]
      MappingDF[grep("\\|",MappingDF$chr_source,invert=TRUE),"chr"]<-MappingDF[grep("\\|",MappingDF$chr_source,invert=TRUE),"chr_source"]
   MappingDF[grep(" ",MappingDF$chr),"chr"]<-do.call(rbind,strsplit(MappingDF[grep(" ",MappingDF$chr),"chr"]," "))[,2]   
    MappingDF$source<-RemoveSpaces(MappingDF$source)
   MappingDF$Position<-NULL 
   MappingDF$chr_source<-NULL
   MappingDF$units<-gsub("\\(|\\)","",MappingDF$units)
   if(!is.na(pmatch("source",colnames(MappingDF))))
      MappingDF[!is.na(MappingDF$source),"map"]<-paste(MappingDF[!is.na(MappingDF$source),"source"],"-",MappingDF[!is.na(MappingDF$source),"map"],sep="") 
   MappingDF$source<-NULL
   colnames(MappingDF)<-gsub(" ","_",colnames(MappingDF))

  } 
   if(CrossRefError=="None" & MappingError=="None")
      return(list(CrossRefs=CrossRef,Maps=MappingDF,WarningString=warningstring)) 
   if(CrossRefError!="None" & MappingError!="None")
      return(list(CrossRefs=CrossRefError,Maps=MappingError,WarningString=warningstring))
   if(CrossRefError=="None" & MappingError!="None")
     return(list(CrossRefs=CrossRef,Maps=MappingError,WarningString=warningstring))
   if(CrossRefError!="None" & MappingError=="None")
      return(list(CrossRefs=CrossRefError,Maps=MappingDF,WarningString=warningstring))
 }


