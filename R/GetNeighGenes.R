GetNeighGenes <-                                                                                 
function(chr,chrpos,FlankingDistance=100000,showurl=FALSE,hyper="HYPERLINK",full=FALSE,web=TRUE,html=FALSE,org="human",cg=TRUE,div="**",sme=FALSE,smt=FALSE,pbar=TRUE)
   {
   if(class(chr)=="data.frame" & missing(chrpos))
      {
      if(ncol(chr)!=2)
        stop("GetNeighGenes failed because the data frame was expected to be two columns. Your dataframe is not. The function can handle multiple entries at the same time, however two columns only - one for chr and one for chrpos")
      chrpos<-as.numeric(as.character(chr[,2]))
      temp<-as.character(chr[,1])
      chr<-as.character(temp)
      }
   chrpos<-as.numeric(chrpos)
   if(full==TRUE)
      {         
      web<-TRUE
      html<-TRUE
      }
   if(substr(div,nchar(div),nchar(div))!=" ")
     div<-paste(div," ",sep="")
   org<-gsub(" ","+",org)
   taxFromOrg<-try(GetTax(org))
   if(class(taxFromOrg)=="try-error")
      stop("NCBI2R error GNG-003. Unable to use function GetTax within GetNeighGenes")
   taxstring<-paste("TAXID=",as.character(taxFromOrg,sme=sme,smt=smt),sep="")
   if(length(taxstring)>1)
     stop("more than one species was identified by the org variable. Please try again with a new organism term")
   if(length(chr)!=length(chrpos))
      stop("The data you entered for the chromosome and position on the chromosome (chrpos) are not of the same length")
   NeighbourDivider<-div  
   Neighbour<-data.frame(locusID=rep("",length(chr)),NeighHTMLlink="",Neigh.web="",stringsAsFactors=FALSE)
   
   Frame1<-as.data.frame(cbind(chr,chrpos),stringsAsFactors=FALSE)
   Frame1$LowPoint<-0
   Frame1$HighPoint<-0
   Frame1$LowPoint[chrpos>0]<-chrpos[chrpos>0]-FlankingDistance
   Frame1$HighPoint[chrpos>0]<-chrpos[chrpos>0]+FlankingDistance
   Frame1[Frame1$LowPoint<0,"LowPoint"]<-0
   Frame1$LowPoint<-formatC(Frame1$LowPoint,digits=9,width=1)
   Frame1$HighPoint<-formatC(Frame1$HighPoint,digits=9,width=1)     
   Frame1$Neigh.web<-paste("http://www.ncbi.nlm.nih.gov/projects/mapview/maps.cgi?",taxstring,"&CHR=",Frame1$chr,"&MAPS=ideogr%2Ccntg-r%2Cgenes%2Csnp&BEG=",Frame1$LowPoint,"&END=",Frame1$HighPoint,"&thmb=on",sep="")
   if(pbar)
      pb <- txtProgressBar (min =0, max=length(chrpos), style=3)
   for(i in 1:length(chrpos)) 
      {   
       if(pbar)
          setTxtProgressBar(pb,i)
       locusID<-try(GetRegion("gene",Frame1$chr[i],Frame1$LowPoint[i],Frame1$HighPoint[i],cg=cg,org=org,sme=sme,smt=smt,showurl=showurl))
       if(class(locusID)=="try-error")
         {
         stop("NCBI2R error GNG-004. Unable to use function GetRegion within GetNeighGenes")
         }
       posindf<-rep(i,length(locusID)) 
       ThisSet<-as.data.frame(cbind(locusID,posindf))
       if(i==1)
         { Frame2<-ThisSet
         } else {
          Frame2<-as.data.frame(rbind(Frame2,ThisSet),stringsAsFactors=FALSE)
          }          
      }


   if(pbar)   
      close(pb)  
   UniqueList<-unique(Frame2$locusID)
   UniqueList<-as.character(UniqueList[UniqueList!=0]) 

   if(length(UniqueList)!=0)
       {
       Frame3<-try(GetGeneNames(UniqueList))
       if(class(Frame3)=="try-error")
          stop("NCBI2R error GNG-005. Unable to use function GetGeneNames")
       Frame23<-merge(Frame2,Frame3,by="locusID",all=TRUE)
       if(cg==TRUE)
          Frame23<-Frame23[Frame23$CurrentRecord!="discontinued",]  
       Frame23$NeighHTML[Frame23$genename==""]<-paste("<a href=\"http://www.ncbi.nlm.nih.gov/sites/entrez?db=gene&term=",Frame23$locusID,"\">(",Frame23$locusID,")</a>",sep="")
       Frame23$NeighHTML[Frame23$genename!=""]<-paste("<a href=\"http://www.ncbi.nlm.nih.gov/sites/entrez?db=gene&term=",Frame23$locusID,"\">",Frame23$genename,"</a>",sep="")
         
       for(k in 1:length(chrpos))
         {
          Frame1$locusID[k]<-paste(Frame23[Frame23$posindf==k,"locusID"],collapse=",")
          Frame1$genename[k]<-paste(Frame23[Frame23$posindf==k,"genename"],collapse=",")
          Frame1$genesymbol[k]<-paste(Frame23[Frame23$posindf==k,"genesymbol"],collapse=",")
          Frame1$NeighHTML[k]<-paste(Frame23[Frame23$posindf==k,"NeighHTML"],collapse=",")   
         }
    }


  if(!("genename" %in% names(Frame1)))
     Frame1$genename<-""
  if(!("locusID" %in% names(Frame1)))
     Frame1$locusID<-""
  if(!("genesymbol" %in% names(Frame1)))
     Frame1$genesymbol<-""
  if(!("NeighHTML" %in% names(Frame1)))
     Frame1$NeighHTML<-""
  if(!("Neigh.web" %in% names(Frame1)))
     Frame1$Neigh.web<-""
  Frame1<-Frame1[,c("chr","LowPoint","HighPoint","locusID","genename","genesymbol","NeighHTML","Neigh.web")]



  if(web==FALSE)
    Frame1$Neigh.web<-NULL
  if(html==TRUE)
    Frame1$genenames<-NULL 
  if(html==FALSE)
     Frame1$NeighHTML<-NULL
  return(Frame1=Frame1)
  }
