GetPathways <-
function(locusIDs,showurl=FALSE,silent=TRUE)  
   {  
   if(showurl!=TRUE & showurl!=FALSE)
      stop("NCBI2R error: a locusID appears to be not entered correctly")
   locusIDs<-locusIDs[locusIDs!=""]    
   locusIDs<-paste(locusIDs,collapse=",")
   locusIDs<-sort(unique(unlist(strsplit(locusIDs,",")))) 
   URLdef<-ncbi2r.options()
   GeneFoundTally<-FALSE
   for(k in 1:length(locusIDs))
      {
      getURL<-paste(URLdef$front,"efetch.fcgi?db=gene&id=",locusIDs[k],"&rettype=XML",URLdef$back,sep="")
      webget<-get.file(getURL,showurl,clean=TRUE)
      V<-try(get.pathways.int(webget)) 
      if(class(V)!="character")
        {
        pathways<-V$Pathways
        pathways<-as.data.frame(cbind(locusID=as.character(locusIDs[k]),pathways),stringsAsFactors=FALSE) 
        if(exists("all_pathways"))                                                         
           all_pathways<-as.data.frame(rbind(all_pathways,pathways),stringsAsFactors=FALSE)
        else
           all_pathways<-pathways   
        }
      if(class(V)=="character")
         GeneFoundTally<-TRUE      
      }
  if(silent==FALSE & GeneFoundTally==TRUE)
    print("Some genes were not found")
  if(exists("all_pathways")) 
    {
    all_pathways$locusID<-as.integer(as.character(all_pathways$locusID)) 
    return(all_pathways) 
    }
  else
    return("No information found")  
}


