GetPhenotypes <-
function(locusIDs,showurl=FALSE,silent=TRUE)  
   {  
   if(showurl!=TRUE & showurl!=FALSE)
      stop("NCBI2R error: a locusID appears to be not entered correctly")
   locusIDs<-locusIDs[locusIDs!=""]   
   locusIDs<-paste(locusIDs,collapse=",")
   locusIDs<-unique(unlist(strsplit(locusIDs,","))) 
   URLdef<-URLdefinitions()
   GeneFoundTally<-FALSE
   for(k in 1:length(locusIDs))
      {
      getURL<-paste(URLdef$front,"efetch.fcgi?db=gene&id=",locusIDs[k],"&rettype=XML",URLdef$back,sep="")
      webget<-get.file(getURL,showurl,clean=TRUE)
      V<-try(get.phenotypes.int(webget))
      if(class(V)!="character")
        {
        phenotypes<-V$Phenotypes
        phenotypes<-as.data.frame(cbind(locusID=locusIDs[k],phenotypes),stringsAsFactors=FALSE) 
        if(exists("all_phenotypes"))                                                           
           all_phenotypes<-as.data.frame(rbind(all_phenotypes,phenotypes),stringsAsFactors=FALSE)
        else
           all_phenotypes<-phenotypes   
        }
      if(class(V)=="character")
         GeneFoundTally<-TRUE      
      }
  if(silent==FALSE & GeneFoundTally==TRUE)
    print("Some genes were not found")
  if(exists("all_phenotypes")) 
    {
    all_phenotypes$locusID<-as.character(all_phenotypes$locusID)
    return(all_phenotypes) 
    }
  else
    return("No information found")  
}
