get.pathways.int <-
function(webget,LC=1)
   {
   LC<-SkimUntil("<Gene-commentary_heading>Pathways</Gene-commentary_heading>",webget,LC)
   if(is.na(webget[LC]))
      stop("NCBI2R error: no Pathways found") 
   Pathways<-data.frame(name=rep("",2000),web="",stringsAsFactors=FALSE)
   i<-0
   Checker<-Finder("<Gene-commentary_text>","</Gene-commentary_comment>",webget,LC)
   while(Checker$Object==1)
      {
      i<-i+1                                     
      Pathways$name[i]<-substr(webget[Checker$RowNumber],23,nchar(webget[Checker$RowNumber])-23)
      Checker<-Finder("<Other-source_url>","</Gene-commentary_comment>",webget,Checker$RowNumber+1)
      Pathways$web[i]<-substr(webget[Checker$RowNumber],19,nchar(webget[Checker$RowNumber])-19)
      Checker<-Finder("<Gene-commentary_text>","</Gene-commentary_comment>",webget,Checker$RowNumber+1)
      }
   LC<-Checker$RowNumber
   Pathways<-Pathways[1:i,] 
   Pathways$name<-gsub("&amp;apos;","'",Pathways$name)  
   Pathways$web<-gsub("&amp;amp;","&",Pathways$web)
   return(list(Pathways=Pathways,LC=LC))
   } 

