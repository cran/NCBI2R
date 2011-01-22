get.pathways.int <-
function(webget,LC=1)
   {
   LC<-skimUntil("<Gene-commentary_heading>Pathways</Gene-commentary_heading>",webget,LC)
   if(is.na(webget[LC]))
      return("NCBI2R: no Pathways found") 
   else
      {   
       Pathways<-data.frame(name=rep("",2000),web="",stringsAsFactors=FALSE)
       i<-0
       Checker<-finder("<Gene-commentary_text>","</Gene-commentary_comment>",webget,LC)
       while(Checker$Object==1)
          {
          i<-i+1                                     
          Pathways$name[i]<-substr(webget[Checker$RowNumber],23,nchar(webget[Checker$RowNumber])-23)
          Checker<-finder("<Other-source_url>","</Gene-commentary_comment>",webget,Checker$RowNumber+1)
          Pathways$web[i]<-substr(webget[Checker$RowNumber],19,nchar(webget[Checker$RowNumber])-19)
          Checker<-finder("<Gene-commentary_text>","</Gene-commentary_comment>",webget,Checker$RowNumber+1)
          }
       LC<-Checker$RowNumber
       Pathways<-Pathways[1:i,] 
       Pathways$name<-gsub("&amp;apos;","'",Pathways$name)  
       Pathways$web<-gsub("&amp;amp;","&",Pathways$web)
       return(list(Pathways=Pathways,LC=LC))
     }  
   } 

