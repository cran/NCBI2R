convert.int <-
function(intdf)
   {
   genelist<-paste(unique(intdf$OtherGene_osa),collapse=", ")
   partiallink<-"http://www.ncbi.nlm.nih.gov/sites/entrez?db=gene&term="
   int.h<-paste("<a href=\"",partiallink,unique(intdf$OtherGene_obj_id),"\">",intdf$OtherGene_osa,"</a>",sep="")   
   int.html<-paste(int.h,collapse="---")
   return(list(int.html=int.html,genelist=genelist)) 
   } 

