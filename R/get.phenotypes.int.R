get.phenotypes.int <-
function(webget,LC=1)
   {
   LC<-SkimUntil("<Gene-commentary_heading>Phenotypes</Gene-commentary_heading>",webget,LC)
   if(is.na(webget[LC]))
      return("NCBI2R: no Phenotypes found") 
   else
      {
       Phenotypes<-data.frame(name=rep("",5000),web="",stringsAsFactors=FALSE)
       i<-0
       Checker<-Finder("<Gene-commentary_text>","</Gene-commentary_comment>",webget,LC)
       while(Checker$Object==1)
          {
          i<-i+1                                         
          Phenotypes$name[i]<-substr(webget[Checker$RowNumber],23,nchar(webget[Checker$RowNumber])-23)
          Checker<-Finder("<Dbtag_db>","</Gene-commentary>",webget,Checker$RowNumber+1)
          
          if(Checker$Object==1)
             {      
             dbtag<-substr(webget[Checker$RowNumber],11,nchar(webget[Checker$RowNumber])-11)
             LC<-((Checker$RowNumber)+3)   
             idnumber<-substr(webget[LC],15,nchar(webget[LC])-15)  
             if(dbtag=="MIM")   
                Phenotypes$web[i]<-paste("http://www.ncbi.nlm.nih.gov/entrez/dispomim.cgi?id=",idnumber,sep="")
             if(dbtag=="MGI")
                Phenotypes$web[i]<-paste("http://www.informatics.jax.org/searches/accession_report.cgi?id=MGI:",idnumber,sep="")
             if(dbtag!="MIM" & dbtag!="MGI") 
                  Phenotypes$web[i]<-substr(webget[Checker$RowNumber+1],19,nchar(webget[Checker$RowNumber+1])-19)
             } 
          Checker<-Finder("<Gene-commentary_text>","</Gene-commentary_comment>",webget,Checker$RowNumber+1)
      
          }
        LC<-Checker$RowNumber
       Phenotypes<-Phenotypes[1:i,]  
       Phenotypes$name<-gsub("&amp;apos;","'",Phenotypes$name)
       return(list(Phenotypes=Phenotypes,LC=LC))
       } 
     }