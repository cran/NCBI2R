get.go.int <-
function(webget,LC=1)
   {
   LC<-skimUntil("<Gene-commentary_heading>GeneOntology</Gene-commentary_heading>",webget,LC)
   
   if(is.na(webget[LC]))
      stop("NCBI2R error: no GeneOntologies found")
   GO<-data.frame(category=rep("",2000),name="",evidence="",pubmed="",db="",db_id="",stringsAsFactors=FALSE) 
   GOcounter<-0
   LC<-skimUntil("<Gene-commentary_label>",webget,LC)
   while(substr(webget[LC],1,23)=="<Gene-commentary_label>")
      {
      CurrentHeading<-ext.f.tags(webget[LC])
      LC<-LC+1
      while(webget[LC+1]=="<Gene-commentary>") 
         {
         LC<-LC+3
         GOcounter<-GOcounter+1
         GO$category[GOcounter]<-CurrentHeading
         if(webget[LC]=="<Gene-commentary_refs>")
            {
            LC<-skimUntil("<Pub>",webget,LC)
            LC<-LC+2
            GO$pubmed[GOcounter]<-ext.f.tags(webget[LC])
            while(substr(webget[LC+5],1,10)=="<PubMedId>")
               {
               LC<-LC+5
               GO$pubmed[GOcounter]<-paste(GO$pubmed[GOcounter],",",ext.f.tags(webget[LC]),sep="")
               }
            LC<-skimUntil("<Gene-commentary_source>",webget,LC)
            }
         if(webget[LC]!="<Gene-commentary_source>")
            {
            stop("error found - we didn't find the source like we thought we would")
            }
         LC<-LC+4  
         GO$db[GOcounter]<-ext.f.tags(webget[LC])
         GO$db_id[GOcounter]<-ext.f.tags(webget[LC+3])
         LC<-skimUntil("<Other-source_anchor>",webget,LC+3)
         GO$name[GOcounter]<-ext.f.tags(webget[LC])
         GO$evidence[GOcounter]<-splitfirst(ext.f.tags(webget[LC+1])," ")[2] 
         LC<-skimUntil("</Gene-commentary>",webget,LC+1) 
         }
      LC<-LC+5 
      } 
   GO<-GO[GO$category!="",]
   return(list(GO=GO,LC=LC)) 
   }

