urlCreator <-
function(term,db,org="human",cg=TRUE)
   {
   pe=FALSE
   counter<-0
   tempterm<-term
   while(org!="" & term!="" & length(splitfirst(tempterm,"[")>1))
      {
      obfusc1<-c("ORGN","ORGANISM","TAXID","TID")
      obfusc2<-c("FILT","FILTER","FT")     
      fields<-rep("",100)
      while(length(splitfirst(tempterm,"["))==2)
         {
         counter<-counter+1
         fields[counter]<-ext.f.tags(tempterm,"[","]")    
         tempterm<-splitfirst(tempterm,"]")[2]
         if(is.na(tempterm))
           tempterm<-""
          }
      fields<-fields[fields!=""]
      for(P in 1:length(fields))
         {
         if(fields[P] %in% obfusc1 | fields[P] %in% obfusc2) 
            pe=TRUE
         }
      }
   if(pe)
      print("NCBI2R WARNING: SOMETHING IN THE SEARCH TERM MIGHT BE CONTRADICTING ANY OF THE SET DEFAULTS+++++++++++++++++")
   if(term!="")
      term<-paste(term,"+AND",sep="")
   if(org=="" | org=="all")
      orgstring<-""
   if(org!="")
      orgstring<-paste("+",org,"[ORGN]",sep="")
   if(cg==TRUE & db=="gene") {
      currentgenestring<-"+gene+all[filter]"
      }  else  { 
      currentgenestring<-""
      }
   return(paste(term,orgstring,currentgenestring,sep=""))
   }
