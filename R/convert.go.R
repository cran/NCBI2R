convert.go <-
function(GOdf)
   {
   fu<-unique(GOdf[GOdf$category=="Function",])
   co<-unique(GOdf[GOdf$category=="Component",])
   pr<-unique(GOdf[GOdf$category=="Process",])
   func<-paste(fu$name,collapse=", ")
   comp<-paste(co$name,collapse=", ")
   proc<-paste(pr$name,collapse=", ")
   partiallink<-"http://amigo.geneontology.org/cgi-bin/amigo/go.cgi?view=details&depth=1&query="
   fu.html<-paste("<a href=\"",partiallink,fu$db_id,"\">",fu$name,"</a>",sep="")
   func.html<-paste(fu.html,collapse=", ")
   co.html<-paste("<a href=\"",partiallink,co$db_id,"\">",co$name,"</a>",sep="")
   comp.html<-paste(co.html,collapse=", ")
   pr.html<-paste("<a href=\"",partiallink,pr$db_id,"\">",pr$name,"</a>",sep="")
   proc.html<-paste(pr.html,collapse=", ")
   return(list(func=func,comp=comp,proc=proc,func.html=func.html,comp.html=comp.html,proc.html=proc.html))
   } 

