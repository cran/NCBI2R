
extract.GetGenenames.top<-function(top)
   {
  genenamesA<-gsub("[[:print:]]*[[:digit:]]+-[[:print:]]{1,3}-[[:print:]]{4}([[:print:]]*)","\\1",top[grep("replaced with",top,invert=TRUE)])
  genenamesA.ln<-grep("replaced with",top,invert=TRUE)
  genenamesB<-gsub("[[:print:]]*with[[:blank:]]GeneID:[[:blank:]][[:digit:]]*([[:print:]]*)","\\1",top[grep("replaced with",top,invert=FALSE)])
  genenamesB.ln<-grep("replaced with",top,invert=FALSE)
  genenames<-c(genenamesA,genenamesB)[order(c(genenamesA.ln,genenamesB.ln))]
  return(genenames)
  }

extract.GetCurrentRecords<-function(top)
   {
   l<-grep("Gene ID:",top)
   LastUpdate<-rep("",length(top))
   CurrentRecord<-rep("",length(top))
   locusID<-rep("",length(top))
   locusID[l]<-gsub("Gene[[:blank:]]ID:[[:blank:]]([[:digit:]]+),[[:blank:]]([[:print:]]*)[[:blank:]]on[[:blank:]]([[:digit:]]+-[[:print:]][[:print:]][[:print:]]-[[:digit:]][[:digit:]][[:digit:]][[:digit:]])[[:print:]]*","\\1",top[grep("Gene ID:",top)])
   CurrentRecord[l]<-gsub("Gene[[:blank:]]ID:[[:blank:]]([[:digit:]]+),[[:blank:]]([[:print:]]*)[[:blank:]]on[[:blank:]]([[:digit:]]+-[[:print:]][[:print:]][[:print:]]-[[:digit:]][[:digit:]][[:digit:]][[:digit:]])[[:print:]]*","\\2",top[grep("Gene ID:",top)])
   LastUpdate[l]<-gsub("Gene[[:blank:]]ID:[[:blank:]]([[:digit:]]+),[[:blank:]]([[:print:]]*)[[:blank:]]on[[:blank:]]([[:digit:]]+-[[:print:]][[:print:]][[:print:]]-[[:digit:]][[:digit:]][[:digit:]][[:digit:]])[[:print:]]*","\\3",top[grep("Gene ID:",top)])
   return(as.data.frame(cbind(locusID,CurrentRecord,LastUpdate),stringsAsFactors=FALSE))
   }

extract.get.replacement.lines<-function(top)
  {
  replace.d<-gsub("([[:print:]]*with[[:blank:]]GeneID:[[:blank:]][[:digit:]]*)[[:print:]]*","\\1",top[grep("replaced with",top,invert=FALSE)])
  replace.l<-grep("replaced with",top,invert=FALSE)
  NewlocusID<-rep("",length(top))
  NewlocusID[replace.l]<-gsub("This record was replaced with GeneID: ","",replace.d)
  return(NewlocusID)
  }

extract.parse.line.sym.name.species<-function(txt)
  {
  genesymbol<-gsub("([[:print:]]*?)[[:blank:]]([[:print:]]*)","\\1",txt)
  genename<-gsub("([[:print:]]*?)[[:blank:]](([[:print:]]*)\\[([[:print:]]*)\\])+","\\3",txt)
  species<-gsub("([[:print:]]*?)[[:blank:]](([[:print:]]*)\\[([[:print:]]*)\\])+","\\4",txt)
  return(as.data.frame(cbind(genesymbol,genename,species),stringsAsFactors=FALSE))
  }

extract.genenames.from.genetabletxt<-function(top)
  {
  a1<-extract.GetGenenames.top(top)[1:(length(top)-1)]
  a2<-extract.GetCurrentRecords(top)[2:length(top),]
  NewlocusID.tmp<-extract.get.replacement.lines(top)[2:length(top)]
  a<-as.data.frame(cbind(a1,a2,NewlocusID.tmp),stringsAsFactors=FALSE)
  a$NewlocusID<-""
  a$NewlocusID[(1:nrow(a))[a$NewlocusID.tmp!=""]-1]<-as.character(a$NewlocusID.tmp[(1:nrow(a))[a$NewlocusID.tmp!=""]])
  a<-a[a$a1!="",]
  d<-extract.parse.line.sym.name.species(a$a1)
  a<-as.data.frame(cbind(a,d),stringsAsFactors=FALSE)
  a$a1<-NULL
  row.names(a)<-1:nrow(a)
  a<-a[,c("genename","genesymbol","NewlocusID","CurrentRecord","LastUpdate","locusID","species")]
  return(a)
  }
