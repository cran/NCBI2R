
get.gene.docset<-function(locusIDs,batchsize=200,showurl=FALSE)
   {
   BatchCounter<-1

   URLdef<-ncbi2r.options()
   url_piece<-paste("&id=",paste(locusIDs,collapse=","),sep="")
   getURL<-paste(URLdef$front,"efetch.fcgi?db=gene",url_piece,"&rettype=docset&retmode=text",URLdef$back,sep="")
   webget<-get.file(getURL,showurl,clean=FALSE)
   parse_heading<-":"


   hj<-unlist(strsplit(webget,"parse_heading"))
   h2<-unique(hj[grep(":",hj)])

   h2<-h2[grep("[[:digit:]]*\\.",h2,invert=TRUE)]
   headers<-unique(gsub("([[:print:]]*?)\\:[[:print:]]*","\\1",h2))
   rm(h2)
   id.lines<-grep("^ID: ",webget)

   res<-as.data.frame(matrix("",nrow=length(id.lines),ncol=length(headers)),stringsAsFactors=FALSE)
   names(res)<-gsub(" ","_",headers)
   res$NewlocusID<-""

   for(k in 1:length(headers))
      {
      d<-grep(paste("^",headers[k],parse_heading," ",sep=""),webget)
      res[cut(d,c(1,id.lines,9999999999),labels=FALSE),k]<-gsub(paste(headers[k],": ",sep=""),"",webget[d])

      }
   if(!is.na(pmatch("Annotation",headers)))
      {

      res$GeneHighPoint<-gsub("[[:print:]]*\\.\\.([[:digit:]]*)(, complement)*\\)","\\1",res$Annotation)
      res$GeneLowPoint<-gsub("([[:print:]]*)\\(([[:digit:]]*)\\.\\.([[:digit:]]*)(, complement)*\\)","\\2",res$Annotation)
      res$ori<-""
      res$ori[grep("complement",res$Annotation)]<-"-"
      res$ori[grep("\\(",res$Annotation)]<-"+"
      res$Annotation<-gsub("([[:print:]]*)[[:blank:]]\\([[:print:]]*","\\1",res$Annotation)
      }

   if(!is.na(pmatch("This record was replaced with GeneID",headers)))
      names(res)[pmatch("This record was replaced with GeneID",headers)]<-"NewlocusID"

   if(!is.na(pmatch("MIM",headers)))
      names(res)[pmatch("MIM",headers)]<-"OMIM"
   if(!is.na(pmatch("ID",headers)))
      names(res)[pmatch("ID",headers)]<-"locusID"
   if(!is.na(pmatch("Chromosome",headers)))
      {
      res$chr<-gsub("([[:print:]]*);[[:blank:]]Location:[[:blank:]][[:print:]]*","\\1",res$Chromosome)
      res$cyto<-gsub("([[:print:]]*);[[:blank:]]Location:[[:blank:]]([[:print:]]*)","\\2",res$Chromosome)
      res$Chromosome<-NULL
      }
   if(!is.na(pmatch("Official Symbol",headers)))
      {
      res$genename<-gsub("[[:print:]]*[[:blank:]]and[[:blank:]]Name:[[:blank:]]([[:print:]]*)\\[([[:print:]]*)\\]","\\1",res$Official_Symbol)
      res$species<-gsub("[[:print:]]*[[:blank:]]and[[:blank:]]Name:[[:blank:]]([[:print:]]*)\\[([[:print:]]*)\\]","\\2",res$Official_Symbol)
      res$genesymbol<-gsub("([[:print:]]*)[[:blank:]]and[[:blank:]]Name:[[:blank:]]([[:print:]]*)\\[([[:print:]]*)\\]","\\1",res$Official_Symbol)
      res$Official_Symbol<-NULL
      }
     return(res)
   }
