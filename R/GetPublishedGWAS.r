GetPublishedGWAS<-function(keycol="",term="")
   {
   tw1<-read.delim("http://www.genome.gov/admin/gwascatalog.txt",stringsAsFactors=FALSE)
   names(tw1)<-gsub("\\.","",names(tw1))
   if(keycol!="")
      tw1<-tw1[grep(term,tw1[,keycol]),]
   return(tw1)
   }
