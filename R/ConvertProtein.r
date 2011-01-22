ConvertProtein<-function(aminoacids,abbrev="aa3",exactstop=FALSE)
   {
   if(class(aminoacids)!="character")
     stop("NCBI2R error: Input class for ConvertProtein needs to be character")
   if(length(aminoacids)==0)
     stop("NCBI2R error: Need at least one item for ConvertProtein")
   d<-length(grep(">",aminoacids))
   if(d!=length(aminoacids) & d!=0)
       stop("NCBI2R error: ConvertProtein has ambigious input")
   a<-convert.protein.a(aminoacids,abbrev=abbrev,exactstop=exactstop)
   return(a)
   }
