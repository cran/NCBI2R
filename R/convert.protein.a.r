convert.protein.a<-function(aminoacids,abbrev="aa3",exactstop=FALSE)
    {
    if(!(abbrev %in% c("aa3","aa1","aalong")))
       stop("NCBI2R error: ConvertProtein: abbrev must be aa1, aa3 or aalong")
   AA<-AminoAcids()
   startcol<-0
   for(i in 1:4)
      {
      gh<-unique(aminoacids %in% AA[,i])
      if(length(gh)==1 & gh[1]==TRUE)
         {
         if(startcol!=0)
            stop("NCBI2R error: ConvertProtein error. Make sure your amino acids are all the same type")
         startcol<-i
         }
      }

   if(startcol==0)
      {
      if(length(grep(">",aminoacids))!=length(aminoacids))
        stop("NCBI2R error: ConvertProtein error. Unclear input for amino acids")
      splitup<-unlist(strsplit(aminoacids,">"))
      changed<-convert.protein.a(splitup,abbrev=abbrev)
      changed.df<-as.data.frame(matrix(changed,byrow=TRUE,ncol=2),stringsAsFactors=FALSE)
      answer<-paste(changed.df[,1],changed.df[,2],sep=">")
      } else {
      if(length(unique(nchar(aminoacids)))!=1 & startcol!=4)
         stop("NCBI2R error: ConvertProtein error. Make sure your amino acids are all the same type. Err.")
      answer<-AA[pmatch(aminoacids,AA[,startcol],duplicates.ok=TRUE),abbrev]
      }
   if((exactstop==TRUE & startcol==2) | (exactstop==FALSE))
     {
     answer[answer=="Amb"|answer=="Och"|answer=="Opl"]<-"Stp"
     answer[answer=="Amber"|answer=="Ocher"|answer=="Opal"]<-"Stop"
     }
   return(answer)
   }
