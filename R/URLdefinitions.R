URLdefinitions <-
function()
   {
   baseurl<-"http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   tool<-"NCBI2R"
   email<-"ncbi2r@gmail.com"
   tailurl<-paste("&tool=",tool,"&email=",email,sep="")
   #mylocation<-system.file("DESCRIPTION",package="NCBI2R")
   #ncbi2rdescription<-scan(mylocation,what="character",sep="\n",quiet=TRUE)
   #buildversion<-gsub("Version: ","",ncbi2rdescription[grep("Version: ",ncbi2rdescription)])
   buildversion<-"1.3.2"
   package.web<-"http://NCBI2R.wordpress.com"
   return(list(front = baseurl,back=tailurl,buildversion=buildversion,package.web=package.web))
   }

