ncbi2r.options.default<-
function()
   {
   baseurl<-"http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   tool<-"NCBI2R"
   email<-"ncbi2r@gmail.com"
   tailurl<-paste("&tool=",tool,"&email=",email,sep="")
   x <- read.dcf(file = system.file("DESCRIPTION", package = "NCBI2R"))
   buildversion<-as.character(as.data.frame(x)$Version)
   vS<-"No update check yet"
   package.web<-"http://NCBI2R.wordpress.com"
   dS<-0.333334
   TimeStampA<-Sys.time()-3
   return(list(front = baseurl,back=tailurl,buildversion=buildversion,vS=vS,package.web=package.web,dS=dS,TimeStampA=TimeStampA))
   }

