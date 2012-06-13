ncbi2r.options.default<-
function()
   {
   baseurl<-"http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   tool<-"NCBI2R"
   email<-"ncbi2r@gmail.com"
   tailurl<-paste("&tool=",tool,"&email=",email,sep="")
   buildversion<-try(utils::packageVersion(pkg=tool),silent=TRUE)
   ifelse(class(buildversion)[1]=="try-error",
      buildversion<-NA,
      buildversion<-as.character(buildversion))
   vS<-"No update check yet"
   package.web<-"http://NCBI2R.wordpress.com"
   dS<-0.333334
   TimeStampA<-Sys.time()-3
   return(list(front = baseurl,back=tailurl,buildversion=buildversion,vS=vS,package.web=package.web,dS=dS,TimeStampA=TimeStampA))
   }

