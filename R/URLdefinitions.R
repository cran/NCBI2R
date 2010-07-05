URLdefinitions <-
function()
   {
   baseurl<-"http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   tool<-"NCBI2R"
   email<-"ncbi2r@gmail.com"
   tailurl<-paste("&tool=",tool,"&email=",email,sep="")
   buildversion<-1.3
   package.web<-"http://drop.io/NCBI2R_package"
   return(list(front = baseurl,back=tailurl,buildversion=buildversion,package.web=package.web))
   }

