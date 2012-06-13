updateCheck.int<-function()
   {
   a<-suppressWarnings(tryScan("http://ncbi2r.wordpress.com/latest/",retry=1,error=FALSE))
   getLatestVersion<-function(txt)
     {
     a1<-txt[grep("NCBI2R latest version: ",txt)]
     a2<-gsub("^[[:print:]]*NCBI2R latest version: ([[:digit:]]{1,2}\\.[[:digit:]]{1,2}\\.[[:digit:]]{1,2})[[:blank:]]+[[:print:]]*$","\\1\\2\\3",a1)
     LatestVersion<-a2[1]
     return(LatestVersion)
     }
   LatestVersion<-getLatestVersion(a)
   URLdef<-ncbi2r.options()
   vc<-suppressWarnings(try(compareVersion(URLdef$buildversion,LatestVersion),silent=TRUE))
   if(class(vc)=="try-error")
     vc<-(-9)
   info<-paste("Your version:",URLdef$buildversion,"Latest Release",LatestVersion)
   versionString<-"";versionDummy<-pn()
   if(vc==1)
     versionString<-paste("NCBI2R beta version.",info)
   if(vc==(-1))
     versionString<-paste("NCBI2R update recommended.",info)
   if(vc==(-9))
     versionString<-paste("NCBI2R update check was unavailable.")
   if(vc==0)
     versionString<-paste("NCBI2R is the current version.",LatestVersion)
   .ncbi2r.options$vS<<-versionString
   return(versionString)
  }