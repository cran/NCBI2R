updateCheck.int<-function()
   {
   a<-suppressWarnings(tryScan("http://ncbi2r.wordpress.com/latest/",retry=1,error=FALSE))
   FindLines<-function(keystring,txt)
      {
      txt<-gsub(keystring,"",a[grep(keystring,a)])
      txt<-RemoveSpaces(gsub("<br />","",gsub("</p>","",txt)))
      return(txt)
      }
   LatestVersion<-FindLines("NCBI2R latest version: ")[1]
   URLdef<-ncbi2r.options()
   vc<-compareVersion(URLdef$buildversion,LatestVersion)
   info<-paste("Your version:",URLdef$buildversion,"Latest Release",LatestVersion)
   versionString<-"";versionDummy<-pn()
   if(vc==1)
     versionString<-paste("NCBI2R beta version.",info)
   if(vc==(-1))
     versionString<-paste("NCBI2R update recommended.",info)
   if(vc==0)
     versionString<-paste("NCBI2R is the current version.",LatestVersion)
   .ncbi2r.options$vS<<-versionString
   return(versionString)
  }