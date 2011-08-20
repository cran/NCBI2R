UpdateCheck<-function()
   {
   a<-suppressWarnings(TryScan("http://ncbi2r.wordpress.com/latest/",retry=1,error=FALSE))
   FindLines<-function(keystring,txt)
      {
      txt<-gsub(keystring,"",a[grep(keystring,a)])
      txt<-RemoveSpaces(gsub("<br />","",gsub("</p>","",txt)))
      return(txt)
      }
   LatestVersion<-FindLines("NCBI2R latest version: ")[1]
   #LatestRelease<-FindLines("NCBI2R latest release date: ")[1]
   #ExpectedRelease<-FindLines("NCBI2R next expected release: ")[1]
   vc<-compareVersion(URLdefinitions()$buildversion,LatestVersion)
   info<-paste("Your version:",URLdefinitions()$buildversion,"Latest Version",LatestVersion)
   if(vc==1)
     print(paste("You have a prerelease version.",info))
   if(vc==(-1))
     print(paste("Update recommended.",info))
   if(vc==0)
     print(paste("You have current version.",LatestVersion))
   }