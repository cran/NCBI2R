UpdateCheck<-function()
   {
   a<-suppressWarnings(TryScan("http://ncbi2r.wordpress.com/latest/",retry=1,error=FALSE))
   FindLines<-function(keystring,txt)
      {
      txt<-gsub(keystring,"",a[grep(keystring,a)])
      txt<-RemoveSpaces(gsub("<br />","",gsub("</p>","",txt)))
      return(txt)
      }
   LatestVersion<-FindLines("NCBI2R latest version: ")
   LatestRelease<-FindLines("NCBI2R latest release date: ")
   ExpectedRelease<-FindLines("NCBI2R next expected release: ")
   if(URLdefinitions()$buildversion!=LatestVersion)
     print(paste("Update recommended. Your version:",URLdefinitions()$buildversion,"Latest Version",LatestVersion))
   }