set.create.dir <-
function(DirectoryName,setdir=FALSE)
   {
   if(!(file.exists(DirectoryName) & file.info(DirectoryName)$isdir))
      dir.create(DirectoryName)
   if(setdir)
      setwd(DirectoryName)
   }

