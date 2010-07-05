EmailAlert<-function (subject = "NCBI2R Job Done", address = "", body="")
   {
   if(version$os=="mingw32" | version$os=="mingw64")
      stop("This command will not work on Windows operating system - only unix/linux") 
   if(missing(address))
      stop("no email address is given")
  commandstring<-paste("echo ", body, "| mailx -s ", "\"",subject,"\" ", address, sep="")
  system(commandstring)
  }
