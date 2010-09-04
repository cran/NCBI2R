OpenURL <-
function(urls,safety=10)
  {
  urls<-as.character(urls)
  if(length(urls)>safety)
     stop(paste("There are more than ",safety," urls! If you are sure, repeat the command, using the safety argument set to a larger number"))
  for(i in 1:length(urls))
     shell.exec(urls[i])
  }

