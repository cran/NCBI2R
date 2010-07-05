DelayUntil <-
function(hours,minutes=0,seconds=0,day=as.numeric(format(Sys.time(), "%d")),month=as.numeric(format(Sys.time(), "%m")),year=as.numeric(format(Sys.time(), "%Y")))
   {
   print(paste("DelayUntil is holding until",ISOdatetime(year,month,day,hours,minutes,seconds)))
   flush.console()
   while(Sys.time()<ISOdatetime(year,month,day,hours,minutes,seconds))
      Delay<-7
   }

