excel.MID<-function(text,start_num,num_chars)
   {
   answer<-substr(text,start_num,start_num+num_chars-1)
   return(answer)
   }
