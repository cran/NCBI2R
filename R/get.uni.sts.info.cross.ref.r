get.uni.sts.info.cross.ref<-function(webget,StartLine)
   {
   StartCrossReference<-grep("<!--- Cross reference --->",webget)
   StartCrossReference<-StartCrossReference[StartCrossReference>StartLine][1]
   CrossRefAvailable<-TRUE
   CrossRef<-"No Cross Reference info available"
   if(substr(webget[StartCrossReference+1],1,5)=="<!---")
      {
      CrossRefAvailable<-FALSE 
      } else {
      chunk1<-GetTable(webget,StartCrossReference)
      if(chunk1[2]== "</table><br>")
         CrossRefAvailable<-FALSE
      }
   CrossRefError<-"None"
   if(CrossRefAvailable==FALSE)
      {
      CrossRefError<-("No Cross Reference info available")
      } else {
      CrossRef<-get.uni.sts.info.cross.ref.available(chunk1)
      } 
   return(list(CrossRefError=CrossRefError,CrossRef=CrossRef))
   }
