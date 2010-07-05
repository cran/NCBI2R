AdjustRangeAroundGene <-
function(LowPoint,HighPoint,orientation,upstream = 10000, downstream=2000)
   {
   LowPoint<-as.numeric(LowPoint)   
   HighPoint<-as.numeric(HighPoint)

   if(orientation=="+")
      {
      LowPoint<-LowPoint-upstream   
      HighPoint<-HighPoint+downstream
      }
   if(orientation=="-")
      {
      LowPoint<-LowPoint-downstream
      HighPoint<-HighPoint+upstream
      }
   if(LowPoint<0)
      LowPoint<-0
   return(list(LowPoint=LowPoint,HighPoint=HighPoint))
   } 

