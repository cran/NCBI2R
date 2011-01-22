finder <-function(FindThisText,FindThisText2,textfile,StartingRow)
   {
   CurrentRow<-StartingRow
   while((substr(textfile[CurrentRow],1,nchar(FindThisText))!=FindThisText)&(CurrentRow<length(textfile)))
      CurrentRow<-CurrentRow+1
   RowFindThisText<-CurrentRow
   CurrentRow<-StartingRow
   while((substr(textfile[CurrentRow],1,nchar(FindThisText2))!=FindThisText2)&(CurrentRow<length(textfile))&(CurrentRow<(RowFindThisText+1))) 
      CurrentRow<-CurrentRow+1
   RowFindThisText2<-CurrentRow
   if(RowFindThisText2<RowFindThisText) {
      return(list(ObjectFound = 2, RowNumber = RowFindThisText2))
      } else {
      return(list(ObjectFound = 1, RowNumber = RowFindThisText))
      }
   } 

