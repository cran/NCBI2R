GetTable<-function(htmlsource,StartLine)
   {
   ActualStartLine<-grep("<table",htmlsource)
   ActualStartLine<-ActualStartLine[ActualStartLine>StartLine][2] 
   EndSection<-grep("</table>",htmlsource)
   EndSection<-EndSection[EndSection>ActualStartLine][1]
   return(htmlsource[ActualStartLine:EndSection])
  }
