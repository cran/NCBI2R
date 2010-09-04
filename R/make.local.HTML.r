make.local.HTML <-function(sourcefilename,folder,CellLimit=30000)
   {
    BigDescription<-""
   set.create.dir(folder,setdir=TRUE)
   sourcefile <- get.file(sourcefilename, showurl = FALSE, clean = FALSE)
   print("BOOOOO")
   print(sourcefilename)
   print(head(sourcefile))
   TD_Row<-0
   findthis<-"  <title>"
   while(substr(sourcefile[TD_Row+1],1,nchar(findthis))!=findthis)
      TD_Row<-TD_Row+1
   titlestring<-sourcefile[TD_Row+1]
   findthis<-"<h1 class=\"heading article-title\">"
   while(substr(sourcefile[TD_Row+1],1,nchar(findthis))!=findthis)
       TD_Row<-TD_Row+1
   HTMLfile<-paste("<HTML><HEAD>",titlestring,"</HEAD><BODY>")
   findthis<-"<p class=\"red-txt\"><strong>Don't forget to mention"
   while(substr(sourcefile[TD_Row+1],1,nchar(findthis))!=findthis)     {
      TD_Row<-TD_Row+1
      if(sourcefile[TD_Row]!="   "  | sourcefile[TD_Row]!="     ")     {
         HTMLfile<-c(HTMLfile,sourcefile[TD_Row])
         BigDescription<-paste(BigDescription,sourcefile[TD_Row])
         }
      }
   HTMLfile<-c(HTMLfile,"</BODY></HTML>")
   HTMLfile<-gsub("<li>"," ",HTMLfile)
   HTMLfile<-gsub("</li>"," ",HTMLfile)
   HTMLfile<-gsub("<h1 class","<h3 class",HTMLfile)
   HTMLfile<-gsub("</h1>","</h3>",HTMLfile)
   HTMLfile<-gsub("<h2 class","<h3 class",HTMLfile)
   HTMLfile<-gsub("</h2>","</h3>",HTMLfile)
   sourcefilename<-substr(sourcefilename,47,nchar(sourcefilename))
   expdate<-GetExpiryDate(HTMLfile)
   destfile<-paste(sourcefilename,".html",sep="")
   write(HTMLfile,destfile)
   setwd("..")
   BigDescription<-gsub("<h1 class=\"heading article-title\">","",BigDescription)
   BigDescription<-gsub("</h1> <dl class=\"job-desc-details\">","",BigDescription)
   BigDescription<-gsub("<dt>Jobs by tag(s):</dt>","  TAGS:",BigDescription)
   BigDescription<-gsub("<li><a href=\"/naturejobs/science/tags/","",BigDescription)
   BigDescription<-gsub("<ul class=\"job-tags\">","",BigDescription)
   BigDescription<-gsub("<dt>Website:</dt>","  Web:  ",BigDescription)
   BigDescription<-gsub("<dt>Expires:</dt>","  EXPIRES:  ",BigDescription)
   BigDescription<-gsub("<dt>Employer:</dt>","  Employer:  ",BigDescription)
   BigDescription<-gsub("<span class=\"cleardiv\"><!-- --></span> <h2 class=\"list-heading underline description-heading\">","",BigDescription)
   BigDescription<-gsub("</h2>","",BigDescription)
   BigDescription<-gsub("&#163;","(Euro)",BigDescription)
   BigDescription<-gsub("&#8211;","-",BigDescription)
   BigDescription<-gsub("&#8217;","'",BigDescription)
   BigDescription<-gsub("(<)+(/)?p>","   ",BigDescription) 
   BigDescription<-gsub("(<)+(/)?strong>","   ",BigDescription)
   BigDescription<-gsub("(<)+(/)?em>","   ",BigDescription) 
   BigDescription<-gsub("<!-- DESCRIPTION --> <div class=\"txt-ed-job-desc\">   <p><strong>","",BigDescription)
   BigDescription<-gsub("</div>","",BigDescription)
   BigDescription<-gsub("<dt>Requisition number:</dt>","Req Num",BigDescription)
   BigDescription<-gsub("<br />","   ",BigDescription)
   BigDescription<-gsub("<span class=\"caps\">","",BigDescription)
   BigDescription<-gsub("</span>","",BigDescription)
   BigDescription<-gsub("&nbsp;"," ",BigDescription)
   BigDescription<-gsub("<div style=\"margin: 0 0 16px 16px; float: right; text-align: center; overflow: hidden\"> ","",BigDescription)
   BigDescription<-gsub("<img(.)+(/>)","",BigDescription)

   BigDescription<-gsub("</a>","",BigDescription) 
   BigDescription<-substr(BigDescription,1,CellLimit)
   return(list(expdate=expdate,BigDescription=BigDescription))
   }
