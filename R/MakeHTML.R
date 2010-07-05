MakeHTML <-
function(anydf,filename,compress=TRUE,markercolumn="marker",keeplocusIDs=FALSE,keepNS=FALSE,kp=TRUE)
   {                                                                                              
    URLdef<-URLdefinitions()
   if(missing(anydf))
      stop("no data frame was provided to create the HTML file")
   if(!missing(anydf))
      if(class(anydf)!="data.frame")
         stop("the object provided for MakeHTML was not of class data.frame")
   if(missing(filename))
      stop("please provide an output file name")
   
   anydf<-ColumnStripper(anydf,".xl")
   deletesome<-FALSE
     
   if(colnames(anydf)[1]=="PMID")
     deletesome<-TRUE
   for(jk in 1:ncol(anydf)) 
      {
      if(colnames(anydf)[jk]=="Neigh.web")
         {
         for(ui in 1:nrow(anydf)) 
            {
             anydf$Neigh.web[ui]<-ConvertURLToHTML(anydf$Neigh.web[ui],anydf$chrpos[ui])
             anydf$chrpos[ui]<-gsub("&thmb=on\"",paste("&thmb=on&query=",anydf[ui,markercolumn],"\"",anydf$chrpos[ui],sep=""),anydf$Neigh.web[ui])
            }
         }
      }
   anydf$Neigh.web<-NULL
   

   if(exists("anydf$GeneLowPoint")==TRUE)
      {
      anydf$GeneLowPoint<-as.character(anydf$GeneLowPoint)
      anydf$GeneHighPoint<-as.character(anydf$GeneHighPoint) 
      anydf$GeneLowPoint[anydf$GeneLowPoint=="0"]<-""
      anydf$GeneHighPoint[anydf$GeneHighPoint=="0"]<-""      
      anydf$OMIM.HTML.web[anydf$OMIM!=""]<-paste("<a href=\"http://www.ncbi.nlm.nih.gov/entrez/dispomim.cgi?id=",anydf$OMIM[anydf$OMIM!=""],"\">OMIM</a>",sep="")
      anydf$OMIM<-anydf$OMIM.HTML.web
      anydf$OMIM.HTML.web<-NULL
      anydf$phenotypes<-anydf$phenotypes.HTML
      anydf$phenotypes.HTML<-NULL
      anydf$pathways<-anydf$pathways.HTML
      anydf$pathways.HTML<-NULL
      if(kp==TRUE & "pathways" %in% names(anydf)==TRUE)
         anydf$pathways<-gsub("KEGG pathway","KP",anydf$pathways)
      anydf$Int.GeneNames<-anydf$Int.GeneNames.HTML
      anydf$Int.GeneNames.HTML<-NULL
      anydf$genelink<-anydf$gene.HTML.web 
      anydf$gene.HTML.web<-NULL
      }
      
   anydf$genename[anydf$locusID!=""]<-paste("<a href=\"http://www.ncbi.nlm.nih.gov/portal/query.fcgi?p$site=entrez&db=gene&cmd=Display&dopt=gene_pubmed&from_uid=",anydf$locusID[anydf$locusID!=""],"\">",anydf$genename[anydf$locusID!=""],"</a>",sep="")
   anydf$genesymbol[anydf$locusID!=""]<-paste("<a href=\"http://www.ncbi.nlm.nih.gov/sites/entrez?db=gene&term=",anydf$locusID[anydf$locusID!=""],"\">",anydf$genesymbol[anydf$locusID!=""],"</a>",sep="")
   
   if(keepNS==FALSE)
      anydf$NeighString<-NULL
   anydf$Neigh.xlweb<-NULL
   anydf<-clean.NAs(anydf)
   anydf$Neigh<-anydf$NeighHTMLlink
   anydf$NeighHTMLlink<-NULL
  if(markercolumn %in% names(anydf)==TRUE) 
      {
      anydf$GWA<-paste("http://www.genome.gov/gwastudies/index.cfm?snp=",anydf[,markercolumn],sep="")
      anydf$GWA<-ConvertURLToHTML(anydf$GWA,"GWA")
      anydf[,markercolumn]<-paste("<a href=\"http://www.ncbi.nlm.nih.gov/SNP/snp_ref.cgi?rs=",anydf[,markercolumn],"\">",anydf[,markercolumn],"</a>",sep="")
      if(nrow(anydf)>2)  
         {  
         for(i in 1:(nrow(anydf)-2)) 
            {
            if(anydf[i+1,markercolumn]==anydf[i+2,markercolumn]) 
               {                                                
               if(anydf[i,"locusID"]==anydf[i+2,"locusID"])
                  anydf<-swap.rows(anydf,i+1,i+2)
               }
            }
         for(i in 1:(nrow(anydf)-2))
            {
            if(anydf[i,markercolumn]==anydf[i+1,markercolumn])   
               {                                                 
               if(anydf[i,"locusID"]==anydf[i+2,"locusID"])
                  anydf<-swap.rows(anydf,i+1,i+2)
               }
            }
         for(i in nrow(anydf):2) 
            {
            if((anydf[i,markercolumn]==anydf[i-1,markercolumn]) & (anydf[i-1,markercolumn]!=""))
               {
               anydf[i,markercolumn]<-paste("<B>",anydf[i,markercolumn],"</B>",sep="")  
               anydf[i-1,markercolumn]<-paste("<B>",anydf[i-1,markercolumn],"</B>",sep="") 
               }
            }    
         } 
      } 
   if(keeplocusIDs==FALSE)
      anydf$locusID<-NULL
   if(compress==TRUE & nrow(anydf)>1)  
      {
      anydf<-compress.df(anydf,"genesummary")
      anydf<-compress.df(anydf,"genename")
      anydf<-compress.df(anydf,"phenotypes")          
      anydf<-compress.df(anydf,"pathways")       
      anydf<-compress.df(anydf,"OMIM")
      anydf<-compress.df(anydf,"synonyms")
      anydf<-compress.df(anydf,"GeneLowPoint") 
      anydf<-compress.df(anydf,"GeneHighPoint")      
      anydf<-compress.df(anydf,"ori")
      anydf<-compress.df(anydf,"pathways")
      anydf<-compress.df(anydf,"phenotypes") 
      anydf<-compress.df(anydf,"Neigh",markercolumn) 
      anydf<-compress.df(anydf,"Neigh")    
       }                         
   if(!(markercolumn %in% names(anydf)==TRUE))
      {anydf<-move.column(anydf,"genesymbol",1)      
       anydf<-move.column(anydf,"genename",2)      
      }
   anydf$GOcomp<-NULL
   anydf$GOproc<-NULL
   anydf$GOfunc<-NULL
   if(deletesome==TRUE)
     {
     anydf$genesymbol<-NULL
     anydf$genename<-NULL
     anydf$OMIM<-NULL
     anydf$GeneLowPoint<-NULL
     anydf$GeneHighPoint<-NULL
     }
   
   HTMLpageoutput<-paste("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/transitional.dtd\"><HTML><HEAD><TITLE>NCBI2R - ",Sys.time(),"(",URLdef$buildversion,")</TITLE></HEAD><BODY><TABLE border=\"1\">",sep="")
   HTMLpageoutput<-c(HTMLpageoutput,"<tr>") 
   column.width.df<-data.frame(colnames=rep("",10),pixels=rep(100,10),stringsAsFactors=FALSE)
   column.width.df[1,]<-c("Int.GeneIDs",200)
   column.width.df[2,]<-c("Int.genesymbols.HTML",20)
   column.width.df<-column.width.df[column.width.df$colnames!="",] 
   for(i in 1:length(colnames(anydf)))
      {
      if(colnames(anydf)[i] %in% column.width.df$colnames) {
         HTMLpageoutput<-c(HTMLpageoutput,paste("<td width=\"",column.width.df[which(colnames(anydf)[i]==column.width.df$colnames),"pixels"],"\">",colnames(anydf)[i],"</td>",sep=""))
         } else { 
         HTMLpageoutput<-c(HTMLpageoutput,paste("<td>",colnames(anydf)[i],"</td>",sep=""))
         }
      }
   HTMLpageoutput<-c(HTMLpageoutput,"</tr>") 
   for(j in 1:nrow(anydf)) 
      {
      for(i in 1:length(colnames(anydf)))   
         {
         if(is.na(anydf[j,i])) 
            HTMLpageoutput<-c(HTMLpageoutput,paste("<td></td>")) 
         if(anydf[j,i]!="as above")
            {
            if(j<nrow(anydf)) 
               {
               if(anydf[j+1,i]=="as above")  
                  {
                  thisrow<-j+2      
                  rowspan<-2
                  while(anydf[thisrow,i]=="as above" & thisrow<=nrow(anydf))
                     {
                     rowspan<-rowspan+1
                     thisrow<-thisrow+1
                     }
                 thisstring<-paste("<td rowspan=\"",rowspan,"\">",anydf[j,i],"</td>",sep="")
                  HTMLpageoutput<-c(HTMLpageoutput,thisstring)
                  } else    {
                  HTMLpageoutput<-c(HTMLpageoutput,paste("<td>",anydf[j,i],"</td>",sep=""))
                  } 
               }
            if(j==nrow(anydf) & anydf[j,i]!="as above")    
               HTMLpageoutput<-c(HTMLpageoutput,paste("<td>",anydf[j,i],"</td>",sep=""))
            } 
         } 
      HTMLpageoutput<-c(HTMLpageoutput,"</tr>")
      } 
   HTMLpageoutput<-c(HTMLpageoutput,"</table></body></html>")
   write.table(HTMLpageoutput,file=filename,row.names=FALSE, col.names=FALSE, quote=FALSE)
   a<-paste("HTML file was written at ",getwd(),"/",filename,sep="")
   return(a=a) 
   }
    