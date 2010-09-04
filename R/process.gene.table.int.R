process.gene.table.int<-function(webget)
   {
   data_summary_start<-grep("mRNA   bp   exons   Protein   aa   exons",webget)+1
   if(length(data_summary_start)!=0 & length(grep("discontinued",webget))==0 & length(grep("withdrawn",webget))==0)  
       {
        Heading<-webget[1:(data_summary_start-2)]
        Heading[1]<-substr(Heading[1],Excel.FIND(":",Heading[1])+2,nchar(Heading[1]))
        LC<-2 
        while(substr(Heading[LC],1,6)!="GeneID")
           LC<-LC+1  
        if(LC!=2)
           {
            Heading[1]<-paste(Heading[1:(LC-1)],collapse=" ")
            Heading[2:(LC-1)]<-""
            Heading<-Heading[Heading!=""]
            }


        nc<-nchar(Heading[1])
        keep<-TRUE
        while(nc>1  & keep)
          { if(substr(Heading[1],nc,nc)=="[")
              keep<-FALSE
           nc<-nc-1
           }
        if(nc==1)
           stop("NCBI2R GetGeneTable parse error 3")
         genename<-substr(Heading[1],1,nc)
         species<-RemoveSpaces(gsub("\\[|\\]","",substr(Heading[1],nc+1,nchar(Heading[1]))))
         h2<-unlist(strsplit(Heading[2]," "))
         if(length(h2)!=4)
           stop("NCBI2R GetGeneTable parse error 4")
         locusID<-h2[2]
         updated<-RemoveSpaces(gsub("updated","",paste(h2[3],h2[4],""))) 
         if(length(grep("RefSeq status",Heading[3]))==1)
           {
            status<-RemoveSpaces(gsub("RefSeq status:", "",Heading[3]))
            }        else  {
            status<-""
            }
         d1<-as.data.frame(cbind(locusID=locusID,genename=genename,species=species,updated=updated,status=status),stringsAsFactors=FALSE)
         if(status!="WITHDRAWN")   {
             data_summary_end<-grep("Exon information:",webget)-1
             ds1<-unlist(strsplit(webget[data_summary_start:data_summary_end]," "))
              ds2<-ds1[ds1!=""]
              
              if(length(ds2) != (data_summary_end-data_summary_start+1)*6)
                { print(paste("NCBI2R GeneTable Parse error - bad NCBI record",locusID))
                } else
                {
                 
              ds3<-as.data.frame(matrix(ds2,ncol=6,byrow=TRUE),stringsAsFactors=FALSE)
              
              
              ACC.DNA<-ds3[,c("V1","V2","V3")]
              ACC.Prot<-ds3[,c("V4","V5","V6")]
              names(ACC.DNA)<-c("Identifier","Length","Exons")
              names(ACC.Prot)<-c("Identifier","Length","Exons")
              startlines<-grep("EXON      Coding EXON",webget)-2
              if(nrow(ACC.DNA)==0)
                 stop("NCBI2R GetGeneTable parse error 1")
              if(nrow(ACC.DNA)==1)
                {
                 stoplines<-length(webget)-1
                } else {
                 stoplines<-c(startlines[2:length(startlines)]-1,length(webget)-1)
                 }
              for(i in 1:length(startlines))
                  {
                  p<-paste(webget[(startlines[i]+4):stoplines[i]],collapse=" ")
                  p2<-unlist(strsplit(p," "))
                  p3<-p2[p2!=""]
                  if(length(p3)/5!=floor(length(p3)/5))
                     stop("NCBI2R GetGeneTable parse error 2")
                  p4<-as.data.frame(matrix(p3,ncol=5,byrow=TRUE),stringsAsFactors=FALSE)


                  p5<-p4[,c("V1","V3","V4")]
                  names(p5)<-c("Start","Stop","Size") 
                  Where<-""
                  for(j in (startlines[i]+4):(stoplines[i]))          
                     {
                      t1<-unlist(strsplit(webget[j]," "))         
                      count<-length(t1[t1!=""])
                      if((count!=5 & count!=10 & count !=15 & count!=0))
                         {
                         webget[j]<-paste(webget[j],webget[j+1],sep=" ")
                         webget[j+1]<-""
                         t1<-unlist(strsplit(webget[j]," "))
                         count<-length(t1[t1!=""])
                         }
                if(count==10)
                   {Where<-c(Where,"Exon","CodExon")}
                if(count==5) 
                   {Where<-c(Where,"Exon")}
                if(count==15)
                   {Where<-c(Where,"Exon","CodExon","Intron")}
                if(count==0)
                   {dummy<-7}
               }
           p5$Where<-Where[2:length(Where)]

            p5$Set<-i
            p7<-p5[,c("Where","Start","Stop","Size","Set")]
            p7$Start<-as.numeric(p7$Start)
            p7$Stop<-as.numeric(p7$Stop)
            p7$Size<-as.numeric(p7$Size)
            
            
            if("ExonInfo" %in% ls())
              { ExonInfo<-as.data.frame(rbind(ExonInfo,p7),stringsAsFactors=FALSE)
              } else {
               ExonInfo<-p7
               }
            }
         }
       }
    }


   if(exists("ExonInfo"))
      {return(list(ExonInfo=ExonInfo,ACC.DNA=ACC.DNA,ACC.Prot=ACC.Prot,RecordInfo=d1))
      }    else  {
      return("No information available")
     }



   }