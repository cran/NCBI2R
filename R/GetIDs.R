GetIDs <-
function(term, org = "human", strict=TRUE, cg=TRUE,db="gene",MaxRet=30000,showurl=FALSE,sme=FALSE,smt=FALSE)
   {
    db<-toupper(db)
    if(db!="GENE" & toupper(db)!="SNP" & toupper(db)!="UNISTS")
       stop("this database is not specified")
   if(org=="" | org=="all")
      orgstring<-""
   if(org!="")
      orgstring<-paste("+",org,"[ORGN]",sep="")
   adj_term<-URLcreator(term,db=db,org=org,cg=cg) 
   URLdef<-URLdefinitions()
   getURL<-paste(URLdef$front,"esearch.fcgi?db=",db,"&term=",adj_term,"&retmax=",MaxRet,"&rettype=FASTA",URLdef$back,sep="")
   getURL<-gsub(" ","%20",getURL)    
   webget<-get.file(getURL,showurl,clean=FALSE)
   ListItems<-GetListFromXML(webget,sme=sme,smt=smt)
   if(length(ListItems)>0 & db=="SNP")
     {
     if(length(ListItems>1))
        {
        print("The following rs identifiers are returned")
        print(ListItems)
        Check<-GetSNPCurrent(ListItems)
        print("But one was found after Checking for merged records")
        ListItems<-Check
        }
     ListItems<-paste("rs",Check,sep="")
     }
    if(db=="GENE" & strict==TRUE & substr(term,nchar(term)-4,nchar(term))=="[sym]" & (length(ListItems[ListItems!=""]))>1) 
      {
      clashbatch<-GetGeneInfo(ListItems[ListItems!=""])
      answer<-as.character(clashbatch[clashbatch$genesymbol==substr(term,1,nchar(term)-5),"locusID"])
      ListItems<-answer
      }
   return(ListItems)  
   }  
