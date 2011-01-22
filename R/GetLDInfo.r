GetLDInfo<-function(chr,pos1,pos2,showurl=FALSE,filter="",build="27_B36",pop="CEU",db="HapMap",quiet=FALSE)
    {
    if(toupper(db)!=c("HAPMAP","SNAP"))
        stop("NCBI2R GetLDInfo error: incorrect db specified. either hapmap or snap required")
    if(build!="3r2_B36" & build!="27_B36")
      stop("NCBI2R GetLDInfo error: Unknown build specified. GetLDInfo function has failed")
    if(abs(pos1-pos2)>=2000000)
       stop("NCBI2R GetLDInfo error: The range you specified was greater than 2Mb. Please fix and try again.")
    if(quiet==TRUE)
       showurl<-FALSE
    pos1<- formatC(pos1,digits=9,width=1)
    pos2<- formatC(pos2,digits=9,width=1)
    if(missing(chr))
       stop("NCBI2R GetLDInfo error: No chromosome specified. GetLDInfo function has failed")
    if(missing(pos1))
       stop("NCBI2R GetLDInfo error: No lower genome position specified. GetLDInfo function has failed")
    if(missing(pos2))
       stop("NCBI2R GetLDInfo error: No upper genome position specified. GetLDInfo function has failed")
    cn<-c("chrpos1","chrpos2","pop","SNPA","SNPB","Dprime","r2","LOD") 
    getURL<-paste("http://www.hapmap.org/cgi-perl/gbrowse/hapmap",build,"/?name=Chr",chr,":",pos1,"..",pos2,";plugin=LDPhase3Dumper;plugin_action=Go;plugin_config=1;LDPhase3Dumper.pop_code=",pop,sep="")
    if(showurl)  
       print(getURL)
    LDdata<-GetTableWithComments(getURL,columnnames=cn,sep=" ",filter=filter)
    if(class(LDdata)!="character")
       {
        LDdata$chrpos1<-as.numeric(LDdata$chrpos1)
        LDdata$chrpos2<-as.numeric(LDdata$chrpos2)
        LDdata$Dprime<-as.numeric(LDdata$Dprime)
        LDdata$r2<-as.numeric(LDdata$r2)
        LDdata$distance<-abs(LDdata$chrpos1-LDdata$chrpos2)
        if(quiet==FALSE)
           print("NCBI2R message: LD data downloaded")
        return(LDdata)
      }  
    if(class(LDdata)=="character")
       {
       return("No LDdata available")
       }
    }
