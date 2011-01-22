
get.uni.sts.info.cross.ref.available<-function(chunk1)
  {
   www<-createframe(chunk1) 
      CrossRef<-cleanframe(www)
      CrossRef<-as.data.frame(cbind(CrossRef,GeneID=rep(0),Symbol=rep(""),UniGene=rep(""),Description=rep(""),Position=rep(""),stringsAsFactors=FALSE))
      i<-0
      lastheading<-""
      if(length(grep("SNP",CrossRef$V1))>0)
         CrossRef<-CrossRef[grep("SNP",CrossRef$V1,invert=TRUE),]
      while(i<nrow(CrossRef))
         {
         i<-i+1
         ExpectedResponses<-c("Gene","UniGene")
         if(CrossRef[i,1]=="&nbsp;" & lastheading=="UniGene")
            CrossRef[i,1]<-lastheading
         if(is.na(pmatch(CrossRef[i,1],ExpectedResponses))==TRUE)
            stop("NCBI2R error: GetUniSTSInfo error 4 - bad parse of record")
         if(CrossRef[i,1]=="Gene")
            {
            toprow<-i;            bottomrow<-toprow
            CrossRef[toprow,"GeneID"]<-CrossRef[i,3] 
            while(i<nrow(CrossRef) & CrossRef[i+1,1]=="&nbsp;")
               {
               i<-i+1
               CrossRef[toprow,splitfirst(CrossRef[i,2],":")]<-CrossRef[i,3]
               }
           lastheading<-"Gene"
            }
         if(CrossRef[i,1]=="UniGene")
            {
            CrossRef[i,"UniGene"]<-CrossRef[i,2]
            CrossRef[i,"Description"]<-CrossRef[i,3];     lastheading<-"UniGene"
            } 
        }
     CrossRef<-CrossRef[CrossRef$V1!="&nbsp;",]
     CrossRef$V1<-NULL;    CrossRef$V2<-NULL    ;     CrossRef$V3<-NULL
     CrossRef<-CrossRef[order(CrossRef$Description,decreasing=TRUE),]     
     if((length(unique(CrossRef$Description))!=length(CrossRef$Description)) & (nrow(CrossRef)>1))
       {
        for(j in 1:(nrow(CrossRef)-1))
           {
           if((CrossRef[j,"Description"]!="") & (CrossRef[j,"Description"]==CrossRef[j+1,"Description"]) & (CrossRef[j,"UniGene"]=="") & CrossRef[j,"GeneID"]!=0)
             {
             CrossRef[j,"UniGene"]<-CrossRef[j+1,"UniGene"]
             CrossRef[j+1,"UniGene"]<-"";         CrossRef[j+1,"Description"]<-""
             }
           CrossRef<-CrossRef[CrossRef$GeneID!=0 | CrossRef$UniGene!="",]  
        }
       } 
     CrossRef$GeneID<-as.numeric(CrossRef$GeneID)
     CrossRef<-CrossRef[order(CrossRef$Symbol),]
     return(CrossRef)
     }
     