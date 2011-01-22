get.int.int <-
function(webget,LC=1,div=",")
   {                                                                                 
   Interactions<-data.frame(RefNo=rep(0,10000),GeneComm="",Refs="",Source_db="",Source_obj_id="",Source_gct="",Source_obj_str="",Source_osa="",Product_db="",Product_obj_id="",Product_gct="",Product_osa="",OtherGene_gct="",OtherGene_osa="",OtherGene_obj_id="",OtherGene_db="",Interactant_db="",Interactant_obj_id="",Interactant_gct="",Interactant_osa="",stringsAsFactors=FALSE)
   joinedtags<-rep("",100000)
   LC<-skimUntil("<Gene-commentary_heading>Interactions<",webget,LC)
   if(is.na(webget[LC]))
      stop("no interactions found")
   GoAgain<-TRUE
   InteractionText<-""
   ThisRecord<-""
   Record<-1
   while(GoAgain==TRUE) 
      {
      ThisRecord<-"" 
      while(webget[LC]!="<Date>")
         {
         if(nchar(gregexpr("<",webget[LC]))>2)
            {
            if(ThisRecord[1]=="") {
               ThisRecord<-webget[LC]
               } else {
               ThisRecord<-c(ThisRecord,webget[LC])
               }
            }
         LC<-LC+1
         }
      joinedtags[Record]<-""
      for(singlelinechecker in 1:length(ThisRecord))
          joinedtags[Record]<-paste(joinedtags[Record],splitfirst(ThisRecord[singlelinechecker],">")[1])
      if(InteractionText[1]=="") {
         InteractionText<-paste("Record ",Record)
         } else  {
         InteractionText<-c(InteractionText,paste("Record ",Record))
         }
      InteractionText<-c(InteractionText,ThisRecord)
      GoAgain<-FALSE  
      for(simpleloop in LC:(LC+70)) 
         {
         if(webget[simpleloop]=="<Gene-commentary_type value=&quot;generif&quot;>18</Gene-commentary_type>")
            {
            GoAgain<-TRUE
            LC<-simpleloop
            Record<-Record+1
            }
         }  
      }    
         
   LineNumber_i<-1  
   Interactions_j<-0 

   while(substr(InteractionText[LineNumber_i],1,7)=="Record " & LineNumber_i<length(InteractionText))
      {
      Interactions_j<-Interactions_j+1
      Interactions$RefNo[Interactions_j]<-substr(InteractionText[LineNumber_i],9,nchar(InteractionText[LineNumber_i]))
      LineNumber_i<-LineNumber_i+1 
      Checker<-finder("<Gene-commentary_text>","<PubMedId>",InteractionText,LineNumber_i)
      LineNumber_i<-Checker$RowNumber
      if(Checker$Object==1)
         {
         Interactions$GeneComm[Interactions_j]<-substr(InteractionText[Checker$RowNumber],23,nchar(InteractionText[Checker$RowNumber])-23)
         LineNumber_i<-Checker$RowNumber+1
         }
      while(substr(InteractionText[LineNumber_i],1,10)=="<PubMedId>")
         {
         filteroff<-substr(InteractionText[LineNumber_i],11,nchar(InteractionText[LineNumber_i])-11)
         if(Interactions$Refs[Interactions_j]!="") {
            Interactions$Refs[Interactions_j]<-paste(Interactions$Refs[Interactions_j],div,filteroff)
            } else {
            Refs<-filteroff
            }
         LineNumber_i<-LineNumber_i+1
         }
      rm(filteroff)
      if(substr(InteractionText[LineNumber_i],1,10)=="<Dbtag_db>" & LineNumber_i<=length(InteractionText))
         {
         Interactions$Source_db[Interactions_j]<-substr(InteractionText[LineNumber_i],11,nchar(InteractionText[LineNumber_i])-11) 
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,10)=="<Object-id" & LineNumber_i<=length(InteractionText))
         {

         if(substr(InteractionText[LineNumber_i],1,14)=="<Object-id_str")
            Interactions$Source_obj_str[Interactions_j]<-substr(InteractionText[LineNumber_i],16,nchar(InteractionText[LineNumber_i])-16) 

         if(substr(InteractionText[LineNumber_i],1,13)=="<Object-id_id")
            {
            Interactions$Source_obj_id[Interactions_j]<-substr(InteractionText[LineNumber_i],15,nchar(InteractionText[LineNumber_i])-15) 
            }
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,20)=="<Other-source_anchor" & LineNumber_i<=length(InteractionText))
         {
         Interactions$Source_osa[Interactions_j]<-substr(InteractionText[LineNumber_i],22,nchar(InteractionText[LineNumber_i])-22)
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,21)=="<Gene-commentary_type" & LineNumber_i<=length(InteractionText))
         {
         Interactions$Source_gct[Interactions_j]<-substr(InteractionText[LineNumber_i],22,nchar(InteractionText[LineNumber_i])-22)
         LineNumber_i<-LineNumber_i+1
         }

      if(substr(InteractionText[LineNumber_i],1,10)=="<Dbtag_db>" & LineNumber_i<=length(InteractionText))
         {
         Interactions$Product_db[Interactions_j]<-substr(InteractionText[LineNumber_i],11,nchar(InteractionText[LineNumber_i])-11) 
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,10)=="<Object-id" & LineNumber_i<=length(InteractionText))
         {
         if(substr(InteractionText[LineNumber_i],1,14)=="<Object-id_str")
            Interactions$Product_obj_str[Interactions_j]<-substr(InteractionText[LineNumber_i],16,nchar(InteractionText[LineNumber_i])-16) 
         if(substr(InteractionText[LineNumber_i],1,13)=="<Object-id_id")
            Interactions$Product_obj_id[Interactions_j]<-substr(InteractionText[LineNumber_i],15,nchar(InteractionText[LineNumber_i])-15) 
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,20)=="<Other-source_anchor" & LineNumber_i<=length(InteractionText)) 
         {
         Interactions$Product_osa[Interactions_j]<-substr(InteractionText[LineNumber_i],22,nchar(InteractionText[LineNumber_i])-22)
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,21)=="<Gene-commentary_type" & LineNumber_i<=length(InteractionText))
         {
         Interactions$Product_gct[Interactions_j]<-substr(InteractionText[LineNumber_i],22,nchar(InteractionText[LineNumber_i])-22)
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,10)=="<Dbtag_db>" & LineNumber_i<=length(InteractionText))
         {
         Interactions$OtherGene_db[Interactions_j]<-substr(InteractionText[LineNumber_i],11,nchar(InteractionText[LineNumber_i])-11) 
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,10)=="<Object-id" & LineNumber_i<=length(InteractionText))
         {
         if(substr(InteractionText[LineNumber_i],1,14)=="<Object-id_str")
            Interactions$OtherGene_obj_str[Interactions_j]<-substr(InteractionText[LineNumber_i],16,nchar(InteractionText[LineNumber_i])-16) 
         if(substr(InteractionText[LineNumber_i],1,13)=="<Object-id_id")
            Interactions$OtherGene_obj_id[Interactions_j]<-substr(InteractionText[LineNumber_i],15,nchar(InteractionText[LineNumber_i])-15)
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,20)=="<Other-source_anchor" & LineNumber_i<=length(InteractionText))
         {
         Interactions$OtherGene_osa[Interactions_j]<-substr(InteractionText[LineNumber_i],22,nchar(InteractionText[LineNumber_i])-22)
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,21)=="<Gene-commentary_type" & LineNumber_i<=length(InteractionText))
         {
         Interactions$OtherGene_gct[Interactions_j]<-substr(InteractionText[LineNumber_i],22,nchar(InteractionText[LineNumber_i])-22)
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,10)=="<Dbtag_db>" & LineNumber_i<=length(InteractionText))
         {
         Interactions$Interactant_db[Interactions_j]<-substr(InteractionText[LineNumber_i],11,nchar(InteractionText[LineNumber_i])-11)
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,10)=="<Object-id" & LineNumber_i<=length(InteractionText))
         {
         if(substr(InteractionText[LineNumber_i],1,14)=="<Object-id_str")
            Interactions$Interactant_obj_str[Interactions_j]<-substr(InteractionText[LineNumber_i],16,nchar(InteractionText[LineNumber_i])-16) 
         if(substr(InteractionText[LineNumber_i],1,13)=="<Object-id_id")
            Interactions$Interactant_obj_id[Interactions_j]<-substr(InteractionText[LineNumber_i],15,nchar(InteractionText[LineNumber_i])-15) 
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,20)=="<Other-source_anchor" & LineNumber_i<=length(InteractionText))
         {
         Interactions$Interactant_osa[Interactions_j]<-substr(InteractionText[LineNumber_i],22,nchar(InteractionText[LineNumber_i])-22)
         LineNumber_i<-LineNumber_i+1
         }
      if(substr(InteractionText[LineNumber_i],1,21)=="<Gene-commentary_type" & LineNumber_i<=length(InteractionText))
         {
         Interactions$Interactant_gct[Interactions_j]<-substr(InteractionText[LineNumber_i],22,nchar(InteractionText[LineNumber_i])-22)
         LineNumber_i<-LineNumber_i+1
         }
      } 

   Interactions<-Interactions[1:Interactions_j,] 
   Interactions$Source_gct<-gsub("value=&quot;","",Interactions$Source_gct)
   Interactions$Product_gct<-gsub("value=&quot;","",Interactions$Product_gct)
   Interactions$Source_gct<-gsub("&quot;>","-",Interactions$Source_gct)
   Interactions$Product_gct<-gsub("&quot;>","-",Interactions$Product_gct)
   Interactions$Source_gct<-gsub("<","",Interactions$Source_gct)
   Interactions$Product_gct<-gsub("<","",Interactions$Product_gct)
   Interactions$RefNo<-NULL 
   
   return(list(Interactions=Interactions,LC=LC)) 
   } 

