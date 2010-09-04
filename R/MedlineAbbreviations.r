MedlineAbbreviations<-function()
   {
  desc1<-c("Abstract","Affiliation","Article_Identifier","Author","Book_Title","Copyright_Information","Corporate_Author","Create_Date","Collection_Title","Date_Created","Date_Completed","Date_of_Electronic_Publication","Date_of_Publication","Editor_Name","Entrez_Date")
  abrv1<-c("AB","AD","AID","AU","BTI","CI","CN","CRDT","CTI","DA","DCOM","DEP","DP","ED","EDAT")
  desc2<-c("Edition","Full_Author","Full_Editor_Name","Full_Investigator_Name","Full_Personal_Name_as_Subject","General_Note","Grant_Number","Gene_Symbol","Issue","Investigator_Name","Investigator_Affiliation","ISSN","ISBN","NLM_Unique_ID","Journal_Title","Language","Location_Identifier","Date_Last_Revised","MeSH_Terms","MeSH_Date","Manuscript_Identifier","Substance_Name","Other_Abstract","Other_Copyright_Information","Other_ID","Other_Term","Other_Term_Owner","Owner","Pagination","Publication_History_Status","Place_of_Publication","PubMed_Central_Identifier","PubMed_Central_Release","PubMed_Unique_Identifier","Personal_Name_as_Subject","Publication_Status","Publication_Type","Publishing_Model","Number_of_References","Registry_Number/EC_Number","Subset","Space_Flight_Mission","Secondary_Source_ID","Source","Status","Journal_Title_Abbreviation","Title","Transliterated_Title","Volume","Volume_Title")
  abrv2<-c("EN","FAU","FED","FIR","FPS","GN","GR","GS","IP","IR","IRAD","IS","ISBN","JID","JT","LA","LID","LR","MH","MHDA","MID","NM","OAB","OCI","OID","OT","OTO","OWN","PG","PHST","PL","PMC","PMCR","PMID","PS","PST","PT","PUBM","RF","RN","SB","SFM","SI","SO","STAT","TA","TI","TT","VI","VTI")
  desc3<-c("Comment_on","Comment_in","Erratum_in","Erratum_for","Corrected_and_Republished_in","Corrected_and_Republished_from","Partial_retraction_in","Partial_retraction_of","Republished_in","Republished_from","Retraction_in","Retraction_of","Update_in","Update_of","Summary_for_patients_in","Original_report_in")
  abrv3<-c("CON","CIN","EIN","EFR","CRI","CRF","PRIN","PROF","RPI","RPF","RIN","ROF","UIN","UOF","SPIN","ORI")
  p<-unique(as.data.frame(rbind(cbind(desc=desc1,abrv=abrv1),cbind(desc=desc2,abrv=abrv2),cbind(desc=desc3,abrv=abrv3)),stringsAsFactors=FALSE))
  p<-p[order(p$abrv),]
   return(p)
   }