PrintFilters <-
function()
   {
   NumberOfFilters<-100 
   filters<-data.frame(Name=rep("",NumberOfFilters),Definition="",stringsAsFactors = FALSE)
   filters[1,]<-c("all","Total records, current or not")
   filters[2,]<-c("gene all","All current records")
   filters[3,]<-c("gene books","Gene records with explicit links to Entrez Books")
   filters[4,]<-c("gene gensat","Gene records with explicit links to Entrez GenSAT")
   filters[5,]<-c("gene geo","Gene records with explicit links to Entrez GEO")
   filters[6,]<-c("gene homologene","Gene records with explicit links to Entrez HomoloGene")
   filters[7,]<-c("gene nucleotide","Gene records with explicit links to Entrez Nucleotide, excluding RefSeq chromosome or contig accessions")
   filters[8,]<-c("gene nucleotide pos","Gene records with explicit links to Entrez Nucleotide, limited to those of RefSeq chromosome or contig accessions, and thus including position data")
   filters[9,]<-c("gene omim","Gene records with explicit links to Entrez OMIM, and thus includes links to both disease and \"gene\" records in OMIM")
   filters[10,]<-c("gene protein","Gene records with explicit links to Entrez Protein, and thus includes links to GenPept and SwissProt accessions")
   filters[11,]<-c("gene pubmed","Gene records with explicit links to Entrez PubMed")
   filters[12,]<-c("gene snp","Gene records with explicit links to Entrez dbSNP, and thus supports finding gene variation information available in dbSNP")
   filters[13,]<-c("gene taxonomy","Gene records with explicit links to Entrez Taxonomy")
   filters[14,]<-c("gene unigene","Gene records with explicit links to Entrez UniGene")
   filters[15,]<-c("gene unists","Gene records with explicit links to Entrez UniSTS (marker data)")
   filters<-(filters[filters$Name!="",])
   print(cat(" This information was taken from NCBI's web site on the last build date of this package and may have changed(?copyright?))","\n","Other filters(if that is what these are) may be presently available.","\n"," Check this URL http://www.ncbi.nlm.nih.gov/books/bookres.fcgi/handbook/ch19.pdf for more information","\n"))
   return(filters)
   } 

