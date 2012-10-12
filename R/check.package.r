
getAllCRANMirrors<-function()
  {
  allmirrors<-scan("http://cran.r-project.org/mirrors.html",what="character",sep="\n",quiet=TRUE)
  allmirrors<-allmirrors[grep("http:",allmirrors)]
  allmirrors<-unique(allmirrors[substr(allmirrors,1,4)=="http"])
  return(allmirrors)
  }

checkAvailablePackageVersion<-function(mirror,package="NCBI2R")
   {
   if(missing(mirror))
     stop("no mirror vector specified")
   p<-scan(paste(mirror,"web/packages/",package,"/index.html",sep=""),what="character",sep="\n",quiet=TRUE)
   PkgSrc<-gsub(paste("[[:print:]]*",package,"_([[:print:]]*)\\.tar.gz[[:print:]]*",sep=""),"\\1",p[grep("Package source",p)])
   MacOS_X<-gsub(paste("[[:print:]]*",package,"_([[:print:]]*)\\.tgz[[:print:]]*",sep=""),"\\1",p[grep("MacOS X binary",p)])
   Win<-gsub(paste("[[:print:]]*",package,"_([[:print:]]*)\\.zip[[:print:]]*",sep=""),"\\1",p[grep("Windows binary",p)])
   return(as.data.frame(cbind(PkgSrc=PkgSrc,MacOS_X=MacOS_X,Win=Win),stringsAsFactors=FALSE))
   }

getPackageVersionsOnAllMirrors<-function(package="NCBI2R")
   {
   t1<-getAllCRANMirrors()
   mdf<-as.data.frame(cbind(mirror=t1,PkgSrc="NA",MacOS_X="NA",Win="NA"),stringsAsFactors=FALSE)
   for(m in 1:length(t1))
     {
     cat(paste("\r",as.character(m),"of",length(t1)))
     flush.console()
     j<-try(checkAvailablePackageVersion(t1[m],package=package))
     if(class(j)!="try-error")
       mdf[m,2:4]<-j
     }
  versions<-sort(unique(c(mdf[,2],mdf[,3],mdf[,4]))) 
  t234<-as.data.frame(matrix(0,ncol=length(versions),nrow=3))
  row.names(t234)<-c("Src","Mac","Win")
  names(t234)<-versions
  for(k in 1:3)
    {
    for(l in 1:length(versions))
      t234[k,l]<-nrow(mdf[mdf[,k+1]==versions[l],])
    }
   print(t234)
   return(list(bySite=mdf,summary=t234))
   }


package.check.status<-function(fn="allerrors.txt",package="NCBI2R")
   {
   a1<-scan(paste("http://cran.r-project.org/web/checks/check_results_",package,".html",sep=""),what="character",sep="\n",quiet=TRUE)
   start.tab<-grep("<table",a1)
   stop.tab<-grep("</table",a1)
   headers<-a1[(start.tab+1):(stop.tab-1)][1]
   h2<-unlist(strsplit(gsub("<tr>|</th>|</tr>|<sub>|</sub>","\\1",headers)," <th> "))
   h2<-h2[h2!=""]
   h2<-gsub(" ","",h2)
   rest<-a1[(start.tab+1):(stop.tab-1)]
   rest<-rest[2:length(rest)]
   u3<-grep("OK|NOTE|WARN",rest,invert=TRUE)
   bad1<-rest[u3]
   allerrors<-c()
   bad.functions<-c()
   if(length(bad1)>0)
      {
      webpages.with.errs.warns<-unique(gsub("[[:print:]]*(http[[:print:]]*00check.html)[[:print:]]*","\\1",bad1))
      allerrors<-c()
      for(i in 1:length(bad1))
         {
         this.fc<-scan(webpages.with.errs.warns[i],what="character",sep="\n",quiet=TRUE)
         this.fc<-this.fc[(grep("<body>",this.fc)+1):(length(this.fc))]
         this.fc<-c("","",webpages.with.errs.warns[i],"","",this.fc)
         allerrors<-c(allerrors,this.fc)

         tmp<-this.fc[grep("Name:",this.fc)]
         if(length(tmp)==1)
           writeLines("Two errors found. Probably one each windows architectures 32 and 64bit")
         first.bad.function<-tmp[1]
         bad.functions<-c(bad.functions,first.bad.function)
         }
      bad.functions<-gsub("^[[:print:]]*[[:blank:]]Name:[[:blank:]]","",bad.functions)
      bad.functions<-gsub("<br/>","",bad.functions)
      allerrors<-gsub("&gt;",">",allerrors)
      allerrors<-gsub("</br/>|</li>|<li>|</html>","",allerrors)
        if(!is.null(fn))
         write.table(allerrors,fn,row.names=FALSE,col.names=FALSE,quote=FALSE,sep="\n")
      }
   f<-unlist(strsplit(paste(rest,collapse=" "),"</td>"))
   f<-gsub("</font>|</a>|<td>|<td align=\\\"right\\\">","",f)
   f2<-gsub("[[:print:]]*<a href=[[:print:]]*html[[:print:]]*\\\">([[:print:]]*)","\\1",f)
   y5<-as.data.frame(matrix(gsub(" ","",f2),byrow=TRUE,ncol=7),stringsAsFactors=FALSE)
   names(y5)<-h2
   y5$errors<-""
   y5$errors[u3]<-bad.functions
   return(list(error.df=y5,error.log=allerrors))
   }


checkPackage<-function(lib.loc=NCBI2R:::ncbi2r.options()$internal.library.check,v=3,package="NCBI2R",recyc=TRUE)
   {
   if(v<1 | v>3)
     stop("improper check mode")
   if(v==1 | v==3)
      a<-checkPackage.v1(package=package)
   if(v==2 | v==3)
      b<-checkPackage.v2(package=package,lib.loc=lib.loc,recyc=recyc)
   }

checkPackage.v1<-function(package="NCBI2R")
   {
    tt<-getPackageVersionsOnAllMirrors(package=package)
    nm<-package.check.status(package=package,fn=NULL)
   write.table(nm[[1]],file="flavorchecks.txt",row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)
   print(tt$summary)
   print(nm[[1]])
   }

test.examples.from.file<-function(fn)
   {
   hlp<-scan(fn,what="character",sep="\n",quiet=TRUE)
   egs.1<-grep("\\\\examples\\{",hlp)
   if(length(egs.1)>0)
      {
      egs.3<-grep("\\}",hlp)
      egs.3<-egs.3[egs.3>egs.1][1]
      egs<-hlp[(egs.1+1):(egs.3-1)]
      egs<-egs[egs!="\\donttest{"]
      egs<-egs[egs!=" \\donttest{"]
      writeLines("");writeLines("");writeLines("");writeLines("");writeLines("");writeLines("");writeLines("");writeLines("")
      print(fn)
      print(egs)
      k<-0
      cont<-TRUE
      while(k<length(egs) & cont==TRUE)
         {
         k<-k+1
         a<-try(eval(parse(text=egs[k])))
         if(class(a)=="try-error")
           {
           print("FAIL")
           print(egs[k])
           cont<-FALSE
           }
         }
      if(cont)
         {
         return("OK")
         } else {
         return(egs[k])
         }
      } else {
      return("No egs")
      }
   }


get.functions.namespace<-function(fn)
  {
  ns<-scan(fn,what="character",sep="\n",quiet=TRUE)
  ns<-unlist(strsplit(ns,"\""))
  ns<-ns[ns!=", "]
  ns<-ns[ns!=",  "]
  ns<-ns[2:(length(ns)-1)]
  return(ns)
  }


sourceDir<-function(path, trace = TRUE, ...)
    {
    for (nm in list.files(path, pattern = "\\.[RrSsQq]$"))
       {
       if(trace) cat(nm,":")
       source(file.path(path, nm), ...)
       if(trace) cat("\n")
       }
   }

checkPackage.v2<-function(lib.loc=ncbi2r.options()$internal.library.check,recyc=TRUE,package="NCBI2R")
   {
   writeLines("examples with data(gwas) won't work unless it's alresady loaded. so load manually?")
    writeLines("perhaps do this at the same time as I sourceDir the files...")
    writeLines("run recyc=FALSE on first attempt in new environment. otherwise won't work.")
    writeLines("load? file name...NCBI2R/data/gwas.rda")
    writeLines("re next line: add a check here")
    writeLines("Note:lib.loc should not end in NCBI2R or /R")
    if(substr(lib.loc,nchar(lib.loc),nchar(lib.loc))!="/")
      lib.loc<-paste(lib.loc,"/",sep="")
    src.r<-paste(lib.loc,package,"/R",sep="")
    src.m<-paste(lib.loc,package,"/man",sep="")
    src.n<-paste(lib.loc,package,"/NAMESPACE",sep="")
    dummy<-sourceDir(src.r,trace=FALSE)

   if(!recyc | !exists(".ncbi2r.options"))
      {
      allhlp<-list.files(src.m,pattern="*.[Rr]d")
      my.func<-get.functions.namespace(src.n)
      b<-rep("FAIL",length(my.func))
      for(p in 1:length(my.func))
         {
         thisfile<-c(allhlp[grep(paste("^",my.func[p],".rd",sep=""),allhlp)],allhlp[grep(paste("^",my.func[p],".Rd",sep=""),allhlp)])
         b[p]<-try(test.examples.from.file(paste(src.m,thisfile,sep="/")))
         }
      f1<-as.data.frame(cbind(func=my.func,failure=b),stringsAsFactors=FALSE)
      .ncbi2r.tests<-f1[f1$failure!="OK" &f1$failure!="No egs",]
      }
   return(.ncbi2r.tests)
   }

test.GetSNPInfo.GetSNPsInGenes<-function(n=100,MaxRet=100000,delay.duration=60,delay.every=100)
   {
   writeLines("NCBI2R internal check.")
   writeLines("Will compare two independent functions to see if same values returned")
   writeLines("If nrow of the output doesn't equal zero, we have a problem")
   writeLines("This problem could be MaxRet needs improving or snps in multiple genes")
  gl.listofSNPs<-NULL
  allgenes<-c()
  while(length(allgenes)==0)
     {
    writeLines("Getting random list of SNPs")
    snplist<-randomSNP(n)
    gsi<-try(GetSNPInfo(snplist))
    if(class(gsi)=="try-error")
      {
      writeLines("ERROR:")
      gl.listofSNPs<<-snplist
      stop("ERROR. check gl.listofSNPs")
      }
    gsi$matches<-""
    allgenes<-unique(gsi$locusID[gsi$locusID!=""])
    }
  for(i in 1:length(allgenes))
     {
     cat(paste("\r",length(allgenes),"-",i))
     if(floor(i/delay.every)==i/delay.every & i!=length(allgenes))
        {
        writeLines(paste("Long delay started for",delay.duration))
        print(Sys.time())
        Sys.sleep(delay.duration)
        print(Sys.time())
        }
     gsig<-GetSNPsInGenes(allgenes[i],MaxRet=MaxRet)
     gsi$matches[gsi$marker %in% gsig]<-paste(gsi$matches[gsi$marker %in% gsig],allgenes[i],sep=",")
     }
    b<-gsi[gsi$locusID!="",]
    b$matches<-gsub("^,","",b$matches)
    gl.listofSNPs<<-gl.listofSNPs
    return(b[b$matches!=b$locusID,])
    }

check.GeneSymbolsAreAppropriate<-function(v)
   {
   writeLines("Input a list of genesymbols and this checks for lower case items which are unusual")
   writeLines("Won't include LOC12345 etc nor anything that starts with a C eg C2ORF.")
   writeLines("But perhaps that C2ORF etc option could be turned off")
   v<-unique(unlist(strsplit(v,",")))
   v<-v[!is.na(v)]
   v<-v[grep("LOC[[:digit:]]+",v,invert=TRUE)]
   lowers<-v[grep("[a-z]",v)]
   lowers.problems<-lowers[grep("^C",lowers,invert=TRUE)]
   return(lowers.problems)
  }


checkSplitGenes<-function()
   {
   snplist<-c("rs12345","rs333","rs1003483")
   mySNPs<-GetSNPInfo(snplist)
   splitSNPs<-SplitGenes(mySNPs)
   if(nrow(splitSNPs)==3)
     stop("SplitGenes is not working properly")
   return(splitSNPs)
   }

testing.function<-function(f="GetSNPInfo",style="snp",randoms=503,iterations=100,delay.duration=120,delay.every=10)
   {
   writeLines("This should work on any function - gene or snp - perhaps the other functions as well")
   writeLines("Could suppress the output and just keep a tally of what works, what does not")
   writeLines("the delay is only on the number of iterations - not within the function itself")
   bad<-0;   good<-0;   i<-1
   gl.lastgood.snplist<-NULL
   gl.lastbad.snplist<-NULL
   gl.snplist<-NULL
   keepoutput<-TRUE
   testlist<-randomExamples(randoms,style=style)
   a<-paste(f,"(testlist)",sep="")
   output<-try(eval(parse(text=a)))
   if(class(output)=="try-error")
      {
      writeLines("could not get the first one to work")
      gl.snplist<<-testlist

      print(testlist)
      writeLines("the list of snps saved globally as gl.snplist")
      stop("")
      } else {

       writeLines("OK. First part worked. snps stored as gl.lastgood.snplist")
       good<-good+1

       gl.lastgood.snplist<-testlist

       while(i<iterations)
            {

            writeLines(as.character(paste("\r",iterations,"-",i,"G:",good,"B:",bad)))
           if(floor(i/delay.every)==i/delay.every & i!=iterations)
              {
              writeLines(paste("Long delay started for",delay.duration))
              print(Sys.time())
              Sys.sleep(delay.duration)
              print(Sys.time())
              }
            testlist<-randomExamples(randoms,style=style)
            a<-paste(f,"(testlist)",sep="")
             thisoutput<-try(eval(parse(text=a)))
             if(class(thisoutput)=="try-error")
               {
               writeLines("failed")
               bad<-bad+1
               gl.lastbad.snplist<-testlist
               writeLines(as.character(testlist))
               } else {
               writeLines("OK")
               gl.lastgood.snplist<-testlist


               if(class(thisoutput)=="data.frame")
                  output<-as.data.frame(rbind(output,thisoutput),stringsAsFactors=FALSE)
               if(class(thisoutput)=="list")
                  {
                  for(j in 1:length(thisoutput))
                     {
                     if(class(thisoutput[[j]])=="data.frame")
                         {
                         output[[j]]<-as.data.frame(rbind(output[[j]],thisoutput[[j]]),stringsAsFactors=FALSE)
                         }
                     if(class(thisoutput[[j]])=="character")
                         {
                         output[[j]]<-c(output[[j]],thisoutput[[j]])
                         }
                     }
                  }
               good<-good+1
               }
             i<-i+1
            }
      }

   print(paste("G/B",good,"/",bad))
   j<-list(d=output,g=good,b=bad)

   gl.lastbad.snplist<<-gl.lastbad.snplist
   gl.lastgood.snplist<<-gl.lastgood.snplist
   return(j)
   }
