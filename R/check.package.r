
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
   t2<-as.data.frame(rbind(table(mdf[,2])))
   t3<-as.data.frame(rbind(table(mdf[,3])))
   t4<-as.data.frame(rbind(table(mdf[,4])))
   t234<-as.data.frame(rbind(t2,t3,t4),stringsAsFactors=FALSE)
   row.names(t234)<-c("Src","Mac","Win")
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


checkPackage<-function(lib.loc=ncbi2r.options()$internal.library.check,v=3,package="NCBI2R",recyc=TRUE)
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

