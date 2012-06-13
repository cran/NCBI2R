check.batchsize<-function(batchsize)
   {
   if(class(batchsize)=="character")
      {
      writeLines("NCBI2R error. batchsize argument is not numeric.")
      writeLines("You may have entered a SNP name where the batchsize should be.")
      stop("Could you have forgotten to place the letter c before your list of SNPs?")
      }
   }
