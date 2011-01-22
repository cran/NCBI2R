ncbi2r.options<-function()
  {
  if(!exists(".ncbi2r.options"))
    .ncbi2r.options<<-ncbi2r.options.default()
  return(.ncbi2r.options)
  }