
getSNPInfo.cleaning<-function(dat)
  {
  dat$genesymbol<-clean.NAs(dat$genesymbol)
  dat$locusID<-clean.NAs(dat$locusID)
  dat$chr<-clean.NAs(dat$chr)
  dat$chrpos<-clean.NAs(dat$chrpos)
  dat$fxn_class<-clean.NAs(dat$fxn_class)
  dat$species<-clean.NAs(dat$species)
  dat$dupl_loc<-clean.NAs(dat$dupl_loc)
  return(dat)
  }
