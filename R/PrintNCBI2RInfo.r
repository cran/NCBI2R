PrintNCBI2RInfo<-function()
   {
   print("________________________________________________________________________________________________________________________")
   print("NCBI2R package loaded. Register for updates, view the PDF manual and tutorials at ")
   temp<-URLdefinitions()
   print(temp$package.web)
   print("Please read the usage guidelines and recommendations on the website")
   print("All functions written by Scott Melville with overall assistance by Christian Fuchsberger, Cristian Pattaro & Yuri D'Elia")
   print("________________________________________________________________________________________________________________________")
   }