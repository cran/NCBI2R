PrintNCBI2RInfo<-function()
   {
   msg1<-"NCBI2R package loaded." 
   msg2<-"Developed by Scott Melville, Christian Fuchsberger," 
   msg3<-"Cristian Pattaro and Yuri D'Elia"
   msg4<-"Please check the web for updates, tutorials, manuals etc"
   temp<-URLdefinitions()
   msg5<-temp$package.web
   msg6<-"Please read the usage guidelines on the website."
   writeLines(c(msg1,msg2,msg3,msg4,msg5,msg6))
   writeLines("now checking if you have the latest version")
   UpdateCheck()
   }