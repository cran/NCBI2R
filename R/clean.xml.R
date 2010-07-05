clean.xml <-
function(webget)
   {
   webget<-gsub("&lt;","<",webget)
   webget<-gsub("&gt;",">",webget) 
   webget<-gsub("                <","<",webget)
   webget<-gsub("                <","<",webget)
   webget<-gsub("              <","<",webget)
   webget<-gsub("            <","<",webget)
   webget<-gsub("          <","<",webget)
   webget<-gsub("        <","<",webget)  
   webget<-gsub("      <","<",webget)    
   webget<-gsub("    <","<",webget)
   webget<-gsub("  <","<",webget)
   webget<-gsub(" <","<",webget)
   return(webget)
   } 

