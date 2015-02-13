#######################################
### 
###     Repiinfo
###
### server.r 
### Institut de Veille Sanitaire 
### Cire sud 
### v0.0      du 11/02/2015
########################################




shinyServer(function(input, output,session) {   ## faut-il session, a la suite de output, ? et, si oui, a quoi ca sert
  observe({
    
    # import du fichier de donnees
  
  
      output$var_class <- renderTable({
        fichier <- input$fichier_brut
        donnees <- read.csv2(fichier$datapath)
        
        
        
        var_class <- data.frame(names(donnees),classeCol(donnees)) # création du df avec les noms des variables et les classes
        names(var_class) <- c("noms des variables","classes des variables")
        return(var_class)
      })
      
    
  })

 
  
})
