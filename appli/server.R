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
        
        # Si aucun fichier n'a encore ete selectionne
        if (is.null(fichier)){
                return("Bienvenue sur Repiinfo. \n Commencez par choisir un fichier .csv")
        } else if (substring(fichier$name, regexpr("\\.([[:alnum:]]+)$", fichier$name)) != ".csv"){ # Si le fichier n'est pas un .csv
                return("Le format du fichier est incorrect. Veuillez inserer un .csv svp.")
        } else { 
                return("Traitement du fichier")
        }
        
        # recherche des variables qui sont des dates
        vect_select <- vector()
        for (i in 1:ncol(donnees)) {
          vect_select[i] <- length(grep("[[:digit:]]+[/-][[:digit:]]+[/-][[:digit:]]+",
                                        donnees[,i]))==length(donnees[,i])
        }
        
        # indice des colonnes contenant des dates
        ind_date <- which(colnames(donnees) %in% names(donnees[vect_select]))
        
        for (i in ind_date) {
                donnees[,i] <- substr(donnees[,i],1,10) # pour enlever les heures si besoin
                donnees[,i] <- as.Date(donnees[,i],format=checkDateFormat(donnees[,i]))
        }
        
        # creation du df de sortie
        var_class <- data.frame(names(donnees),classeCol(donnees)) # creation du df avec les noms des variables et les classes
        names(var_class) <- c("noms des variables","classes des variables")
        return(var_class)
      })
      
    
  })

 
  
})
