#######################################
### 
###     Repiinfo
###
### ui.r 
### Institut de Veille Sanitaire 
### Cire sud 
### v0.0      du 11/02/2015
########################################







shinyUI(fluidPage(

  titlePanel("Repiinfo"),
  
  sidebarLayout(position = "right",
    sidebarPanel(
      # chargement
      fileInput("fichier_brut", "Importez le fichier .csv",
                accept = c('text/csv', 'text/comma-separated-values,text/plain'))
      
      # présentation de l'import
      
      
    ),
#   
    mainPanel(tableOutput("var_class"))
  )

  
  
))
