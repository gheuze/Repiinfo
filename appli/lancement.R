#######################################
### 
###     Repiinfo
###
### Repiinfo.r 
### Institut de veille sanitaire 
### Cire sud 
### v0.0      du 11/02/2015
########################################



# setwd("I:\\DIRECTION ACTION TERRITORIALE DE SANTE\\VSSE\\CIRE\\service\\formation\\R\\exemples scripts R\\Repiinfo\\appli")
setwd("/home/guillaume/R/Repiinfo/appli")

library(shiny)

# Fonction pour verifier en quel format sont les dates du csv  ### merci Julien Navarre
checkDateFormat <- function(dateD){
  if(sum(is.na(as.Date(dateD, format = "%Y-%m-%d"))) == length(dateD)){
    format.date <- "%d/%m/%Y"
  } else {
    format.date <- "%Y-%m-%d"
  }
  return(format.date)
}


# fonction pour recuperer les classes des colonnes d'un dataframe
classeCol <- function(df) {
  classe <- vector()
  for (i in 1:ncol(df)) 
    classe[i] <- class(df[,i])
  return(classe)
}



runApp(".")
