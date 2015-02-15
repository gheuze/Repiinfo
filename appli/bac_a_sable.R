setwd("I:\\DIRECTION ACTION TERRITORIALE DE SANTE\\VSSE\\CIRE\\service\\formation\\R\\exemples scripts R\\Repiinfo\\appli")

setwd("/home/guillaume/R/Repiinfo/appli")

essai <- read.table("Etablissements.csv",header=T,sep=",")
essai
rm(list=ls())
class(colnames(essai),data=essai)
class(col(essai))


classeCol <- function(df) {
    classe <- vector()
    for (i in 1:ncol(df)) 
    classe[i] <- class(df[,i])
    return(classe)
}
  
  classeCol(essai)



regexpr("[[:digit:]]+[/-][[:digit:]]+[/-][[:digit:]]+","02/02-2015")


length(grep("[[:digit:]]+[/-][[:digit:]]+[/-][[:digit:]]+",essai[,52]))==length(essai[,52])

[[:digit:]]+[/-][[:digit:]]+[/-][[:digit:]]+
  
  
  
  
  
  
  vect_select <- vector()
for (i in 1:ncol(essai)) {
  vect_select[i] <- length(grep("[[:digit:]]+[/-][[:digit:]]+[/-][[:digit:]]+",essai[,i]))==length(essai[,i])
}
# indice des colonnes contenant des dates
ind_date <- which(colnames(essai) %in% names(essai[vect_select]))

for (i in ind_date) {
  essai[,i] <- as.Date(essai[,i],format=checkDateFormat(essai[,i]))
}


