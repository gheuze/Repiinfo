########################################################################
### source.r ###
### Institut de Veille Sanitaire ###
### Cire Languedoc-Roussillon ###
### Navarre Julien ###
### 01/08/2013 ###
### Version 0. ###
########################################################################

library(shiny)
suppressPackageStartupMessages(library(googleVis))
library(epitools)
library(maptools)
library(rgdal)

# Table de correspondance des r?gions, d?partements et leurs codes.
table <- read.csv(".\\Bases\\table.csv", header = TRUE, sep = ";")
table[, which(names(table) == "region")] <- iconv(table[, which(names(table) == "region")], from = "LATIN2", to = "ASCII//TRANSLIT")


# Vecteur contenant les r?gions qui ont des d?partements en surveillance
# Construit par les r?gions qui ont au moins un departement en surveillance.
regions <- as.character(unique(table$region[which(!is.na(table$Niveau.de.surveillance))]))
# On encode les string au bon format (conservation des accents)
##regions <- iconv(regions, from = "LATIN2", to = "UTF-8")

# Liste des d?partements en fonction de la r?gion
dep.list <- list()
length(dep.list) <- length(regions)
# Chaque ?lement de la liste est une r?gion du vecteur "region"
names(dep.list) <- regions
# Chaque ?lement de la liste est un vecteur des departement en surveillance de la r?gion en titre de cet ?lement
for (i in 1:length(dep.list)){
	dep.list[[i]] <- as.character(table$departement[which((table$region == names(dep.list)[i]) & (!is.na(table$Niveau.de.surveillance)))])
}

# Initialisation du vecteur dep.list.tmp
dep.list.tmp <- c()

# Fonction pour verifier en quel format sont les dates du csv
checkDateFormat <- function(dateD){
	if(sum(is.na(as.Date(dateD, format = "%Y-%m-%d"))) == length(dateD)){
		format.date <- "%d/%m/%Y"
	}else {
		format.date <- "%Y-%m-%d"
	}
	return(format.date)
}

# Fond de carte de la France
france <- readOGR('./Cartes', 'DEPARTEMENT')

# On lance l'application shiny
runApp(".\\app")

