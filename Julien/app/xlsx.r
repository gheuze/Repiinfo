########################################################################
### xlsx.r ###
### Institut de Veille Sanitaire ###
### Cire Languedoc-Roussillon ###
### Navarre Julien ###
### 01/08/2013 ###
### Version 0. ###
########################################################################

########################################################################################################################
################################ FICHIER EXECUTE LORS DE L'APPUI SUR LE BOUTON DOWNLOAD ################################
########################################################################################################################
library(xlsx)

# Titre du tableau
tab.title <- paste(do.call("paste", as.list(lab.reg)), "-", paste(lab.date.deb, "-", lab.date.fin, ".xlsx", sep = ""))
# Chemin vers le tableau
tab.path.tmp <- unlist(strsplit(getwd(), split = "/"))
tab.path.tmp[length(tab.path.tmp)] = "Tableaux"
tab.path <- paste(paste(tab.path.tmp, collapse = "/"), "/", tab.title, sep = "")

if (last.days.bool == FALSE){
	tab <- df.to.write
} else {
	tab <- merge(df.to.write, df.to.append, by = "Departements", sort = FALSE)
}
# Le nom des colonnes principales (sans la première colonne)
col.names <- c("Départements","Cas suspects signalés",
				"Cas Confirmés Importés","Cas Confirmés Autochtones",
				"En attente de résultats biologiques", "Investigations Entomologiques")
# Le nombre de sous colonnes que chaque colonne principale contient
col.width <- c(1, 1, 2, 2, 1, 3)
# Nombre de colonnes au total (sommes de toutes les sous colonnes + la première colonne)
col.length <- sum(col.width) + 1
# Les sous titres des colonnes
sub.col.names <- c("", "", "Dengue", "Chik", "Dengue", "Chik", "", "Information de L'EID", "Prospection", "Traitement LAV")

# La couleur des colonnes (premier paramètre la première colonne et ensuite les colonnes principales, dont les sous colonnes auront la même couleur)
cols.colors <- c("pink", "TURQUOISE", "lavender", "lightblue", "red", "yellow", "purple")

# Si on a activé l'option "cas des 7 derniers jours"
if (last.days.bool == TRUE){
	col.names <- c(col.names, hard.lab.last.days)
	col.width <- c(col.width, 1)
	col.length <- col.length + 1
	sub.col.names <- c(sub.col.names, "")
	cols.colors <- c(cols.colors, "lightgreen")
}
#

lab.reg.tmp <- c()
for (i in 1:length(lab.reg)){
	if(sum(dep.list[[lab.reg[i]]] %in% lab.dep) >= 1){
		lab.reg.tmp <- c(lab.reg.tmp, lab.reg[i])
	}
}
lab.reg <- lab.reg.tmp


tab <- as.data.frame(rbind(sub.col.names, tab))


write.xlsx(tab, tab.path,
			sheetName = "Tableau", col.names = TRUE, row.names = TRUE)
			
wb <- loadWorkbook(file = tab.path)	#Chargement du classeur Excel
sheets <- getSheets(wb) # Obtention du chemin de chaque feuille du classeur
rows <- getRows(sheets[[1]]) # Stock le chemin de toutes les lignes de la 1ère feuille
cells <- getCells(rows)	# Stock le chemin de toutes les cellules

# On commence le compteur à 1, car on ne s'interesse pas à la première colonne.
# Fusion des lignes ou des colonnes.
cpt <- 1
for (i in 1:length(col.width)){
	cpt <- cpt + col.width[i]
	if (col.width[i] == 1){
		addMergedRegion(sheets[[1]], 1, 2, cpt, cpt)
	} else {
		addMergedRegion(sheets[[1]], 1, 1, cpt - col.width[i] + 1, cpt)
	}
}

# Titres des colonnes
vec <- 1:length(col.width)
for (i in 1:length(vec)){
	vec[i] <- sum(col.width[1:i])
}
vec[which(col.width > 1)] = vec[which(col.width > 1)] - (col.width[which(col.width > 1)] - 1)
for (i in 1:length(vec)){
	setCellValue(cells[[vec[i] + 1]], col.names[i])
}

# Réglages de la première colonne
setCellValue(cells[['1.1']], "Régions")
setCellValue(cells[['2.1']], "")
setCellValue(cells[[paste(nrow(tab) + 1, ".1", sep = "")]], "Total")
addMergedRegion(sheets[[1]], 1, 2, 1, 1)

start <- 3
tic.stop <- c()
for (i in 1:length(lab.reg)){
	somme <- sum(lab.dep %in% dep.list[[lab.reg[i]]])
	stop <- start - 1 + somme
	tic.stop <- c(tic.stop, stop)
	addMergedRegion(sheets[[1]], start, stop, 1, 1)
	setCellValue(cells[[paste(start, ".1", sep = "")]], lab.reg[i])
	start <- start + somme
}

# Ajustement automatique de la largeur des colonnes
autoSizeColumn(sheets[[1]], 1:(ncol(tab) + 1))

saveWorkbook(wb, file = tab.path)


# Colorier et centrer les cellules
wb <- loadWorkbook( file = tab.path) # Où est le fichier excel

setColumnStyle <- function(wb, sheet = 1, column.to.set, nb.row, col.color){

	sheets <- getSheets(wb)
	rows <- getRows(sheets[[sheet]])
	cells <- getCells(rows)
	
	for (i in 1:nb.row){
		if (i == nb.row || i == 2 || i == 1){
			position <- c("BOTTOM", "TOP", "LEFT", "RIGHT")
		} else if(i %in% tic.stop) {
			position <- c("BOTTOM", "LEFT", "RIGHT")
		} else {
			position <- c("LEFT", "RIGHT")
		}
		cellStyle <- CellStyle(wb) + Alignment(h = "ALIGN_CENTER", v = "VERTICAL_CENTER") +
						Fill(backgroundColor = col.color, foregroundColor = col.color, pattern = "BRICKS") +
						Border(pen = "BORDER_THIN", position = position)
						
		  setCellStyle(cells[[paste(i, ".", column.to.set, sep = "")]], cellStyle)
	}
}

setSheetStyle <- function(wb, sheet = 1, nb.col, cols.colors = cols.colors, col.width = col.width){
	
	setColumnStyle(wb = wb, column.to.set = 1, nb.row = nrow(tab) + 1, col.color = cols.colors[1])

	colors <- c()
	for (i in 2:length(cols.colors)){
		colors <- c(colors, rep(cols.colors[i], col.width[i - 1]))
	}
	for (i in 1:sum(col.width)){
		setColumnStyle(wb = wb, column.to.set = i + 1, nb.row = nrow(tab) + 1, col.color = colors[i])
	}
}

setSheetStyle(wb = wb, sheet = 1, nb.col = col.length, cols.colors = cols.colors, col.width = col.width)

saveWorkbook(wb, file = tab.path)

print("FINISH")
browseURL(tab.path)