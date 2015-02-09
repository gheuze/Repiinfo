########################################################################
### server.r ###
### Institut de Veille Sanitaire ###
### Cire Languedoc-Roussillon ###
### Navarre Julien ###
### 01/08/2013 ###
### Version 0. ###
########################################################################

# Les assignations dans l'environnement global "<<-" permettent de r?utiliser les r?sultats produits _
# dans Shiny pour les sorties en dehors de l'IHM

# Define server logic
shinyServer(function(input, output, session) {
	
	#################################################
	### Updating du checkboxGroup selon la R?gion ###
	#################################################
	
	observe({
		inpt.reg <- input$reg
		# Si aucune r?gion selectionn? on ne retourne rien
		if (length(inpt.reg) <= 0){
			return()
		}
		# On exporte le vecteur des r?gions choisies dans l'env global
		lab.reg <<- inpt.reg
		# Pour chaque r?gion coch?e on va chercher dans la liste dep.list l'?lement qui porte le nom de la r?gion i
		# (chaque element est un vecteur de departements de la r?gion i)
		# On cr?e un vecteur avec tous les departements (en surveillance) de toutes les r?gions choisies
		for (i in 1:length(inpt.reg)){
			dep.list.tmp <- c(dep.list.tmp, dep.list[[sprintf("%s", inpt.reg[i])]])
		}
		# On met ? jour le checkbox group avec comme choix dep.list.tmp
		updateCheckboxGroupInput(session, "dep", choices = dep.list.tmp)
		# Si l'option "tous les d?partements est coch?e on set TRUE toutes les cases
		if (input$dep.all == TRUE){ updateCheckboxGroupInput(session, "dep", choices = dep.list.tmp, selected = dep.list.tmp)}
	})
	
	###################################
	### Titre de la page principale ###
	###################################
	
	output$title.gen <- renderText({
		inFile <- input$file1
		inpt.reg <- input$reg
		
		# Si aucun fichier n'a encore ?t? selectionn?
		if (is.null(inFile)){
			return("Bienvenue sur la page de reporting VOOZARBO. \n Commencez par choisir un fichier .csv")
		} else if (substring(inFile$name, regexpr("\\.([[:alnum:]]+)$", inFile$name)) != ".csv"){ # Si le fichier n'est pas un .csv
			return("Le format du fichier est incorrect. Veuillez inserer un .csv svp.")
		} else if (length(inpt.reg) <= 0){ # Si aucune r?gion n'a ?t? selectionn?e
			return("Aucune r?gion selectionn?e. Veuillez selectionner au moins une r?gion.")
		} else { # Sinon
			return(paste("Retro-Information du", format(input$date.range[1], "%d/%m/%Y"), "au", format(input$date.range[2], "%d/%m/%Y")))
		}
	})
	
	###########################################################
	#####												  #####
	##### Output : Tableau de r?tro-information principal #####
	#####												  #####
	###########################################################
	
	output$contents <- renderTable({
	
		# input$file1 le fichier upload?
		inFile <- input$file1
		inpt.reg <- input$reg
		# si il n'y a pas de fichier upload? on ne renvoit rien etc....
		if (is.null(inFile)){
			return(NULL)
		} else if (substring(inFile$name, regexpr("\\.([[:alnum:]]+)$", inFile$name)) != ".csv"){
			return(NULL)
		} else if (length(inpt.reg) <= 0){
			return(NULL)
		}
		
		# On encode les d?partements selectionn?s en UTF 8
		inpt.dep <- iconv(input$dep, from = "UTF-8", to = "LATIN2")
		# On lit le fichier .csv selectionn?
		data <- read.csv(inFile$datapath, header = TRUE, sep = ";")
		
		# Initialisation de vecteurs
		cas.suspects <- 1:length(inpt.dep)
		cas.conf.imp.deng <- 1:length(inpt.dep)
		cas.conf.imp.chik <- 1:length(inpt.dep)
		cas.conf.auto.deng <- 1:length(inpt.dep)
		cas.conf.auto.chik <- 1:length(inpt.dep)
		att.results <- 1:length(inpt.dep)
		info.eid <- 1:length(inpt.dep)
		prospection <- 1:length(inpt.dep)
		trait.lav <- 1:length(inpt.dep)
		
		date.fin <- input$date.range[2]
		date.deb <- input$date.range[1]
		
		# Le format de date ? utiliser "%Y-%m-%d" ou "%d/%m/%Y"
		format.date <- checkDateFormat(data$sign_dat)
		
		# SOUS REQUETAGE
		for (i in 1:length(inpt.dep)){
		#Cas suspect : nombre de lignes du sous data frame
			cas.suspects[i] <- nrow(subset(data, sign_dep == as.character(table$code[which(table$departement == inpt.dep[i])])
									& ((as.Date(sign_dat, format = format.date)) <= input$date.range[2])
									& ((as.Date(sign_dat, format = format.date)) >= input$date.range[1])))
									
		# Cas confirm?s import?s dengue : cd_conc = 2 (dengue) & cd_typcas = 1 (import?)						
			cas.conf.imp.deng[i] <- nrow(subset(data, sign_dep == as.character(table$code[which(table$departement == inpt.dep[i])])
									& ((as.Date(sign_dat, format = format.date)) <= input$date.range[2])
									& ((as.Date(sign_dat, format = format.date)) >= input$date.range[1])
									& (cd_conc == "2") & (cd_typcas == "1")))
									
		# Cas confirm?s import?s chik : cd_conc = 1 (chik) & cd_typcas = 1 (import?)								
			cas.conf.imp.chik[i] <- nrow(subset(data, sign_dep == as.character(table$code[which(table$departement == inpt.dep[i])])
									& ((as.Date(sign_dat, format = format.date)) <= input$date.range[2])
									& ((as.Date(sign_dat, format = format.date)) >= input$date.range[1])
									& (cd_conc == "1") & (cd_typcas == "1")))
		
		# Cas confirm?s autochtones dengue : cd_conc = 2 (dengue) & cd_typcas = 2 (autochtone)	
			cas.conf.auto.deng[i] <- nrow(subset(data, sign_dep == as.character(table$code[which(table$departement == inpt.dep[i])])
									& ((as.Date(sign_dat, format = format.date)) <= input$date.range[2])
									& ((as.Date(sign_dat, format = format.date)) >= input$date.range[1])
									& (cd_conc == "2") & (cd_typcas == "2")))
		
		# Cas confirm?s autochtones chik : cd_conc = 1 (chik) & cd_typcas = 2 (autochtone)	
			cas.conf.auto.chik[i] <- nrow(subset(data, sign_dep == as.character(table$code[which(table$departement == inpt.dep[i])])
									& ((as.Date(sign_dat, format = format.date)) <= input$date.range[2])
									& ((as.Date(sign_dat, format = format.date)) >= input$date.range[1])
									& (cd_conc == "1") & (cd_typcas == "2")))
		
		# En attente de resultats biologiques : cd_conc = 7 (en attente)
			att.results[i] <- nrow(subset(data, sign_dep == as.character(table$code[which(table$departement == inpt.dep[i])])
									& ((as.Date(sign_dat, format = format.date)) <= input$date.range[2])
									& ((as.Date(sign_dat, format = format.date)) >= input$date.range[1])
									& cd_conc == "7"))
		
		# Information EID : eid_inf = 1 (oui)
			info.eid[i] <- nrow(subset(data, sign_dep == as.character(table$code[which(table$departement == inpt.dep[i])])
									& ((as.Date(sign_dat, format = format.date)) <= input$date.range[2])
									& ((as.Date(sign_dat, format = format.date)) >= input$date.range[1])
									& eid_inf == "1"))
		
		# Prospection : eid_prosp = 1 (oui)
			prospection[i] <- nrow(subset(data, sign_dep == as.character(table$code[which(table$departement == inpt.dep[i])])
									& ((as.Date(sign_dat, format = format.date)) <= input$date.range[2])
									& ((as.Date(sign_dat, format = format.date)) >= input$date.range[1])
									& data$eid_prosp == "1"))
									
		# Traitement LAV : eid_ttadu = 1 (oui) OU eid_ttlar = 1 (oui)
			trait.lav[i] <- nrow(subset(data, sign_dep == as.character(table$code[which(table$departement == inpt.dep[i])])
									& ((as.Date(sign_dat, format = format.date)) <= input$date.range[2])
									& ((as.Date(sign_dat, format = format.date)) >= input$date.range[1])
									& ((data$eid_ttadu == "1") | (data$eid_ttlar == "1"))))
		}
		# DATA FRAME A RENVOYER
		# On ajoute comme dernier element aux vecteurs ci dessus la somme des elements de chaque vecteur, ce qui represente le total
		df <- data.frame("Departements" = c(inpt.dep, "Total"),
					"Cas Suspects Signales" = c(cas.suspects, sum(cas.suspects)),
					"Cas Confirmes Importes Dengue" = c(cas.conf.imp.deng, sum(cas.conf.imp.deng)),
					"Cas Confirmes Importes Chik" = c(cas.conf.imp.chik, sum(cas.conf.imp.chik)),
					"Cas Confirmes Autochtones Dengue" = c(cas.conf.auto.deng, sum(cas.conf.auto.deng)),
					"Cas Confirmes Autochones Chik" = c(cas.conf.auto.chik, sum(cas.conf.auto.chik)),
					"En Attente de Resultats Bio." = c(att.results, sum(att.results)),
					"Information EID" = c(info.eid, sum(info.eid)),
					"Prospection" = c(prospection, sum(prospection)),
					"Traitement LAV" = c(trait.lav, sum(trait.lav))
		)
		# On enregistre le data frame dans l'environement g?n?ral pour pouvoir le r?utiliser en dehors de l'IHM
		# /!\ Mais avec un autre nom pour ne pas bloquer la r?activit? /!\
		df.to.write <<- df
		df.to.write.bis <<- df
		
		lab.date.fin <<- format(input$date.range[2], "%d-%m-%Y")
		lab.date.deb <<- format(input$date.range[1], "%d-%m-%Y")
		lab.dep <<- inpt.dep
		
		# On renvoie le df dans l'output
		return(df)
	})
	
	#######################################
	### Titre du sous tableau de retroI ###
	#######################################
	
	output$title.sub.table <- renderText({
		inFile <- input$file1
		inpt.reg <- input$reg
		if (is.null(inFile)){
			return(NULL)
		} else if (input$last.days != TRUE){
			return(NULL)
		} else if (substring(inFile$name, regexpr("\\.([[:alnum:]]+)$", inFile$name)) != ".csv"){
			return(NULL)
		} else if (length(inpt.reg) <= 0){
			return(NULL)
		} else {
			date.fin <- input$date.range[2]
			date.ant <- date.fin - 7
			lab.last.days <- paste("Cas Suspects Signales du", format(date.ant, "%d/%m/%Y"), "au", format(date.fin, "%d/%m/%Y"))
			hard.lab.last.days <<- lab.last.days
			return(lab.last.days)
		}
	})
	
	##################################################################
	#####												  		 #####
	##### Output : Tableau de r?tro-information 7 derniers jours #####
	#####												  		 #####
	##################################################################

	
	output$tab.last.days <- renderTable({
		inFile <- input$file1
		inpt.reg <- input$reg
		
		last.days.bool <<- input$last.days
		
		if (is.null(inFile)){
			return(NULL)
		} else if (input$last.days != TRUE){
			df.to.write <<- df.to.write.bis
			return(NULL)
		} else if (substring(inFile$name, regexpr("\\.([[:alnum:]]+)$", inFile$name)) != ".csv"){
			return(NULL)
		} else if (length(inpt.reg) <= 0){
			return(NULL)
		} else {
			inpt.dep <- iconv(input$dep, from = "UTF-8", to = "LATIN2")
			data <- read.csv(inFile$datapath, header = TRUE, sep = ";")
			
			date.fin <- input$date.range[2]
			date.ant <- date.fin - 7
			
			format.date <- checkDateFormat(data$sign_dat)
			
			cas.suspects.der.jours <- 1:length(inpt.dep)
			
			for (i in 1:length(inpt.dep)){
				cas.suspects.der.jours[i] <- nrow(subset(data, sign_dep == as.character(table$code[which(table$departement == inpt.dep[i])])
											& ((as.Date(sign_dat, format = format.date)) <= date.fin)
											& ((as.Date(sign_dat, format = format.date)) >= date.ant)))
			}
			
			dff <- data.frame("Departements" = c(inpt.dep, "Total"),
						"Cas 7 derniers jours" = c(cas.suspects.der.jours, sum(cas.suspects.der.jours))
			)
			
			df.to.append <<- dff
			return(dff)
		}
	})
	
	#######################################
	### Bouton d'exportation vers excel ###
	#######################################

	observe({
		inFile <- input$file1

		# Don't do anything if the button has never been pressed
		if (input$download == 0){
			return()
		} else if (substring(inFile$name, regexpr("\\.([[:alnum:]]+)$", inFile$name)) != ".csv"){
			return(NULL)
		}
		# Pb avec le if (length(inpt.reg) <= 0) return null
		# On observe sinon la liste des regions
		# Apr?s un premier export XLSX(inpt$dwnld > 0) l'isolate se lance tout seul ? chaque changement de r?gions
		# L'isolate r?pond avant l'update du chckbxgrp, les depts ne sont pas update avant le lancement d'xl, BUG.

		isolate({
			# Your arbitrary R code goes here
			source(".\\xlsx.r")
		})
	})
	
	#############################
	### Titre de l'onglet map ###
	#############################
	
	output$title.gvis.map <- renderText({
		inFile <- input$file1
		
		if (is.null(inFile)){
			return("Bienvenue sur la page de reporting VOOZARBO. \n Commencez par choisir un fichier .csv")
		} else if (substring(inFile$name, regexpr("\\.([[:alnum:]]+)$", inFile$name)) != ".csv"){
			return("Le format du fichier est incorrect. Veuillez inserer un .csv svp.")
		} else {
			return(paste("R?partitions des cas suspects du", format(input$date.range[1], "%d/%m/%Y"), "au", format(input$date.range[2], "%d/%m/%Y")))
		}
	})
	
	##################################################
	#####										 #####
	##### Output : Carte de repartition des cas  #####
	#####										 #####
	##################################################
	
	output$gvis <- renderGvis({
	
		inFile <- input$file1

		if (is.null(inFile)){
			return(NULL)
		} else if (substring(inFile$name, regexpr("\\.([[:alnum:]]+)$", inFile$name)) != ".csv"){
			return(NULL)
		} else {
			data <- read.csv(inFile$datapath, header = TRUE, sep = ";")
			format.date <- checkDateFormat(data$sign_dat)
			data <- subset(data, ((as.Date(sign_dat, format = format.date)) <= input$date.range[2])
									& ((as.Date(sign_dat, format = format.date)) >= input$date.range[1]))
			effectifs <- table(data$sign_region)
			## myData <- data.frame("region" = unique(table$region[which(table$abreviation %in% names(effectifs))]), cas = effectifs[], row.names = NULL)
			
			myData <- data.frame("region" = unique(table$region[which(table$abreviation %in% names(effectifs))]), cas = effectifs[],
			                     
			                     "iso" = unique(table$ISO_3166.2[which(table$abreviation %in% names(effectifs))]), row.names = NULL)
			
      
      
			##gvisGeoChart(myData,
				##locationvar = "region", colorvar = "cas",
				##options = list(region = "FR", displayMode = "regions",
				##resolution = "provinces",
				##width = 500, height = 400,
				##colorAxis = "{colors:['#FFFFFF', '#0000FF']}"

			gvisGeoChart(myData,
        locationvar = "iso", colorvar = "cas", hovervar = "region",
				options = list(region = "FR", displayMode = "regions",
				resolution = "provinces",
				width = 500, height = 400,
				colorAxis = "{colors:['#FFFFFF', '#0000FF']}"
				               
                      
                       
			))
		}
	})
	
	###################################
	### Titre de l'onglet graphique ###
	###################################
	
		
	output$title.curve <- renderText({
		inFile <- input$file1
		inpt.reg <- input$reg
		if (is.null(inFile)){
			return("Bienvenue sur la page de reporting VOOZARBO. \n Commencez par choisir un fichier .csv")
		} else if (substring(inFile$name, regexpr("\\.([[:alnum:]]+)$", inFile$name)) != ".csv"){
			return("Le format du fichier est incorrect. Veuillez inserer un .csv svp.")
		} else if (length(inpt.reg) <= 0){
			return("Aucune r?gion selectionn?e. Veuillez selectionner au moins une r?gion.")
		} else {
			return(paste("R?partition des cas suspects signal?s en fonction de la semaine et selon le departement du", format(input$date.range[1], "%d/%m/%Y"), "au", format(input$date.range[2], "%d/%m/%Y")))
		}
	})
	
	
	############################################
	#####								   #####
	##### Output : Courbe Epid?miologique  #####
	#####								   #####
	############################################
	
	output$epi.curve <- renderPlot({
	
		output$alert.curve <- renderText({
			return(NULL)
		})
	
		inFile <- input$file1
		inpt.reg <- input$reg
		if (is.null(inFile)){
			return(NULL)
		} else if (substring(inFile$name, regexpr("\\.([[:alnum:]]+)$", inFile$name)) != ".csv"){
			return(NULL)
		} else if (length(inpt.reg) <= 0){
			return(NULL)
		}

		inpt.dep <- iconv(input$dep, from = "UTF-8", to = "LATIN2")
		data <- read.csv(inFile$datapath, header = TRUE, sep = ";")
		format.date <- checkDateFormat(data$sign_dat)

		x <- data$sign_dat[which((as.Date(data$sign_dat, format = format.date) >= input$date.range[1])
							& (as.Date(data$sign_dat, format = format.date) <= input$date.range[2])
							& data$sign_dep %in% as.character(table$code[which(table$departement %in% inpt.dep)]))]
		x <- as.Date(x, format = format.date)
		xs <- data$sign_dep[which((as.Date(data$sign_dat, format = format.date) >= input$date.range[1])
							& (as.Date(data$sign_dat, format = format.date) <= input$date.range[2])
							& data$sign_dep %in% as.character(table$code[which(table$departement %in% inpt.dep)]))]
		
		if(length(data$sign_dat[which((as.Date(data$sign_dat, format = format.date) >= input$date.range[1])
							& (as.Date(data$sign_dat, format = format.date) <= input$date.range[2]))]) <= 0) {
			output$alert.curve <- renderText({
				return("Aucunes donn?es ? afficher pour cette p?riode")
			})
			return(NULL)
		} else if (length(xs) <= 0) {
			output$alert.curve <- renderText({
				return("Aucunes donn?es ? afficher pour ces departements")
			})
			return(NULL)
		}
		
		lab.dep.leg <- 1:length(levels(xs))
		for(i in 1:length(levels(xs))){
			lab.dep.leg[i] <- as.character(table$departement[which(table$code == levels(xs)[i])])
		}
		lab.dep.leg <- lab.dep.leg[lab.dep.leg %in% inpt.dep]
		lab.dep.leg <- lab.dep.leg[lab.dep.leg %in% as.character(table$departement[which(table$code %in% unique(as.character(xs)))])]

		rnbw.col <- rainbow(length(levels(xs)))
		rnbw.col.red <- rnbw.col[which(levels(xs) %in% unique(as.character(xs)))]

		# Trac? de la courbe
		rr <- epicurve.weeks(x, strata = xs, segments = TRUE, axisnames = FALSE, col = rnbw.col,
										xlab = "Semaine", ylab = "Nombres de cas",
										main = "R?partition des cas suspects signal?s en fonction de la semaine et du departement")
		axis(1, at = rr$xvals, labels = rr$cweek, tick = FALSE, line = 0) #

		legend("topleft", legend = c(lab.dep.leg, "Cas confirm?s Chik/Dengue"), col = c(rnbw.col.red, "black"),
					pch = c(rep(15, length(lab.dep.leg)), 20), bty = "n", pt.cex = 2, cex = 0.8,
					text.col = "black", horiz = FALSE, inset = c(0.1, 0.1))
		
		#####
		# Nombre de semaines pour lesquelles on a des donn?es
		long.i <- ifelse((length(which(rr$week == rr$cweek[length(rr$cweek)])) > 0), length(rr$cweek), length(rr$cweek) - 1)

		xss <- data$cd_conc[which((as.Date(data$sign_dat, format = format.date) >= input$date.range[1])
									& (as.Date(data$sign_dat, format = format.date) <= input$date.range[2])
									& data$sign_dep %in% as.character(table$code[which(table$departement %in% inpt.dep)]))]
		sub.data <- as.data.frame(cbind('sign_dat' = format(x, "%d/%m/%Y"), 'sign_dep' = as.character(xs), 'cd_conc' = xss))

		for (i in 2:long.i){	# Pour chaque semaines
			vec <- which(rr$week == rr$cweek[i]) # R?cup?ration des cas (n? de ligne) des cas de la semaine i
			d <- c()

			sub.sub.data <- subset(sub.data, row.names(sub.data) %in% vec)
			sub.sub.data <- merge(data.frame('sign_dep' = levels(xs)), sub.sub.data, sort = FALSE)

			if(length(vec) > 0){
				d <- as.character(sub.sub.data$cd_conc)

				for (k in 1:length(d)){
					if ((d[k] == "2" | d[k] == "1") & !is.na(d[k])){	#Si le diagnostic est ?gal ? 1 ou 2 (chik ou dengue) (on exclut les valeurs manquantes NA)
						#segments(c(i - 1, i - 1, i - 0.5), c(k - 1, k - 0.5, k - 1),
								#c(i, i - 0.5, i), c(k, k, k - 0.5))	#Trac? des hachures
						points(i - 0.5, k - 0.5, pch = 20, cex = 2)
					}
				}
			}
		}
		
		return(rr)
	})
	
	
	
	###################################
	### Titre du deuxieme graphique ###
	###################################
		
	output$title.curve.conf <- renderText({
		inFile <- input$file1
		inpt.reg <- input$reg
		if (is.null(inFile)){
			return(NULL)
		} else if (substring(inFile$name, regexpr("\\.([[:alnum:]]+)$", inFile$name)) != ".csv"){
			return(NULL)
		} else if (length(inpt.reg) <= 0){
			return(NULL)
		} else {
			return(paste("R?partition des cas confirm?s en fonction de la semaine et selon le departement du", format(input$date.range[1], "%d/%m/%Y"), "au", format(input$date.range[2], "%d/%m/%Y")))
		}
	})
	
	#############################################################
	#####								   					#####
	##### Output : Courbe Epid?miologique des Cas Confirm?s #####
	#####								   					#####
	#############################################################
	
	output$epi.curve.conf <- renderPlot({
	
		output$alert.curve.conf <- renderText({
			return(NULL)
		})
	
		inFile <- input$file1
		inpt.reg <- input$reg
		if (is.null(inFile)){
			return(NULL)
		} else if (substring(inFile$name, regexpr("\\.([[:alnum:]]+)$", inFile$name)) != ".csv"){
			return(NULL)
		} else if (length(inpt.reg) <= 0){
			return(NULL)
		}

		inpt.dep <- iconv(input$dep, from = "UTF-8", to = "LATIN2")
		data <- read.csv(inFile$datapath, header = TRUE, sep = ";")
		format.date <- checkDateFormat(data$sign_dat)

		x <- data$sign_dat[which((as.Date(data$sign_dat, format = format.date) >= input$date.range[1])
							& (as.Date(data$sign_dat, format = format.date) <= input$date.range[2])
							& data$sign_dep %in% as.character(table$code[which(table$departement %in% inpt.dep)])
							& (data$cd_conc == "1" | data$cd_conc == "2"))]
		x <- as.Date(x, format = format.date)
		xs <- data$sign_dep[which((as.Date(data$sign_dat, format = format.date) >= input$date.range[1])
							& (as.Date(data$sign_dat, format = format.date) <= input$date.range[2])
							& data$sign_dep %in% as.character(table$code[which(table$departement %in% inpt.dep)])
							& (data$cd_conc == "1" | data$cd_conc == "2"))]
		
		if(length(data$sign_dat[which((as.Date(data$sign_dat, format = format.date) >= input$date.range[1])
							& (as.Date(data$sign_dat, format = format.date) <= input$date.range[2])
							& (data$cd_conc == "1" | data$cd_conc == "2"))]) <= 0) {
			output$alert.curve.conf <- renderText({
				return("\n Aucunes donn?es ? afficher pour cette p?riode")
			})
			return(NULL)
		} else if (length(xs) <= 0) {
			output$alert.curve.conf <- renderText({
				return("\n Aucunes donn?es ? afficher pour ces departements")
			})
			return(NULL)
		}
		
		lab.dep.leg <- 1:length(levels(xs))
		for(i in 1:length(levels(xs))){
			lab.dep.leg[i] <- as.character(table$departement[which(table$code == levels(xs)[i])])
		}
		lab.dep.leg <- lab.dep.leg[lab.dep.leg %in% inpt.dep]
		lab.dep.leg <- lab.dep.leg[lab.dep.leg %in% as.character(table$departement[which(table$code %in% unique(as.character(xs)))])]


		rnbw.col <- rainbow(length(levels(xs)))
		rnbw.col.red <- rnbw.col[which(levels(xs) %in% unique(as.character(xs)))]

		# Trac? de la courbe
		rr <- epicurve.weeks(x, strata = xs, segments = TRUE, axisnames = FALSE, col = rnbw.col,
										xlab = "Semaine", ylab = "Nombres de cas",
										main = "R?partition des cas confirm?s en fonction de la semaine et du departement")
		axis(1, at = rr$xvals, labels = rr$cweek, tick = FALSE, line = 0) #

		legend("topleft", legend = c(lab.dep.leg, "Cas Import?"), col = c(rnbw.col.red, "black"),
					pch = c(rep(15, length(lab.dep.leg)), 20), bty = "n", pt.cex = 2, cex = 0.8,
					text.col = "black", horiz = FALSE, inset = c(0.1, 0.1))
		
		#####
		# Nombre de semaines pour lesquelles on a des donn?es
		long.i <- ifelse((length(which(rr$week == rr$cweek[length(rr$cweek)])) > 0), length(rr$cweek), length(rr$cweek) - 1)

		xss <- data$cd_typcas[which((as.Date(data$sign_dat, format = format.date) >= input$date.range[1])
									& (as.Date(data$sign_dat, format = format.date) <= input$date.range[2])
									& data$sign_dep %in% as.character(table$code[which(table$departement %in% inpt.dep)])
									& (data$cd_conc == "1" | data$cd_conc == "2"))]
		sub.data <- as.data.frame(cbind('sign_dat' = format(x, "%d/%m/%Y"), 'sign_dep' = as.character(xs), 'cd_typcas' = xss))

		for (i in 2:long.i){	# Pour chaque semaines
			vec <- which(rr$week == rr$cweek[i]) # R?cup?ration des cas (n? de ligne) des cas de la semaine i
			d <- c()
			
			sub.sub.data <- subset(sub.data, row.names(sub.data) %in% vec)
			sub.sub.data <- merge(data.frame('sign_dep' = levels(xs)), sub.sub.data, sort = FALSE)

			if(length(vec) > 0){
				d <- as.character(sub.sub.data$cd_typcas)

				for (k in 1:length(d)){
					if (d[k] == "1" & !is.na(d[k])){	#Si le type de cas ? 1 (import?) (on exclut les valeurs manquantes NA)
						#segments(c(i - 1, i - 1, i - 0.5), c(k - 1, k - 0.5, k - 1),
								#c(i, i- 0.5, i), c(k, k, k - 0.5))	#Trac? des hachures
						points(i - 0.5, k - 0.5, pch = 20, cex = 2)
					}
				}
			}
		}
		
		return(rr)
	})
	
	#################################
	### Titre des maps d?taill?es ###
	#################################
	
	output$title.map.reg <- renderText({
		inFile <- input$file1
		
		if (is.null(inFile)){
			return(NULL)
		} else if (substring(inFile$name, regexpr("\\.([[:alnum:]]+)$", inFile$name)) != ".csv"){
			return(NULL)
		} else {
			return("D?tail par R?gion")
		}
	})
	
	#################################################
	#####								   		#####
	##### Output : D?tail Des Cartes par R?gion #####
	#####								   		#####
	#################################################
	
	output$map.reg <- renderPlot({
		
		inFile <- input$file1
		inpt.reg <- input$reg
		
		output$alert.map.reg <- renderText({
			return(NULL)
		})
		
		if (is.null(inFile)){
			return(NULL)
		} else if (substring(inFile$name, regexpr("\\.([[:alnum:]]+)$", inFile$name)) != ".csv"){
			return(NULL)
		} else if (length(inpt.reg) <= 0){
			output$alert.map.reg <- renderText({
				return('\n Aucunes donn?es pour les r?gions selectionn?es')
			})
			return(NULL)
		} else {
			data <- read.csv(inFile$datapath, header = TRUE, sep = ";")
			format.date <- checkDateFormat(data$sign_dat)

			data.reg <- subset(data, sign_region %in% unique(as.character(table$abreviation[which(table$region %in% inpt.reg)]))
										& ((as.Date(sign_dat, format = format.date)) <= input$date.range[2])
										& ((as.Date(sign_dat, format = format.date)) >= input$date.range[1]))

			check.data.reg <- table(data.reg$sign_region)
			df <- as.data.frame(table(data.reg$sign_dep))
			names(df) <- c('dep', 'eff')
			df <- merge(df, table[, which(names(table) %in% c('region', 'code'))], by.x = 'dep', by.y = 'code')
			dff <- merge(df, table[, which(names(table) %in% c('departement', 'code'))], by.x = 'dep', by.y = 'code')
			dff <- merge(dff, subset(table[, which(names(table) %in% c('departement', 'region', 'code'))], table$region %in% unique(dff$region)), all.y = TRUE)
			dff <- dff[-which(names(dff) == 'dep')]
			dff <- subset(dff, region %in% inpt.reg)
			dff$region <- gsub(" ", "-", toupper(iconv(dff$region, to = "ASCII//TRANSLIT")))
			dff$departement <- gsub(" ", "-", toupper(iconv(dff$departement, to = "ASCII//TRANSLIT")))
		
			if(nrow(dff) <= 0){
				output$alert.map.reg <- renderText({
					return('\n Aucunes donn?es pour les r?gions selectionn?es')
				})
				return(NULL)
			}
			
			if (length(unique(dff$region)) == 1){
				par(mfrow = c(1, 1))
			} else {
				par(mfrow = c(round(length(unique(dff$region))/2), 2))
			}

			ColWithAlpha <- function(col, alpha = 191){
			  col <- as.vector(col2rgb(col))
			  out <- rgb(col[1], col[2], col[3], alpha, maxColorValue = 255)
			  return(out)
			}

			for (i in 1:length(unique(dff$region))){
				reg <- france[france$NOM_REGION == unique(dff$region)[i], ]
				plot(reg, col = 'lightgrey')
				eff <- dff$eff[which(dff$region == unique(dff$region)[i])]
				#points(x = reg$X_CENTROID * 100, y = reg$Y_CENTROID * 100, pch = 21, cex = (eff + 10) * 0.5, bg = "#FFFFFFBF")
				text(x = reg$X_CENTROID * 100, y = reg$Y_CENTROID * 100, eff, cex = 1.5)
			}
		}
	})
})
