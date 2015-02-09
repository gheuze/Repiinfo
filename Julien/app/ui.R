########################################################################
### ui.r ###
### Institut de Veille Sanitaire ###
### Cire Languedoc-Roussillon ###
### Navarre Julien ###
### 01/08/2013 ###
### Version 0. ###
########################################################################

shinyUI(pageWithSidebar(
	# Titre
	headerPanel("Reporting VOOZARBO"),
	# Barre d'outils à gauche
	sidebarPanel(
		# Upload
		fileInput('file1', 'Importez le fichier .CSV',
					accept = c('text/csv', 'text/comma-separated-values,text/plain')),
		# Barre horizontale
		tags$hr(),
		# Liste des régions (choix multiple = TRUE)
		selectInput("reg", "Regions", regions, selected = regions[2],  multiple = TRUE),
		helpText("Note: Maintenez la touche 'CTRL' pour choisir plusieurs regions"),
		tags$hr(),
		# CheckboxGroup des départements de la région
		checkboxInput('dep.all', 'Tous les departements ?', TRUE),
		tags$hr(),
		checkboxGroupInput('dep', 'Departements', dep.list[[2]]),
		tags$hr(),
			
		# PERIODE
		dateRangeInput("date.range", "Periode", start = paste(format(Sys.Date(), "%Y"), "-05-01", sep = ""), end = NULL,
			min = NULL, max = Sys.Date(), format = "dd/mm/yyyy",
			startview = "month", weekstart = 0, language = "fr",
			separator = " a "),
		checkboxInput('last.days', 'Afficher les cas des 7 derniers jours ?', TRUE),
		tags$hr(),
		actionButton('download', 'Exporter en XLSX')
	),
	# Layout principal
	mainPanel(
		tabsetPanel(
			tabPanel("Retro-Info", h3(textOutput('title.gen')), tableOutput('contents'), h3(textOutput('title.sub.table')), tableOutput('tab.last.days')), 
			tabPanel("Graphiques", h3(textOutput('title.curve')), h4(textOutput('alert.curve')), plotOutput('epi.curve'),
						h3(textOutput('title.curve.conf')), h4(textOutput('alert.curve.conf')), plotOutput('epi.curve.conf')),
			tabPanel("Cartes", h3(textOutput('title.gvis.map')), htmlOutput("gvis"),
						h3(textOutput('title.map.reg')), h4(textOutput('alert.map.reg')), plotOutput('map.reg'))
		)
	)
))
