library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(readr)

# Simuler l'authentification (login et mot de passe)
credentials <- list(user = "admin", password = "password123")

# Charger les fichiers CSV
load_data <- function() {
  logements_combines <- read.csv("combined_logements.csv", sep = ",")
  adresses <- read.csv("adresses-69.csv", sep = ";")
  
  # Fusionner les données
  merge(logements_combines, adresses, by.x = c("Coordonnée_cartographique_X_.BAN.", "Coordonnée_cartographique_Y_.BAN."), by.y = c("x", "y"), all.x = TRUE)
}

logements_adresses <- load_data()

# Fonction pour déterminer la couleur en fonction de l'étiquette DPE
getColor <- function(dpe) {
  if (dpe == "A") {
    return("green")
  } else if (dpe == "B") {
    return("lightgreen")
  } else if (dpe == "C") {
    return("yellow")
  } else if (dpe == "D") {
    return("orange")
  } else if (dpe == "E") {
    return("orangered")
  } else if (dpe == "F") {
    return("red")
  } else {
    return("darkred")
  }
}

logements_adresses$color <- sapply(logements_adresses$Etiquette_DPE, getColor)

# UI
ui <- fluidPage(
  titlePanel("Analyse complète des logements"),
  
  # UI de connexion
  uiOutput("login_ui"),
  
  # UI de l'application après connexion
  uiOutput("app_ui")
)

# Serveur
server <- function(input, output, session) {
  
  # Stocker l'état de connexion
  values <- reactiveValues(authenticated = FALSE)
  
  # Vérifier les identifiants de connexion
  observeEvent(input$login_btn, {
    if (input$user == credentials$user && input$password == credentials$password) {
      values$authenticated <- TRUE
    } else {
      showModal(modalDialog(
        title = "Erreur",
        "Identifiants incorrects. Veuillez réessayer.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Interface de connexion
  output$login_ui <- renderUI({
    if (!values$authenticated) {
      fluidPage(
        titlePanel("Connexion"),
        textInput("user", "Nom d'utilisateur"),
        passwordInput("password", "Mot de passe"),
        actionButton("login_btn", "Se connecter")
      )
    }
  })
  
  # Interface principale après connexion
  output$app_ui <- renderUI({
    if (values$authenticated) {
      tabsetPanel(
        
        # Onglet 1 : Contexte avec des images
        tabPanel("Contexte",
                 sidebarLayout(
                   sidebarPanel(),
                   mainPanel(
                     p("GreenTech Solutions est une société de services dédiée au développement d'applications innovantes, avec un focus particulier sur les enjeux environnementaux et énergétiques. Face aux défis actuels liés au changement climatique et à l'augmentation des coûts de l'énergie, la transition énergétique et l'amélioration de l'efficacité énergétique sont devenues des priorités nationales en France.

Dans ce contexte, Enedis, l'un des principaux gestionnaires de réseaux de distribution d'électricité, nous a sollicités pour évaluer l'impact des performances énergétiques des logements sur leur consommation d'électricité. Cette étude porte spécifiquement sur le Diagnostic de Performance Énergétique (DPE) des bâtiments, une mesure clé de l'efficacité énergétique qui classe les logements en fonction de leur consommation d'énergie (de A à G).

"),
                     img(src = "green.jpg", height = "200px"),  # Chemin relatif à partir du dossier www
                     p("Objectifs de l'analyse :

Évaluer la consommation énergétique des logements en fonction de leur classe DPE.
Comparer les performances énergétiques des logements neufs et existants.
Visualiser les disparités géographiques en termes de consommation d'énergie et de performances énergétiques.
Données utilisées : Les données proviennent d'un ensemble de bâtiments neufs et existants répartis à travers différentes régions de France. Pour chaque logement, les informations suivantes sont analysées :

Consommation énergétique : la consommation électrique en kilowattheures (kWh).
Étiquette DPE : une note allant de A (très performant) à G (très énergivore).
Localisation géographique : coordonnées GPS des logements pour permettre une analyse géospatiale des consommations."),
                     img(src = "DPE.png", height = "200px"),  # Chemin relatif à partir du dossier www
                     p("Les résultats permettent d'analyser les performances énergétiques à travers les étiquettes DPE et autres indicateurs clés.")
                   )
                 )
        ),
        
        # Onglet 2 : KPI
        tabPanel("KPI",
                 sidebarLayout(
                   sidebarPanel(),
                   mainPanel(
                     h3("Indicateurs clés de performance"),
                     p("Nombre total de logements : ", nrow(logements_adresses)),
                     p("Consommation énergétique moyenne (kWh) : ", round(mean(as.numeric(logements_adresses$Conso_5_usages_é_finale), na.rm = TRUE), 2)),
                     tableOutput("kpiTable")
                   )
                 )
        ),
        
        # Onglet 3 : Choix du thème des graphiques
        tabPanel("Thème des graphiques",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("themeChoice", "Choisir un thème graphique", 
                                 choices = c("Minimal" = "theme_minimal", 
                                             "Classique" = "theme_classic", 
                                             "Gris" = "theme_gray",
                                             "Noir et Blanc" = "theme_bw"))
                   ),
                   mainPanel(
                     plotOutput("themedPlot")
                   )
                 )
        ),
        
        # Onglet 4 : Carte interactive
        tabPanel("Carte",
                 sidebarLayout(
                   sidebarPanel(
                     textInput("communeFilter", "Filtrer par commune", value = ""),
                     selectInput("colorChoice", "Choisir un type de couleur", 
                                 choices = c("Étiquette DPE", "Consommation"))
                   ),
                   mainPanel(
                     leafletOutput("map")
                   )
                 )
        ),
        
        # Onglet 5 : Graphiques de consommation
        tabPanel("Graphiques",
                 sidebarLayout(
                   sidebarPanel(
                     checkboxGroupInput("dpeFilter", "Filtrer par étiquette DPE",
                                        choices = unique(logements_adresses$Etiquette_DPE), selected = unique(logements_adresses$Etiquette_DPE)),
                     sliderInput("consumptionRange", "Consommation énergétique", 
                                 min = min(as.numeric(logements_adresses$Conso_5_usages_é_finale), na.rm = TRUE), 
                                 max = max(as.numeric(logements_adresses$Conso_5_usages_é_finale), na.rm = TRUE), 
                                 value = c(min(as.numeric(logements_adresses$Conso_5_usages_é_finale), na.rm = TRUE), 
                                           max(as.numeric(logements_adresses$Conso_5_usages_é_finale), na.rm = TRUE))
                     )
                   ),
                   mainPanel(
                     plotOutput("consoPlot"),
                     plotOutput("histDPE"),
                     plotOutput("scatterPlot")
                   )
                 )
        ),
        
        # Onglet 6 : Régression avec export
        tabPanel("Régression",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("xVar", "Choisir la variable X", choices = names(logements_adresses)),
                     selectInput("yVar", "Choisir la variable Y", choices = names(logements_adresses)),
                     actionButton("regressionBtn", "Calculer la régression"),
                     downloadButton("downloadPlot", "Télécharger le graphique en PNG"),
                     downloadButton("downloadData", "Télécharger les données en CSV")
                   ),
                   mainPanel(
                     plotOutput("regressionPlot"),
                     textOutput("correlation")
                   )
                 )
        )
      )
    }
  })
  
  # Carte interactive
  output$map <- renderLeaflet({
    leaflet(data = logements_adresses) %>%
      addTiles() %>%
      addCircleMarkers(~lon, ~lat, color = ~color, radius = 5, popup = ~paste("DPE:", Etiquette_DPE))
  })
  
  # Tableau KPI
  output$kpiTable <- renderTable({
    logements_adresses %>%
      group_by(Etiquette_DPE) %>%
      summarise(Nombre_logements = n(),
                Conso_moyenne_kWh = round(mean(as.numeric(Conso_5_usages_é_finale), na.rm = TRUE), 2))
  })
  
  # Graphique consommation énergétique par DPE (boîte à moustaches)
  output$consoPlot <- renderPlot({
    data <- logements_adresses %>%
      filter(Etiquette_DPE %in% input$dpeFilter) %>%
      filter(as.numeric(Conso_5_usages_é_finale) >= input$consumptionRange[1], 
             as.numeric(Conso_5_usages_é_finale) <= input$consumptionRange[2])
    
    ggplot(data, aes(x = Etiquette_DPE, y = as.numeric(Conso_5_usages_é_finale), fill = Etiquette_DPE)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Consommation énergétique par classe DPE", x = "Étiquette DPE", y = "Consommation (kWh)")
  })
  
  # Histogramme des étiquettes DPE
  output$histDPE <- renderPlot({
    ggplot(logements_adresses, aes(x = Etiquette_DPE, fill = Etiquette_DPE)) +
      geom_bar() +
      theme_minimal() +
      labs(title = "Répartition des classes DPE", x = "Étiquette DPE", y = "Nombre de logements")
  })
  
  # Nuage de points pour différents types d'énergie
  output$scatterPlot <- renderPlot({
    ggplot(logements_adresses, aes(x = Etiquette_DPE, y = as.numeric(Conso_5_usages_é_finale), color = Etiquette_DPE)) +
      geom_point(size = 3) +
      theme_minimal
    geom_point(size = 3) +
      theme_minimal() +
      labs(title = "Nuage de points pour Consommation 5 usages finale", x = "Étiquette DPE", y = "Consommation 5 usages finale (kWh)")
  })
  
  # Régression linéaire et tracé
  regressionData <- reactive({
    req(input$xVar, input$yVar)
    logements_adresses %>%
      select(all_of(input$xVar), all_of(input$yVar)) %>%
      filter(!is.na(.[[1]]) & !is.na(.[[2]]))
  })
  
  output$regressionPlot <- renderPlot({
    req(input$regressionBtn)
    data <- regressionData()
    ggplot(data, aes_string(x = input$xVar, y = input$yVar)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = paste("Régression linéaire entre", input$xVar, "et", input$yVar))
  })
  
  output$correlation <- renderText({
    req(input$regressionBtn)
    data <- regressionData()
    corr <- cor(data[[input$xVar]], data[[input$yVar]], use = "complete.obs")
    paste("Coefficient de corrélation :", round(corr, 2))
  })
  
  # Export du graphique en PNG
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("regression_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      data <- regressionData()
      plot <- ggplot(data, aes_string(x = input$xVar, y = input$yVar)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        labs(title = paste("Régression linéaire entre", input$xVar, "et", input$yVar))
      print(plot)
      dev.off()
    }
  )
  
  # Export des données en CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("regression_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(regressionData(), file)
    }
  )
  
  # Choix du thème graphique pour les autres graphiques
  output$themedPlot <- renderPlot({
    theme_func <- match.fun(input$themeChoice)
    ggplot(logements_adresses, aes(x = Etiquette_DPE, y = as.numeric(Conso_5_usages_é_finale), fill = Etiquette_DPE)) +
      geom_boxplot() +
      theme_func() +
      labs(title = "Consommation par Étiquette DPE", x = "Étiquette DPE", y = "Consommation (kWh)")
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)