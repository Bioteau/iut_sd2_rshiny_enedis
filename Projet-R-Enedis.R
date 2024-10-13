library(shiny)
library(DT)
library(leaflet)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(shinyauthr)
library(httr)
library(jsonlite)
library(tidyr)
library(shinyjs)
csv <- "https://raw.githubusercontent.com/Bioteau/iut_sd2_rshiny_enedis/main/DATA.csv"
data <- read.csv(csv)
data$Code_postal_.BAN. <- paste0("0", data$Code_postal_.BAN.)
# Créer une table des utilisateurs avec noms et mots de passe
user_base <- tibble::tibble(
  user = c("admin"),  # Noms d'utilisateurs
  password = c("admin"),  # Mots de passe correspondants
  permissions = c("admin"),  # Permissions si vous souhaitez des rôles
  name = c("Administrateur")  # Noms réels des utilisateurs
)

# Interface utilisateur (UI)
ui <- fluidPage(
  tags$head(
  tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
  tags$style(HTML("
    body {
        background: url('https://image.freepik.com/vecteurs-libre/fond-connexion-reseau_23-2148879892.jpg') no-repeat center center fixed;
        background-size: cover;
    }
    .wrapper {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%,-50%);
        width: 350px;
        text-align: center;
        border: 1px solid rgb(241,241,241);
        border-radius: 12px;
        padding: 10px 20px;
        background: transparent;
        backdrop-filter: blur(6px);
        box-shadow: 5px 5px 10px 0 rgba(0,0,0,0.5);
    }
    .wrapper h2 {
        color: #0;
        font-size: 30px;
        text-decoration: underline;
        text-decoration-thickness: 1px;
        text-underline-offset: 5px;
    }
        .input-field input {
        border-radius: 10px;
        background: transparent;
        margin: 15px;
        border: 2px solid rgb(255,255,255);
        width: 280px;
        padding: 20p;
        backdrop-filter: blur(15px);
    }
    .input-field i {
        position: absolute;
        right: 10px;
        top: 50%;
        transform: translateY(-50%);
        color: rgb(252,252,252);
    }
    .login {
        background: #fff;
        border: none;
        cursor: pointer;
        font-weight: 600;
        border-radius: 45px;
        width: 200px;
        height: 30px;
    }
    .logged-in .wrapper h2{
    backdrop-filter: none; /* je voulais enlever la barre avec le flou derrière :'( */
    }
    .sidebarPanel {
      color: #000000; /* Couleur du texte pour la sidebarPanel (noir) */
    }
    .tab-content h1 {
      color: #ffffff; /* Couleur du titre h1 */
      }
    .tab-content p {
      color: #ffffff; /* Couleur du texte paragraphe */
    }
    /* Styles personnalisés pour les tableaux */
      table {
        width: 100%;
        border-collapse: collapse;
        margin-bottom: 20px;
      }
      th {
        background-color: #f2f2f2; /* Couleur de fond de l'en-tête */
        color: #333; /* Couleur du texte de l'en-tête */
        padding: 8px;
        text-align: left;
        border-bottom: 1px solid #ddd;
      }
      td {
        padding: 8px;
        text-align: left;
        border-bottom: 1px solid #ddd;
        background-color: #f2f2f2;
      }
      table.dataTable tbody td {
      padding: 1px;  /* Réduction de l'espace entre les cellules */
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      padding: 1px;  /* Réduction de la taille des boutons de pagination */
    }
    .dataTables_length, .dataTables_filter, .dataTables_info {
      font-size: 20%; /* Réduction de la taille de la police */
    }
      .custom-button {
    background-color: #FFA500 !important;  /* Couleur orange forcée */
    color: white !important;  /* Couleur du texte */
    border: none !important;  /* Pas de bordure */
    padding: 10px 20px !important;  /* Taille du bouton */
    text-align: center !important;
    text-decoration: none !important;
    display: inline-block !important;
    font-size: 12.5px !important;
    margin: 4px 2px !important;
    cursor: pointer !important;
    border-radius: 12px !important;  /* Arrondi des bords */
    transition-duration: 0.4s !important;  /* Transition pour l'effet hover */
  }
  
  .custom-button:hover {
    background-color: #000000 !important;  /* Couleur au survol */
  }
  ")),
  
  # Interface de connexion
  div(class = "wrapper", 
      shinyauthr::loginUI("login")),
  
  # Interface principale de l'application, cachée tant que l'utilisateur n'est pas connecté
  uiOutput("appUI")
))

# Serveur
server <- function(input, output, session) {
  
  # Configurer l'authentification
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password
  )
  
  # Si l'utilisateur est connecté, afficher l'interface de l'application
  output$appUI <- renderUI({
    req(credentials()$user_auth)  # Vérifier que l'utilisateur est authentifié
    
    # Interface de l'application principale après authentification
    tagList(
      shinythemes::themeSelector(),
      navbarPage("Application de Consommation Énergétique",
                 tabPanel("Carte", 
                          sidebarLayout(
                            sidebarPanel(br(),
                              useShinyjs(),# Inclure shinyjs
                              div(
                                style = "texte-align: center; display: flex; align-items: center;",  # Style pour alignement
                                actionButton("get_data","Mettre à jour la base de données",class="custom-button")
                              ),
                              div(id = "loading_image", 
                                  tags$img(src = "https://cdn.dribbble.com/users/2374064/screenshots/4737393/bus-truning.gif", height = "50px", width = "200px"), 
                                  style = "margin-left: 10px; display:none;"
                              ),
                              div(
                                id="loading_text",
                                "(Chargement des données...)",
                                style="display:none; margin-top: 5px;"),
                                br(),br(),br(),
                              checkboxGroupInput("lettres", "Sélectionnez une ou plusieurs lettres (A-G) :", 
                                                 choices = sort(unique(data$Etiquette_DPE)),  
                                                 selected = sort(unique(data$Etiquette_DPE))),
                              
                              selectInput("code_postal", "Sélectionnez un code postal :", 
                                          choices = c("Tous" = "Tous", sort(unique(data$Code_postal_.BAN.))),  
                                          multiple = TRUE),
                              img(src = "https://yt3.googleusercontent.com/ytc/AIdro_kgtfH_f6It_DR8WyxPyNWyWxTg1GY-b1zBmIX2e9310No=s900-c-k-c0x00ffffff-no-rj", 
                                  height = "165px", width = "165px", class="img-fluid")
                            ),
                            mainPanel(
                              leafletOutput("map"),
                              br(),br(),
                              img(src = "https://invisty.com/wp-content/uploads/2021/11/diagnostic-de-performance-energetique-1024x640.jpg", 
                                  height = "330px", width = "600px", class="img-fluid")
                            )
                          )
                 ),
                 
                 tabPanel("Tableau", 
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("colonnes", "Sélectionnez les colonnes à afficher :", 
                                                 choices = c(
                                                   "Etiquette DPE" = "Etiquette_DPE",
                                                   "Nombre de logements" = "Nombre_logements",
                                                   "Consommation moyenne" = "Consommation_moyenne"),
                                                 selected = c("Etiquette_DPE",
                                                              "Nombre_logements", "Consommation_moyenne")),
                              downloadButton("downloadData", "Télécharger les données")
                            ),
                            mainPanel(
                              tableOutput("tableau")  
                            )
                          )
                 ),
                 
                 tabPanel("Existant/Neuf",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("type_batiment", "Sélectionnez le type de bâtiment :", 
                                                 choices = c("Existant", "Neuf"),selected = "Existant"),
                              downloadButton("downloadGraph", "Télécharger le graphique")
                            ),
                            mainPanel(
                              plotOutput("graphique_DPE")
                            )
                          )
                 ),
                 tabPanel("Graphiques",
                          sidebarLayout(
                            sidebarPanel(
                              # Ajoutez ici vos KPI
                              h4("Indicateurs Clés de Performance (KPI)"),
                              verbatimTextOutput("kpi1"),
                              verbatimTextOutput("kpi2"),
                              verbatimTextOutput("kpi3"),
                              # Ajoutez d'autres KPI selon vos besoins
                            ),
                            mainPanel(
                              plotOutput("BoiteMoustache"),
                              br(),
                              plotOutput("NuagePoints")
                            )
                          )
                 )
                 ,
                 tabPanel("Histogramme",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("bins", "Nombre de classes :", 
                                          min = 5, max = 50, value = 20),
                              checkboxGroupInput("lettres_DPE_histogramme", "Sélectionnez les lettres DPE :", 
                                                 choices = sort(unique(data$Etiquette_DPE)),
                                                 selected = sort(unique(data$Etiquette_DPE)))
                            ),
                            mainPanel(
                              plotOutput("Histogramme")  # Ajout de l'histogramme
                            )
                          )
                 ),
                 tabPanel("Contexte",
                              h1("Bienvenue sur cette dernière page !",style="font-size:70px;"),
                              p("Cette page supplémentaire permet de présenter et visualiser les données disponibles.",style="font-size:20px;"),
                              p("Les données récoltés dans la base de donné ne sont pas touté utilisé dans l'application",style="font-size:20px;"),
                              p("Les données utilisés sont la Consommation, Latitude, Longitude, Code postal, Type bâtiment, l'Etiquette DPE et le type de logement (neuf/ancien).",style="font-size:20px;"),
                              div(style = 'width:100%;margin:auto;',
                                DTOutput("Donnée"))
                 )
      )
    )
  })
  
  # Filtrer les données en fonction des sélections de l'utilisateur
  data_filtered <- reactive({
    if ("Tous" %in% input$code_postal) {
      data %>%
        filter(Etiquette_DPE %in% input$lettres)
    } else {
      data %>%
        filter(Etiquette_DPE %in% input$lettres,
               Code_postal_.BAN. %in% input$code_postal)
    }
  })
  
  # Afficher la carte
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 5.35, lat = 46.0, zoom = 9)
  })
  
  # Mettre à jour les marqueurs de la carte
  observe({
    if (length(input$lettres) > 0) {
      leafletProxy("map", data = data_filtered()) %>%
        clearMarkers() %>%
        addCircleMarkers(~Longitude, ~Latitude, radius = 1, 
                         popup = ~paste("Consommation:", Conso_5_usages_é_finale),
                         clusterOptions = markerClusterOptions(
                           maxClusterRadius = 40, 
                           spiderfyOnMaxZoom = TRUE, 
                           zoomToBoundsOnClick = TRUE  
                         )
        )
    } else {
      leafletProxy("map") %>%
        clearMarkers()
    }
  })
  
  # Afficher le tableau récapitulatif
  output$tableau <- renderTable({
    tableau <- data_filtered() %>%
      group_by(Code_postal_.BAN., Etiquette_DPE) %>%
      summarise(
        Nombre_logements = n(),  
        Consommation_moyenne = mean(Conso_5_usages_é_finale, na.rm = TRUE)  
      )
    
    tableau %>%
      select(all_of(input$colonnes))
  })
  
  # Gestionnaire de téléchargement pour le CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_filtered() %>%
                  group_by(Code_postal_.BAN., Etiquette_DPE) %>%
                  summarise(
                    Nombre_logements = n(),  
                    Consommation_moyenne = mean(Conso_5_usages_é_finale, na.rm = TRUE)
                  ) %>%
                  select(all_of(input$colonnes)), file, row.names = FALSE)
    }
  )
  
  # Graphique comparant la consommation entre les DPE
  output$graphique_DPE <- renderPlot({
    # Filtrer les données en fonction des choix de l'utilisateur
    data_filtered <- data
    
    if (length(input$type_batiment) == 1) {
      # Si un seul type de bâtiment est sélectionné, filtrer sur ce type
      data_filtered <- data %>% filter(Type == input$type_batiment)
    } else if (length(input$type_batiment) == 2) {
      # Si les deux types sont sélectionnés, afficher tout
      data_filtered <- data
    } else if (length(input$type_batiment) == 0) {
      # Si aucun type n'est sélectionné, peut-être afficher un message d'erreur ou ne rien afficher
      return(NULL)
    }
    
    # Calcul de la consommation moyenne par étiquette DPE
    data_summarised <- data_filtered %>%
      group_by(Etiquette_DPE) %>%
      summarise(Consommation_moyenne = mean(Conso_5_usages_é_finale, na.rm = TRUE))
    
    # Création du graphique
    ggplot(data_summarised, aes(x = Etiquette_DPE, y = Consommation_moyenne, fill = Etiquette_DPE)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Étiquette DPE", y = "Consommation moyenne (kWh)", 
           title = "Consommation moyenne par Étiquette DPE")
  })
  
  # Dans le serveur
  
  output$downloadGraph <- downloadHandler(
    filename = function() {
      paste("graphique_DPE_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Ouvrir un fichier PNG pour enregistrer le graphique
      png(file)
      
      # Générer le même graphique que celui affiché
      data_filtered <- data
      
      if (length(input$type_batiment) == 1) {
        # Si un seul type de bâtiment est sélectionné, filtrer sur ce type
        data_filtered <- data %>% filter(Type == input$type_batiment)
      } else if (length(input$type_batiment) == 2) {
        # Si les deux types sont sélectionnés, afficher tout
        data_filtered <- data
      } else if (length(input$type_batiment) == 0) {
        # Si aucun type n'est sélectionné, peut-être afficher un message d'erreur ou ne rien afficher
        return(NULL)
      }
      
      # Calcul de la consommation moyenne par étiquette DPE
      data_summarised <- data_filtered %>%
        group_by(Etiquette_DPE) %>%
        summarise(Consommation_moyenne = mean(Conso_5_usages_é_finale, na.rm = TRUE))
      
      # Création du graphique
      ggplot(data_summarised, aes(x = Etiquette_DPE, y = Consommation_moyenne, fill = Etiquette_DPE)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(x = "Étiquette DPE", y = "Consommation moyenne (kWh)", 
             title = "Consommation moyenne par Étiquette DPE")
      
      # Fermer le fichier PNG
      dev.off()
      })
  
  output$BoiteMoustache <- renderPlot({
    boxplot(Conso_5_usages_é_finale ~ Etiquette_DPE,
            data=data,
            main="Consommation moyenne énergétique par DPE",
            xlab="DPE",
            ylab="Consommation moyenne")})
  
  output$NuagePoints <- renderPlot({
    ggplot(data, aes(x = Type_bâtiment, y = Conso_5_usages_é_finale, color = Etiquette_DPE)) +
      geom_point(alpha = 0.7, size = 3) + 
      labs(title = "Relation entre le type de batiment et la consommation énergétique",
           x = "Type de bâtiment", 
           y = "Consommation énergétique (kWh)",
           color = "Étiquette DPE") +
      theme_minimal()
  })
  
  # Histogramme des consommations énergétiques par DPE
  output$Histogramme <- renderPlot({
    data_filtered <- data %>% 
      filter(Etiquette_DPE %in% input$lettres_DPE_histogramme)
    
    ggplot(data_filtered, aes(x = Conso_5_usages_é_finale)) +
      geom_histogram(bins = input$bins, fill = "blue", color = "black", alpha = 0.7) +
      labs(title = "Répartition des consommations énergétiques",
           x = "Consommation énergétique (kWh)", 
           y = "Nombre de bâtiments") +
      theme_minimal()
  })
  
  output$kpi1 <- renderText({
    paste("Consommation moyene de l'Ain: ", mean(data$Conso_5_usages_é_finale, na.rm = TRUE))  # Remplacez par votre calcul
  })
  
  output$kpi2 <- renderText({
    paste(nrow(data), " logements dans l'Ain")  # Remplacez par votre calcul
  })
  
  output$kpi3 <- renderText({
    paste("Nombre de logement neuf: ", nrow(subset(data,Type=="Neuf")))  # Remplacez par votre calcul
  })
  
  # Afficher le tableau récapitulatif
  output$Donnée <- renderDT({
    head(data,10)
  })
  
  datap <- reactiveVal(data.frame()) 
  
  observeEvent(input$get_data, {
    show("loading_image")
    show("loading_text")
    # Votre code pour récupérer et traiter les données
    csv <- "https://raw.githubusercontent.com/Bioteau/iut_sd2_rshiny_enedis/main/DATA.csv"
    datav <- read.csv(csv)
    cp <- paste0("0", unique(datav$Code_postal_.BAN.))
    
    # Logement Neuf
    base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-neufs/lines"
    max_date <- max(as.Date(datav$Date_réception_DPE, format="%Y-%m-%d"), na.rm = TRUE)
    df <- data.frame()
    
    for (i in 1:length(cp)) {
      params <- list(
        page = 1,
        size = 10000,
        select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,_geopoint,Type_bâtiment,Conso_5_usages_é_finale,Date_réception_DPE",
        q = cp[i],
        q_fields = "Code_postal_(BAN)",
        qs = paste("Date_réception_DPE:[", max_date, " TO ", Sys.Date(), "]", sep = "")
      )
      
      url_encoded <- modify_url(base_url, query = params)
      response <- GET(url_encoded)
      
      if (status_code(response) == 200) {
        content <- fromJSON(rawToChar(response$content), flatten = FALSE)
        df <- bind_rows(df, content$results)
      } else {
        cat("Erreur lors de la récupération des données (Neuf):", status_code(response), "\n")
      }
    }
    
    neufs <- df
    
    # Logement Existant
    base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"
    df <- data.frame()
    
    for (i in 1:length(cp)) {
      params <- list(
        page = 1,
        size = 10000,
        select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,_geopoint,Type_bâtiment,Conso_5_usages_é_finale,Date_réception_DPE",
        q = cp[i],
        q_fields = "Code_postal_(BAN)",
        qs = paste("Date_réception_DPE:[", max_date, " TO ", Sys.Date(), "]", sep = "")
      )
      
      url_encoded <- modify_url(base_url, query = params)
      response <- GET(url_encoded)
      
      if (status_code(response) == 200) {
        content <- fromJSON(rawToChar(response$content), flatten = FALSE)
        df <- bind_rows(df, content$results)
      } else {
        cat("Erreur lors de la récupération des données (Existant):", status_code(response), "\n")
      }
    }
    
    existants <- df
    neufs$Type <- "Neuf"
    existants$Type <- "Existant"
    data1_combined <- rbind(existants, neufs)
    
    # Fusionner les deux datasets
    data1_combined <- data1_combined %>%
      filter(!is.na(Conso_5_usages_é_finale)) %>%
      separate("_geopoint", into = c("Latitude", "Longitude"), sep = ",")
    colnames(data1_combined) <- colnames(datav)
    
    
    data <- rbind(datav,data1_combined)
    hide("loading_image")
    hide("loading_text")
  })
}



# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
