library(shiny)
library(shinydashboard)
library(ggplot2)
library(tm)
library(slam)
library(dplyr)
library(tidyverse)
library(sf)
library(leaflet)

#Préparation des graphiques
tab <- read.csv2("tab.csv")

#préparation 2 options pour le graphique des qualificatifs
copietabcomplet <- tab
modalites_mode  <- c("pietons", "cycliste")

copietabcomplet_filtre <- copietabcomplet %>%
  filter(Mode %in% modalites_mode) %>%
  select(ID, Mode, Qualificatif_1)

tab_pietons <- copietabcomplet_filtre %>%
  filter(Mode=="pietons")

tab_cyclistes <- copietabcomplet_filtre %>%
  filter(Mode=="cycliste")

#nettoyage tab_pietons pour avoir les qualificatifs sans erreurs
tab_pietons$Qualificatif_1 <- ifelse(
  is.na(tab_pietons$Qualificatif_1),
  "",
  tab_pietons$Qualificatif_1
)

tab_pietons$Qualificatif_1 <- iconv(
  tab_pietons$Qualificatif_1,
  from = "latin1",
  to   = "UTF-8",
  sub  = "byte"
)
tab_pietons$Qualificatif_1 <- gsub(" ", "_", tab_pietons$Qualificatif_1)
tab_pietons$Qualificatif_1 <- sub("[ _]+$", "", tab_pietons$Qualificatif_1)

corpus_p <- Corpus(VectorSource(tab_pietons$Qualificatif_1))
corpus_p <- tm_map(corpus_p, content_transformer(tolower))
corpus_p <- tm_map(corpus_p, removeWords, stopwords("french"))
corpus_p <- tm_map(corpus_p, stripWhitespace)
dtm_p <- DocumentTermMatrix(corpus_p)
dtm_p <- removeSparseTerms(dtm_p, 0.99)

frequences_p <- sort(col_sums(dtm_p), decreasing = TRUE)
frequences_p <- frequences_p[frequences_p > 1] # plus d'une occurrence

df_freq_p <- data.frame(
  terme     = names(frequences_p),
  frequence = as.numeric(frequences_p),
  row.names = NULL
)

#même chose pour tab_cyclistes
tab_cyclistes$Qualificatif_1 <- ifelse(
  is.na(tab_cyclistes$Qualificatif_1),
  "",
  tab_cyclistes$Qualificatif_1
)

tab_cyclistes$Qualificatif_1 <- iconv(
  tab_cyclistes$Qualificatif_1,
  from = "latin1",
  to   = "UTF-8",
  sub  = "byte"
)
tab_cyclistes$Qualificatif_1 <- gsub(" ", "_", tab_cyclistes$Qualificatif_1)
tab_cyclistes$Qualificatif_1 <- sub("[ _]+$", "", tab_cyclistes$Qualificatif_1)

corpus_c <- Corpus(VectorSource(tab_cyclistes$Qualificatif_1))
corpus_c <- tm_map(corpus_c, content_transformer(tolower))
corpus_c <- tm_map(corpus_c, removeWords, stopwords("french"))
corpus_c <- tm_map(corpus_c, stripWhitespace)
dtm_c <- DocumentTermMatrix(corpus_c)
dtm_c <- removeSparseTerms(dtm_c, 0.99)

frequences_c <- sort(col_sums(dtm_c), decreasing = TRUE)
frequences_c <- frequences_c[frequences_c > 1] # plus d'une occurrence

df_freq_c <- data.frame(
  terme     = names(frequences_c),
  frequence = as.numeric(frequences_c),
  row.names = NULL
)

## Préparation pour la carte
# Stations vélohop
stations <- st_read("stations-velhop.geojson")

stations_sf <- st_as_sf(
  stations,
  coords = c("lon", "lat"),
  crs = 4326,
  agr = "constant"
)

# Secteurs
secteurs <- st_read("data_strasbourg_mode_pct/strasbourg_secteurs.geojson")
secteurs <- st_transform(secteurs, 4326)

# Données % vélo
mode_pct <- read.csv("data_strasbourg_mode_pct/strasbourg_mode_pct.csv", stringsAsFactors = FALSE)
mode_pct$district <- sprintf("%03d", as.integer(mode_pct$district))

# conversion de la colonne "hour" en format 24h
convert_hour_24 <- function(x) {
  x <- trimws(as.character(x))
  
  h <- as.numeric(sub("(am|pm)$", "", x))
  ampm <- sub("^[0-9]+", "", x)
  
  h[ampm == "am" & h == 12] <- 0
  h[ampm == "pm" & h != 12] <- h[ampm == "pm" & h != 12] + 12
  
    h
}

mode_pct$hour_num <- convert_hour_24(mode_pct$hour)
mode_pct$hour_24 <- paste0(mode_pct$hour_num, "h")

evol_velo <- mode_pct %>% #évolution heure par heure
  group_by(hour_num) %>%
  summarise(
    pct_velo_moy = mean(mode1, na.rm = TRUE),
    pct_voiture_moy = mean(mode2,na.rm = TRUE),
    pct_commun_moy = mean(mode3,na.rm = TRUE)
  )

tab_quai <- mode_pct %>%
  filter(district == "037")



ui <- dashboardPage(
  
  dashboardHeader(
    title = "Mobilité à Strasbourg",
    dropdownMenu(
                      type = "notifications",
                      icon = icon("share-alt", title = "Partager"),
                      
                      badgeStatus = NULL,
                      notificationItem(
                        text = "Twitter",
                        icon = icon("twitter"),
                        href = "https://twitter.com/intent/tweet?url=https://luz-azucena.shinyapps.io/rendu_dashboard_2/"
                      ),
                      
                      notificationItem(
                        text = "Instagram",
                        icon = icon("instagram"),
                        href = "https://www.instagram.com/"
                      ),
                      
                      notificationItem(
                        text = "Facebook",
                        icon = icon("facebook"),
                        href = "https://www.facebook.com/sharer/sharer.php?u=https://luz-azucena.shinyapps.io/rendu_dashboard_2/"
                      ),
                      
                      notificationItem(
                        text = "LinkedIn",
                        icon = icon("linkedin"),
                        href = "http://www.linkedin.com/shareArticle?mini=true&url=https://luz-azucena.shinyapps.io/rendu_dashboard_2/"
                      )
                    ),
    dropdownMenu(
      type = "tasks", # juste pour créer un <li>
      badgeStatus = NULL,
      headerText = NULL,
      icon = NULL,
      .list = list(
        tags$li(
      class = "dropdown",
      tags$a(
        href = "#",
        tags$img(src = "logo_upc - copie.png", height = "30px", style = "margin-top:10px;")
      ))))
    ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "onglets",
      menuItem("Informations générales", tabName = "general", icon = icon("info")),
      menuItem("Carte de la mobilité douce", tabName = "strasbourg_mode", icon = icon("globe"))
    )
  ),
  
  dashboardBody(
    tags$head(
    tags$style(HTML("
      .dropdown-menu .header {
        display: none;
      }
    "))
  ),
    tabItems(
      tabItem(
        tabName = "general",
        h1("Strasbourg : modes de déplacement et aménagements de la voirie pour les mobilités douces"),
        p("Grâce aux données de mobiliscope et aux résultats de l'enquête comove portant sur le ressenti des habitants de Strasbourg après la rénovation du quai des bateliers, il est possible de dresser un premier portrait de la relation des habitants de Strasbourg aux différents modes de transports utilisés au quotidien.
          Le graphique d'évolution dans la journée des modes de transports dans la population montre une prédominence évidente de la voiture comme mode de déplacement privilégié à toute heure de la journée, avec plus de 62%, loin devant les transports en commun, qui représentent un peu moins de 30% des déplacements. 
          Le vélo représente entre 8 et 10% des déplacements selon l'heure de la journée, ce qui positionne la ville de Strasbourg parmi les plus pratiquées à vélo en France. 
          En effet, d'après la fédération française des usagères et usagers de la bicyclette (FUB) la moyenne nationale serait de 3%, avec de grandes disparités. Un facteur explicatif pourrait être la part de la voirie comportant un aménagement cyclable, qui est également élevé à Strasbourg, autour de 49% pour une moyenne de 19% dans les agglomération de plus de 100 000 habitants."),
        fluidRow(
          box( #graphique1
            title = "Évolution par mode de transport",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("evol_velo", height = 400),
            p("Source : Enquête Ménages Déplacements (EMD) 2009 - Strasbourg / Bas-Rhin, Cerema (prod.), Progedo (distrib.)")
          ),
          h2("Résultats de l'enquête Co-Move"),
          p("L'enquête Co-Move, réalisées auprès de cyclistes et piétons à Strasbourg sur le quai des bateliers, met en lumière le ressenti des usagers de la voirie depuis la piétonnisation en 2020."),
          
          box(#séléctionneur + graphique2
            title = "Adjectifs les plus cités en premier (top 10)",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              inputId = "modededeplacement",
              label = "Mode de déplacement :",
              choices = c("pietons", "cycliste"),
              selected = "pietons"),
         
            plotOutput("graph_adj", height = 400) ),
          ),
      ),
          
      tabItem(
        tabName = "strasbourg_mode",
        h1("Proportion d'utilisation de la mobilité douce à Strasbourg heure par heure"),
        h3("et localisation des stations vélhop (vélos en libre-service)"),
        p("Le quai des bateliers se situe dans le secteur 37, où les déplacements à vélo sont les plus importants (31,6%). Cliquez sur chaque élement de la carte pour plus d'information."),
        sliderInput(
          inputId = "hour_num",
          label   = "Heure de la journée",
          min     = 0,
          max     = 23,
          value   = 0,
          step    = 1,
          sep     = "",
          post    = " h"
        ),
        
        leafletOutput("map", height = 600),
        p("Source : Enquête Ménages Déplacements (EMD) 2009 - Strasbourg / Bas-Rhin, Cerema (prod.), Progedo (distrib.)"),
        box(
          title = "Zone du quai des bateliers",
          width = 12,
          status = "primary",
          solidHeader = FALSE,
          plotOutput("graph_quai", height = 400)),
        box(
          width = 12,
          status = "primary",
          solidHeader = FALSE,
          p("On peut supposer de l'efficacité du service de bus de nuit pour expliquer l'activité entre 1h et 7h du matin, et la priorisation du vélo et de la voiture à partir de 8h pour se rendre au travail, à l'école, université etc."))
        )))
  )


server <- function(input, output, session) {
  
  data_hour <- reactive({
    mode_pct[mode_pct$hour_num == input$hour_num, ]
  })
  
  # Jointure secteurs + données
  secteurs_hour <- reactive({
    merge(
      secteurs,
      data_hour(),
      by.x = "CODE_SEC",
      by.y = "district",
      all.x = TRUE
    )
  })
  
  #Graphique 1:
  output$evol_velo <- renderPlot({
    ggplot(evol_velo, aes(x = hour_num)) +
      # Courbe vélo
      geom_line(aes(y = pct_velo_moy, color = "Vélo"), linewidth = 1) +
      geom_point(aes(y = pct_velo_moy, color = "Vélo"), size = 2) +
      # Courbe voiture
      geom_line(aes(y = pct_voiture_moy, color = "Voiture"), linewidth = 1) +
      geom_point(aes(y = pct_voiture_moy, color = "Voiture"), size = 2) +
      # Courbe transports en commun
      geom_line(aes(y = pct_commun_moy, color = "Transports en commun"), linewidth = 1) +
      geom_point(aes(y = pct_commun_moy, color = "Transports en commun"), size = 2) +
      scale_x_continuous(
        breaks = 0:23,
        labels = paste0(0:23, "h")) +
      scale_color_manual(values = c("Vélo" = "deeppink", "Voiture" = "steelblue", "Transports en commun" = "olivedrab2")) +
      labs(
        title = "Évolution en moyenne à Strasbourg au cours de la journée",
        x = "Heure",
        y = "Part (%)",
        color = "Mode") +
      theme_minimal(base_size = 12)
  })
  
  #Graphique 2 :
  output$graph_adj <- renderPlot({
    top_n <- 20
    df <- df_freq_reactif()
    ggplot(head(df, top_n), aes(x = reorder(terme, frequence), y = frequence)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = frequence), hjust = -0.1, size = 4) +
      coord_flip() +
      labs(
        x = "Adjectif",
        y = "Fréquence"
      ) +
      theme_linedraw() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
    
  })
  
  #input pour séléctionneur pour le graphique 2 :
  df_freq_reactif <- reactive({
    req(input$modededeplacement)
    if (input$modededeplacement == "pietons") {
      df_freq_p
    } else if (input$modededeplacement == "cycliste") {
      df_freq_c
    }
  })
  

  #Graphique 3:
  output$graph_quai <- renderPlot({
    ggplot(tab_quai, aes(x = hour_num)) +
    geom_line(aes(y = mode1, color = "Vélo"), linewidth = 1) +
    geom_point(aes(y = mode1, color = "Vélo"), size = 2) +
    geom_line(aes(y = mode2, color = "Voiture"), linewidth = 1) +
    geom_point(aes(y = mode2, color = "Voiture"), size = 2) +
    geom_line(aes(y = mode3, color = "Transports en commun"), linewidth = 1) +
    geom_point(aes(y = mode3, color = "Transports en commun"), size = 2) +
    scale_x_continuous(
      breaks = 0:23,
      labels = paste0(0:23, "h")) +
    scale_color_manual(values = c("Vélo" = "deeppink", "Voiture" = "steelblue", "Transports en commun" = "olivedrab2")) +
    labs(
      title = "Évolution pour chaque mode de transport au cours de la journée",
      x = "Heure",
      y = "Part (%)",
      color = "Mode") +
    theme_minimal(base_size = 12)
})
  
# Affichage de la carte
  
  output$map <- renderLeaflet({
    sh <- secteurs_hour()
    vals_ok <- sh$mode1[!is.na(sh$mode1)]
    pal <- colorNumeric("Blues", domain = vals_ok, na.color = "transparent")
    
    leaflet(sh) %>%
      addTiles() %>%
      setView(lng = 7.7521, lat = 48.5734, zoom = 12) %>% #(coordonnées de Strasbourg)
      addPolygons(
        fillColor   = ~pal(mode1),
        fillOpacity = 0.7,
        color       = "black",
        weight      = 1,
        popup       = ~paste0("Secteur : ", CODE_SEC, " • vélos : ", round(mode1, 1), " %"
        )) %>%
      addCircleMarkers(
        data = stations_sf,
        radius = 4,
        color = "deeppink",
        fillOpacity = 0.8,
        popup = ~paste0(na, " • capacité : ", to, " vélos")
      ) %>%
      addLegend(
        pal    = pal,
        values = vals_ok,
        title  = "Part du vélo (%)"
      )
  })
}


shinyApp(ui, server)

