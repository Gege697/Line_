#
# Application Shiny pour la Collecte de Données en Génie Civil
#
# Cette application simule un formulaire de collecte de données pour des projets
# de construction, permettant à l'utilisateur de saisir des informations
# sur les matériaux, les charges et la localisation.
# Les données sont stockées temporairement dans la session de l'application
# et affichées en temps réel.

# --- Chargement des bibliothèques ---
library(shiny)
library(ggplot2)
library(dplyr)
library(DT) # Pour un meilleur affichage de la table de données

# --- Définition de l'Interface Utilisateur (UI) ---
ui <- fluidPage(
    # Titre de l'application
    titlePanel(
        h1("Système de Collecte de Données de Projet en Génie Civil", 
           style = "color: #2c3e50; font-weight: bold;")
    ),
    
    # Mise en page avec une barre latérale pour les entrées
    sidebarLayout(
        # Barre latérale (Inputs)
        sidebarPanel(
            width = 4,
            h3("Saisie des Données de Projet", style = "color: #3498db;"),
            
            # Entrée 1: Nom du Projet
            textInput("nom_projet", 
                      label = strong("Nom du Projet ou de l'Échantillon"), 
                      value = paste0("P-", floor(runif(1, 1000, 9999))),
                      placeholder = "Ex: Pont-Nord-01"),
            
            # Entrée 2: Type de Matériau
            selectizeInput("materiau", 
                           label = strong("Type de Matériau Principal"),
                           choices = c("Béton Armé", "Acier de Construction", 
                                       "Bois Lamellé-Collé", "Maçonnerie", "Autre"),
                           selected = "Béton Armé"),
            
            # Entrée 3: Résistance du Béton (Conditionnel)
            # Cette entrée n'est pertinente que si 'Béton Armé' est sélectionné
            numericInput("resistance_concrete", 
                         label = strong("Résistance Caractéristique du Béton (MPa)"),
                         value = 30, min = 15, max = 100, step = 1),
            
            # Entrée 4: Charge Maximale
            numericInput("charge_max", 
                         label = strong("Charge Maximale Appliquée (kN)"),
                         value = 500, min = 0, step = 10),
            
            # Entrée 5 & 6: Coordonnées Géographiques (Simulation de données de localisation)
            fluidRow(
                column(6, numericInput("coord_x", label = strong("Coordonnée X (Latitude)"), 
                                       value = 48.85, step = 0.01)),
                column(6, numericInput("coord_y", label = strong("Coordonnée Y (Longitude)"), 
                                       value = 2.35, step = 0.01))
            ),
            
            # Bouton pour collecter/soumettre les données
            actionButton("submit_data", 
                         label = "Collecter les Données", 
                         icon = icon("database"),
                         class = "btn-primary w-100 mt-4"),
            
            br(),
            
            # Afficher le nombre d'entrées
            div(textOutput("nombre_entrees"), class = "mt-3 p-2 bg-light rounded")
        ),
        
        # Panneau Principal (Outputs)
        mainPanel(
            width = 8,
            tabsetPanel(
                # Onglet 1: Table des Données
                tabPanel(
                    title = "Données Collectées Brutes",
                    icon = icon("table"),
                    h3("Historique des Entrées", style = "color: #34495e;"),
                    # Utilisation de DT pour une table interactive
                    DTOutput("data_table") 
                ),
                
                # Onglet 2: Visualisation
                tabPanel(
                    title = "Analyse Visuelle",
                    icon = icon("chart-bar"),
                    h3("Visualisation: Charge vs. Résistance", style = "color: #34495e;"),
                    plotOutput("data_plot")
                )
            )
        )
    )
)

# --- Définition de la Logique Serveur (Server) ---
server <- function(input, output, session) {
    
    # Initialisation d'un data.frame réactif pour stocker les données
    # Cette approche simule une base de données en mémoire.
    reactive_data <- reactiveVal(
        data.frame(
            ID = numeric(0),
            Nom_Projet = character(0),
            Matériau = character(0),
            Résistance_MPa = numeric(0),
            Charge_kN = numeric(0),
            Latitude = numeric(0),
            Longitude = numeric(0),
            stringsAsFactors = FALSE
        )
    )
    
    # Obtenir le User ID (simulation d'un ID de session)
    user_id <- reactiveVal(paste0("User-", floor(runif(1, 100, 999))))
    
    # Message d'affichage du nombre d'entrées
    output$nombre_entrees <- renderText({
        data <- reactive_data()
        paste0("ID de Session: ", user_id(), " | Nombre d'entrées : ", nrow(data))
    })
    
    # Gérer l'événement du bouton 'Collecter les Données'
    observeEvent(input$submit_data, {
        # 1. Créer la nouvelle ligne de données
        new_row <- data.frame(
            ID = nrow(reactive_data()) + 1,
            Nom_Projet = input$nom_projet,
            Matériau = input$materiau,
            Résistance_MPa = input$resistance_concrete,
            Charge_kN = input$charge_max,
            Latitude = input$coord_x,
            Longitude = input$coord_y,
            stringsAsFactors = FALSE
        )
        
        # 2. Mettre à jour le data.frame réactif
        current_data <- reactive_data()
        updated_data <- bind_rows(current_data, new_row)
        reactive_data(updated_data)
        
        # 3. Afficher une notification de succès
        showNotification(
            paste("Données pour le projet '", input$nom_projet, "' collectées avec succès."),
            type = "message",
            duration = 5
        )
        
        # 4. (Optionnel) Réinitialiser les champs de saisie pour la prochaine entrée
        updateTextInput(session, "nom_projet", 
                        value = paste0("P-", floor(runif(1, 1000, 9999))))
    })
    
    # --- Sortie 1: Table des Données ---
    output$data_table <- renderDT({
        data <- reactive_data()
        
        # Renommer les colonnes pour l'affichage
        colnames(data) <- c("ID", "Nom du Projet", "Matériau", 
                            "Résistance (MPa)", "Charge (kN)", "Latitude", "Longitude")
        
        # Configuration de l'affichage de la table
        datatable(
            data,
            options = list(
                pageLength = 10,
                lengthMenu = c(5, 10, 20),
                scrollX = TRUE,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/French.json')
            ),
            rownames = FALSE
        )
    })
    
    # --- Sortie 2: Visualisation ---
    output$data_plot <- renderPlot({
        data <- reactive_data()
        
        # Assurez-vous qu'il y a des données et qu'elles sont numériques
        if (nrow(data) > 0) {
            
            # Filtrer pour les matériaux ayant une résistance pertinente à tracer (comme le Béton)
            plot_data <- data %>%
                filter(Matériau == "Béton Armé" | Matériau == "Acier de Construction") %>%
                mutate(
                    # Utiliser la résistance du béton pour le béton, et simuler une valeur pour l'acier
                    Résistance_Effective = case_when(
                        Matériau == "Béton Armé" ~ Résistance_MPa,
                        # Simuler une haute "résistance" pour l'acier pour la différenciation
                        Matériau == "Acier de Construction" ~ 60, 
                        TRUE ~ Résistance_MPa # Fallback
                    )
                )

            # Créer le graphique de dispersion
            ggplot(plot_data, aes(x = Résistance_Effective, y = Charge_kN, color = Matériau)) +
                geom_point(size = 4, alpha = 0.7) +
                geom_smooth(method = "lm", se = FALSE) +
                labs(
                    title = "Relation entre la Résistance des Matériaux et la Charge Maximale",
                    x = "Résistance du Matériau (MPa ou Équivalent)",
                    y = "Charge Maximale (kN)",
                    color = "Type de Matériau"
                ) +
                theme_minimal(base_size = 14) +
                theme(
                    plot.title = element_text(hjust = 0.5, face = "bold"),
                    legend.position = "bottom"
                )
        } else {
            # Afficher un message si aucune donnée n'est disponible
            ggplot() + 
                geom_text(aes(x = 0, y = 0, label = "Veuillez entrer des données pour visualiser le graphique.")) +
                theme_void()
        }
    })
}

# --- Lancer l'Application Shiny ---
shinyApp(ui = ui, server = server)
