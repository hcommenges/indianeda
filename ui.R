##############################
# Shiny App: indianeda
# User interface
##############################

shinyUI(fluidPage(
  theme = shinytheme("united"),
  titlePanel("indianeda - Exploratory Data Analysis of the Indian Census",
             tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
                       tags$title("indianeda - Exploratory Data Analysis of the Indian Census"))
  ),
  
  tabsetPanel(
    
    # Guide ----
    
    tabPanel("Guide d'utilisation", 
             fluidRow(
               column(2, wellPanel(
                 htmlOutput("citation")
               )),
               column(10, includeMarkdown("README_app.md")))),
    
    # Données ----
    
    tabPanel("Données",
             fluidRow(
               column(9, wellPanel(
                 selectInput("choosestate", label = "Choisir un état", choices = "", multiple = FALSE),
                 textInput("filterrow", label = 'Filtrer les observations (test conditionnel)', width = "50%"),
                 fluidRow(column(3,
                                 actionButton("addfilter", label = "Appliquer le filtre")),
                          column(3,
                                 actionButton("delfilter", label = "Supprimer le filtre")))
               )),
               column(9,
                      div(dataTableOutput("contentstable"), style = "overflow-x: auto;")
               )
             )
    ),
    
    # Univarié ----
    
    tabPanel("Univarié",
             fluidRow(
               column(3, wellPanel(
                 tags$h4("Choisir la variable à explorer"),
                 selectInput(inputId = "uniquanti", 
                             label = "Choisir une variable quanti", 
                             choices = "", 
                             selected = "", 
                             multiple = FALSE, 
                             selectize = TRUE),
                 selectInput(inputId = "uniquali", 
                             label = "Choisir une variable quali", 
                             choices = "", 
                             selected = "", 
                             multiple = FALSE, 
                             selectize = TRUE),
                 checkboxInput("uniset", "Personnaliser l'histogramme"),
                 conditionalPanel(condition = "input.uniset == true",
                                  sliderInput(inputId = "nbins", 
                                              label = "Nombre de classes", 
                                              min = 0, 
                                              max = 30, 
                                              value = 10,
                                              step = 1),
                                  checkboxInput("drawsummary", 
                                                label = "Tracer les résumés (Q1, Q2, Q3, Moyenne)", 
                                                value = FALSE)
                 ),
                 tags$br(),
                 tags$h4("Récupérer le graphique"),
                 numericInput(inputId = "widthuni", label = "Width (cm)", value = 20, min = 1, max = 30),
                 numericInput(inputId = "heightuni", label = "Height (cm)", value = 15, min = 1, max = 30),
                 downloadButton("downloaduniplot", "Télécharger")
               )),
               column(5, 
                      tags$h4("Résumé graphique"),
                      plotOutput("uniplot")),
               column(4, 
                      tags$h4("Résumé numérique"),
                      htmlOutput("unisummary"),
                      tableOutput("unitab"))
               
             )
    ),
    
    # Bivarié ----
    
    tabPanel("Bivarié",
             tabsetPanel(
               tabPanel("CONTINGENCE (quali-quali)",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            selectInput(inputId = "qualidep", 
                                        label = "Choisir la variable à expliquer", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = FALSE, 
                                        selectize = TRUE),
                            selectInput(inputId = "qualiindep", 
                                        label = "Choisir la variable explicative", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = FALSE, 
                                        selectize = TRUE),
                            radioButtons(inputId = "contcont", label = "Contenu du tableau",
                                         c("Effectifs observés" = "obsfreq",
                                           "Pourcentages en ligne" = "rowpct",
                                           "Effectifs espérés" = "expfreq",
                                           "Résidus bruts" = "rawresid",
                                           "Résidus standardisés" = "stdresid"),
                                         selected = "obsfreq"),
                            tags$br(),
                            tags$h4("Récupérer le graphique"),
                            numericInput(inputId = "widthmosaic", label = "Width (cm)", value = 20, min = 1, max = 30),
                            numericInput(inputId = "heightmosaic", label = "Height (cm)", value = 15, min = 1, max = 30),
                            downloadButton("downloadmosaicplot", "Télécharger")
                          )),
                          column(5,
                                 tags$h4("Résumé graphique"),
                                 plotOutput("mosaic")
                          ),
                          column(4, 
                                 tags$h4("Résumé numérique"),
                                 tags$h5(HTML("<strong>Mesures locales</strong>")),
                                 tableOutput("contingtab"),
                                 tags$h5(HTML("<strong>Mesures globales</strong>")),
                                 htmlOutput("contingtext")
                          )
                        )
               ),
               
               
               tabPanel("RÉGRESSION (quanti-quanti)",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            selectInput(inputId = "quantidep", 
                                        label = "Choisir la variable à expliquer", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = FALSE, 
                                        selectize = TRUE),
                            selectInput(inputId = "quantiindep", 
                                        label = "Choisir la variable explicative", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = FALSE, 
                                        selectize = TRUE),
                            checkboxInput("reg1save", "Enregistrer les résidus"),
                            conditionalPanel(condition = "input.reg1save == true",
                                             textInput(inputId = "reg1prefix", label = "Préfixe", value = ""),
                                             actionButton(inputId = "addreg1resid", label = "Ajouter les résidus")),
                            tags$br(),
                            tags$h4("Récupérer le graphique"),
                            numericInput(inputId = "widthreg1", label = "Width (cm)", value = 20, min = 1, max = 30),
                            numericInput(inputId = "heightreg1", label = "Height (cm)", value = 15, min = 1, max = 30),
                            downloadButton("downloadreg1", "Télécharger")
                          )),
                          column(5, 
                                 tags$h4("Résumé graphique"),
                                 plotOutput("scatterplot")),
                          column(4,
                                 tags$h4("Résumé numérique"),
                                 tableOutput("coefreg"))
                        )
               ),
               
               tabPanel("ANOVA (quali-quanti)",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            selectInput(inputId = "quanlidep", 
                                        label = "Choisir la variable à expliquer (quanti)", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = FALSE, 
                                        selectize = TRUE),
                            selectInput(inputId = "quanliindep", 
                                        label = "Choisir la variable explicative (quali)", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = FALSE, 
                                        selectize = TRUE),
                            checkboxInput(inputId = "bpjitter", label = "Surimposer les points", value = FALSE),
                            checkboxInput("aov1save", "Enregistrer les résidus"),
                            conditionalPanel(condition = "input.aov1save == true",
                                             textInput(inputId = "aov1prefix", label = "Préfixe", value = ""),
                                             actionButton(inputId = "addaov1resid", label = "Ajouter les résidus")),
                            tags$br(),
                            tags$h4("Récupérer le graphique"),
                            numericInput(inputId = "widthanova1", label = "Width (cm)", value = 20, min = 1, max = 30),
                            numericInput(inputId = "heightanova1", label = "Height (cm)", value = 15, min = 1, max = 30),
                            downloadButton("downloadanova", "Télécharger")
                          )),
                          column(5, 
                                 tags$h4("Résumé graphique"),
                                 tags$h5("Écarts à la moyenne"),
                                 plotOutput("aovplot"),
                                 tags$h5("Boîtes à moustaches"),
                                 plotOutput("boxes")),
                          column(4,
                                 tags$h4("Résumé numérique"),
                                 tableOutput("coefanova"),
                                 tableOutput("tabanova")
                          )
                        )
               )
             )
    ),
    
    # Trivarié ----
    
    tabPanel("Trivarié",
             tabsetPanel(
               tabPanel("ANOVA (2 quali)",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            selectInput(inputId = "aovdep",
                                        label = "Choisir la variable à expliquer (quanti)", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = FALSE, 
                                        selectize = TRUE),
                            selectInput(inputId = "aovindep",
                                        label = "Choisir deux variables explicatives (quali)", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = TRUE, 
                                        selectize = TRUE),
                            checkboxInput(inputId = "interactaov2", 
                                          label = "Interaction entre les variables explicatives",
                                          value = FALSE),
                            checkboxInput("aov2save", "Enregistrer les résidus"),
                            conditionalPanel(condition = "input.aov2save == true",
                                             textInput(inputId = "aov2prefix", label = "Préfixe", value = ""),
                                             actionButton(inputId = "addaov2resid", label = "Ajouter les résidus")),
                            tags$br(),
                            tags$h4("Récupérer le graphique"),
                            numericInput(inputId = "widthanova2", label = "Width (cm)", value = 20, min = 1, max = 30),
                            numericInput(inputId = "heightanova2", label = "Height (cm)", value = 15, min = 1, max = 30),
                            downloadButton("downloadanova2", "Télécharger")
                          )),
                          column(5,
                                 tags$h4("Résumé graphique"),
                                 tags$h5("Écarts à la moyenne"),
                                 plotOutput("anovaplot2"),
                                 tags$h5("Boîtes à moustaches"),
                                 plotOutput("boxes2")
                          ),
                          column(4, 
                                 tags$h4("Résumé numérique"),
                                 tableOutput("coefanova2"),
                                 tableOutput("tabanova2"),
                                 tableOutput("tabanova2interact")
                          )
                        )
               ),
               
               tabPanel("ANCOVA (1 quanti / 1 quali)",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            selectInput(inputId = "ancovdep", 
                                        label = "Choisir la variable à expliquer (quanti)", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = FALSE, 
                                        selectize = TRUE),
                            selectInput(inputId = "ancovindep", 
                                        label = "Choisir deux variables explicatives (1 quanti puis 1 quali)", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = TRUE, 
                                        selectize = TRUE),
                            checkboxInput(inputId = "interactancov", 
                                          label = "Interaction entre les variables explicatives",
                                          value = FALSE),
                            checkboxInput("reg2save", "Enregistrer les résidus"),
                            conditionalPanel(condition = "input.reg2save == true",
                                             textInput(inputId = "reg2prefix", label = "Préfixe", value = ""),
                                             actionButton(inputId = "addreg2resid", label = "Ajouter les résidus")),
                            tags$br(),
                            tags$h4("Récupérer le graphique"),
                            numericInput(inputId = "widthancova", label = "Width (cm)", value = 20, min = 1, max = 30),
                            numericInput(inputId = "heightancova", label = "Height (cm)", value = 15, min = 1, max = 30),
                            downloadButton("downloadancova", "Télécharger")
                          )
                          ),
                          column(5,
                                 tags$h4("Résumé graphique"),
                                 plotOutput("scatterancov")
                          ),
                          column(4, 
                                 tags$h4("Résumé numérique"),
                                 tableOutput("coefancov")
                          )
                        )
               ),
               
               tabPanel("RÉGRESSION (2 quanti)",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            selectInput(inputId = "quanti2dep", 
                                        label = "Choisir la variable à expliquer (quanti)", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = FALSE, 
                                        selectize = TRUE),
                            selectInput(inputId = "quanti2indep", 
                                        label = "Choisir deux variables explicatives (quanti)", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = TRUE, 
                                        selectize = TRUE),
                            checkboxInput(inputId = "interactreg", 
                                          label = "Interaction entre les variables explicatives",
                                          value = FALSE),
                            checkboxInput("reg3save", "Enregistrer les résidus"),
                            conditionalPanel(condition = "input.reg3save == true",
                                             textInput(inputId = "reg3prefix", label = "Préfixe", value = ""),
                                             actionButton(inputId = "addreg3resid", label = "Ajouter les résidus")),
                            tags$br(),
                            tags$h4("Récupérer le graphique"),
                            numericInput(inputId = "widthreg2", label = "Width (cm)", value = 20, min = 1, max = 30),
                            numericInput(inputId = "heightreg2", label = "Height (cm)", value = 15, min = 1, max = 30),
                            downloadButton("downloadreg2", "Télécharger")
                          )),
                          column(5,
                                 tags$h4("Résumé graphique"),
                                 plotOutput("scatterreg2")
                          ),
                          column(4, 
                                 tags$h4("Résumé numérique"),
                                 tableOutput("coefreg2")
                          )
                        )
               )
             )
    ),
    
    # Multivarié ----
    
    tabPanel("Multivarié",
             tabsetPanel(
               tabPanel("RÉGRESSION",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            selectInput(inputId = "regmultdep", 
                                        label = "Choisir une variable à expliquer (quanti)", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = FALSE, 
                                        selectize = TRUE),
                            selectInput(inputId = "regmultindep", 
                                        label = "Choisir plusieurs variables explicatives (quanti)", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = TRUE, 
                                        selectize = TRUE),
                            checkboxInput("reg4save", "Enregistrer les résidus"),
                            conditionalPanel(condition = "input.reg4save == true",
                                             textInput(inputId = "reg4prefix", label = "Préfixe", value = ""),
                                             actionButton(inputId = "addreg4resid", label = "Ajouter les résidus")
                            )
                          )),
                          column(5,
                                 tags$h4("Matrice de corrélation"),
                                 div(tableOutput("matcor"), style = "overflow-x: auto;")
                          ),
                          column(4, 
                                 tags$h4("Résumé numérique du modèle"),
                                 div(tableOutput("coefregmult"), style = "overflow-x: auto;")
                          )
                        )
               ),
               
               tabPanel("ANALYSE FACTORIELLE",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            selectInput(inputId = "factovar", 
                                        label = "Choisir plusieurs variables quantitatives", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = TRUE, 
                                        selectize = TRUE),
                            actionButton("buttonpca", "Calculer l'ACP"),
                            selectInput("xaxis", label = "Axe des abscisses (x)", choices = 1:4, selected = 1, multiple = FALSE, selectize = TRUE),
                            selectInput("yaxis", label = "Axe des ordonnées (y)", choices = 1:4, selected = 2, multiple = FALSE, selectize = TRUE))
                          ),
                          column(9,
                                 tags$h4("Matrice de corrélation"),
                                 div(tableOutput("facmatcor"), style = "overflow-x: auto;"))
                        ),
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Récupérer le graphique"),
                            numericInput(inputId = "widthpca", label = "Width (cm)", value = 20, min = 1, max = 30),
                            numericInput(inputId = "heightpca", label = "Height (cm)", value = 25, min = 1, max = 30),
                            downloadButton("downloadpca", "Télécharger"))),
                          column(4,
                                 tags$h4("Cercle des corrélations"),
                                 plotOutput("corcircle")),
                          column(5,
                                 tags$h4("Décomposition de l'inertie"),
                                 plotOutput("compinert")
                          )
                        ),
                        fluidRow(
                          column(3, wellPanel(checkboxInput("labelindiv", "Etiqueter les individus", value = FALSE),
                                              checkboxInput("facsave", "Enregistrer les coordonnées"),
                                              conditionalPanel(condition = "input.facsave == true",
                                                               textInput(inputId = "facprefix", label = "Préfixe", value = ""),
                                                               actionButton(inputId = "addfaccoord", label = "Ajouter les coordonnées factorielles"))
                          )),
                          column(9,
                                 tags$h4("Coordonnées des individus"),
                                 plotOutput("indivpca"))
                        ),
                        fluidRow(
                          column(3, wellPanel()),
                          column(4,
                                 tags$h4("Contribution des variables (somme = 1000)"),
                                 tableOutput("contribvar")),
                          column(5,
                                 tags$h4("Contribution des individus (somme = 1000)"),
                                 dataTableOutput("contribind")
                          )
                        )
               ),
               tabPanel("CLASSIFICATION",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            selectInput(inputId = "cahvar", 
                                        label = "Choisir plusieurs variables quantitatives", 
                                        choices = "", 
                                        selected = "", 
                                        multiple = TRUE, 
                                        selectize = TRUE),
                            checkboxInput("cahstandardize", label = "Standardiser les variables", value = FALSE),
                            selectInput("cahmethod", label = "Choisir un critère d'aggrégation", choices = c("Minimum" = "single",
                                                                                                             "Maximum" = "complete",
                                                                                                             "Moyenne" = "average",
                                                                                                             "Ward" = "ward"), 
                                        selected = "average", multiple = FALSE, selectize = TRUE),
                            actionButton(inputId = "buttoncah", label = "Calculer la CAH"),
                            tags$hr(),
                            sliderInput("cahnclass", label = "Choisir le nombre de classes", min = 2, max = 12, step = 1, value = 4),
                            checkboxInput("cahsave", "Enregistrer les classes"),
                            conditionalPanel(condition = "input.cahsave == true",
                                             textInput(inputId = "cahprefix", label = "Préfixe", value = ""),
                                             actionButton(inputId = "addcahclass", label = "Ajouter les classes"))
                          )),
                          column(5,
                                 tags$h4("Dendrogramme"),
                                 plotOutput("cahdendro")),
                          column(4, 
                                 tags$h4("Niveaux"),
                                 plotOutput("cahheight"))),
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Récupérer le graphique"),
                            numericInput(inputId = "widthclus", label = "Width (cm)", value = 20, min = 1, max = 30),
                            numericInput(inputId = "heightclus", label = "Height (cm)", value = 30, min = 1, max = 30),
                            downloadButton("downloadclus", "Télécharger")
                          )),
                          column(9, 
                                 tags$h4("Profil des observations"),
                                 plotOutput("cahprofile")))
               )
             )
    ),
    
    # Cartographie ----
    
    tabPanel("Cartographie", 
             fluidRow(
               column(3, wellPanel(
                 selectInput("colpal", label = "Type de palette", choices = c("Quantitative" = "quanti", "Qualitative" = "quali"), selected = "quanti"),
                 selectInput(inputId = "cartovar", 
                             label = "Choisir la variable", 
                             choices = "", 
                             selected = "", 
                             multiple = FALSE, 
                             selectize = TRUE),
                 selectInput("colcol", label = "Choix de couleur", choices = c("Bleu" = "blue.pal", 
                                                                               "Rouge" = "red.pal", 
                                                                               "Vert" = "green.pal",
                                                                               "Gris" = "grey.pal",
                                                                               "Vin" = "wine.pal",
                                                                               "Violet" = "purple.pal",
                                                                               "Sable" = "sand.pal",
                                                                               "Pastel (quali)" = "pastel.pal",
                                                                               "Multi (quali)" = "multi.pal")),
                 selectInput("cartomethod", label = "Méthode de discrétisation", choices = c("Amplitude égale" = "equal", 
                                                                                             "Quantiles" = "quantile", 
                                                                                             "Seuils naturels" = "fisher-jenks"), 
                             selected = NULL, multiple = FALSE, selectize = TRUE),
                 sliderInput("cartoclass", label = "Nombre de classes", step = 1, min = 1, max = 8, value = 4),
                 tags$h4("Récupérer le graphique"),
                 numericInput(inputId = "widthcarto", label = "Width (cm)", value = 20, min = 1, max = 30),
                 numericInput(inputId = "heightcarto", label = "Height (cm)", value = 30, min = 1, max = 30),
                 downloadButton("downloadcarto", "Télécharger")
               )),
               column(9, 
                      plotOutput("carto", height = 700))
             )
    ),
    
    tabPanel("Exploration spatiale", 
             fluidRow(
               column(3, wellPanel(
                 selectInput("expcolpal", label = "Type de palette", choices = c("Quantitative" = "quanti", "Qualitative" = "quali"), selected = "quanti"),
                 selectInput(inputId = "expcartovar", 
                             label = "Choisir la variable", 
                             choices = "", 
                             selected = "", 
                             multiple = FALSE, 
                             selectize = TRUE),
                 selectInput("expcolcol", label = "Choix de couleur", choices = c("Bleu" = "Blues", 
                                                                                  "Rouge" = "Reds", 
                                                                                  "Vert" = "Greens",
                                                                                  "Violet" = "Purples",
                                                                                  "Pastel (quali)" = "Pastel1",
                                                                                  "Multi (quali)" = "Set1")),
                 sliderInput("expcartoclass", label = "Nombre de classes", step = 1, min = 1, max = 8, value = 4),
                 sliderInput("expopac", label = "Transparence", step = 0.1, min = 0, max = 1, value = 0.6)
               )),
               column(9, 
                      leafletOutput("expcarto", height = "700px"))
               )
             )
    )
)
)
