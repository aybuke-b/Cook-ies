library(shiny)
library(arrow)
library(ggplot2)
library(tidyverse)
library(bslib)
library(DT)
library(shinydashboard)
library(shinythemes)
library(fontawesome)
library(bsicons)
library(textdata)
library(tidyverse)
library(tidytext)
library(Xplortext)
library(wordcloud)
library(gutenbergr)
library(FactoMineR)
library(janitor)
library(arrow)

path <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/")
#source(paste0(path, "server.R"))

df_comment <- read.csv("C:/Users/guill/OneDrive - Université de Tours/Bureau/M2/Shiny/data/comment_en.csv", sep = ",", header = TRUE, fileEncoding = "utf-8")
df <- read_parquet("C:/Users/guill/OneDrive - Université de Tours/Bureau/M2/Shiny/data/recette.parquet")

#df <- read_parquet("C:/Users/aybuk/Desktop/Cours M2/Big Data/Shiny/data/recette.parquet")
#df_comment <- read.csv("C:/Users/aybuk/Desktop/Cours M2/Big Data/Shiny/data/comment_en.csv", sep = ",", header = TRUE, fileEncoding = "utf-8")

df$temps <- round(df$temps,2)

title_css <- ".title { font-family: 'Satisfy', cursive; font-weight: bold; }"


custom_theme <- bs_theme(
  version = 5,
  primary = "#74736e",
  bg = "#FFFFFF",
  fg = "#74736e", #403f3b
  font_scale = 1.1,
  heading_font = font_google("Playfair Display"),
  base_font = font_google("Roboto")
)

test <- bs_add_rules(custom_theme, title_css)

color_bg <- "#2c4263"

ui <- page_navbar(
  theme = test,
    bg = alpha(color_bg, 0.9), #2c4263
  title = span(class = "title", img(src = "logo2.png", height = 90), "Cook'ies"),
  sidebar = sidebar(
    checkboxInput("select_all", "Tout sélectionner", value = FALSE),
    selectInput(
      "select_pays",
      "Pays",
      choices = unique(df$pays),
      multiple = TRUE,
      selected = c("Espagne", "Turquie", "Inde", "Bresil")
    ),
    sliderInput(
      "select_temps",
      "Temps",
      min = 0,
      value = 20,
      max = max(df$temps),
      step = 1
      
    ),
    selectInput(
      "select_niveau",
      "Niveau",
      choices = unique(df$niveau),
      multiple = TRUE,
      selected = "Facile"
    )
  ),
  nav_panel(
    title = "Accueil 🏠︎",
    card(
      card_header("Bienvenue sur Cook'ies 🍪"),
      markdown("**Cook'ies**, une application qui vous ouvre les portes des saveurs du monde entier 🌍 et vous aide à perfectionner vos talents culinaires en choisissant parmi les recettes les plus appréciés par les utilisateurs, vous permettant ainsi de devenir de véritables chefs cuisiniers 👩🏻‍🍳. Cette application innovante vous permet d'explorer des recettes notées à l'aide d'un système révolutionnaire de text mining , basé sur les commentaires des utilisateurs."),
      markdown("Vous trouverez différents onglets dont:"),
      markdown("- Recette 🍽 : Vous donne accès à une liste de recettes correspondant aux critères que vous aurez sélectionnés dans la sidebar. 😋 
               \n 
               - Détail Recette 🧾: Découvrez les secrets de chaque recette pour maîtriser la préparation de vos plats favoris. 🥧
               \n
               - Note ⭐ : Découvrez comment la note a été calculée en se basant sur les commentaires des utilisateurs."   ),
    ), 
    layout_columns(
      value_box(
        title = "Nombre total de recettes",
        value =  textOutput("nb_recette"),
        showcase = bs_icon("cookie"),
        theme = value_box_theme(bg = "#e6f2fd", fg = "#2b3254")
      ),
      value_box(
        title = "Nombre de pays",
        value = textOutput("nb_pays"),
        showcase = bs_icon("geo"),
        theme = value_box_theme(bg = "#e6f2fd", fg = "#0B538E"))
      ),
      layout_columns(
      value_box(
        title = "Coût moyen par recette",
        value = textOutput("cout_recette"), "€/pers",
        showcase = bs_icon("cash-stack"),
        theme = value_box_theme(bg = "#e6f2fd", fg = "#0B538E" )
      ),
      value_box(
        title = "Temps moyen par recette",
        value = textOutput("tps_recette"),
        showcase = bs_icon("clock-history"),
        theme = value_box_theme(bg = "#e6f2fd", fg = "#2b3254")
      )
    )
    ),
  nav_panel(
    title = "Recettes 🍽️",
    card(
      card_header("Les recettes"),
      gt_output("table_recette") 
    )
  ),
  nav_panel(
    title = "Détail recette 📋",
    card(
      #uiOutput("titre"),
      tags$head(
        tags$style(HTML("
            #select_recette + .selectize-control .selectize-input {
                font-size: 18px; /* Taille de police */
                font-weight: bold; /* Mise en gras du contenu */
                width: 400px; /* Largeur de la boîte de sélection */
            }
        "))
      ),
      fluidRow(
        column(width = 5,
          selectInput(
          "select_recette",
          "Sélectionnez une recette : ",
          choices = NULL,
          multiple = FALSE,
          selected = "Churros")),
        column(width = 6,
               uiOutput("img_recette"))
        ),
      fluidRow(
        column(width = 5,
          HTML("<h3>INGRÉDIENTS :</h3>"),
          uiOutput("details_ing")),
      column(width = 6,
          HTML("<h3>PRÉPARATION :</h3>"),
          uiOutput("details_recette")))
    )
    
  ),
  nav_panel(
    title = "Statistiques 📊",
    card(
      card_header("Les coûts"),
      plotlyOutput("plot_cout"), plotlyOutput("plot_pays")
    )
  ),
  nav_panel(
    title = "Carte 🗺",
      card(plotlyOutput("map_monde")),
      #card(plotlyOutput("map_monde_cout"))
  ),
  nav_panel(
    title = "Note ⭐",
    card(
      layout_columns(card(
        card_header("Comment est calculé la note ?"), 
        markdown("
La note est constituée de deux composantes principales. \n
- La première est un ratio qui évalue la proportion d'adjectifs positifs par rapport au nombre total d'adjectifs. \n
- La seconde composante est basée sur la méthode Afinn, qui attribue un score à chaque adjectif. Pour chaque recette, on calcule la somme de tous les scores des adjectifs, que l'on divise ensuite par le nombre de commentaires. \n
Ensuite, nous standardisons indépendamment les deux composantes en les centrant et les réduisant, en leur attribuant chacune un poids de un demi. Enfin, nous multiplions les deux valeurs pour obtenir une note comprise entre 0 et 5."
      )), card(plotOutput("plot_words"))),
      layout_columns(card(plotOutput("plot_by_note")), card(plotOutput("plot_note_pays")))
    )
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right"
  )
)

