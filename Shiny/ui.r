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

path <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/")
source(paste0(path, "server.R"))

df <- read_parquet("C:/Users/aybuk/Desktop/Cours M2/Big Data/Shiny/data/recette.parquet")

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


ui <- page_navbar(
  theme = test,
  bg = "#e7eef2",
  title = span(class = "title", img(src = "logo2.png", height = 90), "Cook'ies"),
  sidebar = sidebar(
    checkboxInput("select_all", "Tout cocher", value = FALSE),
    selectInput(
      "select_pays",
      "Pays",
      choices = unique(df$pays),
      multiple = TRUE,
      selected = "espagne"
    ),
    sliderInput(
      "select_temps",
      "Temps",
      min = 0,
      value = 20,
      max = max(df$temps),
      step = 5
      
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
      p("Test")
    ), 
    layout_columns(
      value_box(
        title = "Nombre total de recettes",
        value =  nrow(df),
        showcase = bs_icon("cookie"),
        theme = value_box_theme(bg = "#e6f2fd", fg = "#2b3254")
      ),
      value_box(
        title = "Nombre de pays",
        value = length(unique(df$pays)),
        showcase = bs_icon("geo"),
        theme = value_box_theme(bg = "#e6f2fd", fg = "#0B538E"))
      ),
      layout_columns(
      value_box(
        title = "Coût moyen par recette",
        value = round(mean(df$cout),3), "€/pers",
        showcase = bs_icon("cash-stack"),
        theme = value_box_theme(bg = "#e6f2fd", fg = "#0B538E" )
      ),
      value_box(
        title = "Temps moyen par recette",
        value = round(mean(df$temps),3),
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
    title = "Statistiques 📊",
    card(
      card_header("Les coûts"),
      plotlyOutput("plot_cout")
    )
  ),
  nav_panel(
    title = "Carte 🗺",
    card(
      card_header("Map"),
      plotlyOutput("map_monde")
    )
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right"
  )
)

