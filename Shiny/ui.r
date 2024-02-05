library(shiny)
library(arrow)
library(ggplot2)
library(tidyverse)
library(bslib)
library(DT)
library(shinydashboard)
library(shinythemes)
library(shiny)
library(shinythemes)

path <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/")
source(paste0(path, "server.R"))

#df <- read_parquet("C:/Users/guill/OneDrive - Université de Tours/Bureau/M2/Shiny/data/recette.parquet")
df <- read_parquet("C:/Users/aybuk/Desktop/Cours M2/Big Data/Shiny/data/recette.parquet")
df$temps <- round(df$temps,2)


custom_theme <- bs_theme(bootswatch = "spacelab")


ui <- page_navbar(
  theme = custom_theme,
  title = HTML(paste0('<img src="', "C:/Users/aybuk/Desktop/Cours M2/Big Data/Shiny/Shiny/chokbarjpg", '" height="30" width="30" style="margin-right:10px;">', "Cook'ies")),
  sidebar = sidebar(
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
    title = "Consultation",
    card(
      card_header("Les recettes"),
      gt_output("table_recette")
    )
  ),
  nav_panel(
    title = "Statistiques",
    card(
      card_header("Les coûts"),
      plotOutput("plot_cout")
    )
  ),
  nav_panel(
    title = "Map",
    p("Third tab content")
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right"
  )
)

