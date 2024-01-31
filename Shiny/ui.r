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

df <- read_parquet("C:/Users/guill/OneDrive - Université de Tours/Bureau/M2/Shiny/data/recette.parquet")
df$temps <- round(df$temps,2)

ui <- page_navbar(
    theme = bs_theme(bootswatch = "minty"),
    title = "Cook'ies",
    bg = "#0062cc",
    underline = TRUE,
    nav_panel(title = "Consultation",
              card(
                card_header("Les recettes"),
                gt_output("table_recette")
              )),
    nav_panel(title = "Statistiques",
              card(
                card_header("Les coûts"),
                plotOutput("plot_cout")
                )
              ),
    nav_panel(title = "Map", p("Third tab content")),
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right"
    ),
    sidebar = sidebar(
      
    ),
  
  )
