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

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = span(tagList(icon("cookie"),"Cook'ies"))),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Consultation", tabName = "consultation", icon = icon("dashboard")),
      menuItem("Statistiques", tabName = "statistiques", icon = icon("th")),
      menuItem("Map", tabName = "map", icon = icon("user-md"))
    ),
    width = 300,
    selectInput("select_pays",
                "Pays",
                choices = unique(df$pays),
                multiple = FALSE,
                selected = "espagne"),
    sliderInput("select_temps",
                "Temps",
                min = 0,
                value = max(df$temps),
                max = max(df$temps),
                step = 0.25),
    selectInput("select_niveau",
                "Niveau",
                choices = unique(df$niveau),
                multiple = FALSE,
                selected = "Facile")
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
    tabItems(
      tabItem(tabName = "consultation",
              h2("Consultation Recette"),
              fluidRow(
                valueBoxOutput("nb_recette"),
                column(width = 12,
                       gt_output(outputId = "table_recette"))
              )
      ),
      tabItem(tabName = "statistiques",
              h2("Statistiques"),
              fluidRow(
                column(width = 12,
                       plotOutput(outputId = "plot_cout"))
              )
      ),
      tabItem(tabName = "map",
              h2("Map")
      )
    )
  )
)