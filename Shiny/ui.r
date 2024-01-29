library(shiny)
library(arrow)
library(ggplot2)
library(tidyverse)
library(bslib)

df <- read_parquet("data/recette.parquet")

ui <- fluidPage(
  titlePanel("Recette"),
    sidebarLayout(
        sidebarPanel(
            selectInput("select_pays",
                    "Pays",
                    choices = unique(df$pays),
                    multiple = TRUE,
                    selected = "espagne"),
            sliderInput("select_temps",
                    "Temps",
                    min = 0,
                    value = 20,
                    max = max(df$temps),
                    step = 5),
            selectInput("select_niveau",
                    "Niveau",
                    choices = unique(df$niveau),
                    multiple = TRUE,
                    selected = "Facile")
            ),
    mainPanel(
        tabsetPanel(
            type = "tabs",
            tabPanel("Consultation Recette", plotOutput("plot_cout")),
            tabPanel("Carte"),
            tabPanel("Statistiques")
            )
        )
    )
)

shinyApp(ui = ui, server = server)
