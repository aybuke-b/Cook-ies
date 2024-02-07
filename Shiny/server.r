library(shiny)
library(ggplot2)
library(tidyverse)
library(arrow)
library(gt)
library(shinydashboard)
library(plotly)

server <- function(input, output) {
#----------------------------PLOT----------------------------#
    output$plot_cout <- renderPlotly({
        df_plot <- df |> 
            filter(pays %in% input$select_pays) |> 
            filter(niveau %in% input$select_niveau) |> 
            filter(temps < input$select_temps) 
          
            plot_ly(x = df_plot$cout, type = "histogram")

    })
#----------------------------TABLE----------------------------# 
  output$table_recette <- render_gt({
    df_rec <- df[,c("img", "nom","pays", "niveau", "temps", "cout")] |> 
      filter(pays %in% input$select_pays) |> 
      filter(niveau %in% input$select_niveau) |> 
      filter(temps < input$select_temps)
    
    df_rec |> 
        gt() |> 
          opt_interactive(use_compact_mode = TRUE) |> 
          text_transform(
            locations = cells_body(columns = img),
            fn = function(x){
              web_image(
                url = df_rec$img,
                height = px(50)
                )
              }
            ) |> 
          tab_header("Recettes")
  })
#----------------------------TABLE----------------------------#  
  output$nb_recette <- renderValueBox({
    n_row <- df[,c("nom","pays", "niveau", "temps", "cout", "img")] |> 
      filter(pays == input$select_pays) %>%
      filter(niveau == input$select_niveau) %>%
      filter(temps < input$select_temps) |> nrow()
    valueBox(
      n_row, "Nombre de recette", icon = icon("cookie"), color = "yellow"
    )
  })
}
