library(shiny)
library(ggplot2)
library(tidyverse)
library(arrow)
library(gt)
library(shinydashboard)
library(plotly)
library(fontawesome)
library(bslib)


server <- function(input, output) {
#----------------------------PLOT----------------------------#
    output$plot_cout <- renderPlotly({
      ifelse(input$select_all,
        df_plot <- df,
        df_plot <- df |> 
            filter(pays %in% input$select_pays) |> 
            filter(niveau %in% input$select_niveau) |> 
            filter(temps < input$select_temps))
          
            plot_ly(x = df_plot$cout, type = "histogram")

    })
    
#----------------------------TABLE----------------------------# 
  output$table_recette <- render_gt({
    ifelse(input$select_all,
      df_rec <- df[,c("img", "nom","pays", "niveau", "temps", "cout","ISO2")],
      df_rec <- df[,c("img", "nom","pays", "niveau", "temps", "cout","ISO2")] |> 
        filter(pays %in% input$select_pays) |> 
        filter(niveau %in% input$select_niveau) |> 
        filter(temps < input$select_temps))
    
    
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
      fmt_integer() |>
      fmt_flag(columns = ISO2) |>
      cols_merge(
        columns = c(pays, ISO2),
        pattern = "{2} {1}"
      ) |>
          tab_header("Recettes 🥣") |>
          cols_label(
            img = html(fontawesome::fa("camera-retro"),"Image"),
            nom = html(fontawesome::fa("utensils"),"Nom"),
            pays = html(fontawesome::fa("globe"),"Pays"),
            niveau = html(fontawesome::fa("layer-group"),"Niveau"),
            temps = html(fontawesome::fa("clock"),"Temps"),
            cout = html(fontawesome::fa("sack-dollar"),"Coût/pers")) 
  })
#----------------------------TABLE----------------------------#  
  output$map_monde <- renderPlotly({

    
    df_mc <- df |> 
      group_by(ISO3, pays) |> summarise(mean_cout = mean(cout))
    
    plot_ly(df_mc, type='choropleth',
            locations=df_mc$ISO3,
            z=df_mc$mean_cout,
            text=df_mc$pays,
            colorscale="Blues")
  })
  

#----------------------------VALUE BOX----------------------------# 

  output$nb_recette <- renderText({
    ifelse(input$select_all,
    df |> nrow() ,
    nb <- df |> 
      filter(pays %in% input$select_pays) |> 
      filter(niveau %in% input$select_niveau) |> 
      filter(temps < input$select_temps) |> 
      nrow())  
  })
  
  output$nb_pays <- renderText({
    ifelse(input$select_all,
    nb <- df,
    nb <- df |> 
      filter(pays %in% input$select_pays) |> 
      filter(niveau %in% input$select_niveau) |> 
      filter(temps < input$select_temps)) 
    
    length(unique(nb$pays))
  })
  
  output$cout_recette <- renderText({
    ifelse(input$select_all,
    nb <- df,
    nb <- df |> 
      filter(pays %in% input$select_pays) |> 
      filter(niveau %in% input$select_niveau) |> 
      filter(temps < input$select_temps))  
    
    round(mean(nb$cout),3)
  })
  
  output$tps_recette <- renderText({
    ifelse(input$select_all,
    nb <- df,
    nb <- df |> 
      filter(pays %in% input$select_pays) |> 
      filter(niveau %in% input$select_niveau) |> 
      filter(temps < input$select_temps))  
    
    round(mean(nb$temps),3)
  })
}
  
