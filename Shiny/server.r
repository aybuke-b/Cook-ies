library(shiny)
library(ggplot2)
library(tidyverse)
library(arrow)
library(gt)
library(shinydashboard)
library(plotly)

iso3 <- data.frame(
  pays = c(
    "allemagne", "autriche", "belgique", "bulgarie", "crete", "croatie", "espagne", "grece", "hongrie",
    "irlandaises", "italie", "lituanie", "norvegiennes", "pologne", "portugal", "roumanie", "royaume-uni",
    "suede", "suisse", "turquie", "azerbaidjan", "chine", "coreennes", "inde", "israel", "japon", "laos",
    "libanaises", "russie", "thailande", "vietnamiennes", "etats-unis", "canada", "caraibes", "cubaines",
    "mexique", "argentine", "bresil", "chili", "perou", "venezuela", "africaines", "afrique-du-sud", "algerie",
    "benin", "cameroun", "cote-ivoire", "ile-maurice", "maroc", "senegal", "tunisie", "australie", "indonesiennes",
    "nouvelle-zelande", "guadeloupe", "guyane", "martinique", "nouvelle-caledonie", "reunion", "tahiti"
  ),
  iso_alpha3 = c(
    "DEU", "AUT", "BEL", "BGR", "GRC", "HRV", "ESP", "GRC", "HUN", "IRL", "ITA", "LTU", "NOR", "POL", "PRT",
    "ROU", "GBR", "SWE", "CHE", "TUR", "AZE", "CHN", "KOR", "IND", "ISR", "JPN", "LAO", "LBN", "RUS", "THA",
    "VNM", "USA", "CAN", "CUB", "CUB", "MEX", "ARG", "BRA", "CHL", "PER", "VEN", "AFG", "ZAF", "DZA", "BEN",
    "CMR", "CIV", "MUS", "MAR", "SEN", "TUN", "AUS", "IDN", "NZL", "GLP", "GUF", "MTQ", "NCL", "REU", "PYF"
  )
)

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
      df_rec <- df[,c("img", "nom","pays", "niveau", "temps", "cout")],
      df_rec <- df[,c("img", "nom","pays", "niveau", "temps", "cout")] |> 
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
          tab_header("Recettes")
  })
#----------------------------TABLE----------------------------#  
  output$map_monde <- renderPlotly({
    data <- merge(df, iso3, by = "pays", all.x = TRUE)
    
    df_mc <- data |> 
      group_by(iso_alpha3, pays) |> summarise(mean_cout = mean(cout))
    
    plot_ly(df_mc, type='choropleth',
            locations=df_mc$iso_alpha3,
            z=df_mc$mean_cout,
            text=df_mc$pays,
            colorscale="Blues")
  })
  
}

