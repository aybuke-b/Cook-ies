library(shiny)
library(ggplot2)
library(tidyverse)
library(arrow)
library(gt)
library(shinydashboard)
library(plotly)
library(fontawesome)
library(bslib)

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


iso2 <- data.frame(
  pays = c(
    "Allemagne", "Autriche", "Belgique", "Bulgarie", "Crete", "Croatie", "Espagne", "Grece", "Hongrie",
    "Irlande", "Italie", "Lituanie", "Norvege", "Pologne", "Portugal", "Roumanie", "Royaume-Uni",
    "Suede", "Suisse", "Turquie", "Azerbaidjan", "Chine", "Coree du Sud", "Inde", "Israel", "Japon", "Laos",
    "Liban", "Russie", "Thailande", "Vietnam", "Etats-Unis", "Canada", "Cuba", "Mexique", "Argentine", "Bresil", "Chili", "Perou", "Venezuela", "Afghanistan", "Afrique du Sud", "Algerie",
    "Benin", "Cameroun", "Cote d'Ivoire", "Ile Maurice", "Maroc", "Senegal", "Tunisie", "Australie", "Indonesie",
    "Nouvelle Zelande", "Guadeloupe", "Guyane", "Martinique", "Nouvelle Caledonie", "Reunion", "Tahiti"
  ),
  iso_alpha2 = c(
    "DE", "AT", "BE", "BG", "GR", "HR", "ES", "GR", "HU", "IE", "IT", "LT", "NO", "PL", "PT", "RO", "GB",
    "SE", "CH", "TR", "AZ", "CN", "KP", "IN", "IL", "JP", "LA", "LB", "RU", "TH", "VN", "US", "CA", "CU", "MX", "AR", "BR", "CL", "PE", "VE", "AF", "ZA", "DZ", "BJ", "CM", "CI", "MU", "MA", "SN", "TN", "AU", "ID", "NZ", "GP", "GF", "MQ", "NC", "RE", "PF"
  )
)

get_flag_url <- function(country_name) {
  iso2$iso_alpha2[match(tolower(country_name), tolower(iso2$pays))]
}

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
    
    df_rec$flag <- sapply(df_rec$pays, get_flag_url)
    
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
      fmt_flag(columns = flag) |>
      cols_merge(
        columns = c(pays, flag),
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
    data <- merge(df, iso3, by = "pays", all.x = TRUE)
    
    df_mc <- data |> 
      group_by(iso_alpha3, pays) |> summarise(mean_cout = mean(cout))
    
    plot_ly(df_mc, type='choropleth',
            locations=df_mc$iso_alpha3,
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
  