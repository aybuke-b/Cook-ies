library(shiny)
library(ggplot2)
library(tidyverse)
library(arrow)
library(gt)
library(shinydashboard)
library(plotly)
library(fontawesome)
library(bslib)
library(gtExtras)

score_afinn <- df_comment |> 
  select("comment_en", "nom", "pays") |>
  unnest_tokens(word,comment_en) |> 
  inner_join(get_sentiments("afinn")) |> 
  group_by(nom, pays) |> 
  mutate(nb_comment = n()) |> 
  group_by(nom, nb_comment, pays) |>
  summarise(sentiment = sum(value)) |> 
  mutate(note_afinn = sentiment/nb_comment)

score_bing <- df_comment |> select("comment_en", "nom", "pays") |>
  unnest_tokens(word,comment_en) |> 
  inner_join(get_sentiments("bing")) |> 
  group_by(nom, pays) |> 
  summarise(
    positive = sum(sentiment == "positive"),
    negative = sum(sentiment == "negative")) |> 
  mutate(note_bing = positive / (negative + positive)) 

max_bing <- max(score_bing$note_bing)
min_bing <- min(score_bing$note_bing)

max_afinn <- max(score_afinn$note_afinn)
min_afinn <- min(score_afinn$note_afinn)

score <- merge(score_afinn, score_bing, on = "nom") |> 
  select("nom", "pays","nb_comment", "note_afinn", "note_bing") |> 
  mutate(note = 5*((note_afinn-min_afinn)/(max_afinn - min_afinn) + (note_bing-min_bing)/(max_bing - min_bing))/2) |> 
  filter(nb_comment > 5)

score_pays <- score |> 
  group_by(pays) |> 
  summarise(note = mean(note),
            nb_comment = sum(nb_comment),
            nb_recette = n())

#df_merge <- merge(df, score, by = "nom", all = TRUE, suffixes = c("",".y"))
df_merge <- left_join(df, score, by = c("nom" = "nom", "pays" = "pays"))

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
    
#----------------------------PLOT-NOTE-----------------------#
    output$plot_words <- renderPlot({
      bing_count <- df_comment |> 
        select("comment_en", "nom") |>
        unnest_tokens(word,comment_en) |> 
        inner_join(get_sentiments("bing")) |> 
        count(word, sentiment, sort = TRUE)
      
      my_stop_words <- tibble(
        word = c("lemon"),
        lexicon = "autres"
      )
      
      bing_count |> 
        group_by(sentiment) |> 
        anti_join(bind_rows(get_stopwords("en"), my_stop_words),
                  by = "word") |> 
        top_n(10) |> 
        ggplot(aes(x = reorder(word, n), y = n)) + 
        geom_col(fill = "royalblue", alpha = 0.35) +
        facet_wrap(~sentiment, nrow= 1, scale = "free") + 
        coord_flip()+
        labs(x = "word")+
        theme_minimal()
      
    })
    
    output$plot_by_note <- renderPlot({
      score |> 
        ggplot()+
        aes(x = note)+
        geom_histogram(aes(y=..density..), alpha=0.1,
                       fill = "darkblue", bins = 50)+
        geom_density(fill = "royalblue",
                     alpha = 0.25)+
        theme_minimal() +
        labs(x = "Note", y = "Densité", 
             title = "Répartition des notes")+
        geom_vline(aes(xintercept=mean(note),
                       color="red"),
                   linetype="dashed")+
        theme(legend.position = "None")
    })
    
    output$plot_note_pays <- renderPlot({
      score_pays |> 
        filter(nb_recette > 5) |> 
        top_n(10) |> 
        ggplot(aes(y = note,
                   x = fct_reorder(pays, note)))+
        geom_col(fill = "royalblue", alpha = 0.25,
                 color = "royalblue", width = 0.75)+
        coord_flip()+
        theme_minimal()+
        labs(x = "Note",
             y = "",
             title = "Note par pays")+
        geom_hline(aes(yintercept=mean(note)),
                   linetype="dashed",
                   size=1.2,
                   color="darkblue")
    })
#----------------------------TABLE----------------------------# 
  output$table_recette <- render_gt({
    ifelse(input$select_all,
      df_rec <- df_merge[,c("img", "nom","pays", "niveau", "temps", "cout","ISO2", "note")],
      df_rec <- df_merge[,c("img", "nom","pays", "niveau", "temps", "cout","ISO2", "note")] |> 
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
            cout = html(fontawesome::fa("sack-dollar"),"Coût/pers"),
            note = html(fontawesome::fa("star"),"Note")) |> 
      gt_fa_rating(note, icon = "star", color = "gold") 
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
  
#----------------------------TEXTE----------------------------# 
  
  output$titre <- renderUI({
    h1(input$select_recette)
  })
  
  output$details_ing <- renderUI({
    recette <- df[df$nom == input$select_recette, ]
    ingredients <- unlist(recette$ingredient_nom)
    quantites <- unlist(recette$ingredient_qte)
    details <- tagList(
      tags$ul(
        lapply(seq_along(ingredients), function(i) {
          tags$li(paste(ingredients[i], " : ", quantites[i]))
        })
      )
    )
    return(details)
  })

  output$details_recette <- renderUI({
    recette <- df[df$nom == input$select_recette, ]
    etapes <- unlist(recette$recette)
    details <- tagList(
      tags$ul(
        lapply(etapes, function(etape) {
          tags$li(etape)
        })
      )
    )
    return(details)
  })
  
}
