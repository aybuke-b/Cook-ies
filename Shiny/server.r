score_afinn <- df_comment |> 
  select("comment_en", "nom", "pays", "ISO2") |>
  unnest_tokens(word,comment_en) |> 
  inner_join(get_sentiments("afinn")) |> 
  group_by(nom, pays, ISO2) |> 
  mutate(nb_comment = n()) |> 
  group_by(nom, nb_comment, pays, ISO2) |>
  summarise(sentiment = sum(value)) |> 
  mutate(note_afinn = sentiment/nb_comment)

score_bing <- df_comment |> 
  select("comment_en", "nom", "pays", "ISO2") |>
  unnest_tokens(word,comment_en) |> 
  inner_join(get_sentiments("bing")) |> 
  group_by(nom, pays, ISO2) |> 
  summarise(
    positive = sum(sentiment == "positive"),
    negative = sum(sentiment == "negative")) |> 
  mutate(note_bing = positive / (negative + positive)) 

max_bing <- max(score_bing$note_bing)
min_bing <- min(score_bing$note_bing)

max_afinn <- max(score_afinn$note_afinn)
min_afinn <- min(score_afinn$note_afinn)

score <- merge(score_afinn, score_bing, on = "nom") |>
  select("nom", "pays","nb_comment", "note_afinn", "note_bing", "ISO2") |>
  mutate(note = 5*((note_afinn-min_afinn)/(max_afinn - min_afinn) + (note_bing-min_bing)/(max_bing - min_bing))/2,
         note_afinn = 5*(note_afinn-min_afinn)/(max_afinn - min_afinn)/2,
         note_bing = 5*(note_bing-min_bing)/(max_bing - min_bing)/2) |>
  filter(nb_comment > 5)

df_merge <- left_join(df, score, by = c("nom" = "nom", "pays" = "pays"))

df_merge <- df_merge %>%
  mutate(
    heures = floor(temps),
    minutes = round((temps - heures) * 60, 0),
    heures_minute = paste(heures, "h", minutes, "min"),
    cout = abs(cout)
  )

server <- function(input, output, session) {
#----------------------------PLOT----------------------------#
    output$plot_cout <- renderPlotly({
      if (!input$select_all) {
        df_plot <- df_merge |>
          filter(pays %in% input$select_pays) |>
          filter(niveau %in% input$select_niveau) |>
          filter(temps < input$select_temps)
      } else {
        df_plot <- df_merge
      }
      
      if (input$only_note) {
        df_plot <- df_plot[complete.cases(df_plot$note), ]
      }
      
      df_plot <- df_plot |> 
        group_by(pays) |> 
        summarise(mean_cout = mean(cout))
      
      df_plot$pays <- factor(df_plot$pays, levels = unique(df_plot$pays)[order(df_plot$mean_cout, decreasing = FALSE)])
      
      brewer_colors <- brewer.pal(6, "Paired")
      
      fig = plot_ly(y = df_plot$mean_cout,
                    x = df_plot$pays,
                    type = "bar",
                    marker = list(color = "#e69f4d"))|> 

              layout(title = 'Répartition des coûts', 
                     bargap = 0.1,
                     xaxis = list(title = "Coût/personne"))

    })
    
    output$plot_pays <- renderPlotly({
      if (!input$select_all) {
        df_plot <- df_merge |>
          filter(pays %in% input$select_pays) |>
          filter(niveau %in% input$select_niveau) |>
          filter(temps < input$select_temps)
      } else {
        df_plot <- df_merge
      }
      
      if (input$only_note) {
        df_plot <- df_plot[complete.cases(df_plot$note), ]
      }
      
      df_plot <- df_plot |> 
        group_by(pays) |> 
        summarise(pays_n = n())
      
      df_plot$pays <- factor(df_plot$pays, levels = unique(df_plot$pays)[order(df_plot$pays_n, decreasing = FALSE)])
      
      plot_ly(y = df_plot$pays_n,
              x = df_plot$pays, 
              type = "bar", 
              marker = list(color = "#3575aa")) |> 
        layout(title = 'Nombre de recette par pays')
    })
    
    
    output$plot_temps <- renderPlotly({
      if (!input$select_all) {
        df_plot <- df_merge |>
          filter(pays %in% input$select_pays) |>
          filter(niveau %in% input$select_niveau) |>
          filter(temps < input$select_temps)
      } else {
        df_plot <- df_merge
      }
      
      if (input$only_note) {
        df_plot <- df_plot[complete.cases(df_plot$note), ]
      }
      df_plot <- df_plot |> 
        group_by(pays) |> 
        summarise(mean_temps = mean(temps))
      
      df_plot$pays <- factor(df_plot$pays,
                             levels = unique(df_plot$pays)[order(df_plot$mean_temps, decreasing = FALSE)])
    
      plot_ly(y = df_plot$mean_temps,
              x = df_plot$pays,
              type = "bar",
              marker = list(color = "#E3735E"))|> 
        layout(title = 'Temps moyen par recette par pays', 
               bargap = 0.1
               )
      
    })
    
    output$plot_niveau <- renderPlotly({
      if (!input$select_all) {
        df_plot <- df_merge |>
          filter(pays %in% input$select_pays) |>
          filter(niveau %in% input$select_niveau) |>
          filter(temps < input$select_temps)
      } else {
        df_plot <- df_merge
      }
      
      if (input$only_note) {
        df_plot <- df_plot[complete.cases(df_plot$note), ]
      }
      
      df_plot$niveau <- as.factor(df_plot$niveau)
      df_plot$niveau <- fct_relevel(df_plot$niveau, c("Facile", "Intermédiaire", "Difficile"))
      
      plot_ly(x = df_plot$pays,
              color = df_plot$niveau,
              type = "histogram")|> 
        layout(title = 'Répartition des nievaux', 
               bargap = 0.1,
               xaxis = list(title = "Niveau"))
      
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
      score_pays <- score |> 
        group_by(pays) |> 
        summarise(note = mean(note),
                  nb_comment = sum(nb_comment),
                  nb_recette = n())
      
      score_pays |> 
        filter(nb_recette > 5) |> 
        top_n(10, wt = note) |> 
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
  output$table_recette <- renderReactable({
    df_rec <- df_merge[,c("img", "nom","pays", "niveau", "temps","heures_minute", "cout", "note")]
    
    if (!input$select_all) {
      df_rec <- df_rec |>
        filter(pays %in% input$select_pays) |>
        filter(niveau %in% input$select_niveau) |>
        filter(temps < input$select_temps)
    }
    
    if (input$only_note) {
      df_rec <- df_rec[complete.cases(df_rec$note), ]
    }
    
    
    df_rec[,c("img", "nom","pays", "niveau","heures_minute", "cout", "note")] |> 
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
      #fmt_flag(columns = ISO2) |>
      #cols_merge(
      #  columns = c(pays, ISO2),
      #  pattern = "{2} {1}"
      #) |>
          tab_header("Recettes 🥣") |>
          cols_label(
            img = html(fontawesome::fa("camera-retro"),"Image"),
            nom = html(fontawesome::fa("utensils"),"Nom"),
            pays = html(fontawesome::fa("globe"),"Pays"),
            niveau = html(fontawesome::fa("layer-group"),"Niveau"),
            heures_minute = html(fontawesome::fa("clock"),"Temps"),
            cout = html(fontawesome::fa("sack-dollar"),"Coût/pers"),
            note = html(fontawesome::fa("star"),"Note")) |> 
      gt_fa_rating(note, icon = "star", color = "gold") |> 
      my_render_as_ithml(selection = 'single', id = 'bla', defaultSelected = 1)
  })
  
  
  output$table_note <- render_gt({
    score_test <- score |>
      group_by(pays) |>
      summarise(mean_afinn = round(mean(note_afinn),2),
                mean_bing = round(mean(note_bing),2),
                mean_note = round(mean(note),2),
                nb_recette = n()) |>
      filter(nb_recette > 5) |> 
      select(mean_afinn, mean_bing, mean_note, pays)  
      
    score_test <- merge(score_test, unique(score[, c("pays", "ISO2")]), by = "pays", all.x = TRUE)
    
     score_test |> 
       gt() |>
        fmt_flag(columns = ISO2) |>
        cols_merge(
          columns = c(pays, ISO2),
          pattern = "{2} {1}"
        ) |> 
        opt_interactive(use_compact_mode = TRUE) |>
        tab_header("Décompostion note") |>
        cols_label(
          pays = html(fontawesome::fa("globe"),"Pays"),
          mean_afinn = html(fontawesome::fa("star"),"Afinn"),
          mean_bing = html(fontawesome::fa("star"),"Bing"),
          mean_note = html(fontawesome::fa("wand-sparkles"),"Note"))
      
    
  })
  
  
#----------------------------TABLE----------------------------#  
  output$map_monde <- renderPlotly({
    
    if(input$select_all){
      df_mc <- df_merge
    } else {
    df_mc <- df_merge |> 
      filter(pays %in% input$select_pays) |> 
      filter(niveau %in% input$select_niveau) |> 
      filter(temps < input$select_temps)}
    
    if (input$only_note) {
      df_mc <- df_mc[complete.cases(df_mc$note), ]
    } 
    
    df_mc <- df_mc |> 
      group_by(ISO3, pays) |> summarise(mean_temps = mean(temps))
    
    
    plot_ly(df_mc, type='choropleth',
            locations=df_mc$ISO3,
            z=df_mc$mean_temps,
            text=df_mc$pays,
            colorscale="Portland")|> 
      layout(title = 'Temps moyen de préparation des recettes par pays',
             showlegend = FALSE)
  })
  
  output$map_monde_cout <- renderPlotly({
    
    if(input$select_all){
      df_mc <- df_merge
    } else {
      df_mc <- df_merge |> 
        filter(pays %in% input$select_pays) |> 
        filter(niveau %in% input$select_niveau) |> 
        filter(temps < input$select_temps)}
    
    if (input$only_note) {
      df_mc <- df_mc[complete.cases(df_mc$note), ]
    } 
    
    df_mc <- df_mc |> 
      group_by(ISO3, pays) |> summarise(mean_cout = mean(cout))
    
    plot_ly(df_mc, type='choropleth',
            locations=df_mc$ISO3,
            z=df_mc$mean_cout,
            text=df_mc$pays,
            colorscale="Portland")|> 
      layout(title = 'Coût moyen individuel des recettes par pays',
             showlegend = FALSE)
  })

#----------------------------VALUE BOX----------------------------# 

  output$nb_recette <- renderText({
    if (!input$select_all) {
      df <- df_merge |>
        filter(pays %in% input$select_pays) |>
        filter(niveau %in% input$select_niveau) |>
        filter(temps < input$select_temps)
    }
    
    if (input$only_note) {
      df <- df[complete.cases(df$note), ]
    }
    
    df |> nrow()
  })
  
  output$nb_pays <- renderText({
    if (!input$select_all) {
      df <- df_merge |>
        filter(pays %in% input$select_pays) |>
        filter(niveau %in% input$select_niveau) |>
        filter(temps < input$select_temps)
    }
    
    if (input$only_note) {
      df <- df[complete.cases(df$note), ]
    }
    length(unique(df$pays))
  })
  
  output$cout_recette <- renderText({
    if (!input$select_all) {
      df <- df_merge |>
        filter(pays %in% input$select_pays) |>
        filter(niveau %in% input$select_niveau) |>
        filter(temps < input$select_temps)
    }
    
    if (input$only_note) {
      df <- df[complete.cases(df$note), ]
    }
    
    round(mean(df$cout),3)
  })
  
  output$tps_recette <- renderText({
    if (!input$select_all) {
      df <- df_merge |>
        filter(pays %in% input$select_pays) |>
        filter(niveau %in% input$select_niveau) |>
        filter(temps < input$select_temps)
    }
    
    if (input$only_note) {
      df <- df[complete.cases(df$note), ]
    }
    
    nb <- df %>%
      mutate(
        heures = floor(temps),
        minutes = round((temps - heures) * 60, 0),
        heures_minute = paste(heures, "h", minutes, "min")
      )
    
    moyenne_temps <- round(mean(nb$temps), 3)
    paste(floor(moyenne_temps), "h", round((moyenne_temps - floor(moyenne_temps)) * 60, 0), "min")
    
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
  
  
  output$img_recette <- renderUI({
    recette <- df[df$nom == input$select_recette, ]
    img_url <- recette$img
    
    if (is.null(img_url)) {
      return(NULL)
    }
    
    img_tag <- tags$img(src = img_url, style = "max-width:35%; height:auto;")
    centered_img <- tags$div(style = "text-align:center;", img_tag)
    return(centered_img)
  })
  
  observe({
    
    df_rec <- df_merge
    
    if (!input$select_all) {
      df_rec <- df_rec |>
        filter(pays %in% input$select_pays) |>
        filter(niveau %in% input$select_niveau) |>
        filter(temps < input$select_temps)
    }
    
    if (input$only_note) {
      df_rec <- df_rec[complete.cases(df_rec$note), ]
    }

    selected <- reactable::getReactableState("table_recette", "selected")
    indice  <- as.integer(selected)
    choix <- df_rec$nom[indice]

    updateSelectInput(session,
                      "select_recette",
                      choices =  unique(df_rec$nom),
                      selected = choix)
    
  })
  
  #----------------------------CLOUD----------------------------# 
  
  output$plot_cloud <- renderPlot({
    df_words <- df_comment |> 
    mutate(niveau = as_factor(niveau),
           pays = as_factor(pays)) |> 
    unnest_tokens(output=words,
                  input=comment_en,
                  token="words") |> 
    group_by(words) |> summarise(freq = n())
  
  my_stop_words <- tibble(
    word = c("1","2", "it's", "4", "5", "3", "30", "g", "10"),
    lexicon = "autres"
  )
  
  df_words_filter <- df_words |> 
    anti_join(bind_rows(rbind(get_stopwords("en")),my_stop_words), 
              by = c("words"="word")) |> 
    filter(freq >= 50)

  wordcloud(words = df_words_filter$words,
            freq = df_words_filter$freq,
            colors = brewer.pal(8, "Dark2"),
            random.order=FALSE,
            rot.per=0.0) 
  })
  
}

