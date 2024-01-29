library(shiny)
library(ggplot2)
library(tidyverse)
library(arrow)

df <- read_parquet("data/recette.parquet")
colnames(df)
server <- function(input, output) {
    output$plot_cout <- renderPlot({
        df %>%
            filter(pays == input$select_pays) %>%
            filter(niveau == input$select_niveau) %>%
            filter(temps < input$select_temps) %>%
                ggplot()+
                    aes(x = cout)+
                    geom_histogram(bins = 50, fill = 'royalblue', alpha = 0.5)+
                    theme_minimal()

    })
}
