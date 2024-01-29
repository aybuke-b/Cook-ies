library(arrow)
library(ggplot2)
library(tidyverse)
df = read_parquet("data/recette.parquet")
colnames(df)

df %>%
    ggplot()+
    aes(x = cout)+
    geom_histogram(bins = 50, fill = 'royalblue', alpha = 0.5)+
    theme_minimal()

df %>%
    ggplot()+
    aes(x = temps)+
    geom_histogram(bins = 100, fill = 'royalblue', alpha = 0.5)+
    theme_minimal()

df %>%
#    filter(nb_comment > 0) %>%
        ggplot()+
        aes(x = nb_comment)+
        geom_histogram(bins = 100, fill = 'royalblue', alpha = 0.5)+
        theme_minimal()

df %>%
    ggplot()+
    aes(x = log(temps), y = cout)+
    geom_point()+
    geom_smooth(method = 'lm')+
    theme_minimal()


