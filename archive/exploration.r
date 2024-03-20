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
    geom_smooth(method = 'lm', se = FALSE)+
    theme_minimal() 

df[df$cout == min(df$cout),][1]
df[df$nb_comment == max(df$nb_comment),]
df[df$temps == max(df$temps),]

df[df$nb_comment > 5,]

colnames(df)
summary(df[,c(1:5,8,11)])
