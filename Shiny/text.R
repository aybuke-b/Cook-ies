library(janeaustenr)
library(textdata)
library(tidyverse)
library(tidytext)

library(Xplortext)
library(wordcloud)
library(gutenbergr)
library(FactoMineR)
library(janitor)
library(arrow)

df <- read.csv("data/comment_en.csv",
               sep = ",",
               header = TRUE,
               fileEncoding = "utf-8")
df <- df[,-c(3)]

liste_perso <- c("a")

## SENTIMENT

### afinn
score_afinn <- df |> select("comment_en", "nom") |>
  unnest_tokens(word,comment_en) |> 
  inner_join(get_sentiments("afinn")) |>
  group_by(nom) |>
  summarise(sentiment = sum(value))

### bing
score_bing <- df |> select("comment_en", "nom") |>
  unnest_tokens(word,comment_en) |> 
  inner_join(get_sentiments("bing")) |> 
  group_by(nom) |> 
  summarise(
    positive = sum(sentiment == "positive"),
    negative = sum(sentiment == "negative"))

score_bing <- score_bing |> 
  mutate(ratio = positive / (negative + positive)) 

bing_count <- df |> select("comment_en", "nom") |>
  unnest_tokens(word,comment_en) |> 
  inner_join(get_sentiments("bing")) |> 
  count(word, sentiment, sort = TRUE)

bing_count |> 
  group_by(sentiment) |> 
  top_n(10) |> 
  ungroup() |> 
  ggplot(aes(x = reorder(word, n), y = n)) + 
  geom_col(fill = "orchid", alpha = 0.6) +
  facet_wrap(~sentiment, nrow= 1, scale = "free") + 
  coord_flip()+
  labs(x = "word")

### nrc
score_nrc <- df |> select("comment_en", "nom") |>
  unnest_tokens(word,comment_en) |> 
  inner_join(get_sentiments("nrc")) 

nrc_count <- df |> select("comment_en", "nom") |>
  unnest_tokens(word,comment_en) |> 
  inner_join(get_sentiments("nrc")) |> 
  count(word, sentiment, sort = TRUE)

nrc_count |> 
  group_by(sentiment) |> 
  top_n(10) |> 
  ungroup() |> 
  ggplot(aes(x = reorder(word, n), y = n)) + 
  geom_col(fill = "orchid", alpha = 0.6) +
  facet_wrap(~sentiment, nrow= 1, scale = "free") + 
  coord_flip()+
  labs(x = "word")

#score = merge(score_afinn, score_bing[,-c(2,3)], by = "nom")

### TEXTDATA

res_td <- TextData(df,
         var.text = c("comment_en"),
         idiom = "en",
         remov.number = FALSE,
         context.quanti = "cout",
         context.quali = c(1,3),
         segment = TRUE,
         seg.nfreq = 4,
         seg.nfreq2 = 10,
         seg.nfreq3 = 10,
         graph = FALSE,
         stop.word.tm = TRUE,
         stop.word.user = liste_perso)

summary(res_td)

plot(res_td,
     nword = 40,
     sel = "word",
     title = "Les 10 mots les plus fréquents",
     xtitle = "Fréquences",
     col.fill = "azure2")



#res_lexCA <- LexCA(res_td, graph = FALSE)
#
#plot(res_lexCA,
#     gtype = "DocWord",
#     title = "Représentation graphique des mots",
#     col.word = "orange",
#     col.doc = "royalblue")
