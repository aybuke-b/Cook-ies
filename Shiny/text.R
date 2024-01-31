library(janeaustenr)
library(textdata)
library(tidyverse)
library(tidytext)

library(Xplortext)
library(wordcloud)
library(gutenbergr)
library(FactoMineR)
library(janitor)


df <- read.csv("data/comment.csv",
               sep = ",",
               header = TRUE,
               fileEncoding = "utf-8")

liste_perso <- c("a")

res_td <- TextData(df,
         var.text = c("comment"),
         idiom = "fr",
         remov.number = FALSE,
         context.quanti = "cout",
         context.quali = c(1,4),
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
     sel = "seg",
     title = "Les 10 mots les plus fréquents",
     xtitle = "Fréquences",
     col.fill = "azure2")

res_lexCA <- LexCA(res_td, graph = FALSE)

plot(res_lexCA,
     gtype = "DocWord",
     title = "Représentation graphique des mots",
     col.word = "orange",
     col.doc = "royalblue")
