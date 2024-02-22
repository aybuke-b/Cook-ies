library(textdata)
library(tidyverse)
library(tidytext)

library(Xplortext)
library(wordcloud)
library(gutenbergr)
library(FactoMineR)
library(janitor)
library(arrow)

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

df <- read.csv("data/comment_en.csv",
               sep = ",",
               header = TRUE,
               fileEncoding = "utf-8")
df <- df[,-c(3)]

## SENTIMENT
df |> select("comment_en", "nom", "pays") |>
  unnest_tokens(word,comment_en) |> 
  inner_join(get_sentiments("afinn")) |> 
  group_by(nom, pays) |> 
  mutate(nb_comment = n()) |> 
  group_by(nom,nb_comment, pays) |>
  summarise(sentiment = sum(value)) |> 
  mutate(note = sentiment/nb_comment)

### afinn
score_afinn <- df |> 
  select("comment_en", "nom", "pays") |>
  unnest_tokens(word,comment_en) |> 
  inner_join(get_sentiments("afinn")) |> 
  group_by(nom, pays) |> 
  mutate(nb_comment = n()) |> 
  group_by(nom,nb_comment, pays) |>
  summarise(sentiment = sum(value)) |> 
  mutate(note = sentiment/nb_comment)

### bing
score_bing <- df |> select("comment_en", "nom", "pays") |>
  unnest_tokens(word,comment_en) |> 
  inner_join(get_sentiments("bing")) |> 
  group_by(nom, pays) |> 
  summarise(
    positive = sum(sentiment == "positive"),
    negative = sum(sentiment == "negative")) |> 
  mutate(ratio = positive / (negative + positive))  


liste_pays <- score_bing |> 
  count(pays) |>
  group_by(pays) |> 
  summarise(nb_recette = sum(n)) |> 
  arrange(desc(nb_recette)) |> 
  top_n(15) |> 
  as.list()

mean_ratio <- score_bing |>
  filter(pays %in% liste_pays$pays) |>
  group_by(pays) |>
  summarize(mean_ratio = mean(ratio))

ggplot(score_bing |>
         filter(pays %in% liste_pays$pays),
       aes(x = ratio)) +
  geom_density(fill = "royalblue", color = "black", alpha = 0.15) +
  theme_minimal() +
  xlim(0, 1) +
  facet_wrap(~ pays,
             scales = "free") +
  geom_vline(data = mean_ratio,
             aes(xintercept = mean_ratio), 
             color = "red",
             linetype = "dashed",
             size = 1)+
  labs(x = "Ratio",
       y = "Densité",
       title = "Distribution des scores par pays")

bing_count <- df |> select("comment_en", "nom") |>
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
  geom_col(fill = "royalblue", alpha = 0.6) +
  facet_wrap(~sentiment, nrow= 1, scale = "free") + 
  coord_flip()+
  labs(x = "word")

#score = merge(score_afinn, score_bing[,-c(2,3)], by = "nom")

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

df_words_pays <- df |> 
  mutate(niveau = as_factor(niveau),
         pays = as_factor(pays)) |> 
  unnest_tokens(output=words,
                input=comment_en,
                token="words") |> 
  group_by(words, pays) |> summarise(freq = n())

df_words_pays_filter <- df_words_pays |> 
  anti_join(bind_rows(rbind(get_stopwords("en")),my_stop_words), 
            by = c("words"="word")) |> 
  filter(freq >= 40)

df_words_pays_filter |> 
  ggplot()+
  aes(x = fct_reorder(words,freq), y = freq)+
  geom_bar(stat="identity")+
  facet_wrap(~pays)+
  coord_flip()

### TEXTDATA
liste_perso <- c("a", "s", "t")

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
     nword = 20,
     sel = "word",
     title = "Les 20 mots les plus fréquents",
     xtitle = "Fréquences",
     col.fill = "azure2")



res_lexCA <- LexCA(res_td, graph = FALSE, ncp = 2)

plot(res_lexCA,
     gtype = "DocWord",
     title = "Représentation graphique des mots",
     col.word = "orange",
     col.doc = "royalblue")

#######################NOTE####################""
### afinn
score_afinn <- df |> 
  select("comment_en", "nom", "pays") |>
  unnest_tokens(word,comment_en) |> 
  inner_join(get_sentiments("afinn")) |> 
  group_by(nom, pays) |> 
  mutate(nb_comment = n()) |> 
  group_by(nom, nb_comment, pays) |>
  summarise(sentiment = sum(value)) |> 
  mutate(note_afinn = sentiment/nb_comment)

### bing
score_bing <- df |> select("comment_en", "nom", "pays") |>
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

score_pays <- score |> 
  group_by(pays) |> 
  summarise(note = mean(note),
            nb_comment = sum(nb_comment),
            nb_recette = n())

score_pays |> 
  filter(nb_recette > 5) |> 
  top_n(15) |> 
  ggplot(aes(y = note,
             x = fct_reorder(pays, note)))+
  geom_col(fill = "royalblue", alpha = 0.80,
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

