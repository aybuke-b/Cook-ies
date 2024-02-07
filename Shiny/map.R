library(ggplot2)

data <- read_parquet("C:/Users/guill/OneDrive - Université de Tours/Bureau/M2/Shiny/data/recette.parquet")

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

df <- merge(data, iso3, by = "pays", all.x = TRUE)

df_mc <- df |> 
  group_by(iso_alpha3, pays) |> summarise(mean_cout = mean(cout))

plot_ly(df_mc, type='choropleth',
        locations=df_mc$iso_alpha3,
        z=df_mc$mean_cout,
        text=df_mc$pays,
        colorscale="Blues")
