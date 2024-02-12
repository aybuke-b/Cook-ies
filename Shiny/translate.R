library(gtranslate)

df <- read_parquet("data/recette.parquet")
df_comment <- df[,c(1,3,10,11,12,13)] |> unnest("comment")

comment_en = list()

comment_en <- lapply(
  df_comment$comment, 
    function(comment) {
    translate(
        text = comment, 
        from = "fr",
        to = "en"
        )
    }
)

df_comment$comment_en <- unlist(comment_en)

write.csv(df_comment, "./data/comment_en.csv", row.names=FALSE)
