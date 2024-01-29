"""environ 10 minutes"""
from scraping import get_pipeline
from cleaning import clean_pipeline
import polars as pl

soupe = get_pipeline("https://www.cuisineaz.com/cuisine-du-monde-p282")
df = pl.read_json(r"data\recette.json")
df_clean = clean_pipeline(df)
df_clean.write_parquet("data/recette.parquet")