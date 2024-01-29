import polars as pl

def _clean_temps(df:pl.DataFrame) -> pl.DataFrame:
    df = df.with_columns(
        pl.col("temps").str.extract(r"(\d+ h)").alias("h"),
        pl.col("temps").str.extract(r"(\d+ min)").alias("min"),
    )
    df = df.with_columns(
        pl.col("h")
        .str.replace(" h", "")
        .cast(pl.Int64)
        .fill_null(strategy="zero"),
        pl.col("min")
        .str.replace(" min", "")
        .cast(pl.Int64)
        .fill_null(strategy="zero")
    )
    
    df = df.with_columns(
        temps = pl.col("h") * 60 + pl.col("min")
    )
    df = df.drop("h", "min")
    return df

def _clean_cout(df:pl.DataFrame) -> pl.DataFrame:
    df = df.with_columns(
        pl.col("cout")
        .str.replace(",", ".")
        .str.replace("\n\n\n", "0")
        .cast(pl.Float64)
    )
    return df

def _clean_nb_comment(df:pl.DataFrame) -> pl.DataFrame:
    df = df.with_columns(
        pl.col("nb_comment")
        .cast(pl.Int64)
    )
    return df

def _clean_pays(df:pl.DataFrame) -> pl.DataFrame:
    df = df.with_columns(
        pl.col("pays")
        .str.replace("https://www.cuisineaz.com", "")
        .str.replace("/cuisine-du-monde/", "")
        .str.replace("/recettes-", "")
        .str.replace(r".{5}$","")
    )
    return df

def clean_pipeline(df:pl.DataFrame) -> pl.DataFrame:
    df = (
        df.pipe(_clean_temps)
        .pipe(_clean_cout)
        .pipe(_clean_nb_comment)
        .pipe(_clean_pays)
    )
    return df