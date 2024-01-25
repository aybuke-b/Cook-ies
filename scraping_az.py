from bs4 import BeautifulSoup as BS
import requests as rq
from Recette import Recette
import re
import numpy as np
from serde.json import to_json

def get_pays(url:str) -> list[str]:
    res = rq.get(url)
    soupe = BS(res.text, 'html.parser')
    pays = [a['href'] for a in soupe.select('div.tile_content a.tile_title')]
    urls = []
    for pay in pays:
        urls.append("https://www.cuisineaz.com" + pay)
    return urls

def get_page_pays(urls_pays:str) -> int:
    str_page = _soupage(urls_pays[0]).find_all("a", "lienPagination")[-1].text
    nb_page = int(re.findall(r'\d+', str_page)[0])
    return nb_page

def get_url_pays(lien:str, nb_page:int) -> list[str]:
    urls_in_pays = []
    for page in range(1,int(nb_page) + 1):
        urls_in_pays.append(lien + f"/{page}/")
    return urls_in_pays

def get_url_recette(urls_in_pays:str) -> list[str]:
    recettes_pays = []
    for url in urls_in_pays:
        blocs  = _soupage(url).find_all("section", class_ = "block-primary tiles --col-2 md:--col-4")
        for bloc in blocs:
            recettes = bloc.find_all("article")
            for recette in recettes:
                href = recette.find("a").get("href")
                recettes_pays.append("https://www.cuisineaz.com" + href)
    return list(np.unique(recettes_pays))
    

def _soupage(lien:str) -> BS:
    requete = rq.get(lien, timeout=1000)
    text = requete.text
    soupe = BS(text, features="lxml")
    return soupe

def _scrap_name(soupe : BS) -> str:
    nom = soupe.find("h1", class_ = "recipe-title").text
    return nom

def _scrap_main_infos(soupe : BS) -> list[str]:
    main_infos = soupe.find("section", class_ = "recipe_infos")
    temps = soupe.find("section", class_ = "recipe_infos").find("div", class_ = "icon-prepa").text
    niveau = main_infos.find("div", class_ = "icon-toque").text
    cout = main_infos.find("span").text
    return temps, niveau, cout

def _scrap_nb_comment(soupe : BS) -> str:
    nb_comment = soupe.find("b").text
    return nb_comment

def _scrap_ingredients(soupe:BS):
    ingredients_nom = soupe.find_all("span", class_ = "ingredient_label")
    ing_nom = [ingredient.text for ingredient in ingredients_nom]
    
    ingredients_qte = soupe.find_all("span", class_ = "js-ingredient-qte ingredient_qte")
    ing_qte= [ingredient.text for ingredient in ingredients_qte]
    return ing_nom, ing_qte

def _scrap_baklava(soupe:BS) -> str:
    img = soupe.find("img", class_="imgRecipe").get("src")
    return img

def _scrap_etapes(soupe:BS) -> list[str]:    
    list_etapes = []

    etapes = soupe.find("section", class_="borderSection instructions").find_all("p")
    for etape in etapes:
        list_etapes.append(etape.find("span"))
        
    rec_etapes = [etape.text for etape in list_etapes if etape != None]
    return rec_etapes

def _scrap_toulousaing(soupe:BS) -> list[str]:
    comments = soupe.find_all("div", class_ = "comment")
    list_comments = []
    for comment in comments:
        list_comments.append(comment.find("p", class_ = "serif-secondary").text)
    return list_comments

def scrap(lien:str) -> Recette:
    soupe = _soupage(lien)
    main_infos = _scrap_main_infos(soupe)
    ing = _scrap_ingredients(soupe)
    res = Recette(
        nom = _scrap_name(soupe),
        note = None,
        temps = main_infos[0],
        niveau = main_infos[1],
        cout = main_infos[2],
        nb_comment = _scrap_nb_comment(soupe),
        ingredient_nom = ing[0],
        ingredient_qte = ing[1],
        img = _scrap_baklava(soupe),
        recette = _scrap_etapes(soupe),
        comment = _scrap_toulousaing(soupe),
        pays = None
    )
    return res

def get_pipeline(url:str):
    urls_pays = get_pays(url)
    nb_page = get_page_pays(urls_pays)
    rec = 0
    recette = []
    for url in urls_pays:
        
        urls_in_pays = get_url_pays(url, nb_page)
        lien_recette_pays = get_url_recette(urls_in_pays)
        for lien in lien_recette_pays:
            try:
                recette.append(scrap(lien))
                rec = rec + 1
                print(f"Recette n°{rec} scrapé !")
            except:
                pass
    rec_json = to_json(recette)
    with open("data/recette.json", "w", encoding="utf-8") as json_file:
        json_file.write(rec_json)
    return print("Export JSON réalisé")