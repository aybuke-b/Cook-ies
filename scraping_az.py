from bs4 import BeautifulSoup as BS
import requests as rq
from Recette import Recette
import re
import numpy as np

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

def scrap(lien:str) -> Recette:
    soupe = _soupage(lien)
    main_infos = _scrap_main_infos(soupe)
    res = Recette(
        nom = _scrap_name(soupe),
        note = None,
        temps = main_infos[0],
        niveau = main_infos[1],
        cout = main_infos[2],
        nb_comment = _scrap_nb_comment(soupe)
        )
    return res

def get_pipeline(url:str):
    urls_pays = get_pays(url)
    nb_page = get_page_pays(urls_pays)
    urls_in_pays = get_url_pays(urls_pays[0], nb_page)
    lien_recette_pays = get_url_recette(urls_in_pays)
    for lien in lien_recette_pays:
        try:
            print(scrap(lien))
        except:
            pass