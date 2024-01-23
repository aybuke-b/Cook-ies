from bs4 import BeautifulSoup as BS
import requests as rq
from Recette import Recette

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