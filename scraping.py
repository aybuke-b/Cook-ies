from bs4 import BeautifulSoup as BS
import requests as rq
from Recette import Recette

def _soupage(lien:str) -> BS:
    requete = rq.get(lien, timeout=1000)
    text = requete.text
    soupe = BS(text, features="lxml")
    return soupe

def _scrap_name(soupe : BS) -> str:
    nom = soupe.find("h1").text
    return nom

def _scrap_main_infos(soupe : BS) -> list[str]:
    main_infos = soupe.find_all("div", class_ = "recipe-primary__item")
    temps = main_infos[0].find("span").text
    niveau = main_infos[1].find("span").text
    cout = main_infos[2].find("span").text
    return temps, niveau, cout

def _scrap_note(soupe: BS) -> str:
    note = soupe.find("span", class_ = "recipe-header__rating-text").text
    return note

def _scrap_nb_comment(soupe: BS) -> str:
    nb_comment = soupe.find("div", class_ = "recipe-header__comment").text
    return nb_comment

def scrap(lien:str) -> Recette:
    soupe = _soupage(lien)
    main_infos = _scrap_main_infos(soupe)[0]
    res = Recette(
        nom = _scrap_name(soupe),
        note = _scrap_note(soupe),
        temps = main_infos[0],
        niveau = main_infos[1],
        cout = main_infos[2],
        nb_comment = _scrap_nb_comment(soupe)
        )
    return res