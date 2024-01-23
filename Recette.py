from dataclasses import dataclass
from serde import serialize


@serialize
@dataclass
class Recette:
    """Classe de l'output brut du scraping"""
    nom: str
    note : str
    temps : str
    niveau : str
    cout : str
    nb_comment : str
