def role ():
    role = """Je zal een artikel vaan een nederlandse krant krijgen over een catastrophe. 
                    Geef de volgende informatie terug in json formaat:
                    -------
                    Dit zijn de velden en korte uitleg (data type):
                    Datum (str): Datum van deze gebeurtenis (formaat dd.mm.yyy). Soms is de datum niet duidelijk vermeld. Er staat bijvoorbeeld "3 dagen na de ramp", dan moet je 3 dagen van de publicatie datum aftrekken.
                    Land (str): Land van deze gebeurtenis
                    Type (str): Wat voor gebeurtenis was dat (explosie, schietincident, oorlog, tsunami, aardbeving, terrorisme, etc)
                    Slachtoffers (int): Hoe veel mensen vielen dood en raakten gewond. Als er geen slachtoffers zijn, geef zero terug.
                    Publicatie_datum (str): Load-Date van het artikel
                    ----
                    Een voorbeeld van verwachte output in json:
                    {"datum": "1.01.2020",
                    "land": "nederland",
                    "type": "oorlog",
                    "slachtoffers": 20,
                    "publicatie_datum":"3.01.2020"
                    }
Als je iets niet kan vinden, geef null terug."""
    return role


def role_2 ():
    role = """Je zal een artikel vaan een nederlandse krant krijgen over een catastrophe. 
                    Geef de volgende informatie terug in json formaat:
                    -------
                    Dit zijn de velden een korte uitleg (data type):
                    Datum (str): Datum van deze gebeurtenis (formaat dd.mm.yyy). Soms is de datum niet duidelijk vermeld. Er staat bijvoorbeeld "3 dagen na de ramp", dan moet je 3 dagen van de publicatie datum aftrekken.
                    Land (str): Land van deze gebeurtenis
                    Type (str): Wat voor gebeurtenis was dat (explosie, schietincident, oorlog, tsunami, aardbeving, terrorisme, etc)
                    Slachtoffers (int): Hoe veel mensen vielen dood en raakten gewond. Als er geen slachtoffers zijn, geef zero terug.
                    Publicatie_datum (str): Load-Date van het artikel
                    Afrika (int): 1 als het evenement in Afrika plaats heeft gevonden of mensen uit Afrika slachtoffers zijn. Zo niet, 0.
                    Amerika (int): 1 als het evenement in Amerika plaats heeft gevonden of mensen uit Amerika slachtoffers zijn. Zo niet, 0.
                    Azië (int): 1 als het evenement in Azië plaats heeft gevonden of mensen uit Azië slachtoffers zijn. Zo niet, 0.
                    België (int): 1 als het evenement in België plaats heeft gevonden of mensen uit België slachtoffers zijn. Zo niet, 0.
                    Duitsland (int): 1 als het evenement in Duitsland plaats heeft gevonden of mensen uit Duitsland slachtoffers zijn. Zo niet, 0.
                    Europa (int): 1 als het evenement in Europa (excl Nederland) plaats heeft gevonden of mensen uit Europa (excl Nederlanders) slachtoffers zijn. Zo niet, 0.
                    Indonesië (int): 1 als het evenement in Indonesië plaats heeft gevonden of mensen uit Indonesië slachtoffers zijn. Zo niet, 0.
                    Nederland (int): 1 als het evenement in Nederland plaats heeft gevonden of mensen uit Nederland slachtoffers zijn. Zo niet, 0.                    
                    Marokko (int): 1 als het evenement in Marokko plaats heeft gevonden of mensen uit Marokko slachtoffers zijn. Zo niet, 0.
                    Oceanië (int): 1 als het evenement in Oceanië plaats heeft gevonden of mensen uit Oceanië slachtoffers zijn. Zo niet, 0.
                    Polen (int): 1 als het evenement in Polen plaats heeft gevonden of mensen uit Polen slachtoffers zijn. Zo niet, 0.
                    Suriname (int): 1 als het evenement in Suriname plaats heeft gevonden of mensen uit Suriname slachtoffers zijn. Zo niet, 0.
                    Turkije (int): 1 als het evenement in Turkije plaats heeft gevonden of mensen uit Turkije slachtoffers zijn. Zo niet, 0.
                    man_made (int): 1 als het door de mens veroorzaakte ramp is. 0 als het natuurlijke ramp is
                    ----
                    Een voorbeeld van verwachte output in json:
                    {"Datum": "1.01.2020",
                    "Land": "Nederland",
                    "Type": "Oorlog",
                    "Slachtoffers": 20,
                    "Publicatie_datum":"3.01.2020",
                    "Afrika": 0,
                    "Amerika": 0,
                    "Azië": 0,
                    "België": 0,
                    "Duitsland": 0,
                    "Europa": 1,
                    "Indonesië": 0,
                    "Nederland": 1,
                    "Marokko": 0,
                    "Oceanië": 0,
                    "man_made": 1,
                    }
Als je iets niet kan vinden, geef null terug. Schrijf alles met kleine letters """
    return role