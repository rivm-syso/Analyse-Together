######################################################################
# Dit script zet een aantal environment variabelen die ook gebruikt
# worden in de productieomgeving. Deze Environment variabelen bepalen
# welke database wordt gebruikt en welke gebruiker is ingelogd.
######################################################################

# Analyse Together data folder
#------------------------

Sys.setenv(ANALYSETOGETHER_DATAFOLDER = "./data")

# actief
#------------------------
# Actief: geeft normale pagina weer | Inactief geeft melding dat de applicatie (tijdelijk) offline is.

Sys.setenv(ACTIVE=TRUE)
