# ==============================================================================
# .Rprofile — garde-fou de chargement
# ------------------------------------------------------------------------------
# Ceinture et bretelles. Le code source est dans src/ précisément pour éviter
# l'auto-chargement alphabétique que Shiny applique à tout dossier nommé R/ :
# cet ordre placerait registre_indicateurs.R avant utils_calc.R et ferait
# échouer le démarrage sur « objet 'calc_bilan_hydrique' introuvable ».
#
# Si le dossier venait à être renommé R/ un jour, cette option neutralise le
# mécanisme et laisse global.R rester seul maître de l'ordre de chargement.
#
# À noter : ce fichier est lu au lancement d'une session R dont le répertoire
# de travail est celui du projet (donc bien pris en compte sur shinyapps.io et
# en ouvrant le .Rproj, mais pas via runApp("chemin/vers/le/projet") depuis
# une session déjà ouverte ailleurs).
# ==============================================================================
options(shiny.autoload.r = FALSE)
