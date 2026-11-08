# ==============================================================================
# global.R — CHARGEMENT DE L'APPLICATION
# ------------------------------------------------------------------------------
# Execute une seule fois au demarrage du processus R, avant toute session.
# C'est ici que sont chargees les bibliotheques et les fichiers du code source.
#
# Ordre de chargement impose : config.R doit preceder tout le reste (les autres
# fichiers lisent CONFIG a l'evaluation), et registre_indicateurs.R doit venir
# apres utils_calc.R et utils_plot.R (il reference les fonctions calc_* et
# plot_* par leur objet, pas par leur nom).
#
# POURQUOI LE DOSSIER S'APPELLE src/ ET NON R/
# -------------------------------------------
# Depuis Shiny 1.5, tout fichier place dans un dossier nomme R/ est charge
# AUTOMATIQUEMENT par Shiny, dans l'ordre alphabetique, et AVANT global.R.
# Cet ordre alphabetique placerait registre_indicateurs.R avant utils_calc.R,
# alors que le registre reference les fonctions calc_* par leur objet :
# le chargement echouerait sur "objet 'calc_bilan_hydrique' introuvable".
# Nommer le dossier src/ desactive ce mecanisme et rend l'ordre entierement
# explicite ci-dessous. Ne pas renommer ce dossier en R/.
# ==============================================================================

# -- Encodage ------------------------------------------------------------------
# Les serveurs shinyapps.io tournent en locale C. On force l'UTF-8 pour que les
# accents des libelles s'affichent correctement.
options(encoding = "UTF-8")
# Le nom de la locale francaise differe selon le systeme : on essaie les
# variantes usuelles et on s'arrete a la premiere qui repond. Aucune n'est
# indispensable (les noms de mois sont codes en dur dans config.R), c'est
# uniquement le tri alphabetique des accents qui en depend.
for (loc in c("fr_FR.UTF-8", "French_France.1252", "fr_FR", "French")) {
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
}

# -- Bibliotheques -------------------------------------------------------------
# Volontairement peu nombreuses : l'instance gratuite de shinyapps.io dispose
# d'environ 1 Go de memoire. On evite sf et terra, non indispensables ici. Le
# module territoire a venir introduira sf : prevoir a ce moment-la un passage
# sur une instance dimensionnee.
#
# PLOTLY — ARBITRAGE ASSUME
# -------------------------
# plotly etait ecarte des l'origine pour la meme raison. Il est desormais requis
# par le choix de graphiques interactifs. Ce que cela coute, pour que la
# decision reste revisable en connaissance de cause :
#   - environ 60 a 80 Mo de memoire par processus R ;
#   - environ 3,5 Mo de JavaScript envoyes au navigateur a la premiere page ;
#   - environ 3,5 Mo ajoutes a chaque rapport HTML autonome contenant des
#     graphiques interactifs.
# Si l'instance vient a saturer, poser CONFIG$graphiques$interactif <- FALSE
# suffit a revenir a des images : rien d'autre n'est a modifier, les fonctions
# plot_* produisent toujours des ggplot.
library(shiny)
library(htmltools)
library(ggplot2)      # fournit aussi .data, utilise dans les aes()
library(leaflet)
library(plotly)
library(httr2)
library(jsonlite)
library(rmarkdown)
library(knitr)
library(base64enc)   # logos embarques dans le rapport autonome

# -- Chargement des fichiers du dossier src/ ----------------------------------
# Chargement explicite plutot que par le mecanisme automatique de Shiny :
# l'ordre est ici significatif et doit rester lisible.
# -- Chemins insensibles a la casse ------------------------------------------
# Un partage reseau ou une copie peut renommer config.R en config.r. Windows ne
# s'en apercoit pas, Linux (poste, serveur de deploiement) si : source() echoue
# et l'application ne demarre pas. Ce resolveur retrouve le vrai nom sur disque.
# Il sert a tout acces a un fichier du projet par son nom (src/, rapport/).
chemin_ci <- function(chemin) {
  cur <- "."
  for (p in strsplit(chemin, "/", fixed = TRUE)[[1]]) {
    if (!nzchar(p) || p == ".") next
    ls <- list.files(cur, all.files = TRUE, no.. = TRUE)
    h <- ls[tolower(ls) == tolower(p)]
    cur <- file.path(cur, if (p %in% h || !length(h)) p else h[1])
  }
  sub("^\\./", "", cur)
}

FICHIERS_R <- c(
  "src/config.R",                 # 1. constantes — aucune dependance
  "src/utils_ui.R",               # 2. helpers d'interface (icones, etapes)
  "src/glossaire.R",              # 2b. vocabulaire : carte Sources + infobulles
  "src/references.R",             # 2c. references datees, statut des seuils, corrections
  "src/utils_cache.R",            # 2d. cache disque : format controle, purge
  "src/utils_limites.R",          # 2e. limites d'usage (fenetre glissante)
  "src/utils_api.R",              # 3. acces aux donnees (SAFRAN, communes)
  "src/utils_stations.R",         # 3b. stations Meteo-France (donnees ouvertes)
  "src/utils_calc.R",             # 4. calcul des indicateurs
  "src/utils_plot.R",             # 5. rendu graphique
  "src/registre_indicateurs.R",   # 6. registre — reference 4 et 5
  "src/utils_rapport.R",          # 6b. rapport et exports : assainissement, parametres
  "src/utils_partage.R",          # 6c. etat partageable d'une analyse (lien)
  "src/carte_couches.R",          # 6d. couches cartographiables (v1.8)
  "src/carte_couches_base.R",     # 6d2. base precalculee : colonnes et references hors carte (v2.2)
  "src/utils_carte.R",            # 6e. fichiers de la carte : validation, transport
  "src/utils_comparer.R",         # 6f. comparaison de lieux importes (v1.9)
  "src/utils_base.R",             # 6f1. bilan d'un point lu dans la base precalculee (v2.2)
  "src/carte_couches_base_stations.R", # 6f1b. base precalculee des stations : qualite, jours manquants, references (v2.2)
  "src/utils_stations_publiees.R",# 6f2. stations publiees a cote des mailles (v2.1)
  "src/utils_comparer_duo.R",     # 6f3. comparaison de deux lieux : maille, station, departement (v2.1)
  "src/utils_profils.R",          # 6g. profils et acces par identifiant (v2.0)
  "src/utils_territoires.R",      # 6h. departements et regions (v2.0)
  "src/utils_fiches.R",           # 6i. fiches importees, point d'accroche Sylvasan (v2.0)
  "src/utils_synthese.R",         # 6j. bilan synthetique : calcul, phrases, graphiques (v2.0)
  "src/utils_synthese_rendu.R",   # 6k. bilan synthetique : tableaux, CSV, bilan a emporter (v2.0)
  "src/utils_envoi.R",            # 6l. synthese a envoyer par courriel (v2.1)
  "src/mod_localisation.R",       # 7. modules Shiny
  "src/mod_station.R",
  "src/mod_carte.R",              # onglet Carte (v1.8)
  "src/mod_lieu_choix.R",         # choix d'un lieu : maille, station, departement (v2.1)
  "src/mod_comparer.R",           # onglet Comparer (v1.9)
  "src/mod_synthese.R",           # onglet Synthese : bilan court a quatre echelles (v2.0)
  "src/mod_terrain.R",            # ecran « Autour de moi » (v2.0)
  "src/mod_periode.R",
  "src/mod_graphique.R",
  "src/mod_bilan.R",
  "src/mod_rapport.R",
  "src/mod_parametres.R",
  "src/mod_sources.R",
  "src/serveur_dsf.R",            # serveur de l'analyse d'un point, profil permanent (v2.0)
  "src/ui_page.R"                 # page : entree, en-tete, onglets par profil (v2.0)
)

for (f in FICHIERS_R) {
  f <- chemin_ci(f)
  if (!file.exists(f)) {
    stop(sprintf(paste("Fichier manquant : %s\nVerifiez que l'application est",
                       "lancee depuis la racine du projet (le dossier contenant",
                       "app.R)."), f))
  }
  source(f, local = FALSE, encoding = "UTF-8")
}

# Controle de coherence : le registre doit avoir trouve ses fonctions.
if (!exists("INDICATEURS") || length(INDICATEURS) == 0) {
  stop("Le registre d'indicateurs est vide : verifiez l'ordre de FICHIERS_R.")
}

# -- Repertoire de cache -------------------------------------------------------
# Le systeme de fichiers de shinyapps.io est ephemere : le cache accelere les
# rechargements au sein d'une meme instance, il ne persiste pas au redeploiement.
if (!dir.exists(CONFIG$cache$repertoire)) {
  dir.create(CONFIG$cache$repertoire, showWarnings = FALSE, recursive = TRUE)
}

# Purge au demarrage : fichiers d'un ancien format, fichiers perimes, plafond de
# taille (audit V1). Ne leve jamais d'erreur.
cache_purger()

# -- Limites de taille ---------------------------------------------------------
options(shiny.maxRequestSize = 10 * 1024^2)

message(sprintf("%s %s — %d indicateurs charges",
                CONFIG$app$nom, CONFIG$app$version, length(INDICATEURS)))
