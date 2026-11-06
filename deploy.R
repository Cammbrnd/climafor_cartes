# ==============================================================================
# deploy.R — DEPLOIEMENT SUR shinyapps.io
# ------------------------------------------------------------------------------
# A executer depuis la racine du projet, en local. Ne pas deployer ce fichier
# lui-meme (il figure dans la liste d'exclusion ci-dessous).
#
# Prerequis, une seule fois :
#   install.packages("rsconnect")
#   rsconnect::setAccountInfo(name = "<compte>", token = "<token>", secret = "<secret>")
#   (jetons disponibles sur shinyapps.io, menu Account > Tokens)
# ==============================================================================

# -- 1. Verification des dependances ------------------------------------------
paquets <- c("shiny", "htmltools", "ggplot2", "leaflet", "plotly", "httr2", "jsonlite",
             "rmarkdown", "knitr", "base64enc", "sf", "rsconnect")

manquants <- paquets[!paquets %in% rownames(installed.packages())]
if (length(manquants)) {
  message("Installation des paquets manquants : ", paste(manquants, collapse = ", "))
  install.packages(manquants)
}

# httr2 >= 1.0 est requis pour req_perform_parallel(), qui divise par six le
# temps de chargement d'une serie longue. L'application fonctionne sans, en
# mode sequentiel, mais l'attente devient penible au-dela de 30 annees.
if (utils::packageVersion("httr2") < "1.0.0") {
  warning("httr2 < 1.0 : le telechargement sera sequentiel. Mise a jour recommandee.")
}

# -- 2. Controle de sante avant envoi -----------------------------------------
# Mieux vaut echouer ici que sur le serveur : un deploiement rate coute
# plusieurs minutes.
message("Verification du chargement de l'application...")
# global.R peut s'appeler global.r si un partage a force les minuscules.
source(list.files(".", pattern = "^global\\.r$", ignore.case = TRUE)[1])
stopifnot(
  "CONFIG absent"              = exists("CONFIG"),
  "Registre vide"              = length(INDICATEURS) > 0,
  "Modele de rapport manquant" = file.exists(chemin_ci("rapport/rapport_climat.Rmd")),
  "Feuille de style manquante" = file.exists("www/climafor.css") &&
                                 file.exists("www/fonts/open-sans-latin-400-normal.woff2"),
  "Script client manquant"     = file.exists("www/app.js"),
  "Composant carte manquant"   = file.exists("www/carte/carte.js") &&
                                 file.exists("www/carte/lib/maplibre-gl.js"),
  # v2.0 : trois profils (terrain, correspondant-observateur, permanent DSF) et bilan
  # synthetique. Sans la liste d'identifiants ni les contours, la page d'entree ne
  # reconnait personne et le bilan par departement ou region est vide.
  "Liste d'acces manquante"    = file.exists(CONFIG$acces$fichier),
  "Contours manquants"         = file.exists(file.path(CONFIG$territoires$dossier, "departements.geojson")) &&
                                 file.exists(file.path(CONFIG$territoires$dossier, "regions.geojson"))
)
message(sprintf("  %d indicateurs declares : OK", length(INDICATEURS)))

# Onglet Carte : si des donnees cartographiques sont embarquees (donnees_carte/),
# elles doivent etre valides ET reelles. Le jeu de demonstration (outils/carte_demo.R)
# est fictif : le publier ferait afficher du faux climat a de vrais utilisateurs.
if (file.exists(file.path(chemin_ci(CONFIG$carte$dossier_local), "meta.json"))) {
  lu <- carte_lire_meta_locale(chemin_ci(CONFIG$carte$dossier_local))
  if (is.null(lu$meta)) {
    stop("donnees_carte/ est invalide : ", paste(lu$erreurs, collapse = " "))
  }
  if (grepl("FICTIVES", lu$meta$source %||% "", fixed = TRUE)) {
    stop("donnees_carte/ contient le jeu de DEMONSTRATION (donnees fictives). ",
         "Supprimez ce dossier ou remplacez-le par un jeu reel avant de deployer.")
  }
  message("  Donnees cartographiques embarquees : OK")
}

# Acces : la liste versionnee ne contient que des identifiants de DEMONSTRATION. Les vrais
# identifiants vont dans config/acces_local.csv (non versionne) ; s'il existe, il est
# envoye avec l'application. Ce n'est pas une authentification : voir README.
if (file.exists(CONFIG$acces$fichier_local)) {
  message("  Identifiants locaux (", CONFIG$acces$fichier_local, ") : envoyes avec l'application")
} else {
  message("  Pas de ", CONFIG$acces$fichier_local, " : seuls les identifiants de demonstration ouvrent un profil")
}
if (isTRUE(CONFIG$acces$demo)) {
  message("  CONFIG$acces$demo = TRUE : ?profil=co|dsf|terrain est accepte. ",
          "Mettez FALSE (src/config.R) pour une exploitation hors demonstration.")
}

# Chaque entree du registre doit exposer les champs attendus par les modules.
champs_requis <- c("id", "titre", "famille", "calc", "plot", "methode", "lecture")
for (cle in names(INDICATEURS)) {
  absents <- setdiff(champs_requis, names(INDICATEURS[[cle]]))
  if (length(absents)) {
    stop(sprintf("Indicateur '%s' : champs manquants (%s)",
                 cle, paste(absents, collapse = ", ")))
  }
}
message("  Structure du registre : OK")

# -- 3. Deploiement -----------------------------------------------------------
# Liste des fichiers envoyes. Le cache est exclu : il se reconstitue tout seul, et son
# envoi ferait inutilement gonfler le paquet. www/ et rapport/ sont parcourus RECURSIVEMENT :
# www/carte/ (carte.js, MapLibre) est un sous-dossier, et un fichier absent du paquet
# casserait l'onglet Carte sans message.
fichiers_app <- c(
  # Motifs insensibles a la casse : avec des fichiers en .r (partage reseau),
  # un motif "\\.R$" n'enverrait AUCUN fichier de src/ au serveur.
  list.files(".", pattern = "^(app|global)\\.r$|^\\.rprofile$",
             ignore.case = TRUE, all.files = TRUE),
  list.files("src", pattern = "^[^_].*\\.r$", ignore.case = TRUE, full.names = TRUE),
  list.files("www", full.names = TRUE, recursive = TRUE),
  list.files("rapport", full.names = TRUE, recursive = TRUE),
  # Jeu cartographique embarque (facultatif : sinon l'application lit l'adresse
  # CONFIG$carte$url_donnees). Vide si le dossier n'existe pas.
  list.files(CONFIG$carte$dossier_local, full.names = TRUE, recursive = TRUE),
  # v2.0 : identifiants d'acces (dont la liste locale, si elle existe) et contours
  # des departements et regions du bilan synthetique.
  CONFIG$acces$fichier,
  if (file.exists(CONFIG$acces$fichier_local)) CONFIG$acces$fichier_local,
  list.files(CONFIG$territoires$dossier, full.names = TRUE, recursive = TRUE)
)
fichiers_app <- sub("^\\./", "", fichiers_app)
stopifnot(
  "Le paquet n'embarque pas www/carte/carte.js"        = "www/carte/carte.js" %in% fichiers_app,
  "Le paquet n'embarque pas MapLibre (www/carte/lib/)" = "www/carte/lib/maplibre-gl.js" %in% fichiers_app,
  "Le paquet n'embarque pas la liste d'acces"          = CONFIG$acces$fichier %in% fichiers_app,
  "Le paquet n'embarque pas les contours (donnees_limites/)" =
    file.path(CONFIG$territoires$dossier, "departements.geojson") %in% fichiers_app
)
message(sprintf("  %d fichiers a envoyer (dont %d pour la carte)", length(fichiers_app),
                sum(grepl("^www/carte/", fichiers_app))))

rsconnect::deployApp(
  appDir      = ".",
  appName     = "climafor",
  appTitle    = "ClimaFor - Bilan climatique forestier",
  appFiles    = fichiers_app,
  forceUpdate = TRUE,
  launch.browser = TRUE
)

# ------------------------------------------------------------------------------
# NOTE SUR L'OFFRE GRATUITE
# ------------------------------------------------------------------------------
# 5 applications, 25 heures actives par mois, 1 Go de memoire, une instance
# unique. Consequences pratiques :
#
#  - Le cache disque n'est pas partage entre instances et disparait a chaque
#    redemarrage. Il accelere une session, pas les suivantes.
#  - Une seule instance signifie un seul processus R : deux utilisateurs
#    lançant simultanement une serie de 60 ans se gênent. Acceptable pour un
#    usage interne ; au-dela, passer sur l'offre Starter.
#  - Le compteur d'heures tourne des qu'une session est ouverte. Reduire
#    le delai d'inactivite dans les reglages de l'application (5 minutes
#    suffisent) economise sensiblement le quota.
# ------------------------------------------------------------------------------
