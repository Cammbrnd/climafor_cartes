# ==============================================================================
# tests/test_v13.R — CORRECTIFS D'ERGONOMIE DE LA VERSION 1.3 (audit, partie 4)
# ------------------------------------------------------------------------------
# Ces tests lisent le balisage produit par l'interface, sans navigateur. Ce
# qu'ils ne peuvent pas verifier (contraste, taille rendue des cibles, ordre de
# tabulation) l'est par l'audit automatise axe-core rejoue sur l'application :
# voir NEWS.md, section « Version 1.3 ».
# ==============================================================================

groupe("v1.3 — page : langue, repere principal, icone (E1, E3, E8)")

app_env <- new.env()
sys.source("app.R", envir = app_env)
# Depuis la v1.4, `ui` est une fonction de la requete (lien partage) : on lui
# presente une requete sans parametre, c'est-a-dire l'ouverture habituelle.
# Depuis la v2.0, une requete sans profil donne la page d'entree : on ouvre le profil DSF,
# qui porte l'interface complete de la v1.x.
ui_app <- app_env$ui(list(QUERY_STRING = "?profil=dsf"))
rendu_app <- htmltools::renderTags(ui_app)
# <head> et <body> : les liens d'icone sont declares dans tags$head().
html_app <- paste(c(as.character(rendu_app$head), as.character(rendu_app$html)), collapse = "\n")

verifier(identical(attr(ui_app, "lang"), "fr"),
         "la page declare lang = « fr » (WCAG 3.1.1)")
verifier(grepl("<main[^>]*id=\"contenu\"", html_app),
         "le contenu est dans un repere <main id=\"contenu\">")
verifier(grepl("href=\"#contenu\"", html_app),
         "le lien d'evitement pointe vers ce repere")
verifier(grepl("rel=\"icon\"[^>]*favicon\\.png", html_app),
         "un favicon PNG (pictogramme du DSF) est declare")
verifier(file.exists(chemin_ci("www/favicon.png")),
         "le fichier favicon existe")


groupe("v1.3 — champs etiquetes, listes natives (E2)")

html_de <- function(ui) paste(as.character(htmltools::renderTags(ui)$html), collapse = "\n")

h_loc <- html_de(mod_localisation_ui("loc"))
verifier(grepl("<label[^>]*for=\"loc-commune\"[^>]*>.*Commune ou code postal", h_loc),
         "le champ commune a une etiquette reliee (pas seulement un placeholder)")

h_per <- html_de(mod_periode_ui("periode"))
verifier(!grepl("data-for=", h_per),
         "les listes de la periode sont des <select> natifs (pas de selectize)")
verifier(sum(gregexpr("<select", h_per)[[1]] > 0) == 4L,
         "les quatre listes de la periode (reference, fenetre, mois) sont presentes")

h_par <- html_de(mod_parametres_ui("parametres"))
verifier(grepl("<label[^>]*for=\"parametres-annees_charge\"", h_par),
         "le curseur d'etendue de la serie a une etiquette reliee")

h_rap <- html_de(mod_rapport_ui("rapport"))
verifier(!grepl("<h4", h_rap) && grepl("<h3 class=\"cf-bloc__titre\"", h_rap),
         "le rapport n'emploie plus de titre h4 sous un h2 (E7)")
verifier(!any(grepl("h4\\(", readLines(chemin_ci("src/mod_rapport.R"), warn = FALSE))),
         "mod_rapport.R ne construit aucun h4")

# Liste d'annees d'une carte graphique : on rend le bloc d'UI de l'indicateur.
# L'etiquette (for) et le <select> natif sont poses par mod_graphique_server ;
# on verifie donc le source plutot que de monter un serveur complet.
src_graph <- paste(readLines(chemin_ci("src/mod_graphique.R"), warn = FALSE), collapse = "\n")
verifier(grepl("selectInput\\(ns\\(\"annee\"\\)[^)]*selectize = FALSE", src_graph),
         "la liste d'annees des cartes est un <select> natif")
verifier(grepl("tags\\$label\\([^)]*`for` = ns\\(\"annee\"\\)", gsub("\n\\s*", " ", src_graph)),
         "elle est reliee a une etiquette (for) qui nomme le graphique")


groupe("v1.3 — cibles et sommaire (E4, E5), contenu des feuilles de style")

css <- paste(readLines(chemin_ci("www/climafor.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
verifier(grepl("input\\[type=\"radio\"\\], input\\[type=\"checkbox\"\\] \\{\\s*width: 24px; height: 24px", css),
         "cases et boutons radio d'au moins 24 px (WCAG 2.5.8)")
verifier(grepl("@media \\(pointer: coarse\\) \\{\\s*select, \\.form-control", css),
         "listes et champs a 44 px au doigt")
verifier(grepl("\\.cf-sommaire \\{", css) && grepl("aria-current=\"location\"", css),
         "le sommaire du bilan est stylise, avec l'etat « section courante »")
js <- paste(readLines(chemin_ci("www/app.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
verifier(grepl("IntersectionObserver", js) && grepl("aria-current", js),
         "le suivi de la section courante est present dans app.js")
regle_famille <- regmatches(css, regexpr("\\.cf-famille \\{[^}]*\\}", css))
verifier(length(regle_famille) == 1L && !grepl("scroll-margin-top", regle_famille),
         "pas de double decalage d'ancre sur les sections (html porte deja scroll-padding-top)")
