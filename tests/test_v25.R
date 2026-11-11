# ==============================================================================
# tests/test_v25.R — LES STATIONS SUR LES CARTES (VERSION 2.5)
# ------------------------------------------------------------------------------
# Ce que ces tests gardent :
#   1. une station designee sur une carte : le serveur ne prend du navigateur QUE l'identifiant,
#      et retrouve la position et le nom dans la table publiee qu'il a lue lui-meme ;
#   2. la station designee reste proposee comme source, meme quand le classement la place apres
#      les trois premieres (un pluviometre, desavantage faute de temperature) ;
#   3. la carte de l'Accueil : la case « stations », le clic sur une station (point + source), le
#      clic « sur la carte » qui l'accompagne ne deplace pas le point, le marqueur du point ne
#      fait plus disparaitre les stations ;
#   4. l'onglet Carte : le squelette et le script du navigateur disent les memes choses, le script
#      lit stations.json comme R, et refuse un fichier altere ;
#   5. l'ouverture de l'analyse d'une station depuis la Carte.
# Aucun test n'appelle Meteo-France : les stations viennent de outils/stations_demo.R (FICTIVES).
# Le dessin reel (Leaflet, MapLibre) est essaye a part, dans un navigateur (outils/apercu_tester.js).
# ==============================================================================

for (f in c("outils/carte_geo.R", "outils/carte_format.R", "outils/carte_precalcul.R",
            "outils/carte_demo.R", "outils/stations_precalcul.R", "outils/stations_demo.R")) source(chemin_ci(f))

lire25 <- function(f) paste(readLines(chemin_ci(f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")


# ==============================================================================
groupe("v2.5 — un jeu de mailles et de stations fictif")
# ==============================================================================

g25 <- expand.grid(i = 0:5, j = 0:5)
ll25 <- carte_depuis_l93(525000 + g25$i * 8000, 6280000 + g25$j * 8000)
an25 <- 1959:2025; n25 <- nrow(g25)
val25 <- lapply(seq_along(CARTE_COUCHES), function(k) {
  base <- k + 0.2 * g25$i + 0.1 * g25$j
  outer(seq_along(an25), seq_len(n25), function(a, c) base[c] + 0.03 * (a - 1) + 0.02 * sin(a / 3 + c))
})
names(val25) <- names(CARTE_COUCHES)
part_m25 <- list(annees = an25, indices = seq_len(n25), echecs = integer(0), valeurs = val25,
                 normale = lapply(val25, function(v) colMeans(v[an25 %in% 1991:2020, ])),
                 pente = lapply(val25, function(v) rep(0.3, n25)),
                 signif = lapply(val25, function(v) rep(TRUE, n25)))
dos25 <- tempfile("carte25")
carte_assembler(list(part_m25), ll25$lon, ll25$lat, an25, "jeu synthetique", dos25)

an_st25 <- 1985:2025
src_brut25 <- stations_demo_source(an_st25, fin = Sys.Date() - 1L, depts_connus = "31")
src25 <- function(dept) {
  b <- src_brut25(dept)
  if (is.null(b)) return(NULL)
  ref <- b[b$nom == "ANCIENNE 31", ][1, ]
  b$lon <- b$lon - ref$lon + 1.05
  b$lat <- b$lat - ref$lat + 43.66
  b
}
p_st25 <- stations_construire(src25, "31", an_st25, silencieux = TRUE)
stations_assembler(list(p_st25), an_st25, STATIONS_DEMO_SOURCE, dos25, n_min = 1L)

tr25 <- carte_transport(url = "", dossier = dos25)
rm(list = ls(.ETAT_COMPARER), envir = .ETAT_COMPARER)
tab25 <- stations_publiees_table(tr25)
verifier(is.data.frame(tab25) && nrow(tab25) == 3L && all(c("id", "nom", "lon", "lat", "alt") %in% names(tab25)),
         "la table des stations publiees se lit depuis le jeu de fichiers")
verifier(is.null(stations_publiees_table(list(mode = "absent", base = NULL, erreurs = "x"))) &&
           is.null(stations_publiees_table(NULL)),
         "sans jeu de fichiers : pas de table, et pas d'erreur")
ancien_actif25 <- CONFIG$stations$actif
CONFIG$stations$actif <- FALSE
verifier(is.null(stations_publiees_table(tr25)), "recherche de stations desactivee : aucune station n'est proposee")
CONFIG$stations$actif <- TRUE


# ==============================================================================
groupe("v2.5 — une station designee sur une carte")
# ==============================================================================

id25 <- tab25$id[1]
p25 <- stations_point_depuis_id(id25, tab25)
verifier(is.list(p25) && identical(p25$source, "station") && identical(p25$station, id25) &&
           isTRUE(all.equal(c(p25$lon, p25$lat), round(c(tab25$lon[1], tab25$lat[1]), 4))) &&
           identical(p25$nom, tab25$nom[1]),
         "un identifiant publie donne le point de la station : position et nom viennent de la table")
verifier(loc_point_valide(p25$lon, p25$lat), "le point d'une station est un point valide de l'application")
verifier(is.null(stations_point_depuis_id("ZZZ999", tab25)) && is.null(stations_point_depuis_id("00000000", tab25)),
         "un identifiant bien forme mais absent de la table : refuse")
for (forge in list(NULL, NA, NA_character_, 12345, TRUE, list("a"), c(id25, id25), "", "ab", "../../etc/passwd",
                   "<script>alert(1)</script>", strrep("A", 40), paste0(id25, "; DROP"), "id 25")) {
  if (!is.null(stations_point_depuis_id(forge, tab25))) {
    verifier(FALSE, sprintf("identifiant forge accepte a tort : %s", paste(format(forge), collapse = " ")))
  }
}
verifier(TRUE, "identifiants forges (vide, NA, nombre, liste, vecteur, chemin, HTML, trop long) : tous refuses")
verifier(is.null(stations_point_depuis_id(id25, NULL)) && is.null(stations_point_depuis_id(id25, tab25[0, ])) &&
           is.null(stations_point_depuis_id(id25, "pas une table")),
         "sans table lue par le serveur : refuse (on ne se fie jamais au navigateur)")
hors25 <- tab25; hors25$lat[1] <- 80
nan25 <- tab25; nan25$lon[1] <- NA
verifier(is.null(stations_point_depuis_id(id25, hors25)) && is.null(stations_point_depuis_id(id25, nan25)),
         "une table aux coordonnees impossibles ou absentes ne produit aucun point")


# ==============================================================================
groupe("v2.5 — la station designee reste proposee")
# ==============================================================================

# Un point au milieu de cinq stations : un pluviometre (pas de temperature : +15 km de
# penalite) place EXACTEMENT sur le point, et quatre stations completes a quelques km.
st_cl25 <- data.frame(
  id = c("P0000001", "S0000001", "S0000002", "S0000003", "S0000004"),
  nom = c("Pluviometre", "Station A", "Station B", "Station C", "Station D"),
  dept = "31", lon = c(1.40, 1.43, 1.37, 1.40, 1.46), lat = c(43.60, 43.60, 43.60, 43.63, 43.60),
  alt = 150, derniere_obs = Sys.Date() - 1L, fr_p = 1, fr_t = c(0, 1, 1, 1, 1),
  a_pluie = TRUE, a_temp = c(FALSE, TRUE, TRUE, TRUE, TRUE), stringsAsFactors = FALSE)
cl25 <- mf_classer_stations(st_cl25, 1.40, 43.60, 150)
verifier(nrow(cl25) == CONFIG$stations$n_propositions && !"P0000001" %in% cl25$id,
         "(controle) sans demande, le pluviometre exact est classe apres trois stations completes")
cl_f25 <- mf_classer_stations(st_cl25, 1.40, 43.60, 150, forcer = "P0000001")
verifier("P0000001" %in% cl_f25$id && nrow(cl_f25) == CONFIG$stations$n_propositions,
         "la station designee est proposee, sans allonger la liste")
verifier(!is.unsorted(cl_f25$pertinence), "la liste reste classee par pertinence")
verifier(identical(cl_f25$id[cl_f25$id != "P0000001"], cl25$id[seq_len(nrow(cl25) - 1L)]),
         "elle prend la place de la derniere : les meilleures restent")
verifier(identical(mf_classer_stations(st_cl25, 1.40, 43.60, 150, forcer = "S0000001")$id, cl25$id),
         "une station deja parmi les premieres : rien ne change")
verifier(identical(mf_classer_stations(st_cl25, 1.40, 43.60, 150, forcer = "NEXISTEPAS")$id, cl25$id) &&
           identical(mf_classer_stations(st_cl25, 1.40, 43.60, 150, forcer = c("a", "b"))$id, cl25$id) &&
           identical(mf_classer_stations(st_cl25, 1.40, 43.60, 150, forcer = NA_character_)$id, cl25$id),
         "une demande inconnue ou mal formee est ignoree")
loin25 <- rbind(st_cl25, transform(st_cl25[1, ], id = "P0000002", nom = "Trop loin", lon = 3.5, lat = 45.5))
verifier(!"P0000002" %in% mf_classer_stations(loin25, 1.40, 43.60, 150, forcer = "P0000002")$id,
         "une station hors du rayon de recherche n'est pas imposee")
verifier(identical(mf_classer_stations(st_cl25, 1.40, 43.60, 150, n = 1L, forcer = "P0000001")$id, "P0000001"),
         "avec une seule proposition, la station designee la remplace")


# ==============================================================================
groupe("v2.5 — la carte de l'Accueil")
# ==============================================================================

ancien_carte25 <- CONFIG$carte
CONFIG$carte$url_donnees <- ""
CONFIG$carte$dossier_local <- dos25
rm(list = ls(.ETAT_COMPARER), envir = .ETAT_COMPARER)

res_loc25 <- list()
tryCatch(shiny::testServer(mod_localisation_server, args = list(choix_station = shiny::reactiveVal(NULL)), {
  session$setInputs(stations = FALSE)   # etat initial de la case, comme dans le navigateur
  session$setInputs(stations = TRUE)
  p0 <- session$returned()
  res_loc25$sans_clic <<- is.null(p0)

  session$setInputs(carte_marker_click = list(id = id25, lat = 10, lng = 10, group = "stations"))
  p1 <- session$returned()
  res_loc25$point <<- p1
}), error = function(e) res_loc25$erreur <<- conditionMessage(e))

verifier(is.null(res_loc25$erreur), if (is.null(res_loc25$erreur)) "le module tourne avec une carte de stations"
         else paste("le module plante :", res_loc25$erreur))
verifier(isTRUE(res_loc25$sans_clic), "cocher « stations » ne choisit aucun point")
verifier(is.list(res_loc25$point) && identical(res_loc25$point$station, id25) &&
           isTRUE(all.equal(c(res_loc25$point$lon, res_loc25$point$lat), c(p25$lon, p25$lat))),
         "toucher une station pose son point, pris dans la table et non dans le clic (lat/lng du navigateur ignores)")
verifier(identical(res_loc25$point$source, "station"), "le point d'une station dit sa provenance")

# Source de donnees : la station est demandee a la recherche de stations ; un meme point ne la redemande pas.
choix25 <- shiny::reactiveVal(NULL)
res_c25 <- list()
tryCatch(shiny::testServer(mod_localisation_server, args = list(choix_station = choix25), {
  session$setInputs(stations = FALSE)   # etat initial de la case, comme dans le navigateur
  session$setInputs(stations = TRUE)
  session$setInputs(carte_marker_click = list(id = id25, lat = 0, lng = 0, group = "stations", .nonce = 1))
  res_c25$apres_clic <<- choix25()
  choix25(NULL)
  session$setInputs(carte_marker_click = list(id = id25, lat = 0, lng = 0, group = "stations", .nonce = 2))
  res_c25$meme_point <<- choix25()
  id2 <- tab25$id[2]
  session$setInputs(carte_marker_click = list(id = id2, lat = 0, lng = 0, group = "stations", .nonce = 3))
  res_c25$autre <<- choix25()
  res_c25$id2 <<- id2
}), error = function(e) res_c25$erreur <<- conditionMessage(e))
verifier(is.null(res_c25$erreur) && identical(res_c25$apres_clic, id25),
         "toucher une station la demande comme source des donnees")
verifier(is.null(res_c25$meme_point),
         "toucher deux fois la meme station : le point ne change pas, aucune demande ne reste en attente")
verifier(identical(res_c25$autre, res_c25$id2), "toucher une autre station : la demande suit")

# Clics refuses : identifiant forge, groupe du point, valeurs qui ne sont pas des listes.
res_r25 <- list()
choix_r25 <- shiny::reactiveVal(NULL)
tryCatch(shiny::testServer(mod_localisation_server, args = list(choix_station = choix_r25), {
  session$setInputs(stations = FALSE)   # etat initial de la case, comme dans le navigateur
  session$setInputs(stations = TRUE)
  for (k in seq_along(list(
    list(id = "ZZZ999", group = "stations"), list(id = "../../x", group = "stations"),
    list(id = id25, group = "point"), list(id = id25), list(id = NULL, group = "stations"),
    list(id = c(id25, id25), group = "stations"), "texte", 12, list(id = list(id25), group = "stations")))) {
    cl <- list(list(id = "ZZZ999", group = "stations"), list(id = "../../x", group = "stations"),
               list(id = id25, group = "point"), list(id = id25), list(id = NULL, group = "stations"),
               list(id = c(id25, id25), group = "stations"), "texte", 12, list(id = list(id25), group = "stations"))[[k]]
    session$setInputs(carte_marker_click = cl)
  }
  res_r25$point <<- session$returned()
  res_r25$choix <<- choix_r25()
}), error = function(e) res_r25$erreur <<- conditionMessage(e))
verifier(is.null(res_r25$erreur) && is.null(res_r25$point) && is.null(res_r25$choix),
         "identifiant forge, clic sur le point lui-meme, valeurs de forme inattendue : aucun point, aucune source")

# Sans stations lisibles : la case se decoche et le dit, sans erreur.
CONFIG$carte$dossier_local <- tempfile("vide25"); dir.create(CONFIG$carte$dossier_local)
rm(list = ls(.ETAT_COMPARER), envir = .ETAT_COMPARER)
res_v25 <- tryCatch({
  shiny::testServer(mod_localisation_server, { session$setInputs(stations = FALSE)   # etat initial de la case, comme dans le navigateur
  session$setInputs(stations = TRUE) })
  "ok"
}, error = function(e) conditionMessage(e))
verifier(identical(res_v25, "ok"), "aucun jeu de fichiers : cocher « stations » ne plante pas")
CONFIG$carte <- ancien_carte25

# Le code de la carte (Leaflet n'est qu'une maquette dans ce test) : ce qui ne s'execute pas ici se lit.
code_loc25 <- gsub("(?m)^\\s*#.*$", "", lire25("src/mod_localisation.R"), perl = TRUE)
verifier(!grepl("clearMarkers", code_loc25, fixed = TRUE),
         "le marqueur du point n'efface plus les stations : clearMarkers() n'est plus appele")
verifier(grepl('clearGroup("point")', code_loc25, fixed = TRUE) && grepl('group = "point"', code_loc25, fixed = TRUE) &&
           grepl('group = "stations"', code_loc25, fixed = TRUE),
         "le point et les stations ont chacun leur groupe")
verifier(grepl("bubblingMouseEvents = FALSE", code_loc25, fixed = TRUE) &&
           grepl("dernier$station", code_loc25, fixed = TRUE),
         "un clic sur une station ne devient pas un clic sur la carte (deux garde-fous)")
verifier(grepl("htmlEscape(tab$nom)", code_loc25, fixed = TRUE),
         "le nom d'une station, mis en etiquette HTML pour Leaflet, est echappe")
bloc_clic25 <- sub("(?s).*observeEvent\\(input\\$carte_marker_click, \\{(.*?)\\n    \\}\\).*", "\\1", code_loc25, perl = TRUE)
verifier(nchar(bloc_clic25) < 1500 && grepl("stations_point_depuis_id(clic$id, stations())", bloc_clic25, fixed = TRUE) &&
           !grepl("clic\\$lat|clic\\$lng", bloc_clic25),
         "le clic sur une station passe par la table du serveur : ni sa latitude ni sa longitude ne sont lues")
h_ui_acc25 <- as.character(mod_localisation_ui("loc", source_station = TRUE))
h_ui_ter25 <- as.character(mod_localisation_ui("loc"))
verifier(grepl('id="loc-stations"', h_ui_acc25, fixed = TRUE) && grepl("Afficher les stations Météo-France", h_ui_acc25, fixed = TRUE),
         "la case « Afficher les stations » est sous la carte")
verifier(!grepl("checked", sub('.*id="loc-stations"(.{0,60}).*', "\\1", h_ui_acc25)),
         "la case est decochee au depart : l'Accueil ne charge pas les stations sans qu'on le demande")
verifier(grepl("source des données", h_ui_acc25, fixed = TRUE) && !grepl("source des données", h_ui_ter25, fixed = TRUE),
         "la consigne ne parle de « source des donnees » que ou la station peut l'etre (Accueil)")
ui25 <- paste(lire25("src/ui_page.R"), collapse = "\n")
verifier(grepl('mod_localisation_ui("loc", source_station = TRUE)', ui25, fixed = TRUE),
         "l'Accueil declare que la station touchee devient la source des donnees")
app25 <- lire25("app.R")
verifier(grepl('mod_localisation_server("loc", choix_station = if (a_onglet("accueil")) choix_initial)', app25, fixed = TRUE) &&
           grepl("choix_initial = choix_initial", app25, fixed = TRUE),
         "app.R partage la demande de source entre la carte de l'Accueil et le serveur d'analyse")


# ==============================================================================
groupe("v2.5 — onglet Carte : squelette R et script du navigateur")
# ==============================================================================

js25 <- lire25("www/carte/carte.js")
html25 <- as.character(mod_carte_ui("carte"))
for (r in c("stations", "stations-bloc", "stations-note")) {
  verifier(grepl(sprintf('data-role="%s"', r), html25, fixed = TRUE) && grepl(sprintf('q("%s")', r), js25, fixed = TRUE),
           sprintf("le role « %s » existe dans le squelette R et dans carte.js", r))
}
verifier(grepl('data-role="stations-bloc"[^>]*hidden', html25), "le bloc des stations est masque jusqu'a ce que carte.js ait lu le fichier")
verifier(grepl('data-role="stations"[^>]*checked', html25), "sur la Carte, les stations sont affichees au depart")
nom_st_js25 <- regmatches(js25, regexec("(?m)^\\s*var NOM_STATION = /(.*)/;$", js25, perl = TRUE))[[1]][2]
verifier(identical(nom_st_js25, "^[A-Za-z0-9]{3,12}$"),
         "carte.js applique le meme motif d'identifiant que stations_meta_valider() et stations_point_depuis_id()")
bornes_js25 <- regmatches(js25, regexec("var BORNES = \\{ lon: \\[(.*?)\\], lat: \\[(.*?)\\] \\};", js25))[[1]]
verifier(identical(as.numeric(strsplit(bornes_js25[2], ",")[[1]]), PARTAGE_BORNES$lon) &&
           identical(as.numeric(strsplit(bornes_js25[3], ",")[[1]]), PARTAGE_BORNES$lat),
         "carte.js applique les memes bornes de territoire que PARTAGE_BORNES")
verifier(grepl(sprintf("var STATIONS_MAX = %d;", CONFIG$stations$publie$stations_max), js25, fixed = TRUE),
         "carte.js applique le meme plafond de stations que CONFIG$stations$publie$stations_max")
mod_carte25 <- lire25("src/mod_carte.R")
for (cle in c("stations", "schema_stations", "entree_station", "analyse_station")) {
  verifier(grepl(sprintf("%s = ", cle), mod_carte25, fixed = TRUE) &&
             grepl(sprintf("opts\\.%s|o\\.%s", cle, cle), js25),
           sprintf("l'option « %s » est envoyee par mod_carte_server() et lue par carte.js", cle))
}
verifier(grepl("stations_point_depuis_id", mod_carte25, fixed = TRUE),
         "mod_carte_server() retrouve la station dans la table du serveur")
verifier(!grepl("innerHTML|outerHTML|insertAdjacentHTML", gsub("(?s)/\\*.*?\\*/", "", js25, perl = TRUE)),
         "carte.js n'insere toujours aucun HTML (les noms de stations passent par textContent)")

if (nzchar(Sys.which("node"))) {
  js_st25 <- tryCatch(jsonlite::fromJSON(system2("node", c(shQuote(chemin_ci("tests/stations_verif_js.js")), shQuote(dos25)),
                                                 stdout = TRUE, stderr = FALSE), simplifyVector = FALSE),
                      error = function(e) NULL)
  verifier(!is.null(js_st25), "carte.js lit stations.json ecrit par R")
  if (!is.null(js_st25)) {
    verifier(js_st25$n == nrow(tab25) && identical(unlist(js_st25$id), tab25$id),
             "memes stations, dans le meme ordre, des deux cotes")
    verifier(identical(unlist(js_st25$nom), tab25$nom), "memes noms lisibles des deux cotes (R et navigateur)")
    verifier(presque_egal(unlist(js_st25$lon), tab25$lon, 1e-9) && presque_egal(unlist(js_st25$lat), tab25$lat, 1e-9),
             "memes coordonnees des deux cotes")
    verifier(identical(as.logical(unlist(js_st25$pluie)), tab25$a_pluie) && identical(as.logical(unlist(js_st25$temp)), tab25$a_temp),
             "les grandeurs mesurees (pluie, temperatures) sont lues de la meme facon")
    verifier(all(unlist(js_st25$rejets)),
             if (!all(unlist(js_st25$rejets))) paste("accepte a tort :", paste(names(js_st25$rejets)[!unlist(js_st25$rejets)], collapse = ", "))
             else "le navigateur refuse schema inconnu, fichier illisible, colonnes absentes ou inegales, identifiants pieges ou en double, coordonnees impossibles")
    verifier(isTRUE(js_st25$alt_nulle_reste_absente), "une altitude absente (null) reste absente : elle ne devient pas 0 m")
    verifier(isTRUE(js_st25$nom_borne), "un nom hostile reste un texte borne")
    verifier(identical(js_st25$nom_lisible, "La Roche-sur-Yon|Toulouse-Blagnac|Pic du Midi"),
             "la mise en forme des noms est celle de stations_nom_lisible()")
  }
} else {
  cat("   (Node absent : controle croise R / navigateur des stations ignore)\n")
}


# ==============================================================================
groupe("v2.5 — onglet Carte : analyser une station")
# ==============================================================================

CONFIG$carte$url_donnees <- ""
CONFIG$carte$dossier_local <- dos25
rm(list = ls(.ETAT_COMPARER), envir = .ETAT_COMPARER)
appels25 <- list()
res_oc25 <- tryCatch({
  shiny::testServer(mod_carte_server, args = list(
    onglet = shiny::reactive("carte"),
    ouvrir_maille = function(p) appels25$maille <<- p,
    ouvrir_station = function(p, id) appels25$station <<- list(p = p, id = id)), {
    session$flushReact()
    session$setInputs(clic_station = list(id = id25, lon = 99, lat = 99, nom = "FORGE"))
    session$setInputs(clic_station = list(id = "ZZZ999"))
    session$setInputs(clic_station = "texte")
  })
  "ok"
}, error = function(e) conditionMessage(e))
verifier(identical(res_oc25, "ok"), if (identical(res_oc25, "ok")) "mod_carte_server() traite les clics sur une station"
         else paste("mod_carte_server() plante :", res_oc25))
verifier(is.list(appels25$station) && identical(appels25$station$id, id25) &&
           identical(appels25$station$p$station, id25) &&
           isTRUE(all.equal(c(appels25$station$p$lon, appels25$station$p$lat), c(p25$lon, p25$lat))) &&
           identical(appels25$station$p$nom, tab25$nom[1]),
         "« Analyser cette station » ouvre l'analyse de CETTE station, a la position et sous le nom de la table (pas ceux du clic)")
verifier(is.null(appels25$maille), "l'analyse d'une station n'est pas confondue avec celle d'une maille")

# Profil sans analyse de station : la fiche s'affiche, le bouton non, et un clic forge ne fait rien.
appels_s25 <- 0L
tryCatch(shiny::testServer(mod_carte_server, args = list(
    onglet = shiny::reactive("carte"), ouvrir_maille = function(p) appels_s25 <<- appels_s25 + 1L), {
    session$flushReact()
    session$setInputs(clic_station = list(id = id25))
  }), error = function(e) NULL)
verifier(appels_s25 == 0L, "sans ouvrir_station, un clic sur une station n'ouvre rien")

# L'ouverture : la source demandee est celle de la station ; pas de nouvelle demande pour le meme point.
ancien_actif_o25 <- CONFIG$stations$actif
CONFIG$stations$actif <- TRUE
shiny::testServer(function(input, output, session) {
  t <- list(lieu = shiny::reactiveVal(NULL), choix = shiny::reactiveVal(NULL),
            src = shiny::reactiveVal(NULL), lances = shiny::reactiveVal(0L))
  t$ouvrir <- mod_carte_ouverture_server(t$lieu, t$choix, t$src,
                                         function() t$lances(shiny::isolate(t$lances()) + 1L))
  session$userData$t <- t
}, {
  t <- session$userData$t
  suppressMessages(t$ouvrir(p25, p25$station)); session$flushReact()
  verifier(identical(t$choix(), id25) && identical(t$lieu()$station, id25),
           "ouvrir une station demande SA source (et non SAFRAN) et pose son point")
  verifier(t$lances() == 0L, "l'analyse attend que la recherche de stations ait vu le nouveau point")
  t$src(list(point = list(lon = p25$lon, lat = p25$lat))); session$flushReact()
  verifier(t$lances() == 1L, "elle part des que la source se rapporte a la station")
  t$choix(NULL)
  suppressMessages(t$ouvrir(p25, p25$station)); session$flushReact()
  verifier(is.null(t$choix()), "meme station, meme point : aucune demande de source ne reste en attente")
  suppressMessages(t$ouvrir(list(lon = p25$lon, lat = p25$lat, nom = "Maille", source = "carte"))); session$flushReact()
  verifier(identical(t$choix(), "safran"), "ouvrir une maille au meme endroit demande bien SAFRAN (le point n'est plus le meme)")
})
CONFIG$stations$actif <- ancien_actif_o25
CONFIG$carte <- ancien_carte25
CONFIG$stations$actif <- ancien_actif25

verifier(grepl("forcer", lire25("src/mod_station.R"), fixed = TRUE),
         "la recherche de stations de l'Accueil recoit la station demandee (forcer)")
verifier(!grepl("indiquée dans le lien", lire25("src/mod_station.R"), fixed = TRUE),
         "le message d'une station non proposee ne parle plus d'un lien : la demande peut venir d'une carte")
