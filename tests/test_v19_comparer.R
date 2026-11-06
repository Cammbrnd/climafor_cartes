# ==============================================================================
# tests/test_v19_comparer.R — ONGLET « COMPARER » (VERSION 1.9)
# ------------------------------------------------------------------------------
# Ce que ces tests gardent :
#   1. l'import de points (CSV) : separateurs, encodages, bornes, noms ;
#   2. l'import de zones (GeoJSON, GeoPackage, shapefile zippe) et le refus des
#      archives piegees : un fichier importe est une entree non fiable ;
#   3. les fichiers de la carte lus par le serveur : telechargement borne, cache,
#      meta.json controle ;
#   4. le calcul (mailles, series, bilan, phrases, exports) sur des valeurs
#      qui se verifient a la main ;
#   5. le module Shiny, de bout en bout, et son raccord a l'application.
# ==============================================================================

for (f in c("outils/carte_geo.R", "outils/carte_format.R", "outils/carte_precalcul.R",
            "outils/carte_demo.R")) source(chemin_ci(f))

AVEC_SF19 <- requireNamespace("sf", quietly = TRUE)
AVEC_ZIP19 <- nzchar(Sys.which("zip"))

# Modifie un reglage le temps d'une expression.
avec_reglage19 <- function(champ, valeur, expr) {
  ancien <- CONFIG$comparer[[champ]]
  CONFIG$comparer[[champ]] <<- valeur
  on.exit(CONFIG$comparer[[champ]] <<- ancien, add = TRUE)
  force(expr)
}

ecrire19 <- function(txt = NULL, ext = ".csv", brut = NULL) {
  f <- tempfile(fileext = ext)
  if (is.null(brut)) writeLines(txt, f, useBytes = TRUE) else writeBin(brut, f)
  f
}


# ==============================================================================
groupe("v1.9 — comparer : import de points (CSV)")
# ==============================================================================

r <- cmp_importer(ecrire19(c("nom;latitude;longitude", "Forêt de Bouconne;43,65;1,17",
                             "Massif de l'Aigoual;44,12;3,58")), "points.csv")
verifier(length(r$lieux) == 2 && !length(r$problemes) &&
           identical(r$lieux[[1]]$nom, "Forêt de Bouconne") &&
           presque_egal(c(r$lieux[[1]]$lat, r$lieux[[1]]$lon), c(43.65, 1.17)),
         "CSV a point-virgule et virgule decimale : noms accentues et coordonnees lus")

r <- cmp_importer(ecrire19(c("Name,Lat,Lon", "\"Parcelle 3, nord\",43.65,1.17", "B,44.12,3.58")), "p.csv")
verifier(length(r$lieux) == 2 && identical(r$lieux[[1]]$nom, "Parcelle 3, nord") &&
           presque_egal(r$lieux[[2]]$lon, 3.58),
         "CSV a virgule : noms entre guillemets et colonnes anglaises reconnus")

r <- cmp_importer(ecrire19(c("nom\tx\ty", "A\t1,17\t43,65")), "p.tsv")
verifier(length(r$lieux) == 1 && presque_egal(c(r$lieux[[1]]$lat, r$lieux[[1]]$lon), c(43.65, 1.17)),
         "CSV a tabulation : x = longitude, y = latitude")

lat1 <- iconv("nom;latitude;longitude\nForêt d'Écouves;48,5;0,1\n", "UTF-8", "latin1", toRaw = TRUE)[[1]]
r <- cmp_importer(ecrire19(brut = lat1), "latin1.csv")
verifier(length(r$lieux) == 1 && identical(r$lieux[[1]]$nom, "Forêt d'Écouves"),
         "CSV enregistre en Latin-1 : les accents survivent")

bom <- c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("nom;latitude;longitude\nA;43,65;1,17\n"))
r <- cmp_importer(ecrire19(brut = bom), "bom.csv")
verifier(length(r$lieux) == 1 && identical(r$lieux[[1]]$nom, "A"),
         "CSV avec marque d'ordre des octets (export Excel) : la premiere colonne est reconnue")

r <- cmp_importer(ecrire19(c("latitude;longitude", "43,65;1,17", "44,1;3,5")), "p.csv")
verifier(length(r$lieux) == 2 && identical(r$lieux[[2]]$nom, "Point 2"),
         "sans colonne de nom, les points sont numerotes")

r <- cmp_importer(ecrire19(c("# provenance : export SIG", "nom;latitude;longitude", "A;43,65;1,17")), "p.csv")
verifier(length(r$lieux) == 1, "les lignes de commentaire (#) sont ignorees")

r <- cmp_importer(ecrire19(c("nom;latitude;longitude", "Inverse;1,17;43,65", "New York;40,7;-74", "Ok;43,65;1,17")), "p.csv")
verifier(length(r$lieux) == 1 && length(r$problemes) == 2 &&
           any(grepl("inversées", r$problemes)) && any(grepl("Ligne 3", r$problemes)),
         "latitude et longitude inversees, ou hors de France : point ecarte avec la ligne en cause")

r <- cmp_importer(ecrire19(c("nom;latitude;longitude", "A;abc;1,17", "B;;", "C;43,65;1,17")), "p.csv")
verifier(length(r$lieux) == 1 && length(r$problemes) == 2 && any(grepl("illisibles", r$problemes)),
         "coordonnees illisibles ou vides : ecartees une a une, sans arreter l'import")

r <- cmp_importer(ecrire19(c("nom;commune;altitude", "A;Toulouse;150")), "p.csv")
verifier(!length(r$lieux) && any(grepl("latitude", r$problemes)),
         "sans colonnes de coordonnees : un message qui dit lesquelles attendre")

r0 <- cmp_importer(ecrire19(character(0)), "vide.csv")
r1 <- cmp_importer(ecrire19("nom;latitude;longitude"), "entete.csv")
verifier(!length(r0$lieux) && length(r0$problemes) == 1 && !length(r1$lieux) && length(r1$problemes) == 1,
         "fichier vide ou reduit a l'en-tete : un message, pas d'erreur")

grand <- c("nom;latitude;longitude", sprintf("P%d;%.4f;%.4f", 1:520, 43 + (1:520) / 1000, 1 + (1:520) / 1000))
r <- cmp_importer(ecrire19(grand), "grand.csv")
verifier(length(r$lieux) == CONFIG$comparer$lignes_max && any(grepl("Plus de 500 lignes", r$problemes)),
         "plus de 500 lignes : les 500 premieres sont lues, le message le dit")

r <- avec_reglage19("fichier_max_mo", 0.0001, cmp_importer(ecrire19(grand), "grand.csv"))
verifier(!length(r$lieux) && any(grepl("trop volumineux", r$problemes)),
         "un fichier au-dela de la taille permise est refuse avant d'etre lu")

r <- cmp_importer(ecrire19("MZ", ".docx"), "rapport.docx")
verifier(!length(r$lieux) && any(grepl("non reconnu", r$problemes)),
         "une extension inconnue est refusee avec la liste des formats acceptes")

nom_dur <- cmp_nom_propre(paste0("a\u0007b\n\tc", strrep("x", 100)))
verifier(!grepl("[[:cntrl:]]", nom_dur) && nchar(nom_dur) <= 60 && startsWith(nom_dur, "a b c"),
         "un nom de lieu perd ses caracteres de controle et tient en 60 caracteres")
verifier(identical(cmp_nom_propre("  ", "Zone"), "Zone") && identical(cmp_nom_propre(NA, "Zone"), "Zone"),
         "un nom vide ou absent devient le nom par defaut")

pts20 <- lapply(c("A", "A", "A", "B"), function(n) cmp_lieu_point(n, 1.17, 43.65))
aj <- cmp_ajouter_lieux(list(), pts20)
verifier(identical(vapply(aj$lieux, function(l) l$nom, ""), c("A", "A (2)", "A (3)", "B")),
         "deux lieux de meme nom restent distincts (« A », « A (2) »...)")
pts45 <- lapply(1:45, function(i) cmp_lieu_point(paste0("P", i), 1.17, 43.65))
aj <- cmp_ajouter_lieux(list(), pts45)
verifier(length(aj$lieux) == CONFIG$comparer$lieux_max && length(aj$problemes) == 1 &&
           grepl("Limite de 40", aj$problemes),
         "le nombre de lieux est borne, avec un message")

m_csv <- cmp_modele_csv()
f_m <- tempfile(fileext = ".csv"); ecrire_csv_avec_metadonnees(m_csv, f_m, character(0))
r <- cmp_importer(f_m, "modele_points.csv")
verifier(length(r$lieux) == nrow(m_csv) && !length(r$problemes),
         "le modele de CSV proposé se reimporte tel quel")


# ==============================================================================
groupe("v1.9 — comparer : import de zones et archives piegees")
# ==============================================================================

poly19 <- function(lon0, lat0, d = 0.1) {
  sf::st_polygon(list(rbind(c(lon0, lat0), c(lon0 + d, lat0), c(lon0 + d, lat0 + d),
                            c(lon0, lat0 + d), c(lon0, lat0))))
}

# Archive ZIP minimale, ecrite a la main : noms et tailles DECLAREES a volonte (les
# tests ne creent ni vrai depassement de chemin ni vraie bombe).
# CRC-32 de « abcd » (0xED82CD11), en entier signe : le contenu ecrit pour chaque entree.
CRC_ABCD <- -310194927L
zip_minimal19 <- function(fichier, noms, tailles = rep(4, length(noms))) {
  con <- rawConnection(raw(0), "wb"); on.exit(close(con))
  w2 <- function(x) writeBin(as.integer(x), con, size = 2L, endian = "little")
  w4 <- function(x) writeBin(as.integer(x), con, size = 4L, endian = "little")
  pos <- function() length(rawConnectionValue(con))
  decal <- integer(length(noms))
  for (i in seq_along(noms)) {
    decal[i] <- pos(); nm <- charToRaw(noms[i])
    w4(0x04034b50); w2(20); w2(0); w2(0); w2(0); w2(0); w4(CRC_ABCD); w4(4); w4(tailles[i])
    w2(length(nm)); w2(0); writeBin(nm, con); writeBin(charToRaw("abcd"), con)
  }
  debut <- pos()
  for (i in seq_along(noms)) {
    nm <- charToRaw(noms[i])
    w4(0x02014b50); w2(20); w2(20); w2(0); w2(0); w2(0); w2(0); w4(CRC_ABCD); w4(4); w4(tailles[i])
    w2(length(nm)); w2(0); w2(0); w2(0); w2(0); w4(0); w4(decal[i]); writeBin(nm, con)
  }
  fin <- pos()
  w4(0x06054b50); w2(0); w2(0); w2(length(noms)); w2(length(noms)); w4(fin - debut); w4(debut); w2(0)
  writeBin(rawConnectionValue(con), fichier)
  fichier
}

z_ok <- zip_minimal19(tempfile(fileext = ".zip"), c("a.shp", "a.shx", "a.dbf", "a.prj"))
verifier(identical(sort(utils::unzip(z_ok, list = TRUE)$Name), c("a.dbf", "a.prj", "a.shp", "a.shx")),
         "(controle) l'archive minimale ecrite a la main se liste normalement")
echec_zip19 <- function(noms, tailles = rep(4, length(noms))) {
  z <- zip_minimal19(tempfile(fileext = ".zip"), noms, tailles)
  d <- tempfile("sortie")
  tryCatch({ cmp_extraire_zip(z, d); "extrait" }, error = function(e) conditionMessage(e))
}
verifier(grepl("chemins refusés", echec_zip19(c("../evil.shp", "a.shx"))) &&
           grepl("chemins refusés", echec_zip19(c("/tmp/evil.shp", "a.dbf"))) &&
           grepl("chemins refusés", echec_zip19(c("C:/evil.shp"))) &&
           grepl("chemins refusés", echec_zip19(c("sub\\evil.shp"))) &&
           grepl("chemins refusés", echec_zip19(c("a/../../evil.shp"))),
         "archive : « .. », chemin absolu, lecteur Windows et antislash sont refuses")
verifier(grepl("trop riche", echec_zip19(sprintf("f%d.dbf", 1:31))),
         "archive : plus de 30 fichiers refuse")
verifier(grepl("trop volumineuse", echec_zip19(c("a.shp", "a.dbf"), c(4, 150 * 1024^2))),
         "archive : taille decompressee declaree au-dela de 100 Mo refusee (bombe)")
verifier(grepl("un seul fichier .shp", echec_zip19(c("a.dbf", "a.prj"))) &&
           grepl("un seul fichier .shp", echec_zip19(c("a.shp", "b.shp"))),
         "archive : aucun ou deux .shp refuses")
d_sortie <- tempfile("sortie")
cmp_extraire_zip(zip_minimal19(tempfile(fileext = ".zip"), c("a.shp", "a.dbf", "run.sh", "notes/lisez.txt")), d_sortie)
verifier(identical(sort(list.files(d_sortie)), c("a.dbf", "a.shp")),
         "archive : seuls .shp, .shx, .dbf, .prj et .cpg sont extraits")

if (!AVEC_SF19) {
  r <- cmp_importer(ecrire19("{}", ".geojson"), "z.geojson")
  verifier(!length(r$lieux) && any(grepl("sf", r$problemes)),
           "sans le paquet sf, l'import de zones l'explique (les points restent permis)")
} else {
  r <- cmp_importer(zip_minimal19(tempfile(fileext = ".zip"), c("../evil.shp")), "z.zip")
  verifier(!length(r$lieux) && any(grepl("Archive refusée", r$problemes)),
           "l'import d'une archive piegee rend un message, jamais une erreur")

  zones19 <- sf::st_sf(nom = c("Forêt A", "Massif B"),
                       geometry = sf::st_sfc(poly19(1.0, 43.5), poly19(1.3, 43.5), crs = 4326))

  # -- GeoJSON ------------------------------------------------------------------
  f <- tempfile(fileext = ".geojson"); sf::st_write(zones19, f, quiet = TRUE)
  r <- cmp_importer(f, "zones.geojson")
  verifier(length(r$lieux) == 2 && all(vapply(r$lieux, function(l) l$type, "") == "zone") &&
             identical(vapply(r$lieux, function(l) l$nom, ""), c("Forêt A", "Massif B")),
           "GeoJSON de polygones : deux zones, nommees d'apres la colonne « nom »")

  mixte <- paste0(
    '{"type":"FeatureCollection","features":[',
    '{"type":"Feature","properties":{"NOM":"Zone ok"},"geometry":{"type":"Polygon","coordinates":[[[1,43.5],[1.1,43.5],[1.1,43.6],[1,43.6],[1,43.5]]]}},',
    '{"type":"Feature","properties":{"NOM":"Pt ok"},"geometry":{"type":"Point","coordinates":[1.2,43.6]}},',
    '{"type":"Feature","properties":{"NOM":"Pt hors"},"geometry":{"type":"Point","coordinates":[-74,40.7]}},',
    '{"type":"Feature","properties":{"NOM":"Ligne"},"geometry":{"type":"LineString","coordinates":[[1,43],[1.1,43.1]]}}]}')
  r <- cmp_importer(ecrire19(mixte, ".geojson"), "mixte.geojson")
  verifier(identical(vapply(r$lieux, function(l) l$nom, ""), c("Zone ok", "Pt ok")) &&
             identical(vapply(r$lieux, function(l) l$type, ""), c("zone", "point")) &&
             length(r$problemes) == 2 && any(grepl("hors de la France", r$problemes)) &&
             any(grepl("linestring", r$problemes)),
           "GeoJSON mixte : zone et point gardes, point hors de France et ligne ecartes avec leur raison")

  noeud <- paste0(
    '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{},"geometry":',
    '{"type":"Polygon","coordinates":[[[1,43.5],[1.1,43.6],[1.1,43.5],[1,43.6],[1,43.5]]]}}]}')
  r <- cmp_importer(ecrire19(noeud, ".geojson"), "noeud.geojson")
  verifier(length(r$lieux) == 1 && identical(r$lieux[[1]]$nom, "Zone 1"),
           "polygone auto-intersecte (nœud papillon) : repare, nom par defaut « Zone 1 »")

  poly_l93 <- sf::st_transform(sf::st_sfc(poly19(1.0, 43.5), crs = 4326), 2154)
  xy <- sf::st_coordinates(poly_l93)[, 1:2]
  json_l93 <- sprintf(paste0(
    '{"type":"FeatureCollection","crs":{"type":"name","properties":{"name":"urn:ogc:def:crs:EPSG::2154"}},',
    '"features":[{"type":"Feature","properties":{"nom":"L93"},"geometry":{"type":"Polygon","coordinates":[[%s]]}}]}'),
    paste(sprintf("[%.2f,%.2f]", xy[, 1], xy[, 2]), collapse = ","))
  r <- cmp_importer(ecrire19(json_l93, ".geojson"), "l93.geojson")
  bb <- if (length(r$lieux)) as.numeric(sf::st_bbox(r$lieux[[1]]$geom)) else rep(NA_real_, 4)
  verifier(length(r$lieux) == 1 && presque_egal(bb, c(1.0, 43.5, 1.1, 43.6), tolerance = 2e-3),
           "GeoJSON declare en Lambert-93 : reprojete en WGS84 au bon endroit")

  r <- cmp_importer(ecrire19("ceci n'est pas du JSON", ".geojson"), "faux.geojson")
  verifier(!length(r$lieux) && any(grepl("n'a pas pu être lu", r$problemes)),
           "un faux .geojson donne un message, pas une erreur")

  cinq <- sf::st_sf(nom = paste0("Z", 1:5),
                    geometry = sf::st_sfc(lapply(1:5, function(i) poly19(1 + i / 10, 43.5)), crs = 4326))
  f5 <- tempfile(fileext = ".geojson"); sf::st_write(cinq, f5, quiet = TRUE)
  r <- avec_reglage19("entites_max", 3L, cmp_importer(f5, "cinq.geojson"))
  verifier(length(r$lieux) == 3 && any(grepl("Plus de 3 entités", r$problemes)),
           "le nombre d'entites lues est borne, avec un message")

  # -- GeoPackage -----------------------------------------------------------------
  f <- tempfile(fileext = ".gpkg")
  sf::st_write(sf::st_transform(zones19, 2154), f, layer = "zones", quiet = TRUE)
  r <- cmp_importer(f, "zones.gpkg")
  bb <- if (length(r$lieux)) as.numeric(sf::st_bbox(r$lieux[[1]]$geom)) else rep(NA_real_, 4)
  verifier(length(r$lieux) == 2 && presque_egal(bb, c(1.0, 43.5, 1.1, 43.6), tolerance = 2e-3),
           "GeoPackage en Lambert-93 : lu et reprojete en WGS84")
  sf::st_write(sf::st_sf(nom = "P", geometry = sf::st_sfc(sf::st_point(c(1.2, 43.6)), crs = 4326)),
               f, layer = "points", quiet = TRUE)
  r <- cmp_importer(f, "deux.gpkg")
  verifier(length(r$lieux) == 2 && any(grepl("2 couches", r$problemes)),
           "GeoPackage a plusieurs couches : la premiere est lue, le message le dit")

  # -- Shapefile zippe -------------------------------------------------------------
  if (!AVEC_ZIP19) {
    cat("  (zip absent : tests de shapefile zippe passes)\n")
  } else {
    zipper19 <- function(dossier, nom = "z.zip") {
      z <- file.path(tempfile("zip"), nom); dir.create(dirname(z))
      f <- list.files(dossier, full.names = TRUE)
      suppressWarnings(system2("zip", c("-j", "-q", shQuote(z), shQuote(f)), stdout = FALSE, stderr = FALSE))
      z
    }
    d_shp <- tempfile("shp"); dir.create(d_shp)
    sf::st_write(zones19, file.path(d_shp, "zones.shp"), quiet = TRUE)
    z <- zipper19(d_shp)
    r <- cmp_importer(z, "zones.zip")
    verifier(length(r$lieux) == 2 && identical(vapply(r$lieux, function(l) l$nom, ""), c("Forêt A", "Massif B")),
             "shapefile zippe : deux zones, accents conserves")

    unlink(file.path(d_shp, "zones.prj"))
    r <- cmp_importer(zipper19(d_shp), "sans_prj.zip")
    verifier(!length(r$lieux) && any(grepl("Système de coordonnées absent", r$problemes)),
             "shapefile sans .prj : refuse, l'emplacement ne se devine pas")

    sf::st_write(zones19, file.path(d_shp, "zones.shp"), quiet = TRUE, delete_layer = TRUE)
    sf::st_write(zones19, file.path(d_shp, "autre.shp"), quiet = TRUE)
    r <- cmp_importer(zipper19(d_shp), "deux_shp.zip")
    verifier(!length(r$lieux) && any(grepl("un seul fichier .shp", r$problemes)),
             "archive a deux shapefiles : refusee, on ne choisit pas pour l'utilisateur")
  }
}


# ==============================================================================
groupe("v1.9 — comparer : fichiers de la carte (telechargement, cache, controle)")
# ==============================================================================

# Jeu synthetique : 6 x 6 mailles de 8 km autour de Toulouse (Lambert-93).
x0_19 <- 525000; y0_19 <- 6280000
g19 <- expand.grid(i = 0:5, j = 0:5)
ll19 <- carte_depuis_l93(x0_19 + g19$i * 8000, y0_19 + g19$j * 8000)
an19 <- 1959:2025; n19 <- nrow(g19)
val19 <- lapply(seq_along(CARTE_COUCHES), function(k) {
  base <- k + 0.2 * g19$i + 0.1 * g19$j
  outer(seq_along(an19), seq_len(n19), function(a, c) base[c] + 0.03 * (a - 1) + 0.02 * sin(a / 3 + c))
})
names(val19) <- names(CARTE_COUCHES)
part19 <- list(annees = an19, indices = seq_len(n19), echecs = integer(0), valeurs = val19,
               normale = lapply(val19, function(v) colMeans(v[an19 %in% 1991:2020, ])),
               pente = lapply(val19, function(v) rep(0.3, n19)),
               signif = lapply(val19, function(v) rep(TRUE, n19)))
dos19 <- tempfile("carte19")
carte_assembler(list(part19), ll19$lon, ll19$lat, an19, "jeu synthetique", dos19)
tr19 <- carte_transport(url = "", dossier = dos19)
verifier(identical(tr19$mode, "local"), "(controle) le jeu synthetique se publie et se retrouve en local")

jeu19 <- cmp_jeu(tr19)
verifier(identical(jeu19$annees, an19) && nrow(jeu19$centres) == n19 &&
           max(abs(jeu19$centres$lon - ll19$lon)) < 1e-4 && max(abs(jeu19$centres$lat - ll19$lat)) < 1e-4 &&
           identical(names(cmp_couches_disponibles(jeu19)), names(CARTE_COUCHES)),
         "le jeu rend ses annees, ses centres de mailles et ses couches")

# -- Transport distant, sans reseau : un executeur qui sert le dossier local ----
BASE19 <- "https://cammbrnd.github.io/climafor_cartes/"
nettoyer19 <- function() {
  rm(list = ls(.ETAT_COMPARER), envir = .ETAT_COMPARER)
  unlink(file.path(tempdir(), "climafor_carte"), recursive = TRUE)
}
serveur19 <- function(compteur, dossier = dos19, hote_final = NULL, statut = NULL) {
  function(req, path) {
    compteur$n <- compteur$n + 1L; compteur$urls <- c(compteur$urls, req$url)
    f <- file.path(dossier, basename(req$url))
    if (!file.exists(f)) return(httr2::response(status_code = 404L, url = req$url))
    file.copy(f, path, overwrite = TRUE)
    httr2::response(status_code = statut %||% 200L,
                    url = if (is.null(hote_final)) req$url else sub("^https://[^/]+", paste0("https://", hote_final), req$url))
  }
}
nouveau19 <- function() { e <- new.env(); e$n <- 0L; e$urls <- character(0); e }

nettoyer19()
trd <- carte_transport(url = BASE19, dossier = "inexistant")
c1 <- nouveau19(); ex1 <- serveur19(c1)
jd <- cmp_jeu(trd, executer = ex1)
verifier(identical(trd$mode, "distant") && c1$n == 2L && identical(jd$annees, an19) &&
           all(startsWith(c1$urls, BASE19)) && max(abs(jd$centres$lon - ll19$lon)) < 1e-4,
         "jeu distant : meta.json et grille.json telecharges (2 requetes), memes centres qu'en local")
cmp_jeu(trd, executer = ex1)
verifier(c1$n == 2L, "le jeu est garde en memoire : un second appel ne telecharge rien")
cmp_jeu(trd, maintenant = Sys.time() + 25 * 3600, executer = ex1)
verifier(c1$n == 4L, "au-dela de 24 h, le jeu et ses fichiers sont retelecharges")

nettoyer19()
c2 <- nouveau19()
cmp_fichier_carte("meta.json", trd, executer = serveur19(c2))
cmp_fichier_carte("meta.json", trd, executer = serveur19(c2))
cmp_fichier_carte("meta.json", trd, maintenant = Sys.time() + 25 * 3600, executer = serveur19(c2))
verifier(c2$n == 2L, "cache de fichiers : valide 24 h, puis renouvele")

nettoyer19()
c3 <- nouveau19()
s3 <- cmp_series(jd, "A", list(list(idx = 1L)), "tmp", executer = serveur19(c3))
s3b <- cmp_series(jd, "A", list(list(idx = 1L)), "tmp", executer = serveur19(c3))
verifier(c3$n == 1L && nrow(s3) == length(an19) && identical(s3, s3b),
         "une couche distante est telechargee une fois, puis lue dans le cache")

refus19 <- function(url, base = BASE19, exec = serveur19(nouveau19())) {
  d <- file.path(tempfile("dl"), "x.bin"); dir.create(dirname(d))
  tryCatch({ cmp_telecharger(url, d, base, exec); if (file.exists(d)) "telecharge" else "rien" },
           error = function(e) conditionMessage(e))
}
c4 <- nouveau19()
verifier(grepl("refusée", refus19("https://evil.example.org/meta.json", exec = serveur19(c4))) && c4$n == 0L &&
           grepl("refusée", refus19("http://cammbrnd.github.io/climafor_cartes/meta.json", base = "http://cammbrnd.github.io/climafor_cartes/")),
         "telechargement : hors de l'adresse de base, ou en http, refuse sans envoyer de requete")
verifier(grepl("autre hôte", refus19(paste0(BASE19, "meta.json"), exec = serveur19(nouveau19(), hote_final = "evil.example.org"))),
         "telechargement : une redirection vers un autre hote est refusee, rien n'est conserve")
verifier(grepl("HTTP 404", refus19(paste0(BASE19, "absent.json"))),
         "telechargement : une reponse 404 est une erreur claire")
verifier(grepl("trop volumineux", avec_reglage19("taille_carte_max_mo", 1e-6, refus19(paste0(BASE19, "meta.json")))),
         "telechargement : un fichier au-dela de la taille permise est refuse")
verifier(!carte_nom_fichier_sur("../meta.json") &&
           inherits(tryCatch(cmp_fichier_carte("../meta.json", trd), error = function(e) e), "error"),
         "un nom de fichier qui sort du dossier est refuse avant toute lecture")

# -- meta.json controle ----------------------------------------------------------
dos_mauvais <- tempfile("carte_mauvaise"); dir.create(dos_mauvais)
invisible(file.copy(list.files(dos19, full.names = TRUE), dos_mauvais))
meta_m <- jsonlite::fromJSON(file.path(dos_mauvais, "meta.json"), simplifyVector = FALSE)
meta_m$grille <- "../grille.json"
jsonlite::write_json(meta_m, file.path(dos_mauvais, "meta.json"), auto_unbox = TRUE)
tr_m <- carte_transport(url = "", dossier = dos_mauvais)
verifier(inherits(tryCatch(cmp_jeu(tr_m), error = function(e) e), "error"),
         "un meta.json qui designe un fichier hors du dossier est refuse")
verifier(inherits(tryCatch(cmp_jeu(list(mode = "absent", base = NULL, erreurs = "x")), error = function(e) e), "error"),
         "sans donnees de carte, cmp_jeu echoue proprement (le module l'affiche)")


# ==============================================================================
groupe("v1.9 — comparer : mailles, series, bilan et exports")
# ==============================================================================

if (!AVEC_SF19) {
  cat("  (sf absent : tests de rattachement aux mailles passes)\n")
} else {
  k19 <- which(g19$i == 2 & g19$j == 3)
  p_centre <- cmp_lieu_point("Centre", ll19$lon[k19], ll19$lat[k19])
  m_centre <- cmp_mailles(p_centre, jeu19)
  verifier(identical(m_centre$idx, k19) && grepl("0,0 km", m_centre$note),
           "un point au centre d'une maille prend cette maille")
  m_loin <- cmp_mailles(cmp_lieu_point("Loin", ll19$lon[k19] + 0.5, ll19$lat[k19] + 0.5), jeu19)
  verifier(!length(m_loin$idx) && grepl("hors de la grille", m_loin$note),
           "un point a plus de 6 km de toute maille est dit hors de la grille")
  m_pres <- cmp_mailles(cmp_lieu_point("Pres", ll19$lon[k19] + 0.02, ll19$lat[k19]), jeu19)
  verifier(identical(m_pres$idx, k19), "un point proche d'un centre prend la maille la plus proche")

  rect <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(x0_19 + 7000, y0_19 + 7000), c(x0_19 + 17000, y0_19 + 7000), c(x0_19 + 17000, y0_19 + 17000),
    c(x0_19 + 7000, y0_19 + 17000), c(x0_19 + 7000, y0_19 + 7000)))), crs = 2154)
  z_2x2 <- cmp_lieu_zone("Zone 2x2", sf::st_transform(rect, 4326))
  m_zone <- cmp_mailles(z_2x2, jeu19)
  verifier(length(m_zone$idx) == 4 && setequal(paste(g19$i[m_zone$idx], g19$j[m_zone$idx]), c("1 1", "1 2", "2 1", "2 2")),
           "une zone prend les mailles dont le centre est dedans (ici 2 x 2)")
  pet <- sf::st_sfc(sf::st_buffer(sf::st_point(c(x0_19 + 4000, y0_19 + 4000)), 500), crs = 2154)
  m_pet <- cmp_mailles(cmp_lieu_zone("Petite", sf::st_transform(pet, 4326)), jeu19)
  verifier(length(m_pet$idx) == 1 && grepl("plus petite qu'une maille", m_pet$note),
           "une zone plus petite qu'une maille prend la maille la plus proche, et le dit")
  vaste <- sf::st_sfc(sf::st_buffer(sf::st_point(c(x0_19, y0_19)), 2e6), crs = 2154)
  m_vaste <- avec_reglage19("mailles_zone_max", 10L, cmp_mailles(cmp_lieu_zone("Vaste", sf::st_transform(vaste, 4326)), jeu19))
  verifier(!length(m_vaste$idx) && grepl("trop grande", m_vaste$note),
           "une zone de trop de mailles est refusee avec une raison")

  lieux19 <- list(p_centre, z_2x2, cmp_lieu_point("Loin", ll19$lon[k19] + 0.5, ll19$lat[k19] + 0.5))
  mail19 <- lapply(lieux19, cmp_mailles, jeu = jeu19)
  tl19 <- cmp_table_lieux(lieux19, mail19)
  verifier(identical(tl19$lieu, c("Centre", "Zone 2x2", "Loin")) && identical(tl19$n_mailles, c(1L, 4L, 0L)) &&
             identical(tl19$retenu, c(TRUE, TRUE, FALSE)),
           "le tableau des lieux dit combien de mailles chacun retient, et ecarte celui qui n'en a pas")

  ser19 <- cmp_series(jeu19, tl19$lieu, mail19, c("tmp", "vag"))
  verifier(setequal(ser19$lieu, c("Centre", "Zone 2x2")) && nrow(ser19) == 2 * 2 * length(an19),
           "les series ne portent que les lieux retenus (2 lieux x 2 grandeurs x 67 annees)")
  direct <- rowMeans(carte_lire_couche(file.path(dos19, "c_tmp.bin.gz"), n19, length(an19), 2)$valeurs[, m_zone$idx])
  verifier(presque_egal(ser19$valeur[ser19$lieu == "Zone 2x2" & ser19$cle == "tmp"], direct, 1e-9),
           "la serie d'une zone est la moyenne annee par annee de ses mailles")

  # -- Bilan : valeurs qui se calculent a la main --------------------------------------
  ctx19 <- ctx_creer(ref = c(1991, 2020), affichage = 1959:2025, courante = 2025)
  lin <- data.frame(lieu = "Droite", cle = "tmp", annee = an19, valeur = 10 + 0.03 * (an19 - 1959),
                    stringsAsFactors = FALSE)
  b <- cmp_bilan(lin, ctx19, 2025)
  nor <- 10 + 0.03 * (mean(1991:2020) - 1959)
  verifier(presque_egal(b$normale, nor, 1e-9) && presque_egal(b$valeur, 10 + 0.03 * 66, 1e-9) &&
             presque_egal(b$ecart, 0.03 * 66 - 0.03 * (mean(1991:2020) - 1959), 1e-9) &&
             identical(b$classe, "haut"),
           "bilan : normale, valeur de l'annee et ecart d'une serie lineaire (calcul a la main)")
  verifier(isTRUE(b$tendance_ok) && presque_egal(b$pente_dec, 0.3, 1e-6) && isTRUE(b$tendance_significative),
           "bilan : tendance de 0,03 par an = 0,3 par decennie, significative")
  b00 <- cmp_bilan(lin, ctx19, 2000)
  verifier(presque_egal(b00$valeur, 10 + 0.03 * 41, 1e-9) && presque_egal(b00$pente_dec, b$pente_dec, 1e-12),
           "bilan : changer l'annee change la valeur et l'ecart, pas la tendance")

  court <- data.frame(lieu = "Court", cle = "tmp", annee = 2015:2025, valeur = seq(10, 12, length.out = 11),
                      stringsAsFactors = FALSE)
  bc <- cmp_bilan(court, ctx19, 2025)
  verifier(is.na(bc$normale) && is.na(bc$ecart) && !isTRUE(bc$tendance_ok) && !length(cmp_phrases_bilan(bc, ctx19, 2025)),
           "moins de 10 annees de reference ou de 20 annees de serie : ni normale, ni tendance, ni phrase")

  lg <- data.frame(lieu = "Zone", cle = "vag", annee = an19, valeur = 2 + 0.01 * (an19 - 1959), stringsAsFactors = FALSE)
  bz <- cmp_bilan(lg, ctx19, 2025)
  verifier(bz$decimales == 1L && grepl("vagues", cmp_phrases_bilan(bz, ctx19, 2025)) &&
             grepl("par rapport à la normale 1991-2020", cmp_phrases_bilan(bz, ctx19, 2025)),
           "un decompte moyenne sur une zone s'affiche avec une decimale")

  ph <- cmp_phrases_bilan(b, ctx19, 2025)
  verifier(length(ph) == 1 && grepl("en 2025", ph) && grepl("nettement au-dessus|au-dessus", ph) &&
             grepl("par décennie", ph) && grepl("significative", ph),
           "la phrase du bilan donne la valeur, l'ecart, sa lecture en mots et la tendance")

  # -- Exports ---------------------------------------------------------------------------
  b_tout <- cmp_bilan(ser19, ctx19, 2025)
  tl_ok <- tl19[tl19$retenu, ]
  ex <- cmp_export_comparaison(b_tout, tl_ok, 2025)
  verifier(nrow(ex) == nrow(b_tout) && all(c("lieu", "type", "n_mailles", "grandeur", "unite", "annee", "valeur",
                                             "normale", "ecart", "lecture", "tendance_par_decennie") %in% names(ex)) &&
             all(ex$annee == 2025),
           "l'export de comparaison : une ligne par lieu et grandeur, avec type, mailles, ecart et tendance")

  piege <- data.frame(lieu = c("=2+2", "+cmd", "-1", "@SUM(A1)", "Normal"), cle = "tmp", annee = 2025,
                      valeur = 1, stringsAsFactors = FALSE)
  verifier(identical(cmp_export_series(piege)$lieu, c("'=2+2", "'+cmd", "'-1", "'@SUM(A1)", "Normal")),
           "export CSV : un nom qui commence par = + - @ est neutralise (injection de formule)")

  ent <- cmp_lignes_metadonnees(jeu19, tl_ok, ctx19, 2025, "Comparaison de lieux")
  verifier(any(grepl("mailles SAFRAN", ent)) && any(grepl("Lieux : 2 \\(1 points, 1 zones\\)", ent)) &&
             any(grepl("Référence : 1991-2020", ent)) && any(grepl("année comparée : 2025", ent)) &&
             any(grepl("seuils", ent, ignore.case = TRUE)) && any(grepl("Licence ouverte", ent)) &&
             !any(grepl(";", ent)),
           "l'en-tete du CSV porte la source, les lieux, la reference, l'annee, les seuils et la licence")
  f_c <- tempfile(fileext = ".csv"); ecrire_csv_avec_metadonnees(ex, f_c, ent)
  relu <- utils::read.csv2(f_c, comment.char = "#", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  verifier(nrow(relu) == nrow(ex) && identical(names(relu), names(ex)) && presque_egal(relu$valeur, ex$valeur, 1e-6),
           "le CSV de comparaison se relit a l'identique dans Excel (BOM, point-virgule)")

  # -- Graphique ---------------------------------------------------------------------------
  ok_graphe <- function(...) !inherits(tryCatch(ggplot2::ggplot_build(cmp_graphe(...)), error = function(e) e), "error")
  verifier(ok_graphe(ser19, b_tout, "tmp", "valeur", ctx19, 2025, FALSE) &&
             ok_graphe(ser19, b_tout, "tmp", "ecart", ctx19, 2025, TRUE),
           "le graphique se construit, en valeur et en ecart, sur grand ecran et sur mobile")
  dix <- do.call(rbind, lapply(1:10, function(i) transform(lin, lieu = paste0("L", i))))
  g10 <- cmp_graphe(dix, cmp_bilan(dix, ctx19, 2025), "tmp", "valeur", ctx19, 2025, FALSE)
  verifier(length(unique(ggplot2::ggplot_build(g10)$data[[1]]$colour)) == 8 && grepl("8 premiers", g10$labels$caption),
           "au-dela de 8 lieux, 8 courbes seulement, et la legende le dit")
  vide <- cmp_graphe(ser19[0, ], b_tout, "tmp", "valeur", ctx19, 2025, FALSE)
  verifier(inherits(vide, "ggplot"), "sans valeur a tracer, un message plutot qu'une erreur")

  # -- Tableau et bilan affiches ------------------------------------------------------------
  lieux_xss <- list(cmp_lieu_point("<script>alert(1)</script>", ll19$lon[k19], ll19$lat[k19]))
  mail_x <- lapply(lieux_xss, cmp_mailles, jeu = jeu19)
  tl_x <- cmp_table_lieux(lieux_xss, mail_x)
  ser_x <- cmp_series(jeu19, tl_x$lieu, mail_x, c("tmp", "vag"))
  b_x <- cmp_bilan(ser_x, ctx19, 2025)
  html_x <- paste(as.character(comparer_tableau_ui(b_x, tl_x, c("tmp", "vag"), 2025)),
                  as.character(shiny::tagList(comparer_bilan_ui(b_x, tl_x, ctx19, 2025))),
                  as.character(comparer_messages_ui("<img src=x onerror=alert(1)>")))
  verifier(!grepl("<script>|<img", html_x) && grepl("&lt;script&gt;", html_x),
           "un nom de lieu ou un message d'import n'est jamais rendu comme du HTML")
  html_t <- as.character(comparer_tableau_ui(b_tout, tl_ok, c("tmp", "vag", "pre"), 2025))
  verifier(grepl("<caption", html_t) && grepl('scope="col"', html_t) && grepl('scope="row"', html_t) &&
             grepl("zone, 4 mailles", html_t) && grepl(">point<", html_t) && grepl("—", html_t),
           "le tableau de comparaison a sa legende, ses en-tetes de ligne et de colonne, et « — » pour une valeur absente")
  html_c <- as.character(comparer_tableau_ui(cmp_bilan(court, ctx19, 2025), cmp_table_lieux(list(cmp_lieu_point("Court", ll19$lon[k19], ll19$lat[k19])), list(m_centre)), "tmp", 2025))
  verifier(grepl("normale non calculable", html_c),
           "une valeur sans normale le dit en clair, sans inventer d'ecart")
}


# ==============================================================================
groupe("v1.9 — comparer : le module, de bout en bout")
# ==============================================================================

if (!AVEC_SF19) {
  cat("  (sf absent : test du module passe)\n")
} else {
  res <- new.env()
  f_pts <- ecrire19(c("nom;latitude;longitude",
                      sprintf("Centre;%.5f;%.5f", ll19$lat[k19], ll19$lon[k19]),
                      sprintf("Voisine;%.5f;%.5f", ll19$lat[k19 + 1L], ll19$lon[k19 + 1L]),
                      sprintf("=HYPERLINK(\"http://x\");%.5f;%.5f", ll19$lat[k19 + 6L], ll19$lon[k19 + 6L]),
                      "Loin;48,5;2,0"))
  f_zone <- tempfile(fileext = ".geojson")
  sf::st_write(sf::st_sf(nom = "Zone Toulouse", geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(1.0, 43.0), c(2.0, 43.0), c(2.0, 44.0), c(1.0, 44.0), c(1.0, 43.0)))), crs = 4326)),
    f_zone, quiet = TRUE)
  ctx_m <- ctx19

  shiny::testServer(
    mod_comparer_server,
    args = list(ctx = shiny::reactive(ctx_m), lieu = shiny::reactive(NULL),
                transport = shiny::reactive(tr19)),
    {
      session$setInputs(fichier = data.frame(name = "points.csv", size = file.size(f_pts), type = "text/csv",
                                             datapath = f_pts, stringsAsFactors = FALSE))
      res$noms_apres_csv <- tl()$lieu
      res$retenus_csv <- retenus()
      res$messages_csv <- messages()

      session$setInputs(fichier = data.frame(name = "zone.geojson", size = file.size(f_zone), type = "application/geo+json",
                                             datapath = f_zone, stringsAsFactors = FALSE))
      res$noms_apres_zone <- tl()$lieu
      res$n_mailles_zone <- tl()$n_mailles[tl()$lieu == "Zone Toulouse"]

      session$setInputs(annee = "2025", couches = c("tmp", "vag"))
      res$cles <- cles(); res$annee <- annee()
      res$bilan <- bilan(); res$series <- series(); res$tend1 <- tendances()
      res$html <- as.character(output$resultats$html)

      session$setInputs(annee = "2000")
      res$annee2 <- annee(); res$bilan2 <- bilan(); res$tend2 <- tendances()

      res$csv_comp <- readLines(output$csv_comparaison, warn = FALSE, encoding = "UTF-8")
      res$csv_ser <- readLines(output$csv_series, warn = FALSE, encoding = "UTF-8")
      res$modele <- readLines(output$modele, warn = FALSE, encoding = "UTF-8")
      res$nom_dl <- NULL

      session$setInputs(retenus = c("Centre", "Voisine"))
      res$retenus_choix <- retenus()

      session$setInputs(vider = 1)
      res$apres_vider <- length(lieux())
    })

  verifier(identical(res$noms_apres_csv, c("Centre", "Voisine", "=HYPERLINK(http://x)", "Loin")) &&
             identical(res$retenus_csv, c("Centre", "Voisine", "=HYPERLINK(http://x)")),
           "module : le CSV importe, le point hors grille est liste mais n'est pas compare")
  verifier(identical(tail(res$noms_apres_zone, 1), "Zone Toulouse") && res$n_mailles_zone > 4,
           "module : un second import s'ajoute au premier, la zone retient plusieurs mailles")
  verifier(identical(res$cles, c("tmp", "vag")) && res$annee == 2025L &&
             setequal(unique(res$bilan$lieu), c("Centre", "Voisine", "=HYPERLINK(http://x)", "Zone Toulouse")) &&
             nrow(res$bilan) == 8,
           "module : 4 lieux retenus x 2 grandeurs dans le bilan de 2025")
  verifier(grepl("3. Comparaison pour 2025", res$html) && grepl("Bilan par lieu", res$html) &&
             grepl("Méthode et limites", res$html),
           "module : la page de resultats porte le tableau, le graphique, le bilan et la methode")
  verifier(res$annee2 == 2000L && identical(res$tend1, res$tend2) &&
             presque_egal(res$bilan2$valeur[res$bilan2$lieu == "Centre" & res$bilan2$cle == "tmp"],
                          res$series$valeur[res$series$lieu == "Centre" & res$series$cle == "tmp" & res$series$annee == 2000]),
           "module : changer d'annee ne recalcule pas les tendances et relit la bonne valeur")
  verifier(any(grepl("# .*Comparaison de lieux", res$csv_comp)) && any(grepl("^# .*année comparée : 2000", res$csv_comp)) &&
             !any(grepl("^\"?=HYPERLINK", res$csv_comp)) && any(grepl("'=HYPERLINK", res$csv_comp)),
           "module : le CSV de comparaison porte sa provenance et neutralise le nom a formule")
  verifier(any(grepl("# .*Séries annuelles", res$csv_ser)) && sum(!grepl("^\ufeff?#", res$csv_ser)) == 1 + 4 * 2 * length(an19),
           "module : le CSV des series a une ligne par lieu, grandeur et annee")
  verifier(sum(!grepl("^\ufeff?#", res$modele)) == 1 + nrow(cmp_modele_csv()),
           "module : le modele de CSV se telecharge")
  verifier(identical(res$retenus_choix, c("Centre", "Voisine")) && res$apres_vider == 0L,
           "module : decocher un lieu le retire de la comparaison, « Tout effacer » vide la liste")

  # -- Point de l'accueil -------------------------------------------------------------------
  shiny::testServer(
    mod_comparer_server,
    args = list(ctx = shiny::reactive(ctx_m),
                lieu = shiny::reactive(list(nom = "Ici", lat = ll19$lat[k19], lon = ll19$lon[k19])),
                transport = shiny::reactive(tr19)),
    {
      session$setInputs(ajouter_point = 1)
      res$accueil <- tl()$lieu
      session$setInputs(ajouter_point = 2)
      res$accueil2 <- tl()$lieu
    })
  verifier(identical(res$accueil, "Ici") && identical(res$accueil2, c("Ici", "Ici (2)")),
           "module : le point choisi sur l'accueil s'ajoute (et se distingue s'il est ajoute deux fois)")

  shiny::testServer(
    mod_comparer_server,
    args = list(ctx = shiny::reactive(ctx_m), lieu = shiny::reactive(NULL), transport = shiny::reactive(tr19)),
    { session$setInputs(ajouter_point = 1); res$sans_point <- messages(); res$n_sans <- length(lieux()) })
  verifier(res$n_sans == 0L && any(grepl("Aucun point", res$sans_point)),
           "module : sans point choisi sur l'accueil, un message plutot qu'un lieu vide")

  # -- Donnees de carte indisponibles ---------------------------------------------------------
  shiny::testServer(
    mod_comparer_server,
    args = list(ctx = shiny::reactive(ctx_m), lieu = shiny::reactive(NULL),
                transport = shiny::reactive(list(mode = "absent", base = NULL, erreurs = "aucune donnee"))),
    { res$jeu_absent <- is.null(jeu()); res$html_absent <- as.character(output$indisponible$html) })
  verifier(res$jeu_absent && grepl("ne sont pas disponibles", res$html_absent),
           "module : sans donnees de carte, la page l'explique au lieu de planter")
}


# ==============================================================================
groupe("v1.9 — comparer : raccord a l'application")
# ==============================================================================

lire19 <- function(f) paste(readLines(chemin_ci(f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
app19 <- lire19("app.R"); css19 <- lire19("www/climafor.css"); mod19 <- lire19("src/mod_comparer.R")
verifier(grepl('mod_comparer_ui("comparer")', app19, fixed = TRUE) &&
           grepl('mod_comparer_server("comparer"', app19, fixed = TRUE) &&
           grepl('value = "comparer"', app19, fixed = TRUE) &&
           !is.null(CF_ICONES$comparer) && !is.null(cf_icone("comparer")),
         "l'onglet Comparer est dans l'application, avec son icone")
verifier(regexpr('value = "carte"', app19) < regexpr('value = "comparer"', app19) &&
           regexpr('value = "comparer"', app19) < regexpr('value = "bilan"', app19),
         "l'onglet Comparer vient apres la Carte et avant le Bilan")
classes19 <- unique(regmatches(css19, gregexpr("\\.cf-comp__[a-z]+", css19))[[1]])
verifier(length(classes19) >= 6 && all(vapply(sub("^\\.", "", classes19), function(k) grepl(k, mod19, fixed = TRUE), TRUE)),
         "chaque classe .cf-comp__* du CSS est utilisee par le module")
verifier(grepl("\\.cf-comp__actions", css19) && grepl("cf-comp__actions,", css19, fixed = TRUE),
         "les commandes de l'onglet Comparer ne s'impriment pas")
src_glob19 <- lire19("global.R")
verifier(regexpr("src/utils_comparer.R", src_glob19, fixed = TRUE) < regexpr("src/mod_comparer.R", src_glob19, fixed = TRUE) &&
           regexpr("src/utils_carte.R", src_glob19, fixed = TRUE) < regexpr("src/utils_comparer.R", src_glob19, fixed = TRUE),
         "global.R charge utils_carte.R, puis utils_comparer.R, puis mod_comparer.R")
