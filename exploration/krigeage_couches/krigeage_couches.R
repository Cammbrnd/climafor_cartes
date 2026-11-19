# ==============================================================================
# krigeage_couches.R
# Cartes par krigeage (maille de 500 m) des COUCHES ANNUELLES de l'onglet Carte de ClimaFor,
# a partir des valeurs par station que l'application publie deja (stations.json et
# s_<grandeur>.bin.gz sur GitHub Pages). SCRIPT EXPLORATOIRE, HORS APPLICATION : il lit les
# fichiers publies, il n'y ecrit rien.
#
# Pourquoi les fichiers publies et non un recalcul : la valeur d'une station y est celle que
# l'application calcule (meme fonction calculer_un_indicateur, annee retenue si 95 % des jours
# sont renseignes, code qualite douteux ecarte). La carte krigee et l'onglet Bilan parlent donc
# de la meme grandeur.
#
# Pour chaque couche :
#   1. valeur de l'annee ANNEE a chaque station publiee (altitude declaree, lon, lat) ;
#   2. trois formules evaluees par validation croisee en 10 groupes de stations tires AU HASARD
#      (memes groupes pour les trois, graine fixe) :
#        - krigeage ordinaire (sans altitude),
#        - krigeage a derive externe sur l'altitude (derive reestimee dans chaque voisinage),
#        - krigeage de regression (tendance globale sur l'altitude + krigeage des residus) ;
#   3. la meilleure est retenue (regles de validite et de parcimonie identiques a celles du
#      krigeage estival : candidat valide si <= 0,5 % de stations sans prediction et RMSE < 5 fois
#      celle du krigeage ordinaire ; derive locale gardee si la tendance globale gagne moins de 1 %) ;
#      la tendance doit en outre BATTRE le krigeage ordinaire, sinon celui-ci est retenu ;
#   4. surface sur une maille de RESOLUTION_M (altitude moyenne de la maille issue des tuiles
#      d'altitude AWS), carte PNG, GeoTIFF a 2 bandes (valeur, ecart-type), diagnostic ;
#   5. une ligne par couche dans validation_couches_<annee>_<groupe>.csv : c'est elle qui dit si la
#      couche tient (Nash, RMSE, part de la variance qui reste en pepite, nombre de stations).
#
# La validation croisee utilise l'altitude DECLAREE des stations : elle ne peut pas juger la finesse
# de la maille ni celle du MNT. Elle juge si une valeur de station se predit depuis les autres.
#
# Reglages : variables d'environnement KRIGEAGE_* (voir parametres.env et LISEZMOI.md).
# Paquets : jsonlite, sf, gstat, ggplot2, scales, httr2, terra, png, ragg (CRAN).
# ==============================================================================

# ---- Reglages ----------------------------------------------------------------
env_ou <- function(nom, defaut) { v <- trimws(Sys.getenv(nom, "")); if (nzchar(v)) v else defaut }
`%||%` <- function(a, b) if (is.null(a) || !length(a) || (length(a) == 1L && is.na(a))) b else a

# Les 22 couches faisables depuis les stations. Exclues : swi, vpd, vpdm, neige (aucune station ne
# les mesure : l'application refuse deja ces indicateurs pour une station).
# pal       : degrade (voir PALETTES) ; lo, hi : bornes physiques de la grandeur (la surface y est ramenee)
# alt_libre : l'altitude des mailles peut depasser celle de la station la plus haute (gradient quasi
#             lineaire : temperature, degres-jours) ; sinon elle est ramenee a la plage des stations
# prudence  : couche a lire avec prudence (evenement rare, date censuree, vent...) ; c'est une
#             information, elle ne change pas le calcul
cc <- function(pal, lo = NA_real_, hi = NA_real_, alt_libre = FALSE, prudence = FALSE)
  list(pal = pal, lo = lo, hi = hi, alt_libre = alt_libre, prudence = prudence)
COUCHES <- list(
  tmp    = cc("temp", alt_libre = TRUE),
  jja_t  = cc("temp", alt_libre = TRUE),
  gdd    = cc("chaud", lo = 0, alt_libre = TRUE),
  pre    = cc("eau", lo = 0),
  jja_p  = cc("eau", lo = 0),
  petp   = cc("bilan"),
  veg    = cc("vert", lo = 0, hi = 366),
  mart   = cc("eau", lo = 0),
  gel    = cc("froid", lo = 0, hi = 366),
  gel5   = cc("froid", lo = 0, hi = 366),
  gel10  = cc("froid", lo = 0, hi = 366),
  geltd  = cc("froid", lo = 0, hi = 366, prudence = TRUE),
  can1   = cc("chaud", lo = 0, hi = 366),
  can    = cc("chaud", lo = 0, hi = 366),
  can3   = cc("chaud", lo = 0, hi = 366),
  can4   = cc("chaud", lo = 0, hi = 366),
  chaud1 = cc("froid", lo = 1, hi = 366, prudence = TRUE),
  chaud2 = cc("froid", lo = 1, hi = 366, prudence = TRUE),
  vag    = cc("chaud", lo = 0, prudence = TRUE),
  vagj   = cc("chaud", lo = 0, hi = 366, prudence = TRUE),
  dry    = cc("chaud", lo = 0, hi = 366, prudence = TRUE),
  vent   = cc("violet", lo = 0, prudence = TRUE))
PALETTES <- list(
  temp   = c("#313695", "#4575b4", "#74add1", "#abd9e9", "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"),
  chaud  = c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026"),
  eau    = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
  bilan  = c("#543005", "#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571", "#003c30"),
  vert   = c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837"),
  violet = c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#756bb1", "#54278f"),
  froid  = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"))

ANNEE        <- as.integer(env_ou("KRIGEAGE_ANNEE", "2025"))
COUCHES_DEMANDEES <- trimws(strsplit(env_ou("KRIGEAGE_COUCHES", paste(names(COUCHES), collapse = ",")), "[,; ]+")[[1]])
GROUPE       <- env_ou("KRIGEAGE_GROUPE", "tout")                       # etiquette des fichiers de ce lancement
LECTURE_SEULE <- tolower(env_ou("KRIGEAGE_LECTURE_SEULE", "non")) %in% c("oui", "o", "1", "true", "vrai", "yes")
URL_DONNEES  <- env_ou("KRIGEAGE_URL_DONNEES", "https://cammbrnd.github.io/climafor_cartes/")
DOSSIER_DONNEES <- env_ou("KRIGEAGE_DOSSIER_DONNEES", "")              # dossier local a la place de l'adresse (tests)
RESOLUTION_M <- as.numeric(env_ou("KRIGEAGE_RESOLUTION_M", "500"))
MNT_ZOOM     <- as.integer(env_ou("KRIGEAGE_MNT_ZOOM", "12"))
MNT_URL      <- env_ou("KRIGEAGE_MNT_URL", "https://s3.amazonaws.com/elevation-tiles-prod/terrarium")
MNT_FICHIER  <- env_ou("KRIGEAGE_MNT_FICHIER", "")                     # GeoTIFF d'altitude (m) a la place des tuiles
DOSSIER      <- env_ou("KRIGEAGE_DOSSIER", "krigeage_couches")         # cache + sorties
SEUIL_STATIONS <- as.integer(env_ou("KRIGEAGE_SEUIL_STATIONS", "30"))  # en dessous, la couche n'est pas krigee
DPI          <- as.numeric(env_ou("KRIGEAGE_DPI", "200"))
NMAX         <- 40        # stations voisines utilisees par le krigeage
NFOLD_CV     <- 10        # validation croisee en groupes de stations tires au hasard

stopifnot(is.finite(ANNEE), ANNEE >= 1950L, ANNEE <= as.integer(format(Sys.Date(), "%Y")),
          is.finite(RESOLUTION_M), RESOLUTION_M >= 500, is.finite(MNT_ZOOM), MNT_ZOOM >= 5L, MNT_ZOOM <= 12L,
          is.finite(SEUIL_STATIONS), SEUIL_STATIONS >= 10L, is.finite(DPI), DPI >= 72)
inconnues <- setdiff(COUCHES_DEMANDEES, names(COUCHES))
if (length(inconnues)) stop("Couche(s) non prevue(s) : ", paste(inconnues, collapse = ", "), call. = FALSE)

for (p in c("jsonlite", "sf", "gstat", "ggplot2", "httr2", "terra", "png")) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Paquet manquant : ", p, call. = FALSE)
}
options(timeout = max(600, getOption("timeout")))
dir.create(file.path(DOSSIER, "donnees"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DOSSIER, "sorties"), recursive = TRUE, showWarnings = FALSE)
JOURNAL <- file.path(DOSSIER, "sorties", sprintf("journal_couches_%d_%s.txt", ANNEE, GROUPE))
cat("", file = JOURNAL)
msg <- function(...) { s <- sprintf(...); cat(s, "\n", sep = ""); cat(s, "\n", sep = "", file = JOURNAL, append = TRUE) }
SUF  <- sprintf("%d_%gkm", ANNEE, RESOLUTION_M / 1000)                  # suffixe des fichiers : 2025_0.5km
msg("Annee %d ; couches : %s ; maille %g m ; groupe %s.", ANNEE, paste(COUCHES_DEMANDEES, collapse = " "), RESOLUTION_M, GROUPE)


# Requete HTTP (delai, nouvelles tentatives)
requete <- function(url, query = NULL) {
  r <- httr2::request(url)
  if (length(query)) r <- do.call(httr2::req_url_query, c(list(.req = r), query))
  r <- httr2::req_user_agent(httr2::req_timeout(r, 90), "krigeage-couches (R, exploration)")
  httr2::req_retry(r, max_tries = 3)
}


# ==============================================================================
# 1. Valeurs par station publiees par l'application
# ==============================================================================
# Format (src/utils_carte.R, outils/carte_format.R) : s_<cle>.bin.gz = gzip d'un tampon little-endian :
#   normale Float32 [n], pente Float32 [n], signif Uint8 [n] (+ remplissage a 4 octets),
#   valeurs Int16 [n_annees * n] annee par annee, valeur stockee = valeur * 10^stock, -32768 = absente.
# La table des stations (stations.json) donne l'ordre des colonnes.

PLAT <- -32768L
chemin_donnee <- function(nom) {
  if (nzchar(DOSSIER_DONNEES)) {
    f <- file.path(DOSSIER_DONNEES, nom)
    if (!file.exists(f)) stop("fichier absent du dossier local : ", f, call. = FALSE)
    return(f)
  }
  dest <- file.path(DOSSIER, "donnees", nom)
  httr2::req_perform(requete(paste0(sub("/*$", "/", URL_DONNEES), nom)), path = dest)
  dest
}
colonne <- function(x, f, na) vapply(x, function(v) if (is.null(v) || !length(v)) na else suppressWarnings(f(v[[1]])), na)

sj <- jsonlite::fromJSON(chemin_donnee("stations.json"), simplifyVector = FALSE)
annees_pub <- as.integer(unlist(sj$annees))
tab <- data.frame(id  = colonne(sj$stations$id, as.character, NA_character_),
                  nom = colonne(sj$stations$nom, as.character, NA_character_),
                  dept = colonne(sj$stations$dept, as.character, NA_character_),
                  lon = colonne(sj$stations$lon, as.numeric, NA_real_),
                  lat = colonne(sj$stations$lat, as.numeric, NA_real_),
                  alt = colonne(sj$stations$alt, as.numeric, NA_real_), stringsAsFactors = FALSE)
if (nrow(tab) != as.integer(sj$n_stations)) stop("stations.json : nombre de stations incoherent.", call. = FALSE)
msg("stations.json : %d stations, annees %d-%d, genere le %s.", nrow(tab), min(annees_pub), max(annees_pub), sj$genere_le %||% "?")
i_annee <- match(ANNEE, annees_pub)
if (is.na(i_annee)) stop(sprintf("L'annee %d n'est pas dans le jeu publie (%d-%d).", ANNEE, min(annees_pub), max(annees_pub)), call. = FALSE)

meta_carte <- tryCatch(jsonlite::fromJSON(chemin_donnee("meta.json"), simplifyVector = FALSE), error = function(e) {
  msg("meta.json illisible (%s) : titres de secours.", conditionMessage(e)); NULL })
info_couche <- function(cle) {
  k <- Filter(function(k) identical(as.character(k$cle)[1], cle), meta_carte$couches %||% list())
  list(titre = if (length(k)) as.character(k[[1]]$titre)[1] else cle,
       unite = if (length(k)) as.character(k[[1]]$unite)[1] else "",
       dec   = if (length(k)) as.integer(k[[1]]$decimales)[1] else 1L)
}

lire_couche_stations <- function(cle) {
  k <- Filter(function(k) identical(as.character(k$cle)[1], cle), sj$couches)
  if (!length(k)) return(NULL)                                  # grandeur non publiee pour les stations
  f <- chemin_donnee(as.character(k[[1]]$fichier)[1])
  stock <- as.integer(k[[1]]$stock); n <- nrow(tab); na <- length(annees_pub)
  con <- gzfile(f, "rb"); on.exit(close(con))
  normale <- readBin(con, "numeric", n, size = 4L, endian = "little")
  readBin(con, "numeric", n, size = 4L, endian = "little")      # pente : non utilisee ici
  readBin(con, "raw", n)                                        # significativite : non utilisee ici
  pad <- (4L - (n %% 4L)) %% 4L
  if (pad) readBin(con, "raw", pad)
  e <- readBin(con, "integer", na * n, size = 2L, endian = "little")
  reste <- length(readBin(con, "raw", 1L))
  if (length(e) != na * n || reste != 0L) stop(sprintf("%s : taille de fichier inattendue (format change ?)", cle), call. = FALSE)
  m <- matrix(ifelse(e == PLAT, NA_real_, e / 10^stock), nrow = na, byrow = TRUE)     # annees x stations
  list(valeur = m[i_annee, ], normale = ifelse(is.finite(normale), normale, NA_real_), annees = m)
}

# Toutes les couches demandees : valeur de l'annee, normale, nombre de stations qui ont une valeur
VAL <- list(); NOR <- list(); lignes_don <- list()
for (cle in COUCHES_DEMANDEES) {
  l <- tryCatch(lire_couche_stations(cle), error = function(e) { msg("%s : lecture impossible (%s)", cle, conditionMessage(e)); NULL })
  inf <- info_couche(cle)
  v <- if (is.null(l)) rep(NA_real_, nrow(tab)) else l$valeur
  VAL[[cle]] <- v; NOR[[cle]] <- if (is.null(l)) rep(NA_real_, nrow(tab)) else l$normale
  nv <- sum(is.finite(v))
  lignes_don[[cle]] <- data.frame(cle = cle, titre = inf$titre, unite = inf$unite, publiee = !is.null(l), n_stations_valeur = nv,
                                  min = if (nv) min(v, na.rm = TRUE) else NA_real_, max = if (nv) max(v, na.rm = TRUE) else NA_real_,
                                  moyenne = if (nv) mean(v, na.rm = TRUE) else NA_real_,
                                  part_zero = if (nv) mean(v[is.finite(v)] == 0) else NA_real_, stringsAsFactors = FALSE)
}
donnees <- do.call(rbind, lignes_don); rownames(donnees) <- NULL
msg("\nValeurs par station, annee %d (%d stations publiees ; altitude connue pour %d) :", ANNEE, nrow(tab), sum(is.finite(tab$alt)))
for (i in seq_len(nrow(donnees))) with(donnees[i, ], msg("  %-7s %5d stations | de %8.2f a %8.2f | part de zeros %3.0f %% | %s",
                                                       cle, n_stations_valeur, min, max, 100 * part_zero, titre))
utils::write.csv(donnees, file.path(DOSSIER, "sorties", sprintf("donnees_stations_%d_%s.csv", ANNEE, GROUPE)), row.names = FALSE, fileEncoding = "UTF-8")
large <- cbind(tab[, c("id", "nom", "dept", "lon", "lat", "alt")], as.data.frame(VAL), stringsAsFactors = FALSE)
names(large)[-(1:6)] <- COUCHES_DEMANDEES
nor_df <- as.data.frame(NOR); names(nor_df) <- paste0(COUCHES_DEMANDEES, "_normale")      # normale de la station (periode de reference de l'application)
large <- cbind(large, nor_df)
utils::write.csv(large, file.path(DOSSIER, "sorties", sprintf("stations_couches_%d_%s.csv", ANNEE, GROUPE)), row.names = FALSE, fileEncoding = "UTF-8")
if (LECTURE_SEULE) { msg("\nLecture seule : arret avant le calcul."); quit(save = "no", status = 0) }


# ==============================================================================
# 2. Contour de la France, grille et altitude moyenne de chaque maille
# ==============================================================================

# Contours des departements metropolitains (simplifies) : plusieurs sources, a defaut enveloppe des stations.
# Un fichier KRIGEAGE_DOSSIER/departements_contours.geojson deja present est reutilise.
contour_france <- function() {
  f <- file.path(DOSSIER, "departements_contours.geojson")
  lire <- function(chemin) {
    d <- tryCatch(sf::read_sf(chemin, quiet = TRUE),
                  error = function(e) { msg("  lecture impossible : %s", conditionMessage(e)); NULL })
    if (is.null(d)) return(NULL)
    if (!("code" %in% names(d))) { msg("  pas de colonne code (colonnes : %s)", paste(names(d), collapse = ", ")); return(NULL) }
    d <- d[grepl("^([0-9]{2}|2[AB])$", as.character(d$code)), ]
    if (!nrow(d)) { msg("  aucun departement metropolitain reconnu"); return(NULL) }
    d
  }
  d <- if (file.exists(f) && file.size(f) > 0) lire(f) else NULL
  sources <- c(
    "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-version-simplifiee.geojson",
    "https://geo.api.gouv.fr/departements?fields=nom,code,contour&format=geojson&geometry=contour")
  for (u in sources) {
    if (!is.null(d)) break
    msg("Contour : %s", sub("\\?.*$", "", u))
    ok <- tryCatch({ httr2::req_perform(requete(u), path = f); TRUE },
                   error = function(e) { msg("  telechargement impossible : %s", conditionMessage(e)); FALSE })
    if (ok) d <- lire(f)
    if (is.null(d)) unlink(f)
  }
  if (is.null(d)) return(NULL)
  sf::st_transform(sf::st_make_valid(d), 2154)
}
deps <- contour_france()
tout_pts <- sf::st_transform(sf::st_as_sf(tab[is.finite(tab$lon) & is.finite(tab$lat), ], coords = c("lon", "lat"), crs = 4326), 2154)
if (is.null(deps)) {
  msg("Contour des departements indisponible : enveloppe des stations (+ 25 km) a la place.")
  masque <- sf::st_buffer(sf::st_convex_hull(sf::st_union(sf::st_geometry(tout_pts))), 25000)
} else {
  masque <- sf::st_union(sf::st_geometry(deps))
}
grille <- sf::st_make_grid(masque, cellsize = RESOLUTION_M, what = "centers")
grille <- grille[sf::st_intersects(grille, masque, sparse = FALSE)[, 1]]
msg("Grille : %d mailles de %g m.", length(grille), RESOLUTION_M)

# -- MNT : altitude moyenne de chaque maille (memes fonctions que le krigeage estival) ------------
# -- MNT : altitude moyenne de chaque maille (tendance du krigeage) -------------------
# Tuiles ouvertes "terrarium" (AWS) : PNG en Web Mercator, altitude (m) = R*256 + G + B/256 - 32768.
# Telechargees une fois (cache : DOSSIER/mnt), puis moyennees sur chaque maille de la grille.
mnt_etat <- new.env()
mnt_etat$echecs <- 0L
mnt_etat$premiere <- NULL

tuile_mnt <- function(z, x, y) {
  dest <- file.path(DOSSIER, "mnt", z, x, paste0(y, ".png"))
  if (file.exists(dest) && file.size(dest) > 0) return(dest)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(dest, ".part")
  url <- sprintf("%s/%d/%d/%d.png", MNT_URL, z, x, y)
  ok <- tryCatch({ httr2::req_perform(requete(url), path = tmp); TRUE }, error = function(e) {
    mnt_etat$echecs <- mnt_etat$echecs + 1L
    if (is.null(mnt_etat$premiere)) {
      mnt_etat$premiere <- paste(conditionMessage(e), if (!is.null(e$parent)) conditionMessage(e$parent) else "")
    }
    FALSE
  })
  if (ok && !identical(readBin(tmp, "raw", 4L), as.raw(c(0x89, 0x50, 0x4e, 0x47)))) {   # signature PNG
    mnt_etat$echecs <- mnt_etat$echecs + 1L
    if (is.null(mnt_etat$premiere)) mnt_etat$premiere <- "reponse qui n'est pas une image PNG"
    ok <- FALSE
  }
  if (!ok) { unlink(tmp); return(NA_character_) }
  file.rename(tmp, dest)
  dest
}

# Tuiles de zoom > 9 (jusqu'a 12 : ~ 28 000 tuiles de 256 x 256 pixels pour la France) : trop nombreuses pour etre
# assemblees pixel par pixel. Elles sont telechargees en parallele, puis chaque tuile est moyennee par blocs de
# fact x fact pixels (la mer, sous le niveau 0, est ramenee a 0 avant la moyenne), ce qui donne un raster d'environ
# 200 m dont les moyennes par maille de 1 km sont celles du MNT d'origine (30 m environ au zoom 12).
prefetch_tuiles <- function(z, xs, ys) {
  tu   <- expand.grid(x = xs, y = ys)
  dest <- file.path(DOSSIER, "mnt", z, tu$x, paste0(tu$y, ".png"))
  present <- function(d) file.exists(d) & !is.na(file.size(d)) & file.size(d) > 0
  for (passe in 1:3) {
    a_faire <- which(!present(dest))
    if (!length(a_faire)) break
    msg("  MNT : %d tuile(s) a telecharger (passe %d, 20 connexions en parallele)...", length(a_faire), passe)
    t0 <- Sys.time()
    for (lot in split(a_faire, ceiling(seq_along(a_faire) / 400))) {
      for (d in unique(dirname(dest[lot]))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
      tmp  <- paste0(dest[lot], ".part")
      reqs <- lapply(sprintf("%s/%d/%d/%d.png", MNT_URL, z, tu$x[lot], tu$y[lot]), requete)
      # httr2 recent : max_active ; httr2 1.0.0 : pool curl
      args <- list(reqs, paths = tmp, on_error = "continue", progress = FALSE)
      if ("max_active" %in% names(formals(httr2::req_perform_parallel))) args$max_active <- 20L
      else args$pool <- curl::new_pool(total_con = 20L, host_con = 20L)
      tryCatch(do.call(httr2::req_perform_parallel, args),
               error = function(e) if (is.null(mnt_etat$premiere)) mnt_etat$premiere <- conditionMessage(e))
      for (k in seq_along(lot)) {
        ok <- file.exists(tmp[k]) && identical(readBin(tmp[k], "raw", 4L), as.raw(c(0x89, 0x50, 0x4e, 0x47)))   # signature PNG
        if (ok) file.rename(tmp[k], dest[lot[k]]) else unlink(tmp[k])
      }
    }
    msg("  MNT : passe %d terminee en %.0f s.", passe, as.numeric(difftime(Sys.time(), t0, units = "secs")))
  }
  invisible(sum(!present(dest)))
}

# moyenne d'une matrice par blocs de fact x fact (les dimensions doivent etre des multiples de fact)
bloc_moy <- function(h, fact) {
  n <- nrow(h) %/% fact
  a <- array(h, c(fact, n, fact, n))        # [ligne dans le bloc, bloc-ligne, colonne dans le bloc, bloc-colonne]
  a <- colMeans(a, dims = 1L)               # moyenne sur les lignes du bloc    -> [bloc-ligne, colonne dans le bloc, bloc-colonne]
  colMeans(aperm(a, c(2L, 1L, 3L)), dims = 1L)    # moyenne sur les colonnes du bloc -> [bloc-ligne, bloc-colonne]
}

# Altitudes (m) d'une tuile terrarium, matrice 256 x 256 (NULL si illisible). Le paquet png est ~30 fois plus rapide
# que terra pour ce decodage ; terra sert de repli.
lire_tuile <- function(f) {
  if (requireNamespace("png", quietly = TRUE)) {
    a <- tryCatch(png::readPNG(f), error = function(e) NULL)
    if (is.null(a) || length(dim(a)) != 3L || dim(a)[3L] < 3L || dim(a)[1L] != 256L || dim(a)[2L] != 256L) return(NULL)
    return(round(a[, , 1L] * 255) * 256 + round(a[, , 2L] * 255) + round(a[, , 3L] * 255) / 256 - 32768)
  }
  r <- tryCatch(terra::rast(f), error = function(e) NULL)
  if (is.null(r) || terra::nlyr(r) < 3L || terra::nrow(r) != 256L || terra::ncol(r) != 256L) return(NULL)
  v <- terra::values(r, mat = TRUE)                                      # une ligne par pixel (de gauche a droite, de haut en bas)
  matrix(v[, 1L] * 256 + v[, 2L] + v[, 3L] / 256 - 32768, 256L, 256L, byrow = TRUE)
}

mnt_brut_agrege <- function(z, xs, ys, demi, pas) {
  fact <- 2L^(z - 9L); nb <- 256L %/% fact
  cache <- file.path(DOSSIER, "mnt", sprintf("agrege_z%d_x%d-%d_y%d-%d.tif", z, xs[1], xs[length(xs)], ys[1], ys[length(ys)]))
  if (file.exists(cache)) {
    msg("MNT : mosaique agregee deja calculee, reutilisee (%s).", basename(cache))
    return(terra::rast(cache))
  }
  prefetch_tuiles(z, xs, ys)
  M <- matrix(NA_real_, nrow = length(ys) * nb, ncol = length(xs) * nb)
  manquantes <- 0L
  t0 <- Sys.time()
  for (i in seq_along(xs)) for (j in seq_along(ys)) {
    f <- file.path(DOSSIER, "mnt", z, xs[i], paste0(ys[j], ".png"))
    h <- if (file.exists(f) && file.size(f) > 0) lire_tuile(f) else NULL
    if (is.null(h)) { manquantes <- manquantes + 1L; next }
    M[(j - 1L) * nb + seq_len(nb), (i - 1L) * nb + seq_len(nb)] <- bloc_moy(pmax(h, 0), fact)      # mer (< 0) -> 0
  }
  total <- length(xs) * length(ys)
  msg("MNT : %d tuiles decodees et moyennees par blocs de %d x %d pixels en %.0f s.", total - manquantes, fact, fact,
      as.numeric(difftime(Sys.time(), t0, units = "secs")))
  if (manquantes > 0) msg("  %d tuile(s) manquante(s) ou illisible(s)%s", manquantes,
                          if (!is.null(mnt_etat$premiere)) paste0(" ; premiere erreur : ", mnt_etat$premiere) else "")
  if (manquantes > 0.1 * total) stop("trop de tuiles manquantes", call. = FALSE)
  r <- terra::rast(M)
  terra::ext(r) <- c(-demi + xs[1] * pas, -demi + (xs[length(xs)] + 1) * pas, demi - (ys[length(ys)] + 1) * pas, demi - ys[1] * pas)
  terra::crs(r) <- "EPSG:3857"
  if (manquantes == 0L) {       # la mosaique complete est gardee pour les calculs suivants (pluie, temperature)
    tryCatch(terra::writeRaster(r, cache, overwrite = TRUE), error = function(e) msg("MNT : mosaique non conservee (%s).", conditionMessage(e)))
  }
  r
}

mnt_brut <- function() {
  if (nzchar(MNT_FICHIER)) {
    if (!file.exists(MNT_FICHIER)) stop("fichier introuvable : ", MNT_FICHIER, call. = FALSE)
    msg("MNT : fichier %s", MNT_FICHIER)
    return(terra::rast(MNT_FICHIER)[[1L]])
  }
  z <- MNT_ZOOM; n <- 2^z
  # emprise en lon/lat : les bords de la boite Lambert-93 sont courbes en lon/lat (le point le plus au nord est au milieu du
  # bord nord, pas aux coins) ; on les densifie avant de les transformer, sinon la pointe nord de la France serait coupee
  bb <- unname(sf::st_bbox(sf::st_transform(sf::st_segmentize(sf::st_as_sfc(sf::st_bbox(masque)), 20000), 4326)))   # lon/lat
  colonne <- function(lon) floor((lon + 180) / 360 * n)
  ligne   <- function(lat) floor((1 - asinh(tan(lat * pi / 180)) / pi) / 2 * n)
  xs <- colonne(bb[1]):colonne(bb[3])
  ys <- ligne(bb[4]):ligne(bb[2])
  total <- length(xs) * length(ys)
  msg("MNT : %d tuiles d'altitude (zoom %d).", total, z)
  demi <- 20037508.342789244; pas <- 2 * demi / n
  if (z > 9L) return(mnt_brut_agrege(z, xs, ys, demi, pas))
  morceaux <- list()
  for (x in xs) for (y in ys) {
    if (!length(morceaux) && mnt_etat$echecs >= 5L) break      # rien ne passe : inutile d'insister
    f <- tuile_mnt(z, x, y)
    if (is.na(f)) next
    r <- terra::rast(f)
    if (terra::nlyr(r) < 3L) next
    h <- r[[1L]] * 256 + r[[2L]] + r[[3L]] / 256 - 32768
    terra::ext(h) <- c(-demi + x * pas, -demi + (x + 1) * pas, demi - (y + 1) * pas, demi - y * pas)
    terra::crs(h) <- "EPSG:3857"
    morceaux[[length(morceaux) + 1L]] <- h
  }
  manquantes <- total - length(morceaux)
  if (manquantes > 0) msg("  %d tuile(s) manquante(s) ou non essayee(s)%s", manquantes,
                          if (!is.null(mnt_etat$premiere)) paste0(" ; premiere erreur : ", mnt_etat$premiere) else "")
  if (!length(morceaux) || manquantes > 0.1 * total) stop("trop de tuiles manquantes", call. = FALSE)
  terra::merge(terra::sprc(morceaux))
}

# Altitude moyenne par maille (Lambert-93), a la resolution de la grille.
mnt_moyen <- function(brut) {
  brut <- terra::ifel(brut < 0, 0, brut)                     # mer (bathymetrie) : altitude nulle
  bb <- unname(sf::st_bbox(grille))
  modele_r <- terra::rast(xmin = bb[1] - RESOLUTION_M, xmax = bb[3] + RESOLUTION_M,
                          ymin = bb[2] - RESOLUTION_M, ymax = bb[4] + RESOLUTION_M,
                          resolution = RESOLUTION_M, crs = "EPSG:2154")
  terra::project(brut, modele_r, method = "average")
}
valeurs_en <- function(r, coord, interp = "simple") {
  v <- terra::extract(r, coord, method = interp)
  as.numeric(v[[ncol(v)]])
}


alt_mailles <- NULL
brut_mnt <- tryCatch(mnt_brut(), error = function(e) { msg("MNT indisponible : %s", conditionMessage(e)); NULL })
moy <- if (is.null(brut_mnt)) NULL else tryCatch(mnt_moyen(brut_mnt), error = function(e) { msg("MNT inutilisable : %s", conditionMessage(e)); NULL })
if (!is.null(moy)) {
  alt_mailles <- valeurs_en(moy, sf::st_coordinates(grille))
  ok_alt <- is.finite(tab$alt) & is.finite(tab$lon) & is.finite(tab$lat)
  alt_stations <- valeurs_en(moy, sf::st_coordinates(sf::st_transform(sf::st_as_sf(tab[ok_alt, ], coords = c("lon", "lat"), crs = 4326), 2154)))
  bon <- is.finite(alt_stations)
  r_ctrl <- if (sum(bon) > 30) stats::cor(alt_stations[bon], tab$alt[ok_alt][bon]) else NA_real_
  msg("MNT : altitude moyenne des mailles de %.0f a %.0f m. Controle aux stations (MNT / altitude declaree) : r = %.3f, ecart median %.0f m.",
      min(alt_mailles, na.rm = TRUE), max(alt_mailles, na.rm = TRUE), r_ctrl, stats::median(abs(alt_stations[bon] - tab$alt[ok_alt][bon])))
  if (!is.finite(r_ctrl) || r_ctrl < 0.7) { msg("MNT incoherent avec l'altitude des stations : krigeage ordinaire seul."); alt_mailles <- NULL }
} else {
  msg("Krigeage ordinaire seul (pas de MNT).")
}
rm(brut_mnt, moy); invisible(gc())
coord_grille <- sf::st_coordinates(grille)


# ==============================================================================
# 3. Une couche : validation croisee, choix de la formule, surface, cartes
# ==============================================================================

# globale = FALSE : krigeage a derive externe (tendance reestimee dans chaque voisinage)
# globale = TRUE  : krigeage de regression (tendance estimee une fois sur toutes les stations, puis krigeage des residus)
evaluer <- function(pts, formule, globale, blocs, cutoff, etiquette) {
  v  <- gstat::variogram(formule, pts, cutoff = cutoff, width = cutoff / 30)
  s2 <- stats::var(stats::residuals(stats::lm(formule, data = sf::st_drop_geometry(pts))))
  essais <- list()
  for (m in c("Exp", "Sph", "Mat")) {
    ini <- gstat::vgm(psill = 0.8 * s2, model = m, range = cutoff / 3, nugget = 0.2 * s2, kappa = if (m == "Mat") 1 else 0.5)
    aj <- tryCatch(suppressWarnings(gstat::fit.variogram(v, ini)), error = function(e) NULL)
    # on refuse les ajustements absurdes (palier ou portee sans rapport avec les donnees)
    if (!is.null(aj) && !isTRUE(attr(aj, "singular")) && all(aj$psill >= 0) && all(aj$range >= 0) &&
        sum(aj$psill) <= 20 * s2 && max(aj$range) <= 10 * cutoff) {
      essais[[m]] <- list(modele = aj, sserr = attr(aj, "SSErr"))
    }
  }
  if (!length(essais)) {
    msg("  Ajustement du variogramme impossible (%s) : modele exponentiel par defaut.", etiquette)
    modele <- gstat::vgm(0.8 * s2, "Exp", cutoff / 3, 0.2 * s2); choix <- "Exp (defaut)"
  } else {
    best <- names(essais)[which.min(vapply(essais, function(e) e$sserr, 0))]
    modele <- essais[[best]]$modele; choix <- best
  }
  if (!globale) {
    cv <- gstat::krige.cv(formule, pts, model = modele, nmax = NMAX, nfold = blocs, verbose = FALSE)
  } else {
    pr <- rep(NA_real_, nrow(pts))
    for (k in sort(unique(blocs))) {
      ap <- pts[blocs != k, ]; te <- pts[blocs == k, ]
      reg <- suppressWarnings(stats::lm(formule, data = sf::st_drop_geometry(ap)))
      ap$res <- stats::residuals(reg)
      kr <- gstat::krige(res ~ 1, ap, te, model = modele, nmax = NMAX, debug.level = 0)
      pr[blocs == k] <- suppressWarnings(stats::predict(reg, sf::st_drop_geometry(te))) + kr$var1.pred
    }
    cv <- data.frame(observed = pts$valeur, var1.pred = pr)
  }
  res <- cv$observed - cv$var1.pred
  bon <- is.finite(res)
  rmse <- sqrt(mean(res[bon]^2)); biais <- mean(-res[bon]); mae <- mean(abs(res[bon]))
  nse  <- 1 - sum(res[bon]^2) / sum((cv$observed[bon] - mean(cv$observed[bon]))^2)
  msg("  %-22s variogramme %-12s | RMSE %.4g | ecart absolu %.4g | biais %+.3g | Nash %.3f%s", etiquette, choix, rmse, mae, biais, nse,
      if (any(!bon)) sprintf(" | %d sans prediction", sum(!bon)) else "")
  list(modele = modele, choix = choix, v = v, s2 = s2, cv = cv, rmse = rmse, mae = mae, biais = biais, nse = nse, n_na = sum(!bon), globale = globale)
}

COLONNES <- c("cle", "titre", "unite", "prudence", "statut", "n_stations", "obs_min", "obs_max", "obs_moy", "obs_sd", "part_zero",
              "r_altitude", "rmse_sans_alt", "mae_sans_alt", "nash_sans_alt", "rmse_locale", "nash_locale", "rmse_globale", "nash_globale",
              "methode", "variogramme", "pepite", "palier", "portee_km", "part_pepite", "rmse", "mae", "biais", "nash", "gain_rmse_pct",
              "n_mailles", "pred_min", "pred_max", "ecart_type_median", "n_mailles_bornees", "duree_s")
nouvelle_ligne <- function(cle) {
  inf <- info_couche(cle)
  L <- stats::setNames(rep(list(NA), length(COLONNES)), COLONNES)
  L$cle <- cle; L$titre <- inf$titre; L$unite <- inf$unite; L$prudence <- COUCHES[[cle]]$prudence; L$statut <- ""
  L
}
en_ligne <- function(L) as.data.frame(lapply(L, function(x) if (is.null(x) || !length(x)) NA else x), stringsAsFactors = FALSE)[, COLONNES]

traiter_couche <- function(cle) {
  t0 <- Sys.time()
  par <- COUCHES[[cle]]; L <- nouvelle_ligne(cle)
  d <- tab; d$valeur <- VAL[[cle]]
  d <- d[is.finite(d$valeur) & is.finite(d$lon) & is.finite(d$lat) & is.finite(d$alt), , drop = FALSE]
  L$n_stations <- nrow(d)
  if (nrow(d) < SEUIL_STATIONS) { L$statut <- sprintf("non kriegee : %d stations seulement", nrow(d)); msg("  %s", L$statut); return(en_ligne(L)) }
  if (!is.finite(stats::sd(d$valeur)) || stats::sd(d$valeur) < 1e-9) { L$statut <- "non kriegee : valeurs constantes"; msg("  %s", L$statut); return(en_ligne(L)) }
  L$obs_min <- min(d$valeur); L$obs_max <- max(d$valeur); L$obs_moy <- mean(d$valeur); L$obs_sd <- stats::sd(d$valeur)
  L$part_zero <- mean(d$valeur == 0); L$r_altitude <- stats::cor(d$valeur, d$alt)
  msg("  %d stations ; valeurs de %.4g a %.4g (moyenne %.4g) ; part de zeros %.0f %% ; correlation avec l'altitude %+.2f",
      nrow(d), L$obs_min, L$obs_max, L$obs_moy, 100 * L$part_zero, L$r_altitude)

  pts <- sf::st_transform(sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326), 2154)
  pts <- pts[!duplicated(sf::st_coordinates(pts)), ]       # deux stations au meme point : une seule
  xy  <- sf::st_coordinates(pts)
  pts$alt_km <- pts$alt / 1000
  etendue <- sqrt(diff(range(xy[, 1]))^2 + diff(range(xy[, 2]))^2)
  cutoff  <- min(400000, etendue / 3)
  nfold   <- min(NFOLD_CV, nrow(pts))
  set.seed(2026)
  blocs <- sample(rep(seq_len(nfold), length.out = nrow(pts)))    # groupes au hasard, les memes pour les trois formules

  ref <- evaluer(pts, valeur ~ 1, FALSE, blocs, cutoff, "sans altitude")
  L$rmse_sans_alt <- ref$rmse; L$mae_sans_alt <- ref$mae; L$nash_sans_alt <- ref$nse
  loc <- glo <- NULL
  if (!is.null(alt_mailles)) {
    loc <- tryCatch(evaluer(pts, valeur ~ alt_km, FALSE, blocs, cutoff, "altitude (locale)"),
                    error = function(e) { msg("  derive locale ecartee : %s", conditionMessage(e)); NULL })
    glo <- tryCatch(evaluer(pts, valeur ~ alt_km, TRUE, blocs, cutoff, "altitude (globale)"),
                    error = function(e) { msg("  tendance globale ecartee : %s", conditionMessage(e)); NULL })
  }
  if (!is.null(loc)) { L$rmse_locale <- loc$rmse; L$nash_locale <- loc$nse }
  if (!is.null(glo)) { L$rmse_globale <- glo$rmse; L$nash_globale <- glo$nse }
  # candidat valide : (presque) toutes les predictions existent et l'erreur reste du meme ordre que le krigeage ordinaire
  ok <- function(e) !is.null(e) && e$n_na <= 0.005 * nrow(pts) && is.finite(e$rmse) && e$rmse < 5 * ref$rmse
  cands <- list(loc = loc, glo = glo)
  valides <- vapply(cands, ok, TRUE)
  meilleur <- NA_character_
  if (any(valides)) {
    rm_ <- vapply(cands[valides], function(e) e$rmse, 0)
    meilleur <- names(which.min(rm_))
    # parcimonie : un gain de moins de 1 % ne justifie pas une tendance globale plutot que la derive locale
    if (isTRUE(valides[["loc"]]) && rm_[[meilleur]] > 0.99 * cands$loc$rmse) meilleur <- "loc"
  }
  retenu <- if (!is.na(meilleur) && cands[[meilleur]]$rmse < ref$rmse) meilleur else "ord"    # la tendance doit battre l'ordinaire
  ev <- switch(retenu, ord = ref, loc = loc, glo = glo)
  formule <- if (retenu == "ord") valeur ~ 1 else valeur ~ alt_km
  L$methode <- switch(retenu, ord = "krigeage ordinaire (sans altitude)", loc = "derive externe sur l'altitude", glo = "regression sur l'altitude + krigeage des residus")
  L$variogramme <- ev$choix
  L$pepite <- ev$modele$psill[1]; L$palier <- sum(ev$modele$psill[-1]); L$portee_km <- max(ev$modele$range) / 1000
  L$part_pepite <- L$pepite / (L$pepite + L$palier)
  L$rmse <- ev$rmse; L$mae <- ev$mae; L$biais <- ev$biais; L$nash <- ev$nse
  L$gain_rmse_pct <- 100 * (ev$rmse / ref$rmse - 1)
  msg("  -> retenue : %s | RMSE %.4g (%+.1f %% par rapport a l'ordinaire) | Nash %.3f | pepite %.0f %% de la variance residuelle",
      L$methode, ev$rmse, L$gain_rmse_pct, ev$nse, 100 * L$part_pepite)

  # -- Surface ---------------------------------------------------------------------------------
  n_bornees <- 0L
  if (retenu != "ord") {
    nouv <- sf::st_sf(alt_km = alt_mailles / 1000, geometry = grille)
    if (!par$alt_libre) {      # la tendance n'est jamais extrapolee au-dela de la plage observee aux stations
      lo <- min(pts$alt_km); hi <- max(pts$alt_km)
      n_hors <- sum(nouv$alt_km < lo | nouv$alt_km > hi, na.rm = TRUE)
      if (n_hors) msg("  %d maille(s) hors de la plage d'altitude des stations : ramenee a [%.2f ; %.2f] km.", n_hors, lo, hi)
      nouv$alt_km <- pmin(pmax(nouv$alt_km, lo), hi)
    }
    nouv <- nouv[is.finite(nouv$alt_km), ]
  } else {
    nouv <- sf::st_sf(geometry = grille)
  }
  if (retenu == "glo") {
    reg <- stats::lm(formule, data = sf::st_drop_geometry(pts))
    pts$res <- stats::residuals(reg)
    pred <- gstat::krige(res ~ 1, pts, newdata = nouv, model = ev$modele, nmax = NMAX, debug.level = 0)
    pred$var1.pred <- pred$var1.pred + as.numeric(stats::predict(reg, sf::st_drop_geometry(nouv)))
  } else {
    pred <- gstat::krige(formule, pts, newdata = nouv, model = ev$modele, nmax = NMAX, debug.level = 0)
  }
  g <- as.data.frame(sf::st_coordinates(pred))
  g$valeur <- pred$var1.pred
  g$ecart_type <- sqrt(pmax(pred$var1.var, 0))
  g <- g[is.finite(g$valeur), , drop = FALSE]
  if (is.finite(par$lo)) { n_bornees <- n_bornees + sum(g$valeur < par$lo); g$valeur <- pmax(g$valeur, par$lo) }
  if (is.finite(par$hi)) { n_bornees <- n_bornees + sum(g$valeur > par$hi); g$valeur <- pmin(g$valeur, par$hi) }
  if (n_bornees) msg("  %d maille(s) ramenee(s) dans les bornes physiques [%s ; %s].", n_bornees,
                     if (is.finite(par$lo)) par$lo else "-", if (is.finite(par$hi)) par$hi else "-")
  L$n_mailles <- nrow(g); L$pred_min <- min(g$valeur); L$pred_max <- max(g$valeur)
  L$ecart_type_median <- stats::median(g$ecart_type); L$n_mailles_bornees <- n_bornees
  msg("  Surface de %.4g a %.4g sur %d mailles (stations : de %.4g a %.4g) ; ecart-type median de prediction %.3g.",
      L$pred_min, L$pred_max, L$n_mailles, L$obs_min, L$obs_max, L$ecart_type_median)

  tryCatch({
    rr <- terra::rast(g[, c("X", "Y", "valeur", "ecart_type")], type = "xyz", crs = "EPSG:2154")
    names(rr) <- c(cle, paste0("ecart_type_", cle))
    terra::writeRaster(rr, file.path(DOSSIER, "sorties", sprintf("%s_%s.tif", cle, SUF)), overwrite = TRUE,
                       gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"))
  }, error = function(e) msg("  GeoTIFF non ecrit : %s", conditionMessage(e)))

  carte_et_diagnostic(cle, par, L, g, pts, xy, ev, ref, retenu, cutoff, nfold)
  L$duree_s <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  en_ligne(L)
}


# ==============================================================================
# 4. Carte et diagnostic d'une couche
# ==============================================================================

carte_et_diagnostic <- function(cle, par, L, g, pts, xy, ev, ref, retenu, cutoff, nfold) {
  inf <- info_couche(cle)
  q   <- as.numeric(stats::quantile(c(g$valeur, pts$valeur), c(0.02, 0.98)))      # bornes de couleur : quantiles 2 % et 98 %
  pas <- if (cle == "petp") 50 else 1
  lim <- c(floor(q[1] / pas) * pas, ceiling(q[2] / pas) * pas)
  if (is.finite(par$lo)) lim[1] <- max(lim[1], par$lo)
  if (lim[2] <= lim[1]) lim <- lim + c(-0.5, 0.5)
  nom_leg <- if (nzchar(inf$unite)) inf$unite else "valeur"
  coul <- PALETTES[[par$pal]]
  echelle <- if (par$pal == "bilan" && lim[1] < 0 && lim[2] > 0) {
    ggplot2::scale_fill_gradientn(colours = coul, limits = lim, oob = scales::squish, name = nom_leg,
      rescaler = function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) scales::rescale_mid(x, to, from, mid = 0))
  } else {
    ggplot2::scale_fill_gradientn(colours = coul, limits = lim, oob = scales::squish, name = nom_leg)
  }
  couche_dep <- if (!is.null(deps)) ggplot2::geom_sf(data = deps, fill = NA, colour = "grey35", linewidth = 0.12, inherit.aes = FALSE)
  pts_df <- data.frame(xy, valeur = pts$valeur)
  tendance_txt <- switch(retenu, ord = "sans altitude", loc = "d\u00e9rive externe sur l'altitude (MNT)", glo = "r\u00e9gression sur l'altitude (MNT) + krigeage des r\u00e9sidus")
  pied <- sprintf("%d stations M\u00e9t\u00e9o-France publi\u00e9es par ClimaFor ; krigeage, %s, variogramme %s.\nValidation crois\u00e9e (%d groupes au hasard) : RMSE %.3g, Nash %.2f ; sans altitude : RMSE %.3g, Nash %.2f.",
                  nrow(pts), tendance_txt, ev$choix, nfold, ev$rmse, ev$nse, ref$rmse, ref$nse)
  theme_c <- ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14),
                   plot.caption = ggplot2::element_text(size = 8, colour = "grey30", hjust = 0),
                   plot.margin = ggplot2::margin(10, 10, 8, 10), legend.position = "right")
  carte <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = g, ggplot2::aes(X, Y, fill = valeur)) +
    couche_dep +
    ggplot2::geom_point(data = pts_df, ggplot2::aes(X, Y, fill = valeur), shape = 21, size = 1.5, stroke = 0.25, colour = "grey15") +
    echelle + ggplot2::coord_sf(crs = 2154, datum = NA) +
    ggplot2::labs(title = sprintf("%s, %d", inf$titre, ANNEE),
                  subtitle = "Surface krig\u00e9e ; les points sont les stations (m\u00eame \u00e9chelle de couleur)", caption = pied) + theme_c
  suppressWarnings(ggplot2::ggsave(file.path(DOSSIER, "sorties", sprintf("carte_%s_%s.png", cle, SUF)), carte,
                                   width = 9, height = 8.5, dpi = DPI, bg = "white"))

  # Diagnostic : variogramme (points + modele), validation croisee, valeur selon l'altitude
  grDevices::png(file.path(DOSSIER, "sorties", sprintf("diagnostic_%s_%d.png", cle, ANNEE)), width = 3000, height = 950, res = 200)
  op <- graphics::par(mfrow = c(1, 3), mar = c(4.5, 4.8, 3.2, 1), cex.main = 0.95)
  graphics::plot(ev$v$dist / 1000, ev$v$gamma, pch = 16, col = "grey25", xlab = "Distance (km)", ylab = "Semi-variance",
                 ylim = c(0, max(ev$v$gamma) * 1.1), main = sprintf("%s : variogramme%s (%s)", cle, if (retenu != "ord") " des r\u00e9sidus" else "", ev$choix))
  vl <- gstat::variogramLine(ev$modele, maxdist = cutoff)
  graphics::lines(vl$dist / 1000, vl$gamma, col = "#b2182b", lwd = 2)
  graphics::abline(h = ev$s2, lty = 3, col = "grey50")
  graphics::plot(ev$cv$var1.pred, ev$cv$observed, pch = 16, cex = 0.5, col = grDevices::rgb(0.1, 0.3, 0.5, 0.45),
                 xlab = "Pr\u00e9dit (validation crois\u00e9e)", ylab = "Observ\u00e9",
                 main = sprintf("Validation crois\u00e9e : RMSE %.3g, Nash %.2f\n(sans altitude : RMSE %.3g, Nash %.2f)", ev$rmse, ev$nse, ref$rmse, ref$nse))
  graphics::abline(0, 1, col = "grey30")
  tend <- stats::lm(valeur ~ alt_km, data = sf::st_drop_geometry(pts))
  graphics::plot(pts$alt, pts$valeur, pch = 16, cex = 0.5, col = grDevices::rgb(0.1, 0.3, 0.5, 0.45),
                 xlab = "Altitude de la station (m)", ylab = nom_leg,
                 main = sprintf("%s selon l'altitude\n(tendance globale : %+.3g par km, R\u00b2 = %.2f)", cle, stats::coef(tend)[2], summary(tend)$r.squared))
  graphics::abline(a = stats::coef(tend)[1], b = stats::coef(tend)[2] / 1000, col = "#b2182b", lwd = 2)
  graphics::par(op)
  invisible(grDevices::dev.off())
}


# ==============================================================================
# 5. Boucle sur les couches
# ==============================================================================

lignes <- list()
fichier_val <- file.path(DOSSIER, "sorties", sprintf("validation_couches_%d_%s.csv", ANNEE, GROUPE))
for (cle in COUCHES_DEMANDEES) {
  msg("\n==== %s ====", cle)
  L <- tryCatch(traiter_couche(cle), error = function(e) {
    msg("  ERREUR : %s", conditionMessage(e))
    l <- nouvelle_ligne(cle); l$statut <- paste("erreur :", conditionMessage(e)); en_ligne(l)
  })
  lignes[[cle]] <- L
  invisible(gc())
  # ecriture apres chaque couche : un arret en cours de route garde ce qui est deja calcule
  utils::write.csv(do.call(rbind, lignes), fichier_val, row.names = FALSE, fileEncoding = "UTF-8")
}
res <- do.call(rbind, lignes)
msg("\n==== Bilan (annee %d, maille %g m) ====", ANNEE, RESOLUTION_M)
for (i in seq_len(nrow(res))) with(res[i, ], msg("  %-7s %5s stations | RMSE %8s (sans altitude %8s) | Nash %6s | %s%s",
  cle, ifelse(is.na(n_stations), "-", n_stations), ifelse(is.na(rmse), "-", sprintf("%.3g", rmse)), ifelse(is.na(rmse_sans_alt), "-", sprintf("%.3g", rmse_sans_alt)),
  ifelse(is.na(nash), "-", sprintf("%.2f", nash)), ifelse(is.na(methode), "", methode), ifelse(!is.na(statut) & nzchar(statut), paste0(" [", statut, "]"), "")))
msg("\nTermine. Fichiers dans : %s", normalizePath(file.path(DOSSIER, "sorties")))
