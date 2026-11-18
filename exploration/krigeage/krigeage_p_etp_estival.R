# ==============================================================================
# krigeage_p_etp_estival.R
# Carte de P - ETP estival (juin a aout) par krigeage ordinaire, a partir des
# stations Meteo-France. SCRIPT AUTONOME : il ne depend pas de ClimaFor.
#
# Methode (memes definitions que ClimaFor, pour que les chiffres se recoupent)
#   - donnees : fichiers quotidiens RR-T-Vent, un par departement ("latest")
#   - code qualite 2 ("douteux") ecarte, comme dans l'application
#   - ETP : Hargreaves-Samani a partir de TN et TX (les fichiers n'ont pas d'ETP)
#   - estival = 1er juin - 31 aout ; P - ETP = somme(RR) - somme(ETP)
#   - une station est gardee si pluie ET temperatures sont renseignees au moins
#     SEUIL du temps sur la periode ; les jours sans pluie ou sans ETP sont
#     retires des DEUX sommes (jamais compte comme zero)
#   - krigeage ordinaire (gstat), voisinage glissant, variogramme ajuste par
#     Exp / Sph / Mat (le meilleur est retenu), validation croisee
#
# Sorties (dossier DOSSIER/sorties) : la carte, la carte d'incertitude, un
# diagnostic (variogramme + validation croisee) et le tableau par station.
#
# Paquets : sf, gstat, ggplot2, httr2 (tous installables depuis CRAN).
# ==============================================================================

# ---- Reglages ----------------------------------------------------------------
# Chaque reglage a une valeur par defaut ; une variable d'environnement KRIGEAGE_* la remplace
# (c'est ce que fait le workflow GitHub). Lance a la main dans RStudio, modifie simplement les
# valeurs par defaut ci-dessous.
env_ou <- function(nom, defaut) { v <- trimws(Sys.getenv(nom, "")); if (nzchar(v)) v else defaut }

ANNEE        <- as.integer(env_ou("KRIGEAGE_ANNEE", "2026"))
# Departements : vide = France metropolitaine ; ou une liste, par ex. "31,09,2A"
DEPARTEMENTS <- {
  v <- env_ou("KRIGEAGE_DEPARTEMENTS", "")
  if (nzchar(v)) toupper(trimws(strsplit(v, "[,; ]+")[[1]])) else NULL
}
SEUIL        <- as.numeric(env_ou("KRIGEAGE_SEUIL", "0.95"))      # part minimale de jours exploitables (comme l'appli)
RESOLUTION_M <- as.numeric(env_ou("KRIGEAGE_RESOLUTION_M", "5000"))  # taille de maille de la carte, en metres
NMAX         <- 40        # nombre de stations voisines utilisees par le krigeage
NFOLD_CV     <- 10        # validation croisee en N blocs ; NULL = "un a un" (exact, ~3 min pour 1400 stations)
DOSSIER      <- env_ou("KRIGEAGE_DOSSIER", "krigeage_p_etp")   # cache des fichiers + sorties

stopifnot(is.finite(ANNEE), ANNEE >= 1950L, ANNEE <= as.integer(format(Sys.Date(), "%Y")),
          is.finite(SEUIL), SEUIL > 0, SEUIL <= 1, is.finite(RESOLUTION_M), RESOLUTION_M >= 500)
if (!is.null(DEPARTEMENTS) && !all(grepl("^([0-9]{2}|2[AB])$", DEPARTEMENTS))) {
  stop("KRIGEAGE_DEPARTEMENTS : codes attendus sur deux caracteres (31, 09, 2A...).", call. = FALSE)
}

# ---- Paquets -----------------------------------------------------------------
for (p in c("sf", "gstat", "ggplot2", "httr2")) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop("Paquet manquant : ", p, " -> install.packages(\"", p, "\")", call. = FALSE)
  }
}
options(timeout = max(600, getOption("timeout")))
dir.create(file.path(DOSSIER, "fichiers"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DOSSIER, "sorties"),  recursive = TRUE, showWarnings = FALSE)
msg <- function(...) cat(sprintf(...), "\n", sep = "")

if (is.null(DEPARTEMENTS)) {
  DEPARTEMENTS <- c(sprintf("%02d", c(1:19, 21:95)), "2A", "2B")
}
DEBUT <- as.Date(sprintf("%d-06-01", ANNEE))
FIN   <- as.Date(sprintf("%d-08-31", ANNEE))
JOURS <- seq(DEBUT, FIN, by = "day")
N_ATTENDU <- length(JOURS)                       # 92 jours


# ==============================================================================
# 1. Fichiers Meteo-France : catalogue, telechargement, lecture
# ==============================================================================

ID_JEU        <- "6569b51ae64326786e4e8e1a"
URL_CAT_V2    <- sprintf("https://www.data.gouv.fr/api/2/datasets/%s/resources/", ID_JEU)
URL_CAT_V1    <- sprintf("https://www.data.gouv.fr/api/1/datasets/%s/", ID_JEU)
URL_MIROIR    <- "https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT"

requete <- function(url, query = NULL) {
  r <- httr2::request(url)
  if (length(query)) r <- do.call(httr2::req_url_query, c(list(.req = r), query))
  r <- httr2::req_user_agent(httr2::req_timeout(r, 90), "krigeage-p-etp-estival (R, exploration)")
  httr2::req_retry(r, max_tries = 3)
}

# Ressources du jeu, API v2 paginee (a defaut : API v1, le jeu complet en un appel).
ressources_v2 <- function() {
  sortie <- list(); page <- 1L
  repeat {
    j <- httr2::resp_body_json(httr2::req_perform(requete(URL_CAT_V2,
           list(page = page, page_size = 100, type = "main"))), simplifyVector = FALSE)
    if (!length(j$data)) break
    sortie <- c(sortie, j$data)
    suivante <- j$next_page
    if (is.null(suivante) || !nzchar(as.character(suivante)[1]) || page >= 80L) break
    page <- page + 1L
  }
  sortie
}
ressources_v1 <- function() {
  httr2::resp_body_json(httr2::req_perform(requete(URL_CAT_V1)), simplifyVector = FALSE)$resources
}

# Adresses des fichiers "RR-T-Vent" qui couvrent ANNEE (un par departement).
# Si le catalogue ne repond pas : adresses du miroir (denomination "latest-2025-2026").
catalogue_fichiers <- function() {
  res <- NULL
  for (methode in list(ressources_v2, ressources_v1)) {
    res <- tryCatch(methode(), error = function(e) { msg("Catalogue : %s", conditionMessage(e)); NULL })
    if (length(res) && is.list(res)) break
  }

  out <- NULL
  if (length(res)) {
    get <- function(r, k) { v <- r[[k]]; if (is.null(v) || !length(v)) "" else as.character(v[[1]]) }
    titre <- vapply(res, get, "", k = "title")
    url   <- vapply(res, get, "", k = "url")
    fich  <- basename(sub("[?#].*$", "", url))
    texte <- paste(titre, fich)
    garde <- grepl("RR-T-Vent", texte, fixed = TRUE) & startsWith(url, "https://")
    if (any(garde)) {
      # departement : dans le nom du fichier (Q_31_...) ou, a defaut, dans le titre (departement_31_...)
      dept <- sub("^Q_([0-9]{2,3}|2[AB])_.*$", "\\1", fich[garde])
      dept[!grepl("^([0-9]{2,3}|2[AB])$", dept)] <- NA
      d_titre <- sub("^.*departement_([0-9]{2,3}|2[AB])(_.*)?$", "\\1", titre[garde])
      d_titre[!grepl("^([0-9]{2,3}|2[AB])$", d_titre)] <- NA
      dept[is.na(dept)] <- d_titre[is.na(dept)]
      an1 <- suppressWarnings(as.integer(sub("^.*?(\\d{4})-\\d{4}.*$", "\\1", texte[garde], perl = TRUE)))
      an2 <- suppressWarnings(as.integer(sub("^.*?\\d{4}-(\\d{4}).*$", "\\1", texte[garde], perl = TRUE)))
      out <- data.frame(dept = dept, debut = an1, fin = an2, url = url[garde],
                        nom = fich[garde], stringsAsFactors = FALSE)
      out <- out[!is.na(out$dept) & !is.na(out$debut) & !is.na(out$fin) &
                   out$debut <= ANNEE & out$fin >= ANNEE, , drop = FALSE]
      out <- out[!duplicated(out$dept), , drop = FALSE]
    }
  }
  if (is.null(out) || !nrow(out)) {
    msg("Catalogue data.gouv.fr indisponible : adresses du miroir (latest-2025-2026).")
    out <- data.frame(dept = DEPARTEMENTS, debut = 2025L, fin = 2026L,
                      url = sprintf("%s/Q_%s_latest-2025-2026_RR-T-Vent.csv.gz", URL_MIROIR, DEPARTEMENTS),
                      nom = sprintf("Q_%s_latest-2025-2026_RR-T-Vent.csv.gz", DEPARTEMENTS),
                      stringsAsFactors = FALSE)
  }
  # Nom du fichier dans le cache : celui de Meteo-France (on peut donc y deposer a la main un
  # fichier telecharge par ailleurs), ou un nom construit si l'adresse n'en porte pas.
  ok_nom <- grepl("^Q_.*\\.csv\\.gz$", out$nom)
  out$nom[!ok_nom] <- sprintf("Q_%s_%d-%d_RR-T-Vent.csv.gz", out$dept[!ok_nom], out$debut[!ok_nom], out$fin[!ok_nom])
  out
}

# Telecharge dans le cache (sans rien refaire si le fichier est deja la).
telecharger <- function(url, dest) {
  if (file.exists(dest) && file.size(dest) > 0) return(TRUE)
  tmp <- paste0(dest, ".part")
  ok <- tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) unlink(tmp)
  if (!ok || !file.exists(tmp)) return(FALSE)
  entete <- readBin(tmp, "raw", 2L)                 # un fichier .gz commence par 1f 8b
  if (length(entete) < 2L || !identical(as.integer(entete), c(31L, 139L))) { unlink(tmp); return(FALSE) }
  file.rename(tmp, dest)
}

COLONNES <- c("NUM_POSTE", "NOM_USUEL", "LAT", "LON", "ALTI", "AAAAMMJJ",
              "RR", "QRR", "TN", "QTN", "TX", "QTX")

# Lit un fichier departemental : seules les colonnes utiles, et seulement juin-aout.
lire_fichier <- function(chemin) {
  con <- gzfile(chemin, open = "rt")
  on.exit(close(con), add = TRUE)
  entete <- readLines(con, n = 1L, warn = FALSE)
  if (!length(entete)) stop("fichier vide")
  # L'en-tete ne contient que des lettres, chiffres, "_" et ";" : tout le reste (BOM, retour
  # chariot, guillemets) est retire octet par octet, quelle que soit la locale.
  entete <- gsub("[^A-Za-z0-9_;]", "", entete, useBytes = TRUE)
  noms <- strsplit(entete, ";", fixed = TRUE)[[1]]
  if (!all(c("NUM_POSTE", "AAAAMMJJ") %in% noms)) stop("format inattendu (NUM_POSTE / AAAAMMJJ absents)")

  lignes <- readLines(con, warn = FALSE)
  if (!length(lignes)) return(NULL)
  # Seules les lignes de juin a aout (date en 6e colonne) sont analysees : le fichier couvre deux ans.
  lignes <- lignes[grepl(sprintf(";%d0[678][0-9]{2};", ANNEE), lignes)]
  if (!length(lignes)) return(NULL)
  mauvais <- !validUTF8(lignes)
  if (any(mauvais)) lignes[mauvais] <- iconv(lignes[mauvais], "latin1", "UTF-8", sub = "")

  # Colonnes retrouvees par leur NOM ; trois colonnes de marge absorbent un ";" final.
  classes <- c(ifelse(noms %in% COLONNES, "character", "NULL"), rep("NULL", 3))
  df <- utils::read.table(text = lignes, sep = ";", header = FALSE, quote = "", comment.char = "",
                          col.names = c(noms, paste0("EXTRA", 1:3)), colClasses = classes,
                          na.strings = c("", "NA"), fill = TRUE, strip.white = TRUE,
                          stringsAsFactors = FALSE)
  num <- function(k) if (k %in% names(df)) suppressWarnings(as.numeric(df[[k]])) else rep(NA_real_, nrow(df))
  b <- data.frame(id = df$NUM_POSTE,
                  nom = if ("NOM_USUEL" %in% names(df)) df$NOM_USUEL else NA_character_,
                  lat = num("LAT"), lon = num("LON"), alt = num("ALTI"),
                  date = as.Date(df$AAAAMMJJ, format = "%Y%m%d"),
                  rr = num("RR"), qrr = num("QRR"), tn = num("TN"), qtn = num("QTN"),
                  tx = num("TX"), qtx = num("QTX"), stringsAsFactors = FALSE)
  b <- b[!is.na(b$date) & b$date >= DEBUT & b$date <= FIN, , drop = FALSE]
  b
}

# Code qualite 2 ("douteux") -> NA, avec le meme garde-fou que l'appli : si ce
# filtre retirait plus de la moitie des valeurs d'une variable, on ne filtre pas.
appliquer_qualite <- function(b, ecartes = 2) {
  for (p in list(c("rr", "qrr"), c("tn", "qtn"), c("tx", "qtx"))) {
    mauvais <- !is.na(b[[p[1]]]) & !is.na(b[[p[2]]]) & b[[p[2]]] %in% ecartes
    n <- sum(!is.na(b[[p[1]]]))
    if (n > 0L && sum(mauvais) / n > 0.5) next
    b[[p[1]]][mauvais] <- NA_real_
  }
  b
}


# ==============================================================================
# 2. P - ETP estival par station
# ==============================================================================

# ETP journaliere de Hargreaves-Samani (1985) : identique a etp_hargreaves() de ClimaFor.
etp_hargreaves <- function(t_moy, t_min, t_max, doy, lat) {
  phi   <- lat * pi / 180
  dr    <- 1 + 0.033 * cos(2 * pi * doy / 365)
  delta <- 0.409 * sin(2 * pi * doy / 365 - 1.39)
  x  <- pmin(pmax(-tan(phi) * tan(delta), -1), 1)
  ws <- acos(x)
  ra <- (24 * 60 / pi) * 0.0820 * dr * (ws * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(ws))
  pmax(0.0023 * (t_moy + 17.8) * sqrt(pmax(t_max - t_min, 0)) * ra * 0.408, 0)
}

premier <- function(x) x[!is.na(x)][1]

bilan_station <- function(s) {
  s <- s[!duplicated(s$date), , drop = FALSE]
  i <- match(JOURS, s$date)
  rr <- s$rr[i]; tn <- s$tn[i]; tx <- s$tx[i]
  doy <- as.integer(format(JOURS, "%j"))
  lat <- premier(s$lat)
  etp <- etp_hargreaves((tn + tx) / 2, tn, tx, doy, lat)
  ok  <- !is.na(rr) & !is.na(etp)
  data.frame(id = s$id[1], nom = premier(s$nom), lon = premier(s$lon), lat = lat, alt = premier(s$alt),
             n_jours = sum(ok), n_pluie = sum(!is.na(rr)), n_temp = sum(!is.na(etp)),
             p_mm = if (any(ok)) sum(rr[ok]) else NA_real_,
             etp_mm = if (any(ok)) sum(etp[ok]) else NA_real_, stringsAsFactors = FALSE)
}


# ==============================================================================
# 3. Lecture de tous les departements
# ==============================================================================

cat_f <- catalogue_fichiers()
absents <- setdiff(DEPARTEMENTS, cat_f$dept)
cat_f <- cat_f[cat_f$dept %in% DEPARTEMENTS, , drop = FALSE]
msg("%d departement(s) a lire pour l'ete %d.", nrow(cat_f), ANNEE)
if (length(absents)) msg("Sans fichier dans le catalogue : %s", paste(absents, collapse = ", "))

brut <- list(); non_lus <- character(0)
for (k in seq_len(nrow(cat_f))) {
  d <- cat_f$dept[k]
  dest <- file.path(DOSSIER, "fichiers", cat_f$nom[k])
  if (!telecharger(cat_f$url[k], dest)) { non_lus <- c(non_lus, d); msg("  %s : telechargement impossible", d); next }
  b <- tryCatch(lire_fichier(dest), error = function(e) { msg("  %s : %s", d, conditionMessage(e)); NULL })
  if (is.null(b) || !nrow(b)) { non_lus <- c(non_lus, d); next }
  brut[[length(brut) + 1L]] <- appliquer_qualite(b)
  msg("  %s : %d station(s)", d, length(unique(b$id)))
}
if (!length(brut)) stop("Aucune donnee lue : voir les messages ci-dessus.", call. = FALSE)
brut <- do.call(rbind, brut)

tab <- do.call(rbind, lapply(split(brut, brut$id), bilan_station))
rownames(tab) <- NULL
n_total <- nrow(tab)
tab <- tab[is.finite(tab$lon) & is.finite(tab$lat), , drop = FALSE]
seuil_j <- ceiling(SEUIL * N_ATTENDU)
tab$retenue <- tab$n_jours >= seuil_j
tab$bilan_mm <- tab$p_mm - tab$etp_mm
msg("\n%d stations lues ; %d avec pluie, %d avec temperatures ; %d retenues (>= %d jours sur %d).",
    n_total, sum(tab$n_pluie >= seuil_j), sum(tab$n_temp >= seuil_j), sum(tab$retenue), seuil_j, N_ATTENDU)
if (length(non_lus)) msg("Departements non lus : %s", paste(non_lus, collapse = ", "))

utils::write.csv(tab[order(tab$id), ], file.path(DOSSIER, "sorties", sprintf("stations_p_etp_jja_%d.csv", ANNEE)),
                 row.names = FALSE, fileEncoding = "UTF-8")
obs <- tab[tab$retenue, , drop = FALSE]
if (nrow(obs) < 30) stop("Moins de 30 stations exploitables : le krigeage n'aurait pas de sens.", call. = FALSE)


# ==============================================================================
# 4. Krigeage ordinaire
# ==============================================================================

pts <- sf::st_transform(sf::st_as_sf(obs, coords = c("lon", "lat"), crs = 4326), 2154)
pts <- pts[!duplicated(sf::st_coordinates(pts)), ]       # deux stations au meme point : une seule
xy  <- sf::st_coordinates(pts)

# Contour de la France metropolitaine (API Geo) ; a defaut, enveloppe des stations.
contour_france <- function() {
  f <- file.path(DOSSIER, "departements_contours.geojson")
  if (!file.exists(f)) {
    ok <- tryCatch({
      suppressWarnings(utils::download.file(
        "https://geo.api.gouv.fr/departements?fields=nom,code&format=geojson&geometry=contour",
        f, mode = "wb", quiet = TRUE)); TRUE }, error = function(e) FALSE)
    if (!ok) unlink(f)
    if (!ok) return(NULL)
  }
  d <- tryCatch(sf::read_sf(f, quiet = TRUE), error = function(e) NULL)
  if (is.null(d) || !("code" %in% names(d))) return(NULL)
  d <- d[grepl("^([0-9]{2}|2[AB])$", d$code), ]
  if (!nrow(d)) return(NULL)
  sf::st_transform(sf::st_make_valid(d), 2154)
}
deps <- contour_france()
if (is.null(deps)) {
  msg("Contour des departements indisponible : enveloppe des stations (+ 25 km) a la place.")
  masque <- sf::st_buffer(sf::st_convex_hull(sf::st_union(sf::st_geometry(pts))), 25000)
  deps <- NULL
} else {
  masque <- sf::st_union(sf::st_geometry(deps))
}

# -- Variogramme experimental et ajustement ------------------------------------
etendue <- sqrt(diff(range(xy[, 1]))^2 + diff(range(xy[, 2]))^2)
cutoff  <- min(400000, etendue / 3)
v <- gstat::variogram(bilan_mm ~ 1, pts, cutoff = cutoff, width = cutoff / 30)
s2 <- stats::var(pts$bilan_mm)
essais <- list()
for (m in c("Exp", "Sph", "Mat")) {
  ini <- gstat::vgm(psill = 0.8 * s2, model = m, range = cutoff / 3, nugget = 0.2 * s2,
                    kappa = if (m == "Mat") 1 else 0.5)
  aj <- tryCatch(suppressWarnings(gstat::fit.variogram(v, ini)), error = function(e) NULL)
  if (!is.null(aj) && !isTRUE(attr(aj, "singular")) && all(aj$psill >= 0) && all(aj$range >= 0)) {
    essais[[m]] <- list(modele = aj, sserr = attr(aj, "SSErr"))
  }
}
if (!length(essais)) {
  msg("Ajustement du variogramme impossible : modele exponentiel par defaut.")
  modele <- gstat::vgm(0.8 * s2, "Exp", cutoff / 3, 0.2 * s2); choix <- "Exp (defaut)"
} else {
  best <- names(essais)[which.min(vapply(essais, function(e) e$sserr, 0))]
  modele <- essais[[best]]$modele; choix <- best
}
msg("\nVariogramme retenu : %s | pepite %.0f, palier partiel %.0f, portee %.0f km (variance des donnees : %.0f)",
    choix, modele$psill[1], sum(modele$psill[-1]), max(modele$range) / 1000, s2)

# -- Validation croisee : un bloc de stations est retire, predit, puis compare -----
# (le variogramme reste celui ajuste sur toutes les stations)
set.seed(2026)
nfold <- if (is.null(NFOLD_CV)) nrow(pts) else min(NFOLD_CV, nrow(pts))
cv <- gstat::krige.cv(bilan_mm ~ 1, pts, model = modele, nmax = NMAX, nfold = nfold, verbose = FALSE)
res <- cv$observed - cv$var1.pred
rmse <- sqrt(mean(res^2)); biais <- mean(cv$var1.pred - cv$observed); mae <- mean(abs(res))
nse <- 1 - sum(res^2) / sum((cv$observed - mean(cv$observed))^2)
msg("Validation croisee (%d stations, %s) : RMSE %.0f mm | ecart absolu moyen %.0f mm | biais %+.1f mm | efficacite (Nash) %.2f",
    nrow(pts), if (nfold == nrow(pts)) "un a un" else sprintf("%d blocs", nfold), rmse, mae, biais, nse)

# -- Grille et prediction -------------------------------------------------------
grille <- sf::st_make_grid(masque, cellsize = RESOLUTION_M, what = "centers")
grille <- grille[sf::st_intersects(grille, masque, sparse = FALSE)[, 1]]
msg("Prediction sur %d mailles de %g km...", length(grille), RESOLUTION_M / 1000)
pred <- gstat::krige(bilan_mm ~ 1, pts, newdata = sf::st_sf(geometry = grille), model = modele,
                     nmax = NMAX, debug.level = 0)
g <- as.data.frame(sf::st_coordinates(pred))
g$bilan <- pred$var1.pred
g$ecart_type <- sqrt(pmax(pred$var1.var, 0))


# ==============================================================================
# 5. Cartes et diagnostic
# ==============================================================================

# Echelle : le zero (equilibre P = ETP) est toujours en blanc ; les bornes suivent les donnees
# (l'ete, P - ETP est presque partout negatif : une echelle symetrique ecraserait la carte).
q <- as.numeric(stats::quantile(c(g$bilan, pts$bilan_mm), c(0.02, 0.98)))
lim_bas  <- min(-50, floor(q[1] / 50) * 50)
lim_haut <- max(50, ceiling(q[2] / 50) * 50)
echelle <- ggplot2::scale_fill_gradientn(
  colours = c("#543005", "#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571", "#003c30"),
  limits = c(lim_bas, lim_haut), oob = scales::squish, name = "P \u2212 ETP (mm)",
  rescaler = function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) scales::rescale_mid(x, to, from, mid = 0))
couche_dep <- if (!is.null(deps)) ggplot2::geom_sf(data = deps, fill = NA, colour = "grey35", linewidth = 0.12, inherit.aes = FALSE)
pts_df <- data.frame(xy, bilan = pts$bilan_mm)
tit_per <- sprintf("juin \u00e0 ao\u00fbt %d", ANNEE)
pied <- sprintf("%d stations M\u00e9t\u00e9o-France ; ETP Hargreaves-Samani ; krigeage ordinaire (%s). Validation crois\u00e9e (%s) : RMSE %.0f mm, biais %+.1f mm.",
                nrow(pts), choix, if (nfold == nrow(pts)) "un \u00e0 un" else sprintf("%d blocs", nfold), rmse, biais)
theme_c <- ggplot2::theme_void(base_size = 11) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14),
                 plot.caption = ggplot2::element_text(size = 8, colour = "grey30", hjust = 0),
                 plot.margin = ggplot2::margin(10, 10, 8, 10), legend.position = "right")

carte <- ggplot2::ggplot() +
  ggplot2::geom_raster(data = g, ggplot2::aes(X, Y, fill = bilan)) +
  couche_dep +
  ggplot2::geom_point(data = pts_df, ggplot2::aes(X, Y, fill = bilan), shape = 21,
                      size = 1.5, stroke = 0.25, colour = "grey15") +
  echelle +
  ggplot2::coord_sf(crs = 2154, datum = NA) +
  ggplot2::labs(title = sprintf("P \u2212 ETP estival, %s", tit_per),
                subtitle = "Surface krig\u00e9e ; les points sont les stations (m\u00eame \u00e9chelle de couleur)",
                caption = pied) + theme_c
ggplot2::ggsave(file.path(DOSSIER, "sorties", sprintf("carte_p_etp_jja_%d.png", ANNEE)), carte,
                width = 9, height = 8.5, dpi = 200, bg = "white")

carte_inc <- ggplot2::ggplot() +
  ggplot2::geom_raster(data = g, ggplot2::aes(X, Y, fill = ecart_type)) +
  couche_dep +
  ggplot2::geom_point(data = pts_df, ggplot2::aes(X, Y), shape = 16, size = 0.5, colour = "grey15") +
  ggplot2::scale_fill_viridis_c(option = "magma", direction = -1, name = "\u00c9cart-type\n(mm)") +
  ggplot2::coord_sf(crs = 2154, datum = NA) +
  ggplot2::labs(title = "Incertitude du krigeage (\u00e9cart-type de pr\u00e9diction)",
                subtitle = "Faible pr\u00e8s des stations, forte l\u00e0 o\u00f9 le r\u00e9seau est lointain",
                caption = pied) + theme_c
ggplot2::ggsave(file.path(DOSSIER, "sorties", sprintf("incertitude_krigeage_%d.png", ANNEE)), carte_inc,
                width = 9, height = 8.5, dpi = 200, bg = "white")

# Diagnostic : variogramme (points + modele) et validation croisee (observe / predit)
grDevices::png(file.path(DOSSIER, "sorties", sprintf("diagnostic_krigeage_%d.png", ANNEE)),
               width = 2000, height = 950, res = 200)
op <- graphics::par(mfrow = c(1, 2), mar = c(4.5, 4.8, 3, 1), cex.main = 0.95)
graphics::plot(v$dist / 1000, v$gamma, pch = 16, col = "grey25", xlab = "Distance (km)",
               ylab = "Semi-variance (mm\u00b2)", ylim = c(0, max(v$gamma) * 1.1),
               main = sprintf("Variogramme (%s)", choix))
vl <- gstat::variogramLine(modele, maxdist = cutoff)
graphics::lines(vl$dist / 1000, vl$gamma, col = "#b2182b", lwd = 2)
graphics::abline(h = s2, lty = 3, col = "grey50")
graphics::plot(cv$var1.pred, cv$observed, pch = 16, cex = 0.5, col = grDevices::rgb(0.1, 0.3, 0.5, 0.45),
               xlab = "Pr\u00e9dit (validation crois\u00e9e, mm)", ylab = "Observ\u00e9 (mm)",
               main = sprintf("Validation crois\u00e9e : RMSE %.0f mm, Nash %.2f", rmse, nse))
graphics::abline(0, 1, col = "grey30")
graphics::par(op)
invisible(grDevices::dev.off())

msg("\nTermine. Fichiers dans : %s", normalizePath(file.path(DOSSIER, "sorties")))
