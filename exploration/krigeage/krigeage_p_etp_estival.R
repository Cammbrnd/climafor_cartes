# ==============================================================================
# krigeage_p_etp_estival.R
# Cartes estivales (juin a aout) par krigeage, a partir des stations Meteo-France :
# P - ETP (par defaut), pluie cumulee ou temperature moyenne, selon KRIGEAGE_VARIABLE
# (p_etp, pluie ou temperature). SCRIPT AUTONOME : il ne depend pas de ClimaFor.
#
# Methode (memes definitions que ClimaFor, pour que les chiffres se recoupent)
#   - donnees : fichiers quotidiens RR-T-Vent, un par departement ("latest")
#   - code qualite 2 ("douteux") ecarte, comme dans l'application
#   - ETP : Hargreaves-Samani a partir de TN et TX (les fichiers n'ont pas d'ETP)
#   - estival = 1er juin - 31 aout ; P - ETP = somme(RR) - somme(ETP)
#   - une station est gardee si pluie ET temperatures sont renseignees au moins
#     SEUIL du temps sur la periode ; les jours sans pluie ou sans ETP sont
#     retires des DEUX sommes (jamais compte comme zero)
#   - pluie : cumul des RR sur la periode (les jours manquants sont compenses au prorata) ; station
#     gardee si RR est renseigne au moins SEUIL du temps
#   - temperature : moyenne de (TN + TX) / 2 (comme dans l'ETP) ; station gardee si TN et TX sont
#     renseignes au moins SEUIL du temps
#   - krigeage (gstat), voisinage glissant, variogramme ajuste par Exp / Sph / Mat
#     (le meilleur est retenu), validation croisee
#   - avec MNT (par defaut) : krigeage a derive externe, l'altitude moyenne de chaque
#     maille (tuiles ouvertes, ou un GeoTIFF fourni) est la tendance ; sans MNT :
#     krigeage ordinaire. Les deux sont evalues, le gain est affiche dans le journal.
#
# Sorties (dossier DOSSIER/sorties) : la carte, la carte d'incertitude, un
# diagnostic (variogramme + validation croisee) et le tableau par station.
#
# Paquets : sf, gstat, ggplot2, httr2 ; terra pour le MNT (tous installables depuis CRAN).
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
# MNT : "oui" = l'altitude sert de tendance (derive externe), "non" = krigeage ordinaire.
MNT          <- tolower(env_ou("KRIGEAGE_MNT", "oui")) %in% c("oui", "o", "1", "true", "vrai", "yes")
MNT_ZOOM     <- as.integer(env_ou("KRIGEAGE_MNT_ZOOM", "7"))   # finesse des tuiles : 7 ~ 0,9 km, 8 ~ 0,4 km
MNT_FICHIER  <- env_ou("KRIGEAGE_MNT_FICHIER", "")             # GeoTIFF d'altitude (m) a utiliser a la place des tuiles
# Covariables candidates en plus de l'altitude (separees par des virgules, vide = altitude seule) :
#   expo = exposition des versants (pente x orientation, calculee sur le MNT)
#   mer  = proximite de la mer, exp(-distance a la mer / MER_ECHELLE_KM)
# Chaque combinaison est evaluee par validation croisee ; la meilleure est retenue.
# Defaut : aucune (altitude seule). Les essais de 2026 (exposition a 1, 2 et 5 km, distance a la mer) n'ont pas ete retenus.
COVARIABLES     <- intersect(trimws(strsplit(tolower(env_ou("KRIGEAGE_COVARIABLES", "")), ",")[[1]]), c("expo", "mer"))
# echelles testees pour la pente et l'exposition (km, separees par des virgules) : 1-2 km = versant de vallee, 5 km = relief regional
EXPO_ECHELLES_KM <- sort(unique(as.numeric(strsplit(env_ou("KRIGEAGE_EXPO_ECHELLE_KM", "1,2,5"), "[,; ]+")[[1]])))
cle_ech <- function(e) gsub("[.]", "p", sprintf("%g", e))    # 1.5 -> "1p5" (nom de colonne valide)
MER_ECHELLE_KM  <- as.numeric(env_ou("KRIGEAGE_MER_ECHELLE_KM", "50"))   # decroissance de l'effet littoral
NMAX         <- 40        # nombre de stations voisines utilisees par le krigeage
NFOLD_CV     <- 10        # validation croisee en N blocs ; NULL = "un a un" (exact, ~3 min pour 1400 stations)
DOSSIER      <- env_ou("KRIGEAGE_DOSSIER", "krigeage_p_etp")   # cache des fichiers + sorties
# Variable cartographiee : p_etp (defaut), pluie (cumul des precipitations) ou temperature (moyenne de (TN + TX) / 2)
VARIABLE     <- tolower(env_ou("KRIGEAGE_VARIABLE", "p_etp"))

stopifnot(is.finite(ANNEE), ANNEE >= 1950L, ANNEE <= as.integer(format(Sys.Date(), "%Y")),
          is.finite(SEUIL), SEUIL > 0, SEUIL <= 1, is.finite(RESOLUTION_M), RESOLUTION_M >= 500,
          is.finite(MNT_ZOOM), MNT_ZOOM >= 5L, MNT_ZOOM <= 9L,
          length(EXPO_ECHELLES_KM) >= 1L, all(is.finite(EXPO_ECHELLES_KM)), all(EXPO_ECHELLES_KM >= 1),
          is.finite(MER_ECHELLE_KM), MER_ECHELLE_KM > 0)
stopifnot(VARIABLE %in% c("p_etp", "pluie", "temperature"))
# Libelles et unites de la variable (u : graphiques ; u_log : journal ; u_col : noms de colonnes ; dec : decimales)
VAR <- switch(VARIABLE,
  p_etp = list(court = "P \u2212 ETP", titre = "P \u2212 ETP estival", u = "mm", u_log = "mm", u_col = "mm", dec = 0L,
               etiq = "", source = "ETP Hargreaves-Samani"),
  pluie = list(court = "Pluie", titre = "Pluie estivale", u = "mm", u_log = "mm", u_col = "mm", dec = 0L,
               etiq = "_pluie", source = "cumul des pr\u00e9cipitations (RR)"),
  temperature = list(court = "Temp\u00e9rature", titre = "Temp\u00e9rature moyenne estivale", u = "\u00b0C", u_log = "degC",
                     u_col = "degC", dec = 2L, etiq = "_temperature", source = "moyenne de (TN + TX) / 2"))
n0 <- function(x) formatC(x, format = "f", digits = VAR$dec)                          # RMSE, ecart absolu, variances
n1 <- function(x) formatC(x, format = "f", digits = VAR$dec + 1L, width = 5L)         # tableaux de comparaison
nb <- function(x) formatC(x, format = "f", digits = max(VAR$dec, 1L), flag = "+")     # biais (avec signe)
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
if (MNT && !requireNamespace("terra", quietly = TRUE)) {
  msg("Paquet terra absent : krigeage ordinaire, sans MNT (install.packages(\"terra\") pour l'activer).")
  MNT <- FALSE
}

# DEPARTEMENTS == NULL : France metropolitaine entiere, d'apres le catalogue (la Corse y figure
# sous "20" ou sous "2A" et "2B" selon les fichiers). Liste de secours si le catalogue est muet :
DEPARTEMENTS_SECOURS <- c(sprintf("%02d", c(1:19, 21:95)), "20", "2A", "2B")
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
    liste <- if (is.null(DEPARTEMENTS)) DEPARTEMENTS_SECOURS else DEPARTEMENTS
    out <- data.frame(dept = liste, debut = 2025L, fin = 2026L,
                      url = sprintf("%s/Q_%s_latest-2025-2026_RR-T-Vent.csv.gz", URL_MIROIR, liste),
                      nom = sprintf("Q_%s_latest-2025-2026_RR-T-Vent.csv.gz", liste),
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
             etp_mm = if (any(ok)) sum(etp[ok]) else NA_real_,
             # pluie sur la periode : somme des jours renseignes, ramenee a 92 jours ; temperature : moyenne de (TN + TX) / 2
             pluie_jja_mm = if (any(!is.na(rr))) sum(rr, na.rm = TRUE) * length(JOURS) / sum(!is.na(rr)) else NA_real_,
             t_moy_c = if (any(!is.na(etp))) mean(((tn + tx) / 2)[!is.na(etp)]) else NA_real_, stringsAsFactors = FALSE)
}


# ==============================================================================
# 3. Lecture de tous les departements
# ==============================================================================

cat_f <- catalogue_fichiers()
if (is.null(DEPARTEMENTS)) {
  cat_f <- cat_f[grepl("^([0-9]{2}|2[AB])$", cat_f$dept), , drop = FALSE]     # metropole (pas les DOM)
} else {
  absents <- setdiff(DEPARTEMENTS, cat_f$dept)
  cat_f <- cat_f[cat_f$dept %in% DEPARTEMENTS, , drop = FALSE]
  if (length(absents)) msg("Sans fichier dans le catalogue : %s", paste(absents, collapse = ", "))
}
cat_f <- cat_f[order(cat_f$dept), , drop = FALSE]
msg("%d departement(s) a lire pour l'ete %d : %s", nrow(cat_f), ANNEE, paste(cat_f$dept, collapse = " "))

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
tab$bilan_mm <- tab$p_mm - tab$etp_mm
# "valeur" = la variable krigee ; une station est gardee si les jours utiles a CETTE variable sont assez nombreux
tab$valeur  <- switch(VARIABLE, p_etp = tab$bilan_mm, pluie = tab$pluie_jja_mm, temperature = tab$t_moy_c)
tab$retenue <- switch(VARIABLE, p_etp = tab$n_jours, pluie = tab$n_pluie, temperature = tab$n_temp) >= seuil_j & is.finite(tab$valeur)
msg("\n%d stations lues ; %d avec pluie, %d avec temperatures ; %d retenues (>= %d jours sur %d).",
    n_total, sum(tab$n_pluie >= seuil_j), sum(tab$n_temp >= seuil_j), sum(tab$retenue), seuil_j, N_ATTENDU)
if (length(non_lus)) msg("Departements non lus : %s", paste(non_lus, collapse = ", "))

utils::write.csv(tab[order(tab$id), ], file.path(DOSSIER, "sorties", sprintf("stations_%s_jja_%d.csv", VARIABLE, ANNEE)),
                 row.names = FALSE, fileEncoding = "UTF-8")
obs <- tab[tab$retenue, , drop = FALSE]
if (nrow(obs) < 30) stop("Moins de 30 stations exploitables : le krigeage n'aurait pas de sens.", call. = FALSE)


# ==============================================================================
# 4. Krigeage ordinaire
# ==============================================================================

pts <- sf::st_transform(sf::st_as_sf(obs, coords = c("lon", "lat"), crs = 4326), 2154)
pts <- pts[!duplicated(sf::st_coordinates(pts)), ]       # deux stations au meme point : une seule
xy  <- sf::st_coordinates(pts)

# Contours des departements de la France metropolitaine : on essaie plusieurs sources et on dit
# pourquoi chacune echoue ; a defaut, enveloppe des stations.
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
    "https://geo.api.gouv.fr/departements?fields=nom,code,contour&format=geojson&geometry=contour",
    "https://geo.api.gouv.fr/departements?fields=nom,code&format=geojson&geometry=contour",
    "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-version-simplifiee.geojson")
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
if (is.null(deps)) {
  msg("Contour des departements indisponible : enveloppe des stations (+ 25 km) a la place.")
  masque <- sf::st_buffer(sf::st_convex_hull(sf::st_union(sf::st_geometry(pts))), 25000)
  deps <- NULL
} else {
  masque <- sf::st_union(sf::st_geometry(deps))
}

# -- Grille de prediction -----------------------------------------------------------
grille <- sf::st_make_grid(masque, cellsize = RESOLUTION_M, what = "centers")
grille <- grille[sf::st_intersects(grille, masque, sparse = FALSE)[, 1]]

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
  url <- sprintf("https://s3.amazonaws.com/elevation-tiles-prod/terrarium/%d/%d/%d.png", z, x, y)
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

mnt_brut <- function() {
  if (nzchar(MNT_FICHIER)) {
    if (!file.exists(MNT_FICHIER)) stop("fichier introuvable : ", MNT_FICHIER, call. = FALSE)
    msg("MNT : fichier %s", MNT_FICHIER)
    return(terra::rast(MNT_FICHIER)[[1L]])
  }
  z <- MNT_ZOOM; n <- 2^z
  bb <- unname(sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(masque)), 4326)))   # lon/lat
  colonne <- function(lon) floor((lon + 180) / 360 * n)
  ligne   <- function(lat) floor((1 - asinh(tan(lat * pi / 180)) / pi) / 2 * n)
  xs <- colonne(bb[1]):colonne(bb[3])
  ys <- ligne(bb[4]):ligne(bb[2])
  total <- length(xs) * length(ys)
  msg("MNT : %d tuiles d'altitude (zoom %d).", total, z)
  demi <- 20037508.342789244; pas <- 2 * demi / n
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

# Covariables derivees du MNT, sur des rasters grossiers (Lambert-93) ; valeurs lues par interpolation.
#  expo_n_<e>, expo_e_<e> : pente (%) orientee vers le nord / vers l'est (la pente "regarde" dans la direction du
#                           versant), calculee sur un MNT moyenne a <e> km, pour chaque echelle de EXPO_ECHELLES_KM
#  littoral       : exp(-distance a la mer / MER_ECHELLE_KM) ; dist_mer_km : distance a la mer (km)
couches_covariables <- function(brut) {
  bb <- unname(sf::st_bbox(masque)); marge <- 100000
  modele_r <- function(res) terra::rast(xmin = bb[1] - marge, xmax = bb[3] + marge, ymin = bb[2] - marge,
                                        ymax = bb[4] + marge, resolution = res, crs = "EPSG:2154")
  sortie <- list()
  if ("expo" %in% COVARIABLES) {
    sec <- terra::ifel(brut < 0, 0, brut)
    for (e in EXPO_ECHELLES_KM) {
      dem   <- terra::project(sec, modele_r(e * 1000), method = "average")
      pente <- terra::terrain(dem, "slope", unit = "radians")
      asp   <- terra::terrain(dem, "aspect", unit = "radians")
      sortie[[paste0("expo_n_", cle_ech(e))]] <- terra::cover(100 * tan(pente) * cos(asp), pente * 0)    # replat : 0
      sortie[[paste0("expo_e_", cle_ech(e))]] <- terra::cover(100 * tan(pente) * sin(asp), pente * 0)
    }
  }
  if ("mer" %in% COVARIABLES) {
    mini <- terra::project(brut, modele_r(2000), method = "min")
    mer  <- terra::ifel(mini <= 0, 1, NA)
    pa   <- terra::patches(mer, directions = 8)
    fr   <- terra::freq(pa)
    grands <- fr$value[fr$count >= 500]                      # >= 2000 km2 : la mer, pas une depression a l'interieur des terres
    if (!length(grands)) {
      msg("Covariable mer : aucune etendue de mer dans le MNT.")
    } else {
      d <- terra::distance(terra::subst(pa, from = grands, to = 1, others = NA)) / 1000
      sortie$dist_mer_km <- d
      sortie$littoral    <- exp(-d / MER_ECHELLE_KM)
    }
  }
  sortie
}

alt_mailles <- NULL
couches <- list(); cov_mailles <- list()
cov_utilisees <- c(expo = FALSE, mer = FALSE)
ech_retenue <- NA_real_      # echelle (km) de l'exposition si elle est retenue dans la tendance
if (MNT) {
  brut_mnt <- tryCatch(mnt_brut(), error = function(e) { msg("MNT indisponible : %s", conditionMessage(e)); NULL })
  moy <- if (is.null(brut_mnt)) NULL else tryCatch(mnt_moyen(brut_mnt), error = function(e) { msg("MNT inutilisable : %s", conditionMessage(e)); NULL })
  if (!is.null(moy)) {
    alt_mailles  <- valeurs_en(moy, sf::st_coordinates(grille))
    alt_stations <- valeurs_en(moy, xy)
    bon <- is.finite(alt_stations) & is.finite(pts$alt)
    r_ctrl <- if (sum(bon) > 30) stats::cor(alt_stations[bon], pts$alt[bon]) else NA_real_
    msg("MNT : altitude moyenne des mailles de %.0f a %.0f m. Controle aux stations (MNT / altitude declaree) : r = %.3f, ecart median %.0f m.",
        min(alt_mailles, na.rm = TRUE), max(alt_mailles, na.rm = TRUE), r_ctrl,
        stats::median(abs(alt_stations[bon] - pts$alt[bon])))
    if (!is.finite(r_ctrl) || r_ctrl < 0.7) {
      msg("MNT incoherent avec l'altitude des stations : krigeage ordinaire, sans MNT.")
      MNT <- FALSE
    }
  } else {
    msg("Krigeage ordinaire, sans MNT.")
    MNT <- FALSE
  }
}
if (MNT && length(COVARIABLES)) {
  couches <- tryCatch(couches_covariables(brut_mnt), error = function(e) { msg("Covariables indisponibles : %s", conditionMessage(e)); list() })
  if (length(couches)) msg("Covariables calculees : %s.", paste(setdiff(names(couches), "dist_mer_km"), collapse = ", "))
}
if (MNT) {
  sans_alt <- !is.finite(pts$alt)
  if (any(sans_alt)) msg("%d station(s) sans altitude ecartee(s) du krigeage avec MNT.", sum(sans_alt))
  pts <- pts[!sans_alt, ]
  xy  <- sf::st_coordinates(pts)
  pts$alt_km <- pts$alt / 1000
  coord_grille <- sf::st_coordinates(grille)
  for (nm in names(couches)) {
    v <- valeurs_en(couches[[nm]], xy, "bilinear")
    if (any(!is.finite(v))) { msg("%d station(s) hors de la couche %s : valeur neutre.", sum(!is.finite(v)), nm); v[!is.finite(v)] <- 0 }
    pts[[nm]] <- v
    cov_mailles[[nm]] <- valeurs_en(couches[[nm]], coord_grille, "bilinear")
  }
  if (length(couches)) {
    utils::write.csv(sf::st_drop_geometry(pts)[, intersect(c("id", "nom", "alt", grep("^expo_", names(pts), value = TRUE), "dist_mer_km", "littoral", "valeur"), names(pts))],
                     file.path(DOSSIER, "sorties", sprintf("stations_covariables%s_%d.csv", VAR$etiq, ANNEE)), row.names = FALSE, fileEncoding = "UTF-8")
  }
}

# -- Variogramme (des residus de la tendance) et validation croisee -------------------
# Validation : un bloc de stations est retire, predit, puis compare (le variogramme reste
# celui ajuste sur toutes les stations). Memes blocs pour les deux formules (graine fixe).
etendue <- sqrt(diff(range(xy[, 1]))^2 + diff(range(xy[, 2]))^2)
cutoff  <- min(400000, etendue / 3)
nfold   <- if (is.null(NFOLD_CV)) nrow(pts) else min(NFOLD_CV, nrow(pts))

# Blocs de validation croisee : les memes pour toutes les tendances (graine fixe)
set.seed(2026)
blocs <- if (is.null(NFOLD_CV)) seq_len(nrow(pts)) else sample(rep(seq_len(nfold), length.out = nrow(pts)))

# globale = FALSE : krigeage a derive externe (la tendance est reestimee dans chaque voisinage)
# globale = TRUE  : krigeage de regression (tendance estimee une fois sur toutes les stations,
#                   puis krigeage des residus) ; plus stable quand il y a plusieurs covariables
evaluer <- function(formule, etiquette, globale = FALSE) {
  v  <- gstat::variogram(formule, pts, cutoff = cutoff, width = cutoff / 30)
  s2 <- stats::var(stats::residuals(stats::lm(formule, data = sf::st_drop_geometry(pts))))
  essais <- list()
  for (m in c("Exp", "Sph", "Mat")) {
    ini <- gstat::vgm(psill = 0.8 * s2, model = m, range = cutoff / 3, nugget = 0.2 * s2,
                      kappa = if (m == "Mat") 1 else 0.5)
    aj <- tryCatch(suppressWarnings(gstat::fit.variogram(v, ini)), error = function(e) NULL)
    # on refuse les ajustements absurdes (palier ou portee sans rapport avec les donnees)
    if (!is.null(aj) && !isTRUE(attr(aj, "singular")) && all(aj$psill >= 0) && all(aj$range >= 0) &&
        sum(aj$psill) <= 20 * s2 && max(aj$range) <= 10 * cutoff) {
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
  msg("\nVariogramme retenu (%s) : %s | pepite %s, palier partiel %s, portee %.0f km (variance : %s)",
      etiquette, choix, n0(modele$psill[1]), n0(sum(modele$psill[-1])), max(modele$range) / 1000, n0(s2))
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
  if (!all(bon)) msg("  %d station(s) sans prediction en validation croisee, ignoree(s).", sum(!bon))
  rmse  <- sqrt(mean(res[bon]^2)); biais <- mean(-res[bon]); mae <- mean(abs(res[bon]))
  nse   <- 1 - sum(res[bon]^2) / sum((cv$observed[bon] - mean(cv$observed[bon]))^2)
  msg("Validation croisee (%s ; %d stations, %s) : RMSE %s %s | ecart absolu moyen %s %s | biais %s %s | efficacite (Nash) %.2f",
      etiquette, sum(bon), if (nfold == nrow(pts)) "un a un" else sprintf("%d blocs", nfold), n0(rmse), VAR$u_log, n0(mae), VAR$u_log,
      nb(biais), VAR$u_log, nse)
  list(modele = modele, choix = choix, v = v, s2 = s2, cv = cv, rmse = rmse, mae = mae, biais = biais, nse = nse,
       n_na = sum(!bon), globale = globale)
}

ref <- if (MNT) evaluer(valeur ~ 1, "sans altitude") else NULL
tendance_globale <- FALSE
if (!MNT) {
  formule <- valeur ~ 1
  ev <- evaluer(formule, "krigeage ordinaire")
} else {
  # (formule, globale) : "derive locale" = krigeage a derive externe ; "tendance globale" = krigeage de regression
  candidats <- list("altitude (derive locale)" = list(f = valeur ~ alt_km, globale = FALSE),
                    "altitude (tendance globale)" = list(f = valeur ~ alt_km, globale = TRUE))
  # exposition : une paire de covariables (expo_n_<e>, expo_e_<e>) par echelle e ; chaque echelle est testee
  # en tendance globale ET en derive locale (celle-ci peut echouer en plaine, ou l'exposition est presque nulle)
  ech_ok <- EXPO_ECHELLES_KM[vapply(EXPO_ECHELLES_KM, function(e) all(paste0(c("expo_n_", "expo_e_"), cle_ech(e)) %in% names(pts)), TRUE)]
  a_mer  <- "littoral" %in% names(pts)
  f_expo <- function(e, mer = FALSE) stats::reformulate(c("alt_km", paste0(c("expo_n_", "expo_e_"), cle_ech(e)), if (mer) "littoral"),
                                                        response = "valeur")
  for (e in ech_ok) {
    candidats[[sprintf("altitude + exposition %g km (tendance globale)", e)]] <- list(f = f_expo(e), globale = TRUE)
    candidats[[sprintf("altitude + exposition %g km (derive locale)", e)]]    <- list(f = f_expo(e), globale = FALSE)
  }
  if (a_mer) candidats[["altitude + distance a la mer"]] <- list(f = valeur ~ alt_km + littoral, globale = TRUE)
  if (a_mer) for (e in ech_ok) candidats[[sprintf("altitude + exposition %g km + distance a la mer", e)]] <- list(f = f_expo(e, TRUE), globale = TRUE)
  evals <- lapply(names(candidats), function(n) {
    tryCatch(evaluer(candidats[[n]]$f, n, candidats[[n]]$globale),
             error = function(err) { msg("Candidat '%s' ecarte (erreur : %s).", n, conditionMessage(err)); NULL })
  })
  names(evals) <- names(candidats)
  ecartes <- vapply(evals, is.null, TRUE)
  stopifnot(!ecartes[["altitude (derive locale)"]], !ecartes[["altitude (tendance globale)"]])
  candidats <- candidats[!ecartes]; evals <- evals[!ecartes]
  rm_ <- vapply(evals, function(e) e$rmse, 0)
  # candidat valide : (presque) toutes les predictions existent et l'erreur reste du meme ordre que le krigeage ordinaire
  # (une derive locale a plusieurs covariables peut diverger quand les voisinages sont presque colineaires)
  valides <- vapply(evals, function(e) e$n_na <= 0.005 * nrow(pts) && is.finite(e$rmse) && e$rmse < 5 * ref$rmse, TRUE)

  # Diagnostic de l'exposition, echelle par echelle : que reste-t-il dans les residus de la validation croisee de
  # l'altitude seule (derive locale), et que gagnent les formules avec exposition par rapport a l'altitude seule ?
  if (length(ech_ok)) {
    cvref <- evals[["altitude (derive locale)"]]$cv
    rref  <- cvref$observed - cvref$var1.pred
    tab_ech <- do.call(rbind, lapply(ech_ok, function(e) {
      cn <- paste0("expo_n_", cle_ech(e)); ce <- paste0("expo_e_", cle_ech(e))
      d  <- data.frame(res = rref, n = pts[[cn]], e = pts[[ce]], alt_km = pts$alt_km, valeur = pts$valeur)
      ok <- is.finite(d$res)
      r2 <- summary(stats::lm(res ~ n + e, data = d[ok, ]))$r.squared
      tt <- summary(stats::lm(valeur ~ alt_km + n + e, data = d))$coefficients[, 3]
      g  <- sprintf("altitude + exposition %g km (tendance globale)", e); l <- sprintf("altitude + exposition %g km (derive locale)", e)
      data.frame(echelle_km = e,
                 r_residus_expo_n = stats::cor(d$res[ok], d$n[ok]), r_residus_expo_e = stats::cor(d$res[ok], d$e[ok]),
                 R2_residus = r2, t_expo_n = unname(tt["n"]), t_expo_e = unname(tt["e"]),
                 RMSE_globale_altitude = rm_[["altitude (tendance globale)"]],
                 RMSE_globale_expo = if (g %in% names(rm_) && valides[[g]]) rm_[[g]] else NA_real_,
                 RMSE_locale_altitude = rm_[["altitude (derive locale)"]],
                 RMSE_locale_expo = if (l %in% names(rm_) && valides[[l]]) rm_[[l]] else NA_real_)
    }))
    tab_ech$variation_rmse_globale_pct <- 100 * (tab_ech$RMSE_globale_expo / tab_ech$RMSE_globale_altitude - 1)
    tab_ech$variation_rmse_locale_pct  <- 100 * (tab_ech$RMSE_locale_expo / tab_ech$RMSE_locale_altitude - 1)
    msg("\nExposition par echelle (residus = validation croisee de l'altitude seule en derive locale ; t = regression globale) :")
    for (i in seq_len(nrow(tab_ech))) with(tab_ech[i, ], msg(
      "  %4g km : r(residus, expo_n) %+.3f, r(residus, expo_e) %+.3f, R2 des residus %.3f | t(expo_n) %+.1f, t(expo_e) %+.1f | RMSE globale %.1f -> %.1f mm (%+.1f %%), locale %.1f -> %.1f mm (%+.1f %%)",
      echelle_km, r_residus_expo_n, r_residus_expo_e, R2_residus, t_expo_n, t_expo_e,
      RMSE_globale_altitude, RMSE_globale_expo, variation_rmse_globale_pct, RMSE_locale_altitude, RMSE_locale_expo, variation_rmse_locale_pct))
    utils::write.csv(tab_ech, file.path(DOSSIER, "sorties", sprintf("exposition_echelles%s_%d.csv", VAR$etiq, ANNEE)), row.names = FALSE, fileEncoding = "UTF-8")
  }
  meilleur <- names(which.min(ifelse(valides, rm_, Inf)))
  # parcimonie : un gain de moins de 1 % ne justifie pas une covariable de plus (ni une tendance globale
  # plutot que la derive locale)
  if (!(meilleur %in% c("altitude (derive locale)", "altitude (tendance globale)")) &&
      rm_[[meilleur]] > 0.99 * rm_[["altitude (tendance globale)"]]) meilleur <- "altitude (tendance globale)"
  if (rm_[[meilleur]] > 0.99 * rm_[["altitude (derive locale)"]]) meilleur <- "altitude (derive locale)"

  msg("\nComparaison des tendances (validation croisee, memes blocs) :")
  msg("  %-46s RMSE %s %s | ecart absolu %s %s | Nash %.3f", "aucune (krigeage ordinaire)", n1(ref$rmse), VAR$u_log, n1(ref$mae), VAR$u_log, ref$nse)
  for (n in names(evals)) {
    if (valides[[n]]) msg("  %-46s RMSE %s %s | ecart absolu %s %s | Nash %.3f%s", n, n1(evals[[n]]$rmse), VAR$u_log, n1(evals[[n]]$mae), VAR$u_log,
                          evals[[n]]$nse, if (n == meilleur) "  <- retenue" else "")
    else msg("  %-46s instable ou predictions manquantes : ecartee", n)
  }
  # les mesures d'un candidat instable n'ont pas de sens : NA dans le tableau
  mesure <- function(champ) ifelse(valides, vapply(evals, function(e) e[[champ]], 0), NA_real_)
  utils::write.csv(data.frame(tendance = c("aucune (krigeage ordinaire)", names(evals)),
                              RMSE = c(ref$rmse, mesure("rmse")), ecart_absolu = c(ref$mae, mesure("mae")),
                              biais = c(ref$biais, mesure("biais")),
                              Nash = c(ref$nse, mesure("nse")),
                              valide = c(TRUE, valides),
                              retenue = c(FALSE, names(evals) == meilleur), row.names = NULL) |>
                     stats::setNames(c("tendance", paste0(c("RMSE_", "ecart_absolu_", "biais_"), VAR$u_col), "Nash", "valide", "retenue")),
                   file.path(DOSSIER, "sorties", sprintf("validation_tendances%s_%d.csv", VAR$etiq, ANNEE)), row.names = FALSE, fileEncoding = "UTF-8")

  # Poids des covariables dans le meilleur modele multi-covariables en tendance globale (moindres carres, a titre indicatif)
  multi <- names(candidats)[vapply(candidats, function(cd) cd$globale && length(all.vars(cd$f)) > 2L, TRUE)]
  if (length(multi)) {
    pleins <- multi[grepl("exposition", multi) & grepl("la mer", multi)]       # le modele le plus complet si possible
    if (!length(pleins)) pleins <- multi
    plein_nom <- pleins[which.min(rm_[pleins])]
    cf <- summary(stats::lm(candidats[[plein_nom]]$f, data = sf::st_drop_geometry(pts)))
    msg("\nTendance globale de '%s' (R2 = %.2f) :", plein_nom, cf$r.squared)
    unites <- c(alt_km = "mm par km d'altitude", expo_n = "mm par % de pente orientee vers le nord",
                expo_e = "mm par % de pente orientee vers l'est", littoral = "mm entre le littoral (1) et l'interieur (0)")
    for (k in rownames(cf$coefficients)[-1]) msg("  %-11s %+8.1f %s (t = %+.1f)", k, cf$coefficients[k, 1],
                                                 unites[[sub("_[0-9p]+$", "", k)]], cf$coefficients[k, 3])
  }

  formule <- candidats[[meilleur]]$f
  tendance_globale <- candidats[[meilleur]]$globale
  ev <- evals[[meilleur]]
  if (!(valides[[meilleur]] && ev$rmse < ref$rmse)) {    # la tendance doit prouver son utilite
    msg("La tendance n'ameliore pas la validation croisee (RMSE %s contre %s %s) : krigeage ordinaire retenu.", n0(ev$rmse), n0(ref$rmse), VAR$u_log)
    MNT <- FALSE; ev <- ref; formule <- valeur ~ 1; tendance_globale <- FALSE
  } else {
    cov_utilisees <- c(expo = grepl("exposition", meilleur), mer = grepl("la mer", meilleur))
    if (cov_utilisees[["expo"]]) ech_retenue <- as.numeric(sub(".*exposition ([0-9.]+) km.*", "\\1", meilleur))
  }
}
v <- ev$v; s2 <- ev$s2; modele <- ev$modele; choix <- ev$choix
cv <- ev$cv; rmse <- ev$rmse; biais <- ev$biais; nse <- ev$nse
if (MNT) msg("Gain de la tendance retenue : RMSE %s -> %s %s (%+.0f %%), Nash %.2f -> %.2f.",
             n0(ref$rmse), n0(rmse), VAR$u_log, 100 * (rmse / ref$rmse - 1), ref$nse, nse)

# -- Prediction -------------------------------------------------------------------------
# suffixe des fichiers : _mnt (+ _expo, _mer selon la tendance retenue), puis _<n>km si la maille n'est pas celle par defaut (5 km)
SUF <- paste0(if (MNT) "_mnt" else "", if (cov_utilisees[["expo"]]) "_expo" else "", if (cov_utilisees[["mer"]]) "_mer" else "",
              if (RESOLUTION_M != 5000) sprintf("_%gkm", RESOLUTION_M / 1000) else "")
if (MNT) {
  nouv <- sf::st_sf(alt_km = alt_mailles / 1000, geometry = grille)
  for (nm in names(cov_mailles)) nouv[[nm]] <- cov_mailles[[nm]]
  for (nm in setdiff(names(cov_mailles), "dist_mer_km")) nouv[[nm]][!is.finite(nouv[[nm]])] <- 0
  # la tendance n'est jamais extrapolee au-dela de la plage observee aux stations ; exception : l'altitude pour la
  # temperature, dont le gradient est quasi lineaire (sommets au-dessus de la station la plus haute)
  for (nm in setdiff(all.vars(formule)[-1], if (VARIABLE == "temperature") "alt_km")) {
    lo <- min(pts[[nm]]); hi <- max(pts[[nm]]); x <- nouv[[nm]]
    n_hors <- sum(x < lo | x > hi, na.rm = TRUE)
    if (n_hors) msg("%d maille(s) hors de la plage des stations pour %s : valeur ramenee a la plage [%.2f ; %.2f].", n_hors, nm, lo, hi)
    nouv[[nm]] <- pmin(pmax(x, lo), hi)
  }
  nouv <- nouv[is.finite(nouv$alt_km), ]
} else {
  nouv <- sf::st_sf(geometry = grille)
}
msg("\nPrediction sur %d mailles de %g km...", nrow(nouv), RESOLUTION_M / 1000)
if (tendance_globale) {
  # krigeage de regression : tendance (moindres carres) + krigeage des residus
  reg <- stats::lm(formule, data = sf::st_drop_geometry(pts))
  pts$res <- stats::residuals(reg)
  pred <- gstat::krige(res ~ 1, pts, newdata = nouv, model = modele, nmax = NMAX, debug.level = 0)
  pred$var1.pred <- pred$var1.pred + as.numeric(stats::predict(reg, sf::st_drop_geometry(nouv)))
} else {
  pred <- gstat::krige(formule, pts, newdata = nouv, model = modele, nmax = NMAX, debug.level = 0)
}
g <- as.data.frame(sf::st_coordinates(pred))
g$bilan <- pred$var1.pred
g$ecart_type <- sqrt(pmax(pred$var1.var, 0))
if (VARIABLE == "pluie" && any(g$bilan < 0, na.rm = TRUE)) {    # une pluie ne peut pas etre negative
  msg("%d maille(s) avec une pluie negative ramenee(s) a 0.", sum(g$bilan < 0, na.rm = TRUE))
  g$bilan <- pmax(g$bilan, 0)
}
if (any(!is.finite(g$bilan))) {
  msg("%d maille(s) sans prediction, ecartee(s).", sum(!is.finite(g$bilan)))
  g <- g[is.finite(g$bilan), , drop = FALSE]
}
msg("Surface krigee : de %s a %s %s (stations : de %s a %s %s).",
    n0(min(g$bilan)), n0(max(g$bilan)), VAR$u_log, n0(min(pts$valeur)), n0(max(pts$valeur)), VAR$u_log)
# Surface en GeoTIFF (Lambert-93, 2 bandes : la variable et l'ecart-type), pour QGIS
if (requireNamespace("terra", quietly = TRUE)) {
  tryCatch({
    rr <- terra::rast(g[, c("X", "Y", "bilan", "ecart_type")], type = "xyz", crs = "EPSG:2154")
    names(rr) <- c(paste0(VARIABLE, "_", VAR$u_col), paste0("ecart_type_", VAR$u_col))
    terra::writeRaster(rr, file.path(DOSSIER, "sorties", sprintf("%s_jja_%d%s.tif", VARIABLE, ANNEE, SUF)), overwrite = TRUE)
  }, error = function(e) msg("GeoTIFF non ecrit : %s", conditionMessage(e)))
}


# ==============================================================================
# 5. Cartes et diagnostic
# ==============================================================================

# Echelle de couleurs ; les bornes suivent les donnees (quantiles 2 % et 98 %).
#  P - ETP     : le zero (equilibre P = ETP) est toujours en blanc (l'ete, P - ETP est presque partout negatif :
#                une echelle symetrique ecraserait la carte)
#  pluie       : du blanc au bleu fonce, a partir de 0
#  temperature : du bleu au rouge
q <- as.numeric(stats::quantile(c(g$bilan, pts$valeur), c(0.02, 0.98)))
echelle <- switch(VARIABLE,
  p_etp = ggplot2::scale_fill_gradientn(
    colours = c("#543005", "#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571", "#003c30"),
    limits = c(min(-50, floor(q[1] / 50) * 50), max(50, ceiling(q[2] / 50) * 50)), oob = scales::squish, name = "P \u2212 ETP (mm)",
    rescaler = function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) scales::rescale_mid(x, to, from, mid = 0)),
  pluie = ggplot2::scale_fill_gradientn(
    colours = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    limits = c(max(0, floor(q[1] / 50) * 50), ceiling(q[2] / 50) * 50), oob = scales::squish, name = "Pluie (mm)"),
  temperature = ggplot2::scale_fill_gradientn(
    colours = c("#313695", "#4575b4", "#74add1", "#abd9e9", "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"),
    limits = c(floor(q[1]), ceiling(q[2])), oob = scales::squish, name = "Temp\u00e9rature (\u00b0C)"))
couche_dep <- if (!is.null(deps)) ggplot2::geom_sf(data = deps, fill = NA, colour = "grey35", linewidth = 0.12, inherit.aes = FALSE)
pts_df <- data.frame(xy, bilan = pts$valeur)
tit_per <- sprintf("juin \u00e0 ao\u00fbt %d", ANNEE)
liste_fr <- function(x) if (length(x) <= 1L) x else paste(paste(x[-length(x)], collapse = ", "), "et", x[length(x)])
tendance_txt <- liste_fr(c("l'altitude", if (cov_utilisees[["expo"]]) sprintf("l'exposition (%g km)", ech_retenue),
                           if (cov_utilisees[["mer"]]) "la distance \u00e0 la mer"))
lib_methode <- if (!MNT) "krigeage ordinaire" else if (tendance_globale) paste0("krigeage de r\u00e9gression sur ", tendance_txt, " (MNT)") else
  paste0("krigeage \u00e0 d\u00e9rive externe sur ", tendance_txt, " (MNT)")
pied <- sprintf("%d stations M\u00e9t\u00e9o-France ; %s ; %s, variogramme %s.\nValidation crois\u00e9e (%s) : RMSE %s %s, biais %s %s%s.",
                nrow(pts), VAR$source, lib_methode, choix, if (nfold == nrow(pts)) "un \u00e0 un" else sprintf("%d blocs", nfold),
                n0(rmse), VAR$u, nb(biais), VAR$u, if (MNT) sprintf(" ; sans l'altitude : RMSE %s %s", n0(ref$rmse), VAR$u) else "")
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
  ggplot2::labs(title = sprintf("%s, %s", VAR$titre, tit_per),
                subtitle = sprintf("Surface krig\u00e9e%s ; les points sont les stations (m\u00eame \u00e9chelle de couleur)",
                                   if (MNT) paste0(" avec ", tendance_txt) else ""),
                caption = pied) + theme_c
ggplot2::ggsave(file.path(DOSSIER, "sorties", sprintf("carte_%s_jja_%d%s.png", VARIABLE, ANNEE, SUF)), carte,
                width = 9, height = 8.5, dpi = 200, bg = "white")

carte_inc <- ggplot2::ggplot() +
  ggplot2::geom_raster(data = g, ggplot2::aes(X, Y, fill = ecart_type)) +
  couche_dep +
  ggplot2::geom_point(data = pts_df, ggplot2::aes(X, Y), shape = 16, size = 0.5, colour = "grey15") +
  ggplot2::scale_fill_viridis_c(option = "magma", direction = -1, name = paste0("\u00c9cart-type\n(", VAR$u, ")")) +
  ggplot2::coord_sf(crs = 2154, datum = NA) +
  ggplot2::labs(title = "Incertitude du krigeage (\u00e9cart-type de pr\u00e9diction)",
                subtitle = "Faible pr\u00e8s des stations, forte l\u00e0 o\u00f9 le r\u00e9seau est lointain",
                caption = pied) + theme_c
ggplot2::ggsave(file.path(DOSSIER, "sorties", sprintf("incertitude_krigeage%s_%d%s.png", VAR$etiq, ANNEE, SUF)), carte_inc,
                width = 9, height = 8.5, dpi = 200, bg = "white")

# Diagnostic : variogramme (points + modele) et validation croisee (observe / predit)
grDevices::png(file.path(DOSSIER, "sorties", sprintf("diagnostic_krigeage%s_%d%s.png", VAR$etiq, ANNEE, SUF)),
               width = if (MNT) 3000 else 2000, height = 950, res = 200)
op <- graphics::par(mfrow = c(1, if (MNT) 3 else 2), mar = c(4.5, 4.8, 3, 1), cex.main = 0.95)
graphics::plot(v$dist / 1000, v$gamma, pch = 16, col = "grey25", xlab = "Distance (km)",
               ylab = sprintf("Semi-variance (%s\u00b2)", VAR$u), ylim = c(0, max(v$gamma) * 1.1),
               main = sprintf("Variogramme%s (%s)", if (MNT) " des r\u00e9sidus" else "", choix))
vl <- gstat::variogramLine(modele, maxdist = cutoff)
graphics::lines(vl$dist / 1000, vl$gamma, col = "#b2182b", lwd = 2)
graphics::abline(h = s2, lty = 3, col = "grey50")
graphics::plot(cv$var1.pred, cv$observed, pch = 16, cex = 0.5, col = grDevices::rgb(0.1, 0.3, 0.5, 0.45),
               xlab = sprintf("Pr\u00e9dit (validation crois\u00e9e, %s)", VAR$u), ylab = sprintf("Observ\u00e9 (%s)", VAR$u),
               main = if (MNT) sprintf("Validation crois\u00e9e : RMSE %s %s, Nash %.2f\n(sans altitude : RMSE %s %s, Nash %.2f)",
                                       n0(rmse), VAR$u, nse, n0(ref$rmse), VAR$u, ref$nse)
                      else sprintf("Validation crois\u00e9e : RMSE %s %s, Nash %.2f", n0(rmse), VAR$u, nse))
graphics::abline(0, 1, col = "grey30")
if (MNT) {
  tend <- stats::lm(valeur ~ alt_km, data = sf::st_drop_geometry(pts))
  graphics::plot(pts$alt, pts$valeur, pch = 16, cex = 0.5, col = grDevices::rgb(0.1, 0.3, 0.5, 0.45),
                 xlab = "Altitude de la station (m)", ylab = sprintf("%s (%s)", VAR$court, VAR$u),
                 main = sprintf("%s selon l'altitude\n(tendance globale : %s %s par km, R\u00b2 = %.2f)",
                                VAR$court, nb(stats::coef(tend)[2]), VAR$u, summary(tend)$r.squared))
  graphics::abline(a = stats::coef(tend)[1], b = stats::coef(tend)[2] / 1000, col = "#b2182b", lwd = 2)   # km -> m
}
graphics::par(op)
invisible(grDevices::dev.off())

msg("\nTermine. Fichiers dans : %s", normalizePath(file.path(DOSSIER, "sorties")))
