# ==============================================================================
# carte_sim.R — SOURCE « sim » : FICHIERS EN MASSE DE METEO-FRANCE (SIM QUOTIDIENNE)
# ------------------------------------------------------------------------------
# Meteo-France publie la reanalyse SAFRAN-ISBA-MODCOU en fichiers annuels
# (QUOT_SIM2_AAAA.csv.gz, ~140 Mo, toutes les mailles) sous Licence Ouverte 2.0
# (data.gouv.fr, jeu « Donnees changement climatique - SIM quotidienne »). C'est la
# source meme de GeoSAS. Elle remplace les ~660 000 requetes « position » par
# ~68 telechargements, sans aucune charge sur GeoSAS, et donne la liste exacte des
# 9 892 mailles (LAMBX/LAMBY, Lambert II etendu, en hectometres).
#
# Les fichiers sont lus ANNEE PAR ANNEE : on ne garde que les mailles du fragment, en
# entiers mis a l'echelle (aucune perte : le fichier a au plus 3 decimales, et on verifie
# a chaque lecture que l'echelle les represente exactement). Le tableau journalier d'une
# maille a ensuite EXACTEMENT la forme que safran_charger() donne a l'application, donc
# les memes fonctions de calcul s'y appliquent.
#
# Verification avant d'utiliser cette source : outils/carte_diagnostic_sim.R compare,
# sur des mailles reelles, le fichier SIM et les valeurs de « position » (identiques
# attendues).
# ==============================================================================

SIM_URL_BASE  <- "https://meteofrance.s3.sbg.io.cloud.ovh.net/data/REF_CC/SIM"
SIM_URL_COORD <- paste0(SIM_URL_BASE, "/coordonnees_grille_safran_lambert-2-etendu.csv")
SIM_LICENCE   <- "Météo-France, SIM quotidienne (SAFRAN-ISBA-MODCOU), Licence Ouverte 2.0"

# Echelle de stockage (valeur x echelle = entier) et bornes de plausibilite, par colonne
# du fichier. Une colonne de CONFIG$variables sans entree ici est refusee : ajouter une
# variable a l'application oblige a decider de son echelle.
SIM_ECHELLES <- c(T = 10, TINF_H = 10, TSUP_H = 10, PRELIQ = 10, PRENEI = 10, ETP = 10,
                  SWI = 1000, HTEURNEIGE = 1000, RESR_NEIGE = 10, FF = 10, HU = 10)
SIM_BORNES <- list(T = c(-60, 60), TINF_H = c(-60, 60), TSUP_H = c(-60, 70),
                   PRELIQ = c(0, 1000), PRENEI = c(0, 1000), ETP = c(0, 30),
                   SWI = c(-1, 3), HTEURNEIGE = c(0, 30), RESR_NEIGE = c(0, 20000),
                   FF = c(0, 100), HU = c(0, 120))

#' Colonnes du fichier SIM utiles a l'application : nom interne -> colonne du fichier
#' (nom de l'API sans son suffixe _Q).
#' @return vecteur nomme (noms = noms internes de CONFIG$variables).
carte_sim_colonnes <- function() {
  api <- vapply(CONFIG$variables, function(v) v$api, "")
  csv <- sub("_Q$", "", api)
  inconnues <- setdiff(csv, names(SIM_ECHELLES))
  if (length(inconnues)) {
    stop(sprintf("Variable(s) sans echelle de stockage SIM : %s", paste(inconnues, collapse = ", ")), call. = FALSE)
  }
  stats::setNames(csv, names(CONFIG$variables))
}

# -- Grille --------------------------------------------------------------------

#' La grille native : une ligne par maille, dans l'ordre du fichier de coordonnees.
#'
#' @param src adresse https (defaut) ou chemin d'un fichier local (tests).
#' @return data.frame(lon, lat, lambx, lamby) ; lambx/lamby en hectometres, lon/lat WGS84
#'   du CENTRE de la maille (arrondis a 6 decimales, ~10 cm).
carte_sim_mailles <- function(src = SIM_URL_COORD) {
  fichier <- src
  if (grepl("^https://", src)) {
    fichier <- tempfile(fileext = ".csv")
    options(timeout = 300)
    ok <- tryCatch(utils::download.file(src, fichier, mode = "wb", quiet = TRUE, method = "libcurl") == 0,
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) stop("Fichier de coordonnees SIM : telechargement impossible.", call. = FALSE)
    on.exit(unlink(fichier), add = TRUE)
  } else if (!file.exists(src)) {
    stop(sprintf("Fichier de coordonnees SIM introuvable : %s", src), call. = FALSE)
  }
  d <- utils::read.csv(fichier, sep = ";", dec = ",", stringsAsFactors = FALSE)
  if (ncol(d) < 2L) stop("Fichier de coordonnees SIM : deux colonnes LAMBX/LAMBY attendues.", call. = FALSE)
  x <- suppressWarnings(as.numeric(d[[1]])); y <- suppressWarnings(as.numeric(d[[2]]))
  if (anyNA(x) || anyNA(y) || any(x != round(x)) || any(y != round(y))) {
    stop("Fichier de coordonnees SIM : LAMBX/LAMBY doivent etre des entiers (hectometres).", call. = FALSE)
  }
  if (anyDuplicated(x * 100000 + y)) stop("Fichier de coordonnees SIM : mailles en double.", call. = FALSE)
  if (length(x) < 100L || length(x) > CONFIG$carte$n_mailles_max) {
    stop(sprintf("Fichier de coordonnees SIM : %d mailles, entre 100 et %d attendues.",
                 length(x), CONFIG$carte$n_mailles_max), call. = FALSE)
  }
  g <- carte_depuis_l2e(x * 100, y * 100)
  m <- data.frame(lon = round(g$lon, 6), lat = round(g$lat, 6),
                  lambx = as.integer(x), lamby = as.integer(y))
  if (any(m$lat < PARTAGE_BORNES$lat[1] | m$lat > PARTAGE_BORNES$lat[2] |
          m$lon < PARTAGE_BORNES$lon[1] | m$lon > PARTAGE_BORNES$lon[2])) {
    stop("Fichier de coordonnees SIM : mailles hors du territoire attendu.", call. = FALSE)
  }
  m
}

# -- Fichiers annuels ----------------------------------------------------------

#' Fournisseur HTTP : telecharge QUOT_SIM2_<annee>.csv.gz et rend son chemin local.
#' Trois essais, controle d'integrite (gzip -t). Le fichier est a supprimer par l'appelant.
carte_sim_fournisseur_http <- function(base = SIM_URL_BASE, essais = 3L) {
  function(annee) {
    annee <- as.integer(annee)
    if (length(annee) != 1L || is.na(annee) || annee < 1950L || annee > 2100L) {
      stop("Annee SIM invalide.", call. = FALSE)
    }
    url <- sprintf("%s/QUOT_SIM2_%d.csv.gz", base, annee)
    f <- tempfile(fileext = ".csv.gz")
    options(timeout = 1800)
    for (k in seq_len(essais)) {
      ok <- tryCatch(utils::download.file(url, f, mode = "wb", quiet = TRUE, method = "libcurl") == 0,
                     error = function(e) FALSE, warning = function(w) FALSE)
      ok <- ok && file.exists(f) && file.size(f) > 1e6 &&
        system2("gzip", c("-t", shQuote(f)), stderr = FALSE) == 0L
      if (ok) return(f)
      unlink(f)
      if (k < essais) Sys.sleep(10 * k)
    }
    stop(sprintf("Fichier SIM %d : telechargement impossible apres %d essais.", annee, essais), call. = FALSE)
  }
}

#' Fournisseur local (tests, ou fichiers deja telecharges) : <dossier>/QUOT_SIM2_<annee>.csv.gz
carte_sim_fournisseur_local <- function(dossier) {
  function(annee) {
    f <- file.path(dossier, sprintf("QUOT_SIM2_%d.csv.gz", as.integer(annee)))
    if (!file.exists(f)) stop(sprintf("Fichier SIM %d introuvable dans %s.", annee, dossier), call. = FALSE)
    f
  }
}

# Valeur -> entier a l'echelle, en REFUSANT toute valeur que l'echelle ne represente pas
# exactement ou qui sort des bornes plausibles (code d'absence du fichier, par exemple).
.sim_encoder <- function(x, col, annee) {
  s <- SIM_ECHELLES[[col]]
  b <- SIM_BORNES[[col]]
  if (any(x < b[1] | x > b[2], na.rm = TRUE)) {
    stop(sprintf("SIM %d, colonne %s : valeur hors de [%s ; %s] (min %s, max %s) - code d'absence ?",
                 annee, col, b[1], b[2], min(x, na.rm = TRUE), max(x, na.rm = TRUE)), call. = FALSE)
  }
  y <- x * s; r <- round(y)
  if (any(abs(y - r) > 1e-6, na.rm = TRUE)) {
    stop(sprintf("SIM %d, colonne %s : plus de decimales que l'echelle x%s ne peut en garder.", annee, col, s),
         call. = FALSE)
  }
  as.integer(r)
}

#' Lit un fichier annuel et garde les mailles demandees.
#'
#' @param fichier .csv.gz du fichier SIM.
#' @param cles cles (lambx * 100000 + lamby) des mailles gardees, dans l'ordre voulu.
#' @return list(dates, vars) : dates (Date, une par jour du fichier) et, par colonne du
#'   fichier, une matrice d'entiers jours x mailles (NA = absent).
carte_sim_lire_annee <- function(fichier, cles) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Le paquet data.table est requis pour lire les fichiers SIM.", call. = FALSE)
  }
  csv <- unname(carte_sim_colonnes())
  d <- data.table::fread(cmd = sprintf("gzip -dc %s", shQuote(fichier)), sep = ";",
                         select = c("LAMBX", "LAMBY", "DATE", csv), showProgress = FALSE,
                         data.table = FALSE)
  annee <- NA_integer_
  m <- match(d$LAMBX * 100000 + d$LAMBY, cles)
  d <- d[!is.na(m), , drop = FALSE]; m <- m[!is.na(m)]
  if (!nrow(d)) stop("Fichier SIM : aucune des mailles demandees.", call. = FALSE)
  jours <- sort(unique(d$DATE))
  annee <- as.integer(substr(as.character(jours[1]), 1, 4))
  dates <- as.Date(as.character(jours), "%Y%m%d")
  if (anyNA(dates)) stop(sprintf("SIM %d : dates illisibles.", annee), call. = FALSE)
  nd <- length(jours); nc <- length(cles)
  pos <- match(d$DATE, jours) + (m - 1L) * nd
  # Une maille qui n'a pas tous ses jours est entierement vide pour l'annee : l'annee n'est
  # alors pas cartographiee pour elle (garde de completude de carte_calculer_maille).
  n_par_maille <- tabulate(m, nc)
  incompletes <- which(n_par_maille != nd)
  vars <- stats::setNames(lapply(csv, function(col) {
    M <- matrix(NA_integer_, nd, nc)
    M[pos] <- .sim_encoder(d[[col]], col, annee)
    if (length(incompletes)) M[, incompletes] <- NA_integer_
    M
  }), csv)
  list(dates = dates, vars = vars, annee = annee, n_incompletes = length(incompletes))
}

#' Source « sim » : charge les annees UNE FOIS (en memoire) puis sert les mailles.
#'
#' @param annees annees a charger (completes).
#' @param mailles data.frame(lon, lat, lambx, lamby) de TOUTES les mailles.
#' @param indices rangs (dans `mailles`) du fragment a calculer.
#' @param fournisseur function(annee) -> chemin d'un .csv.gz (HTTP par defaut).
#' @param supprimer TRUE si le fichier rendu par le fournisseur est a effacer apres lecture.
#' @param parametres_api noms API des variables que la collection expose (l'application ne
#'   lit que celles-la) ; par defaut, la negociation de l'application.
#' @return function(i, lon, lat) -> tableau journalier (format safran_charger) ou NULL.
carte_source_sim <- function(annees, mailles, indices, fournisseur = carte_sim_fournisseur_http(),
                             supprimer = TRUE, parametres_api = safran_parametres_utiles()$api) {
  if (!all(c("lambx", "lamby") %in% names(mailles))) {
    stop("La source sim exige les colonnes lambx et lamby (mailles=sim).", call. = FALSE)
  }
  # PARITE AVEC L'APPLICATION. Le fichier SIM contient des variables que la collection GeoSAS
  # n'expose pas (hauteur de neige, par exemple) : l'application, qui lit GeoSAS, les voit
  # absentes. La carte les traite de la meme facon (colonne vide) plutot que d'afficher ce
  # que l'analyse d'une maille ne montrerait pas.
  exposees <- vapply(CONFIG$variables, function(v) v$api %in% parametres_api, TRUE)
  if (!all(exposees)) {
    cat(sprintf("  Variables non exposees par GeoSAS, laissees vides comme dans l'application : %s\n",
                paste(names(exposees)[!exposees], collapse = ", ")))
  }
  cles <- mailles$lambx[indices] * 100000 + mailles$lamby[indices]
  nom_csv <- carte_sim_colonnes()
  stock <- vector("list", length(annees)); names(stock) <- annees
  debut <- Sys.time()
  for (a in annees) {
    f <- fournisseur(a)
    r <- tryCatch(carte_sim_lire_annee(f, cles), finally = if (supprimer) unlink(f))
    if (r$annee != a) stop(sprintf("Le fichier SIM %d contient l'annee %d.", a, r$annee), call. = FALSE)
    stock[[as.character(a)]] <- r
    cat(sprintf("  SIM %d : %d jours, %d maille(s) incomplete(s) (%.0f s au total)\n", a, length(r$dates),
                r$n_incompletes, as.numeric(difftime(Sys.time(), debut, units = "secs"))))
  }
  dates <- do.call(c, lapply(stock, function(r) r$dates))
  lt <- as.POSIXlt(dates)
  base <- data.frame(date = dates, annee = lt$year + 1900L, mois = lt$mon + 1L,
                     jour = lt$mday, doy = lt$yday + 1L)

  function(i, lon, lat) {
    j <- match(i, indices)
    if (is.na(j)) return(NULL)
    df <- base
    for (nom in names(nom_csv)) {
      col <- nom_csv[[nom]]
      if (!exposees[[nom]]) { df[[nom]] <- NA_real_; next }
      v <- unlist(lapply(stock, function(r) r$vars[[col]][, j]), use.names = FALSE)
      df[[nom]] <- ifelse(is.na(v), NA_real_, v / SIM_ECHELLES[[col]])
    }
    if (all(is.na(df$t_moy)) && all(is.na(df$p_liq))) return(NULL)
    df <- safran_deriver(df)
    # Memes secours que safran_charger() (utils_api.R, etapes 4 et 5).
    if (!"etp" %in% names(df) || all(is.na(df$etp))) {
      df$etp <- etp_hargreaves(df$t_moy, df$t_min, df$t_max, df$doy, lat)
      attr(df, "etp_source") <- "Hargreaves-Samani (ETP_Q indisponible)"
    } else {
      attr(df, "etp_source") <- "SAFRAN ETP_Q (Penman-Monteith)"
    }
    if (all(is.na(df$t_min)) && !all(is.na(df$t_moy)) && !all(is.na(df$t_max))) {
      df$t_min <- 2 * df$t_moy - df$t_max
    }
    attr(df, "source") <- "sim"
    attr(df, "point") <- c(lon = lon, lat = lat)
    df
  }
}
