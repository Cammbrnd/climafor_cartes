# ==============================================================================
# Comparaison de l'ETP Hargreaves-Samani (calculee aux stations par
# krigeage_p_etp_estival.R) avec l'ETP Penman-Monteith spatialisee de Meteo-France
# (jeu "Donnees ETP spatialisees quotidiennes" de data.gouv.fr, grille 0,125 degre).
#
# Entrees : le CSV des stations ecrit par krigeage_p_etp_estival.R, et le fichier annuel
#           de Meteo-France (telecharge, ou fourni avec KRIGEAGE_ETP_FICHIER).
# Sorties : comparaison_etp_<annee>.csv et comparaison_etp_<annee>.png (dans sorties/).
#
# Le format du fichier de Meteo-France n'est pas decrit sur la page du jeu : les colonnes
# (latitude, longitude, date, ETP) sont reconnues d'apres leur nom, et l'en-tete est
# affiche dans le journal pour verification.
# Source ASCII uniquement.
# ==============================================================================

env_ou <- function(nom, defaut) { v <- Sys.getenv(nom, unset = ""); if (nzchar(v)) v else defaut }
msg <- function(...) cat(sprintf(...), "\n", sep = "")

ANNEE   <- as.integer(env_ou("KRIGEAGE_ANNEE", "2026"))
DOSSIER <- env_ou("KRIGEAGE_DOSSIER", "krigeage_p_etp")
FICHIER <- env_ou("KRIGEAGE_ETP_FICHIER", "")        # fichier de Meteo-France deja telecharge (optionnel)
JEU     <- "donnees-etp-spatialisees-quotidiennes"
stopifnot(is.finite(ANNEE), ANNEE >= 1950L, ANNEE <= 2100L)

for (p in c("httr2")) if (!requireNamespace(p, quietly = TRUE)) stop("Paquet manquant : ", p, call. = FALSE)
dir.create(file.path(DOSSIER, "etp_mf"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DOSSIER, "sorties"), recursive = TRUE, showWarnings = FALSE)

requete <- function(url, query = NULL) {
  r <- httr2::request(url)
  if (length(query)) r <- do.call(httr2::req_url_query, c(list(.req = r), query))
  r <- httr2::req_user_agent(httr2::req_timeout(r, 120), "krigeage-p-etp-estival (R, exploration)")
  httr2::req_retry(r, max_tries = 3)
}

# ---- 1. Stations ----------------------------------------------------------------
f_st <- file.path(DOSSIER, "sorties", sprintf("stations_p_etp_jja_%d.csv", ANNEE))
if (!file.exists(f_st)) stop("CSV des stations introuvable : ", f_st, " (lancer d'abord krigeage_p_etp_estival.R).", call. = FALSE)
st <- utils::read.csv(f_st, stringsAsFactors = FALSE, colClasses = c(id = "character"))
st <- st[st$retenue %in% c(TRUE, "TRUE") & is.finite(st$etp_mm) & is.finite(st$lon) & is.finite(st$lat) & st$n_jours > 0, ]
jours <- seq(as.Date(sprintf("%d-06-01", ANNEE)), as.Date(sprintf("%d-08-31", ANNEE)), by = "day")
st$etp_hs <- st$etp_mm * length(jours) / st$n_jours      # Hargreaves-Samani ramene a 92 jours
msg("%d stations retenues pour la comparaison.", nrow(st))

# ---- 2. Fichier de Meteo-France --------------------------------------------------
trouver_fichier <- function() {
  ressources <- tryCatch({
    j <- httr2::resp_body_json(httr2::req_perform(requete(sprintf("https://www.data.gouv.fr/api/2/datasets/%s/resources/", JEU),
                                                          list(page = 1, page_size = 100, type = "main"))), simplifyVector = FALSE)
    j$data
  }, error = function(e) { msg("API v2 indisponible : %s", conditionMessage(e)); NULL })
  if (!length(ressources)) {
    ressources <- tryCatch(httr2::resp_body_json(httr2::req_perform(requete(sprintf("https://www.data.gouv.fr/api/1/datasets/%s/", JEU))),
                                                 simplifyVector = FALSE)$resources,
                           error = function(e) { msg("API v1 indisponible : %s", conditionMessage(e)); NULL })
  }
  if (!length(ressources)) stop("catalogue du jeu ETP indisponible", call. = FALSE)
  champ <- function(r, k) { v <- r[[k]]; if (is.null(v)) "" else as.character(v)[1] }
  msg("Jeu ETP : %d fichier(s). Premiers :", length(ressources))
  for (r in utils::head(ressources, 12)) msg("  %s | %s | %s", champ(r, "title"), champ(r, "last_modified"), champ(r, "url"))
  texte <- vapply(ressources, function(r) paste(champ(r, "title"), champ(r, "url"), champ(r, "description")), "")
  i <- which(grepl(as.character(ANNEE), texte, fixed = TRUE))
  if (!length(i)) {
    if (ANNEE == as.integer(format(Sys.Date(), "%Y"))) {      # annee en cours : le fichier mis a jour le plus recemment
      dates <- vapply(ressources, function(r) champ(r, "last_modified"), "")
      i <- which.max(dates)
      msg("Aucun fichier ne porte %d dans son nom : fichier le plus recemment mis a jour retenu.", ANNEE)
    } else stop("aucun fichier de ", ANNEE, " dans le jeu", call. = FALSE)
  }
  r <- ressources[[i[1]]]
  url <- champ(r, "latest"); if (!nzchar(url)) url <- champ(r, "url")
  list(url = url, nom = basename(sub("[?].*$", "", champ(r, "url"))), titre = champ(r, "title"))
}

if (nzchar(FICHIER)) {
  if (!file.exists(FICHIER)) stop("fichier introuvable : ", FICHIER, call. = FALSE)
  chemin <- FICHIER
} else {
  r <- trouver_fichier()
  msg("Fichier retenu : %s (%s)", r$titre, r$url)
  chemin <- file.path(DOSSIER, "etp_mf", sprintf("etp_mf_%d.dat", ANNEE))
  if (!(file.exists(chemin) && file.size(chemin) > 0)) {
    tmp <- paste0(chemin, ".part")
    utils::download.file(r$url, tmp, mode = "wb", quiet = TRUE)
    file.rename(tmp, chemin)
  }
}
msg("Fichier : %s (%.1f Mo)", chemin, file.size(chemin) / 1e6)

ouvrir <- function(f) {
  e <- readBin(f, "raw", 2L)
  if (identical(as.integer(e), c(31L, 139L))) gzfile(f, open = "rt") else file(f, open = "rt")
}
con <- ouvrir(chemin)
debut <- readLines(con, n = 4L, warn = FALSE)
close(con)
msg("En-tete et premieres lignes :")
for (l in debut) msg("  %s", substr(l, 1, 200))
compte <- vapply(c(";", ",", "\t"), function(x) nchar(gsub(paste0("[^", x, "]"), "", debut[1])), 0L)
sep <- if (max(compte) == 0L) ";" else c(";", ",", "\t")[which.max(compte)]
msg("Separateur retenu : %s", if (sep == "\t") "tabulation" else sep)

con <- ouvrir(chemin)
mf <- utils::read.table(con, header = TRUE, sep = sep, quote = "\"", comment.char = "", stringsAsFactors = FALSE,
                        check.names = FALSE, fill = TRUE, strip.white = TRUE)
msg("%d lignes, %d colonnes lues.", nrow(mf), ncol(mf))
if (ncol(mf) > 200L) stop("format large (un point de grille par colonne ?) non gere : voir l'en-tete ci-dessus", call. = FALSE)
noms <- toupper(gsub("[^A-Za-z0-9_]", "", names(mf)))
names(mf) <- noms

trouve <- function(motifs) { for (m in motifs) { i <- which(grepl(m, noms)); if (length(i)) return(noms[i[1]]) }; NA_character_ }
c_date <- trouve(c("^(DATE|AAAAMMJJ|JOUR|DAY)$", "DATE"))
c_lat  <- trouve(c("^(LAT|LATITUDE)$", "^LAT"))
c_lon  <- trouve(c("^(LON|LONG|LONGITUDE)$", "^LON"))
c_lx   <- trouve(c("^LAMBX$", "^LAMBERTX$", "^X$"))
c_ly   <- trouve(c("^LAMBY$", "^LAMBERTY$", "^Y$"))
c_etp  <- trouve(c("^ETP", "ETP", "PENMAN"))
msg("Colonnes reconnues : date = %s, lat = %s, lon = %s, X = %s, Y = %s, ETP = %s", c_date, c_lat, c_lon, c_lx, c_ly, c_etp)
if (is.na(c_date) || is.na(c_etp)) stop("colonnes date / ETP non reconnues : voir l'en-tete ci-dessus", call. = FALSE)

d <- as.character(mf[[c_date]])
d <- as.Date(if (all(grepl("^[0-9]{8}$", utils::head(d, 50)))) as.character(d) else substr(d, 1, 10),
             tryFormats = c("%Y%m%d", "%Y-%m-%d", "%d/%m/%Y"))
etp <- suppressWarnings(as.numeric(gsub(",", ".", as.character(mf[[c_etp]]))))
etp[!is.finite(etp) | etp < -50] <- NA_real_
if (!is.na(c_lat) && !is.na(c_lon)) {
  lat <- suppressWarnings(as.numeric(gsub(",", ".", as.character(mf[[c_lat]]))))
  lon <- suppressWarnings(as.numeric(gsub(",", ".", as.character(mf[[c_lon]]))))
} else if (!is.na(c_lx) && !is.na(c_ly)) {
  msg("Coordonnees Lambert II etendu (hectometres) : conversion en longitude / latitude.")
  xy <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(data.frame(x = as.numeric(mf[[c_lx]]) * 100, y = as.numeric(mf[[c_ly]]) * 100),
                                                         coords = c("x", "y"), crs = 27572), 4326))
  lon <- xy[, 1]; lat <- xy[, 2]
} else stop("coordonnees non reconnues : voir l'en-tete ci-dessus", call. = FALSE)

garde <- !is.na(d) & d %in% jours & is.finite(etp) & is.finite(lat) & is.finite(lon)
msg("Lignes de l'ete %d : %d.", ANNEE, sum(garde))
if (sum(garde) < 1000) stop("trop peu de lignes pour l'ete ", ANNEE, " : le fichier ne couvre pas cette periode ?", call. = FALSE)
mf <- data.frame(lon = round(lon[garde], 4), lat = round(lat[garde], 4), etp = etp[garde])
msg("ETP quotidienne de l'ete : mediane %.2f, de %.2f a %.2f mm/jour (a verifier : unite en mm ?).",
    stats::median(mf$etp), min(mf$etp), max(mf$etp))
if (stats::median(mf$etp) < 0.5 || stats::median(mf$etp) > 15) warning("unite inattendue pour l'ETP : verifier les echelles")

cle <- paste(mf$lon, mf$lat)
pts <- data.frame(lon = as.numeric(sub(" .*$", "", names(tapply(mf$etp, cle, length)))),
                  lat = as.numeric(sub("^.* ", "", names(tapply(mf$etp, cle, length)))),
                  n = as.integer(tapply(mf$etp, cle, length)), somme = as.numeric(tapply(mf$etp, cle, sum)))
pts <- pts[pts$n >= 0.95 * length(jours), ]
pts$somme <- pts$somme * length(jours) / pts$n
msg("%d points de grille avec >= 95 %% des jours ; ETP d'ete de %.0f a %.0f mm (mediane %.0f).",
    nrow(pts), min(pts$somme), max(pts$somme), stats::median(pts$somme))

# ---- 3. ETP de Meteo-France a la position de chaque station (ponderation par l'inverse de la distance, 4 voisins) ----
echelle_x <- cos(mean(st$lat) * pi / 180)
etp_mf <- rep(NA_real_, nrow(st)); dist_km <- rep(NA_real_, nrow(st))
for (i in seq_len(nrow(st))) {
  dx <- (pts$lon - st$lon[i]) * echelle_x; dy <- pts$lat - st$lat[i]
  dd <- sqrt(dx^2 + dy^2) * 111.2
  o <- order(dd)[1:4]
  w <- 1 / pmax(dd[o], 0.5)^2
  etp_mf[i] <- sum(w * pts$somme[o]) / sum(w)
  dist_km[i] <- dd[o[1]]
}
st$etp_mf <- etp_mf; st$dist_grille_km <- dist_km
st <- st[is.finite(st$etp_mf) & st$dist_grille_km <= 25, ]
st$ecart <- st$etp_hs - st$etp_mf
st$ecart_pct <- 100 * st$ecart / st$etp_mf
msg("%d stations a moins de 25 km d'un point de grille.", nrow(st))

# ---- 4. Resultats ------------------------------------------------------------------
rm_ <- sqrt(mean(st$ecart^2)); r <- stats::cor(st$etp_hs, st$etp_mf)
reg <- stats::lm(etp_hs ~ etp_mf, data = st)
msg("\nETP d'ete (92 jours), Hargreaves-Samani (stations) contre Penman-Monteith (Meteo-France) :")
msg("  ecart moyen (HS - MF) : %+.0f mm (%+.1f %%) | mediane %+.0f mm | ecart-type %.0f mm | RMSE %.0f mm", mean(st$ecart),
    mean(st$ecart_pct), stats::median(st$ecart), stats::sd(st$ecart), rm_)
msg("  correlation : r = %.3f ; droite HS = %.0f + %.2f x MF", r, stats::coef(reg)[1], stats::coef(reg)[2])
msg("  => avec l'ETP de Meteo-France, le bilan P - ETP serait %s de %.0f mm en moyenne.",
    if (mean(st$ecart) > 0) "plus eleve" else "plus bas", abs(mean(st$ecart)))
classe <- cut(st$alt, c(-1, 200, 500, 1000, 3000), labels = c("< 200 m", "200-500 m", "500-1000 m", "> 1000 m"))
msg("\nPar altitude (ecart moyen HS - MF, mm) :")
for (l in levels(classe)) { i <- classe == l; if (any(i)) msg("  %-11s n = %4d : %+5.0f mm (ecart-type %3.0f)", l, sum(i), mean(st$ecart[i]), stats::sd(st$ecart[i])) }
zone <- cut(st$lat, c(0, 44, 47, 90), labels = c("sud (< 44 N)", "centre (44-47 N)", "nord (> 47 N)"))
msg("Par latitude (ecart moyen HS - MF, mm) :")
for (l in levels(zone)) { i <- zone == l; if (any(i)) msg("  %-17s n = %4d : %+5.0f mm (ecart-type %3.0f)", l, sum(i), mean(st$ecart[i]), stats::sd(st$ecart[i])) }
cr <- stats::cor(st$ecart, st$alt)
msg("Correlation de l'ecart avec l'altitude : %.2f", cr)

utils::write.csv(st[, c("id", "nom", "lon", "lat", "alt", "n_jours", "etp_hs", "etp_mf", "ecart", "ecart_pct", "dist_grille_km")],
                 file.path(DOSSIER, "sorties", sprintf("comparaison_etp_%d.csv", ANNEE)), row.names = FALSE, fileEncoding = "UTF-8")

# ---- 5. Figure ------------------------------------------------------------------------
fig <- file.path(DOSSIER, "sorties", sprintf("comparaison_etp_%d.png", ANNEE))
grDevices::png(fig, width = 3000, height = 1000, res = 200)
op <- graphics::par(mfrow = c(1, 3), mar = c(4.5, 4.8, 3.2, 1), cex.main = 0.95)
lim <- range(c(st$etp_hs, st$etp_mf))
graphics::plot(st$etp_mf, st$etp_hs, pch = 16, cex = 0.5, col = grDevices::rgb(0.1, 0.3, 0.5, 0.4), xlim = lim, ylim = lim,
               xlab = "ETP Penman-Monteith, M\u00e9t\u00e9o-France (mm)", ylab = "ETP Hargreaves-Samani, stations (mm)",
               main = sprintf("ETP d'\u00e9t\u00e9 %d (92 jours)\n\u00e9cart moyen %+.0f mm, r = %.2f", ANNEE, mean(st$ecart), r))
graphics::abline(0, 1, col = "grey30"); graphics::abline(reg, col = "#b2182b", lwd = 2)
graphics::plot(st$alt, st$ecart, pch = 16, cex = 0.5, col = grDevices::rgb(0.1, 0.3, 0.5, 0.4),
               xlab = "Altitude de la station (m)", ylab = "Hargreaves \u2212 M\u00e9t\u00e9o-France (mm)",
               main = "\u00c9cart selon l'altitude")
graphics::abline(h = 0, col = "grey30")
if (nrow(st) > 50) graphics::lines(stats::lowess(st$alt, st$ecart, f = 0.3), col = "#b2182b", lwd = 2)
lim_e <- stats::quantile(abs(st$ecart), 0.95)
pal <- grDevices::colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(41)
col <- pal[pmax(1, pmin(41, round(20.5 + 20 * st$ecart / lim_e)))]
graphics::plot(st$lon, st$lat, pch = 16, cex = 0.55, col = col, asp = 1 / cos(mean(st$lat) * pi / 180), axes = FALSE, xlab = "", ylab = "",
               main = sprintf("\u00c9cart Hargreaves \u2212 M\u00e9t\u00e9o-France (mm)\n(rouge : Hargreaves plus \u00e9lev\u00e9 ; \u00e9chelle \u00b1%.0f mm)", lim_e))
f_dep <- file.path(DOSSIER, "departements_contours.geojson")
if (file.exists(f_dep) && requireNamespace("sf", quietly = TRUE)) {
  dep <- tryCatch(sf::st_geometry(sf::read_sf(f_dep, quiet = TRUE)), error = function(e) NULL)
  if (!is.null(dep)) plot(dep, add = TRUE, border = "grey70", lwd = 0.4)
}
graphics::points(st$lon, st$lat, pch = 16, cex = 0.55, col = col)
graphics::par(op)
grDevices::dev.off()
msg("\nTermine. Figure : %s", fig)
