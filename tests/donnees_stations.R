# ==============================================================================
# tests/donnees_stations.R — FICHIERS METEO-FRANCE SYNTHETIQUES
# ------------------------------------------------------------------------------
# Fabrique des fichiers au FORMAT des « donnees climatologiques de base
# quotidiennes » de Meteo-France (csv.gz, separateur « ; », en-tete NUM_POSTE,
# NOM_USUEL, LAT, LON, ALTI, AAAAMMJJ, RR, QRR, TN, QTN...), pour tester la
# lecture, la validation annuelle et la qualite SANS ACCES RESEAU.
#
# Ces fichiers reproduisent le FORMAT, pas la meteo : les valeurs sont
# deterministes (sinusoide + pluie un jour sur trois) pour que les resultats se
# calculent a la main. Le format lui-meme est celui dont je disposais au moment
# de l'ecriture ; diagnostic_stations.R le confronte aux vrais fichiers.
# ==============================================================================

MF_ENTETE_TEST <- paste(
  "NUM_POSTE", "NOM_USUEL", "LAT", "LON", "ALTI", "AAAAMMJJ",
  "RR", "QRR", "TN", "QTN", "HTN", "QHTN", "TX", "QTX", "HTX", "QHTX",
  "TM", "QTM", "TNTXM", "QTNTXM", "TAMPLI", "QTAMPLI", "TNSOL", "QTNSOL",
  "TN50", "QTN50", "DG", "QDG", "FFM", "QFFM", "FF2M", "QFF2M", "FXY", "QFXY",
  "DXY", "QDXY", "HXY", "QHXY", "FXI", "QFXI", "DXI", "QDXI", "HXI", "QHXI",
  "DRR", "QDRR", sep = ";")

#' Une station synthetique.
#'
#' @param trous liste de plages de dates (c(debut, fin)) sans pluie renseignee.
#' @param sans_t TRUE pour un pluviometre (pas de temperature).
#' @param q_rr code qualite attribue a toutes les pluies (1 = valide).
station_test <- function(id, nom, lat, lon, alt, debut, fin,
                         trous_rr = list(), sans_t = FALSE, q_rr = 1,
                         t_depuis = NULL) {
  dates <- seq(as.Date(debut), as.Date(fin), by = "day")
  doy <- as.integer(format(dates, "%j"))
  tm <- 11 + 9 * sin(2 * pi * (doy - 110) / 365)
  tn <- tm - 4; tx <- tm + 5
  rr <- ifelse(doy %% 3 == 0, 2, 0)
  ff <- rep(3.2, length(dates))
  for (tr in trous_rr) rr[dates >= as.Date(tr[1]) & dates <= as.Date(tr[2])] <- NA
  if (sans_t) { tn[] <- NA; tx[] <- NA; tm[] <- NA }
  if (!is.null(t_depuis)) { sel <- dates < as.Date(t_depuis); tn[sel] <- NA; tx[sel] <- NA; tm[sel] <- NA }
  data.frame(id = id, nom = nom, lat = lat, lon = lon, alt = alt, date = dates,
             rr = rr, tn = tn, tx = tx, tm = tm, tntxm = (tn + tx) / 2, ffm = ff,
             q_rr = q_rr, stringsAsFactors = FALSE)
}

#' Ecrit des stations dans un fichier csv.gz au format Meteo-France.
ecrire_fichier_mf <- function(stations, chemin, latin1 = FALSE) {
  f <- function(x, d = 1) ifelse(is.na(x), "", formatC(x, format = "f", digits = d))
  morceaux <- lapply(stations, function(s) {
    qv <- function(v, q = 1) ifelse(is.na(v), "", as.character(q))
    paste(
      s$id, s$nom, formatC(s$lat, format = "f", digits = 6), formatC(s$lon, format = "f", digits = 6),
      s$alt, format(s$date, "%Y%m%d"),
      f(s$rr), qv(s$rr, s$q_rr), f(s$tn), qv(s$tn), "", "", f(s$tx), qv(s$tx), "", "",
      f(s$tm), qv(s$tm), f(s$tntxm), qv(s$tntxm), "", "", "", "", "", "", "", "",
      f(s$ffm), qv(s$ffm), "", "", "", "", "", "", "", "", "", "", "", "", "", "",
      "", "", sep = ";")
  })
  lignes <- c(MF_ENTETE_TEST, unlist(morceaux))
  con <- gzfile(chemin, "wt", encoding = if (latin1) "latin1" else "UTF-8")
  writeLines(lignes, con)
  close(con)
  invisible(chemin)
}

#' Jeu complet : un departement « 31 » fictif, fichiers previous et latest.
#'
#' - 31001001 AUTERIVE : pluie + temperatures + vent, 1990-2026 ; un trou de
#'   pluie de 40 jours en 2000 (l'annee 2000 doit etre ECARTEE pour la pluie,
#'   conservee pour la temperature) ;
#' - 31002002 PLUVIO-SEUL : pluie seule ;
#' - 31003003 ARRETEE : derniere mesure il y a plus d'un an ;
#' - 31004004 RECENTE : temperatures seulement depuis 2005 (pluie depuis 1990).
jeu_stations_test <- function(dossier = tempfile("mf_test_")) {
  dir.create(dossier, showWarnings = FALSE, recursive = TRUE)
  veille <- Sys.Date() - 1
  an <- as.integer(format(veille, "%Y"))

  def <- list(
    list(id = "31001001", nom = "AUTERIVE", lat = 43.35, lon = 1.47, alt = 150,
         debut = "1990-01-01", trous_rr = list(c("2000-03-01", "2000-04-09"))),
    list(id = "31002002", nom = "PLUVIO-SEUL", lat = 43.30, lon = 1.50, alt = 160,
         debut = "1995-01-01", sans_t = TRUE),
    list(id = "31003003", nom = "ARRETEE", lat = 43.40, lon = 1.40, alt = 140,
         debut = "1990-01-01", fin_force = "2025-03-01"),
    list(id = "31004004", nom = "RECENTE", lat = 43.50, lon = 1.60, alt = 400,
         debut = "1990-01-01", t_depuis = "2005-01-01"))

  prev <- list(); lat <- list()
  for (d in def) {
    fin_tot <- if (!is.null(d$fin_force)) as.Date(d$fin_force) else veille
    mk <- function(a, b) {
      if (as.Date(b) < as.Date(a)) return(NULL)
      do.call(station_test, c(list(id = d$id, nom = d$nom, lat = d$lat, lon = d$lon,
                                   alt = d$alt, debut = a, fin = b),
                              d[intersect(names(d), c("trous_rr", "sans_t", "t_depuis"))]))
    }
    p <- mk(d$debut, min(fin_tot, as.Date(sprintf("%d-12-31", an - 2))))
    l <- mk(sprintf("%d-01-01", an - 1), fin_tot)
    if (!is.null(p)) prev[[length(prev) + 1L]] <- p
    if (!is.null(l)) lat[[length(lat) + 1L]] <- l
  }

  f_prev <- file.path(dossier, sprintf("Q_31_previous-1950-%d_RR-T-Vent.csv.gz", an - 2))
  f_lat  <- file.path(dossier, sprintf("Q_31_latest-%d-%d_RR-T-Vent.csv.gz", an - 1, an))
  ecrire_fichier_mf(prev, f_prev)
  ecrire_fichier_mf(lat, f_lat)
  list(dossier = dossier, previous = f_prev, latest = f_lat, an = an, veille = veille)
}
