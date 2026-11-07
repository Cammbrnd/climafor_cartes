# ==============================================================================
# stations_demo.R — STATIONS FICTIVES (DONNEES DE DEMONSTRATION)
# ------------------------------------------------------------------------------
# Sert a tester le precalcul des stations (stations_precalcul.R) et l'application SANS acces
# a Meteo-France : il fabrique, au FORMAT de mf_lire_fichier(), des series journalieres
# inventees (cycle saisonnier, gradient nord-sud, tendance, bruit deterministe) pour quelques
# stations par departement, dont des cas qui eprouvent les gardes :
#   - un pluviometre (pas de temperature : ni gel ni bilan hydrique) ;
#   - une station arretee depuis longtemps (non publiee) ;
#   - une station trop courte (non publiee) ;
#   - une station dont une annee est tres lacunaire (annee absente, pas comptee comme zero).
#
# CES VALEURS NE SONT PAS DU CLIMAT. Le jeu publie porte la mention « DONNEES FICTIVES ».
# Ne jamais le deposer dans l'adresse publique de l'application.
# ==============================================================================

STATIONS_DEMO_SOURCE <- "DONNEES FICTIVES (jeu de démonstration, pas du climat)"

#' Une station fictive au format de mf_lire_fichier().
#' @param debut,fin dates de la premiere et de la derniere mesure.
#' @param sans_t pluviometre ; @param trous plages de dates sans AUCUNE mesure.
#' @param tendance rechauffement fictif, en degres par decennie.
station_demo <- function(id, nom, lon, lat, alt, debut, fin, sans_t = FALSE,
                         trous = list(), tendance = 0.3, graine = 1L) {
  dates <- seq(as.Date(debut), as.Date(fin), by = "day")
  n <- length(dates)
  doy <- as.integer(format(dates, "%j"))
  an <- as.integer(format(dates, "%Y"))
  set.seed(graine + sum(utf8ToInt(id)))
  # Temperature : cycle annuel, gradient avec la latitude et l'altitude, tendance, bruit.
  tm <- 11.5 - 0.55 * (lat - 46) - 0.0065 * alt + 9 * sin(2 * pi * (doy - 110) / 365) +
    tendance * (an - 1990) / 10 + stats::rnorm(n, 0, 2.2)
  tn <- tm - 4.5; tx <- tm + 5
  rr <- ifelse(stats::runif(n) < 0.32, stats::rexp(n, 1 / 6), 0)
  ff <- pmax(0, 3.2 + stats::rnorm(n, 0, 0.8))
  if (sans_t) { tn[] <- NA; tx[] <- NA; tm[] <- NA }
  vide <- rep(FALSE, n)
  for (tr in trous) vide <- vide | (dates >= as.Date(tr[1]) & dates <= as.Date(tr[2]))
  rr[vide] <- NA; tn[vide] <- NA; tx[vide] <- NA; tm[vide] <- NA; ff[vide] <- NA
  data.frame(id = id, nom = nom, lat = lat, lon = lon, alt = alt, date = dates,
             rr = round(rr, 1), qrr = 1, tn = round(tn, 1), qtn = 1, tx = round(tx, 1), qtx = 1,
             tm = round(tm, 1), qtm = 1, tntxm = round((tn + tx) / 2, 1), qtntxm = 1,
             ffm = round(ff, 1), qffm = 1, stringsAsFactors = FALSE)
}

#' Source fictive : function(dept) -> tableau brut des stations fictives du departement.
#'
#' Cinq stations par departement : une complete et ancienne, une complete plus recente, un
#' pluviometre, une arretee en 2010 (non publiee), une trop courte (non publiee). Le
#' departement fixe la position (decalage deterministe a l'interieur de la France).
#' @param fin derniere date de mesure (par defaut, hier).
stations_demo_source <- function(annees, fin = Sys.Date() - 1L, depts_connus = NULL) {
  force(annees)
  function(dept) {
    k <- match(dept, depts_connus %||% sprintf("%02d", 1:95))
    if (is.na(k)) return(NULL)
    lon <- -3.5 + 11 * ((k * 37) %% 97) / 97
    lat <- 42.6 + 7.6 * ((k * 53) %% 89) / 89
    a0 <- min(annees)
    id <- function(j) sprintf("%02d%03d%03d", k %% 100, 100 + j, 1)
    st <- list(
      station_demo(id(1), sprintf("ANCIENNE %s", dept), lon, lat, 180 + k, sprintf("%d-01-01", a0), fin, graine = k),
      station_demo(id(2), sprintf("RECENTE %s", dept), lon + 0.15, lat + 0.1, 320 + k,
                   sprintf("%d-01-01", max(a0, 1985L)), fin, graine = k + 1L),
      station_demo(id(3), sprintf("PLUVIOMETRE %s", dept), lon - 0.1, lat + 0.05, 240 + k,
                   sprintf("%d-01-01", max(a0, 1975L)), fin, sans_t = TRUE, graine = k + 2L),
      station_demo(id(4), sprintf("ARRETEE %s", dept), lon + 0.05, lat - 0.1, 200 + k,
                   sprintf("%d-01-01", a0), "2010-12-31", graine = k + 3L),
      station_demo(id(5), sprintf("COURTE %s", dept), lon - 0.05, lat - 0.05, 210 + k,
                   "2018-01-01", fin, graine = k + 4L))
    do.call(rbind, st)
  }
}
