# ==============================================================================
# carte_demo.R — JEU DE DEMONSTRATION (DONNEES FICTIVES)
# ------------------------------------------------------------------------------
# Sert a tester l'onglet Carte et le precalcul SANS acces au service SAFRAN : il
# fabrique des series journalieres inventees (cycle saisonnier, gradient nord-sud,
# tendance, bruit) pour des mailles reparties sur un contour grossier de la France.
#
# CES VALEURS NE SONT PAS DU CLIMAT. Le jeu publie porte la mention
# « DONNEES FICTIVES » dans meta.json (champ source), que l'onglet affiche. Ne
# jamais deposer ce jeu dans donnees_carte/ d'une application deployee.
# ==============================================================================

CARTE_DEMO_SOURCE <- "DONNEES FICTIVES (jeu de démonstration, pas du climat)"

# Contour grossier (lon, lat) : seul compte le nombre de mailles, pas leur position.
.demo_contour <- list(
  continent = rbind(
    c(-4.8, 48.4), c(-3.0, 48.9), c(-1.6, 49.7), c(1.5, 50.2), c(2.5, 51.1),
    c(4.2, 50.0), c(5.9, 49.5), c(8.2, 49.0), c(7.6, 47.6), c(6.9, 47.0),
    c(6.1, 46.2), c(7.0, 45.6), c(7.1, 44.2), c(7.6, 43.8), c(6.5, 43.1),
    c(4.5, 43.3), c(3.1, 42.4), c(1.7, 42.4), c(-0.3, 42.8), c(-1.8, 43.4),
    c(-1.3, 45.0), c(-1.2, 46.2), c(-2.4, 47.2), c(-4.3, 47.8)),
  corse = rbind(c(8.55, 41.4), c(9.1, 41.45), c(9.55, 42.0), c(9.35, 43.0),
                c(8.65, 42.6), c(8.55, 41.9)))

# Point dans un polygone (lancer de rayon), vectorise sur les points.
.demo_dans <- function(lon, lat, poly) {
  n <- nrow(poly); dedans <- rep(FALSE, length(lon)); j <- n
  for (i in seq_len(n)) {
    xi <- poly[i, 1]; yi <- poly[i, 2]; xj <- poly[j, 1]; yj <- poly[j, 2]
    coupe <- ((yi > lat) != (yj > lat)) & (lon < (xj - xi) * (lat - yi) / (yj - yi) + xi)
    dedans <- xor(dedans, coupe); j <- i
  }
  dedans
}

#' Centres de mailles fictives sur un pas de `pas_km` (8 = grille SAFRAN).
carte_demo_mailles <- function(pas_km = 32) {
  pas <- pas_km * 1000
  xs <- seq(90000 + pas / 2, 1270000, by = pas)
  ys <- seq(6040000 + pas / 2, 7130000, by = pas)
  g <- expand.grid(x = xs, y = ys)
  ll <- carte_depuis_l93(g$x, g$y)
  ok <- .demo_dans(ll$lon, ll$lat, .demo_contour$continent) |
        .demo_dans(ll$lon, ll$lat, .demo_contour$corse)
  data.frame(lon = round(ll$lon[ok], 4), lat = round(ll$lat[ok], 4))
}

# Squelette des dates, commun a toutes les mailles.
.demo_calendrier <- function(annees) {
  dates <- seq(as.Date(sprintf("%d-01-01", min(annees))),
               as.Date(sprintf("%d-12-31", max(annees))), by = "day")
  data.frame(date = dates, annee = as.integer(format(dates, "%Y")),
             mois = as.integer(format(dates, "%m")), jour = as.integer(format(dates, "%d")),
             doy = as.integer(format(dates, "%j")))
}

#' Fabrique la fonction lire_maille() du jeu fictif.
#'
#' La serie de chaque maille est deterministe (graine = rang de la maille) : deux
#' executions donnent le meme jeu.
carte_demo_source <- function(annees) {
  cal <- .demo_calendrier(annees)
  function(i, lon, lat) {
    set.seed(10000L + i)
    n <- nrow(cal); a <- cal$annee - min(annees); doy <- cal$doy
    # Temperature : plus chaude au sud, plus fraiche en altitude fictive, cycle
    # saisonnier, tendance de 0,03 degC/an, bruit journalier correle.
    alt <- 600 * pmax(0, sin(lon * 1.7) * cos(lat * 2.3))
    t0 <- 12.5 - 0.55 * (lat - 46) - 0.0055 * alt
    bruit <- as.numeric(stats::filter(stats::rnorm(n, 0, 2.2), 0.6, "recursive"))
    t_moy <- t0 + 8.5 * sin((doy - 110) / 365.25 * 2 * pi) + 0.03 * a + bruit
    t_min <- t_moy - 4.5; t_max <- t_moy + 4.5
    # Pluie : plus abondante a l'ouest et en altitude ; neige sous 0 degC.
    pluie_j <- 2.2 + 0.0015 * alt + 0.08 * (3 - lon)
    p <- ifelse(stats::runif(n) < 0.42, stats::rexp(n, 1 / (pluie_j * 2.2)), 0)
    p_sol <- ifelse(t_moy < 0, p, 0); p_liq <- p - p_sol
    etp <- pmax(0, 1.6 + 0.16 * t_moy)
    swi <- pmin(1, pmax(0.05, 0.55 + 0.2 * cos((doy - 20) / 365.25 * 2 * pi) +
                          as.numeric(stats::filter(stats::rnorm(n, 0, 0.03), 0.97, "recursive"))))
    don <- cbind(cal, data.frame(
      t_moy = t_moy, t_min = t_min, t_max = t_max, p_liq = p_liq, p_sol = p_sol,
      etp = etp, swi = swi, hu = pmin(100, pmax(30, 78 - 0.8 * t_moy + stats::rnorm(n, 0, 6))),
      ff = pmax(0.3, 3 + 0.9 * sin((doy - 30) / 365.25 * 2 * pi) + stats::rnorm(n, 0, 0.8))))
    safran_deriver(don)
  }
}
