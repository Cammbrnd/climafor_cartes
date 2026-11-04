# ==============================================================================
# carte_geo.R — GEOMETRIE DES MAILLES (outil du precalcul, hors application)
# ------------------------------------------------------------------------------
# Une maille SAFRAN est un carre de 8 km. On la dessine en le construisant dans
# un repere en METRES (Lambert-93), puis en reprojetant ses quatre coins en
# longitude/latitude : un simple rectangle en degres laisserait des fentes et des
# recouvrements entre mailles voisines.
#
# La projection est ecrite ici (conique conforme de Lambert, 2 paralleles, GRS80)
# pour ne dependre d'aucun paquet cartographique.
#
# LIMITE A CONNAITRE. La grille SAFRAN est definie en Lambert II etendu, pas en
# Lambert-93 : les deux reperes different par une legere rotation (inferieure au
# degre sur la France). Tant que les centres viennent du service, les mailles
# sont exactes au centre et leurs bords approches a quelques dizaines de metres
# pres. Cela ne change aucune valeur, seulement la forme du trace.
# ==============================================================================

L93 <- list(a = 6378137, e2 = 0.00669438002290, lat1 = 44, lat2 = 49,
            lat0 = 46.5, lon0 = 3, fe = 700000, fn = 6600000)

.l93_constantes <- function() {
  p <- L93; rad <- pi / 180; e <- sqrt(p$e2)
  m <- function(f) cos(f) / sqrt(1 - p$e2 * sin(f)^2)
  t <- function(f) tan(pi / 4 - f / 2) / ((1 - e * sin(f)) / (1 + e * sin(f)))^(e / 2)
  f1 <- p$lat1 * rad; f2 <- p$lat2 * rad; f0 <- p$lat0 * rad
  n <- (log(m(f1)) - log(m(f2))) / (log(t(f1)) - log(t(f2)))
  F <- m(f1) / (n * t(f1)^n)
  list(e = e, n = n, F = F, rho0 = p$a * F * t(f0)^n, t = t, rad = rad)
}

#' Longitude/latitude (degres) -> Lambert-93 (metres).
#' @return list(x, y)
carte_vers_l93 <- function(lon, lat) {
  k <- .l93_constantes(); p <- L93
  rho <- p$a * k$F * k$t(lat * k$rad)^k$n
  th <- k$n * (lon - p$lon0) * k$rad
  list(x = p$fe + rho * sin(th), y = p$fn + k$rho0 - rho * cos(th))
}

#' Lambert-93 (metres) -> longitude/latitude (degres).
#' @return list(lon, lat)
carte_depuis_l93 <- function(x, y) {
  k <- .l93_constantes(); p <- L93
  dx <- x - p$fe; dy <- k$rho0 - (y - p$fn)
  rho <- sign(k$n) * sqrt(dx^2 + dy^2)
  t <- (rho / (p$a * k$F))^(1 / k$n)
  lon <- atan2(dx, dy) / k$n / k$rad + p$lon0
  f <- pi / 2 - 2 * atan(t)
  for (i in 1:12) {
    f <- pi / 2 - 2 * atan(t * ((1 - k$e * sin(f)) / (1 + k$e * sin(f)))^(k$e / 2))
  }
  list(lon = lon, lat = f / k$rad)
}

#' Anneau (5 points, ferme) du carre de `cote` metres centre sur (lon, lat).
#' @return matrice 5 x 2 (lon, lat), arrondie a 4 decimales (~ 8 m).
carte_carre <- function(lon, lat, cote = 8000) {
  c0 <- carte_vers_l93(lon, lat); d <- cote / 2
  x <- c0$x + c(-d, d, d, -d, -d); y <- c0$y + c(-d, -d, d, d, -d)
  g <- carte_depuis_l93(x, y)
  cbind(round(g$lon, 4), round(g$lat, 4))
}
