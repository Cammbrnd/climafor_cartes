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

# ------------------------------------------------------------------------------
# Lambert II etendu (EPSG:27572) <-> WGS84 : la grille SAFRAN NATIVE
# ------------------------------------------------------------------------------
# Les fichiers SIM donnent chaque maille par LAMBX/LAMBY, en hectometres, dans ce
# repere. Dessiner les carres directement dedans (au lieu de Lambert-93) rend leurs
# bords exacts. Conique conforme a un parallele (Snyder, 1SP) sur l'ellipsoide
# Clarke 1880 IGN, meridien d'origine = Paris, puis changement de datum NTF -> WGS84
# par translation geocentrique (dX, dY, dZ) = (-168, -60, +320) m : precision de
# l'ordre du metre, sans objet pour des carres de 8 km.
L2E <- list(a = 6378249.2, b = 6356515, lat0 = 46.8, lon0 = 2.33722917,
            k0 = 0.99987742, fe = 600000, fn = 2200000, dx = -168, dy = -60, dz = 320)
WGS84 <- list(a = 6378137, e2 = 0.00669438002290)

.l2e_constantes <- function() {
  p <- L2E; rad <- pi / 180
  e2 <- 1 - p$b^2 / p$a^2; e <- sqrt(e2)
  t <- function(f) tan(pi / 4 - f / 2) / ((1 - e * sin(f)) / (1 + e * sin(f)))^(e / 2)
  f0 <- p$lat0 * rad; n <- sin(f0)
  m0 <- cos(f0) / sqrt(1 - e2 * sin(f0)^2)
  F <- m0 / (n * t(f0)^n)
  list(e = e, e2 = e2, n = n, F = F, rho0 = p$a * p$k0 * F * t(f0)^n, t = t, rad = rad)
}

# Geodesique <-> geocentrique (h = 0), pour le changement de datum.
.geo_vers_xyz <- function(lon, lat, a, e2) {
  rad <- pi / 180; N <- a / sqrt(1 - e2 * sin(lat * rad)^2)
  list(x = N * cos(lat * rad) * cos(lon * rad), y = N * cos(lat * rad) * sin(lon * rad),
       z = N * (1 - e2) * sin(lat * rad))
}
.xyz_vers_geo <- function(x, y, z, a, e2) {
  p <- sqrt(x^2 + y^2); lat <- atan2(z, p * (1 - e2))
  for (i in 1:8) { N <- a / sqrt(1 - e2 * sin(lat)^2); lat <- atan2(z + e2 * N * sin(lat), p) }
  list(lon = atan2(y, x) * 180 / pi, lat = lat * 180 / pi)
}

#' Lambert II etendu (metres) -> longitude/latitude WGS84 (degres).
#' @return list(lon, lat)
carte_depuis_l2e <- function(x, y) {
  k <- .l2e_constantes(); p <- L2E
  dx <- x - p$fe; dy <- k$rho0 - (y - p$fn)
  rho <- sign(k$n) * sqrt(dx^2 + dy^2)
  t <- (rho / (p$a * p$k0 * k$F))^(1 / k$n)
  lon_ntf <- atan2(dx, dy) / k$n / k$rad + p$lon0        # Greenwich
  f <- pi / 2 - 2 * atan(t)
  for (i in 1:12) f <- pi / 2 - 2 * atan(t * ((1 - k$e * sin(f)) / (1 + k$e * sin(f)))^(k$e / 2))
  g <- .geo_vers_xyz(lon_ntf, f / k$rad, p$a, k$e2)
  .xyz_vers_geo(g$x + p$dx, g$y + p$dy, g$z + p$dz, WGS84$a, WGS84$e2)
}

#' Longitude/latitude WGS84 (degres) -> Lambert II etendu (metres).
#' @return list(x, y)
carte_vers_l2e <- function(lon, lat) {
  k <- .l2e_constantes(); p <- L2E
  g <- .geo_vers_xyz(lon, lat, WGS84$a, WGS84$e2)
  n <- .xyz_vers_geo(g$x - p$dx, g$y - p$dy, g$z - p$dz, p$a, k$e2)
  rho <- p$a * p$k0 * k$F * k$t(n$lat * k$rad)^k$n
  th <- k$n * (n$lon - p$lon0) * k$rad
  list(x = p$fe + rho * sin(th), y = p$fn + k$rho0 - rho * cos(th))
}

#' Anneau (5 points, ferme) de la maille native de centre (x, y) en Lambert II etendu.
#' @return matrice 5 x 2 (lon, lat), arrondie a 4 decimales (~ 8 m).
carte_carre_l2e <- function(x, y, cote = 8000) {
  d <- cote / 2
  g <- carte_depuis_l2e(x + c(-d, d, d, -d, -d), y + c(-d, -d, d, d, -d))
  cbind(round(g$lon, 4), round(g$lat, 4))
}
