# ==============================================================================
# utils_territoires.R — DEPARTEMENTS ET REGIONS (v2.0)
# ------------------------------------------------------------------------------
# Les contours (donnees_limites/*.geojson, voir son LISEZMOI) servent a designer un
# territoire dans le bilan synthetique. Un territoire devient un « lieu » de type
# zone de la comparaison (utils_comparer.R) : sa valeur est la moyenne, a poids
# egaux, des mailles SAFRAN dont le centre est dans le contour.
#
# MOYENNE SIMPLE, PAS PONDEREE PAR LA FORET. Un departement de plaine et de
# montagne a une moyenne qui ne decrit ni l'un ni l'autre ; ponderer par la surface
# forestiere (BD Foret) serait plus juste pour un bilan forestier, mais ajoute une
# donnee. C'est une evolution prevue, annoncee dans la methode du bilan.
# ==============================================================================

.ETAT_TERRITOIRES <- new.env(parent = emptyenv())

TERRITOIRES_FICHIERS <- c(departement = "departements.geojson", region = "regions.geojson")

#' Contours d'un type de territoire (objet sf, WGS84), lus une fois par processus.
#' @param type "departement" ou "region".
#' @keywords internal
territoires_lire <- function(type = c("departement", "region")) {
  type <- match.arg(type)
  if (!is.null(.ETAT_TERRITOIRES[[type]])) return(.ETAT_TERRITOIRES[[type]])
  if (!requireNamespace("sf", quietly = TRUE)) stop("paquet sf absent")
  f <- chemin_ci(file.path(CONFIG$territoires$dossier, TERRITOIRES_FICHIERS[[type]]))
  if (!file.exists(f)) stop(sprintf("contours introuvables : %s", f))
  x <- sf::st_make_valid(sf::st_read(f, quiet = TRUE))
  x <- x[order(x$code), ]
  .ETAT_TERRITOIRES[[type]] <- x
  x
}

#' Les territoires d'un type : code, nom, region (departements), libelle.
#' @return data.frame(code, nom, region, libelle).
#' @keywords internal
territoires_table <- function(type = c("departement", "region")) {
  type <- match.arg(type)
  x <- sf::st_drop_geometry(territoires_lire(type))
  reg <- if ("region" %in% names(x)) as.character(x$region) else rep(NA_character_, nrow(x))
  data.frame(code = as.character(x$code), nom = as.character(x$nom), region = reg,
             libelle = if (type == "departement") sprintf("%s — %s", x$code, x$nom)
                       else as.character(x$nom),
             stringsAsFactors = FALSE)
}

#' Choix pour une liste deroulante : libelle -> code.
#' @keywords internal
territoires_choix <- function(type = c("departement", "region")) {
  t <- territoires_table(match.arg(type))
  stats::setNames(t$code, t$libelle)
}

#' Le lieu (zone) d'un territoire, pret a etre rattache aux mailles.
#' @param codes un ou plusieurs codes ; les codes inconnus sont ignores.
#' @return liste de lieux (cmp_lieu_zone), nommes « Haute-Garonne (31) » ou « Occitanie ».
#' @keywords internal
territoires_lieux <- function(type, codes) {
  x <- territoires_lire(type)
  codes <- unique(as.character(codes))
  codes <- codes[codes %in% x$code]
  lapply(codes, function(cd) {
    i <- match(cd, x$code)
    nom <- if (type == "departement") sprintf("%s (%s)", x$nom[i], x$code[i]) else x$nom[i]
    cmp_lieu_zone(nom, sf::st_geometry(x)[i])
  })
}

#' Departement (code) qui contient un point, ou NA : sert a proposer le departement
#' d'une commune choisie.
#' @keywords internal
territoires_departement_du_point <- function(lon, lat) {
  x <- territoires_lire("departement")
  p <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  i <- suppressMessages(which(lengths(sf::st_intersects(sf::st_geometry(x), p)) > 0))
  if (length(i)) as.character(x$code[i[1]]) else NA_character_
}
