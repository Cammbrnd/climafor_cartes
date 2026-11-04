# ==============================================================================
# carte_sources.R — D'OU VIENNENT LES SERIES JOURNALIERES DES MAILLES
# ------------------------------------------------------------------------------
# Le precalcul (carte_precalcul.R) ne connait qu'une fonction lire_maille(i, lon, lat)
# qui rend le tableau journalier de la maille au format de safran_charger(), ou NULL.
# Ce fichier fournit les sources reelles ; le jeu fictif est dans carte_demo.R.
#
# SOURCE « position » : une requete par maille et par annee sur le point d'entree
# /position de GeoSAS, via safran_charger() — le MEME chemin que l'application.
# C'est la seule source dont le comportement est connu aujourd'hui. Pour les ~9 900
# mailles et ~67 annees, cela fait ~660 000 requetes : charge a faire accepter par
# le service AVANT un premier calcul complet (voir README, « Avant le premier calcul »).
# Une source par zone (area / cube) ou par fichiers SIM s'ajoutera ici si le
# diagnostic (outils/carte_diagnostic_zone.R) montre que le service les accepte.
# ==============================================================================

#' Source « position » (safran_charger, point par point).
#' @param annees annees a charger.
#' @param pause_s attente entre deux mailles, pour menager le service.
carte_source_position <- function(annees, pause_s = 0.5) {
  function(i, lon, lat) {
    don <- tryCatch(safran_charger(lon, lat, annees), error = function(e) NULL)
    if (pause_s > 0) Sys.sleep(pause_s)
    if (!is.data.frame(don) || !nrow(don)) return(NULL)
    don
  }
}

#' Lit la liste des mailles : un CSV (colonnes lon, lat) ou « demo:<pas_km> ».
#'
#' Le fichier de mailles est une donnee du depot, pas du code : le produire est une
#' etape a part (enumeration de la grille SAFRAN reelle).
#' @return data.frame(lon, lat), une ligne par maille, dans l'ordre du fichier.
carte_lire_mailles <- function(spec) {
  if (grepl("^demo:[0-9.]+$", spec)) return(carte_demo_mailles(as.numeric(sub("^demo:", "", spec))))
  if (!file.exists(spec)) stop(sprintf("Fichier de mailles introuvable : %s", spec), call. = FALSE)
  m <- utils::read.csv(spec, stringsAsFactors = FALSE)
  if (!all(c("lon", "lat") %in% names(m))) stop("Le fichier de mailles doit avoir les colonnes lon et lat.", call. = FALSE)
  m <- m[, c("lon", "lat")]
  if (anyNA(m) || !all(is.finite(m$lon)) || !all(is.finite(m$lat))) stop("Mailles : coordonnees manquantes ou non numeriques.", call. = FALSE)
  if (any(m$lat < PARTAGE_BORNES$lat[1] | m$lat > PARTAGE_BORNES$lat[2] |
          m$lon < PARTAGE_BORNES$lon[1] | m$lon > PARTAGE_BORNES$lon[2])) {
    stop("Mailles : coordonnees hors du territoire.", call. = FALSE)
  }
  if (anyDuplicated(round(m, 3))) stop("Mailles : doublons.", call. = FALSE)
  if (nrow(m) < 1L || nrow(m) > CONFIG$carte$n_mailles_max) {
    stop(sprintf("Mailles : %d, entre 1 et %d attendues.", nrow(m), CONFIG$carte$n_mailles_max), call. = FALSE)
  }
  m
}
