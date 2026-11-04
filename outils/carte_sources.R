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
# SOURCE « sim » (carte_sim.R) : fichiers annuels en masse de Meteo-France, la source
# meme de GeoSAS. Le diagnostic a montre que le cube de GeoSAS interpole (a ecarter) et
# que « position » ne se prete pas a 660 000 requetes ; les fichiers SIM, eux, se lisent
# en quelques minutes et donnent la grille native exacte.
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

#' Lit la liste des mailles : « sim » (grille native de Meteo-France, telechargee),
#' « sim:<fichier> » (le meme fichier de coordonnees, en local), un CSV (colonnes lon, lat,
#' et lambx, lamby si on les a) ou « demo:<pas_km> ».
#'
#' @return data.frame(lon, lat[, lambx, lamby]), une ligne par maille, dans l'ordre du fichier.
carte_lire_mailles <- function(spec) {
  if (grepl("^demo:[0-9.]+$", spec)) return(carte_demo_mailles(as.numeric(sub("^demo:", "", spec))))
  if (identical(spec, "sim")) return(carte_sim_mailles())
  if (grepl("^sim:.+", spec)) return(carte_sim_mailles(sub("^sim:", "", spec)))
  if (!file.exists(spec)) stop(sprintf("Fichier de mailles introuvable : %s", spec), call. = FALSE)
  m <- utils::read.csv(spec, stringsAsFactors = FALSE)
  if (!all(c("lon", "lat") %in% names(m))) stop("Le fichier de mailles doit avoir les colonnes lon et lat.", call. = FALSE)
  m <- m[, intersect(c("lon", "lat", "lambx", "lamby"), names(m)), drop = FALSE]
  if (anyNA(m) || !all(is.finite(m$lon)) || !all(is.finite(m$lat))) stop("Mailles : coordonnees manquantes ou non numeriques.", call. = FALSE)
  if (any(m$lat < PARTAGE_BORNES$lat[1] | m$lat > PARTAGE_BORNES$lat[2] |
          m$lon < PARTAGE_BORNES$lon[1] | m$lon > PARTAGE_BORNES$lon[2])) {
    stop("Mailles : coordonnees hors du territoire.", call. = FALSE)
  }
  if (anyDuplicated(round(m[, c("lon", "lat")], 3))) stop("Mailles : doublons.", call. = FALSE)
  if (nrow(m) < 1L || nrow(m) > CONFIG$carte$n_mailles_max) {
    stop(sprintf("Mailles : %d, entre 1 et %d attendues.", nrow(m), CONFIG$carte$n_mailles_max), call. = FALSE)
  }
  m
}
