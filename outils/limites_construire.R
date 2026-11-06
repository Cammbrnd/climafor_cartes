# ==============================================================================
# outils/limites_construire.R — CONTOURS DES DEPARTEMENTS ET DES REGIONS
# ------------------------------------------------------------------------------
# Produit donnees_limites/departements.geojson et regions.geojson a partir des
# fichiers « version simplifiee » du depot gregoiredavid/france-geojson (trace :
# IGN Admin Express COG 2018 ; noms et codes : INSEE COG). France metropolitaine
# seulement : la grille SAFRAN n'a pas d'outre-mer.
#
#   Rscript outils/limites_construire.R <departements.geojson> <regions.geojson>
#
# Chaque departement recoit le code de sa region (propriete « region »).
# Les fichiers sont du GeoJSON : lisibles tels quels par n'importe quel langage.
# ==============================================================================
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L, requireNamespace("sf", quietly = TRUE))
dep <- sf::st_read(args[1], quiet = TRUE)
reg <- sf::st_read(args[2], quiet = TRUE)
metro <- function(x) x[!grepl("^97|^98", x$code), c("code", "nom")]
dep <- metro(dep); reg <- metro(reg)
sf::sf_use_s2(FALSE)
pt <- suppressWarnings(sf::st_point_on_surface(dep))
dep$region <- reg$code[vapply(sf::st_intersects(pt, reg), function(i) i[1], 1L)]
stopifnot(!anyNA(dep$region), nrow(dep) == 96L, nrow(reg) == 13L)
dir.create("donnees_limites", showWarnings = FALSE)
for (n in c("dep", "reg")) {
  f <- file.path("donnees_limites", if (n == "dep") "departements.geojson" else "regions.geojson")
  unlink(f)
  sf::st_write(get(n), f, driver = "GeoJSON", quiet = TRUE,
               layer_options = "COORDINATE_PRECISION=4")
}
cat("ok :", nrow(dep), "departements,", nrow(reg), "regions\n")
