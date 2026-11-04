# ==============================================================================
# carte_diagnostic_coord.R — QUE CONTIENT LE FICHIER DES COORDONNEES DE LA GRILLE ?
# ------------------------------------------------------------------------------
# Le premier calcul de la carte s'est arrete sur « mailles hors du territoire attendu »
# (bornes lat 41-52, lon -6-10). Ce script lit le fichier de coordonnees, convertit chaque
# maille en WGS84 avec la fonction de la carte et affiche : les etendues, les mailles qui
# sortent des bornes (avec leur rang), et l'ecart avec les colonnes LAT_DG/LON_DG du
# fichier. Il ne modifie rien. Lancement : workflow « Diagnostic GeoSAS » (etape « coord »).
# ==============================================================================
ci <- function(chemin) {
  cur <- "."
  for (p in strsplit(chemin, "/", fixed = TRUE)[[1]]) {
    ls <- list.files(cur, all.files = TRUE, no.. = TRUE); h <- ls[tolower(ls) == tolower(p)]
    cur <- file.path(cur, if (p %in% h || !length(h)) p else h[1])
  }
  sub("^\\./", "", cur)
}
source(ci("outils/carte_geo.R"), encoding = "UTF-8")
URL <- "https://meteofrance.s3.sbg.io.cloud.ovh.net/data/REF_CC/SIM/coordonnees_grille_safran_lambert-2-etendu.csv"
BORNES <- list(lat = c(41, 52), lon = c(-6, 10))

f <- tempfile(fileext = ".csv")
options(timeout = 300)
utils::download.file(URL, f, mode = "wb", quiet = TRUE, method = "libcurl")
d <- utils::read.csv(f, sep = ";", dec = ",", stringsAsFactors = FALSE)
cat("Colonnes :", paste(names(d), collapse = " | "), "\n")
cat("Lignes   :", nrow(d), "\n")
print(utils::head(d, 5))
x <- as.numeric(d[[1]]); y <- as.numeric(d[[2]])
cat(sprintf("LAMBX (hm) : %g a %g | LAMBY (hm) : %g a %g\n", min(x), max(x), min(y), max(y)))
cat("Entiers :", all(x == round(x)) && all(y == round(y)), "\n")
g <- carte_depuis_l2e(x * 100, y * 100)
cat(sprintf("lon : %.4f a %.4f | lat : %.4f a %.4f\n", min(g$lon), max(g$lon), min(g$lat), max(g$lat)))
hors <- which(g$lat < BORNES$lat[1] | g$lat > BORNES$lat[2] | g$lon < BORNES$lon[1] | g$lon > BORNES$lon[2])
cat(sprintf("Mailles hors bornes (lat %g-%g, lon %g-%g) : %d sur %d\n",
            BORNES$lat[1], BORNES$lat[2], BORNES$lon[1], BORNES$lon[2], length(hors), nrow(d)))
if (length(hors)) {
  h <- data.frame(rang = hors, lambx = x[hors], lamby = y[hors], lon = round(g$lon[hors], 4), lat = round(g$lat[hors], 4))
  print(utils::head(h, 40)); cat("...\n")
  cat(sprintf("Hors bornes : lon %.3f a %.3f | lat %.3f a %.3f\n",
              min(h$lon), max(h$lon), min(h$lat), max(h$lat)))
}
if (ncol(d) >= 4) {
  lat_f <- as.numeric(d[[3]]); lon_f <- as.numeric(d[[4]])
  cat(sprintf("Colonnes 3-4 du fichier : plage %.3f a %.3f et %.3f a %.3f\n",
              min(lat_f), max(lat_f), min(lon_f), max(lon_f)))
  cat(sprintf("Ecart a la conversion R (deg) : lat %.4f a %.4f | lon %.4f a %.4f\n",
              min(lat_f - g$lat), max(lat_f - g$lat), min(lon_f - g$lon), max(lon_f - g$lon)))
}
