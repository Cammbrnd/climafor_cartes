# ==============================================================================
# carte_format.R — ECRITURE ET RELECTURE DES FICHIERS DE LA CARTE
# ------------------------------------------------------------------------------
# Le format est decrit en tete de src/utils_carte.R (schema CONFIG$carte$schema).
# Ce fichier l'ECRIT (cote precalcul) et sait le RELIRE (pour les tests : un
# format que l'on ne sait pas relire est un format que l'on ne sait pas verifier).
# Le navigateur, lui, le lit dans www/carte/carte.js ; les deux doivent rester
# d'accord, et tests/test_v18.R compare leurs descriptions.
# ==============================================================================

CARTE_MANQUANT <- -32768L

#' Valeurs reelles -> entiers 16 bits (valeur * 10^stock), NA -> -32768.
#' Refuse d'ecrire une valeur qui deborderait : un debordement silencieux
#' donnerait une carte fausse mais plausible.
carte_encoder_valeurs <- function(x, stock, cle = "?") {
  e <- round(x * 10^stock)
  e[!is.finite(e)] <- NA
  if (any(abs(e) > 32000, na.rm = TRUE)) {
    stop(sprintf("Couche %s : valeur hors de la plage d'un entier 16 bits (max %g).",
                 cle, max(abs(x), na.rm = TRUE)), call. = FALSE)
  }
  e <- as.integer(e)
  e[is.na(e)] <- CARTE_MANQUANT
  e
}

carte_decoder_valeurs <- function(e, stock) {
  x <- as.numeric(e) / 10^stock
  x[e == CARTE_MANQUANT] <- NA_real_
  x
}

#' Ecrit une couche : c_<cle>.bin.gz.
#' @param valeurs matrice n_annees x n (NA = absente).
carte_ecrire_couche <- function(dossier, couche, normale, pente, signif, valeurs) {
  n <- length(normale)
  stopifnot(length(pente) == n, length(signif) == n, ncol(valeurs) == n)
  fichier <- file.path(dossier, sprintf("c_%s.bin.gz", couche$cle))
  con <- gzfile(fichier, "wb", compression = 9)
  on.exit(close(con))
  writeBin(as.numeric(normale), con, size = 4L, endian = "little")
  writeBin(as.numeric(pente),   con, size = 4L, endian = "little")
  writeBin(as.raw(as.integer(signif %in% TRUE)), con)
  pad <- (4L - (n %% 4L)) %% 4L
  if (pad) writeBin(raw(pad), con)
  # Une annee apres l'autre (toutes les mailles de l'annee 1, puis de l'annee 2...),
  # comme le lit le navigateur : t() car une matrice R se parcourt par colonnes.
  enc <- carte_encoder_valeurs(as.vector(t(valeurs)), couche$stock, couche$cle)
  writeBin(enc, con, size = 2L, endian = "little")
  invisible(fichier)
}

#' Relit une couche (tests).
carte_lire_couche <- function(fichier, n, n_annees, stock) {
  con <- gzfile(fichier, "rb"); on.exit(close(con))
  normale <- readBin(con, "numeric", n, size = 4L, endian = "little")
  pente   <- readBin(con, "numeric", n, size = 4L, endian = "little")
  signif  <- as.integer(readBin(con, "raw", n))
  pad <- (4L - (n %% 4L)) %% 4L
  if (pad) readBin(con, "raw", pad)
  e <- readBin(con, "integer", n_annees * n, size = 2L, endian = "little")
  reste <- length(readBin(con, "raw", 1L))
  list(normale = normale, pente = pente, signif = signif,
       valeurs = matrix(carte_decoder_valeurs(e, stock), nrow = n_annees, byrow = TRUE),
       taille_exacte = reste == 0L && length(e) == n_annees * n)
}

#' grille.json : une maille = un polygone, id = rang 0..n-1.
carte_ecrire_grille <- function(dossier, lon, lat, cote = 8000) {
  n <- length(lon)
  feats <- vapply(seq_len(n), function(i) {
    r <- carte_carre(lon[i], lat[i], cote)
    sprintf('{"type":"Feature","properties":{"id":%d},"geometry":{"type":"Polygon","coordinates":[[%s]]}}',
            i - 1L, paste(sprintf("[%s,%s]", format(r[, 1], nsmall = 4, trim = TRUE),
                                  format(r[, 2], nsmall = 4, trim = TRUE)), collapse = ","))
  }, "")
  txt <- paste0('{"type":"FeatureCollection","features":[', paste(feats, collapse = ","), "]}")
  f <- file.path(dossier, "grille.json")
  writeLines(txt, f, useBytes = TRUE)
  invisible(f)
}

#' meta.json — pretty-printed court, UTF-8.
carte_ecrire_meta <- function(dossier, meta) {
  f <- file.path(dossier, "meta.json")
  txt <- jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null", na = "null",
                          digits = NA, pretty = TRUE)
  writeLines(enc2utf8(as.character(txt)), f, useBytes = TRUE)
  invisible(f)
}
