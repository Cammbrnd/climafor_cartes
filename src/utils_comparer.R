# ==============================================================================
# utils_comparer.R — ONGLET COMPARER : IMPORT, AGREGATION, BILAN, EXPORT
# ------------------------------------------------------------------------------
# Comparer des POINTS et des ZONES que l'on apporte soi-meme (placettes d'un
# reseau, forets d'une propriete, massifs, communes...), et en tirer un bilan
# climatique.
#
# D'OU VIENNENT LES CHIFFRES
# --------------------------
# Des mailles PRECALCULEES de la carte (src/carte_couches.R) : pour chaque maille
# SAFRAN de 8 km, la valeur annuelle de chaque indicateur depuis 1959. Aucun
# appel au service SAFRAN : comparer vingt lieux ne coute ni le quota d'analyses
# ni le temps de telecharger vingt series journalieres. La contrepartie est
# dite a l'utilisateur : les valeurs sont celles des mailles, calculees avec les
# seuils par defaut, et une maille lisse le relief.
#
# COMMENT UN LIEU SE RAMENE AUX MAILLES
# -------------------------------------
#   point : la maille dont le centre est le plus proche (a 6 km au plus : au-dela,
#           le point n'est pas dans la grille) ;
#   zone  : toutes les mailles dont le CENTRE est dans la zone ; si aucun centre
#           n'y est (zone plus petite qu'une maille), la maille la plus proche.
# La valeur d'une zone est la MOYENNE de ses mailles, a poids egaux (elles ont
# la meme surface). La normale et la tendance sont calculees sur cette serie
# annuelle, comme l'analyse d'un point le fait sur la sienne.
#
# CE QUE L'ON IMPORTE EST UNE ENTREE NON FIABLE. Taille, nombre de lignes,
# extensions, contenu des archives, noms de lieux (qui ressortent dans les CSV
# exportes) : tout est borne ou assaini ici (CONFIG$comparer).
# ==============================================================================

.ETAT_COMPARER <- new.env(parent = emptyenv())

# Un dossier de cache PAR ADRESSE de donnees : changer CLIMAFOR_CARTE_URL ne doit
# pas relire les fichiers de l'ancienne adresse.
cmp_dossier_cache <- function(base) {
  id <- substr(gsub("[^A-Za-z0-9]+", "_", base), 1L, 80L)
  d <- file.path(tempdir(), "climafor_carte", id)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}


# ------------------------------------------------------------------------------
# 1. Fichiers de la carte, lus par le serveur
# ------------------------------------------------------------------------------

#' Telecharge un fichier publie de la carte.
#'
#' L'adresse de base vient de la configuration, jamais de l'utilisateur. Le
#' telechargement est tout de meme borne : HTTPS, redirections limitees, taille
#' plafonnee, et l'adresse FINALE doit rester sur le meme hote que la base.
#' @param executer fonction(req, path) qui effectue la requete (injectable : les
#'   tests n'ont pas de reseau).
#' @keywords internal
cmp_telecharger <- function(url, dest, base, executer = httr2::req_perform) {
  if (!carte_url_sure(base) || !startsWith(url, base)) {
    stop("adresse de téléchargement refusée")
  }
  req <- httr2::request(url)
  req <- httr2::req_timeout(req, 120)
  req <- httr2::req_user_agent(req, sprintf("%s/%s (%s)", CONFIG$app$nom,
                                            CONFIG$app$version, CONFIG$app$organisme))
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  taille_max <- CONFIG$comparer$taille_carte_max_mo
  req <- do.call(httr2::req_options, c(list(req), mf_options_telechargement(taille_max)))

  tmp <- tempfile(tmpdir = dirname(dest))
  on.exit(unlink(tmp), add = TRUE)
  rep <- executer(req, path = tmp)
  if (httr2::resp_status(rep) >= 400) {
    stop(sprintf("téléchargement refusé (HTTP %d)", httr2::resp_status(rep)))
  }
  hote <- function(u) tolower(as.character(httr2::url_parse(u)$hostname %||% ""))
  if (!identical(hote(httr2::resp_url(rep)), hote(base))) {
    stop("redirection vers un autre hôte refusée")
  }
  taille <- suppressWarnings(file.size(tmp))
  if (!is.finite(taille) || taille <= 0 || taille > taille_max * 1024^2) {
    stop("fichier absent, vide ou trop volumineux")
  }
  if (!file.rename(tmp, dest)) {
    file.copy(tmp, dest, overwrite = TRUE)
  }
  invisible(dest)
}

#' Chemin local d'un fichier de la carte : le dossier embarque, ou la copie
#' telechargee (conservee `CONFIG$comparer$cache_h` heures).
#' @param nom nom de fichier publie (doit passer carte_nom_fichier_sur).
#' @param transport resultat de carte_transport().
#' @param executer voir cmp_telecharger().
#' @keywords internal
cmp_fichier_carte <- function(nom, transport = carte_transport(), maintenant = Sys.time(),
                              executer = httr2::req_perform) {
  if (!carte_nom_fichier_sur(nom)) stop("nom de fichier refusé")
  if (identical(transport$mode, "local")) {
    f <- file.path(transport$dossier, nom)
    if (!file.exists(f)) stop("fichier de la carte introuvable")
    return(f)
  }
  if (!identical(transport$mode, "distant")) {
    stop(paste(c("Données cartographiques indisponibles.", transport$erreurs), collapse = " "))
  }
  f <- file.path(cmp_dossier_cache(transport$base), nom)
  frais <- file.exists(f) &&
    as.numeric(difftime(maintenant, file.mtime(f), units = "hours")) < CONFIG$comparer$cache_h
  if (!frais) cmp_telecharger(paste0(transport$base, nom), f, transport$base, executer)
  f
}

#' Centres des mailles d'une grille.json : moyenne des quatre sommets du carre.
#' @return data.frame(lon, lat), une ligne par maille, dans l'ordre de `id`.
#' @keywords internal
cmp_centres_grille <- function(fichier, n_attendu) {
  g <- jsonlite::fromJSON(fichier, simplifyVector = FALSE)
  feats <- g$features
  if (length(feats) != n_attendu) stop("la grille ne compte pas le nombre de mailles annoncé")
  centre <- function(f) {
    anneau <- f$geometry$coordinates[[1]][1:4]
    c(mean(vapply(anneau, function(p) as.numeric(p[[1]]), 0)),
      mean(vapply(anneau, function(p) as.numeric(p[[2]]), 0)))
  }
  xy <- vapply(feats, centre, numeric(2))
  ids <- vapply(feats, function(f) as.integer(f$properties$id), 1L)
  if (!identical(sort(ids), seq_len(n_attendu) - 1L)) stop("identifiants de mailles incohérents")
  data.frame(lon = xy[1, ][order(ids)], lat = xy[2, ][order(ids)])
}

#' Le jeu de donnees de la carte : description, annees, centres des mailles.
#' Conserve en memoire `CONFIG$comparer$cache_h` heures : meta.json et la grille
#' (1,7 Mo) ne changent qu'a chaque precalcul.
#' @return list(meta, annees, centres, transport).
#' @keywords internal
cmp_jeu <- function(transport = carte_transport(), maintenant = Sys.time(),
                    executer = httr2::req_perform) {
  cle <- transport$base %||% "absent"
  if (identical(transport$mode, "local")) cle <- paste0(cle, normalizePath(transport$dossier))
  c0 <- .ETAT_COMPARER[[cle]]
  if (!is.null(c0) &&
      as.numeric(difftime(maintenant, c0$t, units = "hours")) < CONFIG$comparer$cache_h) {
    return(c0$jeu)
  }
  f_meta <- cmp_fichier_carte("meta.json", transport, maintenant, executer)
  meta <- jsonlite::fromJSON(f_meta, simplifyVector = FALSE)
  v <- carte_meta_valider(meta)
  if (!v$ok) stop(paste("Données cartographiques invalides :", paste(v$erreurs, collapse = " ")))
  centres <- cmp_centres_grille(cmp_fichier_carte(meta$grille, transport, maintenant, executer),
                                as.integer(meta$n_mailles))
  jeu <- list(meta = meta, annees = as.integer(unlist(meta$annees)),
              centres = centres, transport = transport)
  # Les centres en Lambert-93, une fois pour toutes (si sf est la : sinon l'import
  # de lieux est de toute facon impossible).
  if (requireNamespace("sf", quietly = TRUE)) jeu$centres_sf <- cmp_centres_sf(jeu)
  .ETAT_COMPARER[[cle]] <- list(t = maintenant, jeu = jeu)
  jeu
}

#' Couches que le jeu publie propose, dans l'ordre de CARTE_COUCHES, avec leur titre.
#' Un jeu publie avant l'ajout d'une couche ne la porte pas : elle n'est pas offerte.
#' @return vecteur nomme : cle -> titre.
#' @keywords internal
cmp_couches_disponibles <- function(jeu) {
  publiees <- vapply(jeu$meta$couches, function(k) as.character(k$cle)[1], "")
  cles <- intersect(names(CARTE_COUCHES), publiees)
  stats::setNames(vapply(cles, function(k) CARTE_COUCHES[[k]]$titre, ""), cles)
}

#' Relit une couche du jeu : matrice annees x mailles.
#' @keywords internal
cmp_lire_couche <- function(jeu, cle, executer = httr2::req_perform) {
  k <- Filter(function(k) identical(as.character(k$cle)[1], cle), jeu$meta$couches)
  if (!length(k)) stop(sprintf("couche %s absente du jeu publié", cle))
  f <- cmp_fichier_carte(as.character(k[[1]]$fichier), jeu$transport, executer = executer)
  l <- carte_lire_couche(f, as.integer(jeu$meta$n_mailles), length(jeu$annees),
                         as.integer(k[[1]]$stock))
  if (!isTRUE(l$taille_exacte)) stop(sprintf("couche %s : taille de fichier inattendue", cle))
  l
}

#' Centres des mailles en Lambert-93 (points sf), conserves avec le jeu.
#' @keywords internal
cmp_centres_sf <- function(jeu) {
  if (!is.null(jeu$centres_sf)) return(jeu$centres_sf)
  sf::st_transform(sf::st_as_sf(jeu$centres, coords = c("lon", "lat"), crs = 4326), 2154)
}


# ------------------------------------------------------------------------------
# 2. Lieux : points et zones
# ------------------------------------------------------------------------------

#' Nom de lieu assaini : sans caractere de controle, espaces simplifies, 60
#' caracteres au plus. Un nom vide ou absent devient `defaut`.
#' @keywords internal
cmp_nom_propre <- function(x, defaut = "Lieu") {
  x <- iconv(as.character(x)[1], from = "UTF-8", to = "UTF-8", sub = "")
  if (is.na(x)) return(defaut)
  x <- trimws(gsub("[[:space:]]+", " ", gsub("[[:cntrl:]]", " ", x)))
  if (!nzchar(x)) return(defaut)
  if (nchar(x) > 60L) x <- paste0(substr(x, 1L, 59L), "…")
  x
}

#' Texte neutralise pour un CSV : un tableur lit comme une formule toute cellule
#' qui commence par = + - @ ou une tabulation (injection de formule).
#' @keywords internal
cmp_texte_csv_sur <- function(x) {
  x <- as.character(x)
  ifelse(!is.na(x) & grepl("^[=+@\t\r-]", x), paste0("'", x), x)
}

#' Un point, prêt a etre compare.
#' @keywords internal
cmp_lieu_point <- function(nom, lon, lat) {
  list(nom = cmp_nom_propre(nom, "Point"), type = "point", lon = lon, lat = lat)
}

#' Une zone (sfc de un polygone, WGS84), prête a etre comparee.
#' @keywords internal
cmp_lieu_zone <- function(nom, geom) {
  list(nom = cmp_nom_propre(nom, "Zone"), type = "zone", geom = geom)
}

#' Ajoute des lieux a une liste, en rendant les noms uniques et en bornant leur
#' nombre.
#' @return list(lieux, problemes).
#' @keywords internal
cmp_ajouter_lieux <- function(existants, nouveaux) {
  problemes <- character(0)
  lieux <- existants
  for (l in nouveaux) {
    if (length(lieux) >= CONFIG$comparer$lieux_max) {
      problemes <- c(problemes, sprintf(
        "Limite de %d lieux atteinte : « %s » et les suivants n'ont pas été ajoutés.",
        CONFIG$comparer$lieux_max, l$nom))
      break
    }
    noms <- vapply(lieux, function(x) x$nom, "")
    if (l$nom %in% noms) {
      k <- 2L
      while (paste0(l$nom, " (", k, ")") %in% noms) k <- k + 1L
      l$nom <- paste0(l$nom, " (", k, ")")
    }
    lieux[[length(lieux) + 1L]] <- l
  }
  list(lieux = lieux, problemes = problemes)
}

# Noms de colonnes reconnus (sans casse, sans accent).
.CMP_COL_LAT <- c("latitude", "lat", "y", "nord")
.CMP_COL_LON <- c("longitude", "lon", "lng", "long", "x")
.CMP_COL_NOM <- c("nom", "name", "lieu", "libelle", "label", "site", "station", "placette",
                  "foret", "massif", "commune", "id", "code")

cmp_normaliser_nom_colonne <- function(x) {
  x <- tolower(trimws(iconv(x, to = "ASCII//TRANSLIT", sub = "")))
  gsub("[^a-z0-9]", "", x)
}

#' Lit un CSV de points : nom, latitude, longitude (WGS84, degres decimaux).
#'
#' Separateur « ; », « , » ou tabulation, reconnu sur la ligne d'en-tete ; virgule
#' decimale acceptee ; UTF-8 (avec ou sans marque d'ordre) ou Latin-1. Une ligne
#' illisible ou hors de la France metropolitaine est ECARTEE ET NOMMEE : on ne
#' compare pas en silence un point que l'on a perdu.
#' @return list(lieux, problemes).
#' @export
cmp_lire_points_csv <- function(chemin) {
  prob <- character(0)
  octets <- readBin(chemin, "raw", n = min(file.size(chemin), CONFIG$comparer$fichier_max_mo * 1024^2))
  txt <- rawToChar(octets); Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) txt <- iconv(rawToChar(octets), "latin1", "UTF-8", sub = "")
  txt <- sub("^﻿", "", txt)
  lignes <- strsplit(txt, "\r\n|\n|\r")[[1]]
  lignes <- lignes[nzchar(trimws(lignes)) & !grepl("^#", lignes)]
  if (length(lignes) < 2L) {
    return(list(lieux = list(), problemes = "Le fichier ne contient pas de ligne de données."))
  }
  if (length(lignes) - 1L > CONFIG$comparer$lignes_max) {
    prob <- c(prob, sprintf("Plus de %d lignes : seules les %d premières sont lues.",
                            CONFIG$comparer$lignes_max, CONFIG$comparer$lignes_max))
    lignes <- lignes[seq_len(CONFIG$comparer$lignes_max + 1L)]
  }
  entete <- lignes[1]
  seps <- c(";", ",", "\t")
  sep <- seps[which.max(vapply(seps, function(s) lengths(regmatches(entete, gregexpr(s, entete, fixed = TRUE))), 1L))]
  d <- tryCatch(
    utils::read.table(text = lignes, sep = sep, header = TRUE, quote = "\"",
                      comment.char = "", fill = TRUE, check.names = FALSE,
                      colClasses = "character", stringsAsFactors = FALSE,
                      na.strings = character(0)),
    error = function(e) NULL)
  if (is.null(d) || !nrow(d)) {
    return(list(lieux = list(), problemes = "Le fichier n'a pas pu être lu comme un tableau."))
  }
  noms_n <- cmp_normaliser_nom_colonne(names(d))
  col <- function(candidats) { i <- which(noms_n %in% candidats); if (length(i)) i[1] else NA_integer_ }
  i_lat <- col(.CMP_COL_LAT); i_lon <- col(.CMP_COL_LON); i_nom <- col(.CMP_COL_NOM)
  if (is.na(i_lat) || is.na(i_lon)) {
    return(list(lieux = list(), problemes = paste(
      "Colonnes de coordonnées introuvables : le fichier doit porter une colonne",
      "« latitude » et une colonne « longitude » (degrés décimaux, WGS84).")))
  }
  nombre <- function(x) suppressWarnings(as.numeric(gsub(",", ".", trimws(x), fixed = TRUE)))
  lat <- nombre(d[[i_lat]]); lon <- nombre(d[[i_lon]])
  lieux <- list()
  for (i in seq_len(nrow(d))) {
    nom <- if (is.na(i_nom)) sprintf("Point %d", i) else d[[i_nom]][i]
    nom <- cmp_nom_propre(nom, sprintf("Point %d", i))
    if (!is.finite(lat[i]) || !is.finite(lon[i])) {
      prob <- c(prob, sprintf("Ligne %d (%s) : coordonnées illisibles, point écarté.", i + 1L, nom))
      next
    }
    if (lat[i] < PARTAGE_BORNES$lat[1] || lat[i] > PARTAGE_BORNES$lat[2] ||
        lon[i] < PARTAGE_BORNES$lon[1] || lon[i] > PARTAGE_BORNES$lon[2]) {
      prob <- c(prob, sprintf(paste(
        "Ligne %d (%s) : hors de la France métropolitaine (latitude %d à %d, longitude %d à %d),",
        "point écarté. Latitude et longitude sont-elles inversées ?"),
        i + 1L, nom, PARTAGE_BORNES$lat[1], PARTAGE_BORNES$lat[2],
        PARTAGE_BORNES$lon[1], PARTAGE_BORNES$lon[2]))
      next
    }
    lieux[[length(lieux) + 1L]] <- cmp_lieu_point(nom, lon[i], lat[i])
  }
  list(lieux = lieux, problemes = prob)
}

#' Extrait un shapefile zippe dans un dossier neuf. Refuse l'archive qui sort de
#' son dossier (« .. », chemin absolu), qui compte trop de fichiers, qui se
#' decompresse trop (bombe) ou qui ne porte pas exactement un .shp.
#' @return chemin du .shp extrait.
#' @keywords internal
cmp_extraire_zip <- function(zip, dossier = tempfile("shp")) {
  liste <- utils::unzip(zip, list = TRUE)
  noms <- liste$Name
  if (nrow(liste) > CONFIG$comparer$zip_fichiers_max) stop("archive trop riche en fichiers")
  if (sum(as.numeric(liste$Length)) > CONFIG$comparer$zip_taille_max_mo * 1024^2) {
    stop("archive trop volumineuse une fois décompressée")
  }
  if (any(grepl("(^|/)\\.\\.(/|$)|^/|^[A-Za-z]:|\\\\", noms))) stop("archive : chemins refusés")
  utiles <- noms[grepl("\\.(shp|shx|dbf|prj|cpg)$", noms, ignore.case = TRUE)]
  shp <- utiles[grepl("\\.shp$", utiles, ignore.case = TRUE)]
  if (length(shp) != 1L) stop("l'archive doit contenir un seul fichier .shp")
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip, files = utiles, exdir = dossier, junkpaths = TRUE)
  file.path(dossier, basename(shp))
}

#' Lit un fichier de zones (ou de points) geographique : GeoJSON, GeoPackage,
#' shapefile zippe. Demande le paquet sf.
#'
#' Reprojete en WGS84. Un fichier sans systeme de coordonnees n'est PAS devine,
#' sauf un GeoJSON (la norme impose WGS84) : un shapefile sans .prj peut etre en
#' Lambert-93 ou en WGS84, et se tromper place les lieux a des centaines de km.
#' @param chemin fichier ; @param nom_origine nom d'origine (l'extension decide).
#' @return list(lieux, problemes).
#' @export
cmp_lire_zones <- function(chemin, nom_origine = chemin) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(list(lieux = list(), problemes = paste(
      "L'import de zones demande le paquet R « sf », absent de ce serveur. Les points",
      "(CSV) restent utilisables.")))
  }
  ext <- tolower(tools::file_ext(nom_origine))
  prob <- character(0)
  echec <- function(m) list(lieux = list(), problemes = m)

  src <- chemin
  if (identical(ext, "zip")) {
    src <- tryCatch(cmp_extraire_zip(chemin), error = function(e) conditionMessage(e))
    if (!file.exists(src)) return(echec(sprintf("Archive refusée : %s.", src)))
  }
  x <- tryCatch({
    couche <- NULL
    if (identical(ext, "gpkg")) {
      l <- sf::st_layers(src)
      ok <- !vapply(l$geomtype, function(g) all(is.na(g)), logical(1))
      if (!any(ok)) stop("aucune couche géographique")
      couche <- l$name[which(ok)[1]]
      if (sum(ok) > 1L) prob <- c(prob, sprintf(
        "Le GeoPackage compte %d couches géographiques : seule « %s » est lue.", sum(ok), couche))
    }
    # Seuls ces trois pilotes sont essayes : GDAL en connait des dizaines, dont
    # certains ouvrent des bases ou des adresses distantes.
    args <- c(list(src, quiet = TRUE, drivers = c("GeoJSON", "GPKG", "ESRI Shapefile")),
              if (!is.null(couche)) list(layer = couche))
    do.call(sf::st_read, args)
  }, error = function(e) NULL)
  if (is.null(x)) {
    return(echec("Le fichier n'a pas pu être lu comme un fichier géographique (GeoJSON, GeoPackage ou shapefile)."))
  }
  if (!nrow(x)) return(echec("Le fichier ne contient aucune entité."))
  if (nrow(x) > CONFIG$comparer$entites_max) {
    prob <- c(prob, sprintf("Plus de %d entités : seules les %d premières sont lues.",
                            CONFIG$comparer$entites_max, CONFIG$comparer$entites_max))
    x <- x[seq_len(CONFIG$comparer$entites_max), ]
  }
  if (is.na(sf::st_crs(x))) {
    if (ext %in% c("geojson", "json")) {
      sf::st_crs(x) <- 4326
    } else {
      return(echec(paste(
        "Système de coordonnées absent (fichier .prj manquant ?) : l'emplacement des zones",
        "serait deviné. Ajoutez le fichier .prj, ou enregistrez la couche en GeoPackage.")))
    }
  }
  x <- tryCatch({
    x <- sf::st_zm(x, drop = TRUE, what = "ZM")
    x <- x[!sf::st_is_empty(x), ]
    x <- sf::st_make_valid(x)
    sf::st_transform(x, 4326)
  }, error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(echec("Les géométries du fichier sont inutilisables."))

  attributs <- sf::st_drop_geometry(x)
  noms_n <- cmp_normaliser_nom_colonne(names(attributs))
  i_nom <- which(noms_n %in% .CMP_COL_NOM)
  type <- as.character(sf::st_geometry_type(x))
  lieux <- list()
  for (i in seq_len(nrow(x))) {
    brut <- if (length(i_nom)) attributs[[i_nom[1]]][i] else NA
    defaut <- sprintf("%s %d", if (type[i] == "POINT") "Point" else "Zone", i)
    nom <- cmp_nom_propre(if (is.na(brut)) NA else brut, defaut)
    g <- sf::st_geometry(x)[i]
    if (type[i] %in% c("POLYGON", "MULTIPOLYGON")) {
      lieux[[length(lieux) + 1L]] <- cmp_lieu_zone(nom, g)
    } else if (type[i] == "POINT") {
      xy <- as.numeric(sf::st_coordinates(g))[1:2]
      if (xy[2] < PARTAGE_BORNES$lat[1] || xy[2] > PARTAGE_BORNES$lat[2] ||
          xy[1] < PARTAGE_BORNES$lon[1] || xy[1] > PARTAGE_BORNES$lon[2]) {
        prob <- c(prob, sprintf("« %s » : hors de la France métropolitaine, écarté.", nom))
      } else {
        lieux[[length(lieux) + 1L]] <- cmp_lieu_point(nom, xy[1], xy[2])
      }
    } else {
      prob <- c(prob, sprintf(
        "« %s » : géométrie de type %s ignorée (seuls les points et les polygones se comparent).",
        nom, tolower(type[i])))
    }
  }
  list(lieux = lieux, problemes = prob)
}

#' Importe un fichier selon son extension.
#' @param chemin fichier recu ; @param nom_origine son nom d'origine.
#' @return list(lieux, problemes).
#' @export
cmp_importer <- function(chemin, nom_origine) {
  ext <- tolower(tools::file_ext(nom_origine))
  tryCatch({
    if (isTRUE(file.size(chemin) > CONFIG$comparer$fichier_max_mo * 1024^2)) {
      return(list(lieux = list(), problemes = sprintf(
        "Fichier trop volumineux (plus de %g Mo).", CONFIG$comparer$fichier_max_mo)))
    }
    if (ext %in% c("csv", "txt", "tsv")) return(cmp_lire_points_csv(chemin))
    if (ext %in% c("geojson", "json", "gpkg", "zip")) return(cmp_lire_zones(chemin, nom_origine))
    list(lieux = list(), problemes = sprintf(paste(
      "Format « .%s » non reconnu. Formats acceptés : CSV (points), GeoJSON,",
      "GeoPackage, shapefile zippé (.zip)."), ext))
  }, error = function(e) {
    journaliser_erreur("Import de lieux", e)
    list(lieux = list(), problemes = "Le fichier n'a pas pu être importé.")
  })
}

#' Modele de CSV de points, a telecharger et a remplir.
#' @export
cmp_modele_csv <- function() {
  data.frame(nom = c("Forêt de Bouconne", "Massif de l'Aigoual", "Forêt de la Massane"),
             latitude = c(43.65, 44.12, 42.48), longitude = c(1.17, 3.58, 3.03),
             stringsAsFactors = FALSE)
}


# ------------------------------------------------------------------------------
# 3. Lieux -> mailles -> series annuelles
# ------------------------------------------------------------------------------

#' Mailles d'un lieu (voir l'en-tete du fichier).
#' @param jeu resultat de cmp_jeu().
#' @return list(idx = indices de mailles (base 1), note = phrase, vide si rien a dire).
#' @keywords internal
cmp_mailles <- function(lieu, jeu) {
  pts <- cmp_centres_sf(jeu)
  dmax <- CONFIG$comparer$distance_max_m
  hors <- list(idx = integer(0), note = sprintf(
    "hors de la grille SAFRAN (aucune maille à moins de %s km)", fmt_fr(dmax / 1000, 0)))

  if (identical(lieu$type, "point")) {
    g <- sf::st_transform(sf::st_sfc(sf::st_point(c(lieu$lon, lieu$lat)), crs = 4326), 2154)
    d <- as.numeric(sf::st_distance(g, pts))
    i <- which.min(d)
    if (!length(i) || d[i] > dmax) return(hors)
    return(list(idx = i, note = sprintf("maille à %s km", fmt_fr(d[i] / 1000, 1))))
  }

  g <- sf::st_transform(lieu$geom, 2154)
  idx <- which(lengths(sf::st_intersects(pts, g)) > 0)
  if (!length(idx)) {
    d <- as.numeric(sf::st_distance(g, pts))
    i <- which.min(d)
    if (!length(i) || d[i] > dmax) return(hors)
    return(list(idx = i, note = "zone plus petite qu'une maille : maille la plus proche"))
  }
  if (length(idx) > CONFIG$comparer$mailles_zone_max) {
    return(list(idx = integer(0), note = sprintf(
      "zone trop grande (plus de %d mailles)", CONFIG$comparer$mailles_zone_max)))
  }
  list(idx = idx, note = "")
}

#' Resume des lieux et de leurs mailles.
#' @param lieux liste de lieux ; @param mailles liste de cmp_mailles(), meme ordre.
#' @return data.frame(lieu, type, n_mailles, note, retenu).
#' @keywords internal
cmp_table_lieux <- function(lieux, mailles) {
  if (!length(lieux)) {
    return(data.frame(lieu = character(0), type = character(0), n_mailles = integer(0),
                      note = character(0), retenu = logical(0), stringsAsFactors = FALSE))
  }
  data.frame(lieu = vapply(lieux, function(l) l$nom, ""),
             type = vapply(lieux, function(l) l$type, ""),
             n_mailles = vapply(mailles, function(m) length(m$idx), 1L),
             note = vapply(mailles, function(m) m$note, ""),
             retenu = vapply(mailles, function(m) length(m$idx) > 0L, TRUE),
             stringsAsFactors = FALSE)
}

#' Serie annuelle de chaque lieu pour chaque grandeur : moyenne des mailles du lieu.
#'
#' Un lieu n'a pas de valeur une annee ou TOUTES ses mailles en sont privees (NA) ;
#' sinon la moyenne porte sur les mailles renseignees.
#' @param jeu cmp_jeu() ; @param mailles cmp_mailles() par lieu ; @param noms noms des lieux.
#' @param cles cles de couches.
#' @return data.frame(lieu, cle, annee, valeur).
#' @keywords internal
cmp_series <- function(jeu, noms, mailles, cles, executer = httr2::req_perform) {
  garde <- which(vapply(mailles, function(m) length(m$idx) > 0L, TRUE))
  if (!length(garde) || !length(cles)) {
    return(data.frame(lieu = character(0), cle = character(0), annee = integer(0),
                      valeur = numeric(0), stringsAsFactors = FALSE))
  }
  morceaux <- list()
  for (cle in cles) {
    v <- cmp_lire_couche(jeu, cle, executer)$valeurs     # annees x mailles
    for (i in garde) {
      s <- rowMeans(v[, mailles[[i]]$idx, drop = FALSE], na.rm = TRUE)
      s[is.nan(s)] <- NA_real_
      morceaux[[length(morceaux) + 1L]] <- data.frame(
        lieu = noms[i], cle = cle, annee = jeu$annees, valeur = as.numeric(s),
        stringsAsFactors = FALSE)
    }
  }
  res <- do.call(rbind, morceaux)
  rownames(res) <- NULL
  res
}


# ------------------------------------------------------------------------------
# 4. Bilan
# ------------------------------------------------------------------------------

#' Tendance de chaque lieu et grandeur : pente de Sen par decennie et test de
#' Mann-Kendall corrige de l'autocorrelation (tendance()), sur les annees
#' affichees, vingt annees au moins.
#'
#' Separee du bilan : la tendance ne depend pas de l'annee comparee, et coute
#' une trentaine de millisecondes par serie. Changer d'annee ne la recalcule pas.
#' @param series cmp_series() ; @param ctx contexte de calcul.
#' @return data.frame(lieu, cle, pente_dec, tendance_ok, tendance_significative).
#' @export
cmp_tendances <- function(series, ctx) {
  vide <- data.frame(lieu = character(0), cle = character(0), pente_dec = numeric(0),
                     tendance_ok = logical(0), tendance_significative = logical(0),
                     stringsAsFactors = FALSE)
  if (is.null(series) || !nrow(series)) return(vide)
  paires <- unique(series[, c("lieu", "cle")])
  lignes <- lapply(seq_len(nrow(paires)), function(i) {
    s <- series[series$lieu == paires$lieu[i] & series$cle == paires$cle[i] &
                  series$annee %in% ctx$affichage, ]
    td <- tendance(s$annee, s$valeur, CARTE_COUCHES[[paires$cle[i]]]$unite, dec = 3)
    data.frame(lieu = paires$lieu[i], cle = paires$cle[i],
               pente_dec = if (isTRUE(td$ok)) td$pente_dec else NA_real_,
               tendance_ok = isTRUE(td$ok), tendance_significative = isTRUE(td$significative),
               stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, lignes)
  rownames(res) <- NULL
  res
}

#' Valeur de l'annee comparee, normale, ecart et tendance de chaque lieu et grandeur.
#'
#' Meme logique que l'analyse d'un point : la normale est la moyenne des annees de
#' reference (`ctx$ref`) de la serie du lieu, au moins `annees_ref_min` annees ; un
#' ecart n'est juge que s'il depasse une demi ecart-type (classe_ecart).
#' @param series cmp_series() ; @param ctx contexte de calcul ; @param annee annee comparee.
#' @param tendances cmp_tendances(), ou NULL pour la calculer ici.
#' @return data.frame(lieu, cle, titre, unite, decimales, valeur, normale, ecart,
#'   ecart_type_ref, classe, pente_dec, tendance_ok, tendance_significative).
#' @export
cmp_bilan <- function(series, ctx, annee, tendances = NULL) {
  vide <- data.frame(lieu = character(0), cle = character(0), titre = character(0),
                     unite = character(0), decimales = integer(0), valeur = numeric(0),
                     normale = numeric(0), ecart = numeric(0), ecart_type_ref = numeric(0),
                     classe = character(0), pente_dec = numeric(0),
                     tendance_ok = logical(0), tendance_significative = logical(0),
                     stringsAsFactors = FALSE)
  if (is.null(series) || !nrow(series)) return(vide)
  if (is.null(tendances)) tendances <- cmp_tendances(series, ctx)
  paires <- unique(series[, c("lieu", "cle")])
  lignes <- lapply(seq_len(nrow(paires)), function(i) {
    s <- series[series$lieu == paires$lieu[i] & series$cle == paires$cle[i], ]
    k <- CARTE_COUCHES[[paires$cle[i]]]
    ref <- s$annee %in% ctx$ref & is.finite(s$valeur)
    normale <- if (sum(ref) >= CONFIG$comparer$annees_ref_min) mean(s$valeur[ref]) else NA_real_
    sd_ref <- if (sum(ref) >= 3L) stats::sd(s$valeur[ref]) else NA_real_
    valeur <- s$valeur[match(annee, s$annee)]
    ecart <- valeur - normale
    td <- tendances[tendances$lieu == paires$lieu[i] & tendances$cle == paires$cle[i], ]
    data.frame(lieu = paires$lieu[i], cle = paires$cle[i], titre = k$titre, unite = k$unite,
               # Un decompte (0 decimale) moyenne sur une zone n'est plus entier : une
               # decimale, sans quoi un ecart de 0,4 vague s'afficherait « 0 ».
               decimales = if (as.integer(k$decimales) == 0L) 1L else as.integer(k$decimales),
               valeur = valeur, normale = normale,
               ecart = ecart, ecart_type_ref = sd_ref,
               classe = classe_ecart(ecart, sd_ref),
               pente_dec = if (nrow(td) == 1L) td$pente_dec else NA_real_,
               tendance_ok = nrow(td) == 1L && isTRUE(td$tendance_ok),
               tendance_significative = nrow(td) == 1L && isTRUE(td$tendance_significative),
               stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, lignes)
  rownames(res) <- NULL
  res
}

#' Le bilan d'un lieu, en phrases : les cinq ecarts les plus marques de l'annee
#' comparee (rapportes a l'ecart-type de la reference), chacun avec sa tendance.
#' @param b lignes de cmp_bilan() d'un seul lieu.
#' @return vecteur de phrases, vide si aucune valeur n'est exploitable.
#' @export
cmp_phrases_bilan <- function(b, ctx, annee) {
  b <- b[is.finite(b$valeur) & is.finite(b$normale), , drop = FALSE]
  if (!nrow(b)) return(character(0))
  z <- ifelse(is.finite(b$ecart_type_ref) & b$ecart_type_ref > 0,
              abs(b$ecart) / b$ecart_type_ref, 0)
  b <- b[order(-z), , drop = FALSE]; z <- sort(z, decreasing = TRUE)
  b <- utils::head(b, 5L); z <- utils::head(z, 5L)
  vapply(seq_len(nrow(b)), function(i) {
    r <- b[i, ]
    unite <- if (nzchar(r$unite)) r$unite else ""
    lecture <- switch(r$classe,
      haut = if (z[i] >= 1) "nettement au-dessus de la normale" else "au-dessus de la normale",
      bas  = if (z[i] >= 1) "nettement en dessous de la normale" else "en dessous de la normale",
      "proche de la normale")
    tend <- if (isTRUE(r$tendance_ok)) sprintf(
      " Tendance : %s par décennie (%s).",
      fmt_ecart(r$pente_dec, unite, r$decimales),
      if (isTRUE(r$tendance_significative)) "significative" else "non significative") else ""
    sprintf("%s en %d : %s, soit %s par rapport à la normale %s (%s).%s",
            r$titre, annee, fmt_val(r$valeur, unite, r$decimales),
            fmt_ecart(r$ecart, unite, r$decimales), ctx$ref_label, lecture, tend)
  }, "")
}


# ------------------------------------------------------------------------------
# 5. Export
# ------------------------------------------------------------------------------

#' La comparaison, en CSV : une ligne par lieu et grandeur.
#' @param bilan cmp_bilan() ; @param lieux cmp_table_lieux() ; @param annee annee comparee.
#' @export
cmp_export_comparaison <- function(bilan, lieux, annee) {
  if (is.null(bilan) || !nrow(bilan)) return(data.frame())
  i <- match(bilan$lieu, lieux$lieu)
  lecture <- ifelse(!is.finite(bilan$ecart) | !is.finite(bilan$ecart_type_ref), "",
             ifelse(bilan$classe == "haut", "au-dessus",
             ifelse(bilan$classe == "bas", "en dessous", "proche de la normale")))
  d <- data.frame(
    lieu = cmp_texte_csv_sur(bilan$lieu), type = lieux$type[i], n_mailles = lieux$n_mailles[i],
    grandeur = bilan$titre, unite = bilan$unite, annee = annee,
    valeur = bilan$valeur, normale = bilan$normale, ecart = bilan$ecart,
    ecart_type_reference = bilan$ecart_type_ref, lecture = lecture,
    tendance_par_decennie = bilan$pente_dec,
    tendance_significative = ifelse(bilan$tendance_ok, bilan$tendance_significative, NA),
    stringsAsFactors = FALSE)
  num <- vapply(d, is.double, logical(1))
  d[num] <- lapply(d[num], round, 3)
  d
}

#' Les series annuelles de chaque lieu, en CSV (format long).
#' @export
cmp_export_series <- function(series) {
  if (is.null(series) || !nrow(series)) return(data.frame())
  d <- data.frame(lieu = cmp_texte_csv_sur(series$lieu),
                  grandeur = vapply(series$cle, function(k) CARTE_COUCHES[[k]]$titre, ""),
                  unite = vapply(series$cle, function(k) CARTE_COUCHES[[k]]$unite, ""),
                  annee = series$annee, valeur = round(series$valeur, 3),
                  stringsAsFactors = FALSE)
  rownames(d) <- NULL
  d
}


# ------------------------------------------------------------------------------
# 6. Graphique et provenance
# ------------------------------------------------------------------------------

#' Les series d'une grandeur, un trace par lieu.
#'
#' Deux lectures : la valeur, ou l'ECART A LA NORMALE DE CHAQUE LIEU (zero = la
#' normale du lieu) : c'est la seule qui compare des lieux de climats differents
#' (un massif et une plaine n'ont pas la meme temperature, mais ils peuvent se
#' rechauffer de la meme facon). Au-dela de huit lieux, seuls les huit premiers
#' sont traces : une legende de quarante couleurs ne se lit plus.
#' @param series cmp_series() ; @param bilan cmp_bilan() ; @param cle grandeur.
#' @param mode "valeur" ou "ecart".
#' @export
cmp_graphe <- function(series, bilan, cle, mode = c("valeur", "ecart"), ctx, annee,
                       mobile = FALSE) {
  mode <- match.arg(mode)
  s <- series[series$cle == cle & series$annee %in% ctx$affichage, , drop = FALSE]
  if (!nrow(s) || all(is.na(s$valeur))) {
    return(graphe_message("Aucune valeur à tracer pour cette grandeur."))
  }
  k <- CARTE_COUCHES[[cle]]
  lieux <- unique(s$lieu)
  note <- NULL
  if (length(lieux) > length(CONFIG$comparer$palette)) {
    lieux <- lieux[seq_along(CONFIG$comparer$palette)]
    s <- s[s$lieu %in% lieux, , drop = FALSE]
    note <- sprintf("Les %d premiers lieux sont tracés ; les autres figurent dans le tableau et les CSV.",
                    length(CONFIG$comparer$palette))
  }
  s$lieu <- factor(s$lieu, levels = lieux)
  if (mode == "ecart") {
    nor <- bilan$normale[bilan$cle == cle][match(as.character(s$lieu), bilan$lieu[bilan$cle == cle])]
    s$y <- s$valeur - nor
  } else {
    s$y <- s$valeur
  }
  unite <- if (nzchar(k$unite)) sprintf(" (%s)", k$unite) else ""
  p <- ggplot2::ggplot(s, ggplot2::aes(x = .data$annee, y = .data$y, colour = .data$lieu)) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = stats::setNames(CONFIG$comparer$palette[seq_along(lieux)], lieux)) +
    ggplot2::scale_x_continuous(breaks = graphe_ruptures_annees(s$annee, mobile)) +
    ggplot2::labs(x = NULL, colour = NULL,
                  y = if (mode == "ecart") sprintf("Écart à la normale %s%s", ctx$ref_label, unite)
                      else paste0(k$titre, unite),
                  title = k$titre,
                  caption = note) +
    theme_climafor(mobile = mobile)
  # Sur mobile, une seule colonne de legende : quatre noms de lieux cote a cote
  # debordent de l'ecran.
  if (mobile) p <- p + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1L))
  if (mode == "ecart") {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = CL$reference, linetype = "dashed",
                                 linewidth = 0.5)
  }
  if (annee %in% s$annee) {
    p <- p + ggplot2::geom_vline(xintercept = annee, colour = CL$annee_courante,
                                 linewidth = 0.4, alpha = 0.5)
  }
  p
}

#' Lignes de provenance d'un CSV de comparaison.
#' @param jeu cmp_jeu() ; @param tl cmp_table_lieux() ; @param titre titre du fichier.
#' @export
cmp_lignes_metadonnees <- function(jeu, tl, ctx, annee, titre) {
  l <- c(
    sprintf("%s %s — %s", CONFIG$app$nom, CONFIG$app$version, titre),
    sprintf("Source : mailles SAFRAN-ISBA précalculées (Météo-France), %d mailles de 8 km, jeu produit le %s",
            as.integer(jeu$meta$n_mailles), jeu$meta$genere_le),
    sprintf("Lieux : %d (%d points, %d zones)", nrow(tl), sum(tl$type == "point"), sum(tl$type == "zone")),
    "Valeur d'un lieu : maille la plus proche pour un point, moyenne des mailles dont le centre est dans la zone",
    "Seuils : valeurs par défaut du précalcul (les réglages de l'onglet Paramètres ne s'y appliquent pas)",
    sprintf("Référence : %s ; année comparée : %d ; années affichées : %d-%d",
            ctx$ref_label, annee, min(ctx$affichage), max(ctx$affichage)),
    sprintf("Extraction : %s", format(Sys.Date(), "%d/%m/%Y")),
    "Licence : Licence ouverte Etalab 2.0 — Météo-France")
  gsub(" ?; ?", ", ", l)
}
