# ==============================================================================
# utils_fiches.R — FICHES IMPORTEES (v2.0)
# ------------------------------------------------------------------------------
# Une FICHE est une observation sanitaire localisee : un point, une date, une
# essence, un probleme. Le bilan ne SAISIT rien (c'est le role de l'application de
# terrain Sylvasan) : il lit des fiches deja saisies et les replace dans leur
# contexte climatique.
#
# FORMAT ATTENDU (CSV, GeoJSON, GeoPackage, shapefile zippe)
# ----------------------------------------------------------
#   obligatoire : un point (colonnes latitude et longitude, ou la geometrie)
#   facultatif  : fiche (numero), date, essence, probleme, nom (lieu, commune...)
# Les noms de colonnes sont reconnus sans casse ni accent (voir .FICHE_COL_*).
#
# UNE FICHE, UNE LIGNE, CE FORMAT-CI. Quand l'API de l'application de terrain sera
# disponible, un adaptateur n'aura qu'a produire le meme tableau (fiches_vide()) :
# le reste du bilan n'en saura rien.
#
# ENTREE NON FIABLE : nombre de lignes, taille, textes et coordonnees sont bornes ou
# assainis ici, comme pour l'onglet Comparer.
# ==============================================================================

.FICHE_COL_ID   <- c("fiche", "numerofiche", "nfiche", "numero", "idfiche", "id", "code")
.FICHE_COL_DATE <- c("date", "datefiche", "dateobservation", "dateobs", "dateconstat",
                     "datesignalement", "annee")
.FICHE_COL_ESS  <- c("essence", "essenceprincipale", "espece")
.FICHE_COL_PB   <- c("probleme", "bioagresseur", "agent", "symptome", "problemesanitaire",
                     "cause", "organisme")
.FICHE_COL_NOM  <- c("nom", "lieu", "commune", "foret", "site", "placette", "name", "libelle")

FICHE_NON_RENSEIGNE <- "Non renseigné"

#' Tableau de fiches vide, avec ses colonnes : le contrat d'une source de fiches.
#' @export
fiches_vide <- function() {
  data.frame(id = character(0), nom = character(0), lon = numeric(0), lat = numeric(0),
             date = as.Date(character(0)), annee = integer(0),
             essence = character(0), probleme = character(0), stringsAsFactors = FALSE)
}

#' Date lue dans un texte : 2025-07-14, 14/07/2025, 2025/07/14, 14-07-2025, ou une
#' annee seule (2025 : le 1er juillet, la fiche n'a pas de jour).
#' @return Date, NA si illisible.
#' @keywords internal
fiches_lire_date <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x))
  for (fmt in c("%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d", "%d-%m-%Y")) {
    pas <- is.na(out) & !is.na(x) & nzchar(x)
    if (!any(pas)) break
    out[pas] <- suppressWarnings(as.Date(substr(x[pas], 1L, 10L), format = fmt))
  }
  annee <- !is.na(x) & grepl("^[0-9]{4}$", x) & is.na(out)
  # (paste0 d'un vecteur vide rend « -07-01 », pas un vecteur vide : d'ou le garde-fou)
  if (any(annee)) out[annee] <- suppressWarnings(as.Date(paste0(x[annee], "-07-01")))
  # Une date hors de 1959 a l'an prochain est une faute de saisie, pas une fiche.
  borne <- as.Date(sprintf("%d-12-31", as.integer(format(Sys.Date(), "%Y")) + 1L))
  out[!is.na(out) & (out < as.Date("1959-01-01") | out > borne)] <- NA
  out
}

#' Texte de fiche assaini : sans caractere de controle, 60 caracteres, sinon « Non renseigné ».
#' @keywords internal
fiches_texte <- function(x) {
  x <- iconv(as.character(x), from = "UTF-8", to = "UTF-8", sub = "")
  x <- trimws(gsub("[[:space:]]+", " ", gsub("[[:cntrl:]]", " ", x)))
  x <- ifelse(is.na(x) | !nzchar(x), FICHE_NON_RENSEIGNE,
              ifelse(nchar(x) > 60L, paste0(substr(x, 1L, 59L), "…"), x))
  x
}

#' Construit le tableau de fiches a partir d'attributs (texte) et de coordonnees.
#' @param attr data.frame d'attributs (toutes colonnes en texte).
#' @param lon,lat coordonnees WGS84, une par ligne.
#' @return list(fiches, problemes).
#' @keywords internal
fiches_construire <- function(attr, lon, lat) {
  prob <- character(0)
  n <- length(lon)
  noms_n <- cmp_normaliser_nom_colonne(names(attr))
  col <- function(cand) {
    for (c in cand) { i <- which(noms_n == c); if (length(i)) return(attr[[i[1]]]) }
    NULL
  }
  id <- col(.FICHE_COL_ID); nom <- col(.FICHE_COL_NOM)
  date_txt <- col(.FICHE_COL_DATE); ess <- col(.FICHE_COL_ESS); pb <- col(.FICHE_COL_PB)
  if (is.null(id)) id <- sprintf("F%d", seq_len(n))
  id <- fiches_texte(id)
  id[id == FICHE_NON_RENSEIGNE] <- sprintf("F%d", which(id == FICHE_NON_RENSEIGNE))
  # Deux fiches ne portent jamais le meme identifiant : il sert de nom de lieu.
  id <- make.unique(id, sep = " #")
  nom_txt <- if (is.null(nom)) rep(FICHE_NON_RENSEIGNE, n) else fiches_texte(nom)
  date <- if (is.null(date_txt)) rep(as.Date(NA), n) else fiches_lire_date(date_txt)

  valide <- is.finite(lon) & is.finite(lat) &
    lat >= PARTAGE_BORNES$lat[1] & lat <= PARTAGE_BORNES$lat[2] &
    lon >= PARTAGE_BORNES$lon[1] & lon <= PARTAGE_BORNES$lon[2]
  if (any(!valide)) {
    ou <- which(!valide)
    prob <- c(prob, sprintf(
      "%d fiche%s écartée%s : coordonnées illisibles ou hors de la France métropolitaine (%s%s). Latitude et longitude sont-elles inversées ?",
      length(ou), if (length(ou) > 1L) "s" else "", if (length(ou) > 1L) "s" else "",
      paste(utils::head(id[ou], 5L), collapse = ", "), if (length(ou) > 5L) ", …" else ""))
  }
  if (!is.null(date_txt)) {
    sans <- valide & is.na(date) & nzchar(trimws(as.character(date_txt)))
    if (any(sans)) prob <- c(prob, sprintf(
      "%d fiche%s avec une date illisible : %s analysée%s avec l'année comparée choisie.",
      sum(sans), if (sum(sans) > 1L) "s" else "", if (sum(sans) > 1L) "elles sont" else "elle est",
      if (sum(sans) > 1L) "s" else ""))
  }
  f <- data.frame(id = id, nom = nom_txt, lon = lon, lat = lat, date = date,
                  annee = suppressWarnings(as.integer(format(date, "%Y"))),
                  essence = if (is.null(ess)) rep(FICHE_NON_RENSEIGNE, n) else fiches_texte(ess),
                  probleme = if (is.null(pb)) rep(FICHE_NON_RENSEIGNE, n) else fiches_texte(pb),
                  stringsAsFactors = FALSE)
  f <- f[valide, , drop = FALSE]
  if (nrow(f) > CONFIG$synthese$fiches_max) {
    prob <- c(prob, sprintf("Plus de %d fiches : seules les %d premières sont lues.",
                            CONFIG$synthese$fiches_max, CONFIG$synthese$fiches_max))
    f <- f[seq_len(CONFIG$synthese$fiches_max), , drop = FALSE]
  }
  rownames(f) <- NULL
  list(fiches = f, problemes = prob)
}

#' Lit un fichier de fiches (voir l'en-tete du fichier pour le format).
#' @param chemin fichier recu ; @param nom_origine son nom d'origine (l'extension decide).
#' @return list(fiches, problemes) ; `fiches` est vide si rien n'est lisible.
#' @export
fiches_lire <- function(chemin, nom_origine = chemin) {
  echec <- function(m) list(fiches = fiches_vide(), problemes = m)
  ext <- tolower(tools::file_ext(nom_origine))
  if (!is.character(chemin) || length(chemin) != 1L || !isTRUE(file.exists(chemin))) return(echec("Fichier introuvable."))
  tryCatch({
    if (isTRUE(file.size(chemin) > CONFIG$comparer$fichier_max_mo * 1024^2)) {
      return(echec(sprintf("Fichier trop volumineux (plus de %g Mo).", CONFIG$comparer$fichier_max_mo)))
    }
    if (ext %in% c("csv", "txt", "tsv")) {
      t <- cmp_lire_table_csv(chemin)
      if (!is.null(t$erreur)) return(echec(t$erreur))
      d <- t$d
      noms_n <- cmp_normaliser_nom_colonne(names(d))
      i_lat <- which(noms_n %in% .CMP_COL_LAT)[1]; i_lon <- which(noms_n %in% .CMP_COL_LON)[1]
      if (is.na(i_lat) || is.na(i_lon)) {
        return(echec(paste("Colonnes de coordonnées introuvables : le fichier doit porter une colonne",
                           "« latitude » et une colonne « longitude » (degrés décimaux, WGS84).")))
      }
      nombre <- function(x) suppressWarnings(as.numeric(gsub(",", ".", trimws(x), fixed = TRUE)))
      r <- fiches_construire(d, nombre(d[[i_lon]]), nombre(d[[i_lat]]))
      r$problemes <- c(t$problemes, r$problemes)
      return(r)
    }
    if (ext %in% c("geojson", "json", "gpkg", "zip")) {
      g <- cmp_lire_sf(chemin, nom_origine)
      if (!is.null(g$erreur)) return(echec(g$erreur))
      x <- g$x
      type <- as.character(sf::st_geometry_type(x))
      pts <- type == "POINT"
      prob <- g$problemes
      if (!any(pts)) return(echec("Le fichier ne contient aucun point : une fiche est un point."))
      if (any(!pts)) prob <- c(prob, sprintf("%d entité(s) qui ne sont pas des points ont été ignorées.", sum(!pts)))
      x <- x[pts, ]
      xy <- sf::st_coordinates(x)
      a <- sf::st_drop_geometry(x)
      a[] <- lapply(a, function(v) as.character(v))
      r <- fiches_construire(a, as.numeric(xy[, 1]), as.numeric(xy[, 2]))
      r$problemes <- c(prob, r$problemes)
      return(r)
    }
    echec(sprintf("Format « .%s » non reconnu. Formats acceptés : CSV, GeoJSON, GeoPackage, shapefile zippé (.zip).", ext))
  }, error = function(e) {
    journaliser_erreur("Import de fiches", e)
    echec("Le fichier n'a pas pu être importé.")
  })
}

#' Modele de CSV de fiches, a telecharger et a remplir.
#' @export
fiches_modele_csv <- function() {
  data.frame(fiche = c("F001", "F002", "F003"),
             latitude = c(43.07, 42.92, 43.80), longitude = c(2.05, 0.41, 3.20),
             date = c("2025-08-20", "2026-04-13", "2025-09-02"),
             essence = c("Cèdre de l'Atlas", "Sapin pectiné", "Chêne pubescent"),
             probleme = c("Rougissement inexpliqué", "Scolytes (Pityokteines)", "Rougissement foliaire"),
             nom = c("Corbières", "Vallée d'Aure", "Hérault"),
             stringsAsFactors = FALSE)
}

#' Source de fiches par l'API de l'application de terrain : PAS ENCORE RACCORDEE.
#' Quand elle le sera, cette fonction rendra le meme tableau que fiches_lire()
#' (list(fiches = fiches_vide() rempli, problemes)). CONFIG$sylvasan$api_fiches en
#' donnera l'adresse.
#' @export
fiches_lire_api <- function(...) {
  list(fiches = fiches_vide(), problemes = "L'API de l'application de terrain n'est pas encore raccordée.")
}

#' Compte des fiches par modalite d'une colonne, du plus frequent au moins frequent.
#' @param col "essence", "probleme" ou "annee".
#' @return data.frame(modalite, n, part) ; part en pourcentage.
#' @keywords internal
fiches_compter <- function(fiches, col) {
  if (is.null(fiches) || !nrow(fiches)) {
    return(data.frame(modalite = character(0), n = integer(0), part = numeric(0)))
  }
  v <- fiches[[col]]
  v <- ifelse(is.na(v), FICHE_NON_RENSEIGNE, as.character(v))
  t <- sort(table(v), decreasing = TRUE)
  data.frame(modalite = names(t), n = as.integer(t), part = 100 * as.integer(t) / nrow(fiches),
             stringsAsFactors = FALSE)
}
