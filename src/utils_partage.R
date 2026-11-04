# ==============================================================================
# utils_partage.R — ETAT PARTAGEABLE D'UNE ANALYSE (lien, rechargement)
# ------------------------------------------------------------------------------
# Avant la v1.4, rien de l'analyse ne survivait a un rechargement de page, et
# deux collegues ne pouvaient pas ouvrir la meme vue (audit, S2). L'adresse de la
# page porte maintenant l'etat utile :
#
#   ?v=1&x=1.4727&y=43.3500&n=Auterive%20(31190)&s=safran&r=1991-2020&a=2026
#    &p=1970-2026&f=4-10&c=1970-2026&t=chaleur_t2:28,gel_t:-1
#
#   x, y  coordonnees WGS 84        n   nom affiche du point
#   s     source : « safran » ou identifiant d'une station
#   r     periode de reference      a   annee comparee
#   p     plage affichee            f   fenetre de mois (debut-fin)
#   c     plage telechargee         t   seuils MODIFIES seulement (nom:valeur)
#
# POURQUOI DES NOMBRES ET NON DES LIBELLES
# ----------------------------------------
# « r=1991-2020 » reste vrai si le libelle d'un preset change ; un libelle
# accentue et long, lui, alourdit le lien et casse au premier renommage.
#
# CE QUI ARRIVE DANS L'ADRESSE EST UNE ENTREE NON FIABLE
# ------------------------------------------------------
# N'importe qui peut forger un lien. Le decodeur ne fait donc confiance a rien :
# chaque champ est converti, borne, puis REJETE (valeur par defaut + remarque)
# s'il sort des limites. Le nom du point, seul texte libre, est nettoye :
# il est affiche dans l'interface et repris dans les rapports.
#
# Les deux fonctions de ce fichier sont pures (aucun reactif, aucun acces
# reseau) : elles se testent sans navigateur.
# ==============================================================================

PARTAGE_VERSION <- 1L

# Limites geographiques de la France metropolitaine (memes bornes que la saisie
# manuelle de coordonnees, mod_localisation.R).
PARTAGE_BORNES <- list(lat = c(41, 52), lon = c(-6, 10))

#' Nettoie un nom de lieu venu d'une adresse : lettres, chiffres, espaces et
#' ponctuation courante seulement, 80 caracteres au plus.
#' @keywords internal
partage_nom_sur <- function(x, defaut = "Point partagé") {
  x <- as.character(x %||% "")[1]
  if (is.na(x)) x <- ""
  x <- gsub("[^[:alnum:] '’().,/-]", "", x)
  x <- trimws(gsub("[[:space:]]+", " ", x))
  x <- substr(x, 1L, 80L)
  if (!nzchar(x)) defaut else x
}

#' Plage d'annees « 1970-2026 » -> c(1970, 2026), ou NULL si elle est invalide.
#' @keywords internal
partage_lire_plage <- function(x, annee_max) {
  x <- as.character(x %||% "")[1]
  m <- regmatches(x, regexec("^(\\d{4})-(\\d{4})$", x))[[1]]
  if (length(m) != 3L) return(NULL)
  v <- as.integer(m[2:3])
  if (anyNA(v) || v[1] > v[2] || v[1] < CONFIG$annee_min || v[2] > annee_max) return(NULL)
  v
}

#' Fenetre « 4-10 » -> c(4, 10), ou NULL.
#' @keywords internal
partage_lire_fenetre <- function(x) {
  x <- as.character(x %||% "")[1]
  m <- regmatches(x, regexec("^(\\d{1,2})-(\\d{1,2})$", x))[[1]]
  if (length(m) != 3L) return(NULL)
  v <- as.integer(m[2:3])
  if (anyNA(v) || any(v < 1L) || any(v > 12L)) return(NULL)
  v
}

#' Seuils « chaleur_t2:28,gel_t:-1 » -> liste nommee (seuls les seuils connus,
#' dans leurs bornes). Le reste est ecarte et signale.
#' @keywords internal
partage_lire_seuils <- function(x) {
  out <- list(); ecartes <- character(0)
  x <- as.character(x %||% "")[1]
  if (is.na(x) || !nzchar(x)) return(list(valeurs = out, ecartes = ecartes))
  for (paire in strsplit(x, ",", fixed = TRUE)[[1]]) {
    kv <- strsplit(paire, ":", fixed = TRUE)[[1]]
    if (length(kv) != 2L) { ecartes <- c(ecartes, paire); next }
    j <- match(kv[1], DESCRIPTIF_SEUILS$id)
    v <- suppressWarnings(as.numeric(kv[2]))
    if (is.na(j) || !is.finite(v) ||
        v < DESCRIPTIF_SEUILS$min[j] || v > DESCRIPTIF_SEUILS$max[j]) {
      ecartes <- c(ecartes, paire); next
    }
    out[[kv[1]]] <- v
  }
  list(valeurs = out, ecartes = ecartes)
}

#' Decode l'adresse d'une page en etat d'analyse.
#'
#' @param requete chaine de requete (« ?x=1&y=2 », avec ou sans « ? ») ou liste
#'   nommee deja decodee (shiny::parseQueryString).
#' @param annee_max derniere annee admise (l'annee en cours).
#' @return NULL si l'adresse ne designe pas un point exploitable ; sinon une
#'   liste (lon, lat, nom, source, ref, courante, plage, fenetre, annees_charge,
#'   seuils, remarques). Tout champ invalide prend sa valeur par defaut et
#'   laisse une phrase dans `remarques`.
#' @export
partage_decoder <- function(requete, annee_max = as.integer(format(Sys.Date(), "%Y"))) {
  q <- if (is.list(requete)) requete else {
    s <- sub("^\\?", "", as.character(requete %||% "")[1])
    if (is.na(s) || !nzchar(s)) return(NULL)
    shiny::parseQueryString(s)
  }
  if (!length(q)) return(NULL)

  un <- function(k) {
    v <- q[[k]]
    if (is.null(v) || !length(v)) return(NULL)
    as.character(v)[1]
  }

  lon <- suppressWarnings(as.numeric(un("x")))
  lat <- suppressWarnings(as.numeric(un("y")))
  if (!isTRUE(is.finite(lon)) || !isTRUE(is.finite(lat))) return(NULL)
  if (lat < PARTAGE_BORNES$lat[1] || lat > PARTAGE_BORNES$lat[2] ||
      lon < PARTAGE_BORNES$lon[1] || lon > PARTAGE_BORNES$lon[2]) return(NULL)

  remarques <- character(0)
  note <- function(...) remarques <<- c(remarques, sprintf(...))

  src <- un("s") %||% "safran"
  if (!grepl("^(safran|[A-Za-z0-9_-]{1,24})$", src)) {
    note("Source « %s » non reconnue : grille SAFRAN utilisée.", partage_nom_sur(src, "?"))
    src <- "safran"
  }

  ref <- partage_lire_plage(un("r"), annee_max)
  if (is.null(ref)) {
    if (!is.null(un("r"))) note("Période de référence invalide : %s utilisée.", CONFIG$reference_defaut)
    ref <- CONFIG$references[[CONFIG$reference_defaut]]
  }

  a <- suppressWarnings(as.integer(un("a")))
  if (length(a) != 1L || is.na(a) || a < CONFIG$annee_min || a > annee_max) {
    if (!is.null(un("a"))) note("Année comparée invalide : %d utilisée.", annee_max)
    a <- annee_max
  }

  plage <- partage_lire_plage(un("p"), annee_max)
  if (is.null(plage)) {
    if (!is.null(un("p"))) note("Plage affichée invalide : plage par défaut utilisée.")
    plage <- c(1970L, annee_max)
  }

  fen <- partage_lire_fenetre(un("f"))
  if (is.null(fen)) {
    if (!is.null(un("f"))) note("Fenêtre de mois invalide : fenêtre par défaut utilisée.")
    fen <- as.integer(CONFIG$fenetre_defaut)
  }

  ch <- partage_lire_plage(un("c"), annee_max)
  if (is.null(ch)) {
    if (!is.null(un("c"))) note("Plage téléchargée invalide : plage par défaut utilisée.")
    ch <- c(1970L, annee_max)
  }
  # Le telechargement doit couvrir la reference : sinon la normale serait fausse.
  ch <- c(min(ch[1], ref[1]), max(ch[2], ref[2], a))

  se <- partage_lire_seuils(un("t"))
  if (length(se$ecartes)) {
    note("%d seuil(s) du lien ignoré(s) (inconnu ou hors bornes).", length(se$ecartes))
  }

  list(lon = round(lon, 4), lat = round(lat, 4),
       nom = partage_nom_sur(un("n")), source = src,
       ref = as.integer(ref), courante = a, plage = as.integer(plage),
       fenetre = fen, annees_charge = as.integer(ch),
       seuils = se$valeurs, remarques = remarques)
}

#' Etat d'une analyse -> chaine de requete (sans « ? »).
#' @param etat liste (lon, lat, nom, source, ref, courante, plage, fenetre,
#'   annees_charge, seuils) ; `ref` et `plage` peuvent etre des suites d'annees,
#'   seuls leurs extremes comptent.
#' @export
partage_encoder <- function(etat) {
  enc <- function(x) utils::URLencode(as.character(x), reserved = TRUE)
  plage <- function(v) sprintf("%d-%d", as.integer(min(v)), as.integer(max(v)))
  seuils <- etat$seuils %||% list()
  t <- if (length(seuils)) {
    paste(sprintf("%s:%s", names(seuils),
                  vapply(seuils, function(v) format(v, trim = TRUE, digits = 10), character(1))),
          collapse = ",")
  } else NULL
  champs <- list(
    v = PARTAGE_VERSION,
    x = sprintf("%.4f", etat$lon), y = sprintf("%.4f", etat$lat),
    n = partage_nom_sur(etat$nom), s = etat$source %||% "safran",
    r = plage(etat$ref), a = as.integer(etat$courante),
    p = plage(etat$plage), f = sprintf("%d-%d", etat$fenetre[1], etat$fenetre[2]),
    c = plage(etat$annees_charge), t = t)
  champs <- Filter(Negate(is.null), champs)
  paste(sprintf("%s=%s", names(champs), vapply(champs, enc, character(1))), collapse = "&")
}

#' Seuils qui different des valeurs par defaut (liste nommee).
#' @export
partage_seuils_modifies <- function(seuils) {
  ids <- intersect(names(CONFIG$seuils), DESCRIPTIF_SEUILS$id)
  out <- list()
  for (i in ids) {
    v <- seuils[[i]]
    if (isTRUE(is.finite(v)) && !isTRUE(all.equal(as.numeric(v), as.numeric(CONFIG$seuils[[i]])))) {
      out[[i]] <- as.numeric(v)
    }
  }
  out
}

#' Preset de reference correspondant a une plage d'annees (ou « custom »).
#' @export
partage_preset_reference <- function(ref) {
  for (nm in names(CONFIG$references)) {
    if (identical(as.integer(range(CONFIG$references[[nm]])), as.integer(range(ref)))) return(nm)
  }
  "custom"
}

#' Preset de fenetre correspondant a deux mois (ou « custom »).
#' @export
partage_preset_fenetre <- function(fen) {
  for (nm in names(CONFIG$fenetres)) {
    if (identical(as.integer(CONFIG$fenetres[[nm]]), as.integer(fen))) return(nm)
  }
  "custom"
}
