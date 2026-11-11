# ==============================================================================
# utils_stations_publiees.R — STATIONS METEO-FRANCE PUBLIEES (v2.1)
# ------------------------------------------------------------------------------
# POURQUOI. Jusqu'a la v2.0, une station ne se lisait que par un telechargement : les
# fichiers departementaux de Meteo-France, ligne a ligne, a chaque analyse. C'est
# l'onglet Bilan climatique, et c'est trop lourd pour comparer une station a une maille
# ou a un departement, ou pour afficher « Autour de moi » en tournee.
#
# DEPUIS LA v2.1, les indicateurs ANNUELS des stations sont precalcules comme ceux des
# mailles (outils/stations_precalcul.R) et publies a cote d'eux :
#
#   stations.json      la liste des stations (position, altitude, derniere mesure,
#                      completude, annees valides) et les grandeurs publiees ;
#   s_<cle>.bin.gz     une grandeur, MEME FORMAT BINAIRE que c_<cle>.bin.gz, avec une
#                      station a la place d'une maille (voir src/utils_carte.R).
#
# Les stations passent donc par le meme chemin que les mailles : une serie annuelle par
# lieu et par grandeur, puis le meme bilan (normale, ecart, tendance, phrases, tableaux,
# graphiques, exports). Ce fichier ne fait que LIRE, VALIDER et PROPOSER.
#
# MESURE ET MODELE. Une station mesure ; une maille SAFRAN est une reanalyse (un modele
# alimente par des observations). Les deux disent « le climat ici » et ne sont pas la
# meme chose : le bilan les affiche COTE A COTE et ne les fond jamais.
#
# ENTREE NON FIABLE. stations.json vient d'un hote tiers (comme meta.json) : chaque champ
# est verifie avant usage, et les noms de station sont assainis (ils ressortent dans
# l'interface et dans les CSV).
# ==============================================================================

STATIONS_PUBLIEES_COLONNES <- c("id", "nom", "dept", "lon", "lat", "alt", "derniere_obs",
                                "fr_p", "fr_t", "a_pluie", "a_temp", "premiere", "n_p", "n_t")

#' Nom de station lisible : « TOULOUSE-BLAGNAC » devient « Toulouse-Blagnac ».
#' Les petits mots (de, du, la, sur...) restent en minuscules sauf en tete.
#' @keywords internal
stations_nom_lisible <- function(x) {
  petits <- c("de", "du", "des", "la", "le", "les", "sur", "sous", "en", "et", "aux", "au", "d", "l")
  un <- function(t) {
    if (is.na(t) || !nzchar(t)) return("")
    t <- tolower(t)
    # Mots separes par espace, tiret ou apostrophe : on garde les separateurs.
    morceaux <- regmatches(t, gregexpr("[^ '’-]+|[ '’-]+", t))[[1]]
    mots <- grepl("^[^ '’-]+$", morceaux)
    prem <- which(mots)[1]
    for (k in which(mots)) {
      if (k == prem || !(morceaux[k] %in% petits)) {
        morceaux[k] <- paste0(toupper(substr(morceaux[k], 1L, 1L)), substr(morceaux[k], 2L, nchar(morceaux[k])))
      }
    }
    paste(morceaux, collapse = "")
  }
  vapply(as.character(x), un, "", USE.NAMES = FALSE)
}

# ------------------------------------------------------------------------------
# 1. Validation de stations.json
# ------------------------------------------------------------------------------

#' Verifie la forme de stations.json (meme rigueur que carte_meta_valider).
#' @param meta liste issue de jsonlite::fromJSON(simplifyVector = FALSE).
#' @return list(ok, erreurs).
#' @keywords internal
stations_meta_valider <- function(meta) {
  err <- character(0)
  ajouter <- function(m) err <<- c(err, m)
  cfg <- CONFIG$stations$publie
  if (!is.list(meta)) return(list(ok = FALSE, erreurs = "stations.json illisible."))

  if (!identical(suppressWarnings(as.integer(meta$schema %||% NA)), as.integer(cfg$schema))) {
    ajouter(sprintf("Version du format %s, %d attendue.", as.character(meta$schema %||% "absente"), cfg$schema))
  }
  annees <- suppressWarnings(as.integer(unlist(meta$annees)))
  if (length(annees) < 2L || anyNA(annees) || is.unsorted(annees, strictly = TRUE) ||
      min(annees) < 1900L || max(annees) > 2200L) {
    ajouter("Liste d'années invalide.")
  }
  n <- suppressWarnings(as.integer(meta$n_stations %||% NA))
  if (length(n) != 1L || is.na(n) || n < 1L || n > cfg$stations_max) ajouter("Nombre de stations invalide.")
  if (!is.character(meta$reference) || !grepl("^[0-9]{4}-[0-9]{4}$", meta$reference[1] %||% "")) {
    ajouter("Période de référence invalide.")
  }
  if (!is.character(meta$genere_le) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", meta$genere_le[1] %||% "")) {
    ajouter("Date de production invalide.")
  }

  # La table : un vecteur par colonne, tous de la longueur annoncee.
  st <- meta$stations
  if (!is.list(st) || !all(STATIONS_PUBLIEES_COLONNES %in% names(st))) {
    ajouter("Table des stations incomplète.")
  } else if (length(n) == 1L && !is.na(n)) {
    longueurs <- vapply(st[STATIONS_PUBLIEES_COLONNES], length, 1L)
    if (any(longueurs != n)) {
      ajouter("Colonnes de la table des stations de longueurs différentes.")
    } else {
      t <- stations_table_brute(st)
      if (anyNA(t$id) || anyDuplicated(t$id) || !all(grepl("^[A-Za-z0-9]{3,12}$", t$id))) {
        ajouter("Identifiants de stations absents, en double ou mal formés.")
      }
      if (anyNA(t$lon) || anyNA(t$lat) ||
          any(t$lat < PARTAGE_BORNES$lat[1] | t$lat > PARTAGE_BORNES$lat[2] |
              t$lon < PARTAGE_BORNES$lon[1] | t$lon > PARTAGE_BORNES$lon[2])) {
        ajouter("Coordonnées de stations absentes ou hors du territoire.")
      }
    }
  }

  couches <- meta$couches
  if (!is.list(couches) || !length(couches)) {
    ajouter("Aucune grandeur publiée pour les stations.")
  } else {
    cles <- vapply(couches, function(k) if (is.list(k)) as.character(k$cle %||% NA_character_)[1] else NA_character_, "")
    if (anyNA(cles) || anyDuplicated(cles)) ajouter("Clés de grandeurs absentes ou en double.")
    for (k in couches) {
      if (!is.list(k)) { ajouter("Grandeur mal formée."); next }
      etiq <- as.character(k$cle %||% "?")[1]
      if (!(etiq %in% names(CARTE_COUCHES))) { ajouter(sprintf("Grandeur inconnue : %s.", etiq)); next }
      if (!carte_nom_fichier_sur(k$fichier %||% NA)) ajouter(sprintf("%s : nom de fichier invalide.", etiq))
      st_k <- suppressWarnings(as.integer(k$stock %||% NA))
      if (length(st_k) != 1L || is.na(st_k) || st_k < 0L || st_k > 4L) ajouter(sprintf("%s : précision de stockage invalide.", etiq))
    }
  }

  # La base precalculee (v2.2) est facultative : un jeu publie avant elle n'en a pas. Si elle est
  # la, chaque entree est verifiee comme une grandeur. [[ ]] et non $ : `$` completerait en
  # silence `base` par `base_parametres` quand `base` est absent.
  b <- meta[["base"]]
  if (!is.null(b)) {
    if (!is.list(b) || !length(b)) {
      ajouter("Base précalculée mal formée.")
    } else {
      cles_b <- vapply(b, function(k) if (is.list(k)) as.character(k$cle %||% NA_character_)[1] else NA_character_, "")
      if (anyNA(cles_b) || anyDuplicated(cles_b)) ajouter("Base précalculée : clés absentes ou en double.")
      for (k in b) {
        if (!is.list(k)) { ajouter("Base précalculée : entrée mal formée."); next }
        etiq <- as.character(k$cle %||% "?")[1]
        ref <- STATIONS_BASE_COUCHES[[etiq]]
        # Une entree inconnue (jeu publie par une version plus recente) est ignoree, comme pour
        # les mailles (carte_meta_valider) : elle ne sert a rien ici, elle ne doit pas tout refuser.
        if (is.null(ref)) next
        if (!identical(as.character(k$fichier %||% NA)[1], sprintf("s_%s.bin.gz", etiq)) ||
            !carte_nom_fichier_sur(k$fichier %||% NA)) {
          ajouter(sprintf("Base précalculée, %s : nom de fichier invalide.", etiq))
        }
        st_b <- suppressWarnings(as.integer(k$stock %||% NA))
        if (length(st_b) != 1L || is.na(st_b) || st_b < 0L || st_b > 4L) {
          ajouter(sprintf("Base précalculée, %s : précision de stockage invalide.", etiq))
        }
        if (!identical(as.character(k$type %||% NA)[1], ref$type)) {
          ajouter(sprintf("Base précalculée, %s : type invalide.", etiq))
        } else if (identical(ref$type, "ref") &&
                   !identical(suppressWarnings(as.integer(k$lignes %||% NA)), as.integer(ref$lignes))) {
          ajouter(sprintf("Base précalculée, %s : nombre de lignes invalide.", etiq))
        }
      }
      bp <- meta[["base_parametres"]]
      emp <- if (is.list(bp)) bp[["empreinte"]] else NULL
      # 3000 caracteres, comme pour la base des mailles (carte_base_valider) : l'empreinte liste tous
      # les seuils, et chaque seuil ajoute a l'application (v2.4 : 405 caracteres) l'allonge.
      if (!is.character(emp) || length(emp) != 1L || !nzchar(emp) || nchar(emp) > 3000L) {
        ajouter("Base précalculée : empreinte des réglages absente ou invalide.")
      }
      vi <- if (is.list(bp)) unlist(bp[["vides"]]) else NULL
      if (length(vi) && (!is.character(vi) || anyNA(vi) || any(nchar(vi) > 60L))) {
        ajouter("Base précalculée : liste des entrées vides invalide.")
      }
    }
  }
  list(ok = !length(err), erreurs = err)
}

# Colonnes de la table, typees ; un `null` du JSON devient NA.
stations_colonne <- function(x, type) {
  f <- switch(type, chr = as.character, num = as.numeric, int = as.integer, lgl = as.logical)
  na <- switch(type, chr = NA_character_, num = NA_real_, int = NA_integer_, lgl = NA)
  vapply(x, function(v) if (is.null(v) || !length(v)) na else suppressWarnings(f(v[[1]])), na)
}

#' La table des stations telle que publiee, typee mais NON assainie (sert a la validation).
#' @keywords internal
stations_table_brute <- function(st) {
  data.frame(
    id = stations_colonne(st$id, "chr"), nom = stations_colonne(st$nom, "chr"),
    dept = stations_colonne(st$dept, "chr"),
    lon = stations_colonne(st$lon, "num"), lat = stations_colonne(st$lat, "num"),
    alt = stations_colonne(st$alt, "num"),
    derniere_obs = as.Date(stations_colonne(st$derniere_obs, "chr")),
    fr_p = stations_colonne(st$fr_p, "num"), fr_t = stations_colonne(st$fr_t, "num"),
    a_pluie = stations_colonne(st$a_pluie, "lgl"), a_temp = stations_colonne(st$a_temp, "lgl"),
    premiere = stations_colonne(st$premiere, "int"),
    n_p = stations_colonne(st$n_p, "int"), n_t = stations_colonne(st$n_t, "int"),
    stringsAsFactors = FALSE)
}

#' La table des stations, assainie : noms lisibles et sans caractere de controle, booleens
#' sans NA, `idx` = rang de la station dans les fichiers de grandeurs.
#' @param meta stations.json valide.
#' @keywords internal
stations_table <- function(meta) {
  t <- stations_table_brute(meta$stations)
  t$nom <- vapply(stations_nom_lisible(t$nom), cmp_nom_propre, "", defaut = "Station")
  t$dept <- ifelse(is.na(t$dept), "", t$dept)
  t$a_pluie <- t$a_pluie %in% TRUE
  t$a_temp <- t$a_temp %in% TRUE
  t$fr_p[!is.finite(t$fr_p)] <- 0
  t$fr_t[!is.finite(t$fr_t)] <- 0
  t$idx <- seq_len(nrow(t))
  t
}


# ------------------------------------------------------------------------------
# 2. Lecture dans l'application
# ------------------------------------------------------------------------------

#' Les stations publiees a cote de la carte.
#'
#' Facultatif : un jeu publie avant la v2.1 n'en porte pas. L'absence n'est PAS une erreur
#' (la carte, la comparaison de mailles et les bilans de mailles marchent comme avant) : le
#' resultat dit seulement ce qui manque, pour que l'interface le dise.
#' @param transport carte_transport() ; @param executer requete HTTP (injectable).
#' @return list(ok = TRUE, meta, annees, table, transport) ou
#'   list(ok = FALSE, raison = "absent" | "invalide", detail, definitif).
#'   `definitif` FALSE : l'echec peut etre passager (reseau), ne pas le conserver en memoire.
#' @keywords internal
stations_jeu <- function(transport, maintenant = Sys.time(), executer = httr2::req_perform) {
  f <- tryCatch(cmp_fichier_carte(CONFIG$stations$publie$fichier, transport, maintenant, executer),
                error = function(e) e)
  if (inherits(f, "error")) {
    m <- conditionMessage(f)
    # Un 404 (ou un fichier absent du dossier local) : le jeu n'a pas de stations, c'est
    # definitif. Tout autre echec (reseau, delai) peut passer : on reessaiera.
    return(list(ok = FALSE, raison = "absent", detail = m,
                definitif = grepl("HTTP 404|introuvable", m)))
  }
  if (isTRUE(file.size(f) > 8 * 1024^2)) {
    return(list(ok = FALSE, raison = "invalide", detail = "stations.json trop volumineux.", definitif = TRUE))
  }
  meta <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  v <- stations_meta_valider(meta)
  if (!v$ok) return(list(ok = FALSE, raison = "invalide", detail = paste(v$erreurs, collapse = " "), definitif = TRUE))
  list(ok = TRUE, meta = meta, annees = as.integer(unlist(meta$annees)),
       table = stations_table(meta), transport = transport)
}


# ------------------------------------------------------------------------------
# 3. Les grandeurs publiees et les stations proches d'un point
# ------------------------------------------------------------------------------

#' Relit une grandeur des stations publiees : matrice annees x stations.
#'
#' Meme fichier que les couches de mailles (carte_lire_couche), avec les stations pour
#' colonnes, dans l'ordre de la table (`idx`). Les valeurs absentes restent NA : une station
#' qui ne mesure pas une grandeur n'a pas zero, elle n'a rien.
#' @param sj cmp_stations() ; @param cle cle de grandeur (CARTE_COUCHES).
#' @return carte_lire_couche() (valeurs : annees x stations), ou NULL si la grandeur n'est
#'   pas publiee pour les stations (SWI, neige, VPD : aucune station ne les mesure).
#' @keywords internal
stations_lire_couche <- function(sj, cle, executer = httr2::req_perform) {
  if (is.null(sj) || !isTRUE(sj$ok)) return(NULL)
  k <- Filter(function(k) identical(as.character(k$cle)[1], cle), sj$meta$couches)
  if (!length(k)) return(NULL)
  f <- cmp_fichier_carte(as.character(k[[1]]$fichier), sj$transport, executer = executer)
  l <- carte_lire_couche(f, nrow(sj$table), length(sj$annees), as.integer(k[[1]]$stock))
  if (!isTRUE(l$taille_exacte)) stop(sprintf("stations, grandeur %s : taille de fichier inattendue", cle))
  l
}

#' Les stations publiees autour d'un point, classees par pertinence.
#'
#' Le classement est celui de la recherche en direct (mf_classer_stations) : distance,
#' ecart d'altitude, fraicheur et completude des mesures. Aucune requete : la table des
#' stations est deja lue.
#' @param sj cmp_stations() ; @param alt altitude du point (NA : critere ignore).
#' @return data.frame trie (vide si aucune station a portee), ou NULL si `sj` n'est pas lu.
#' @keywords internal
stations_proches_publiees <- function(sj, lon, lat, alt = NA_real_) {
  if (is.null(sj) || !isTRUE(sj$ok)) return(NULL)
  mf_classer_stations(sj$table, lon, lat, alt)
}

#' Une station proposee, en mots : distance, altitude, grandeurs mesurees, qualite.
#' @param s une ligne de stations_proches_publiees().
#' @param qualite FALSE quand la qualite est deja montree a cote (pastille).
#' @return texte court.
#' @keywords internal
stations_detail <- function(s, qualite = TRUE) {
  d <- mf_decrire_station(s)
  alt <- if (is.finite(s$alt)) {
    sprintf("altitude %s%s", d$altitude, if (is.null(d$ecart_alt)) "" else sprintf(" (%s)", d$ecart_alt))
  } else d$altitude
  paste(c(sprintf("%s du point", d$distance), alt, d$variables,
          if (qualite) sprintf("qualité : %s", tolower(s$libelle))), collapse = " · ")
}


# ------------------------------------------------------------------------------
# 3 bis. Une station designee sur une carte (Accueil, onglet Carte)
# ------------------------------------------------------------------------------

#' La table des stations publiees, pour un jeu de fichiers.
#'
#' Une seule lecture par `CONFIG$comparer$cache_h` heures (cmp_stations garde le fichier en
#' memoire). Sert a deux ecrans qui designent une station sur une carte : le serveur retrouve
#' la position et le nom ICI, il ne les prend jamais au navigateur.
#' @param transport carte_transport() ; @param executer requete HTTP (injectable).
#' @return data.frame (stations_table) ou NULL si aucune station n'est publiee ou lisible.
#' @keywords internal
stations_publiees_table <- function(transport, executer = httr2::req_perform) {
  if (!is.list(transport) || identical(transport$mode, "absent") || !isTRUE(CONFIG$stations$actif)) return(NULL)
  sj <- tryCatch(cmp_stations(list(transport = transport), executer = executer), error = function(e) NULL)
  if (is.null(sj) || !isTRUE(sj$ok)) NULL else sj$table
}

#' Le point d'une station designee par son identifiant.
#'
#' L'identifiant est une entree utilisateur comme une autre (clic d'un navigateur : une valeur
#' forgee est possible) : il doit avoir la forme d'un identifiant ET exister dans la table lue par
#' le serveur. Position et nom viennent de la table, jamais du clic.
#' @param id identifiant recu ; @param table stations_table().
#' @return list(lon, lat, nom, source = "station", station = id), ou NULL.
#' @keywords internal
stations_point_depuis_id <- function(id, table) {
  if (!is.data.frame(table) || !nrow(table)) return(NULL)
  if (!is.character(id) || length(id) != 1L || is.na(id) || !grepl("^[A-Za-z0-9]{3,12}$", id)) return(NULL)
  i <- match(id, table$id)
  if (is.na(i)) return(NULL)
  lon <- suppressWarnings(as.numeric(table$lon[i])); lat <- suppressWarnings(as.numeric(table$lat[i]))
  if (!isTRUE(is.finite(lon)) || !isTRUE(is.finite(lat)) ||
      lat < PARTAGE_BORNES$lat[1] || lat > PARTAGE_BORNES$lat[2] ||
      lon < PARTAGE_BORNES$lon[1] || lon > PARTAGE_BORNES$lon[2]) return(NULL)
  list(lon = round(lon, 4), lat = round(lat, 4), nom = as.character(table$nom[i]),
       source = "station", station = id)
}


# ------------------------------------------------------------------------------
# 4. Une station, comme lieu de comparaison
# ------------------------------------------------------------------------------

#' Une station, prete a etre comparee (type « station » : voir cmp_mailles).
#' @param s une ligne de la table des stations (id, nom, lon, lat, alt).
#' @keywords internal
cmp_lieu_station <- function(s, nom = NULL) {
  list(nom = cmp_nom_propre(nom %||% sprintf("%s (mesure)", s$nom), "Station"),
       type = "station", id = as.character(s$id), lon = as.numeric(s$lon), lat = as.numeric(s$lat),
       alt = as.numeric(s$alt %||% NA_real_))
}

#' Nom court d'une source, utilise dans les titres : « Maille SAFRAN (modèle) ».
#' @keywords internal
stations_libelle_maille <- function() "Maille SAFRAN (modèle)"
