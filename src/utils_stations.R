# ==============================================================================
# utils_stations.R — STATIONS METEO-FRANCE (DONNEES CLIMATOLOGIQUES DE BASE)
# ------------------------------------------------------------------------------
# Responsabilite : a partir d'un point, trouver les stations Meteo-France
# voisines, juger leur qualite, puis rendre la serie journaliere de la station
# choisie dans le MEME format que safran_charger() (voir utils_api.R). Les
# indicateurs ne savent donc pas d'ou vient la donnee.
#
# SOURCE : fichiers ouverts de Meteo-France sur data.gouv.fr, un jeu par
# departement et par periode (RR-T-Vent). Pas de cle d'API. Voir
# CONFIG$stations pour le raisonnement complet (pourquoi pas l'API temps reel,
# pourquoi la liste des fichiers n'est pas ecrite en dur).
#
# DEUX NIVEAUX DE QUALITE
# -----------------------
#   1. A LA PROPOSITION (instantane) : distance, ecart d'altitude, fraicheur de
#      la derniere mesure, part de jours valides sur 12 mois. Calcule a partir
#      du seul fichier « latest » du departement, petit et mis en cache.
#   2. A L'ANALYSE (apres chargement de l'historique) : part des annees
#      exploitables sur la periode de reference choisie. Seule cette mesure dit
#      si une normale 1991-2020 est possible sur cette station.
#
# CE QU'UNE STATION NE DONNE PAS
# ------------------------------
# Ni SWI, ni humidite, ni hauteur de neige, et la pluie n'est pas separee de la
# neige : ces colonnes arrivent a NA, et le registre d'indicateurs ecarte les
# indicateurs qui en dependent (champ `requiert`, voir registre_indicateurs.R).
# L'ETP est recalculee par Hargreaves-Samani a partir des temperatures.
#
# CE FICHIER N'A PAS PU ETRE TESTE CONTRE LES SERVEURS REELS au moment de son
# ecriture (acces reseau restreint). Les formats attendus sont verifies par
# tests/test_stations.R sur des fichiers synthetiques au format Meteo-France ;
# le premier essai reel se fait avec diagnostic_stations.R.
# ==============================================================================


.ETAT_MF <- new.env(parent = emptyenv())

# Colonnes lues dans les fichiers Meteo-France. Une colonne absente du fichier
# arrive a NA : le format a deja evolue par le passe.
MF_COLONNES <- c("NUM_POSTE", "NOM_USUEL", "LAT", "LON", "ALTI", "AAAAMMJJ",
                 "RR", "QRR", "TN", "QTN", "TX", "QTX", "TM", "QTM",
                 "TNTXM", "QTNTXM", "FFM", "QFFM")

MF_LIBELLES_NIVEAU <- c("Bonne", "Correcte", "Limitée", "Insuffisante")


# ------------------------------------------------------------------------------
# 0. Utilitaires
# ------------------------------------------------------------------------------

#' Repertoire du cache des stations (cree a la demande, silencieux en cas d'echec).
#' @keywords internal
mf_cache_dir <- function() {
  d <- file.path(CONFIG$cache$repertoire, "stations")
  if (!dir.exists(d)) suppressWarnings(dir.create(d, recursive = TRUE, showWarnings = FALSE))
  d
}

#' Lit une entree du cache des stations (nom de fichier valide uniquement ; le
#' format est controle par cache_lire_table).
#' @keywords internal
mf_cache_lire <- function(nom, ttl_h) {
  if (!cache_est_actif() || !cache_nom_valide(nom)) return(NULL)
  f <- file.path(mf_cache_dir(), nom)
  if (!file.exists(f)) return(NULL)
  age_h <- as.numeric(difftime(Sys.time(), file.mtime(f), units = "hours"))
  if (!is.finite(age_h) || age_h > ttl_h) return(NULL)
  cache_lire_table(f)
}

#' @keywords internal
mf_cache_ecrire <- function(nom, objet) {
  if (!cache_est_actif() || !cache_nom_valide(nom)) return(invisible(FALSE))
  tryCatch({
    cache_ecrire_table(objet, file.path(mf_cache_dir(), nom))
    cache_purger_si_besoin()
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

#' Requete httr2 qui NE leve PAS d'erreur sur un code HTTP >= 400 : l'appelant
#' lit le statut et decide (un message precis vaut mieux qu'une trace).
#' @keywords internal
mf_requete <- function(url, timeout = 60, query = NULL) {
  req <- httr2::request(url)
  if (length(query)) {
    req <- do.call(httr2::req_url_query, c(list(.req = req), query))
  }
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_user_agent(req, sprintf(
    "%s/%s (%s)", CONFIG$app$nom, CONFIG$app$version, CONFIG$app$organisme))
  req <- httr2::req_retry(req, max_tries = 2)
  httr2::req_error(req, is_error = function(resp) FALSE)
}

#' Distance orthodromique en km (formule de haversine), vectorisee.
#' @export
mf_distance_km <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * 6371.0088 * asin(pmin(1, sqrt(a)))
}

#' Premiere capture d'un motif regex pour chaque element, NA sinon.
#' @keywords internal
mf_capture <- function(x, motif) {
  m <- regmatches(x, regexec(motif, x, perl = TRUE))
  vapply(m, function(z) if (length(z) >= 2L) z[2L] else NA_character_, character(1))
}


# ------------------------------------------------------------------------------
# 1. Liste des fichiers publies (data.gouv.fr)
# ------------------------------------------------------------------------------

#' Convertit la liste de ressources data.gouv.fr en tableau de fichiers.
#'
#' Le titre d'une ressource ressemble a « ...departement_31_periode_1950-2024_
#' RR-T-Vent » et le fichier a « Q_31_previous-1950-2024_RR-T-Vent.csv.gz ».
#' On accepte les deux ecritures, dans le titre comme dans l'adresse, plutot
#' que de parier sur l'une.
#' @param res liste de ressources (title, url, last_modified).
#' @keywords internal
mf_parser_ressources <- function(res) {
  vide <- data.frame(dept = character(0), debut = integer(0), fin = integer(0),
                     type = character(0), titre = character(0), url = character(0),
                     modifie = character(0), stringsAsFactors = FALSE)
  if (!length(res)) return(vide)

  champ_txt <- function(r, nom) {
    v <- r[[nom]]
    if (is.null(v) || !length(v)) "" else as.character(v[[1]])
  }
  titre   <- vapply(res, champ_txt, character(1), nom = "title")
  url     <- vapply(res, champ_txt, character(1), nom = "url")
  modifie <- vapply(res, champ_txt, character(1), nom = "last_modified")
  fichier <- basename(sub("[?#].*$", "", url))
  texte   <- paste(titre, fichier)

  ok <- grepl("RR-T-Vent", texte, fixed = TRUE) & nzchar(url)
  if (!any(ok)) return(vide)
  titre <- titre[ok]; url <- url[ok]; modifie <- modifie[ok]
  fichier <- fichier[ok]; texte <- texte[ok]

  dept <- mf_capture(titre, "departement_([0-9]{2,3}|2[AB])(?:_|\\b)")
  dept2 <- mf_capture(fichier, "^Q_([0-9]{2,3}|2[AB])_")
  dept[is.na(dept)] <- dept2[is.na(dept)]

  an1 <- suppressWarnings(as.integer(mf_capture(texte, "(\\d{4})-\\d{4}")))
  an2 <- suppressWarnings(as.integer(mf_capture(texte, "\\d{4}-(\\d{4})")))
  type <- ifelse(grepl("latest", texte, ignore.case = TRUE), "latest", "previous")

  out <- data.frame(dept = dept, debut = an1, fin = an2, type = type,
                    titre = titre, url = url, modifie = modifie,
                    stringsAsFactors = FALSE)
  out[!is.na(out$dept) & !is.na(out$debut) & !is.na(out$fin), , drop = FALSE]
}

#' Ressources via l'API v2 de data.gouv.fr (paginee).
#' @keywords internal
mf_ressources_v2 <- function() {
  url <- sprintf("%s/%s/resources/", CONFIG$stations$api_v2, CONFIG$stations$dataset_id)
  sortie <- list(); page <- 1L
  repeat {
    rep <- httr2::req_perform(mf_requete(
      url, timeout = 40, query = list(page = page, page_size = 100, type = "main")))
    if (httr2::resp_status(rep) >= 400) {
      stop(sprintf("API data.gouv.fr v2 : HTTP %d", httr2::resp_status(rep)))
    }
    j <- httr2::resp_body_json(rep, simplifyVector = FALSE)
    if (!length(j$data)) break
    sortie <- c(sortie, j$data)
    suivante <- j$next_page
    if (is.null(suivante) || !nzchar(as.character(suivante)[1]) || page >= 80L) break
    page <- page + 1L
  }
  sortie
}

#' Ressources via l'API v1 (le jeu de donnees complet, en un appel).
#' @keywords internal
mf_ressources_v1 <- function() {
  url <- sprintf("%s/%s/", CONFIG$stations$api_v1, CONFIG$stations$dataset_id)
  rep <- httr2::req_perform(mf_requete(url, timeout = 60))
  if (httr2::resp_status(rep) >= 400) {
    stop(sprintf("API data.gouv.fr v1 : HTTP %d", httr2::resp_status(rep)))
  }
  httr2::resp_body_json(rep, simplifyVector = FALSE)$resources
}

#' Liste des fichiers « RR-T-Vent » disponibles, un par departement et periode.
#'
#' Mise en cache 24 h (memoire + disque). En cas d'echec des deux API, on
#' renvoie un tableau vide ; `mf_fichier_secours()` prend alors le relais.
#' @return data.frame(dept, debut, fin, type, titre, url, modifie), avec
#'   l'attribut "erreurs" (chaine) si quelque chose a echoue.
#' @export
mf_index_fichiers <- function(forcer = FALSE) {
  ttl <- CONFIG$stations$cache_index_h
  if (!forcer) {
    m <- .ETAT_MF$index
    if (!is.null(m) && as.numeric(difftime(Sys.time(), m$t, units = "hours")) < ttl) {
      return(m$valeur)
    }
    d <- mf_cache_lire(paste0("index_fichiers", CACHE_EXT), ttl)
    if (!is.null(d) && nrow(d)) {
      .ETAT_MF$index <- list(t = Sys.time(), valeur = d)
      return(d)
    }
  }

  erreurs <- character(0)
  idx <- NULL
  for (methode in list(mf_ressources_v2, mf_ressources_v1)) {
    res <- tryCatch(methode(), error = function(e) {
      erreurs <<- c(erreurs, conditionMessage(e)); NULL })
    if (!is.null(res)) {
      idx <- mf_parser_ressources(res)
      if (nrow(idx)) break
      erreurs <- c(erreurs, "liste de fichiers recue mais aucun fichier RR-T-Vent reconnu")
    }
  }

  if (is.null(idx) || !nrow(idx)) {
    idx <- mf_parser_ressources(list())
    attr(idx, "erreurs") <- paste(
      "Liste des fichiers Météo-France indisponible :", paste(unique(erreurs), collapse = " ; "))
    return(idx)
  }

  .ETAT_MF$index <- list(t = Sys.time(), valeur = idx)
  mf_cache_ecrire(paste0("index_fichiers", CACHE_EXT), idx)
  idx
}

#' Fichiers du departement qui couvrent des annees donnees, repli compris.
#'
#' @param type "latest", "previous" ou NULL (les deux).
#' @return data.frame comme mf_index_fichiers(), avec l'attribut "secours" TRUE
#'   si les adresses viennent du miroir de secours (peut-etre perime).
#' @keywords internal
mf_fichiers_dept <- function(dept, annees = NULL, type = NULL) {
  idx <- mf_index_fichiers()
  f <- idx[idx$dept == dept, , drop = FALSE]
  secours <- FALSE

  if (!nrow(f)) {
    # Dernier recours : adresses du miroir, selon la denomination connue au
    # moment de l'ecriture. Peut etre perime (le miroir a cesse d'etre mis a
    # jour pour les donnees SYNOP) : on le signale a l'utilisateur.
    s <- CONFIG$stations$secours
    if (!is.null(s)) {
      secours <- TRUE
      mk <- function(t, a, b) data.frame(
        dept = dept, debut = a, fin = b, type = t,
        titre = sprintf("Q_%s_%s-%d-%d_RR-T-Vent", dept, t, a, b),
        url = sprintf("%s/Q_%s_%s-%d-%d_RR-T-Vent.csv.gz",
                      CONFIG$stations$miroir, dept, t, a, b),
        modifie = "", stringsAsFactors = FALSE)
      f <- rbind(mk("previous", s$previous[1], s$previous[2]),
                 mk("latest",   s$latest[1],   s$latest[2]))
    }
  }

  if (!is.null(type)) f <- f[f$type %in% type, , drop = FALSE]
  if (!is.null(annees) && nrow(f)) {
    f <- f[f$fin >= min(annees) & f$debut <= max(annees), , drop = FALSE]
  }
  attr(f, "secours") <- secours
  f
}


# ------------------------------------------------------------------------------
# 2. Telechargement et lecture d'un fichier departemental
# ------------------------------------------------------------------------------

#' L'adresse peut-elle etre telechargee ? (audit V6)
#'
#' Les adresses des fichiers viennent de la reponse du catalogue data.gouv.fr :
#' c'est une donnee distante, pas une constante du code. On n'accepte que
#' HTTPS, sans identifiants dans l'adresse, sur le port par defaut, vers un
#' domaine de `CONFIG$stations$hotes_autorises` ou l'un de ses sous-domaines.
#' La comparaison porte sur le NOM DE DOMAINE ENTIER : « data.gouv.fr.autre.com »,
#' « mondata.gouv.fr » et « data.gouv.fr@autre.com » sont refusees.
#'
#' @param url adresse (une seule).
#' @param autorises domaines acceptes.
#' @return TRUE ou FALSE ; ne leve jamais d'erreur.
#' @keywords internal
mf_url_autorisee <- function(url, autorises = CONFIG$stations$hotes_autorises) {
  url <- as.character(url)[1]
  if (is.na(url) || !nzchar(url) || nchar(url) > 2000L) return(FALSE)
  if (grepl("[[:space:][:cntrl:]\\\\]", url)) return(FALSE)
  u <- tryCatch(httr2::url_parse(url), error = function(e) NULL)
  if (is.null(u)) return(FALSE)
  hote <- tolower(as.character(u$hostname %||% ""))[1]
  if (is.na(hote) || !nzchar(hote)) return(FALSE)
  autorises <- tolower(as.character(autorises))
  autorises <- autorises[!is.na(autorises) & nzchar(autorises)]
  if (!length(autorises)) return(FALSE)
  port <- u$port
  isTRUE(identical(tolower(as.character(u$scheme %||% "")), "https")) &&
    is.null(u$username) && is.null(u$password) &&
    (is.null(port) || identical(as.character(port), "443")) &&
    any(hote == autorises | endsWith(hote, paste0(".", autorises)))
}

#' Adresse raccourcie et sans caracteres de controle, pour un message d'erreur.
#' @keywords internal
mf_url_pour_message <- function(url) {
  url <- gsub("[[:cntrl:]]", "", as.character(url)[1])
  if (is.na(url)) url <- "(vide)"
  if (nchar(url) > 100L) url <- paste0(substr(url, 1L, 97L), "...")
  url
}

#' Options curl d'un telechargement : HTTPS seulement, redirections bornees,
#' taille plafonnee. `redir_protocols_str` n'existe que dans les curl recents ;
#' sinon l'ancienne option (2 = CURLPROTO_HTTPS).
#' @keywords internal
mf_options_telechargement <- function(taille_max_mo = CONFIG$stations$taille_max_mo) {
  connues <- names(curl::curl_options())
  opts <- list(maxredirs = as.integer(CONFIG$stations$redirections_max %||% 5L))
  if ("redir_protocols_str" %in% connues) opts$redir_protocols_str <- "https"
  else if ("redir_protocols" %in% connues) opts$redir_protocols <- 2L
  if (is.finite(taille_max_mo) && taille_max_mo > 0 && "maxfilesize_large" %in% connues) {
    opts$maxfilesize_large <- as.numeric(taille_max_mo) * 1024^2
  }
  opts
}

#' Controle apres telechargement : adresse FINALE (apres redirections) toujours
#' dans les domaines autorises, taille sous le plafond. Supprime le fichier et
#' leve une erreur sinon. Le plafond curl ne suffit pas seul : il ne s'applique
#' que si le serveur annonce la taille ou si la version de curl le verifie en
#' cours de transfert.
#' @keywords internal
mf_verifier_telechargement <- function(dest, url_finale,
                                       taille_max_mo = CONFIG$stations$taille_max_mo) {
  if (!mf_url_autorisee(url_finale)) {
    unlink(dest)
    stop(sprintf("redirection vers un hôte non autorisé : %s", mf_url_pour_message(url_finale)))
  }
  taille <- suppressWarnings(file.size(dest))
  if (!is.finite(taille)) {
    unlink(dest)
    stop("téléchargement introuvable après réception")
  }
  if (is.finite(taille_max_mo) && taille_max_mo > 0 && taille > taille_max_mo * 1024^2) {
    unlink(dest)
    stop(sprintf("fichier trop volumineux (plus de %s Mo)", format(taille_max_mo)))
  }
  invisible(dest)
}

#' Telecharge un fichier dans un emplacement temporaire.
#'
#' Refuse toute adresse hors des domaines autorises (voir mf_url_autorisee),
#' borne les redirections et la taille, et controle l'adresse finale.
#' @return chemin local (a supprimer par l'appelant).
#' @keywords internal
mf_telecharger <- function(url, timeout = 300,
                           taille_max_mo = CONFIG$stations$taille_max_mo) {
  if (!mf_url_autorisee(url)) {
    stop(sprintf("adresse de téléchargement refusée (hôte non autorisé) : %s",
                 mf_url_pour_message(url)))
  }
  dest <- tempfile(fileext = ".csv.gz")
  req <- do.call(httr2::req_options,
                 c(list(mf_requete(url, timeout = timeout)),
                   mf_options_telechargement(taille_max_mo)))
  rep <- tryCatch(httr2::req_perform(req, path = dest),
                  error = function(e) { unlink(dest); stop(e) })
  if (httr2::resp_status(rep) >= 400) {
    unlink(dest)
    stop(sprintf("téléchargement refusé (HTTP %d)", httr2::resp_status(rep)))
  }
  mf_verifier_telechargement(dest, httr2::resp_url(rep), taille_max_mo)
  dest
}

#' Lit un fichier departemental par blocs, en ne gardant que certaines stations.
#'
#' Un fichier « previous » de grand departement fait plusieurs millions de
#' lignes : le lire en entier saturerait l'instance gratuite (~1 Go). On le
#' parcourt par blocs de 100 000 lignes et on ne conserve que celles de la
#' station demandee — pur R de base, aucune dependance supplementaire.
#'
#' Les colonnes sont retrouvees PAR LEUR NOM dans l'en-tete : un changement
#' d'ordre dans le fichier ne casse rien, une colonne absente arrive a NA.
#'
#' @param ids vecteur de numeros de poste (8 caracteres), ou NULL pour tout garder.
#' @return data.frame(id, nom, lat, lon, alt, date, rr, qrr, tn, qtn, tx, qtx,
#'   tm, qtm, tntxm, qtntxm, ffm, qffm)
#' @export
mf_lire_fichier <- function(chemin, ids = NULL) {

  con <- gzfile(chemin, open = "rt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  entete <- readLines(con, n = 1L, warn = FALSE)
  if (!length(entete)) stop("fichier vide")
  entete <- sub("^﻿", "", sub("\r$", "", entete))
  noms <- gsub("\"", "", strsplit(entete, ";", fixed = TRUE)[[1]], fixed = TRUE)
  if (!all(c("NUM_POSTE", "AAAAMMJJ") %in% noms)) {
    stop("colonnes NUM_POSTE / AAAAMMJJ absentes : le fichier n'a pas le format attendu")
  }

  # Seules les colonnes utiles sont lues (les autres : colClasses = "NULL").
  # Trois colonnes de marge absorbent un eventuel « ; » final.
  noms_ext   <- c(noms, paste0("EXTRA", 1:3))
  classes    <- c(ifelse(noms %in% MF_COLONNES, "character", "NULL"), rep("NULL", 3))
  prefixes   <- if (!is.null(ids)) paste0(ids, ";") else NULL

  morceaux <- list(); k <- 0L
  repeat {
    lignes <- readLines(con, n = 100000L, warn = FALSE)
    if (!length(lignes)) break

    if (!all(valide <- validUTF8(lignes))) {
      lignes[!valide] <- iconv(lignes[!valide], "latin1", "UTF-8", sub = "")
    }
    if (!is.null(prefixes)) {
      garde <- Reduce(`|`, lapply(prefixes, function(p) startsWith(lignes, p)))
      lignes <- lignes[garde]
    }
    if (!length(lignes)) next

    df <- utils::read.table(
      text = lignes, sep = ";", header = FALSE, quote = "", comment.char = "",
      col.names = noms_ext, colClasses = classes, na.strings = c("", "NA"),
      fill = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)
    k <- k + 1L
    morceaux[[k]] <- df
  }

  if (!k) return(mf_brut_vide())
  brut <- do.call(rbind, morceaux)

  num <- function(nom) {
    if (nom %in% names(brut)) suppressWarnings(as.numeric(brut[[nom]]))
    else rep(NA_real_, nrow(brut))
  }
  txt <- function(nom) {
    if (nom %in% names(brut)) as.character(brut[[nom]])
    else rep(NA_character_, nrow(brut))
  }
  data.frame(
    id = txt("NUM_POSTE"), nom = txt("NOM_USUEL"),
    lat = num("LAT"), lon = num("LON"), alt = num("ALTI"),
    date = as.Date(txt("AAAAMMJJ"), format = "%Y%m%d"),
    rr = num("RR"), qrr = num("QRR"), tn = num("TN"), qtn = num("QTN"),
    tx = num("TX"), qtx = num("QTX"), tm = num("TM"), qtm = num("QTM"),
    tntxm = num("TNTXM"), qtntxm = num("QTNTXM"),
    ffm = num("FFM"), qffm = num("QFFM"),
    stringsAsFactors = FALSE)
}

#' @keywords internal
mf_brut_vide <- function() {
  data.frame(id = character(0), nom = character(0), lat = numeric(0),
             lon = numeric(0), alt = numeric(0), date = as.Date(character(0)),
             rr = numeric(0), qrr = numeric(0), tn = numeric(0), qtn = numeric(0),
             tx = numeric(0), qtx = numeric(0), tm = numeric(0), qtm = numeric(0),
             tntxm = numeric(0), qtntxm = numeric(0), ffm = numeric(0),
             qffm = numeric(0), stringsAsFactors = FALSE)
}

#' Met a NA les valeurs dont le code qualite est ecarte.
#'
#' GARDE-FOU : la signification exacte des codes n'a pas pu etre verifiee a
#' l'ecriture. Si ecarter les codes configures supprimait plus de la moitie des
#' valeurs d'une variable, on suppose que le code est mal interprete et on ne
#' filtre PAS cette variable (elle est nommee dans l'attribut "non_filtrees").
#' @keywords internal
mf_appliquer_qualite <- function(brut) {
  ecartes <- CONFIG$stations$qualite_ecartee
  non_filtrees <- character(0)
  n_ecartees <- 0L

  for (p in list(c("rr", "qrr"), c("tn", "qtn"), c("tx", "qtx"),
                 c("tm", "qtm"), c("tntxm", "qtntxm"), c("ffm", "qffm"))) {
    v <- brut[[p[1]]]; q <- brut[[p[2]]]
    mauvais <- !is.na(v) & !is.na(q) & q %in% ecartes
    n_val <- sum(!is.na(v))
    if (n_val > 0L && sum(mauvais) / n_val > 0.5) {
      non_filtrees <- c(non_filtrees, toupper(p[1]))
      next
    }
    brut[[p[1]]][mauvais] <- NA_real_
    n_ecartees <- n_ecartees + sum(mauvais)
  }
  attr(brut, "non_filtrees") <- non_filtrees
  attr(brut, "n_ecartees")   <- n_ecartees
  brut
}


# ------------------------------------------------------------------------------
# 3. Stations d'un departement (fichier « latest »)
# ------------------------------------------------------------------------------

#' Une ligne par station : position, altitude, fraicheur et completude recente.
#' @param brut sortie de mf_lire_fichier(ids = NULL), qualite deja appliquee.
#' @keywords internal
mf_resumer_stations <- function(brut, dept) {
  vide <- data.frame(
    id = character(0), nom = character(0), lat = numeric(0), lon = numeric(0),
    alt = numeric(0), derniere_obs = as.Date(character(0)),
    fr_p = numeric(0), fr_t = numeric(0), a_pluie = logical(0),
    a_temp = logical(0), dept = character(0), stringsAsFactors = FALSE)
  if (!nrow(brut)) return(vide)

  ref_date <- max(brut$date, na.rm = TRUE)
  lignes <- lapply(split(brut, brut$id), function(b) {
    b <- b[order(b$date), , drop = FALSE]
    dernier <- b[nrow(b), ]
    ok_p <- !is.na(b$rr)
    ok_t <- !is.na(b$tn) & !is.na(b$tx)
    obs  <- ok_p | ok_t | !is.na(b$tn) | !is.na(b$tx)
    w    <- b$date > (ref_date - 365L)
    data.frame(
      id = dernier$id, nom = dernier$nom, lat = dernier$lat, lon = dernier$lon,
      alt = dernier$alt,
      derniere_obs = if (any(obs)) max(b$date[obs]) else as.Date(NA),
      fr_p = sum(ok_p & w) / 365, fr_t = sum(ok_t & w) / 365,
      a_pluie = any(ok_p), a_temp = any(ok_t),
      dept = dept, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, lignes)
  rownames(out) <- NULL
  out[is.finite(out$lat) & is.finite(out$lon), , drop = FALSE]
}

#' Stations d'un departement ayant mesure depuis le debut du fichier « latest ».
#'
#' Cache 12 h. Les stations fermees avant 2025 n'y figurent pas : elles ne
#' sont de toute facon pas des candidates pour un suivi courant.
#' @return data.frame, avec l'attribut "erreur" (chaine) en cas d'echec.
#' @keywords internal
mf_stations_dept <- function(dept) {
  ttl <- CONFIG$stations$cache_dept_h
  cle <- sprintf("dept_%s%s", cache_nom_sur(dept), CACHE_EXT)

  m <- .ETAT_MF$depts[[dept]]
  if (!is.null(m) && as.numeric(difftime(Sys.time(), m$t, units = "hours")) < ttl) {
    return(m$valeur)
  }
  d <- mf_cache_lire(cle, ttl)
  if (!is.null(d)) {
    if (is.null(.ETAT_MF$depts)) .ETAT_MF$depts <- list()
    .ETAT_MF$depts[[dept]] <- list(t = Sys.time(), valeur = d)
    return(d)
  }

  res <- tryCatch({
    f <- mf_fichiers_dept(dept, type = "latest")
    if (!nrow(f)) stop(sprintf("aucun fichier récent pour le département %s", dept))
    f <- f[which.max(f$fin), , drop = FALSE]
    chemin <- mf_telecharger(f$url)
    on.exit(unlink(chemin), add = TRUE)
    brut <- mf_appliquer_qualite(mf_lire_fichier(chemin))
    st <- mf_resumer_stations(brut, dept)
    attr(st, "secours") <- isTRUE(attr(f, "secours"))
    st
  }, error = function(e) {
    st <- mf_resumer_stations(mf_brut_vide(), dept)
    attr(st, "erreur") <- sprintf("Département %s : %s", dept, conditionMessage(e))
    st
  })

  if (is.null(attr(res, "erreur"))) {
    if (is.null(.ETAT_MF$depts)) .ETAT_MF$depts <- list()
    .ETAT_MF$depts[[dept]] <- list(t = Sys.time(), valeur = res)
    mf_cache_ecrire(cle, res)
  }
  res
}


# ------------------------------------------------------------------------------
# 4. Departements autour d'un point, altitude du point
# ------------------------------------------------------------------------------

#' Codes des departements situes dans un rayon autour du point.
#'
#' Sans table de voisinage a maintenir : on interroge l'API Geo sur le point
#' lui-meme et sur deux anneaux de huit points (20 et 40 km). Les departements
#' rencontres sont ceux dont les stations peuvent etre les plus proches.
#' @return vecteur de codes (le departement du point en premier), ou
#'   character(0) si l'API Geo ne repond pas.
#' @keywords internal
mf_departements_proches <- function(lon, lat) {
  cle <- sprintf("%.1f_%.1f", lon, lat)
  if (!is.null(.ETAT_MF$voisins[[cle]])) return(.ETAT_MF$voisins[[cle]])

  angles <- seq(0, 315, by = 45) * pi / 180
  pts <- data.frame(lon = lon, lat = lat)
  for (r in CONFIG$stations$anneaux_km) {
    pts <- rbind(pts, data.frame(
      lat = lat + r * cos(angles) / 111.32,
      lon = lon + r * sin(angles) / (111.32 * cos(lat * pi / 180))))
  }

  requetes <- lapply(seq_len(nrow(pts)), function(i) {
    mf_requete(CONFIG$api$geo_communes, timeout = 15, query = list(
      lon = round(pts$lon[i], 5), lat = round(pts$lat[i], 5),
      format = "json", fields = "departement"))
  })
  reponses <- tryCatch(
    httr2::req_perform_parallel(requetes, on_error = "continue", progress = FALSE),
    error = function(e) lapply(requetes, function(r) tryCatch(httr2::req_perform(r), error = function(e2) NULL)))

  codes <- vapply(reponses, function(rep) {
    if (inherits(rep, "condition") || is.null(rep)) return(NA_character_)
    if (httr2::resp_status(rep) >= 400) return(NA_character_)
    j <- tryCatch(httr2::resp_body_json(rep, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(j) || !length(j)) return(NA_character_)
    as.character(j[[1]]$departement$code %||% NA_character_)
  }, character(1))

  centre <- codes[1]
  out <- unique(c(centre, codes[!is.na(codes)]))
  out <- out[!is.na(out)]
  if (is.null(.ETAT_MF$voisins)) .ETAT_MF$voisins <- list()
  if (length(out)) .ETAT_MF$voisins[[cle]] <- out
  out
}

#' Altitude du point (m), via le service d'altimetrie de la Geoplateforme IGN.
#' Facultative : NA en cas d'echec, et le critere d'altitude est alors ignore.
#' @export
mf_altitude <- function(lon, lat) {
  tryCatch({
    rep <- httr2::req_perform(mf_requete(
      CONFIG$stations$api_altitude, timeout = 12,
      query = list(lon = round(lon, 5), lat = round(lat, 5),
                   resource = "ign_rge_alti_wld", zonly = "true")))
    if (httr2::resp_status(rep) >= 400) return(NA_real_)
    j <- httr2::resp_body_json(rep, simplifyVector = FALSE)
    z <- j$elevations[[1]]
    if (is.list(z)) z <- z$z
    z <- suppressWarnings(as.numeric(z))
    if (!length(z) || !is.finite(z) || z < -400) NA_real_ else z
  }, error = function(e) NA_real_)
}


# ------------------------------------------------------------------------------
# 5. Qualite a la proposition (niveau 1)
# ------------------------------------------------------------------------------

#' Niveau 1 (bonne) a 4 (insuffisante) d'une valeur selon trois seuils.
#' @param plus_petit_mieux TRUE pour une distance ou un age, FALSE pour une part.
#' @keywords internal
mf_niveau <- function(x, seuils, plus_petit_mieux = TRUE) {
  n <- rep(NA_integer_, length(x))
  ok <- is.finite(x)
  if (plus_petit_mieux) {
    n[ok] <- 1L + (x[ok] > seuils[1]) + (x[ok] > seuils[2]) + (x[ok] > seuils[3])
  } else {
    n[ok] <- 1L + (x[ok] < seuils[1]) + (x[ok] < seuils[2]) + (x[ok] < seuils[3])
  }
  as.integer(n)
}

#' Evalue chaque station proposee et ajoute les colonnes de qualite.
#'
#' Le niveau global est le PIRE des criteres disponibles. Une station qui ne
#' mesure ni la pluie ni les temperatures est insuffisante ; une station qui
#' n'en mesure qu'une des deux est au mieux « limitee » — la moitie des
#' indicateurs lui serait inaccessible.
#' @param st data.frame de stations avec distance_km, ecart_alt_m, derniere_obs,
#'   fr_p, fr_t, a_pluie, a_temp.
#' @return st + colonnes niv_distance, niv_altitude, niv_fraicheur, niv_donnees,
#'   niveau (1-4), libelle, age_j.
#' @export
mf_qualite_stations <- function(st) {
  if (!nrow(st)) return(st)
  s <- CONFIG$stations$seuils

  st$age_j <- as.numeric(Sys.Date() - st$derniere_obs)
  st$niv_distance <- mf_niveau(st$distance_km, s$distance_km)
  st$niv_altitude <- mf_niveau(st$ecart_alt_m, s$ecart_alt_m)
  st$niv_fraicheur <- mf_niveau(st$age_j, s$fraicheur_j)
  st$niv_fraicheur[is.na(st$niv_fraicheur)] <- 4L

  # Completude : la plus mauvaise des variables que la station mesure
  comp <- pmin(ifelse(st$a_pluie, st$fr_p, 1), ifelse(st$a_temp, st$fr_t, 1))
  st$niv_donnees <- mf_niveau(comp, s$completude, plus_petit_mieux = FALSE)
  st$niv_donnees[!st$a_pluie & !st$a_temp] <- 4L
  # Une seule des deux familles de variables : au mieux « limitee »
  une_seule <- xor(st$a_pluie, st$a_temp)
  st$niv_donnees[une_seule] <- pmax(st$niv_donnees[une_seule], 3L, na.rm = TRUE)

  st$niveau <- as.integer(pmax(st$niv_distance, st$niv_altitude, st$niv_fraicheur,
                               st$niv_donnees, na.rm = TRUE))
  st$libelle <- MF_LIBELLES_NIVEAU[st$niveau]
  st
}

#' Classe des stations autour d'un point : distance, ecart d'altitude, qualite, pertinence.
#'
#' Partagee par la recherche en direct (mf_stations_proches) et par la lecture des stations
#' publiees (stations_proches_publiees) : un seul classement, donc les memes propositions
#' d'où que viennent les stations.
#' @param tous data.frame de stations (id, nom, lat, lon, alt, derniere_obs, fr_p, fr_t,
#'   a_pluie, a_temp, dept).
#' @param alt altitude du point (NA : critere ignore) ; @param n nombre de stations gardees.
#' @return stations a moins de `rayon_km`, triees par pertinence, avec distance_km,
#'   ecart_alt_m, les niveaux de qualite et `pertinence` ; vide si aucune n'est assez proche.
#' @export
mf_classer_stations <- function(tous, lon, lat, alt = NA_real_, n = CONFIG$stations$n_propositions) {
  cfg <- CONFIG$stations
  tous$distance_km <- mf_distance_km(lon, lat, tous$lon, tous$lat)
  tous <- tous[tous$distance_km <= cfg$rayon_km, , drop = FALSE]
  if (!nrow(tous)) return(tous)

  tous$ecart_alt_m <- if (is.finite(alt)) abs(tous$alt - alt) else NA_real_
  tous <- mf_qualite_stations(tous)

  alt_pen <- ifelse(is.finite(tous$ecart_alt_m), tous$ecart_alt_m * cfg$poids_alt_km_par_m, 0)
  tous$pertinence <- tous$distance_km + alt_pen +
    ifelse(tous$a_temp, 0, cfg$penalite_sans_t_km) +
    ifelse(tous$a_pluie, 0, cfg$penalite_sans_p_km) +
    ifelse(!is.na(tous$age_j) & tous$age_j > cfg$seuils$fraicheur_j[3],
           cfg$penalite_inactive_km, 0)

  tous <- tous[order(tous$pertinence, tous$distance_km), , drop = FALSE]
  tous <- utils::head(tous, n)
  rownames(tous) <- NULL
  tous
}

#' Stations Meteo-France les plus pertinentes autour d'un point.
#'
#' CLASSEMENT. La distance seule ferait proposer un pluviometre voisin a la
#' place d'une station complete un peu plus loin, ou une station de fond de
#' vallee pour un point d'altitude. Le classement utilise donc une distance
#' corrigee : + poids_alt par metre d'ecart d'altitude, + une penalite si la
#' station ne mesure pas les temperatures ou la pluie, ou si elle semble
#' arretee. La distance reelle reste affichee ; le poids est dans CONFIG.
#'
#' @param alt altitude du point (NA : critere ignore).
#' @return data.frame trie, ou vide ; attribut "erreurs" (vecteur de messages).
#' @export
mf_stations_proches <- function(lon, lat, alt = NA_real_) {
  erreurs <- character(0)

  deps <- mf_departements_proches(lon, lat)
  if (!length(deps)) {
    out <- mf_resumer_stations(mf_brut_vide(), "")
    out$distance_km <- numeric(0)
    attr(out, "erreurs") <- "API Géo indisponible : impossible de déterminer les départements voisins."
    return(out)
  }

  morceaux <- lapply(deps, function(d) {
    st <- mf_stations_dept(d)
    if (!is.null(attr(st, "erreur"))) erreurs <<- c(erreurs, attr(st, "erreur"))
    if (isTRUE(attr(st, "secours"))) {
      erreurs <<- c(erreurs, sprintf(
        "Département %s : fichier du miroir de secours, les données peuvent ne pas être à jour.", d))
    }
    st
  })
  tous <- do.call(rbind, morceaux)
  if (is.null(tous) || !nrow(tous)) {
    out <- mf_resumer_stations(mf_brut_vide(), "")
    out$distance_km <- numeric(0)
    attr(out, "erreurs") <- erreurs
    return(out)
  }
  tous <- tous[!duplicated(tous$id), , drop = FALSE]

  tous <- mf_classer_stations(tous, lon, lat, alt)
  attr(tous, "erreurs") <- erreurs
  tous
}

#' Distance en kilometres, lisible : « < 1 km » plutot que « 0 km ».
#' @keywords internal
mf_km <- function(x) ifelse(is.finite(x) & x < 1, "< 1 km", ifelse(is.finite(x), sprintf("%.0f km", x), "? km"))

#' Description courte d'une station, pour l'interface.
#' @keywords internal
mf_decrire_station <- function(s) {
  var <- c(if (isTRUE(s$a_pluie)) "pluie", if (isTRUE(s$a_temp)) "températures")
  var <- if (length(var)) paste(var, collapse = " + ") else "aucune mesure exploitable"
  list(
    titre = s$nom,
    distance = mf_km(s$distance_km),
    altitude = if (is.finite(s$alt)) sprintf("%.0f m", s$alt) else "altitude inconnue",
    ecart_alt = if (is.finite(s$ecart_alt_m)) sprintf("%.0f m d'écart", s$ecart_alt_m) else NULL,
    variables = var,
    fraicheur = if (is.finite(s$age_j))
      sprintf("dernière mesure : %s", format(s$derniere_obs, "%d/%m/%Y")) else "aucune mesure récente",
    completude = sprintf("%s%s",
      if (isTRUE(s$a_pluie)) sprintf("pluie %.0f %%", 100 * s$fr_p) else "",
      if (isTRUE(s$a_temp)) sprintf("%sT %.0f %%", if (isTRUE(s$a_pluie)) " · " else "", 100 * s$fr_t) else ""))
}


# ------------------------------------------------------------------------------
# 6. Serie d'une station -> tableau journalier ClimaFor
# ------------------------------------------------------------------------------

#' Nombre de jours d'une annee, en s'arretant a une date pour l'annee en cours.
#' @keywords internal
mf_jours_attendus <- function(annees, derniere) {
  an_der <- as.integer(format(derniere, "%Y"))
  bissextile <- (annees %% 4 == 0 & annees %% 100 != 0) | annees %% 400 == 0
  complet <- ifelse(bissextile, 366L, 365L)
  partiel <- as.integer(derniere - as.Date(sprintf("%d-01-01", annees))) + 1L
  ifelse(annees < an_der, complet, pmin(complet, pmax(partiel, 1L)))
}

#' Transforme les lignes brutes d'UNE station en tableau journalier ClimaFor.
#'
#' - pluie : RR devient p_tot ; p_liq et p_sol restent NA (la station ne
#'   separe pas pluie et neige) ;
#' - temperatures : TN, TX ; la moyenne est (TN + TX) / 2 sur TOUTE la serie
#'   (definition homogene, voir CONFIG$stations$definition_tmoy) ;
#' - vent : FFM, vent moyen journalier a 10 m ;
#' - ETP : Hargreaves-Samani (aucune ETP mesuree dans ces fichiers) ;
#' - SWI, hauteur de neige, humidite : NA.
#'
#' LE CALENDRIER EST COMPLET de la premiere a la derniere date observee : les
#' jours sans mesure sont des lignes a NA, de sorte que completude() (utils_calc)
#' ne confonde pas « jour manquant » et « jour absent du tableau ».
#'
#' ANNEES VALIDES. Pour chaque groupe de variables (p, t, ff), une annee est
#' valide si au moins CONFIG$stations$seuil_annee de ses jours sont renseignes.
#' La liste est posee en attribut "annees_valides" ; les indicateurs ne voient
#' que leurs annees valides (voir donnees_pour_indicateur). Une annee mal
#' renseignee est donc ABSENTE d'un indicateur, jamais comptee comme zero.
#'
#' @param brut sortie de mf_lire_fichier() pour une station, plusieurs fichiers
#'   eventuellement concatenes.
#' @export
mf_normaliser <- function(brut) {
  brut <- brut[!duplicated(brut[, c("id", "date")]), , drop = FALSE]
  brut <- brut[!is.na(brut$date), , drop = FALSE]
  if (!nrow(brut)) return(safran_tableau_vide())

  brut <- mf_appliquer_qualite(brut)
  non_filtrees <- attr(brut, "non_filtrees")
  n_ecartees <- attr(brut, "n_ecartees")
  brut <- brut[order(brut$date), , drop = FALSE]

  # -- Temperature moyenne : UNE definition sur toute la serie ---------------
  # Les fichiers portent TM (moyenne des valeurs horaires, souvent presente
  # seulement sur la partie recente de la serie), TNTXM et (TN + TX) / 2. Les
  # prendre dans cet ordre jour par jour (v11) fait changer la definition de la
  # temperature moyenne au milieu de la serie ; or (TN + TX) / 2 s'ecarte de la
  # vraie moyenne de plusieurs dixiemes de degre, et ce biais n'est pas le meme
  # selon la saison : le changement de definition produit une pente fictive.
  #
  # Par defaut ("tntx") la moyenne est (TN + TX) / 2 PARTOUT — TNTXM est la meme
  # grandeur calculee par Meteo-France, on l'accepte quand TN ou TX manque. TM
  # n'est utilisee que si la station ne mesure ni TN ni TX. Reglage :
  # CONFIG$stations$definition_tmoy.
  tntx <- (brut$tn + brut$tx) / 2
  tntx <- ifelse(is.na(tntx), brut$tntxm, tntx)
  if (identical(CONFIG$stations$definition_tmoy, "tm")) {
    tm <- brut$tm
    tm <- ifelse(is.na(tm), tntx, tm)
    definition_tmoy <- "TM, sinon (TN + TX) / 2 (definition variable dans le temps)"
  } else if (any(!is.na(tntx))) {
    tm <- tntx
    definition_tmoy <- "(TN + TX) / 2 sur toute la serie"
  } else {
    tm <- brut$tm
    definition_tmoy <- "TM (la station ne mesure ni TN ni TX)"
  }

  mesure <- !is.na(brut$rr) | !is.na(brut$tn) | !is.na(brut$tx) | !is.na(brut$ffm)
  if (!any(mesure)) return(safran_tableau_vide())
  premiere <- min(brut$date[mesure]); derniere <- max(brut$date[mesure])
  # Calendrier complet, de janvier de la premiere annee a la derniere mesure
  debut <- as.Date(sprintf("%s-01-01", format(premiere, "%Y")))
  dates <- seq(debut, derniere, by = "day")
  pos <- match(dates, brut$date)
  pre <- function(v) v[pos]

  df <- data.frame(date = dates, stringsAsFactors = FALSE)
  lt <- as.POSIXlt(df$date)
  df$annee <- lt$year + 1900L
  df$mois  <- lt$mon + 1L
  df$jour  <- lt$mday
  df$doy   <- lt$yday + 1L
  df$t_moy <- pre(tm)
  df$t_min <- pre(brut$tn)
  df$t_max <- pre(brut$tx)
  df$p_liq <- NA_real_
  df$p_sol <- NA_real_
  df$p_tot <- pre(brut$rr)
  df$etp   <- NA_real_
  df$swi   <- NA_real_
  df$neig_h <- NA_real_
  df$neig_e <- NA_real_
  df$ff    <- pre(brut$ffm)
  df$hu    <- NA_real_
  df$vpd   <- NA_real_

  # -- Validite annuelle par groupe de variables -----------------------------
  annees <- sort(unique(df$annee))
  attendu <- mf_jours_attendus(annees, derniere)
  frac <- function(ok) {
    n <- tapply(ok, df$annee, sum)
    as.numeric(n[as.character(annees)]) / attendu
  }
  q <- data.frame(
    annee = annees,
    frac_p  = frac(!is.na(df$p_tot)),
    frac_t  = frac(!is.na(df$t_min) & !is.na(df$t_max)),
    frac_ff = frac(!is.na(df$ff)))
  seuil <- CONFIG$stations$seuil_annee
  valides <- list(
    p  = q$annee[q$frac_p  >= seuil],
    t  = q$annee[q$frac_t  >= seuil],
    ff = q$annee[q$frac_ff >= seuil])

  attr(df, "annees_valides")    <- valides
  attr(df, "definition_tmoy")   <- definition_tmoy
  attr(df, "qualite_annuelle")  <- q
  attr(df, "qualite_non_filtree") <- non_filtrees
  attr(df, "n_valeurs_ecartees") <- n_ecartees
  attr(df, "derniere_mesure")   <- derniere
  df
}

#' Tableau journalier ClimaFor d'une station, a partir de ses lignes brutes.
#'
#' Tout ce que station_charger() fait apres la lecture des fichiers : calendrier complet,
#' annees valides, ETP de Hargreaves-Samani, attributs de source. Separee pour que le
#' precalcul des stations publiees (outils/stations_precalcul.R) passe par EXACTEMENT les
#' memes lignes de code que l'analyse d'une station : pas deux manieres de lire une station.
#'
#' @param brut lignes brutes de la station (mf_lire_fichier), eventuellement de plusieurs fichiers.
#' @param station ligne decrivant la station (id, nom, dept, lat, lon, alt[, distance_km]).
#' @param lon,lat point d'analyse (conserve dans l'attribut "point").
#' @param annees annees conservees ; @param erreurs messages deja recueillis.
#' @return tableau journalier, ou tableau vide portant l'attribut "erreurs".
#' @keywords internal
station_depuis_brut <- function(brut, station, lon, lat, annees, erreurs = character(0)) {
  annees <- sort(unique(as.integer(annees)))
  vide <- function() {
    v <- safran_tableau_vide()
    attr(v, "erreurs") <- erreurs
    attr(v, "source") <- "station"
    v
  }
  df <- mf_normaliser(brut)
  if (!nrow(df)) {
    erreurs <- c(erreurs, "La station n'a aucune valeur exploitable sur la période demandée.")
    return(vide())
  }

  # ETP de Hargreaves-Samani : aucune ETP mesuree dans ces fichiers
  df$etp <- etp_hargreaves(df$t_moy, df$t_min, df$t_max, df$doy, station$lat)

  # Les attributs de qualite sont poses par mf_normaliser(). Un sous-ensemble de
  # lignes ne les conserve pas de facon fiable : on les releve avant, on les
  # repose apres.
  garder <- c("annees_valides", "qualite_annuelle", "qualite_non_filtree",
              "n_valeurs_ecartees", "derniere_mesure", "definition_tmoy")
  attrs <- attributes(df)[garder]

  nf <- attr(df, "qualite_non_filtree")
  if (length(nf)) {
    erreurs <- c(erreurs, sprintf(paste(
      "Codes qualité Météo-France non appliqués pour %s : ils auraient écarté plus de",
      "la moitié des valeurs, ce qui laisse penser que leur signification diffère de",
      "celle supposée. À vérifier (CONFIG$stations$qualite_ecartee)."),
      paste(nf, collapse = ", ")))
  }
  ne <- attr(df, "n_valeurs_ecartees")
  if (isTRUE(ne > 0)) {
    erreurs <- c(erreurs, sprintf("%s valeur(s) écartée(s) par le contrôle qualité Météo-France.",
                                  format(ne, big.mark = " ")))
  }

  # Annees hors de la plage demandee : on ne les garde pas
  df <- df[df$annee %in% annees, , drop = FALSE]
  rownames(df) <- NULL
  if (!nrow(df)) {
    erreurs <- c(erreurs, sprintf("La station n'a pas de mesure sur %d-%d.", min(annees), max(annees)))
    return(vide())
  }

  # Les annees hors de la plage demandee ont ete retirees des lignes : on les
  # retire aussi des listes d'annees valides, pour que tout reste coherent.
  attrs$annees_valides <- lapply(attrs$annees_valides, function(v) v[v %in% annees])
  attrs$qualite_annuelle <- attrs$qualite_annuelle[attrs$qualite_annuelle$annee %in% annees, , drop = FALSE]
  for (nom in names(attrs)) attr(df, nom) <- attrs[[nom]]
  attr(df, "etp_source") <- "Hargreaves-Samani (station : pas d'ETP mesurée)"
  attr(df, "erreurs")  <- erreurs
  attr(df, "source")   <- "station"
  attr(df, "station")  <- list(
    id = station$id, nom = station$nom, dept = station$dept,
    lat = station$lat, lon = station$lon, alt = station$alt,
    distance_km = station$distance_km %||% NA_real_)
  attr(df, "point")    <- c(lon = lon, lat = lat)
  attr(df, "extrait")  <- Sys.time()
  df
}

#' Charge la serie d'une station et la rend au format du tableau ClimaFor.
#'
#' Meme contrat de sortie que safran_charger() : data.frame journalier, ou
#' data.frame vide dont l'attribut "erreurs" explique pourquoi. Attributs
#' ajoutes : "source" = "station", "station" (description), "annees_valides",
#' "qualite_annuelle", "derniere_mesure".
#'
#' @param station une ligne de mf_stations_proches() (id, nom, lat, lon, alt, dept...).
#' @param lon,lat point d'analyse (conserve dans l'attribut "point").
#' @param annees annees souhaitees.
#' @export
station_charger <- function(station, lon, lat, annees, progres = NULL) {
  notifier <- function(f, m) if (is.function(progres)) progres(f, m)
  erreurs <- character(0)
  annees <- sort(unique(as.integer(annees)))
  vide <- function() {
    v <- safran_tableau_vide()
    attr(v, "erreurs") <- erreurs
    attr(v, "source") <- "station"
    v
  }

  notifier(0.03, "Liste des fichiers Météo-France...")
  fichiers <- mf_fichiers_dept(station$dept, annees = annees)
  if (isTRUE(attr(fichiers, "secours"))) {
    erreurs <- c(erreurs, paste(
      "Liste des fichiers Météo-France indisponible : adresses du miroir de",
      "secours utilisées, les données peuvent ne pas être à jour."))
  }
  if (!nrow(fichiers)) {
    erreurs <- c(erreurs, sprintf("Aucun fichier Météo-France pour le département %s.", station$dept))
    return(vide())
  }
  # Les plus anciens d'abord
  fichiers <- fichiers[order(fichiers$debut), , drop = FALSE]

  morceaux <- list()
  for (i in seq_len(nrow(fichiers))) {
    f <- fichiers[i, ]
    notifier(0.05 + 0.85 * (i - 1) / nrow(fichiers),
             sprintf("Lecture %s %d-%d...", f$type, f$debut, f$fin))

    cle <- sprintf("serie_%s_%s_%s_%s%s", cache_nom_sur(station$id), cache_nom_sur(f$type),
                   cache_nom_sur(as.integer(f$debut)), cache_nom_sur(as.integer(f$fin)), CACHE_EXT)
    ttl_h <- if (identical(f$type, "latest")) CONFIG$stations$cache_dept_h
             else CONFIG$stations$cache_serie_j * 24
    brut <- mf_cache_lire(cle, ttl_h)

    if (is.null(brut)) {
      brut <- tryCatch({
        chemin <- mf_telecharger(f$url)
        tryCatch(mf_lire_fichier(chemin, ids = station$id), finally = unlink(chemin))
      }, error = function(e) {
        erreurs <<- c(erreurs, sprintf("%d-%d : %s", f$debut, f$fin, conditionMessage(e)))
        NULL
      })
      if (!is.null(brut)) mf_cache_ecrire(cle, brut)
    }
    if (!is.null(brut) && nrow(brut)) morceaux[[length(morceaux) + 1L]] <- brut
  }

  if (!length(morceaux)) {
    erreurs <- c(erreurs, sprintf("Aucune mesure pour la station %s (%s).", station$nom, station$id))
    return(vide())
  }

  notifier(0.93, "Mise en forme de la série...")
  df <- station_depuis_brut(do.call(rbind, morceaux), station, lon, lat, annees, erreurs)
  if (!nrow(df)) return(df)
  notifier(1, "Terminé")
  df
}


# ------------------------------------------------------------------------------
# 7. Qualite de la SERIE (niveau 2) — une fois les donnees chargees
# ------------------------------------------------------------------------------

#' Qualite de la serie chargee, au regard de la periode de reference choisie.
#'
#' Le niveau 1 (mf_qualite_stations) juge la station AVANT le chargement :
#' distance, altitude, fraicheur, completude sur 12 mois. Il ne peut pas dire si
#' la station couvre la periode de reference choisie. Cette fonction le dit, une
#' fois la serie chargee : pour la pluie et pour les temperatures, quelle part
#' des annees de reference est valide (>= CONFIG$stations$seuil_annee de jours
#' renseignes), quelles annees ont ete ecartees, d'ou la serie commence.
#'
#' Le niveau global est le pire des deux groupes MESURES ; une station qui n'en
#' mesure qu'un est au mieux « limitee » (la moitie des indicateurs manque).
#'
#' @param don sortie de station_charger().
#' @param ctx contexte de calcul (ctx_creer).
#' @return NULL si `don` n'est pas une serie de station ; sinon une liste :
#'   niveau, libelle, groupes (data.frame), alertes (character), derniere_mesure,
#'   age_j.
#' @export
mf_qualite_serie <- function(don, ctx) {
  valides <- attr(don, "annees_valides")
  q <- attr(don, "qualite_annuelle")
  if (!identical(attr(don, "source"), "station") || is.null(valides) ||
      is.null(q) || !nrow(don)) return(NULL)

  seuils <- CONFIG$stations$seuils$annees_ref
  ref <- ctx$ref
  vus <- union(ctx$ref, ctx$affichage)

  une <- function(cle, etiquette) {
    v <- valides[[cle]]
    mesure <- length(v) > 0
    dans_ref <- intersect(v, ref)
    part <- if (length(ref)) length(dans_ref) / length(ref) else NA_real_
    data.frame(
      groupe = cle, etiquette = etiquette, mesure = mesure,
      premiere = if (mesure) min(v) else NA_integer_,
      derniere = if (mesure) max(v) else NA_integer_,
      n_ref_valides = length(dans_ref), n_ref = length(ref),
      part_ref = part,
      niveau = if (mesure) mf_niveau(part, seuils, plus_petit_mieux = FALSE) else NA_integer_,
      stringsAsFactors = FALSE)
  }
  g <- rbind(une("p", "Précipitations"), une("t", "Températures"))
  # Annees de la serie ou la variable existe en partie (frac > 0) mais trop
  # lacunaire : ecartees. Une annee a 0 % n'est pas « ecartee », elle est absente.
  ecartees <- list(
    p = setdiff(intersect(q$annee[q$frac_p > 0], vus), valides$p),
    t = setdiff(intersect(q$annee[q$frac_t > 0], vus), valides$t))
  g$ecartees <- I(list(ecartees$p, ecartees$t))

  mesures <- g$niveau[g$mesure]
  niveau <- if (!length(mesures)) 4L else as.integer(max(mesures))
  if (sum(g$mesure) == 1L) niveau <- max(niveau, 3L)

  # Fraicheur : une station arretee reste utilisable sur le passe, mais le dit
  derniere <- attr(don, "derniere_mesure")
  age_j <- if (is.null(derniere)) NA_real_ else as.numeric(Sys.Date() - as.Date(derniere))

  alertes <- character(0)
  for (i in seq_len(nrow(g))) {
    gi <- g[i, ]
    if (!gi$mesure) {
      alertes <- c(alertes, sprintf("%s : non mesurées par cette station.", gi$etiquette))
      next
    }
    if (is.finite(gi$premiere) && gi$premiere > min(ref)) {
      alertes <- c(alertes, sprintf(
        "%s : série valide à partir de %d seulement — la référence %s n'est couverte qu'à %.0f %% (%d ans sur %d).",
        gi$etiquette, gi$premiere, ctx$ref_label, 100 * gi$part_ref, gi$n_ref_valides, gi$n_ref))
    } else if (isTRUE(gi$part_ref < 1)) {
      alertes <- c(alertes, sprintf(
        "%s : %d année(s) sur %d de la référence %s exploitables (%.0f %%).",
        gi$etiquette, gi$n_ref_valides, gi$n_ref, ctx$ref_label, 100 * gi$part_ref))
    }
    ec <- g$ecartees[[i]]
    if (length(ec)) {
      alertes <- c(alertes, sprintf(
        "%s : %s écartée(s) car trop de jours sans mesure (moins de %.0f %% renseignés) — absente(s) des graphiques, pas comptée(s) comme zéro.",
        gi$etiquette, mf_liste_annees(ec), 100 * CONFIG$stations$seuil_annee))
    }
  }
  if (is.finite(age_j) && age_j > CONFIG$stations$seuils$fraicheur_j[1]) {
    alertes <- c(alertes, sprintf("Dernière mesure disponible : %s (il y a %.0f jours).",
                                  format(as.Date(derniere), "%d/%m/%Y"), age_j))
  }
  if (length(attr(don, "qualite_non_filtree")) > 0) {
    alertes <- c(alertes, "Codes qualité Météo-France non appliqués (signification à vérifier).")
  }

  list(niveau = niveau, libelle = MF_LIBELLES_NIVEAU[niveau], groupes = g,
       alertes = alertes, derniere_mesure = derniere, age_j = age_j)
}

#' « 2000 », « 2000 et 2003 », « 2000, 2003 et 2010 », « 2000-2004 et 2010 ».
#' @keywords internal
mf_liste_annees <- function(a) {
  a <- sort(unique(as.integer(a)))
  if (!length(a)) return("")
  # regroupe les suites de 3 annees consecutives ou plus
  suites <- split(a, cumsum(c(1, diff(a) != 1)))
  morceaux <- unname(unlist(lapply(suites, function(x)
    if (length(x) >= 3) sprintf("%d-%d", min(x), max(x)) else as.character(x))))
  if (length(morceaux) == 1) return(morceaux)
  paste0(paste(utils::head(morceaux, -1), collapse = ", "), " et ", utils::tail(morceaux, 1))
}
