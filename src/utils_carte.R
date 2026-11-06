# ==============================================================================
# utils_carte.R — ONGLET CARTE : FICHIERS PUBLIES, VALIDATION, TRANSPORT
# ------------------------------------------------------------------------------
# La carte n'est PAS calculee par l'application. Un precalcul (outils/carte_*.R,
# lance par GitHub Actions) produit, pour chaque maille SAFRAN et chaque annee,
# la valeur des indicateurs annuels (src/carte_couches.R). Le navigateur charge
# ces fichiers et recolore la carte lui-meme : changer d'annee ne passe plus par
# le serveur.
#
# Ce fichier ne contient que ce dont l'APPLICATION a besoin : retrouver ou sont
# les fichiers, verifier qu'ils ont la forme attendue AVANT de les laisser
# piloter le navigateur, et assainir ce que le navigateur renvoie.
#
# FORMAT DES FICHIERS (schema CONFIG$carte$schema)
# ------------------------------------------------------------------------------
#   meta.json        description du jeu (annees, couches, domaines de couleur)
#   grille.json      GeoJSON : une maille = un polygone, `id` = rang 0..n-1
#   c_<cle>.bin.gz   une couche, gzip d'un tampon binaire LITTLE-ENDIAN :
#                      normale  Float32 [n]
#                      pente    Float32 [n]   unite par decennie
#                      signif   Uint8   [n]   1 si la tendance est significative
#                      (remplissage a un multiple de 4 octets)
#                      valeurs  Int16   [n_annees * n], annee par annee ;
#                               valeur stockee = valeur * 10^stock ;
#                               -32768 = absente
#
# SECURITE. meta.json vient d'un hote tiers : le navigateur ne construit une
# adresse qu'a partir d'un NOM DE FICHIER qui passe carte_nom_fichier_sur(), et
# insere les textes dans la page sans jamais les interpreter comme du HTML.
# ==============================================================================

#' Nom de fichier publiable : minuscules, chiffres, soulignements, une extension
#' connue. Aucun separateur de chemin, aucun point double.
#' @keywords internal
carte_nom_fichier_sur <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^[a-z0-9_]{1,60}\\.(json|bin\\.gz)$", x)
}

#' L'adresse de base des fichiers est-elle acceptable ? https, sans identifiants.
#' @keywords internal
carte_url_sure <- function(url) {
  is.character(url) && length(url) == 1L && !is.na(url) &&
    grepl("^https://[A-Za-z0-9.-]+(:[0-9]+)?(/[A-Za-z0-9._~%/-]*)?$", url) &&
    !grepl("@", url, fixed = TRUE) && !grepl("..", url, fixed = TRUE)
}

#' Verifie la forme de meta.json.
#'
#' Rigoureux a dessein : ces valeurs pilotent le navigateur (noms de fichiers,
#' bornes de couleur). Un jeu qui ne passe pas n'est pas affiche.
#'
#' @param meta liste issue de jsonlite::fromJSON(simplifyVector = FALSE).
#' @return list(ok = logical, erreurs = character).
#' @keywords internal
carte_meta_valider <- function(meta) {
  err <- character(0)
  ajouter <- function(m) err <<- c(err, m)
  if (!is.list(meta)) return(list(ok = FALSE, erreurs = "meta.json illisible."))

  if (!identical(suppressWarnings(as.integer(meta$schema %||% NA)),
                 as.integer(CONFIG$carte$schema))) {
    ajouter(sprintf("Version du format %s, %d attendue.",
                    as.character(meta$schema %||% "absente"), CONFIG$carte$schema))
  }

  annees <- suppressWarnings(as.integer(unlist(meta$annees)))
  if (length(annees) < 2L || anyNA(annees) || is.unsorted(annees, strictly = TRUE) ||
      min(annees) < 1900L || max(annees) > 2200L) {
    ajouter("Liste d'années invalide.")
  }

  n <- suppressWarnings(as.integer(meta$n_mailles %||% NA))
  if (length(n) != 1L || is.na(n) || n < 1L || n > CONFIG$carte$n_mailles_max) {
    ajouter("Nombre de mailles invalide.")
  }

  if (!carte_nom_fichier_sur(meta$grille %||% NA)) ajouter("Nom du fichier de grille invalide.")
  if (!is.character(meta$reference) || !grepl("^[0-9]{4}-[0-9]{4}$", meta$reference[1] %||% "")) {
    ajouter("Période de référence invalide.")
  }
  if (!is.character(meta$genere_le) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", meta$genere_le[1] %||% "")) {
    ajouter("Date de production invalide.")
  }

  couches <- meta$couches
  if (!is.list(couches) || !length(couches)) {
    ajouter("Aucune couche.")
  } else {
    cles <- vapply(couches, function(k) {
      if (is.list(k)) as.character(k$cle %||% NA_character_)[1] else NA_character_
    }, "")
    if (anyNA(cles) || anyDuplicated(cles)) ajouter("Clés de couches absentes ou en double.")
    for (k in couches) {
      if (!is.list(k)) { ajouter("Couche mal formée."); next }
      etiq <- as.character(k$cle %||% "?")[1]
      if (!(etiq %in% names(CARTE_COUCHES))) { ajouter(sprintf("Couche inconnue : %s.", etiq)); next }
      if (!carte_nom_fichier_sur(k$fichier %||% NA)) ajouter(sprintf("%s : nom de fichier invalide.", etiq))
      for (champ in c("domaine", "domaine_ecart", "domaine_pente")) {
        d <- suppressWarnings(as.numeric(unlist(k[[champ]])))
        if (length(d) != 2L || any(!is.finite(d)) || d[1] >= d[2]) {
          ajouter(sprintf("%s : %s invalide.", etiq, champ))
        }
      }
      for (champ in c("titre", "unite", "palette", "ecart")) {
        v <- k[[champ]]
        if (!is.character(v) || length(v) != 1L || is.na(v) || nchar(v) > 400L) {
          ajouter(sprintf("%s : %s invalide.", etiq, champ))
        }
      }
      if (!(as.character(k$palette %||% "")[1] %in% CARTE_PALETTES)) ajouter(sprintf("%s : palette inconnue.", etiq))
      if (!(as.character(k$ecart %||% "")[1] %in% CARTE_ECARTS)) ajouter(sprintf("%s : couleur d'écart inconnue.", etiq))
      st <- suppressWarnings(as.integer(k$stock %||% NA))
      if (length(st) != 1L || is.na(st) || st < 0L || st > 4L) ajouter(sprintf("%s : précision de stockage invalide.", etiq))
    }
  }
  list(ok = !length(err), erreurs = err)
}

#' Lit et valide le jeu de donnees d'un dossier local.
#' @param dossier chemin du dossier contenant meta.json.
#' @return list(meta = liste ou NULL, erreurs = character).
#' @keywords internal
carte_lire_meta_locale <- function(dossier) {
  f <- file.path(dossier, "meta.json")
  if (!file.exists(f)) return(list(meta = NULL, erreurs = "Aucun jeu de données cartographiques installé."))
  if (isTRUE(file.size(f) > 2 * 1024^2)) return(list(meta = NULL, erreurs = "meta.json trop volumineux."))
  meta <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  v <- carte_meta_valider(meta)
  if (!v$ok) return(list(meta = NULL, erreurs = v$erreurs))
  fichiers <- c(meta$grille, vapply(meta$couches, function(k) as.character(k$fichier)[1], ""))
  absents <- fichiers[!file.exists(file.path(dossier, fichiers))]
  if (length(absents)) {
    return(list(meta = NULL, erreurs = sprintf("Fichier(s) manquant(s) : %s.", paste(absents, collapse = ", "))))
  }
  list(meta = meta, erreurs = character(0))
}

#' Ou le navigateur ira-t-il chercher les fichiers de la carte ?
#'
#' 1. CONFIG$carte$url_donnees (ou la variable d'environnement CLIMAFOR_CARTE_URL),
#'    en https : les fichiers publies par le depot GitHub.
#' 2. Sinon le dossier local CONFIG$carte$dossier_local, servi par l'application
#'    elle-meme (aucun tiers contacte).
#' 3. Sinon rien : l'onglet l'explique.
#'
#' @param url,dossier reglages a utiliser (par defaut ceux de CONFIG) ; les tests
#'   les passent pour ne pas modifier la configuration.
#' @return list(mode = "distant" | "local" | "absent", base, erreurs).
#' @keywords internal
carte_transport <- function(url = CONFIG$carte$url_donnees,
                            dossier = CONFIG$carte$dossier_local) {
  if (is.character(url) && length(url) == 1L && nzchar(url)) {
    if (!carte_url_sure(url)) {
      return(list(mode = "absent", base = NULL,
                  erreurs = "L'adresse des données cartographiques n'est pas acceptable (https exigé)."))
    }
    return(list(mode = "distant", base = sub("/*$", "/", url), erreurs = character(0)))
  }
  # Chemin absolu ou deja exact : tel quel ; sinon, recherche insensible a la casse
  # (copie sur un partage qui a force les minuscules).
  dossier <- if (dir.exists(dossier)) dossier else chemin_ci(dossier)
  lu <- carte_lire_meta_locale(dossier)
  if (is.null(lu$meta)) return(list(mode = "absent", base = NULL, erreurs = lu$erreurs))
  nom <- CONFIG$carte$ressource
  # Une seule fois par dossier : le re-enregistrer a chaque session est inutile.
  if (!identical(unname(shiny::resourcePaths()[nom]), normalizePath(dossier))) {
    shiny::addResourcePath(nom, dossier)
  }
  list(mode = "local", base = paste0(nom, "/"), dossier = dossier, erreurs = character(0))
}

# ------------------------------------------------------------------------------
# Relecture d'une couche (onglet Comparer, tests)
# ------------------------------------------------------------------------------

CARTE_MANQUANT <- -32768L

#' Entiers 16 bits -> valeurs reelles (-32768 = absente).
#' @keywords internal
carte_decoder_valeurs <- function(e, stock) {
  x <- as.numeric(e) / 10^stock
  x[e == CARTE_MANQUANT] <- NA_real_
  x
}

#' Relit une couche : normale, pente, significativite et valeurs annee par annee.
#' @param n nombre de mailles ; @param n_annees ; @param stock decimales conservees.
#' @return liste(normale, pente, signif, valeurs = matrice n_annees x n,
#'   taille_exacte = le fichier ne contient ni plus ni moins que prevu).
#' @keywords internal
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


#' Nom affiche pour une maille choisie sur la carte.
#' @keywords internal
carte_nom_maille <- function(lon, lat) {
  sprintf("Maille SAFRAN (%s °N, %s °E)", fmt_fr(lat, 2), fmt_fr(lon, 2))
}

#' Assainit le clic renvoye par le navigateur.
#'
#' Ce que le navigateur envoie est une entree utilisateur comme une autre : on ne
#' retient que deux nombres finis, dans les bornes du territoire, arrondis a la
#' precision des liens partages.
#'
#' @param clic liste (id, lon, lat) ou NULL.
#' @return list(lon, lat, nom, source) pour l'application, ou NULL.
#' @keywords internal
carte_point_depuis_clic <- function(clic) {
  if (!is.list(clic)) return(NULL)
  lon <- suppressWarnings(as.numeric(clic$lon)[1])
  lat <- suppressWarnings(as.numeric(clic$lat)[1])
  if (!isTRUE(is.finite(lon)) || !isTRUE(is.finite(lat))) return(NULL)
  if (lat < PARTAGE_BORNES$lat[1] || lat > PARTAGE_BORNES$lat[2] ||
      lon < PARTAGE_BORNES$lon[1] || lon > PARTAGE_BORNES$lon[2]) return(NULL)
  lon <- round(lon, 4); lat <- round(lat, 4)
  list(lon = lon, lat = lat, nom = carte_nom_maille(lon, lat), source = "carte")
}
