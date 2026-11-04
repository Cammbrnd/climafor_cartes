# ==============================================================================
# src/utils_cache.R — Cache disque : format, noms de fichiers, purge
# ------------------------------------------------------------------------------
# Audit v1.7, parties V1 et V7.
#
# V7 : le cache etait ecrit et relu par saveRDS()/readRDS(). Or readRDS() peut
# EXECUTER DU CODE si le fichier a ete forge (une fonction ou une expression y
# est une valeur comme une autre). Sans effet tant que seul ClimaFor ecrit dans
# le dossier, mais le dossier peut etre persistant ou partage. Le cache ne
# contient pourtant que des tableaux de nombres, de textes et de dates : il est
# donc ecrit comme tableau « sans code » (JSON compresse) et RELU AVEC CONTROLE :
#
#   - 2 lignes exactement : un en-tete (format, schema, nombre de lignes, classe
#     de chaque colonne) puis les colonnes ;
#   - classes admises : character, numeric, integer, logical, Date. Toute autre
#     classe (liste, fonction, facteur...) est REFUSEE A L'ECRITURE aussi ;
#   - noms de colonnes de forme simple, une valeur par ligne et par colonne,
#     dates reelles, entiers entiers ;
#   - taille decompressee plafonnee (`CONFIG$cache$lecture_max_mo`) : un petit
#     fichier gzip qui se decompresserait en plusieurs Go est ecarte sans etre lu.
#   Un fichier qui ne passe pas ces controles est supprime et traite comme absent
#   (la donnee est simplement retelechargee).
#
# Les noms de fichiers sont, eux aussi, assainis : un identifiant de station
# venu d'une donnee distante ne peut ni sortir du dossier de cache ni former un
# nom ambigu.
#
# V1 : le cache n'etait jamais purge (la duree de validite n'etait controlee
# qu'a la lecture). cache_purger() supprime les fichiers trop vieux, puis les
# plus anciens jusqu'a tenir sous le plafond de taille.
#
# Rien ici ne touche le reseau ni Shiny : tout se teste sans navigateur.
# ==============================================================================

CACHE_FORMAT   <- "climafor-cache"
CACHE_EXT      <- ".json.gz"
CACHE_CLASSES  <- c("character", "numeric", "integer", "logical", "Date")
# Attributs de tableau conserves (valeurs simples uniquement).
CACHE_ATTRIBUTS <- c("secours")
# Seuls les fichiers de cette forme sont lus ou supprimes par ClimaFor.
CACHE_MOTIF_FICHIER <- "^(safran_|serie_|dept_|index_fichiers)[A-Za-z0-9._-]*\\.(json\\.gz|rds)$"
CACHE_MOTIF_TEMP    <- "^\\.cf_tmp_[A-Za-z0-9._-]+$"

.ETAT_CACHE <- new.env(parent = emptyenv())


# ------------------------------------------------------------------------------
# 1. Noms de fichiers
# ------------------------------------------------------------------------------

#' Rend un morceau de nom de fichier sur : lettres, chiffres, point, tiret et
#' soulignement seulement, points repetes reduits a un, 60 caracteres au plus.
#' Jamais vide (« x » a defaut). Aucun separateur de dossier ne survit.
#' @keywords internal
cache_nom_sur <- function(x) {
  x <- as.character(x)[1]
  if (is.na(x)) x <- ""
  x <- gsub("[^A-Za-z0-9._-]", "_", x)
  x <- gsub("\\.{2,}", ".", x)
  x <- substr(x, 1L, 60L)
  if (!nzchar(x)) "x" else x
}

#' Un nom de fichier de cache est-il acceptable tel quel ? (pas de dossier, pas
#' de caractere inattendu, pas de nom cache ni de « .. »)
#' @keywords internal
cache_nom_valide <- function(nom) {
  nom <- as.character(nom)[1]
  !is.na(nom) && nzchar(nom) && nchar(nom) <= 200L &&
    grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", nom) && !grepl("\\.\\.", nom)
}


# ------------------------------------------------------------------------------
# 2. Ecriture
# ------------------------------------------------------------------------------

#' Classe de cache d'une colonne, ou NA si elle n'est pas admise.
#' @keywords internal
cache_classe_colonne <- function(x) {
  if (inherits(x, "Date"))                          return("Date")
  if (is.object(x) || is.list(x))                   return(NA_character_)
  if (is.integer(x))                                return("integer")
  if (is.double(x))                                 return("numeric")
  if (is.character(x))                              return("character")
  if (is.logical(x))                                return("logical")
  NA_character_
}

#' Ecrit un tableau en cache : JSON compresse, ecriture atomique.
#'
#' Leve une erreur si le tableau contient autre chose que les classes admises ;
#' les appelants (cache_ecrire, mf_cache_ecrire) l'attrapent : un cache qui ne
#' s'ecrit pas ne doit jamais faire echouer l'analyse.
#'
#' L'ecriture passe par un fichier temporaire PUIS renomme : une autre session
#' qui lit en meme temps voit l'ancien fichier ou le nouveau, jamais un fichier
#' coupe en deux. Les nombres sont ecrits avec 15 chiffres significatifs (toute
#' la precision utile des mesures) ; NA, NaN et l'infini deviennent « null ».
#'
#' @param df data.frame.
#' @param chemin chemin du fichier (nom valide, voir cache_nom_valide).
#' @return TRUE, invisiblement.
#' @keywords internal
cache_ecrire_table <- function(df, chemin) {
  if (!is.data.frame(df)) stop("cache : un tableau est attendu")
  nom <- basename(chemin)
  if (!cache_nom_valide(nom)) stop("cache : nom de fichier refuse")
  noms <- names(df)
  if (!length(noms) || anyNA(noms) || anyDuplicated(noms) ||
      !all(grepl("^[A-Za-z][A-Za-z0-9_.]{0,63}$", noms))) {
    stop("cache : noms de colonnes non admis")
  }
  classes <- vapply(df, cache_classe_colonne, character(1))
  if (anyNA(classes)) {
    stop(sprintf("cache : classe de colonne non admise (%s)",
                 paste(noms[is.na(classes)], collapse = ", ")))
  }

  colonnes <- lapply(seq_along(df), function(j) {
    x <- df[[j]]
    if (classes[j] == "Date") format(x, "%Y-%m-%d") else as.vector(x)
  })
  names(colonnes) <- noms

  attributs <- list()
  for (a in CACHE_ATTRIBUTS) {
    v <- attr(df, a, exact = TRUE)
    if (!is.null(v) && length(v) == 1L && !is.na(v) &&
        (is.logical(v) || is.character(v) || (is.numeric(v) && is.finite(v)))) {
      attributs[[a]] <- v
    }
  }

  entete <- list(format = CACHE_FORMAT,
                 schema = as.integer(CONFIG$cache$schema %||% 1L),
                 n = nrow(df),
                 classes = as.list(classes),
                 attributs = attributs)
  ligne1 <- as.character(jsonlite::toJSON(entete, auto_unbox = TRUE, null = "null"))
  ligne2 <- as.character(jsonlite::toJSON(colonnes, auto_unbox = FALSE,
                                          digits = NA, na = "null", null = "null"))

  dossier <- dirname(chemin)
  if (!dir.exists(dossier)) dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  tmp <- file.path(dossier, sprintf(".cf_tmp_%d_%s", Sys.getpid(),
                                    paste(sample(c(letters, 0:9), 8, TRUE), collapse = "")))
  con <- gzfile(tmp, open = "wb", compression = 6L)
  ok <- tryCatch({
    writeLines(c(ligne1, ligne2), con, useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE, finally = close(con))
  if (!ok || !file.rename(tmp, chemin)) {
    unlink(tmp)
    stop("cache : ecriture impossible")
  }
  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# 3. Lecture avec controle
# ------------------------------------------------------------------------------

#' Lit au plus `max_octets` octets decompresses ; NULL si le fichier est plus gros.
#' Lecture par blocs : un fichier gzip piege ne remplit jamais la memoire.
#' @keywords internal
cache_lire_octets <- function(chemin, max_octets) {
  con <- gzfile(chemin, open = "rb")
  on.exit(close(con), add = TRUE)
  blocs <- list(); total <- 0
  repeat {
    b <- readBin(con, "raw", n = 1048576L)
    if (!length(b)) break
    total <- total + length(b)
    if (total > max_octets) return(NULL)
    blocs[[length(blocs) + 1L]] <- b
  }
  do.call(c, blocs)
}

#' Convertit une colonne lue dans JSON vers sa classe declaree, ou leve une
#' erreur si les valeurs ne sont pas ce que la classe annonce.
#' @keywords internal
cache_convertir_colonne <- function(v, classe, n) {
  if (n == 0L) {
    return(switch(classe,
      character = character(0), numeric = numeric(0), integer = integer(0),
      logical = logical(0), Date = as.Date(character(0))))
  }
  if (is.list(v) || length(v) != n) stop("colonne mal formee")
  tout_na <- is.logical(v) && all(is.na(v))   # une colonne de null seulement
  switch(classe,
    numeric = {
      if (!is.numeric(v) && !tout_na) stop("colonne non numerique")
      as.numeric(v)
    },
    integer = {
      if (!is.numeric(v) && !tout_na) stop("colonne non entiere")
      w <- as.numeric(v)
      if (any(!is.na(w) & (w != round(w) | abs(w) >= .Machine$integer.max))) {
        stop("entier invalide")
      }
      as.integer(w)
    },
    character = {
      if (!is.character(v) && !tout_na) stop("colonne non textuelle")
      as.character(v)
    },
    logical = {
      if (!is.logical(v)) stop("colonne non logique")
      v
    },
    Date = {
      if (!is.character(v) && !tout_na) stop("colonne de dates invalide")
      d <- as.Date(as.character(v), format = "%Y-%m-%d")
      if (any(is.na(d) != is.na(v))) stop("date invalide")
      d
    },
    stop("classe inconnue"))
}

#' Relit un tableau ecrit par cache_ecrire_table, ou NULL.
#'
#' NULL si le fichier est absent, trop gros, d'un autre format ou d'un autre
#' schema, ou s'il ne passe pas un des controles du descriptif en tete de ce
#' fichier. Un fichier ILLISIBLE, INVALIDE ou trop gros est supprime : ClimaFor
#' ne l'a pas ecrit, il ne peut que gener, et la donnee sera retelechargee.
#'
#' @param max_mo taille decompressee maximale, en Mo.
#' @keywords internal
cache_lire_table <- function(chemin, max_mo = CONFIG$cache$lecture_max_mo %||% 100) {
  if (!cache_nom_valide(basename(chemin)) || !file.exists(chemin)) return(NULL)
  max_octets <- max_mo * 1024^2
  taille <- suppressWarnings(file.size(chemin))
  if (!is.finite(taille)) return(NULL)

  # Trop gros (compresse ou une fois decompresse) : ClimaFor n'ecrit pas de tels
  # fichiers, donc celui-ci n'est pas le sien. Il est ecarte sans etre lu et
  # supprime, comme un fichier invalide.
  invalide <- FALSE
  res <- tryCatch({
    if (taille > max_octets) stop("fichier trop gros")
    octets <- cache_lire_octets(chemin, max_octets)
    if (is.null(octets)) stop("fichier trop gros une fois decompresse")
    texte <- rawToChar(octets)
    Encoding(texte) <- "UTF-8"
    lignes <- strsplit(texte, "\n", fixed = TRUE)[[1]]
    if (length(lignes) != 2L) stop("deux lignes attendues")

    e <- jsonlite::fromJSON(lignes[1], simplifyVector = TRUE)
    if (!is.list(e) || !identical(e$format, CACHE_FORMAT)) stop("format inconnu")
    if (!identical(as.integer(e$schema), as.integer(CONFIG$cache$schema %||% 1L))) {
      stop("schema different")
    }
    n <- e$n
    if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 0 || n != round(n) ||
        n > 5e6) stop("nombre de lignes invalide")
    n <- as.integer(n)
    classes <- unlist(e$classes)
    noms <- names(e$classes)
    if (!length(classes) || is.null(noms) || anyDuplicated(noms) ||
        !all(classes %in% CACHE_CLASSES) ||
        !all(grepl("^[A-Za-z][A-Za-z0-9_.]{0,63}$", noms))) {
      stop("colonnes invalides")
    }

    donnees <- jsonlite::fromJSON(lignes[2], simplifyVector = TRUE,
                                  simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
    if (!is.list(donnees) || !identical(names(donnees), noms)) stop("colonnes inattendues")
    cols <- lapply(seq_along(noms), function(j) {
      cache_convertir_colonne(donnees[[j]], unname(classes[j]), n)
    })
    names(cols) <- noms
    df <- structure(cols, class = "data.frame", row.names = .set_row_names(n))

    for (a in intersect(names(e$attributs), CACHE_ATTRIBUTS)) {
      v <- e$attributs[[a]]
      if (length(v) == 1L && !is.list(v) && !is.na(v)) attr(df, a) <- v
    }
    df
  }, error = function(err) { invalide <<- TRUE; NULL })

  if (invalide) unlink(chemin)
  res
}


# ------------------------------------------------------------------------------
# 4. Purge (V1)
# ------------------------------------------------------------------------------

#' Fichiers du cache qui appartiennent a ClimaFor (jamais autre chose) :
#' data.frame(chemin, taille, mtime, temp).
#' @keywords internal
cache_inventaire <- function(racine = CONFIG$cache$repertoire) {
  vide <- data.frame(chemin = character(0), taille = numeric(0),
                     mtime = as.POSIXct(character(0)), temp = logical(0),
                     stringsAsFactors = FALSE)
  if (!dir.exists(racine)) return(vide)
  f <- list.files(racine, recursive = TRUE, full.names = TRUE, all.files = TRUE,
                  no.. = TRUE)
  if (!length(f)) return(vide)
  nom <- basename(f)
  ours <- grepl(CACHE_MOTIF_FICHIER, nom) | grepl(CACHE_MOTIF_TEMP, nom)
  f <- f[ours]
  if (!length(f)) return(vide)
  info <- file.info(f)
  ok <- !is.na(info$isdir) & !info$isdir
  data.frame(chemin = f[ok], taille = info$size[ok], mtime = info$mtime[ok],
             temp = grepl(CACHE_MOTIF_TEMP, basename(f[ok])),
             stringsAsFactors = FALSE)
}

#' Purge le cache : format ancien, fichiers trop vieux, puis plafond de taille.
#'
#'   1. les fichiers `.rds` (format d'avant la v1.7, plus jamais relus) ;
#'   2. les fichiers temporaires d'ecriture de plus d'une heure (ecriture
#'      interrompue) ;
#'   3. les fichiers plus vieux que la plus longue duree de validite d'une
#'      entree : ils ne seraient de toute facon plus relus ;
#'   4. si le dossier depasse `taille_max_mo`, les plus anciens fichiers
#'      d'abord, jusqu'a retomber a 80 % du plafond.
#'
#' Ne supprime que des fichiers de forme connue (CACHE_MOTIF_FICHIER) ; ne leve
#' jamais d'erreur (un hebergement en lecture seule n'est pas un probleme).
#'
#' @param racine dossier du cache.
#' @param maintenant instant de reference (injectable pour les tests).
#' @param taille_max_mo plafond du dossier.
#' @return liste(supprimes, liberes_o, restants, taille_o), invisiblement.
#' @export
cache_purger <- function(racine = CONFIG$cache$repertoire,
                         maintenant = Sys.time(),
                         taille_max_mo = CONFIG$cache$taille_max_mo) {
  bilan <- list(supprimes = 0L, liberes_o = 0, restants = 0L, taille_o = 0)
  # Date de la derniere purge, notee d'emblee : meme sur un dossier vide, la
  # prochaine ecriture ne relance pas tout de suite un parcours du dossier.
  .ETAT_CACHE$derniere_purge <- maintenant
  tryCatch({
    inv <- cache_inventaire(racine)
    if (!nrow(inv)) return(invisible(bilan))

    age_j <- as.numeric(difftime(maintenant, inv$mtime, units = "days"))
    age_j[!is.finite(age_j)] <- Inf
    duree_max_j <- max(CONFIG$cache$duree_j %||% 30,
                       CONFIG$stations$cache_serie_j %||% 30)

    ancien  <- grepl("\\.rds$", inv$chemin)
    temp_vieux <- inv$temp & age_j > 1 / 24
    perime  <- age_j > duree_max_j
    a_supprimer <- ancien | temp_vieux | perime
    # Un fichier temporaire recent est une ecriture en cours : on n'y touche pas.
    a_supprimer[inv$temp & !temp_vieux] <- FALSE

    reste <- !a_supprimer
    if (is.finite(taille_max_mo) && taille_max_mo > 0) {
      plafond <- taille_max_mo * 1024^2
      if (sum(inv$taille[reste], na.rm = TRUE) > plafond) {
        cible <- 0.8 * plafond
        ordre <- order(inv$mtime, na.last = FALSE)       # le plus ancien d'abord
        ordre <- ordre[reste[ordre] & !inv$temp[ordre]]
        for (i in ordre) {
          if (sum(inv$taille[reste], na.rm = TRUE) <= cible) break
          reste[i] <- FALSE
          a_supprimer[i] <- TRUE
        }
      }
    }

    chemins <- inv$chemin[a_supprimer]
    for (p in chemins) unlink(p)
    apres <- cache_inventaire(racine)
    bilan$supprimes <- sum(!file.exists(chemins))
    bilan$liberes_o <- sum(inv$taille[inv$chemin %in% chemins[!file.exists(chemins)]], na.rm = TRUE)
    bilan$restants  <- nrow(apres)
    bilan$taille_o  <- sum(apres$taille, na.rm = TRUE)
    invisible(bilan)
  }, error = function(e) invisible(bilan))
}

#' Purge si la derniere purge date de plus de `purge_intervalle_min` minutes.
#' Appelee apres chaque ecriture : le plus souvent, elle ne fait rien.
#' @keywords internal
cache_purger_si_besoin <- function(maintenant = Sys.time()) {
  derniere <- .ETAT_CACHE$derniere_purge
  pas <- (CONFIG$cache$purge_intervalle_min %||% 10) * 60
  if (!is.null(derniere) &&
      as.numeric(difftime(maintenant, derniere, units = "secs")) < pas) {
    return(invisible(NULL))
  }
  cache_purger(maintenant = maintenant)
}
