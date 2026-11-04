# ==============================================================================
# carte_precalcul.R — CALCUL DES COUCHES DE LA CARTE, MAILLE PAR MAILLE
# ------------------------------------------------------------------------------
# PRINCIPE. La carte ne recalcule rien : pour chaque maille, ce fichier appelle
# les fonctions de calcul MEMES de l'application (INDICATEURS[[id]]$calc, tendance,
# stats_ref) sur la serie journaliere de la maille. Une valeur de la carte est donc
# la valeur que l'onglet d'analyse afficherait pour ce point : il n'y a pas deux
# formules a maintenir (tests/test_v18.R le verifie).
#
# DEUX ETAPES, pour pouvoir repartir le travail sur plusieurs machines :
#   1. carte_construire()  calcule une PART (les mailles dont l'indice tombe dans
#                          ce fragment) et l'ecrit dans un .rds ;
#   2. carte_assembler()   reunit les parts, fixe les bornes de couleur, ecrit les
#                          fichiers publies et les verifie.
#
# La serie journaliere d'une maille vient d'une fonction `lire_maille(i)` que l'on
# fournit : elle rend le tableau au format de safran_charger() (voir utils_api.R)
# pour les seules annees COMPLETES, ou NULL en cas d'echec.
# ==============================================================================

#' Contexte de calcul de la carte : reference et fenetre par defaut de
#' l'application, annee courante = derniere annee cartographiee.
carte_ctx <- function(annees, reference = CONFIG$reference_defaut) {
  ctx_creer(ref = CONFIG$references[[reference]], affichage = annees,
            courante = max(annees), fenetre = CONFIG$fenetre_defaut,
            seuils = CONFIG$seuils)
}

#' Famille d'affichage d'une couche : la sienne si elle en declare une, sinon
#' celle de l'indicateur du registre.
#' @param couche element de CARTE_COUCHES.
#' @return chaine, ou NA si l'indicateur source n'existe pas.
carte_famille <- function(couche) {
  if (is.character(couche$famille) && length(couche$famille) == 1L) return(couche$famille)
  ind <- INDICATEURS[[couche$source]]
  if (is.null(ind)) NA_character_ else ind$famille
}

#' Les couches d'une maille.
#'
#' @param don tableau journalier de la maille (format safran_charger()).
#' @param ctx contexte de carte_ctx().
#' @param annees annees cartographiees.
#' @return liste nommee par cle de couche ; chaque element : valeurs (une par
#'   annee, NA = absente), normale, pente (par decennie), signif (logique).
carte_calculer_maille <- function(don, ctx, annees, couches = CARTE_COUCHES) {
  sources <- unique(vapply(couches, function(k) k$source, ""))
  # Chaque indicateur n'est calcule qu'une fois, meme s'il porte plusieurs couches.
  res <- stats::setNames(lapply(sources, function(s) {
    tryCatch(INDICATEURS[[s]]$calc(don, ctx), error = function(e) NULL)
  }), sources)

  # Garde propre a la carte, en plus de la completude de chaque indicateur : une
  # annee dont la serie journaliere est incomplete (SAFRAN publie avec retard)
  # n'est cartographiee pour aucune couche.
  bissextile <- (annees %% 4L == 0L & annees %% 100L != 0L) | annees %% 400L == 0L
  n_jours <- tabulate(match(don$annee, annees), length(annees))
  incomplete <- n_jours < ifelse(bissextile, 366L, 365L) - 2L

  lapply(couches, function(k) {
    vide <- list(valeurs = rep(NA_real_, length(annees)), normale = NA_real_,
                 pente = NA_real_, signif = FALSE)
    r <- res[[k$source]]
    if (is.null(r) || !nrow(r) || !(k$colonne %in% names(r))) return(vide)

    pos <- match(annees, r$annee)
    v <- r[[k$colonne]][pos]
    # Une annee incomplete (SAFRAN publie avec retard) n'est pas cartographiee :
    # la comparer a des annees pleines ferait une carte fausse.
    v[r$partielle[pos] %in% TRUE | incomplete | !is.finite(v)] <- NA_real_

    normale <- if (!is.null(k$normale)) r[[k$normale]][1]
               else stats_ref(r, k$colonne, ctx)$normale
    t <- tendance(annees, v, unite = k$unite, dec = k$decimales)
    list(valeurs = v,
         normale = if (is.finite(normale)) normale else NA_real_,
         pente = if (isTRUE(t$ok)) t$pente_dec else NA_real_,
         signif = isTRUE(t$ok) && isTRUE(t$significative))
  })
}

#' Calcule une part : les mailles `indices` (rangs 1..n dans lon/lat).
#'
#' @param lire_maille function(i, lon, lat) -> tableau journalier ou NULL.
#' @param fichier si non NULL, la part est ecrite en .rds a cet endroit.
#' @return la part (liste), de façon invisible.
carte_construire <- function(lire_maille, lon, lat, annees, indices,
                             couches = CARTE_COUCHES, fichier = NULL,
                             progres_tous = 200L) {
  ctx <- carte_ctx(annees)
  na <- length(annees); m <- length(indices)
  part <- list(
    indices = as.integer(indices), annees = annees,
    valeurs = lapply(couches, function(k) matrix(NA_real_, na, m)),
    normale = lapply(couches, function(k) rep(NA_real_, m)),
    pente   = lapply(couches, function(k) rep(NA_real_, m)),
    signif  = lapply(couches, function(k) rep(FALSE, m)),
    echecs  = integer(0))
  debut <- Sys.time()
  for (j in seq_len(m)) {
    i <- indices[j]
    don <- tryCatch(lire_maille(i, lon[i], lat[i]), error = function(e) NULL)
    if (is.null(don) || !is.data.frame(don) || !nrow(don)) {
      part$echecs <- c(part$echecs, i); next
    }
    out <- carte_calculer_maille(don, ctx, annees, couches)
    for (cle in names(couches)) {
      part$valeurs[[cle]][, j] <- out[[cle]]$valeurs
      part$normale[[cle]][j]   <- out[[cle]]$normale
      part$pente[[cle]][j]     <- out[[cle]]$pente
      part$signif[[cle]][j]    <- out[[cle]]$signif
    }
    if (j %% progres_tous == 0L) {
      cat(sprintf("  %d / %d mailles (%.1f s)\n", j, m,
                  as.numeric(difftime(Sys.time(), debut, units = "secs"))))
    }
  }
  if (!is.null(fichier)) saveRDS(part, fichier)
  invisible(part)
}

# -- Bornes de couleur ---------------------------------------------------------

# Arrondit vers l'exterieur a un pas « joli » (1, 2 ou 5 x 10^k), pour des bornes
# de legende lisibles.
.carte_pas <- function(etendue) {
  if (!is.finite(etendue) || etendue <= 0) return(1)
  p <- 10^floor(log10(etendue)) / 2
  p
}

#' Bornes des couleurs d'une couche, fixes pour toutes les annees.
#'
#' Percentiles 2 et 98 de l'ensemble (mailles x annees) : quelques mailles
#' extremes ne ecrasent pas le degrade des autres. Les ecarts et tendances sont
#' symetriques autour de zero, pour que le blanc veuille toujours dire « normale ».
carte_domaines <- function(valeurs, normale, pente) {
  bornes <- function(x, symetrique) {
    x <- x[is.finite(x)]
    if (!length(x)) return(c(-1, 1))
    if (symetrique) {
      m <- stats::quantile(abs(x), 0.98, names = FALSE)
      pas <- .carte_pas(m); m <- ceiling(m / pas) * pas
      if (!(m > 0)) m <- 1
      return(c(-m, m))
    }
    q <- stats::quantile(x, c(0.02, 0.98), names = FALSE)
    pas <- .carte_pas(q[2] - q[1])
    lo <- floor(q[1] / pas) * pas; hi <- ceiling(q[2] / pas) * pas
    if (!(hi > lo)) hi <- lo + 1
    c(lo, hi)
  }
  ecarts <- sweep(valeurs, 2, normale, "-")
  list(domaine = bornes(valeurs, FALSE),
       domaine_ecart = bornes(ecarts, TRUE),
       domaine_pente = bornes(pente, TRUE))
}

#' Reunit les parts et ecrit les fichiers publies.
#'
#' @param parts liste de parts (carte_construire) ou vecteur de chemins .rds.
#' @param source texte affiche a l'utilisateur (origine des donnees).
#' @param taux_echec_max part de mailles sans donnees au-dela de laquelle on
#'   refuse de publier : une carte trouee vaut moins que la version precedente.
#' @param part_min_annee une annee finale dont la part de valeurs renseignees tombe
#'   sous cette fraction de la meilleure annee est retiree du jeu.
#' @return liste (dossier, n, annees, echecs, annees_retirees, taille_octets).
carte_assembler <- function(parts, lon, lat, annees, source, dossier,
                            couches = CARTE_COUCHES, taux_echec_max = 0.01,
                            genere_le = format(Sys.Date()), annee_partielle = NULL,
                            part_min_annee = 0.95) {
  if (is.character(parts)) parts <- lapply(parts, readRDS)
  n <- length(lon); na <- length(annees)
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)

  agr <- list(
    valeurs = lapply(couches, function(k) matrix(NA_real_, na, n)),
    normale = lapply(couches, function(k) rep(NA_real_, n)),
    pente   = lapply(couches, function(k) rep(NA_real_, n)),
    signif  = lapply(couches, function(k) rep(FALSE, n)))
  traite <- logical(n); echecs <- integer(0)
  for (p in parts) {
    if (!identical(as.integer(p$annees), as.integer(annees))) {
      stop("Les parts ne portent pas les memes annees.", call. = FALSE)
    }
    traite[p$indices] <- TRUE
    echecs <- c(echecs, p$echecs)
    for (cle in names(couches)) {
      agr$valeurs[[cle]][, p$indices] <- p$valeurs[[cle]]
      agr$normale[[cle]][p$indices]   <- p$normale[[cle]]
      agr$pente[[cle]][p$indices]     <- p$pente[[cle]]
      agr$signif[[cle]][p$indices]    <- p$signif[[cle]]
    }
  }
  if (!all(traite)) {
    stop(sprintf("%d maille(s) sur %d n'ont ete traitees par aucune part.", sum(!traite), n),
         call. = FALSE)
  }
  if (length(echecs) > taux_echec_max * n) {
    stop(sprintf("%d maille(s) sur %d sans donnees (plus de %.0f %%) : publication refusee.",
                 length(echecs), n, 100 * taux_echec_max), call. = FALSE)
  }

  # SAFRAN publie avec retard : la derniere annee demandee peut ne pas etre encore
  # complete. Les dernieres annees presque vides sont retirees (et le disent), plutot
  # que publiees comme une carte quasi blanche.
  renseigne <- vapply(seq_len(na), function(k) {
    mean(vapply(names(couches), function(cle) mean(is.finite(agr$valeurs[[cle]][k, ])), 0))
  }, 0)
  garde <- na
  while (garde > 2L && renseigne[garde] < part_min_annee * max(renseigne)) garde <- garde - 1L
  retirees <- if (garde < na) annees[(garde + 1L):na] else integer(0)
  if (length(retirees)) {
    annees <- annees[seq_len(garde)]
    for (cle in names(couches)) agr$valeurs[[cle]] <- agr$valeurs[[cle]][seq_len(garde), , drop = FALSE]
  }

  ctx <- carte_ctx(annees)
  meta_couches <- list()
  for (cle in names(couches)) {
    k <- couches[[cle]]
    dom <- carte_domaines(agr$valeurs[[cle]], agr$normale[[cle]], agr$pente[[cle]])
    carte_ecrire_couche(dossier, k, agr$normale[[cle]], agr$pente[[cle]],
                        agr$signif[[cle]], agr$valeurs[[cle]])
    ind <- INDICATEURS[[k$source]]
    note <- if (k$source == "petp") {
      sprintf("Calculé sur la fenêtre de %s à %s.", MOIS_LONG[ctx$fenetre[1]], MOIS_LONG[ctx$fenetre[2]])
    } else ""
    meta_couches[[length(meta_couches) + 1L]] <- list(
      cle = k$cle, fichier = sprintf("c_%s.bin.gz", k$cle),
      famille = carte_famille(k), titre = k$titre, unite = k$unite,
      decimales = k$decimales, stock = k$stock, palette = k$palette, ecart = k$ecart,
      domaine = dom$domaine, domaine_ecart = dom$domaine_ecart, domaine_pente = dom$domaine_pente,
      methode = substr(paste(ind$methode, collapse = " "), 1, 1500),
      lecture = substr(paste(ind$lecture, collapse = " "), 1, 1500),
      note = note)
  }
  carte_ecrire_grille(dossier, lon, lat)
  carte_ecrire_meta(dossier, list(
    schema = CONFIG$carte$schema, annees = annees, n_mailles = n,
    grille = "grille.json", reference = ctx$ref_label, genere_le = genere_le,
    source = source, annee_partielle = annee_partielle,
    couches = meta_couches))

  # Controle final : ce qui est ecrit doit passer la validation de l'application.
  lu <- carte_lire_meta_locale(dossier)
  if (is.null(lu$meta)) {
    stop(paste("Les fichiers produits ne passent pas la validation :",
               paste(lu$erreurs, collapse = " ")), call. = FALSE)
  }
  fichiers <- list.files(dossier, full.names = TRUE)
  list(dossier = dossier, n = n, annees = annees, echecs = sort(unique(echecs)),
       annees_retirees = retirees, taille_octets = sum(file.size(fichiers)))
}
