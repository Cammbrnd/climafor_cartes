# ==============================================================================
# utils_synthese.R — BILAN SYNTHETIQUE : CALCUL, PHRASES, TABLEAUX, GRAPHIQUES (v2.0)
# ------------------------------------------------------------------------------
# Un bilan court d'un territoire, d'un point ou d'un ensemble de fiches : les
# grandeurs que l'on coche, chacune en graphique et/ou en tableau, avec quelques
# phrases d'explication en tete et sous chaque element.
#
# D'OU VIENNENT LES CHIFFRES
# --------------------------
# Des mailles precalculees de la carte, comme l'onglet Comparer : tout le calcul de
# rattachement, de serie, de normale et de tendance est celui de utils_comparer.R.
# Ce fichier ajoute ce que le bilan court demande en plus : le rang de l'annee, les
# phrases, les tableaux lisibles, les graphiques d'un seul lieu, et le cas des fiches.
#
# LES PHRASES SONT ECRITES PAR DES GABARITS, PAS PAR UN MODELE DE LANGAGE. Chaque
# phrase est une fonction des chiffres : le meme bilan donne toujours le meme texte, et
# chaque mot se retrouve dans le calcul. Elles decrivent un ecart a la normale ; elles
# n'expliquent jamais une cause.
#
# LES FICHES NE SONT PAS UN ECHANTILLON. Elles sont signalees ou la on regarde : on les
# replace dans leur contexte climatique, on n'en tire ni frequence ni lien de cause.
# ==============================================================================

SYNTHESE_ECHELLES <- c(point = "Un point", departement = "Département", region = "Région",
                       fiches = "Fiches importées")

SYNTHESE_BLOCS_FICHES <- c(f_essence = "Fiches par essence", f_probleme = "Fiches par problème",
                           f_annee = "Fiches par année")

SYNTHESE_COULEURS <- c(chaud = CL$temperature, eau = CL$pluie, vert = CL$vert,
                       violet = "#6A5A8C", froid = "#4A7FA5")

#' Couleur de trace d'une couche : celle de sa palette, sinon le vert de l'application.
#' (Un acces `[[` a un nom absent d'un vecteur nomme est une erreur : on passe par match.)
#' @keywords internal
synthese_couleur <- function(k) {
  i <- match(k$palette %||% "", names(SYNTHESE_COULEURS))
  if (is.na(i)) CL$vert else unname(SYNTHESE_COULEURS[i])
}

#' Titre en minuscules au milieu d'une phrase, sauf sigles (« SWI », « ETP ») : seule
#' la premiere lettre est abaissee, et seulement si la deuxieme est une minuscule.
#' @keywords internal
synthese_minuscule <- function(x) {
  vapply(x, function(t) {
    if (!nzchar(t) || nchar(t) < 2L) return(t)
    if (grepl("^[[:upper:]][[:lower:]]", t)) paste0(tolower(substr(t, 1L, 1L)), substr(t, 2L, nchar(t))) else t
  }, "", USE.NAMES = FALSE)
}

# ------------------------------------------------------------------------------
# 1. Catalogue : ce que l'on peut cocher
# ------------------------------------------------------------------------------

#' Famille d'une couche : celle qu'elle declare, sinon celle de son indicateur.
#' @keywords internal
synthese_famille <- function(cle) {
  k <- CARTE_COUCHES[[cle]]
  k$famille %||% INDICATEURS[[k$source]]$famille %||% "autre"
}

#' Grandeurs offertes par un profil : le jeu publie, restreint pour les profils
#' « restreint » (terrain, co).
#' @return vecteur nomme cle -> titre.
#' @keywords internal
synthese_catalogue <- function(jeu, profil) {
  dispo <- cmp_couches_disponibles(jeu)
  if (isTRUE(profil$restreint)) dispo <- dispo[names(dispo) %in% CONFIG$synthese$couches_co]
  dispo
}

#' Les grandeurs rangees par famille, dans l'ordre de FAMILLES.
#' @return liste nommee par CLE de famille : list(libelle, choix) ; `choix` est un vecteur
#'   nomme cle -> titre. Une famille inconnue du registre se range sous « autre ».
#' @keywords internal
synthese_groupes <- function(dispo) {
  fam <- vapply(names(dispo), synthese_famille, "")
  fam[!fam %in% names(FAMILLES)] <- "autre"
  out <- list()
  for (f in c(names(FAMILLES), "autre")) {
    i <- which(fam == f)
    if (length(i)) out[[f]] <- list(libelle = if (f == "autre") "Autres" else unname(FAMILLES[[f]]), choix = dispo[i])
  }
  out
}

#' Jeu de base d'un profil, limite aux grandeurs disponibles.
#' @return list(g, t) : cles dont le graphique et le tableau sont coches d'emblee.
#' @keywords internal
synthese_base <- function(profil_id, cles) {
  b <- CONFIG$synthese$base[[profil_id]] %||% CONFIG$synthese$base$co
  list(g = intersect(b$g, cles), t = intersect(b$t, cles))
}


# ------------------------------------------------------------------------------
# 2. Calcul
# ------------------------------------------------------------------------------

#' Rang de l'annee comparee dans la serie de chaque lieu et grandeur : 1 = la valeur
#' la plus elevee. `n` est le nombre d'annees renseignees.
#' @return data.frame(lieu, cle, rang_haut, n, premiere).
#' @keywords internal
synthese_rangs <- function(series, annee) {
  vide <- data.frame(lieu = character(0), cle = character(0), rang_haut = integer(0),
                     n = integer(0), premiere = integer(0), stringsAsFactors = FALSE)
  if (is.null(series) || !nrow(series)) return(vide)
  paires <- unique(series[, c("lieu", "cle")])
  l <- lapply(seq_len(nrow(paires)), function(i) {
    s <- series[series$lieu == paires$lieu[i] & series$cle == paires$cle[i] & is.finite(series$valeur), ]
    v <- s$valeur[match(annee, s$annee)]
    data.frame(lieu = paires$lieu[i], cle = paires$cle[i],
               rang_haut = if (is.finite(v)) as.integer(1L + sum(s$valeur > v)) else NA_integer_,
               n = nrow(s), premiere = if (nrow(s)) min(s$annee) else NA_integer_,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, l)
}

#' Contexte climatique de chaque fiche : valeur de l'annee de la fiche (ou de l'annee
#' comparee si la fiche n'a pas de date), normale de sa maille, ecart et lecture.
#'
#' Une fiche = une maille : la serie est celle de sa maille, la normale la moyenne de
#' la reference sur cette serie (dix annees au moins).
#' @param fiches fiches dans la grille ; @param mailles cmp_mailles() de ces fiches.
#' @return data.frame(fiche, cle, annee, valeur, normale, ecart, ecart_type_ref, classe).
#' @keywords internal
synthese_contexte_fiches <- function(fiches, mailles, jeu, cles, ctx, annee,
                                     executer = httr2::req_perform) {
  n <- nrow(fiches)
  if (!n || !length(cles)) {
    return(data.frame(fiche = character(0), cle = character(0), annee = integer(0),
                      valeur = numeric(0), normale = numeric(0), ecart = numeric(0),
                      ecart_type_ref = numeric(0), classe = character(0), stringsAsFactors = FALSE))
  }
  idx <- vapply(mailles, function(m) m$idx[1], 1L)
  an <- ifelse(is.na(fiches$annee), annee, fiches$annee)
  ref <- jeu$annees %in% ctx$ref
  l <- lapply(cles, function(cle) {
    S <- cmp_lire_couche(jeu, cle, executer)$valeurs[, idx, drop = FALSE]   # annees x fiches
    ok <- !is.na(S[ref, , drop = FALSE])
    nref <- colSums(ok)
    normale <- ifelse(nref >= CONFIG$comparer$annees_ref_min,
                      colMeans(S[ref, , drop = FALSE], na.rm = TRUE), NA_real_)
    sdref <- vapply(seq_len(n), function(j) {
      v <- S[ref, j]; v <- v[is.finite(v)]
      if (length(v) >= 3L) stats::sd(v) else NA_real_
    }, 0)
    ia <- match(an, jeu$annees)
    valeur <- ifelse(is.na(ia), NA_real_, S[cbind(ia, seq_len(n))])
    ecart <- valeur - normale
    data.frame(fiche = fiches$id, cle = cle, annee = as.integer(an), valeur = as.numeric(valeur),
               normale = as.numeric(normale), ecart = as.numeric(ecart), ecart_type_ref = sdref,
               classe = classe_ecart(ecart, sdref), stringsAsFactors = FALSE)
  })
  r <- do.call(rbind, l); rownames(r) <- NULL
  r
}

#' Resume du contexte des fiches, une ligne par grandeur : combien au-dessus, en dessous,
#' ecart median.
#' @keywords internal
synthese_resume_contexte <- function(ctxf) {
  if (is.null(ctxf) || !nrow(ctxf)) {
    return(data.frame(cle = character(0), n = integer(0), n_haut = integer(0), n_bas = integer(0),
                      mediane = numeric(0), stringsAsFactors = FALSE))
  }
  l <- lapply(split(ctxf, ctxf$cle), function(d) {
    d <- d[is.finite(d$ecart), , drop = FALSE]
    data.frame(cle = d$cle[1] %||% NA_character_, n = nrow(d), n_haut = sum(d$classe == "haut"),
               n_bas = sum(d$classe == "bas"),
               mediane = if (nrow(d)) stats::median(d$ecart) else NA_real_, stringsAsFactors = FALSE)
  })
  r <- do.call(rbind, l)
  r$cle <- names(l); rownames(r) <- NULL
  r[order(match(r$cle, names(CARTE_COUCHES))), , drop = FALSE]
}

#' Prepare un bilan : tout ce que l'affichage et les exports lisent.
#'
#' @param echelle "point", "departement", "region" ou "fiches".
#' @param lieux lieux a analyser (cmp_lieu_point / cmp_lieu_zone), pour les trois
#'   premieres echelles ; @param fiches tableau de fiches (fiches_vide()), pour « fiches ».
#' @param jeu cmp_jeu() ; @param cles_g,cles_t grandeurs dont on veut le graphique, le tableau.
#' @param ctx contexte (ctx_creer) ; @param annee annee comparee.
#' @return liste `res` (voir le code) : tl, series, tendances, bilan, rang, contexte...
#' @export
synthese_preparer <- function(echelle, lieux = list(), fiches = NULL, jeu, cles_g, cles_t,
                              ctx, annee, profil = NULL, executer = httr2::req_perform) {
  cles <- intersect(names(CARTE_COUCHES), union(cles_g, cles_t))
  res <- list(echelle = echelle, annee = as.integer(annee), ctx = ctx, profil = profil$id %||% NA_character_,
              cles_g = intersect(cles_g, cles), cles_t = intersect(cles_t, cles), cles = cles,
              jeu = list(genere_le = as.character(jeu$meta$genere_le), n_mailles = as.integer(jeu$meta$n_mailles),
                         annees = jeu$annees),
              fiches = NULL, contexte = NULL)

  if (identical(echelle, "fiches")) {
    f <- fiches %||% fiches_vide()
    mailles <- lapply(seq_len(nrow(f)), function(i) cmp_mailles(cmp_lieu_point(f$id[i], f$lon[i], f$lat[i]), jeu))
    dans <- vapply(mailles, function(m) length(m$idx) > 0L, TRUE)
    res$fiches <- f
    res$fiches_dans <- dans
    res$tl <- data.frame(lieu = "Fiches importées", type = "fiches", n_mailles = sum(dans),
                         note = sprintf("%d fiche%s sur %d dans la grille", sum(dans), if (sum(dans) > 1L) "s" else "", nrow(f)),
                         retenu = any(dans), stringsAsFactors = FALSE)
    res$contexte <- synthese_contexte_fiches(f[dans, , drop = FALSE], mailles[dans], jeu, cles, ctx, annee, executer)
    res$resume_contexte <- synthese_resume_contexte(res$contexte)
    res$series <- cmp_series(jeu, character(0), list(), character(0))
    res$bilan <- cmp_bilan(res$series, ctx, annee)
    res$rang <- synthese_rangs(res$series, annee)
    res$tendances <- cmp_tendances(res$series, ctx)
    return(res)
  }

  mailles <- lapply(lieux, cmp_mailles, jeu = jeu)
  tl <- cmp_table_lieux(lieux, mailles)
  garde <- which(tl$retenu)
  res$tl <- tl
  res$series <- cmp_series(jeu, tl$lieu[garde], mailles[garde], cles, executer)
  res$tendances <- cmp_tendances(res$series, ctx)
  res$bilan <- cmp_bilan(res$series, ctx, annee, res$tendances)
  res$rang <- synthese_rangs(res$series, annee)
  res
}


# ------------------------------------------------------------------------------
# 3. Phrases
# ------------------------------------------------------------------------------

#' « 3e » : ordinal francais pour un rang >= 2.
#' @keywords internal
synthese_ieme <- function(k) paste0(k, "e")

#' Mention du rang de l'annee, seulement quand elle figure parmi les cinq extremes de
#' la serie (sinon elle n'apprend rien) ; chaine vide sinon.
#' @keywords internal
synthese_phrase_rang <- function(rg) {
  if (is.null(rg) || !nrow(rg) || !is.finite(rg$rang_haut[1]) || rg$n[1] < 20L) return("")
  k <- rg$rang_haut[1]; n <- rg$n[1]; depuis <- rg$premiere[1]
  if (k <= 5L) {
    return(if (k == 1L) sprintf("C'est la valeur la plus élevée depuis %d.", depuis)
           else sprintf("C'est la %s valeur la plus élevée depuis %d (sur %d années).", synthese_ieme(k), depuis, n))
  }
  b <- n - k + 1L
  if (b <= 5L) {
    return(if (b == 1L) sprintf("C'est la valeur la plus basse depuis %d.", depuis)
           else sprintf("C'est la %s valeur la plus basse depuis %d (sur %d années).", synthese_ieme(b), depuis, n))
  }
  ""
}

#' Lecture en mots d'un ecart.
#' @keywords internal
synthese_lecture_ecart <- function(classe, ecart, sd_ref) {
  if (!is.finite(ecart) || !is.finite(sd_ref)) return("normale non évaluable")
  z <- if (sd_ref > 0) abs(ecart) / sd_ref else 0
  switch(classe,
    haut = if (z >= 1) "nettement au-dessus de la normale" else "au-dessus de la normale",
    bas  = if (z >= 1) "nettement en dessous de la normale" else "en dessous de la normale",
    "proche de la normale")
}

#' Phrases d'une grandeur pour un lieu : l'ecart, le rang s'il est extreme, la tendance.
#' @param r une ligne de cmp_bilan() ; @param rg la ligne de synthese_rangs() correspondante.
#' @keywords internal
synthese_phrases_ligne <- function(r, rg, ctx, annee, avec_lieu = FALSE) {
  lieu <- if (avec_lieu) paste0(" (", r$lieu, ")") else ""
  if (!is.finite(r$valeur)) {
    return(sprintf("%s%s : aucune valeur pour %d.", r$titre, lieu, annee))
  }
  unite <- if (nzchar(r$unite)) r$unite else ""
  s1 <- if (is.finite(r$normale)) {
    sprintf("%s%s en %d : %s, soit %s par rapport à la normale %s (%s).",
            r$titre, lieu, annee, fmt_val(r$valeur, unite, r$decimales),
            fmt_ecart(r$ecart, unite, r$decimales), ctx$ref_label,
            synthese_lecture_ecart(r$classe, r$ecart, r$ecart_type_ref))
  } else {
    sprintf("%s%s en %d : %s (normale %s non calculable : série trop courte).",
            r$titre, lieu, annee, fmt_val(r$valeur, unite, r$decimales), ctx$ref_label)
  }
  tend <- if (isTRUE(r$tendance_ok)) sprintf(
    "Tendance depuis %d : %s par décennie (%s).", min(ctx$affichage),
    fmt_ecart(r$pente_dec, unite, r$decimales),
    if (isTRUE(r$tendance_significative)) "significative" else "non significative") else ""
  p <- c(s1, synthese_phrase_rang(rg), tend)
  p[nzchar(p)]
}

#' Phrases d'une grandeur : un lieu, ses trois phrases ; plusieurs, une comparaison.
#' @param res resultat de synthese_preparer().
#' @export
synthese_phrases_cle <- function(res, cle) {
  if (identical(res$echelle, "fiches")) {
    rc <- res$resume_contexte
    rc <- if (is.null(rc)) rc else rc[rc$cle == cle, , drop = FALSE]
    if (is.null(rc) || !nrow(rc) || rc$n[1] == 0L) return("Aucune fiche avec une valeur pour cette grandeur.")
    k <- CARTE_COUCHES[[cle]]
    dec <- if (as.integer(k$decimales) == 0L) 1L else as.integer(k$decimales)
    return(c(
      sprintf("Pour %d fiche%s, l'année de la fiche est au-dessus de la normale %s pour %d et en dessous pour %d.",
              rc$n[1], if (rc$n[1] > 1L) "s" else "", res$ctx$ref_label, rc$n_haut[1], rc$n_bas[1]),
      sprintf("Écart médian : %s.", fmt_ecart(rc$mediane[1], k$unite, dec))))
  }
  b <- res$bilan[res$bilan$cle == cle, , drop = FALSE]
  if (!nrow(b)) return(character(0))
  if (nrow(b) == 1L) {
    rg <- res$rang[res$rang$lieu == b$lieu[1] & res$rang$cle == cle, , drop = FALSE]
    return(synthese_phrases_ligne(b[1, ], rg, res$ctx, res$annee))
  }
  b <- b[is.finite(b$ecart), , drop = FALSE]
  if (!nrow(b)) return(sprintf("Aucune valeur exploitable pour %d.", res$annee))
  u <- if (nzchar(b$unite[1])) b$unite[1] else ""
  d <- b$decimales[1]
  h <- b[which.max(b$ecart), ]; l <- b[which.min(b$ecart), ]
  c(sprintf("Écart à la normale %s en %d : le plus élevé à %s, %s ; le plus faible à %s, %s.",
            res$ctx$ref_label, res$annee, h$lieu, fmt_ecart(h$ecart, u, d), l$lieu, fmt_ecart(l$ecart, u, d)),
    sprintf("%d territoire%s sur %d au-dessus de la normale, %d en dessous.",
            sum(b$classe == "haut"), if (sum(b$classe == "haut") > 1L) "s" else "", nrow(b), sum(b$classe == "bas")))
}

#' Phrases generales d'un bilan : l'ensemble en deux ou trois phrases.
#' @export
synthese_phrases_generales <- function(res) {
  a <- res$annee; ref <- res$ctx$ref_label

  if (identical(res$echelle, "fiches")) {
    f <- res$fiches; n <- nrow(f); dans <- sum(res$fiches_dans)
    if (!n) return("Aucune fiche à analyser.")
    p <- sprintf("%d fiche%s importée%s, dont %d située%s dans la grille SAFRAN.",
                 n, if (n > 1L) "s" else "", if (n > 1L) "s" else "", dans, if (dans > 1L) "s" else "")
    ess <- fiches_compter(f, "essence"); pb <- fiches_compter(f, "probleme")
    ess <- ess[ess$modalite != FICHE_NON_RENSEIGNE, ]; pb <- pb[pb$modalite != FICHE_NON_RENSEIGNE, ]
    if (nrow(ess) && nrow(pb)) {
      p <- c(p, sprintf("Essence la plus fréquente : %s (%d fiche%s) ; problème le plus fréquent : %s (%d fiche%s).",
                        ess$modalite[1], ess$n[1], if (ess$n[1] > 1L) "s" else "",
                        pb$modalite[1], pb$n[1], if (pb$n[1] > 1L) "s" else ""))
    }
    rc <- res$resume_contexte
    if (!is.null(rc) && nrow(rc)) {
      rc <- rc[rc$n > 0, , drop = FALSE]
      if (nrow(rc)) {
        i <- which.max(abs(rc$n_haut - rc$n_bas) / rc$n)
        k <- CARTE_COUCHES[[rc$cle[i]]]
        p <- c(p, sprintf(
          "Au regard de la normale %s, l'année de la fiche est au-dessus de la normale pour %d fiche%s sur %d (%s) et en dessous pour %d ; l'écart médian est de %s.",
          ref, rc$n_haut[i], if (rc$n_haut[i] > 1L) "s" else "", rc$n[i], synthese_minuscule(k$titre), rc$n_bas[i],
          fmt_ecart(rc$mediane[i], k$unite, if (as.integer(k$decimales) == 0L) 1L else k$decimales)))
      }
    }
    return(p)
  }

  b <- res$bilan[is.finite(res$bilan$valeur) & is.finite(res$bilan$normale), , drop = FALSE]
  lieux <- unique(b$lieu)
  if (!nrow(b)) return(sprintf("Aucune valeur exploitable pour %d avec les grandeurs choisies.", a))
  z <- ifelse(is.finite(b$ecart_type_ref) & b$ecart_type_ref > 0, abs(b$ecart) / b$ecart_type_ref, 0)

  if (length(lieux) == 1L) {
    p <- sprintf("%s : bilan de %d comparé à la normale %s, sur %d grandeur%s.",
                 lieux, a, ref, nrow(b), if (nrow(b) > 1L) "s" else "")
    t <- b[b$cle == "tmp", , drop = FALSE]
    if (nrow(t) == 1L) {
      p <- c(p, switch(t$classe,
        haut = sprintf("L'année est plus chaude que la normale (température moyenne %s).",
                       fmt_ecart(t$ecart, t$unite, t$decimales)),
        bas = sprintf("L'année est plus fraîche que la normale (température moyenne %s).",
                      fmt_ecart(t$ecart, t$unite, t$decimales)),
        "La température moyenne de l'année est proche de la normale."))
    }
    o <- order(-z); o <- utils::head(o, 3L)
    p <- c(p, sprintf("Écarts les plus marqués : %s.",
      paste(vapply(o, function(i) sprintf("%s (%s)", synthese_minuscule(b$titre[i]),
                                          fmt_ecart(b$ecart[i], b$unite[i], b$decimales[i])), ""),
            collapse=", ")))
    nh <- sum(b$classe == "haut" & z >= 1); nb <- sum(b$classe == "bas" & z >= 1)
    p <- c(p, sprintf("%d grandeur%s nettement au-dessus de la normale, %d nettement en dessous (écart supérieur à un écart-type).",
                      nh, if (nh > 1L) "s" else "", nb))
    return(p)
  }

  p <- sprintf("Comparaison de %d territoires pour %d, rapportée à la normale %s de chacun.",
               length(lieux), a, ref)
  o <- order(-z); o <- utils::head(o, 3L)
  c(p, sprintf("Écarts les plus marqués : %s.",
    paste(vapply(o, function(i) sprintf("%s à %s, %s", synthese_minuscule(b$titre[i]), b$lieu[i],
                                        fmt_ecart(b$ecart[i], b$unite[i], b$decimales[i])), ""),
          collapse = " ; ")))
}

#' Texte « comment lire » d'une grandeur : celui du registre de l'indicateur qui la calcule.
#' @keywords internal
synthese_lecture_cle <- function(cle) {
  k <- CARTE_COUCHES[[cle]]
  paste(INDICATEURS[[k$source]]$lecture %||% character(0), collapse = " ")
}

#' Date de production du jeu de mailles, en jj/mm/aaaa ; le texte brut si elle est illisible.
#' @keywords internal
synthese_date_jeu <- function(x) {
  x <- as.character(x %||% "")[1]
  d <- tryCatch(as.Date(substr(x, 1L, 10L)), error = function(e) NA)
  if (is.na(d)) x else format(d, "%d/%m/%Y")
}

#' Paragraphes de methode et de limites, toujours joints au bilan.
#' @export
synthese_methode <- function(res) {
  dmax <- fmt_fr(CONFIG$comparer$distance_max_m / 1000, 0)
  zone <- switch(res$echelle,
    point = sprintf("Un point prend la valeur de sa maille la plus proche (à %s km au plus).", dmax),
    fiches = sprintf("Chaque fiche prend la valeur de sa maille la plus proche (à %s km au plus), pour l'année de la fiche (l'année comparée si la fiche n'est pas datée).", dmax),
    "Un territoire prend la moyenne, à poids égaux, des mailles dont le centre est dans son contour ; un contour plus petit qu'une maille prend la maille la plus proche. La moyenne n'est pas pondérée par la surface forestière.")
  m <- c(
    sprintf("Source : réanalyse SAFRAN-ISBA (Météo-France), mailles de 8 km précalculées le %s (%d mailles). Les seuils sont ceux par défaut du précalcul : les réglages de l'onglet Paramètres ne s'y appliquent pas.",
            synthese_date_jeu(res$jeu$genere_le), res$jeu$n_mailles),
    zone,
    sprintf("La normale est la moyenne de la période %s ; un écart n'est qualifié d'« au-dessus » ou « en dessous » que s'il dépasse un demi écart-type de cette période. Le rang compare l'année aux autres années de la série. La tendance est la pente de Sen avec le test de Mann-Kendall corrigé de l'autocorrélation, sur vingt années au moins ; « non significative » ne veut pas dire « absente ».", res$ctx$ref_label),
    "Une maille lisse le relief : en terrain accidenté, la valeur d'un lieu peut s'écarter de celle d'une station proche. Ce bilan décrit un climat, il ne se substitue pas à un diagnostic stationnel.")
  if (identical(res$echelle, "fiches")) {
    m <- c(m, "Les fiches sont signalées là où l'on regarde : elles ne forment pas un échantillon. Leur contexte climatique éclaire une observation, il n'en établit pas la cause.")
  }
  m
}


# ------------------------------------------------------------------------------
# 4. Tableaux
# ------------------------------------------------------------------------------

#' Tableau lisible des grandeurs cochees : une ligne par grandeur et par lieu.
#' @return data.frame(Grandeur, Lieu, Valeur, Normale, Écart, Lecture, Rang, Tendance) en texte.
#' @export
synthese_tableau <- function(res, cles = res$cles_t) {
  b <- res$bilan[res$bilan$cle %in% cles, , drop = FALSE]
  if (!nrow(b)) return(NULL)
  b <- b[order(match(b$cle, names(CARTE_COUCHES)), match(b$lieu, res$tl$lieu)), , drop = FALSE]
  rang <- vapply(seq_len(nrow(b)), function(i) {
    rg <- res$rang[res$rang$lieu == b$lieu[i] & res$rang$cle == b$cle[i], , drop = FALSE]
    if (!nrow(rg) || !is.finite(rg$rang_haut[1])) return("—")
    k <- rg$rang_haut[1]; n <- rg$n[1]
    h <- k <= (n + 1L) %/% 2L
    j <- if (h) k else n - k + 1L
    sprintf("%s %s sur %d", if (j == 1L) "la plus" else paste0(synthese_ieme(j), " plus"),
            if (h) "élevée" else "basse", n)
  }, "")
  d <- data.frame(
    grandeur = b$titre, lieu = b$lieu,
    valeur = vapply(seq_len(nrow(b)), function(i) fmt_val(b$valeur[i], b$unite[i], b$decimales[i]), ""),
    normale = vapply(seq_len(nrow(b)), function(i) fmt_val(b$normale[i], b$unite[i], b$decimales[i]), ""),
    ecart = vapply(seq_len(nrow(b)), function(i) if (is.finite(b$ecart[i])) fmt_ecart(b$ecart[i], b$unite[i], b$decimales[i]) else "—", ""),
    lecture = vapply(seq_len(nrow(b)), function(i) if (is.finite(b$ecart[i]))
      sub(" de la normale", "", synthese_lecture_ecart(b$classe[i], b$ecart[i], b$ecart_type_ref[i])) else "—", ""),
    rang = rang,
    tendance = vapply(seq_len(nrow(b)), function(i) if (isTRUE(b$tendance_ok[i]))
      sprintf("%s par décennie%s", fmt_ecart(b$pente_dec[i], b$unite[i], b$decimales[i]),
              if (isTRUE(b$tendance_significative[i])) "" else " (non signif.)") else "—", ""),
    stringsAsFactors = FALSE)
  names(d) <- c("Grandeur", "Lieu", "Valeur", "Normale", "Écart", "Lecture", "Rang", "Tendance")
  rownames(d) <- NULL
  d
}

#' Tableau des fiches avec leur contexte : une ligne par fiche, une colonne par grandeur
#' cochee (« valeur (écart) »).
#' @export
synthese_tableau_fiches <- function(res, cles = res$cles_t) {
  f <- res$fiches
  if (is.null(f) || !nrow(f)) return(NULL)
  cles <- intersect(cles, unique(res$contexte$cle))
  d <- data.frame(fiche = f$id, date = ifelse(is.na(f$date), "—", format(f$date, "%d/%m/%Y")),
                  essence = ifelse(is.na(f$essence), "—", f$essence),
                  probleme = ifelse(is.na(f$probleme), "—", f$probleme), stringsAsFactors = FALSE)
  names(d) <- c("Fiche", "Date", "Essence", "Problème")
  for (cle in cles) {
    k <- CARTE_COUCHES[[cle]]; dec <- if (as.integer(k$decimales) == 0L) 1L else k$decimales
    c1 <- res$contexte[res$contexte$cle == cle, , drop = FALSE]
    i <- match(f$id, c1$fiche)
    d[[k$titre]] <- ifelse(is.na(i), "hors grille",
      ifelse(!is.finite(c1$valeur[i]), "—",
             sprintf("%s (%s)", fmt_fr(c1$valeur[i], dec),
                     ifelse(is.finite(c1$ecart[i]), fmt_fr(c1$ecart[i], dec, signe = TRUE), "n.d."))))
  }
  rownames(d) <- NULL
  d
}

#' Contexte climatique des fiches, une ligne par grandeur cochee : combien de fiches au-dessus,
#' en dessous de la normale, et l'ecart median.
#' @export
synthese_tableau_resume_fiches <- function(res, cles = res$cles_t) {
  rc <- res$resume_contexte
  if (is.null(rc) || !nrow(rc)) return(NULL)
  rc <- rc[rc$cle %in% cles, , drop = FALSE]
  if (!nrow(rc)) return(NULL)
  d <- data.frame(
    grandeur = vapply(rc$cle, function(k) CARTE_COUCHES[[k]]$titre, ""),
    n = as.character(rc$n), haut = as.character(rc$n_haut), bas = as.character(rc$n_bas),
    mediane = vapply(seq_len(nrow(rc)), function(i) {
      k <- CARTE_COUCHES[[rc$cle[i]]]
      fmt_ecart(rc$mediane[i], k$unite, if (as.integer(k$decimales) == 0L) 1L else as.integer(k$decimales))
    }, ""), stringsAsFactors = FALSE)
  names(d) <- c("Grandeur", "Fiches", "Au-dessus de la normale", "En dessous de la normale", "Écart médian")
  rownames(d) <- NULL
  d
}

#' Tableau de repartition des fiches (essence, probleme ou annee).
#' @export
synthese_tableau_repartition <- function(res, col) {
  r <- fiches_compter(res$fiches, col)
  if (!nrow(r)) return(NULL)
  if (col == "annee") r <- r[order(r$modalite), ]
  d <- data.frame(c1 = r$modalite, Fiches = r$n, Part = paste0(fmt_fr(r$part, 0), " %"), stringsAsFactors = FALSE)
  names(d)[1] <- c(essence = "Essence", probleme = "Problème", annee = "Année")[[col]]
  rownames(d) <- NULL
  d
}


# ------------------------------------------------------------------------------
# 5. Graphiques
# ------------------------------------------------------------------------------

#' Serie annuelle d'une grandeur. Un lieu : la serie, sa normale en pointillés, l'annee
#' comparee en relief et la tendance de Sen. Plusieurs lieux : l'ecart de chacun a sa
#' propre normale (la seule lecture qui compare des climats differents).
#' @export
synthese_graphe_serie <- function(res, cle, mobile = FALSE) {
  s <- res$series[res$series$cle == cle, , drop = FALSE]
  if (!nrow(s) || all(is.na(s$valeur))) return(graphe_message("Aucune valeur à tracer pour cette grandeur."))
  lieux <- unique(s$lieu)
  if (length(lieux) > 1L) return(cmp_graphe(res$series, res$bilan, cle, "ecart", res$ctx, res$annee, mobile))
  k <- CARTE_COUCHES[[cle]]
  b <- res$bilan[res$bilan$cle == cle & res$bilan$lieu == lieux, , drop = FALSE]
  couleur <- synthese_couleur(k)
  unite <- if (nzchar(k$unite)) sprintf(" (%s)", k$unite) else ""
  p <- ggplot2::ggplot(s, ggplot2::aes(x = .data$annee, y = .data$valeur))
  if (nrow(b) && is.finite(b$normale[1])) {
    p <- p + ggplot2::geom_hline(yintercept = b$normale[1], colour = CL$reference, linetype = "dashed", linewidth = 0.5)
  }
  p <- p + ggplot2::geom_line(colour = couleur, linewidth = 0.7, na.rm = TRUE)
  if (nrow(b) && isTRUE(b$tendance_ok[1]) && is.finite(b$pente_dec[1])) {
    sp <- s[is.finite(s$valeur) & s$annee %in% res$ctx$affichage, ]
    pente <- b$pente_dec[1] / 10
    origine <- stats::median(sp$valeur - pente * sp$annee)
    tr <- data.frame(annee = range(sp$annee)); tr$valeur <- origine + pente * tr$annee
    p <- p + ggplot2::geom_line(data = tr, colour = CL$texte, linewidth = 0.5, linetype = "solid", alpha = 0.7)
  }
  pt <- s[s$annee == res$annee & is.finite(s$valeur), , drop = FALSE]
  if (nrow(pt)) p <- p + ggplot2::geom_point(data = pt, colour = CL$annee_courante, size = 3)
  p + ggplot2::scale_x_continuous(breaks = graphe_ruptures_annees(s$annee, mobile)) +
    ggplot2::labs(x = NULL, y = paste0(k$titre, unite), title = k$titre,
                  subtitle = sprintf("%s — normale %s en pointillés, %d en relief, tendance en trait plein",
                                     lieux, res$ctx$ref_label, res$annee)) +
    theme_climafor(mobile = mobile)
}

#' Ecart a la normale de l'annee de chaque fiche, pour une grandeur : un point par
#' fiche, la mediane en trait plein.
#' @export
synthese_graphe_fiches_ecart <- function(res, cle, mobile = FALSE) {
  d <- res$contexte[res$contexte$cle == cle & is.finite(res$contexte$ecart), , drop = FALSE]
  if (!nrow(d)) return(graphe_message("Aucune fiche avec une valeur pour cette grandeur."))
  k <- CARTE_COUCHES[[cle]]
  unite <- if (nzchar(k$unite)) sprintf(" (%s)", k$unite) else ""
  set.seed(1)
  d$y <- stats::runif(nrow(d), -0.35, 0.35)
  med <- stats::median(d$ecart)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$ecart, y = .data$y)) +
    ggplot2::geom_vline(xintercept = 0, colour = CL$reference, linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = med, colour = CL$texte, linewidth = 0.6) +
    ggplot2::geom_point(colour = synthese_couleur(k), size = 2.4, alpha = 0.75) +
    ggplot2::scale_y_continuous(limits = c(-1, 1), breaks = NULL) +
    ggplot2::labs(x = sprintf("Écart à la normale %s de l'année de la fiche%s", res$ctx$ref_label, unite), y = NULL,
                  title = k$titre,
                  subtitle = sprintf("%d fiche%s ; écart médian %s (trait plein), normale en pointillés", nrow(d),
                                     if (nrow(d) > 1L) "s" else "", fmt_ecart(med, k$unite, if (as.integer(k$decimales) == 0L) 1L else k$decimales))) +
    theme_climafor(mobile = mobile) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
}

#' Repartition des fiches (essence, probleme, annee) : barres, dix modalites au plus.
#' @export
synthese_graphe_repartition <- function(res, col, mobile = FALSE) {
  r <- fiches_compter(res$fiches, col)
  if (!nrow(r)) return(graphe_message("Aucune fiche à compter."))
  titre <- c(essence = "Fiches par essence", probleme = "Fiches par problème", annee = "Fiches par année")[[col]]
  if (col == "annee") {
    r <- r[order(r$modalite), ]; r$modalite <- factor(r$modalite, levels = r$modalite)
  } else {
    r <- utils::head(r, 10L); r$modalite <- factor(r$modalite, levels = rev(r$modalite))
  }
  p <- ggplot2::ggplot(r, ggplot2::aes(x = .data$modalite, y = .data$n)) +
    ggplot2::geom_col(fill = CL$vert, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = .data$n), hjust = if (col == "annee") 0.5 else -0.3,
                       vjust = if (col == "annee") -0.4 else 0.5, size = 3.4, colour = CL$texte) +
    ggplot2::labs(x = NULL, y = "Nombre de fiches", title = titre) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    theme_climafor(mobile = mobile)
  if (col != "annee") p <- p + ggplot2::coord_flip()
  p
}

#' Le graphique d'un element du bilan : une grandeur, ou une repartition de fiches.
#' @param id cle de grandeur, ou « f_essence », « f_probleme », « f_annee ».
#' @export
synthese_graphe <- function(res, id, mobile = FALSE) {
  if (id %in% names(SYNTHESE_BLOCS_FICHES)) {
    return(synthese_graphe_repartition(res, sub("^f_", "", id), mobile))
  }
  if (identical(res$echelle, "fiches")) return(synthese_graphe_fiches_ecart(res, id, mobile))
  synthese_graphe_serie(res, id, mobile)
}
