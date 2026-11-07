# ==============================================================================
# utils_comparer_duo.R — COMPARER DEUX LIEUX, ET LES SOURCES D'UN POINT (v2.1)
# ------------------------------------------------------------------------------
# Deux usages, un seul calcul.
#
# 1. L'onglet Comparer, mode « Deux lieux » : on choisit deux lieux parmi
#      - une MAILLE SAFRAN (un point : commune, coordonnees),
#      - une STATION Meteo-France,
#      - un DEPARTEMENT,
#    dans n'importe quelle combinaison (maille-maille, station-station, maille-station,
#    departement-departement, departement-maille, departement-station), et on lit ce qui
#    les separe.
# 2. Le bilan d'un POINT (Synthese, Terrain, correspondant-observateur) : la maille du
#    point et les stations proches, cote a cote — la mesure et le modele.
#
# Dans les deux cas, ce sont des « lieux » du bilan synthetique (utils_comparer.R) :
# serie annuelle, normale, ecart, tendance, phrases, tableaux, graphiques et exports sont
# ceux des autres echelles. Ce fichier n'ajoute que ce qu'il faut pour LIRE UN ECART ENTRE
# DEUX LIEUX.
#
# PERIODE COMMUNE. Une station commence en 1975, la maille en 1959 : comparer leurs
# moyennes sur des annees differentes comparerait deux climats. Dans le mode « Deux lieux »,
# chaque grandeur n'est donc comparee que sur les annees ou LES DEUX lieux ont une valeur ;
# normales et tendances sont calculees sur ces annees-la. La periode retenue est dite.
#
# MESURE ET MODELE. Un ecart entre une station et une maille n'est pas une erreur de
# l'une ou de l'autre : la maille lisse le relief (8 km), la station decrit son
# emplacement. Les phrases le disent chaque fois que l'un des lieux est une mesure.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Les lieux
# ------------------------------------------------------------------------------

#' Ce qu'est un lieu : une mesure ou un modele.
#' @param lieu lieu du bilan (type point, zone ou station).
#' @return "mesure" (station), "modèle" (maille SAFRAN, point), "moyenne de mailles" (zone).
#' @keywords internal
duo_nature <- function(lieu) {
  switch(lieu$type, station = "mesure", zone = "moyenne de mailles", "modèle")
}

#' Nom d'un lieu sans la mention « (mesure) » / « (modèle) » : les phrases la disent deja.
#' @keywords internal
duo_nom_court <- function(x) sub(" \\((mesure|modèle)\\)$", "", x)

#' Noms uniques pour deux lieux : si A et B portent le meme nom, on les distingue.
#' @return list(a, b) : les lieux renommes au besoin.
#' @keywords internal
duo_noms_uniques <- function(a, b) {
  if (identical(a$nom, b$nom)) {
    a$nom <- cmp_nom_propre(paste0(a$nom, " (A)"), "Lieu A")
    b$nom <- cmp_nom_propre(paste0(b$nom, " (B)"), "Lieu B")
  }
  list(a = a, b = b)
}

#' Le lieu « departement » d'un code, ou NULL s'il est inconnu.
#' @keywords internal
duo_lieu_departement <- function(code) {
  l <- tryCatch(territoires_lieux("departement", code), error = function(e) list())
  if (length(l)) l[[1]] else NULL
}


# ------------------------------------------------------------------------------
# 2. Preparer la comparaison de deux lieux
# ------------------------------------------------------------------------------

#' Series restreintes aux annees ou LES DEUX lieux ont une valeur, grandeur par grandeur.
#' Une grandeur sans annee commune garde ses lignes, vides : elle reste dans le tableau,
#' avec « — », au lieu de disparaitre sans explication.
#' @keywords internal
duo_restreindre <- function(series, a, b) {
  if (is.null(series) || !nrow(series)) return(series)
  morceaux <- lapply(split(series, series$cle), function(s) {
    ok <- function(l) s$annee[s$lieu == l & is.finite(s$valeur)]
    communes <- intersect(ok(a), ok(b))
    if (!length(communes)) {
      s <- s[!duplicated(s[, c("lieu", "annee")]), , drop = FALSE]
      s$valeur <- NA_real_
      return(s)
    }
    s[s$annee %in% communes, , drop = FALSE]
  })
  r <- do.call(rbind, morceaux)
  r <- r[order(match(r$cle, unique(series$cle)), match(r$lieu, unique(series$lieu)), r$annee), , drop = FALSE]
  rownames(r) <- NULL
  r
}

#' Prepare la comparaison de deux lieux.
#'
#' @param a,b lieux (cmp_lieu_point, cmp_lieu_station ou territoires_lieux) ;
#'   @param jeu cmp_jeu() ; @param cles grandeurs ; @param ctx contexte ; @param annee annee comparee.
#' @return la liste de synthese_preparer() (tl, series, tendances, bilan, rang...) dont
#'   les series sont restreintes a la periode commune, plus `a`, `b` (noms), `lieux`
#'   (les deux lieux) et `series_completes` (avant restriction).
#' @export
duo_preparer <- function(a, b, jeu, cles, ctx, annee, executer = httr2::req_perform) {
  u <- duo_noms_uniques(a, b); a <- u$a; b <- u$b
  res <- synthese_preparer("point", list(a, b), NULL, jeu, cles, cles, ctx, annee, NULL, executer)
  res$duo <- TRUE
  res$a <- a$nom; res$b <- b$nom; res$lieux <- list(a = a, b = b)
  res$series_completes <- res$series
  if (all(res$tl$retenu)) {
    res$series <- duo_restreindre(res$series, a$nom, b$nom)
    res$tendances <- cmp_tendances(res$series, ctx)
    res$bilan <- cmp_bilan(res$series, ctx, annee, res$tendances)
    res$rang <- synthese_rangs(res$series, annee)
  }
  res
}

#' Ce qui separe les deux lieux pour une grandeur, sur leurs annees communes.
#' @param res duo_preparer().
#' @return list(cle, n, premiere, derniere, ecart_moyen = moyenne de A − B, r = correlation
#'   des valeurs annuelles, NA sous dix annees communes).
#' @export
duo_stats <- function(res, cle) {
  s <- res$series[res$series$cle == cle, , drop = FALSE]
  sa <- s[s$lieu == res$a, c("annee", "valeur")]; sb <- s[s$lieu == res$b, c("annee", "valeur")]
  m <- merge(sa, sb, by = "annee", suffixes = c("_a", "_b"))
  m <- m[is.finite(m$valeur_a) & is.finite(m$valeur_b), , drop = FALSE]
  n <- nrow(m)
  r <- if (n >= 10L && stats::sd(m$valeur_a) > 0 && stats::sd(m$valeur_b) > 0)
    stats::cor(m$valeur_a, m$valeur_b) else NA_real_
  list(cle = cle, n = n, premiere = if (n) min(m$annee) else NA_integer_,
       derniere = if (n) max(m$annee) else NA_integer_,
       ecart_moyen = if (n) mean(m$valeur_a - m$valeur_b) else NA_real_, r = r)
}

#' Grandeurs qui ont au moins une valeur chez l'un des deux lieux, dans l'ordre de la carte.
#' Une grandeur sans annee commune reste (le tableau dit pourquoi), au lieu de disparaitre.
#' @keywords internal
duo_cles_utiles <- function(res) {
  s <- res$series_completes %||% res$series
  cles <- unique(s$cle[is.finite(s$valeur)])
  cles[order(match(cles, names(CARTE_COUCHES)))]
}


# ------------------------------------------------------------------------------
# 3. Lecture : tableau, phrases, export
# ------------------------------------------------------------------------------

#' « A au-dessus de B », « A en dessous de B » ou « valeurs identiques ».
#' @keywords internal
duo_sens <- function(ecart, dec) {
  if (!is.finite(ecart)) return("—")
  if (abs(ecart) < 0.5 * 10^(-dec)) return("valeurs identiques")
  if (ecart > 0) "A au-dessus de B" else "A en dessous de B"
}

#' Le tableau de comparaison, en texte : une ligne par grandeur.
#' @return data.frame(Grandeur, A, B, Écart A − B, Écart à la normale (A), Écart à la normale (B), Lecture)
#'   ou NULL.
#' @export
duo_tableau <- function(res) {
  cles <- duo_cles_utiles(res)
  if (!length(cles)) return(NULL)
  compl <- res$series_completes %||% res$series
  a_des <- function(lieu, cle) any(is.finite(compl$valeur[compl$lieu == lieu & compl$cle == cle]))
  lignes <- lapply(cles, function(cle) {
    ba <- res$bilan[res$bilan$lieu == res$a & res$bilan$cle == cle, , drop = FALSE]
    bb <- res$bilan[res$bilan$lieu == res$b & res$bilan$cle == cle, , drop = FALSE]
    k <- CARTE_COUCHES[[cle]]
    dec <- if (nrow(ba)) ba$decimales[1] else if (as.integer(k$decimales) == 0L) 1L else as.integer(k$decimales)
    va <- if (nrow(ba)) ba$valeur[1] else NA_real_; vb <- if (nrow(bb)) bb$valeur[1] else NA_real_
    ea <- if (nrow(ba)) ba$ecart[1] else NA_real_; eb <- if (nrow(bb)) bb$ecart[1] else NA_real_
    d <- va - vb
    # Sans annee commune, la valeur de l'annee comparee reste visible, mais rien ne se compare.
    if (!is.finite(va)) va <- compl$valeur[compl$lieu == res$a & compl$cle == cle & compl$annee == res$annee][1]
    if (!is.finite(vb)) vb <- compl$valeur[compl$lieu == res$b & compl$cle == cle & compl$annee == res$annee][1]
    lect_n <- if (is.finite(ea) && is.finite(eb)) {
      if (identical(ba$classe[1], bb$classe[1])) "même lecture à la normale" else "lectures différentes à la normale"
    } else NULL
    lecture <- if (!a_des(res$a, cle)) sprintf("%s ne donne pas cette grandeur", duo_nom_court(res$a))
      else if (!a_des(res$b, cle)) sprintf("%s ne donne pas cette grandeur", duo_nom_court(res$b))
      else if (!is.finite(d)) "aucune année en commun"
      else paste(c(duo_sens(d, dec)[duo_sens(d, dec) != "—"], lect_n), collapse = " ; ")
    data.frame(
      g = k$titre,
      a = fmt_val(va, k$unite, dec), b = fmt_val(vb, k$unite, dec),
      d = if (is.finite(d)) fmt_ecart(d, k$unite, dec) else "—",
      ea = if (is.finite(ea)) fmt_ecart(ea, k$unite, dec) else "—",
      eb = if (is.finite(eb)) fmt_ecart(eb, k$unite, dec) else "—",
      l = if (nzchar(lecture)) lecture else "—",
      stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, lignes)
  names(d) <- c("Grandeur", "A", "B", "Écart A − B", "Écart à la normale (A)", "Écart à la normale (B)", "Lecture")
  rownames(d) <- NULL
  d
}

#' Qualificatif d'une correlation, en mots.
#' @keywords internal
duo_mot_correlation <- function(r) {
  if (!is.finite(r)) return("non évaluable")
  r2 <- abs(r)
  if (r2 >= 0.9) "très forte" else if (r2 >= 0.7) "forte" else if (r2 >= 0.4) "modérée" else "faible"
}

#' Lecture de la comparaison, en quelques paragraphes.
#' @param res duo_preparer().
#' @return vecteur de paragraphes (texte).
#' @export
duo_lecture <- function(res) {
  a <- res$a; b <- res$b; ref <- res$ctx$ref_label
  na <- duo_nature(res$lieux$a); nb <- duo_nature(res$lieux$b)
  p <- sprintf("%s (A, %s) comparé à %s (B, %s) pour %d, chacun rapporté à sa propre normale %s.",
               duo_nom_court(a), na, duo_nom_court(b), nb, res$annee, ref)

  cles <- duo_cles_utiles(res)
  st <- lapply(cles, function(k) duo_stats(res, k))
  st <- Filter(function(s) s$n > 0L, st)
  if (!length(st)) {
    return(c(p, paste("Les deux lieux n'ont aucune année en commun pour les grandeurs choisies :",
                      "ils ne peuvent pas être comparés année par année.")))
  }

  # -- Periode commune
  n <- vapply(st, function(s) s$n, 1L)
  deb <- vapply(st, function(s) s$premiere, 1L); fin <- vapply(st, function(s) s$derniere, 1L)
  periode <- if (length(unique(paste(deb, fin, n))) == 1L) {
    sprintf("Les deux séries ne sont comparées que sur leurs années communes : %d–%d (%d année%s).",
            deb[1], fin[1], n[1], if (n[1] > 1L) "s" else "")
  } else {
    sprintf(paste("Les deux séries ne sont comparées que sur leurs années communes : de %d à %d années selon la",
                  "grandeur, entre %d et %d. Une grandeur que l'un des lieux ne mesure pas n'est pas comparée."),
            min(n), max(n), min(deb), max(fin))
  }
  p <- c(p, periode)

  # -- Ecart moyen A - B : les grandeurs ou il est le plus marque, rapporte a la variabilite
  sdv <- vapply(st, function(s) {
    v <- res$series$valeur[res$series$cle == s$cle & res$series$lieu == a]
    v <- v[is.finite(v)]
    if (length(v) >= 3L) stats::sd(v) else NA_real_
  }, 0)
  z <- ifelse(is.finite(sdv) & sdv > 0, abs(vapply(st, function(s) s$ecart_moyen, 0)) / sdv, 0)
  for (i in utils::head(order(-z), 3L)) {
    s <- st[[i]]; k <- CARTE_COUCHES[[s$cle]]
    dec <- if (as.integer(k$decimales) == 0L) 1L else as.integer(k$decimales)
    if (abs(s$ecart_moyen) < 0.5 * 10^(-dec)) next
    p <- c(p, sprintf("%s : en moyenne sur la période commune, A est %s %s B.",
                      k$titre, fmt_val(abs(s$ecart_moyen), k$unite, dec),
                      if (s$ecart_moyen > 0) "au-dessus de" else "en dessous de"))
  }

  # -- Correlation des valeurs annuelles
  cr <- Filter(function(s) is.finite(s$r), st)
  if (length(cr)) {
    cr <- utils::head(cr[order(match(vapply(cr, function(s) s$cle, ""), c("tmp", "pre", names(CARTE_COUCHES))))], 3L)
    p <- c(p, sprintf(
      "Les variations d'une année à l'autre sont liées entre les deux lieux : %s. Une corrélation forte dit que les années chaudes, ou sèches, le sont aux deux endroits ; elle ne dit pas que les valeurs sont égales.",
      paste(vapply(cr, function(s) sprintf("%s, corrélation %s (r = %s)", synthese_minuscule(CARTE_COUCHES[[s$cle]]$titre),
                                           duo_mot_correlation(s$r), fmt_fr(s$r, 2)), ""), collapse = " ; ")))
  }

  # -- Mise en garde, selon ce que sont les deux lieux
  tp <- c(res$lieux$a$type, res$lieux$b$type)
  if (any(tp == "station") && any(tp != "station")) {
    p <- c(p, paste(
      "Un des lieux est une mesure, l'autre un modèle : un écart entre eux n'est pas une erreur de l'un ou de l'autre.",
      "La maille de 8 km lisse le relief, la station décrit son emplacement (altitude, exposition, abri), et un département est une moyenne de mailles."))
  } else if (all(tp == "station")) {
    p <- c(p, "Deux stations décrivent deux emplacements : leur altitude, leur exposition et leurs lacunes de mesure diffèrent, et une année manquante dans l'une écarte cette année pour les deux.")
  } else if (any(tp == "zone")) {
    p <- c(p, "Un département est la moyenne, à poids égaux, de ses mailles de 8 km : il lisse les contrastes de relief que montre un point, et n'est pas pondéré par la surface forestière.")
  } else {
    p <- c(p, "Chaque point prend la valeur de sa maille de 8 km : deux points voisins peuvent tomber dans la même maille, donc avoir des valeurs identiques.")
  }
  p
}

#' Les ecarts A − B en CSV : une ligne par grandeur.
#' @export
duo_export_ecarts <- function(res) {
  cles <- duo_cles_utiles(res)
  if (!length(cles)) return(data.frame())
  l <- lapply(cles, function(cle) {
    ba <- res$bilan[res$bilan$lieu == res$a & res$bilan$cle == cle, , drop = FALSE]
    bb <- res$bilan[res$bilan$lieu == res$b & res$bilan$cle == cle, , drop = FALSE]
    s <- duo_stats(res, cle); k <- CARTE_COUCHES[[cle]]
    va <- if (nrow(ba)) ba$valeur[1] else NA_real_; vb <- if (nrow(bb)) bb$valeur[1] else NA_real_
    data.frame(
      lieu_a = cmp_texte_csv_sur(res$a), nature_a = duo_nature(res$lieux$a),
      lieu_b = cmp_texte_csv_sur(res$b), nature_b = duo_nature(res$lieux$b),
      grandeur = k$titre, unite = k$unite, annee = res$annee,
      valeur_a = va, valeur_b = vb, ecart_a_moins_b = va - vb,
      normale_a = if (nrow(ba)) ba$normale[1] else NA_real_,
      normale_b = if (nrow(bb)) bb$normale[1] else NA_real_,
      ecart_normale_a = if (nrow(ba)) ba$ecart[1] else NA_real_,
      ecart_normale_b = if (nrow(bb)) bb$ecart[1] else NA_real_,
      annees_communes = s$n, premiere_annee_commune = s$premiere, derniere_annee_commune = s$derniere,
      ecart_moyen_a_moins_b = s$ecart_moyen, correlation = s$r,
      stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, l)
  num <- vapply(d, is.double, logical(1))
  d[num] <- lapply(d[num], round, 3)
  rownames(d) <- NULL
  d
}

#' Lignes de provenance d'un CSV de la comparaison de deux lieux.
#' @export
duo_lignes_metadonnees <- function(res, jeu, titre) {
  l <- cmp_lignes_metadonnees(jeu, res$tl, res$ctx, res$annee, titre)
  c(l, sprintf("A : %s (%s) ; B : %s (%s)", res$a, duo_nature(res$lieux$a), res$b, duo_nature(res$lieux$b)),
    "Période : seules les années où les deux lieux ont une valeur sont comparées ; normales et tendances sont calculées sur ces années")
}


# ------------------------------------------------------------------------------
# 4. Les sources d'un point : la maille et les stations proches
# ------------------------------------------------------------------------------

#' Les sources proposees pour un point : la maille SAFRAN, puis les stations publiees les
#' plus pertinentes (le classement de la recherche en direct).
#'
#' @param jeu cmp_jeu() ; @param alt altitude du point (NA : critere ignore).
#' @return list(stations = data.frame ou NULL, publiees = logical, raison = texte ou NULL).
#'   `publiees` FALSE : le jeu n'a pas de stations (publie avant la v2.1) ; `raison` le dit.
#' @export
sources_point <- function(jeu, lon, lat, alt = NA_real_, executer = httr2::req_perform) {
  sj <- cmp_stations(jeu, executer = executer)
  if (!isTRUE(sj$ok)) {
    return(list(stations = NULL, publiees = FALSE,
                raison = if (identical(sj$raison, "invalide")) "invalide" else "absent"))
  }
  list(stations = stations_proches_publiees(sj, lon, lat, alt), publiees = TRUE, raison = NULL)
}

#' Les sources cochees par defaut : la maille, et la meilleure station si sa qualite n'est
#' pas « insuffisante ».
#' @keywords internal
sources_defaut <- function(src) {
  st <- src$stations
  c("maille", if (!is.null(st) && nrow(st) && st$niveau[1] <= 3L) st$id[1])
}

#' Les lieux du bilan d'un point, selon les sources cochees.
#'
#' Une seule source (la maille) : le lieu porte le nom du point, comme avant la v2.1. Plusieurs :
#' chaque source se nomme pour ce qu'elle est, « Maille SAFRAN (modèle) » ou « Station X (mesure) ».
#' @param p le point (nom, lon, lat) ; @param choix « maille » et/ou identifiants de stations ;
#' @param src sources_point().
#' @return liste de lieux.
#' @export
sources_lieux <- function(p, choix, src) {
  nom_p <- cmp_nom_propre(p$nom %||% "Point choisi", "Point choisi")
  st <- src$stations
  ids <- intersect(choix, if (is.null(st)) character(0) else st$id)
  maille <- "maille" %in% choix || !length(ids)      # jamais aucune source
  if (!length(ids)) return(list(cmp_lieu_point(nom_p, p$lon, p$lat)))
  noms <- vapply(ids, function(i) st$nom[st$id == i][1], "")
  # Deux stations de meme nom : l'identifiant les distingue.
  noms <- ifelse(duplicated(noms) | duplicated(noms, fromLast = TRUE), paste(noms, ids), noms)
  c(if (maille) list(cmp_lieu_point(stations_libelle_maille(), p$lon, p$lat)),
    lapply(seq_along(ids), function(j)
      cmp_lieu_station(st[st$id == ids[j], , drop = FALSE][1, ], sprintf("%s (mesure)", noms[j]))))
}

#' Plusieurs sources pour un meme point ?
#' @keywords internal
sources_multiples <- function(res) {
  identical(res$echelle, "point") && nrow(res$tl) >= 2L
}

#' Nom de la source « maille » d'un bilan de point (la maille SAFRAN), ou NA.
#' @keywords internal
sources_nom_maille <- function(res) {
  l <- res$tl$lieu[res$tl$source == "maille"]
  if (length(l)) l[1] else NA_character_
}

#' Phrases d'une grandeur quand le point a plusieurs sources : la lecture de chacune, puis ce
#' qui separe chaque station de la maille.
#' @param res synthese_preparer("point", ...) a plusieurs lieux ; @param cle grandeur.
#' @export
sources_phrases_cle <- function(res, cle) {
  b <- res$bilan[res$bilan$cle == cle, , drop = FALSE]
  if (!nrow(b)) return(character(0))
  k <- CARTE_COUCHES[[cle]]
  p <- character(0)
  for (l in intersect(res$tl$lieu, b$lieu)) {
    r <- b[b$lieu == l, , drop = FALSE][1, ]
    rg <- res$rang[res$rang$lieu == l & res$rang$cle == cle, , drop = FALSE]
    # « (Ancienne, mesure) » plutot que « (Ancienne (mesure)) » : pas de parentheses imbriquees.
    r$lieu <- sprintf("%s, %s", duo_nom_court(l), if (identical(res$tl$source[match(l, res$tl$lieu)], "station")) "mesure" else "modèle")
    p <- c(p, synthese_phrases_ligne(r, rg, res$ctx, res$annee, avec_lieu = TRUE))
  }
  m <- sources_nom_maille(res)
  if (!is.na(m)) {
    bm <- b[b$lieu == m, , drop = FALSE]
    for (l in setdiff(b$lieu, m)) {
      bs <- b[b$lieu == l, , drop = FALSE]
      if (!nrow(bm) || !nrow(bs) || !is.finite(bm$valeur[1]) || !is.finite(bs$valeur[1])) next
      dec <- bs$decimales[1]; u <- if (nzchar(k$unite)) k$unite else ""
      accord <- if (is.finite(bm$ecart[1]) && is.finite(bs$ecart[1])) {
        if (identical(bm$classe[1], bs$classe[1])) "même lecture" else "lectures différentes"
      } else NULL
      p <- c(p, sprintf("%s : la station %s donne %s, la maille %s (station − maille : %s)%s.",
                        k$titre, duo_nom_court(l), fmt_val(bs$valeur[1], u, dec), fmt_val(bm$valeur[1], u, dec),
                        fmt_ecart(bs$valeur[1] - bm$valeur[1], u, dec),
                        if (!is.null(accord) && is.finite(bs$ecart[1]))
                          sprintf(" ; écart à la normale %s pour la station, %s pour la maille (%s)",
                                  fmt_ecart(bs$ecart[1], u, dec), fmt_ecart(bm$ecart[1], u, dec), accord) else ""))
    }
  }
  p
}

#' Phrases generales d'un point a plusieurs sources.
#' @export
sources_phrases_generales <- function(res) {
  b <- res$bilan[is.finite(res$bilan$valeur) & is.finite(res$bilan$normale), , drop = FALSE]
  nom <- res$point_nom %||% "ce point"
  lieux <- res$tl$lieu[res$tl$retenu]
  m <- sources_nom_maille(res)
  n_st <- sum(res$tl$source[res$tl$retenu] == "station")
  p <- sprintf("%s : bilan de %d comparé à la normale %s, d'après %s.", nom, res$annee, res$ctx$ref_label,
               paste(c(if (!is.na(m) && m %in% lieux) "la maille SAFRAN (modèle)",
                       if (n_st) sprintf("%d station%s Météo-France (mesure)", n_st, if (n_st > 1L) "s" else "")),
                     collapse = " et "))
  t <- b[b$cle == "tmp", , drop = FALSE]
  if (nrow(t) >= 2L) {
    p <- c(p, sprintf("Température moyenne annuelle, écart à la normale : %s.",
                      paste(vapply(seq_len(nrow(t)), function(i) sprintf("%s %s", t$lieu[i],
                                  fmt_ecart(t$ecart[i], t$unite[i], t$decimales[i])), ""), collapse = " ; ")))
  }
  if (!is.na(m) && n_st) {
    for (l in setdiff(unique(b$lieu), m)) {
      cles <- intersect(unique(b$cle[b$lieu == l]), unique(b$cle[b$lieu == m]))
      if (!length(cles)) next
      ok <- vapply(cles, function(k) {
        ca <- b$classe[b$lieu == l & b$cle == k][1]; cm <- b$classe[b$lieu == m & b$cle == k][1]
        identical(ca, cm)
      }, TRUE)
      p <- c(p, sprintf("La station %s et la maille ont la même lecture de la normale pour %d grandeur%s sur %d.",
                        duo_nom_court(l), sum(ok), if (sum(ok) > 1L) "s" else "", length(cles)))
    }
  }
  p
}


# ------------------------------------------------------------------------------
# 5. Le choix des sources, a l'ecran
# ------------------------------------------------------------------------------

#' Nom du point, tel qu'il titre le bilan.
#' @keywords internal
sources_nom_point <- function(p) cmp_nom_propre(p$nom %||% "Point choisi", "Point choisi")

#' Detail de chaque source retenue, en mots, nomme par le nom du lieu du bilan : pour la
#' synthese a envoyer et les exports.
#' @param lieux sources_lieux() ; @param src sources_point().
#' @return vecteur nomme (nom du lieu -> detail), vide pour la maille seule.
#' @keywords internal
sources_details <- function(lieux, src) {
  st <- src$stations
  d <- vapply(lieux, function(l) {
    if (!identical(l$type, "station") || is.null(st)) return("")
    s <- st[st$id == l$id, , drop = FALSE]
    if (!nrow(s)) "" else stations_detail(s[1, ])
  }, "")
  names(d) <- vapply(lieux, function(l) l$nom, "")
  d[nzchar(d)]
}

#' Le groupe de cases « Sources des valeurs » : la maille SAFRAN (modele) et les stations
#' publiees proches (mesures), avec leur qualite.
#'
#' La qualite est celle de la recherche en direct (distance, ecart d'altitude, fraicheur,
#' donnees) : mf_pastille() donne symbole ET texte, jamais la couleur seule.
#' @param inputId identifiant de l'entree (deja passe par ns()).
#' @param src sources_point() ; @param selection cases cochees actuellement (ou NULL).
#' @return balise.
#' @export
sources_choix_ui <- function(inputId, src, selection = NULL) {
  maille <- shiny::span(class = "cf-source__opt",
                        shiny::strong(stations_libelle_maille()),
                        shiny::span(class = "cf-source__optmeta", "maille de 8 km · depuis 1959"))
  note <- if (!isTRUE(src$publiees)) {
    shiny::p(class = "cf-aide", if (identical(src$raison, "invalide"))
      "Les stations publiées avec ce jeu de données sont illisibles : seule la maille SAFRAN est utilisée."
    else "Ce jeu de données ne contient pas encore de stations Météo-France : seule la maille SAFRAN est utilisée.")
  } else if (is.null(src$stations) || !nrow(src$stations)) {
    shiny::p(class = "cf-aide", sprintf("Aucune station publiée à moins de %s km de ce point : seule la maille SAFRAN est utilisée.",
                                        fmt_fr(CONFIG$stations$rayon_km, 0)))
  }
  st <- src$stations
  if (is.null(st) || !nrow(st)) return(shiny::div(class = "cf-sources", maille, note))
  noms <- c(list(maille), lapply(seq_len(nrow(st)), function(i) {
    s <- st[i, , drop = FALSE]
    shiny::span(class = "cf-source__opt",
                shiny::strong(s$nom), " ", mf_pastille(s$niveau, tolower(s$libelle)),
                shiny::span(class = "cf-source__optmeta", paste0("mesure · ", stations_detail(s, qualite = FALSE))),
                mf_motifs_qualite(s))
  }))
  valeurs <- c("maille", st$id)
  sel <- intersect(selection %||% sources_defaut(src), valeurs)
  if (!length(sel)) sel <- "maille"
  shiny::tags$fieldset(
    class = "cf-sources",
    shiny::tags$legend(class = "cf-sources__legende", "Sources des valeurs"),
    shiny::checkboxGroupInput(inputId, NULL, choiceNames = noms, choiceValues = valeurs,
                              selected = sel, width = "100%"),
    shiny::p(class = "cf-aide", paste(
      "La maille est un modèle de Météo-France (8 km) ; une station est une mesure faite à son emplacement.",
      "Cochez les deux pour les voir côte à côte : leur écart n'est pas une erreur de l'une ou de l'autre.")))
}
