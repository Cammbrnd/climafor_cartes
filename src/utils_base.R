# ==============================================================================
# utils_base.R — LE BILAN D'UN POINT LU DANS LA BASE PRECALCULEE (v2.2)
# ------------------------------------------------------------------------------
# PRINCIPE. Le bilan d'un point calcule ses indicateurs depuis la serie JOURNALIERE de la
# maille SAFRAN : soixante-six annees, soixante-six requetes. Or le precalcul de la carte
# (outils/carte_precalcul.R) a deja calcule, pour chaque maille et chaque annee complete,
# presque tout ce que le bilan montre (src/carte_couches_base.R). Ce fichier LIT ce jeu
# publie, et ne demande a SAFRAN que ce qui manque :
#
#   - les annees posterieures au jeu (l'annee en cours, jamais publiee) ;
#   - l'annee comparee, pour les profils du mois et du jour.
#
# DEUX CHEMINS, UN SEUL CALCUL. Le chemin complet (serie journaliere entiere) reste le
# chemin PAR DEFAUT. Le chemin rapide ne fait que recomposer le tableau annuel :
#   lignes des annees publiees  = valeurs lues (colonnes de la base)
#   lignes des annees chargees  = calcul habituel de l'indicateur sur ces annees seules
#   puis enrichir_annuel() sur l'ensemble : MEME normale, MEME ecart-type, MEME anomalie.
# Les valeurs qui ne dependent pas de l'annee (normale mensuelle, normale du jour, seuil de
# vent) sont lues dans les references de la base et glissees dans le contexte (`ctx$base`),
# que les fonctions de calcul consultent avant de recalculer. tests/test_v22.R compare les
# deux chemins indicateur par indicateur : c'est la garantie qu'ils ne divergent pas.
#
# QUAND LA BASE NE VAUT PLUS. Les valeurs publiees sont calculees avec la reference, la
# fenetre et les seuils PAR DEFAUT. Des que l'utilisateur en change un, ou que le jeu ne
# porte pas toute la base, base_applicable() le dit et l'indicateur n'est PAS calcule en
# mode rapide : on ne montre jamais un nombre calcule avec d'autres reglages que ceux affiches.
#
# LES STATIONS (v2.3). Meme principe, mais une station a des trous : le bilan ecarte d'abord
# les annees dont moins de 95 % des jours sont renseignes. Le jeu de stations publie donc, en
# plus, la validite de chaque annee, les jours manquants et des references PAR INDICATEUR
# (src/carte_couches_base_stations.R). Cote lecture :
#   base_ouvrir_station()           retrouve la station dans le jeu publie ;
#   base_station_attributs()        complete la qualite des seules annees chargees par celle de
#                                   TOUTES les annees (c'est elle qui fait ecarter les memes annees) ;
#   base_station_colonnes_absentes() juge « variable non mesuree » sur la base, pas sur l'annee
#                                   chargee (un capteur arrete cette annee a mesure avant).
# tests/test_v22_stations.R compare les deux chemins sur des stations aux trous reels.
#
# CE QUE LE MODE RAPIDE NE FAIT PAS. La synthese chiffree « a periode egale » compare les
# jours de l'annee en cours aux memes jours de chaque annee de reference : elle exige la
# serie journaliere de la reference, qu'aucun jeu annuel ne remplace. Elle demande donc le
# chargement complet (un bouton le declenche).
# ==============================================================================

.ETAT_BASE <- new.env(parent = emptyenv())

# NB. meta.json est lu en liste : `meta$base` completerait en silence `base_parametres` quand
# `base` est absent (appariement partiel de `$`). Tout acces a ces deux champs passe par
# [["..."]] ; tests/test_v22.R garde le cas.

# Colonne journaliere -> reference mensuelle de la base.
BASE_REF_MOIS <- c(p_tot = "r_mois_p", t_moy = "r_mois_t", p_sol = "r_mois_neige",
                   etp = "r_mois_etp", vpd = "r_mois_vpd", ff = "r_mois_ff")


# ------------------------------------------------------------------------------
# 1. Lecture d'une maille dans le jeu publie
# ------------------------------------------------------------------------------

#' Le jeu est-il celui des stations (base_ouvrir_station) et non celui des mailles ?
#' [["type"]] et non $ : un appariement partiel ne doit jamais decider du type.
#' @keywords internal
base_est_station <- function(jeu) identical(jeu[["type"]], "station")

#' Entree de la carte ou de la base portant cette cle, ou NULL.
#' @keywords internal
base_entree <- function(jeu, cle) {
  for (k in c(jeu$meta$couches, jeu$meta[["base"]])) {
    if (identical(as.character(k$cle)[1], cle)) return(k)
  }
  NULL
}

#' Les valeurs d'une entree pour UNE maille (ou UNE station) : une par annee, mois ou jour selon
#' l'entree.
#'
#' Memoise pour la session (cle : jeu, entree, lieu) : un meme point relu par la barre de
#' contexte, le bilan et le rapport ne releit pas les fichiers. Une entree absente est une
#' ERREUR, jamais un vecteur vide : un calcul qui prendrait une reference manquante pour
#' « pas de reference » rendrait un nombre faux. Seule exception : une entree que le jeu declare
#' VIDE (`base_parametres$vides` : aucune station ne la renseigne) rend des NA.
#' @param jeu resultat de cmp_jeu() (mailles) ou de base_jeu_station() (stations) ;
#'   @param i rang du lieu dans les fichiers (base 1).
#' @return numeric (NA = absente).
#' @keywords internal
base_lire <- function(jeu, cle, i, executer = httr2::req_perform) {
  mem <- paste(jeu[["type"]] %||% "maille", jeu$meta$genere_le %||% "",
               jeu$transport$base %||% jeu$transport$dossier %||% "", cle, i, sep = "|")
  if (!is.null(.ETAT_BASE[[mem]])) return(.ETAT_BASE[[mem]])
  k <- base_entree(jeu, cle)
  if (is.null(k)) {
    vides <- unlist(jeu$meta[["base_parametres"]][["vides"]])
    reg <- if (base_est_station(jeu)) STATIONS_BASE_COUCHES[[cle]] else NULL
    if (cle %in% vides && !is.null(reg)) {
      return(rep(NA_real_, if (identical(reg$type, "ref")) as.integer(reg$lignes) else length(jeu$annees)))
    }
    stop(sprintf("base précalculée : entrée « %s » absente du jeu publié", cle))
  }
  f <- cmp_fichier_carte(as.character(k$fichier)[1], jeu$transport, executer = executer)
  nl <- if (identical(as.character(k$type %||% "annuel")[1], "ref")) as.integer(k$lignes)
        else length(jeu$annees)
  l <- carte_lire_colonne(f, jeu[["n"]] %||% as.integer(jeu$meta$n_mailles), nl, as.integer(k$stock), as.integer(i))
  if (!isTRUE(l$taille_exacte)) stop(sprintf("base précalculée : fichier de « %s » de taille inattendue", cle))
  if (length(ls(.ETAT_BASE)) > 4000L) rm(list = ls(.ETAT_BASE), envir = .ETAT_BASE)
  assign(mem, l$valeurs, envir = .ETAT_BASE)
  l$valeurs
}

#' Ouvre la base pour un point : le jeu publie et la maille la plus proche.
#' @return list(ok, jeu, i, note) ; sinon list(ok = FALSE, raison, message).
#' @keywords internal
base_ouvrir <- function(lon, lat, transport = carte_transport(), executer = httr2::req_perform) {
  non <- function(raison, message) list(ok = FALSE, raison = raison, message = message)
  jeu <- tryCatch(cmp_jeu(transport, executer = executer), error = function(e) NULL)
  if (is.null(jeu)) return(non("jeu_indisponible", "Les données précalculées ne sont pas joignables."))
  if (!length(jeu$meta[["base"]])) {
    return(non("sans_base", "Le jeu publié ne porte pas encore la base précalculée du bilan."))
  }
  if (!requireNamespace("sf", quietly = TRUE)) return(non("sans_sf", "Le paquet sf est absent."))
  m <- tryCatch(cmp_mailles(cmp_lieu_point("Point", lon, lat), jeu, executer), error = function(e) NULL)
  if (is.null(m) || !length(m$idx)) {
    return(non("hors_grille", "Le point est hors de la grille SAFRAN publiée."))
  }
  list(ok = TRUE, jeu = jeu, i = as.integer(m$idx[1]), note = m$note)
}

#' Ouvre la base pour une STATION : le jeu de stations publie a cote de la carte et le rang de
#' la station dans ses fichiers.
#'
#' Le « jeu » rendu a la forme de celui des mailles (meta, transport, annees) pour que
#' base_lire(), base_applicable() et base_annuel() n'aient qu'un seul chemin ; `type` et `n`
#' disent qu'il s'agit de stations.
#' @param id identifiant Meteo-France de la station.
#' @return list(ok, jeu, i, note) ; sinon list(ok = FALSE, raison, message).
#' @keywords internal
base_ouvrir_station <- function(id, transport = carte_transport(), executer = httr2::req_perform) {
  non <- function(raison, message) list(ok = FALSE, raison = raison, message = message)
  jeu <- tryCatch(cmp_jeu(transport, executer = executer), error = function(e) NULL)
  sj <- if (is.null(jeu)) NULL else tryCatch(cmp_stations(jeu, executer = executer), error = function(e) NULL)
  if (is.null(sj) || !isTRUE(sj$ok)) {
    return(non("jeu_indisponible", "Les stations précalculées ne sont pas joignables."))
  }
  if (!length(sj$meta[["base"]])) {
    return(non("sans_base", "Le jeu de stations publié ne porte pas encore la base précalculée du bilan."))
  }
  i <- match(as.character(id), sj$table$id)
  if (is.na(i)) return(non("station_non_publiee", "Cette station n'est pas dans le jeu précalculé."))
  list(ok = TRUE, i = i, note = sprintf("station %s", sj$table$nom[i]),
       jeu = list(type = "station", n = nrow(sj$table), meta = sj$meta, transport = sj$transport,
                  annees = sj$annees))
}


# ------------------------------------------------------------------------------
# 2. Validite : la base ne vaut que pour les reglages par defaut
# ------------------------------------------------------------------------------

#' Empreinte des reglages dont depend la base : fenetre de la saison et seuils.
#' Ecrite dans meta.json au precalcul, recalculee ici sur le contexte courant.
#' @keywords internal
base_empreinte <- function(ctx) {
  s <- unlist(ctx$seuils)
  paste0(paste(as.integer(ctx$fenetre), collapse = "-"), "|",
         paste(names(s), vapply(s, function(x) format(x, digits = 10, trim = TRUE), ""),
               sep = "=", collapse = ";"))
}

#' La base peut-elle servir pour ce contexte ?
#' @return list(ok, raison, message).
#' @keywords internal
base_applicable <- function(ctx, jeu) {
  non <- function(raison, message) list(ok = FALSE, raison = raison, message = message)
  m <- jeu$meta
  # Une entree que le jeu declare vide (aucune station ne la renseigne) compte comme presente.
  cles <- c(vapply(m[["base"]] %||% list(), function(k) as.character(k$cle)[1], ""),
            as.character(unlist(m[["base_parametres"]][["vides"]])))
  attendues <- names(if (base_est_station(jeu)) STATIONS_BASE_COUCHES else BASE_COUCHES)
  if (!all(attendues %in% cles)) {
    return(non("base_incomplete", "Le jeu publié ne porte pas toute la base précalculée."))
  }
  if (!identical(as.character(m$reference %||% "")[1], ctx$ref_label)) {
    return(non("reference", sprintf(
      "La base précalculée est calculée sur la référence %s ; vous avez choisi %s.",
      as.character(m$reference %||% "?")[1], ctx$ref_label)))
  }
  if (!identical(as.character(m[["base_parametres"]][["empreinte"]] %||% "")[1], base_empreinte(ctx))) {
    return(non("reglages", paste(
      "La fenêtre de saison ou les seuils ne sont plus ceux de la base précalculée.")))
  }
  list(ok = TRUE, raison = NA_character_, message = "")
}

#' Resultat vide portant les attributs que l'interface affiche (comme calculer_un_indicateur).
#' @keywords internal
base_vide <- function(raison, message) {
  r <- data.frame()
  attr(r, "raison") <- raison
  attr(r, "message") <- message
  r
}


# ------------------------------------------------------------------------------
# 3. References de la base, glissees dans le contexte (lues a la demande)
# ------------------------------------------------------------------------------

#' Environnement `ctx$base` : chaque reference n'est lue qu'au premier usage (promesse).
#' Un calcul qui n'en consulte aucune ne telecharge rien.
#' @param ind_id indicateur calcule : pour une station, la reference depend de lui.
#' @keywords internal
base_refs <- function(jeu, i, executer = httr2::req_perform, ind_id = NULL) {
  e <- new.env(parent = emptyenv())
  if (base_est_station(jeu)) {
    # Station : les references sont celles de l'INDICATEUR (jours valides pour lui, jours ou ses
    # variables existent toutes) : voir src/carte_couches_base_stations.R.
    ajouter <- function(nom, cle) {
      force(cle)
      delayedAssign(nom, base_lire(jeu, cle, i, executer), assign.env = e)
    }
    r <- base_refs_station(ind_id)
    for (nom in names(r)) ajouter(nom, r[[nom]])
    return(e)
  }
  for (col in names(BASE_REF_MOIS)) local({
    cle <- BASE_REF_MOIS[[col]]
    delayedAssign(paste0("mois_", col), base_lire(jeu, cle, i, executer), assign.env = e)
  })
  delayedAssign("seuil_vent", base_lire(jeu, "r_seuil_ff", i, executer), assign.env = e)
  delayedAssign("normale_jour_tmax", base_lire(jeu, "r_jour_tmax", i, executer), assign.env = e)
  delayedAssign("normale_jour_tmoy", base_lire(jeu, "r_jour_tmoy", i, executer), assign.env = e)
  e
}


# ------------------------------------------------------------------------------
# 4. Recomposition des tableaux annuels
# ------------------------------------------------------------------------------

# Chaque indicateur annuel : les colonnes LUES dans la base (`col`), la colonne dont on tire
# la normale (`cle`), les variables journalieres dont depend « un jour manquant » (`vars`,
# comme dans calc_*), la fenetre de saison (`fenetre` : NULL = annee civile, "ctx" = celle du
# contexte, ou c(mois, mois)), la colonne lissee (`lissee`), les colonnes DERIVEES (`derive`),
# et l'ordre des colonnes (`ordre`, celui de calc_*). `enrichir = "ete"` pour l'ete, qui porte
# la normale de deux grandeurs.
BASE_INDICATEURS <- list(
  tmp  = list(col = "t_moy", cle = "t_moy", vars = "t_moy", lissee = "t_moy"),
  jja  = list(col = c("t_jja", "p_jja"), cle = "t_jja", vars = c("t_moy", "p_tot"),
              fenetre = c(6L, 8L), enrichir = "ete"),
  can  = list(col = c("n_t1", "n_t2", "n_t3"), cle = "n_t2", vars = "t_max"),
  vag  = list(col = c("n_vagues", "n_jours_vague", "duree_max", "ecart_max"), cle = "n_vagues",
              vars = "t_max", lissee = "n_vagues"),
  gel  = list(col = c("n_gel", "n_gel_severe", "n_gel_tardif", "doy_dernier_gel"), cle = "n_gel",
              vars = "t_min"),
  pre  = list(col = c("p_liq", "p_sol", "p_tot"), cle = "p_tot", vars = "p_tot", lissee = "p_tot",
              derive = function(r, ctx) { r$part_neige <- 100 * r$p_sol / pmax(r$p_tot, 1e-9); r }),
  neige = list(col = c("p_sol", "p_tot", "n_jours_neige", "neige_3j_max"), cle = "p_sol", vars = "p_sol",
               derive = function(r, ctx) { r$part_neige <- 100 * r$p_sol / pmax(r$p_tot, 1e-9); r },
               ordre = c("p_sol", "p_tot", "part_neige", "n_jours_neige", "neige_3j_max")),
  swi  = list(col = c("swi_moy", "swi_min", "n_stress"), cle = "swi_moy", vars = "swi",
              fenetre = c(6L, 8L)),
  dry  = list(col = c("duree_max", "n_sequences", "n_jours_secs"), cle = "duree_max", vars = "p_tot"),
  # La classe est lue en code (1 a 6) pour les annees publiees et derivee du facteur calcule pour
  # les annees chargees, puis remise en facteur avec les memes niveaux que calc_martonne().
  mart = list(col = c("p_an", "t_an", "i_annuel", "i_estival", "classe_code"), cle = "i_annuel",
              vars = c("p_tot", "t_moy"),
              vives = function(v) { v$classe_code <- as.integer(v$classe); v },
              derive = function(r, ctx) {
                niv <- c("Hyperaride", "Aride", "Semi-aride", "Subhumide", "Humide", "Tres humide")
                r$classe <- factor(niv[r$classe_code], levels = niv)
                r$classe_code <- NULL
                r },
              ordre = c("p_an", "t_an", "i_annuel", "i_estival", "classe"),
              apres = function(r) { r$classe[r$partielle] <- NA; r }),
  gdd  = list(col = "gdd", cle = "gdd", vars = "t_moy", lissee = "gdd"),
  veg  = list(col = c("doy_debut", "doy_fin", "duree"), cle = "duree", vars = "t_moy"),
  petp = list(col = c("p", "etp", "petp"), cle = "petp", vars = c("p_tot", "etp"),
              fenetre = "ctx", lissee = "petp"),
  vpd  = list(col = c("vpd_moy", "vpd_jja", "n_stress"), cle = "vpd_moy", vars = "vpd"),
  vent = list(col = c("ff_moy", "ff_max", "n_ventes"), cle = "ff_moy", vars = "ff"),
  vent_soutenu = list(col = "n_p95", cle = "n_p95", vars = "ff",
                      derive = function(r, ctx) { r$seuil_p95 <- as.numeric(ctx$base$seuil_vent); r },
                      ordre = c("n_p95", "seuil_p95")),
  vent_sol = list(col = c("n_combo", "n_sature"), cle = "n_combo", vars = c("ff", "swi"),
                  derive = function(r, ctx) {
                    r$seuil_p95 <- as.numeric(ctx$base$seuil_vent)
                    r$seuil_swi <- ctx$seuils$swi_sature %||% CONFIG$seuils$swi_sature
                    r },
                  ordre = c("n_combo", "n_sature", "seuil_p95", "seuil_swi"))
)

# Indicateurs dont le calcul n'a besoin que des references de la base et de l'annee comparee :
# on lance leur calcul habituel sur les jours charges.
BASE_INDICATEURS_DIRECTS <- c("bh", "ombro", "pre_mois", "tmp_mois", "neige_mois", "vpd_mois",
                              "vent_mois", "tmp_jour")

# Indicateurs ANNUELS qui consultent aussi une reference (normale du jour, seuil de vent).
BASE_REFS_STATION_INDICATEURS <- c("vag", "vent_soutenu", "vent_sol")

#' Completude d'annees entieres et sans trou : celle de la base, qui ne publie que des annees
#' completes. Calculee par completude() sur un calendrier fabrique, pour que « jours attendus »
#' et « dernier jour » s'entendent exactement comme dans le calcul habituel (saison a cheval
#' sur deux annees comprise).
#' @keywords internal
base_completude <- function(annees, fenetre = NULL, vars = NULL) {
  if (!length(annees)) return(completude(NULL))
  jours <- seq(as.Date(sprintf("%d-01-01", min(annees))), as.Date(sprintf("%d-12-31", max(annees))), by = "day")
  an <- as.integer(format(jours, "%Y"))
  jours <- jours[an %in% annees]
  cal <- data.frame(date = jours, annee = as.integer(format(jours, "%Y")),
                    mois = as.integer(format(jours, "%m")))
  for (v in c("t_moy", "t_min", "t_max", "p_tot", "etp", "swi", "ff", "vpd")) cal[[v]] <- 0
  completude(cal, fenetre, vars)
}

.BASE_COMP <- c("annee", "n_jours", "n_manquants", "partielle", "dernier_jour")

#' Tableau annuel d'un indicateur : annees publiees (lues) + annees chargees (calculees).
#' @keywords internal
base_annuel <- function(d, ind, don, ctx, rap, executer = httr2::req_perform) {
  jeu <- rap$jeu; i <- rap$i
  b <- data.frame(annee = jeu$annees)
  for (c0 in d$col) {
    cle <- base_cle_colonne(ind$id, c0)
    if (is.na(cle)) stop(sprintf("base précalculée : colonne %s.%s non publiée", ind$id, c0))
    b[[c0]] <- base_lire(jeu, cle, i, executer)
  }
  b <- b[rowSums(is.finite(as.matrix(b[, d$col, drop = FALSE]))) > 0, , drop = FALSE]

  ctx_b <- ctx
  ctx_b$base <- base_refs(jeu, i, executer, ind$id)
  fenetre <- if (identical(d$fenetre, "ctx")) ctx$fenetre else d$fenetre

  # Annees chargees (hors jeu) : le calcul habituel, sur ces annees seules. Il rend aussi la
  # completude de ces annees (une annee en cours est partielle).
  d_vives <- don[!don$annee %in% jeu$annees, , drop = FALSE]
  vives <- if (nrow(d_vives)) ind$calc(d_vives, ctx_b) else NULL
  if (!is.data.frame(vives) || !nrow(vives)) vives <- NULL

  if (!nrow(b) && is.null(vives)) return(base_vide("vide", "Aucun résultat pour cet indicateur sur la période choisie."))

  comp <- if (nrow(b)) base_completude(b$annee, fenetre, d$vars) else NULL
  comp <- if (is.null(comp)) NULL else comp[, .BASE_COMP]
  # Une station a des jours sans mesure dans une annee valide : ils sont publies, pas supposes nuls.
  if (!is.null(comp) && base_est_station(jeu)) {
    mq <- base_lire(jeu, base_cle_manquants(d$vars, fenetre), i, executer)[match(comp$annee, jeu$annees)]
    comp$n_manquants <- as.integer(ifelse(is.finite(mq), mq, 0))
  }
  if (!is.null(vives)) {
    if (!is.null(d$vives)) vives <- d$vives(vives)
    manque <- setdiff(d$col, names(vives))
    for (c0 in manque) vives[[c0]] <- NA_real_
    b <- rbind(b[, c("annee", d$col), drop = FALSE], vives[, c("annee", d$col), drop = FALSE])
    comp <- rbind(comp, vives[, .BASE_COMP])
  }
  b <- b[order(b$annee), , drop = FALSE]
  rownames(b) <- NULL

  if (!is.null(d$derive)) b <- d$derive(b, ctx_b)
  # Colonnes derivees a la suite des colonnes lues, sauf ordre explicite (celui de calc_*).
  b <- b[, c("annee", d$ordre %||% c(d$col, setdiff(names(b), c("annee", d$col)))), drop = FALSE]

  res <- if (identical(d$enrichir, "ete")) enrichir_ete(b, comp, ctx)
         else enrichir_annuel(b, NULL, ctx, d$cle, comp = comp)
  if (!is.null(d$apres)) res <- d$apres(res)
  if (!is.null(d$lissee)) res$lissee <- lissage_annuel(res$annee, res[[d$lissee]], exclure = res$partielle)
  res
}

#' Calcule un indicateur en mode rapide.
#'
#' Appelee par calculer_un_indicateur() quand les donnees portent l'attribut « base ».
#' @param don les jours CHARGES (annees posterieures au jeu et annee comparee) ;
#'   @param rap list(jeu, i) : le jeu publie et la maille du point.
#' @return tableau annuel ou profil, comme ind$calc ; ou un resultat vide qui dit pourquoi.
#' @keywords internal
base_calculer <- function(don, ind, ctx, rap, executer = httr2::req_perform) {
  ok <- base_applicable(ctx, rap$jeu)
  if (!ok$ok) return(base_vide(paste0("rapide_", ok$raison), paste(
    ok$message, "Chargez la série complète pour ce réglage.")))
  # Un indicateur de station qui consulte une reference sans qu'elle soit publiee ne doit PAS la
  # recalculer sur les seuls jours charges : ce serait une normale sur une seule annee.
  if (base_est_station(rap$jeu) && !length(base_refs_station(ind$id)) &&
      ind$id %in% c(BASE_INDICATEURS_DIRECTS, BASE_REFS_STATION_INDICATEURS)) {
    return(base_vide("rapide_inconnu", "Cet indicateur demande la série complète pour une station."))
  }
  if (ind$id %in% BASE_INDICATEURS_DIRECTS) {
    ctx_b <- ctx
    ctx_b$base <- base_refs(rap$jeu, rap$i, executer, ind$id)
    return(ind$calc(don, ctx_b))
  }
  d <- BASE_INDICATEURS[[ind$id]]
  if (is.null(d)) {
    return(base_vide("rapide_inconnu", "Cet indicateur demande la série complète."))
  }
  base_annuel(d, ind, don, ctx, rap, executer)
}

#' Complete la qualite d'une station chargee en mode rapide avec celle des annees publiees.
#'
#' Le chargement ne porte que les annees que le jeu n'a pas : les attributs `annees_valides` et
#' `qualite_annuelle` ne decriraient que celles-la. Or ils servent a trois choses qui regardent
#' TOUTE la serie : ecarter les annees trop lacunaires (donnees_pour_indicateur), dire lesquelles
#' l'ont ete (la note des graphiques) et juger la couverture de la reference (mf_qualite_serie).
#' Ici, les annees publiees prennent leur validite et leur part de jours renseignes dans la
#' base ; une annee chargee garde la sienne, calculee sur ses jours.
#' @param don serie de la station (station_charger) ; @param rap list(jeu, i) de base_ouvrir_station.
#' @return `don`, ses deux attributs completes.
#' @keywords internal
base_station_attributs <- function(don, rap, executer = httr2::req_perform) {
  jeu <- rap$jeu; i <- rap$i
  groupes <- c("p", "t", "ff")
  lire <- function(prefixe) lapply(stats::setNames(groupes, groupes), function(g)
    base_lire(jeu, sprintf("%s_%s", prefixe, g), i, executer))
  frac <- lire("b_q_frac"); ok <- lire("b_q_ok")

  q_base <- data.frame(annee = jeu$annees, frac_p = frac$p, frac_t = frac$t, frac_ff = frac$ff)
  q_base <- q_base[rowSums(is.finite(as.matrix(q_base[, -1L, drop = FALSE]))) > 0, , drop = FALSE]
  q_vive <- attr(don, "qualite_annuelle")
  q_base <- q_base[!q_base$annee %in% q_vive$annee, , drop = FALSE]
  q <- if (is.null(q_vive)) q_base else rbind(q_base, q_vive[, names(q_base), drop = FALSE])
  q <- q[order(q$annee), , drop = FALSE]
  rownames(q) <- NULL

  v_vive <- attr(don, "annees_valides")
  valides <- lapply(stats::setNames(groupes, groupes), function(g) {
    publiees <- setdiff(jeu$annees[ok[[g]] %in% 1], q_vive$annee)
    sort(unique(c(publiees, v_vive[[g]])))
  })
  attr(don, "qualite_annuelle") <- q
  attr(don, "annees_valides") <- valides
  don
}

#' Parmi les colonnes absentes des jours charges, celles que la station n'a JAMAIS mesurees.
#'
#' En chargement complet, une colonne est « non mesuree » quand elle est vide sur toute la serie.
#' En mode rapide, les jours charges ne sont qu'une fin de serie : un capteur de vent arrete cette
#' annee a mesure le vent les annees precedentes, et la base le sait (part des jours renseignes,
#' completee par base_station_attributs). Une colonne qu'aucun groupe de la station ne couvre
#' (SWI, neige au sol, humidite : une station ne les mesure pas) est toujours absente.
#' @param colonnes colonnes requises et absentes des jours charges ; @param don serie chargee,
#'   ses attributs deja completes.
#' @return le sous-ensemble de `colonnes` que la station n'a jamais mesure.
#' @keywords internal
base_station_colonnes_absentes <- function(colonnes, don) {
  q <- attr(don, "qualite_annuelle")
  groupe <- c(p_tot = "p", t_moy = "t", t_min = "t", t_max = "t", etp = "t", ff = "ff")
  mesuree <- function(col) {
    g <- unname(groupe[col])
    !is.na(g) && !is.null(q) && any(q[[paste0("frac_", g)]] > 0, na.rm = TRUE)
  }
  colonnes[!vapply(colonnes, mesuree, TRUE)]
}

#' Annees a demander a SAFRAN en mode rapide : celles que le jeu ne porte pas, et l'annee
#' comparee (profils du mois et du jour).
#' @keywords internal
base_annees_a_charger <- function(jeu, annees, courante) {
  a <- sort(unique(c(setdiff(as.integer(annees), jeu$annees), as.integer(courante))))
  a[a >= CONFIG$annee_min]
}


# ------------------------------------------------------------------------------
# 5. Ce que l'utilisateur voit du mode rapide
# ------------------------------------------------------------------------------

#' Provenance d'un bilan en mode rapide, pour les exports et les pieds de graphique.
#' @keywords internal
base_texte_source <- function(don) {
  rap <- attr(don, "base")
  station <- base_est_station(rap$jeu)
  sprintf(paste0("base précalculée (%s, années %d-%d, calcul du %s, Météo-France), ",
                 "complétée par %s pour %s"),
          if (station) "station Météo-France" else "maille SAFRAN de 8 km",
          min(rap$jeu$annees), max(rap$jeu$annees),
          format(as.Date(as.character(rap$jeu$meta$genere_le)[1]), "%d/%m/%Y"),
          if (station) "les mesures" else "SAFRAN",
          paste(base_annees_vives(don), collapse = ", "))
}

#' Avis en tete du bilan : ce qui est lu, ce qui est telecharge, ce qui manque, et le bouton
#' qui charge la serie complete.
#' @param id identifiant du bouton (un par ecran : un identifiant ne figure qu'une fois).
#' @keywords internal
base_avis_ui <- function(don, id = "charger_complet") {
  rap <- attr(don, "base")
  if (is.null(rap)) return(NULL)
  shiny::div(
    class = "cf-alerte cf-alerte--info", role = "status",
    shiny::span(class = "cf-alerte__titre", "Mode rapide : base précalculée"),
    sprintf(paste("Les années %d à %d sont lues dans le jeu précalculé (%s) ; %s n'a été",
                  "interrogé que pour %s."),
            min(rap$jeu$annees), max(rap$jeu$annees), rap$note %||% "maille la plus proche",
            if (base_est_station(rap$jeu)) "Météo-France" else "SAFRAN",
            paste(base_annees_vives(don), collapse = ", ")),
    " La synthèse chiffrée à période égale et le rapport demandent la série complète.",
    shiny::div(class = "cf-aide", paste(
      "Les valeurs publiées sont arrondies (entiers 16 bits) : elles diffèrent du calcul",
      "complet de moins d'une unité de la dernière décimale affichée.")),
    shiny::actionButton(id, "Charger la série complète", class = "cf-btn cf-btn--sec"))
}
