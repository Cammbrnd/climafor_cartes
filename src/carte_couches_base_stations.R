# ==============================================================================
# carte_couches_base_stations.R — LA BASE PRECALCULEE DES STATIONS (v2.2)
# ------------------------------------------------------------------------------
# POURQUOI UN FICHIER A PART. La base des mailles (carte_couches_base.R) suppose une serie
# SANS LACUNE : une annee y est toujours complete, une normale mensuelle ne depend que de la
# variable. Une station a des trous (src/utils_stations.R : 95 % des jours, variable par
# variable), et le bilan d'une station ecarte les annees trop lacunaires AVANT de calculer.
# Pour que le mode rapide rende exactement ce que rend le chargement complet, la base d'une
# station publie donc, en plus des colonnes annuelles :
#
#   1. LA QUALITE de chaque annee et pour chaque groupe de variables (p, t, ff) :
#        b_q_frac_<g>  part des jours renseignes (3 decimales, pour l'affichage) ;
#        b_q_ok_<g>    1 si l'annee est valide (>= CONFIG$stations$seuil_annee), 0 sinon.
#      Le drapeau est publie a part : comparer la part ARRONDIE au seuil ferait basculer une
#      annee a 94,96 % du mauvais cote.
#   2. LES JOURS MANQUANTS par annee, pour chaque couple (variables, fenetre) que les
#      indicateurs utilisent : la colonne « n_manquants » des tableaux et la note « 2012 (14 j) »
#      des graphiques. Une maille n'a jamais de jour manquant ; une station, si.
#   3. LES REFERENCES, calculees PAR INDICATEUR. Une normale mensuelle de precipitations n'est
#      pas la meme pour le profil des pluies (annees valides pour la pluie) et pour le bilan
#      hydrique (annees valides pour la pluie ET la temperature, jours ou les deux existent).
#      Chacune est donc calculee sur les jours que l'indicateur recevrait, par la fonction
#      meme du calcul habituel (donnees_pour_indicateur puis la normale de reference).
#
# Une entree que AUCUNE station ne renseigne (SWI, neige, humidite : une station ne les mesure
# pas) n'est pas ecrite ; stations.json la nomme dans `base_parametres$vides`, et sa lecture
# rend des NA. Les indicateurs qui en dependent sont refuses avant d'arriver ici.
#
# Les fichiers sont ceux de la carte (src/utils_carte.R) : s_<cle>.bin.gz, une station a la
# place d'une maille, une ligne par annee (ou 1, 12, 366 lignes pour une reference).
# ==============================================================================

#' Cle de la colonne « jours manquants » pour un couple (variables, fenetre).
#' @param vars colonnes devant etre renseignees ; @param fenetre NULL (annee civile) ou c(mois, mois).
#' @keywords internal
base_cle_manquants <- function(vars, fenetre = NULL) {
  f <- if (is.null(fenetre)) "an" else paste(as.integer(fenetre), collapse = "_")
  sprintf("b_c_%s_%s", paste(sort(vars), collapse = "_"), f)
}

# Fenetre d'un indicateur de BASE_INDICATEURS : NULL, un couple, ou « ctx » (celle du contexte,
# egale a la fenetre par defaut des que la base sert : voir base_applicable()).
.base_fenetre_resolue <- function(fenetre) {
  if (identical(fenetre, "ctx")) CONFIG$fenetre_defaut else fenetre
}

# Une colonne annuelle calculee depuis le tableau journalier entier de la station.
.base_annuelle_station <- function(cle, titre, stock, annuel) {
  k <- carte_couche(cle, NA_character_, NA_character_, titre, "", stock, stock, "chaud", "positif_rouge")
  k$base <- TRUE; k$type <- "annuel"; k$annuel <- annuel
  k
}

.base_manquants_calc <- function(vars, fenetre) {
  force(vars); force(fenetre)
  function(don, ctx, annees) {
    comp <- completude(don, fenetre, vars)
    as.numeric(comp$n_manquants[match(annees, comp$annee)])
  }
}

.base_qualite_calc <- function(colonne) {
  force(colonne)
  function(don, ctx, annees) {
    q <- attr(don, "qualite_annuelle")
    if (is.null(q)) return(rep(NA_real_, length(annees)))
    as.numeric(q[[colonne]][match(annees, q$annee)])
  }
}

.base_valide_calc <- function(groupe) {
  force(groupe)
  function(don, ctx, annees) {
    v <- attr(don, "annees_valides")[[groupe]]
    q <- attr(don, "qualite_annuelle")
    # Seules les annees dont la qualite est connue ont un drapeau : les autres restent vides.
    ifelse(annees %in% q$annee, as.numeric(annees %in% v), NA_real_)
  }
}

# Une reference de station : `calc(d, ctx)` sur les jours que recevrait l'indicateur `ind`.
.base_reference_station <- function(ind, nom, lignes, stock, titre, calc) {
  force(ind); force(calc)
  k <- base_reference(sprintf("rs_%s_%s", ind, nom), lignes, stock,
                      function(don, ctx) calc(donnees_pour_indicateur(don, INDICATEURS[[ind]]), ctx),
                      titre)
  k$ind <- ind; k$nom <- nom
  k
}

.base_mensuelle_station <- function(colonne, fun, vars) {
  force(colonne); force(fun); force(vars)
  function(d, ctx) profil_mois(sel_annees(d, ctx$ref), colonne, fun, vars, strict = TRUE)$v
}

# Qui lit quoi : pour chaque indicateur qui consulte une reference, le nom de la reference dans
# `ctx$base` et l'entree de la base qui la porte. Calques sur les appels de utils_calc.R
# (ref_mensuelle, vague_normale_jour, seuil_vent_reference, calc_temperature_journaliere) :
# tests/test_v22_stations.R compare le resultat aux deux chemins.
.BASE_STATIONS_REFS <- list(
  .base_reference_station("bh", "p", 12L, 1, "Bilan hydrique : normale mensuelle des precipitations",
                          .base_mensuelle_station("p_tot", "somme", c("p_tot", "etp"))),
  .base_reference_station("bh", "etp", 12L, 1, "Bilan hydrique : normale mensuelle de l'ETP",
                          .base_mensuelle_station("etp", "somme", c("p_tot", "etp"))),
  .base_reference_station("ombro", "p", 12L, 1, "Ombrothermique : normale mensuelle des precipitations",
                          .base_mensuelle_station("p_tot", "somme", "p_tot")),
  .base_reference_station("ombro", "t", 12L, 2, "Ombrothermique : normale mensuelle de la temperature",
                          .base_mensuelle_station("t_moy", "moyenne", "t_moy")),
  .base_reference_station("pre_mois", "p", 12L, 1, "Precipitations par mois : normale",
                          .base_mensuelle_station("p_tot", "somme", "p_tot")),
  .base_reference_station("tmp_mois", "t", 12L, 2, "Temperature par mois : normale",
                          .base_mensuelle_station("t_moy", "moyenne", "t_moy")),
  .base_reference_station("vent_mois", "ff", 12L, 2, "Vent par mois : normale",
                          .base_mensuelle_station("ff", "moyenne", "ff")),
  .base_reference_station("tmp_jour", "tmoy", 366L, 2, "Courbe de l'annee : normale du jour",
                          .base_normale_jour_tmoy),
  .base_reference_station("vag", "tmax", 366L, 2, "Vagues de chaleur : normale du jour de T max",
                          .base_normale_jour_tmax),
  .base_reference_station("vent_soutenu", "seuil", 1L, 2, "Vent soutenu : seuil de reference",
                          function(d, ctx) seuil_vent_reference(d, ctx))
)

# Nom d'une reference dans `ctx$base` (celui que lisent utils_calc.R) selon son role.
.BASE_STATIONS_NOM_CTX <- c(p = "mois_p_tot", t = "mois_t_moy", etp = "mois_etp", ff = "mois_ff",
                            tmoy = "normale_jour_tmoy", tmax = "normale_jour_tmax", seuil = "seuil_vent")

.BASE_STATIONS_LISTE <- c(
  # Colonnes annuelles de la base des mailles : memes calculs, memes cles.
  Filter(function(k) identical(k$type, "annuel"), unname(BASE_COUCHES)),
  # Qualite de chaque annee, par groupe de variables.
  unlist(lapply(c("p", "t", "ff"), function(g) list(
    .base_annuelle_station(sprintf("b_q_frac_%s", g), sprintf("Part des jours renseignes (%s)", g), 3,
                           .base_qualite_calc(sprintf("frac_%s", g))),
    .base_annuelle_station(sprintf("b_q_ok_%s", g), sprintf("Annee valide (%s)", g), 0,
                           .base_valide_calc(g)))), recursive = FALSE),
  # Jours manquants, par couple (variables, fenetre) des indicateurs annuels.
  unname(lapply(
    unique(lapply(BASE_INDICATEURS, function(d) list(vars = d$vars, fenetre = .base_fenetre_resolue(d$fenetre)))),
    function(x) .base_annuelle_station(base_cle_manquants(x$vars, x$fenetre),
                                       sprintf("Jours manquants (%s)", paste(x$vars, collapse = " + ")), 0,
                                       .base_manquants_calc(x$vars, x$fenetre)))),
  .BASE_STATIONS_REFS)

#' Entrees de la base d'une STATION, rangees par cle (voir BASE_COUCHES pour les mailles).
STATIONS_BASE_COUCHES <- stats::setNames(.BASE_STATIONS_LISTE, vapply(.BASE_STATIONS_LISTE, function(k) k$cle, ""))
rm(.BASE_STATIONS_LISTE, .BASE_STATIONS_REFS)

#' Les references qu'un indicateur de station consulte : nom dans `ctx$base` -> cle de la base.
#' @return vecteur nomme, vide si l'indicateur n'en consulte aucune.
#' @keywords internal
base_refs_station <- function(ind_id) {
  out <- character(0)
  for (k in STATIONS_BASE_COUCHES) {
    if (identical(k$type, "ref") && identical(k$ind, ind_id)) out[[.BASE_STATIONS_NOM_CTX[[k$nom]]]] <- k$cle
  }
  out
}
