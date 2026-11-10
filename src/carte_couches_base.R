# ==============================================================================
# carte_couches_base.R — LA BASE PRECALCULEE : CE QUE LA CARTE NE MONTRE PAS (v2.2)
# ------------------------------------------------------------------------------
# POURQUOI. Une COUCHE de la carte (carte_couches.R) est UNE colonne d'un tableau
# annuel : « jours de gel » est cartographiable, mais le tableau du bilan porte
# aussi les jours de gel SEVERE et la date du dernier gel. Pour que le bilan d'un
# point se lise dans le jeu publie au lieu d'etre recalcule depuis 66 annees de
# series journalieres, il faut publier ces colonnes aussi.
#
# CE QUI EST PUBLIE EN PLUS, a cote des couches de la carte, dans meta.json (champ
# `base`, que le navigateur ne lit pas) :
#   - des COLONNES ANNUELLES (type « annuel ») : une valeur par maille et par annee,
#     calculee par la fonction MEME de l'indicateur (aucune formule en double) ;
#   - des REFERENCES (type « ref ») : quelques valeurs par maille qui ne dependent PAS
#     de l'annee et que l'annee en cours ne peut pas recalculer a elle seule :
#       . la normale mensuelle (12 valeurs) des profils du bilan,
#       . le seuil de vent soutenu (1 valeur : percentile 95 du vent de reference),
#       . la normale journaliere de T max (vagues de chaleur) et de T moy (courbe
#         de l'annee) : 366 valeurs.
#
# MEME FORMAT BINAIRE que les couches (src/utils_carte.R) : une « annee » y est une
# ligne, qu'elle soit une annee, un mois (12 lignes), un jour (366) ou rien (1).
#
# VALIDITE. Ces valeurs sont calculees avec la reference, la fenetre et les seuils PAR
# DEFAUT de l'application. Si l'utilisateur en change, la base ne vaut plus : le
# bilan recalcule alors depuis les series journalieres (src/utils_base.R).
#
# AJOUTER UNE COLONNE = une ligne ci-dessous, puis relancer le precalcul et ajouter
# l'entree correspondante dans BASE_INDICATEURS (src/utils_base.R).
# ==============================================================================

#' Une colonne annuelle de la base : meme calcul que l'indicateur `source`.
#' @keywords internal
#' @param nom nom de la colonne dans la base, si ce n'est pas celui de l'indicateur (une valeur
#'   derivee, comme le code de la classe d'aridite).
#' @param extraire function(tableau annuel) -> vecteur numerique, si la colonne n'est pas
#'   numerique telle quelle (un facteur, par exemple).
base_annuelle <- function(source, colonne, stock, nom = colonne, extraire = NULL) {
  cle <- sprintf("b_%s_%s", source, nom)
  k <- carte_couche(cle, source, colonne, sprintf("%s — %s (base)", source, nom),
                    "", stock, stock, "chaud", "positif_rouge")
  k$base <- TRUE; k$type <- "annuel"; k$nom <- nom; k$extraire <- extraire
  k
}

#' Une reference de la base : `lignes` valeurs par maille, calculees par `calc(don, ctx)`.
#' @keywords internal
base_reference <- function(cle, lignes, stock, calc, titre) {
  k <- carte_couche(cle, NA_character_, NA_character_, titre, "", stock, stock,
                    "chaud", "positif_rouge")
  k$base <- TRUE; k$type <- "ref"; k$lignes <- as.integer(lignes); k$calc <- calc
  k
}

# Normale mensuelle d'une colonne : memes mois COMPLETS de la reference que les profils
# du bilan (calc_profil_mensuel). Sur SAFRAN aucun jour ne manque : exiger p_tot et etp
# ensemble (bilan hydrique) ou p_tot seul (profil des pluies) donne les memes mois.
.base_normale_mensuelle <- function(colonne, fun) {
  force(colonne); force(fun)
  function(don, ctx) profil_mois(sel_annees(don, ctx$ref), colonne, fun, colonne, strict = TRUE)$v
}

# Normale journaliere de la temperature moyenne, par rang dans l'annee (1..366), NON lissee :
# le lissage circulaire se fait a la lecture, comme dans calc_temperature_journaliere().
.base_normale_jour_tmoy <- function(don, ctx) {
  ref <- sel_annees(don, ctx$ref)
  if (!nrow(ref)) return(rep(NA_real_, 366L))
  doy <- as.integer(format(ref$date, "%j"))
  m <- tapply(ref$t_moy, factor(doy, levels = 1:366), function(x) mean(x, na.rm = TRUE))
  v <- as.numeric(m); v[!is.finite(v)] <- NA_real_
  v
}

# Normale journaliere de T max des vagues de chaleur : celle que vagues_detecter() utilise
# (lissee, 366 jours du calendrier), ou NA si la reference est trop courte.
.base_normale_jour_tmax <- function(don, ctx) {
  v <- vague_normale_jour(don, ctx)
  if (is.null(v)) rep(NA_real_, 366L) else as.numeric(v)
}

.BASE_LISTE <- list(
  # ---- colonnes annuelles absentes des couches de la carte -------------------
  base_annuelle("vag",  "duree_max",     0),
  base_annuelle("vag",  "ecart_max",     1),
  base_annuelle("gel",  "doy_dernier_gel", 0),
  base_annuelle("geltd", "doy_debourrement", 0),
  base_annuelle("pre",  "p_liq",         0),
  base_annuelle("pre",  "p_sol",         0),
  base_annuelle("neige", "neige_3j_max", 1),
  base_annuelle("swi",  "swi_min",       3),
  base_annuelle("swi",  "n_stress",      0),
  base_annuelle("dry",  "n_sequences",   0),
  base_annuelle("dry",  "n_jours_secs",  0),
  base_annuelle("mart", "i_estival",     1),
  # La classe est un FACTEUR derive de l'indice : elle est publiee en code (1 a 6) pour que la
  # classe lue soit toujours celle du calcul, meme quand l'indice arrondi tomberait de l'autre
  # cote d'une borne (10, 20, 30...).
  base_annuelle("mart", "classe",        0, nom = "classe_code", extraire = function(r) as.integer(r$classe)),
  base_annuelle("veg",  "doy_debut",     0),
  base_annuelle("veg",  "doy_fin",       0),
  base_annuelle("petp", "p",             0),
  base_annuelle("petp", "etp",           0),
  base_annuelle("vpd",  "n_stress",      0),
  base_annuelle("vent", "ff_max",        1),
  base_annuelle("vent", "n_ventes",      0),
  base_annuelle("vent_soutenu", "n_p95", 0),
  base_annuelle("vent_sol", "n_combo",   0),
  base_annuelle("vent_sol", "n_sature",  0),

  # ---- references (ne dependent pas de l'annee) -----------------------------------
  base_reference("r_mois_p",   12L, 1, .base_normale_mensuelle("p_tot", "somme"),
                 "Normale mensuelle des precipitations"),
  base_reference("r_mois_t",   12L, 2, .base_normale_mensuelle("t_moy", "moyenne"),
                 "Normale mensuelle de la temperature"),
  base_reference("r_mois_neige", 12L, 1, .base_normale_mensuelle("p_sol", "somme"),
                 "Normale mensuelle des precipitations neigeuses"),
  base_reference("r_mois_etp", 12L, 1, .base_normale_mensuelle("etp", "somme"),
                 "Normale mensuelle de l'ETP"),
  base_reference("r_mois_vpd", 12L, 3, .base_normale_mensuelle("vpd", "moyenne"),
                 "Normale mensuelle du deficit de pression de vapeur"),
  base_reference("r_mois_ff",  12L, 2, .base_normale_mensuelle("ff", "moyenne"),
                 "Normale mensuelle du vent"),
  base_reference("r_seuil_ff", 1L,  2, function(don, ctx) seuil_vent_reference(don, ctx),
                 "Seuil de vent soutenu (percentile de reference)"),
  base_reference("r_jour_tmax", 366L, 2, .base_normale_jour_tmax,
                 "Normale journaliere de la temperature maximale"),
  base_reference("r_jour_tmoy", 366L, 2, .base_normale_jour_tmoy,
                 "Normale journaliere de la temperature moyenne")
)

BASE_COUCHES <- stats::setNames(.BASE_LISTE, vapply(.BASE_LISTE, function(k) k$cle, ""))
rm(.BASE_LISTE)

# Colonnes de deux indicateurs qui sont LA MEME grandeur : une seule couche les porte.
# Le cumul annuel de pluie de De Martonne est celui des precipitations, la temperature
# annuelle de De Martonne celle de l'indicateur de temperature, et la neige reprend les
# phases de l'indicateur des precipitations. tests/test_v22.R compare chaque alias a
# l'indicateur qui l'emploie.
BASE_ALIAS <- c("neige:p_sol" = "pre:p_sol", "neige:p_tot" = "pre:p_tot",
                "mart:p_an" = "pre:p_tot", "mart:t_an" = "tmp:t_moy")

#' Cle de la base (couche cachee) ou de la carte qui porte la colonne `colonne` de
#' l'indicateur `source`, ou NA si elle n'est pas publiee.
#' @keywords internal
base_cle_colonne <- function(source, colonne) {
  a <- BASE_ALIAS[paste0(source, ":", colonne)]
  if (!is.na(a)) { dec <- strsplit(unname(a), ":", fixed = TRUE)[[1]]; source <- dec[1]; colonne <- dec[2] }
  tout <- c(CARTE_COUCHES, BASE_COUCHES)
  for (k in tout) if (identical(k$type %||% "annuel", "annuel") &&
                      identical(k$source, source) && identical(k$nom %||% k$colonne, colonne)) return(k$cle)
  NA_character_
}
