# ==============================================================================
# carte_couches.R — QUELS INDICATEURS SE CARTOGRAPHIENT, ET COMMENT
# ------------------------------------------------------------------------------
# Une COUCHE est une grandeur ANNUELLE calculee pour chaque maille SAFRAN :
# une valeur par maille et par annee. Les indicateurs qui decrivent un profil
# (mensuel, journalier) n'ont pas de valeur unique par annee : ils ne sont pas
# cartographiables tels quels et restent dans l'analyse d'un point.
#
# AJOUTER UNE COUCHE = ajouter une ligne dans CARTE_COUCHES (puis relancer le
# precalcul). La couche s'appuie sur un indicateur du registre : c'est SA
# fonction de calcul qui produit les valeurs, donc la carte ne peut pas diverger
# de l'analyse d'un point (tests/test_v18.R le verifie).
#
# Champs d'une couche
# -------------------
#   cle       identifiant de la couche (minuscules, chiffres, soulignement)
#   source    identifiant de l'indicateur du registre qui la calcule
#   colonne   colonne de son tableau annuel que l'on cartographie
#   normale   colonne de ce tableau portant la normale de CETTE colonne, ou NULL
#             quand le tableau ne porte la normale que d'une autre colonne : elle
#             est alors recalculee par stats_ref(), la fonction meme du calcul
#   titre     libelle affiche
#   unite     unite affichee (chaine vide pour un indice sans unite)
#   decimales decimales affichees
#   stock     decimales CONSERVEES dans le fichier (entiers 16 bits : la valeur
#             stockee est valeur * 10^stock, elle doit rester sous 32 000)
#   palette   degrade des valeurs : "chaud" | "eau" | "vert" | "violet" | "froid"
#   ecart     couleur du sens POSITIF d'un ecart : "positif_rouge" |
#             "positif_bleu" | "positif_vert" | "positif_violet". Les degrades sont
#             ceux de ColorBrewer, sans danger pour le daltonisme.
#   famille   (facultatif) famille d'affichage dans le selecteur, quand elle diffère
#             de celle de l'indicateur source : « jja » est classe en temperatures,
#             mais ses precipitations estivales se rangent avec l'eau.
# ==============================================================================

carte_couche <- function(cle, source, colonne, titre, unite, decimales, stock,
                         palette, ecart, normale = NULL, famille = NULL) {
  list(cle = cle, source = source, colonne = colonne, normale = normale,
       titre = titre, unite = unite, decimales = decimales, stock = stock,
       palette = palette, ecart = ecart, famille = famille)
}

CARTE_PALETTES <- c("chaud", "eau", "vert", "violet", "froid")
CARTE_ECARTS   <- c("positif_rouge", "positif_bleu", "positif_vert", "positif_violet")

CARTE_COUCHES <- list(

  # ---- Temperatures --------------------------------------------------------
  tmp   = carte_couche("tmp", "tmp", "t_moy", "Température moyenne annuelle",
                       "°C", 1, 2, "chaud", "positif_rouge", "normale"),
  jja_t = carte_couche("jja_t", "jja", "t_jja", "Température estivale (juin à août)",
                       "°C", 1, 2, "chaud", "positif_rouge", "t_normale"),
  can   = carte_couche("can", "can", "n_t2",
                       sprintf("Jours de forte chaleur (au-dessus de %g °C)", CONFIG$seuils$chaleur_t2),
                       "jours", 0, 0, "chaud", "positif_rouge", "normale"),
  can3  = carte_couche("can3", "can", "n_t3",
                       sprintf("Jours de chaleur extrême (au-dessus de %g °C)", CONFIG$seuils$chaleur_t3),
                       "jours", 0, 0, "chaud", "positif_rouge"),

  vag   = carte_couche("vag", "vag", "n_vagues", "Vagues de chaleur (nombre par an)",
                       "vagues", 0, 0, "chaud", "positif_rouge", "normale"),
  vagj  = carte_couche("vagj", "vag", "n_jours_vague", "Jours en vague de chaleur",
                       "jours", 0, 0, "chaud", "positif_rouge"),

  # ---- Eau et bilan hydrique ----------------------------------------------
  pre   = carte_couche("pre", "pre", "p_tot", "Précipitations annuelles",
                       "mm", 0, 0, "eau", "positif_vert", "normale"),
  jja_p = carte_couche("jja_p", "jja", "p_jja", "Précipitations estivales (juin à août)",
                       "mm", 0, 0, "eau", "positif_vert", "p_normale",
                       famille = "hydrique"),
  petp  = carte_couche("petp", "petp", "petp", "Bilan P − ETP de la saison de végétation",
                       "mm", 0, 0, "eau", "positif_vert", "normale"),
  swi   = carte_couche("swi", "swi", "swi_moy", "Humidité du sol estivale (SWI)",
                       "", 2, 3, "eau", "positif_vert", "normale"),
  dry   = carte_couche("dry", "dry", "duree_max", "Plus longue séquence sèche",
                       "jours", 0, 0, "chaud", "positif_rouge", "normale"),

  # ---- Gel -----------------------------------------------------------------
  gel   = carte_couche("gel", "gel", "n_gel", "Jours de gel",
                       "jours", 0, 0, "froid", "positif_bleu", "normale"),
  geltd = carte_couche("geltd", "gel", "n_gel_tardif", "Jours de gel tardif (avril et mai)",
                       "jours", 0, 0, "froid", "positif_bleu"),

  # ---- Aridite et bioclimat -----------------------------------------------
  mart  = carte_couche("mart", "mart", "i_annuel", "Indice d'aridité de De Martonne",
                       "", 1, 1, "eau", "positif_vert", "normale"),
  vpd   = carte_couche("vpd", "vpd", "vpd_jja", "Déficit de pression de vapeur estival",
                       "kPa", 2, 3, "chaud", "positif_rouge"),
  vpdm  = carte_couche("vpdm", "vpd", "vpd_moy", "Déficit de pression de vapeur annuel",
                       "kPa", 2, 3, "chaud", "positif_rouge", "normale"),

  # ---- Phenologie ----------------------------------------------------------
  gdd   = carte_couche("gdd", "gdd", "gdd", "Degrés-jours de croissance",
                       "°C·j", 0, 0, "chaud", "positif_rouge", "normale"),
  veg   = carte_couche("veg", "veg", "duree", "Durée de la saison de végétation",
                       "jours", 0, 0, "vert", "positif_vert", "normale"),

  # ---- Neige et vent ---------------------------------------------------------
  neige = carte_couche("neige", "neige", "n_jours_neige", "Jours de neige",
                       "jours", 0, 0, "froid", "positif_bleu"),
  vent  = carte_couche("vent", "vent", "ff_moy", "Vent moyen annuel (10 m)",
                       "m/s", 1, 2, "violet", "positif_violet", "normale")
)
