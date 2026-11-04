# ==============================================================================
# registre_indicateurs.R — LE COEUR GENERIQUE
# ------------------------------------------------------------------------------
# Ce fichier est le point d'extension principal de l'application. Il declare,
# sous forme de donnees et non de code, TOUT ce que l'interface doit savoir d'un
# indicateur. Les modules Shiny ne connaissent aucun indicateur en particulier :
# ils parcourent ce registre.
#
# AJOUTER UN GRAPHIQUE = ajouter une entree ici (+ un calc_ et un plot_).
# Aucun autre fichier n'a besoin d'etre modifie.
#
# Schema d'une entree
# -------------------
#   id          identifiant technique unique (sert d'ancre HTML et d'ID de module)
#   titre       libelle affiche
#   famille     regroupement thematique : "hydrique" | "thermique" | "gel" |
#               "aridite" | "phenologie" | "neige"
#   calc        fonction(don, ctx) -> data.frame
#   plot        fonction(dat, ctx, mobile) -> ggplot
#   colonnes    colonnes exportees en CSV (NULL = toutes)
#   comparaison TRUE si l'indicateur supporte la confrontation
#               "annee en cours vs periode de reference"
#   fenetre     TRUE si l'indicateur depend de la fenetre mensuelle choisie
#   interactif  FALSE pour forcer un rendu en image, meme quand l'application
#               est en mode interactif. A poser sur tout trace que ggplotly ne
#               convertit pas fidelement — en pratique, ceux qui utilisent un
#               axe secondaire, que plotly perd SANS lever d'erreur. Absent =
#               TRUE, l'interactivite est le defaut.
#   selecteur_annee  TRUE si la carte doit porter un menu deroulant permettant
#               de choisir l'annee comparee, independamment du reglage global.
#               L'indicateur est alors RECALCULE a chaque changement, avec un
#               contexte local ou ctx$courante vaut l'annee choisie. A reserver
#               aux profils mensuels et journaliers : sur une serie annuelle,
#               qui montre deja toutes les annees, le selecteur n'aurait aucun
#               sens.
#   methode     texte : comment la valeur est calculee
#   lecture     texte : comment l'interpreter en gestion forestiere
#   resume      fonction(dat, ctx) -> chaine, commentaire automatique du rapport
#               (NULL si l'indicateur n'en produit pas)
# ==============================================================================


# ------------------------------------------------------------------------------
# Helpers de redaction des commentaires automatiques
# ------------------------------------------------------------------------------

#' Formate un ecart avec son signe et son unite, en francais.
#' @keywords internal
fmt_ecart <- function(x, unite, dec = 1) {
  if (!is.finite(x)) return("non evalue")
  paste0(fmt_fr(x, dec, signe = TRUE), if (nzchar(unite)) paste0(" ", unite) else "")
}

#' Formate une valeur simple.
#' @keywords internal
fmt_val <- function(x, unite = "", dec = 1) {
  if (!is.finite(x)) return("n.d.")
  paste0(formatC(x, format = "f", digits = dec, decimal.mark = ","),
         if (nzchar(unite)) paste0(" ", unite) else "")
}

#' Extrait la valeur d'une annee donnee dans un tableau annuel.
#' @keywords internal
val_annee <- function(dat, annee, colonne) {
  if (is.null(dat) || !nrow(dat) || !colonne %in% names(dat)) return(NA_real_)
  v <- dat[[colonne]][dat$annee == annee]
  if (!length(v)) NA_real_ else v[1]
}

#' L'annee comparee est-elle incomplete dans ce resultat ?
#' @return list(oui, fin) ; `fin` est le dernier jour connu.
#' @keywords internal
annee_incomplete <- function(dat, ctx) {
  l <- if (!is.null(dat) && nrow(dat) && "partielle" %in% names(dat))
    dat[dat$annee == ctx$courante, , drop = FALSE] else NULL
  oui <- !is.null(l) && nrow(l) > 0 && isTRUE(l$partielle[1])
  list(oui = oui,
       fin = if (oui && "dernier_jour" %in% names(l)) l$dernier_jour[1] else as.Date(NA))
}

#' Phrase de commentaire pour une annee (ou saison) incomplete.
#' @keywords internal
phrase_incomplete <- function(libelle, ctx, fin, periode = "année") {
  sprintf(
    "%s : %s %d est incomplète%s ; elle n'est pas comparée à la normale %s (comparaison à période égale dans la synthèse chiffrée).",
    libelle, if (periode == "saison") "la saison" else "l'année", ctx$courante,
    if (length(fin) && !is.na(fin)) sprintf(" (données jusqu'au %s)", date_fr(fin)) else "",
    ctx$ref_label)
}

#' Ecarts d'un profil mensuel, sans le mois incomplet de l'annee comparee.
#' @keywords internal
ecarts_mensuels <- function(dat) {
  e <- dat$val_cur - dat$val_ref
  if ("cur_partiel" %in% names(dat)) e[dat$cur_partiel %in% TRUE] <- NA_real_
  e
}

#' Commentaire generique "annee comparee vs normale" reutilisable.
#'
#' Trois cas pour la position, et non deux : un ecart plus petit que la moitie de
#' l'ecart-type inter-annuel de la reference n'est pas distinguable de la
#' variabilite ordinaire ; le dire « au-dessus de la normale » serait une
#' precision que la donnee n'a pas.
#'
#' @param sens_haut,sens_bas locution placee AVANT « la normale », sans « à » :
#'   « supérieure à », « inférieure à ». (v11 : « au-dessus de à la normale ».)
#'
#' L'annee comparee INCOMPLETE n'est pas comparee a une normale annuelle : le
#' commentaire le dit et renvoie a la synthese chiffree, qui compare a periode
#' egale.
#' @keywords internal
resume_standard <- function(dat, ctx, colonne, libelle, unite, dec = 1,
                            sens_haut = "supérieure à", sens_bas = "inférieure à",
                            tendance_txt = NULL, periode = "année") {
  v <- val_annee(dat, ctx$courante, colonne)
  n <- if ("normale" %in% names(dat) && nrow(dat)) dat$normale[1] else NA_real_
  inc <- annee_incomplete(dat, ctx)
  if (inc$oui) {
    base <- phrase_incomplete(libelle, ctx, inc$fin, periode)
  } else if (!is.finite(v) || !is.finite(n)) {
    base <- sprintf("%s : données insuffisantes pour comparer %d à la normale %s.",
                    libelle, ctx$courante, ctx$ref_label)
  } else {
    ecart <- v - n
    sd_ref <- if ("ecart_type_ref" %in% names(dat)) dat$ecart_type_ref[1] else NA_real_
    # Un ecart qui s'affiche « 0 » (arrondi au pas d'affichage) n'est ni
    # superieur ni inferieur : ecrire « 0 jours, valeur inferieure » serait
    # contradictoire.
    classe <- if (round(ecart, dec) == 0) "neutre" else classe_ecart(ecart, sd_ref)
    pos <- switch(classe, haut = sens_haut, bas = sens_bas, "proche de")
    base <- sprintf("%s en %d : %s, soit %s (valeur %s la normale %s de %s).",
                    libelle, ctx$courante, fmt_val(v, unite, dec),
                    fmt_ecart(ecart, unite, dec), pos, ctx$ref_label,
                    fmt_val(n, unite, dec))
  }
  if (!is.null(tendance_txt)) paste(base, tendance_txt) else base
}

#' Texte de tendance d'un resultat annuel, sur la plage affichee.
#' @keywords internal
texte_tendance <- function(dat, ctx, colonne, unite, dec = 2) {
  d <- dat[dat$annee %in% ctx$affichage, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  tendance(d$annee, d[[colonne]], unite, exclure = d$partielle, dec = dec)$libelle
}


# ------------------------------------------------------------------------------
# LE REGISTRE
# ------------------------------------------------------------------------------

INDICATEURS <- list(

  # ---- Bilan hydrique ------------------------------------------------------
  bh = list(
    id = "bh", titre = "Bilan hydrique mensuel", famille = "hydrique",
    calc = calc_bilan_hydrique, plot = plot_bilan_hydrique,
    colonnes = NULL, comparaison = TRUE, fenetre = FALSE,
    selecteur_annee = TRUE,
    methode = paste(
      "Pour chaque mois, cumul des précipitations totales (pluie + neige)",
      "diminue du cumul d'evapotranspiration potentielle. Les cumuls mensuels",
      "sont d'abord calculés année par année, puis moyennes sur la période de",
      "référence : on ne moyenne jamais des valeurs journalières."),
    lecture = paste(
      "Un solde mensuel négatif indique que la demande atmosphérique excède",
      "l'apport pluviométrique : le peuplement puise dans la réserve utile du",
      "sol. La succession de plusieurs mois négatifs, et surtout leur",
      "aggravation d'une décennie à l'autre, oriente le choix des essences et",
      "la densité de reprise après coupe."),
    resume = function(dat, ctx) {
      if (is.null(dat) || !nrow(dat)) return("")
      neg_ref <- sum(dat$bh_ref < 0, na.rm = TRUE)
      neg_cur <- sum(dat$bh_cur < 0, na.rm = TRUE)
      bh_ok <- dat$bh_cur
      if ("cur_partiel" %in% names(dat)) bh_ok[dat$cur_partiel %in% TRUE] <- NA
      pire <- if (any(is.finite(bh_ok))) MOIS_LONG[which.min(bh_ok)] else NA
      sprintf(paste0("La normale %s compte %d mois déficitaires. En %d, on en dénombre %d",
                     "%s."),
              ctx$ref_label, neg_ref, ctx$courante, neg_cur,
              if (!is.na(pire)) sprintf(", le plus déficitaire etant %s", pire) else "")
    }
  ),

  petp = list(
    id = "petp", titre = "P - ETP sur fenêtre choisie", famille = "hydrique",
    calc = calc_petp, plot = plot_petp,
    colonnes = c("annee", "p", "etp", "petp", "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = TRUE,
    methode = paste(
      "Somme des précipitations totales moins somme de l'ETP sur la fenêtre",
      "mensuelle sélectionnée, année par année. La normale est la moyenne de",
      "ce solde sur la période de référence, calculée sur la même fenêtre."),
    lecture = paste(
      "C'est l'indicateur de déficit hydrique climatique le plus directement",
      "mobilisable en sylviculture. La fenêtre se choisit selon la question :",
      "juin-août pour le stress de pointe, avril-octobre pour le bilan de la",
      "saison de végétation, mai-septembre pour un compromis. Un solde",
      "durablement inférieur à -300 mm sur avril-octobre signale un contexte",
      "ou les essences à fort besoin en eau (hêtre, épicéa, frêne) deviennent",
      "risquees."),
    resume = function(dat, ctx) resume_standard(
      dat, ctx, "petp", sprintf("Solde P - ETP (%s)", libelle_fenetre(ctx$fenetre)),
      "mm", 0, tendance_txt = texte_tendance(dat, ctx, "petp", "mm", 0),
      periode = "saison")
  ),

  swi = list(
    id = "swi", titre = "SWI estival - stress hydrique du sol", famille = "hydrique",
    calc = calc_swi, plot = plot_swi,
    colonnes = c("annee", "swi_moy", "swi_min", "n_stress", "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Moyenne de juin à août de l'indice d'humidité des sols SWI simulé par",
      "le modèle ISBA. Le SWI normalise le contenu en eau du réservoir",
      "racinaire entre le point de flétrissement (0) et la capacité au champ (1)."),
    lecture = paste(
      "Contrairement au P - ETP, le SWI intègre le fonctionnement réel du sol",
      "(réserve, drainage, remontées). Les repères de 0,5 et de 0,3 sont des ordres",
      "de grandeur, pas des valeurs établies : la littérature place la régulation de",
      "la transpiration des peuplements sous environ 0,4 d'eau extractible relative",
      "(Granier et al. 1999 ; Bréda et al. 2006), grandeur voisine du SWI mais",
      "pas identique, et relie les attaques de scolytes sur épicéa à des déficits",
      "hydriques marqués (Netherer et al. 2015) sans fixer de seuil de SWI ISBA.",
      "Attention : le SWI ISBA porte sur un sol standard, il ne remplace pas",
      "une mesure de réserve utile à la tarière."),
    resume = function(dat, ctx) resume_standard(
      dat, ctx, "swi_moy", "SWI estival moyen", "", 2,
      "supérieure à", "inférieure à", periode = "saison")
  ),

  dry = list(
    id = "dry", titre = "Séquences sèches consécutives", famille = "hydrique",
    calc = calc_secheresse, plot = plot_secheresse,
    colonnes = c("annee", "duree_max", "n_sequences", "n_jours_secs", "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Longueur maximale, sur chaque année civile, d'une suite ininterrompue",
      "de journées dont le cumul de précipitations reste inférieur au seuil",
      "configuré (1 mm par défaut)."),
    lecture = paste(
      "La durée des épisodes secs pèse davantage que le cumul annuel sur la",
      "reprise des plantations et sur le risque d'incendie. Une séquence de",
      "plus de trois semaines en période de végétation compromet une plantation",
      "de printemps non paillée."),
    resume = function(dat, ctx) resume_standard(
      dat, ctx, "duree_max", "Durée maximale de séquence sèche", "jours", 0,
      "plus longue que", "plus courte que")
  ),

  # ---- Thermique -----------------------------------------------------------
  tmp = list(
    id = "tmp", titre = "Évolution thermique annuelle", famille = "thermique",
    calc = calc_temperature_annuelle, plot = plot_temperature_annuelle,
    colonnes = c("annee", "t_moy", "lissee", "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Moyenne annuelle de la température moyenne journalière (SAFRAN ou station), assortie",
      "d'une moyenne mobile centrée sur 10 ans et d'une régression linéaire",
      "sur la plage affichée."),
    lecture = paste(
      "La pente de la droite de tendance, exprimée en degrés par décennie, est",
      "la grandeur à retenir : elle se compare directement aux trajectoires",
      "régionales et aux exigences autecologiques des essences. Le lissage",
      "décennal permet de distinguer le signal climatique de la variabilité",
      "interannuelle."),
    resume = function(dat, ctx) {
      resume_standard(dat, ctx, "t_moy", "Température moyenne annuelle",
                      "°C", 1, tendance_txt = texte_tendance(dat, ctx, "t_moy", "°C", 2))
    }
  ),

  jja = list(
    id = "jja", titre = "Été (juin-juillet-août) : température et pluie",
    famille = "thermique",
    calc = calc_ete, plot = plot_ete,
    # Double axe : voir la note du diagramme ombrothermique.
    interactif = FALSE,
    colonnes = c("annee", "t_jja", "p_jja", "t_normale", "p_normale",
                 "t_anomalie", "p_anomalie", "t_ecart_type_ref", "p_ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Moyenne de la température et cumul des précipitations sur l'été",
      "météorologique, soit les trois mois de juin, juillet et août."),
    lecture = paste(
      "L'été concentre l'essentiel de la contrainte subie par le peuplement.",
      "La combinaison été chaud + été sec est plus informative que chacun des",
      "deux termes isolé : c'est celle que la littérature associe aux",
      "dépérissements différés et aux pullulations de scolytes les années",
      "suivantes (Bréda et al. 2006 ; Netherer et al. 2015). Elle signale un",
      "risque, elle ne prédit ni un seuil ni l'état d'un peuplement donné."),
    resume = function(dat, ctx) {
      t <- val_annee(dat, ctx$courante, "t_jja"); tn <- if (nrow(dat)) dat$t_normale[1] else NA
      p <- val_annee(dat, ctx$courante, "p_jja"); pn <- if (nrow(dat)) dat$p_normale[1] else NA
      inc <- annee_incomplete(dat, ctx)
      if (inc$oui) return(phrase_incomplete("Été", ctx, inc$fin, "saison"))
      if (!is.finite(t) || !is.finite(p)) return("")
      sprintf("Été %d : %s de moyenne (%s vs normale) et %s de pluie (%s vs normale).",
              ctx$courante, fmt_val(t, "°C"), fmt_ecart(t - tn, "°C"),
              fmt_val(p, "mm", 0), fmt_ecart(p - pn, "mm", 0))
    }
  ),

  can = list(
    id = "can", titre = "Jours de chaleur", famille = "thermique",
    calc = calc_chaleur, plot = plot_chaleur,
    colonnes = c("annee", "n_t1", "n_t2", "n_t3", "n_vagues", "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Dénombrement annuel des journées dont la température maximale dépasse",
      "trois seuils emboîtés (25, 30 et 35 °C par défaut, modifiables dans",
      "l'onglet Paramètres). Une vague de chaleur est comptée dès que trois",
      "jours consécutifs franchissent le seuil intermédiaire."),
    lecture = paste(
      "Vers 35 °C et au-delà, l'assimilation nette des essences tempérées diminue",
      "le plus souvent : l'optimum se situe en général entre 20 et 30 °C, selon",
      "l'essence et l'acclimatation (Sage et Kubien 2007). 35 °C est un repère,",
      "non une limite physiologique unique. Le nombre de jours au-dessus de ce",
      "seuil, et sa progression, entrent dans le choix d'une essence en station",
      "exposée, avec le sol et l'humidité de l'air."),
    resume = function(dat, ctx) resume_standard(
      dat, ctx, "n_t2", sprintf("Nombre de jours à plus de %g °C", ctx$seuils$chaleur_t2),
      "jours", 0)
  ),

  # ---- Gel -----------------------------------------------------------------
  gel = list(
    id = "gel", titre = "Jours de gel et gel tardif", famille = "gel",
    calc = calc_gel, plot = plot_gel,
    colonnes = c("annee", "n_gel", "n_gel_severe", "n_gel_tardif",
                 "doy_dernier_gel", "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Dénombrement des journées à température minimale négative, sous abri",
      "(à 2 m). Le gel tardif est compté sur une fenêtre de dates fixe, du 1er",
      "avril au 31 mai par défaut (réglable dans Paramètres) : elle ne suit pas le",
      "débourrement réel du peuplement, qui varie avec l'essence, l'année et",
      "l'altitude. La date du dernier gel de printemps est le jour julien du",
      "dernier événement survenu avant le 30 juin."),
    lecture = paste(
      "Le réchauffement réduit le nombre total de jours de gel mais avance le",
      "débourrement : l'exposition au gel tardif peut donc augmenter alors",
      "même que le gel recule, ce que l'on observe en Europe entre 1959 et 2017",
      "(Zohner et al. 2020). C'est un point de vigilance pour les essences à",
      "débourrement précoce, en fond de vallon froid : l'air froid y stagne, et",
      "le gel y est plus fréquent et plus intense que sous l'abri météorologique",
      "dont sont tirées les données."),
    resume = function(dat, ctx) {
      base <- resume_standard(dat, ctx, "n_gel", "Nombre de jours de gel",
                              "jours", 0)
      gt <- val_annee(dat, ctx$courante, "n_gel_tardif")
      if (is.finite(gt)) paste(base, sprintf("Dont %d jour(s) de gel tardif.", gt)) else base
    }
  ),

  # ---- Pluviometrie et neige ----------------------------------------------
  pre = list(
    id = "pre", titre = "Précipitations annuelles", famille = "hydrique",
    calc = calc_precipitations, plot = plot_precipitations,
    colonnes = c("annee", "p_liq", "p_sol", "p_tot", "part_neige", "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Cumul annuel des précipitations totales, obtenu en additionnant la",
      "phase liquide (PRELIQ_Q) et la phase solide (PRENEI_Q, exprimée en",
      "équivalent en eau). La decomposition des deux phases est conservée."),
    lecture = paste(
      "Le cumul annuel seul renseigne peu : sa repartition et sa confrontation",
      "a l'ETP importent davantage. Il reste utile comme cadrage général et",
      "pour repérer les séries d'années déficitaires, dont l'effet sur les",
      "peuplements est cumulatif."),
    resume = function(dat, ctx) resume_standard(
      dat, ctx, "p_tot", "Cumul annuel de précipitations", "mm", 0)
  ),

  neige = list(
    id = "neige", titre = "Précipitations neigeuses", famille = "neige",
    calc = calc_neige, plot = plot_neige,
    colonnes = c("annee", "p_sol", "p_tot", "part_neige", "n_jours_neige",
                 "neige_3j_max", "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Cumul annuel de la seule phase solide, nombre de journées à chute de",
      "neige supérieure à 0,5 mm équivalent eau, et cumul maximal sur trois",
      "jours glissants (approche du risque de neige lourde)."),
    lecture = paste(
      "La neige joue deux roles opposés : réserve d'eau restituée au",
      "printemps, et aléa mecanique. Le cumul sur trois jours est le meilleur",
      "proxy simple du risque de bris de cimes et de volis, particulièrement",
      "sur les peuplements résineux non éclaircis et sur les perchis."),
    resume = function(dat, ctx) resume_standard(
      dat, ctx, "p_sol", "Cumul de précipitations neigeuses", "mm", 0)
  ),

  # ---- Aridite -------------------------------------------------------------
  mart = list(
    id = "mart", titre = "Indice d'aridité de De Martonne", famille = "aridite",
    calc = calc_martonne, plot = plot_martonne,
    colonnes = c("annee", "p_an", "t_an", "i_annuel", "i_estival", "classe",
                 "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Indice annuel I = P / (T + 10) et indice estival I = 4 x P_JJA /",
      "(T_JJA + 10), transposition trimestrielle de la formule mensuelle.",
      "Classes : moins de 5 hyperaride, 5-10 aride, 10-20 semi-aride,",
      "20-30 subhumide, 30-60 humide, plus de 60 très humide."),
    lecture = paste(
      "L'indice synthétise en un nombre la relation chaleur-humidité. Son",
      "intérêt principal est comparatif : suivre son glissement d'une classe",
      "vers la suivante matérialise la migration bioclimatique de la station,",
      "et donc l'évolution du cortège d'essences envisageable."),
    resume = function(dat, ctx) {
      v <- val_annee(dat, ctx$courante, "i_annuel")
      n <- if (nrow(dat)) dat$normale[1] else NA_real_
      cl <- dat$classe[dat$annee == ctx$courante]
      inc <- annee_incomplete(dat, ctx)
      if (inc$oui) return(phrase_incomplete("Indice de De Martonne", ctx, inc$fin))
      if (!is.finite(v)) return("")
      sprintf("Indice de De Martonne %d : %s (classe %s), %s par rapport à la normale %s.",
              ctx$courante, fmt_val(v, "", 1),
              if (length(cl)) as.character(cl[1]) else "n.d.",
              fmt_ecart(v - n, "", 1), ctx$ref_label)
    }
  ),

  # ---- Phenologie ----------------------------------------------------------
  gdd = list(
    id = "gdd", titre = "Degrés-jours de croissance (GDD)", famille = "phenologie",
    calc = calc_gdd, plot = plot_gdd,
    colonnes = c("annee", "gdd", "lissee", "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Cumul annuel de max(0 ; min(T moyenne ; plafond) - base), avec une base",
      "de 5 °C et un plafond de 30 °C par défaut. Le plafonnement évite de",
      "créditer d'un gain de croissance des journées caniculaires qui, en",
      "pratique, la bloquent."),
    lecture = paste(
      "Le cumul de degrés-jours conditionne la vitesse de développement de",
      "tout organisme à sang froid : arbre comme insecte. C'est la même",
      "grandeur qui sera mobilisée par le futur module ravageurs pour estimer",
      "le nombre de générations annuelles de scolytes."),
    resume = function(dat, ctx) resume_standard(
      dat, ctx, "gdd", "Cumul annuel de degrés-jours", "°C-jours", 0,
      tendance_txt = texte_tendance(dat, ctx, "gdd", "°C-jours", 0))
  ),

  veg = list(
    id = "veg", titre = "Saison de végétation", famille = "phenologie",
    calc = calc_saison_vegetation, plot = plot_vegetation,
    colonnes = c("annee", "doy_debut", "doy_fin", "duree", "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Début : premier jour d'une série de 5 jours consécutifs à température",
      "moyenne supérieure à 5 °C, recherchée à partir du 1er février.",
      "Fin : premier jour d'une série de 5 jours consécutifs sous ce même",
      "seuil, recherchée à partir du 1er août. Durée : nombre de jours",
      "separant les deux bornes."),
    lecture = paste(
      "L'allongement de la saison est ambivalent. Il offre plus de temps de",
      "croissance, mais uniquement si l'eau suit : conjugué à un P - ETP",
      "déficitaire, il prolonge surtout la période d'exposition au stress",
      "hydrique. Le décalage de la date de debut est par ailleurs le meilleur",
      "indicateur d'exposition au gel tardif."),
    resume = function(dat, ctx) resume_standard(
      dat, ctx, "duree", "Durée de la saison de végétation", "jours", 0,
      "plus longue que", "plus courte que")
  ),

  # ---- Ombrothermique ------------------------------------------------------
  ombro = list(
    id = "ombro", titre = "Diagramme ombrothermique", famille = "aridite",
    calc = calc_ombrothermique, plot = plot_ombrothermique,
    colonnes = NULL, comparaison = TRUE, fenetre = FALSE,
    selecteur_annee = TRUE,
    # Double axe (precipitations / temperatures) : ggplotly perd l'axe de
    # droite sans prevenir. Le graphique resterait affichable mais faux.
    interactif = FALSE,
    methode = paste(
      "Représentation de Bagnouls et Gaussen : profil mensuel moyen des",
      "précipitations et des températures, l'axe pluviométrique etant dilaté",
      "d'un facteur 2 par rapport à l'axe thermique. Un mois est dit sec",
      "lorsque le cumul de pluie, en millimètres, tombe sous le double de la",
      "température moyenne, en degrés."),
    lecture = paste(
      "La surface ou la courbe thermique surplombe le profil pluviométrique",
      "matérialise la période sèche. Sa durée et sa profondeur constituent le",
      "critère historique de classement bioclimatique des stations",
      "forestières, encore employé dans les catalogues de stations."),
    resume = function(dat, ctx) {
      if (is.null(dat) || !nrow(dat)) return("")
      s_ref <- sum(dat$sec_ref, na.rm = TRUE); s_cur <- sum(dat$sec_cur, na.rm = TRUE)
      mois_secs <- MOIS_LONG[which(dat$sec_ref)]
      n_inc <- sum(dat$cur_partiel %in% TRUE)
      sprintf("Sur la normale %s, %d mois sont secs au sens de Bagnouls-Gaussen%s. En %d : %d mois%s.",
              ctx$ref_label, s_ref,
              if (length(mois_secs)) paste0(" (", paste(mois_secs, collapse = ", "), ")") else "",
              ctx$courante, s_cur,
              if (n_inc) sprintf(" (le mois incomplet n'est pas classé : %d mois jugés sur 12)", 12 - n_inc) else "")
    }
  )
,

  # ---- Profils mensuels et journaliers d'une annee choisie -----------------
  # Ces six entrees partagent selecteur_annee = TRUE : chaque carte porte son
  # propre menu deroulant d'annee. Voir mod_graphique.R pour le mecanisme.

  pre_mois = list(
    id = "pre_mois", titre = "Précipitations mensuelles", famille = "hydrique",
    calc = calc_precip_mensuel, plot = plot_precip_mensuel,
    colonnes = c("mois", "val_ref", "val_cur", "cur_partiel"),
    comparaison = TRUE, fenetre = FALSE, selecteur_annee = TRUE,
    methode = paste(
      "Cumul des précipitations totales de chaque mois, année par année, puis",
      "moyenne inter-annuelle de ces cumuls sur la période de référence. La",
      "normale mensuelle n'est donc jamais une moyenne de valeurs journalières :",
      "l'ordre des opérations importe dès qu'une année est incomplète."),
    lecture = paste(
      "Le cumul annuel masque la répartition. Deux années de 900 mm n'ont pas",
      "le même effet sur un peuplement selon que la pluie tombe en hiver, quand",
      "la végétation est au repos, ou pendant la saison de végétation. C'est",
      "cette répartition que le profil mensuel donne à voir."),
    resume = function(dat, ctx) {
      if (is.null(dat) || !nrow(dat)) return("")
      ecarts <- ecarts_mensuels(dat)
      if (!any(is.finite(ecarts))) return("")
      sec  <- which.min(ecarts); hum <- which.max(ecarts)
      sprintf(paste("Répartition mensuelle des pluies en %d : mois le plus",
                    "déficitaire %s (%s), mois le plus excédentaire %s (%s)."),
              ctx$courante, MOIS_LONG[sec], fmt_ecart(ecarts[sec], "mm", 0),
              MOIS_LONG[hum], fmt_ecart(ecarts[hum], "mm", 0))
    }
  ),

  tmp_mois = list(
    id = "tmp_mois", titre = "Températures mensuelles", famille = "thermique",
    calc = calc_temperature_mensuelle, plot = plot_temperature_mensuelle,
    colonnes = c("mois", "val_ref", "val_cur", "cur_partiel"),
    comparaison = TRUE, fenetre = FALSE, selecteur_annee = TRUE,
    methode = paste(
      "Moyenne des températures journalières de chaque mois, année par année,",
      "puis moyenne inter-annuelle sur la période de référence."),
    lecture = paste(
      "Un même écart annuel de +1 °C n'a pas la même portée sylvicole selon le",
      "mois où il se concentre. Porté par un hiver doux, il avance le",
      "débourrement et expose au gel tardif ; porté par l'été, il aggrave la",
      "contrainte hydrique au moment où elle pèse le plus."),
    resume = function(dat, ctx) {
      if (is.null(dat) || !nrow(dat)) return("")
      ecarts <- ecarts_mensuels(dat)
      if (!any(is.finite(ecarts))) return("")
      pire <- which.max(ecarts)
      sprintf("Écart thermique mensuel maximal en %d : %s en %s.",
              ctx$courante, fmt_ecart(ecarts[pire], "°C", 1), MOIS_LONG[pire])
    }
  ),

  tmp_jour = list(
    id = "tmp_jour", titre = "Températures journalières", famille = "thermique",
    calc = calc_temperature_journaliere, plot = plot_temperature_journaliere,
    colonnes = c("doy", "t_cur", "t_norm", "ecart"),
    comparaison = TRUE, fenetre = FALSE, selecteur_annee = TRUE,
    methode = paste(
      "Température moyenne de chaque jour de l'année sélectionnée, confrontée à",
      "la normale du même jour calculée sur la période de référence et lissée",
      "sur une fenêtre centrée de quinze jours. Le lissage est circulaire : le",
      "1er janvier a pour voisins les derniers jours de décembre, faute de quoi",
      "les deux extrémités de la courbe seraient plus bruitées que le reste —",
      "or c'est la période des gels."),
    lecture = paste(
      "Ce graphique montre ce qu'aucune moyenne annuelle ne dit : la CHRONOLOGIE",
      "des écarts. Une plage rouge en avril signale un printemps précoce ; une",
      "plage bleue brève et profonde en mai, un coup de froid tardif sur des",
      "bourgeons déjà sortis. La surface colorée, elle, donne l'ampleur cumulée",
      "de l'écart."),
    resume = function(dat, ctx) {
      if (is.null(dat) || !nrow(dat) || !any(is.finite(dat$ecart))) return("")
      n_chauds <- sum(dat$ecart > 0, na.rm = TRUE)
      n_tot    <- sum(is.finite(dat$ecart))
      pire <- which.max(dat$ecart)
      sprintf(paste("Sur %d jours renseignés en %d, %d se situent au-dessus de la",
                    "normale journalière %s, soit %.0f %%. Écart le plus fort : %s."),
              n_tot, ctx$courante, n_chauds, ctx$ref_label,
              100 * n_chauds / max(n_tot, 1), fmt_ecart(dat$ecart[pire], "°C", 1))
    }
  ),

  neige_mois = list(
    id = "neige_mois", titre = "Précipitations neigeuses mensuelles",
    famille = "neige",
    calc = calc_neige_mensuelle, plot = plot_neige_mensuelle,
    colonnes = c("mois", "val_ref", "val_cur", "cur_partiel"),
    comparaison = TRUE, fenetre = FALSE, selecteur_annee = TRUE,
    methode = paste(
      "Cumul mensuel de la phase solide des précipitations (PRENEI_Q), exprimé",
      "en équivalent en eau, moyenné sur la période de référence selon le même",
      "ordre d'opérations que les précipitations totales."),
    lecture = paste(
      "La saisonnalité de la neige compte autant que son cumul. Un enneigement",
      "qui se décale vers l'hiver profond, en se raréfiant en début et en fin de",
      "saison, réduit la durée de protection du sol et la restitution printanière",
      "à la réserve utile, à cumul annuel pourtant inchangé."),
    resume = function(dat, ctx) {
      if (is.null(dat) || !nrow(dat) || !any(is.finite(dat$val_cur))) return("")
      tot_cur <- sum(dat$val_cur, na.rm = TRUE)
      tot_ref <- sum(dat$val_ref, na.rm = TRUE)
      if (tot_cur < 1 && tot_ref < 1) {
        return("Précipitations neigeuses négligeables sur ce point, en référence comme en année comparée.")
      }
      sprintf("Neige cumulée en %d : %s, contre %s pour la normale %s.",
              ctx$courante, fmt_val(tot_cur, "mm", 0),
              fmt_val(tot_ref, "mm", 0), ctx$ref_label)
    }
  )
,

  # ---- Demande evaporative de l'atmosphere --------------------------------
  vpd = list(
    id = "vpd", titre = "Déficit de pression de vapeur", famille = "aridite",
    calc = calc_vpd, plot = plot_vpd,
    colonnes = c("annee", "vpd_moy", "vpd_jja", "n_stress", "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Déficit de saturation journalier calculé selon la FAO-56 (éq. 12 et 19) :",
      "es = [es(Tmax) + es(Tmin)] / 2, ea = es × HU / 100, VPD = es - ea, puis",
      "moyenné sur l'année. La pression de saturation croît exponentiellement avec",
      "la température : elle est donc évaluée sur les deux extrêmes, et non sur la",
      "seule température moyenne, qui la sous-estimerait d'autant plus que",
      "l'amplitude thermique est forte. L'humidité SAFRAN étant une moyenne",
      "journalière, le VPD obtenu est un VPD moyen, plus faible que celui du milieu",
      "d'après-midi effectivement subi par la plante. (Les versions antérieures à",
      "la 1.5 surestimaient le VPD de 5 à 20 %.)"),
    lecture = paste(
      "Le VPD dit ce que l'ATMOSPHÈRE réclame à l'arbre, quand le SWI dit ce que",
      "le SOL peut lui fournir. Les deux se lisent ensemble : un sol encore",
      "pourvu n'empêche pas la fermeture des stomates si l'air est trop sec, et",
      "c'est ce découplage qui explique des pertes de croissance sans déficit",
      "hydrique édaphique apparent. La fermeture des stomates est progressive et",
      "propre à chaque essence : elle s'amorce quand le VPD s'élève",
      "(Grossiord et al. 2020), sans valeur universelle. Le repère de 1,5 kPa,",
      "réglable, est conventionnel et s'applique ici à un VPD moyen journalier,",
      "plus faible que celui de l'après-midi : les jours comptés au-dessus sont des",
      "jours d'air très sec, non des jours de stomates fermés. La tendance de long",
      "terme importe plus que le niveau absolu, celui-ci dépendant du pas de temps",
      "de l'humidité disponible."),
    resume = function(dat, ctx) {
      if (is.null(dat) || !nrow(dat)) return("")
      l <- dat[dat$annee == ctx$courante, , drop = FALSE]
      inc <- annee_incomplete(dat, ctx)
      if (inc$oui) return(phrase_incomplete("VPD moyen", ctx, inc$fin))
      if (!nrow(l) || !is.finite(l$vpd_moy[1])) return("")
      paste(sprintf(paste("VPD moyen en %d : %s (%s par rapport à la normale %s).",
                    "Moyenne de juin à août : %s."),
              ctx$courante, fmt_val(l$vpd_moy[1], "kPa", 2),
              fmt_ecart(l$anomalie[1], "kPa", 2), ctx$ref_label,
              fmt_val(l$vpd_jja[1], "kPa", 2)),
            texte_tendance(dat, ctx, "vpd_moy", "kPa", 3))
    }
  ),

  vpd_mois = list(
    id = "vpd_mois", titre = "Déficit de pression de vapeur mensuel",
    famille = "aridite",
    calc = calc_vpd_mensuel, plot = plot_vpd_mensuel,
    colonnes = c("mois", "val_ref", "val_cur", "cur_partiel"),
    comparaison = TRUE, fenetre = FALSE, selecteur_annee = TRUE,
    methode = "Moyenne mensuelle du VPD journalier, selon le même ordre d'opérations que les autres profils mensuels.",
    lecture = paste(
      "La demande évaporative se concentre sur quelques mois. Un excédent de VPD",
      "en juillet ne se compense pas par un déficit en décembre : c'est la forme",
      "de la courbe pendant la saison de végétation qui compte, pas sa moyenne."),
    resume = function(dat, ctx) {
      if (is.null(dat) || !nrow(dat)) return("")
      ecarts <- ecarts_mensuels(dat)
      if (!any(is.finite(ecarts))) return("")
      pire <- which.max(ecarts)
      sprintf("Écart de VPD le plus marqué en %d : %s en %s.",
              ctx$courante, fmt_ecart(ecarts[pire], "kPa", 2), MOIS_LONG[pire])
    }
  ),

  # ---- Regime de vent ------------------------------------------------------
  vent = list(
    id = "vent", titre = "Vent moyen annuel", famille = "vent",
    calc = calc_vent, plot = plot_vent,
    colonnes = c("annee", "ff_moy", "ff_max", "n_ventes", "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Moyenne annuelle du vent moyen journalier à 10 mètres (FF_Q), nombre de",
      "journées dont la moyenne dépasse le seuil réglable, et maximum des",
      "moyennes journalières."),
    lecture = paste(
      "AVERTISSEMENT DE PORTÉE — à lire avant toute conclusion. La source (SAFRAN",
      "ou vent moyen journalier d'une station) ne fournit qu'un vent MOYEN JOURNALIER, jamais de rafale. Or un chablis se",
      "déclenche sur une rafale de quelques secondes, pas sur une moyenne de",
      "vingt-quatre heures : la tempête du 26 décembre 1999 apparaît à peine sur",
      "une telle série. Ce graphique décrit donc un RÉGIME DE VENT — son niveau",
      "habituel, sa saisonnalité, son évolution — et n'autorise aucune lecture",
      "en termes de risque de chablis. Pour cela il faut des rafales, que",
      "fournit la réanalyse ERA5 avec sa variable fg10."),
    resume = function(dat, ctx) {
      if (is.null(dat) || !nrow(dat)) return("")
      l <- dat[dat$annee == ctx$courante, , drop = FALSE]
      inc <- annee_incomplete(dat, ctx)
      if (inc$oui) return(phrase_incomplete("Vent moyen", ctx, inc$fin))
      if (!nrow(l) || !is.finite(l$ff_moy[1])) return("")
      paste(sprintf(paste("Vent moyen en %d : %s (%s par rapport à la normale %s),",
                    "%d journée(s) au-dessus du seuil venteux."),
              ctx$courante, fmt_val(l$ff_moy[1], "m/s", 1),
              fmt_ecart(l$anomalie[1], "m/s", 1), ctx$ref_label, l$n_ventes[1]),
            texte_tendance(dat, ctx, "ff_moy", "m/s", 2))
    }
  ),

  vent_mois = list(
    id = "vent_mois", titre = "Vent moyen mensuel", famille = "vent",
    calc = calc_vent_mensuel, plot = plot_vent_mensuel,
    colonnes = c("mois", "val_ref", "val_cur", "cur_partiel"),
    comparaison = TRUE, fenetre = FALSE, selecteur_annee = TRUE,
    methode = "Moyenne mensuelle du vent moyen journalier, référence contre année sélectionnée.",
    lecture = paste(
      "La saisonnalité du vent conditionne l'exposition des peuplements au moment",
      "où ils sont le plus vulnérables : sol gorgé d'eau en hiver, feuillage",
      "développé au printemps. Ici encore, il s'agit de moyennes journalières et",
      "non de rafales."),
    resume = NULL
  )
,

  vent_soutenu = list(
    id = "vent_soutenu", titre = "Journées de vent soutenu", famille = "vent",
    calc = calc_vent_soutenu, plot = plot_vent_soutenu,
    colonnes = c("annee", "n_p95", "seuil_p95", "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Nombre de journées dont le vent moyen dépasse strictement le 95e",
      "percentile des vents moyens journaliers de la période de référence,",
      "calculé SUR CE POINT. Le seuil est donc relatif et non absolu.",
      "La comparaison est stricte : le vent moyen journalier étant fortement",
      "discrétisé, un test large compterait toutes les journées égales au",
      "percentile, soit la quasi-totalité de l'année sur un point peu venté."),
    lecture = paste(
      "Un seuil absolu ne veut pas dire la même chose partout : 8 m/s de moyenne",
      "journalière est exceptionnel dans le Bourbonnais et ordinaire sur une",
      "crête cévenole. Le percentile de référence répond à une question qui a un",
      "sens partout : cette année a-t-elle été plus ventée que d'habitude ICI ?",
      "Le chiffre devient comparable d'un point à l'autre, et une tendance à la",
      "hausse signale un régime de vent qui se décale, non un point venté."),
    resume = function(dat, ctx) {
      if (is.null(dat) || !nrow(dat)) return("")
      l <- dat[dat$annee == ctx$courante, , drop = FALSE]
      inc <- annee_incomplete(dat, ctx)
      if (inc$oui) return(phrase_incomplete("Journées de vent soutenu", ctx, inc$fin))
      if (!nrow(l) || !is.finite(l$n_p95[1])) return("")
      sprintf(paste("%d journée(s) de vent soutenu en %d (au-delà de %s m/s),",
                    "contre %s en moyenne sur la référence %s."),
              l$n_p95[1], ctx$courante, fmt_fr(l$seuil_p95[1], 1),
              fmt_fr(l$normale[1], 0), ctx$ref_label)
    }
  ),

  vent_sol = list(
    id = "vent_sol", titre = "Vent soutenu sur sol gorgé d'eau",
    famille = "vent",
    calc = calc_vent_sol_sature, plot = plot_vent_sol_sature,
    colonnes = c("annee", "n_combo", "n_sature", "seuil_p95", "seuil_swi",
                 "normale", "anomalie", "ecart_type_ref", "n_manquants", "partielle"),
    comparaison = TRUE, fenetre = FALSE,
    methode = paste(
      "Nombre de journées réunissant deux conditions : vent moyen au-delà du 95e",
      "percentile du point, et indice d'humidité des sols au-delà du seuil de",
      "saturation réglable. Les deux variables proviennent de la même série",
      "source, au même pas de temps journalier."),
    lecture = paste(
      "Un arbre ne tombe pas seulement parce qu'il vente : il tombe parce qu'il",
      "vente ALORS QUE son ancrage est réduit. Un sol saturé d'eau retient moins",
      "bien les racines (Gardiner et al. 2016) : un vent qui n'aurait rien produit",
      "sur sol ressuyé peut alors déchausser des arbres. Les grandes tempêtes à",
      "chablis d'Europe surviennent surtout en hiver, saison où les sols sont",
      "souvent saturés.",
      "PORTÉE : cet indicateur mesure une EXPOSITION, pas un risque. Il repose",
      "toujours sur des moyennes journalières et jamais sur des rafales. Il dit",
      "combien de journées ont réuni les conditions de fond favorables au",
      "déchaussement, pas si une rafale est survenue ce jour-là."),
    resume = function(dat, ctx) {
      if (is.null(dat) || !nrow(dat)) return("")
      l <- dat[dat$annee == ctx$courante, , drop = FALSE]
      inc <- annee_incomplete(dat, ctx)
      if (inc$oui) return(phrase_incomplete("Journées de vent soutenu sur sol saturé", ctx, inc$fin))
      if (!nrow(l) || !is.finite(l$n_combo[1])) return("")
      sprintf(paste("%d journée(s) en %d ont réuni vent soutenu et sol gorgé",
                    "d'eau, sur %d journée(s) de sol saturé. Moyenne de la",
                    "référence %s : %.0f."),
              l$n_combo[1], ctx$courante, l$n_sature[1], ctx$ref_label,
              l$normale[1])
    }
  )
)


# ------------------------------------------------------------------------------
# Accesseurs du registre — utilises par les modules Shiny
# ------------------------------------------------------------------------------

#' Ordre d'affichage des indicateurs dans le bilan.
#' Modifier ce vecteur suffit a reorganiser toute l'interface.
#' @export
ORDRE_INDICATEURS <- c(
  "bh", "ombro", "tmp", "tmp_mois", "tmp_jour", "gel", "can", "pre", "pre_mois",
  "jja", "swi", "dry", "mart", "vpd", "vpd_mois", "gdd", "veg",
  "petp", "neige", "neige_mois", "vent", "vent_mois", "vent_soutenu", "vent_sol"
)

#' Libelles des familles thematiques.
#' @export
FAMILLES <- c(
  hydrique   = "Eau et bilan hydrique",
  thermique  = "Températures",
  gel        = "Gel",
  aridite    = "Aridité et bioclimat",
  phenologie = "Phénologie",
  neige      = "Neige",
  vent       = "Vent"
)

#' Retourne les indicateurs dans l'ordre d'affichage.
#' @param familles Restreint a une ou plusieurs familles (NULL = toutes).
#' @export
indicateurs_ordonnes <- function(familles = NULL) {
  ids <- ORDRE_INDICATEURS[ORDRE_INDICATEURS %in% names(INDICATEURS)]
  liste <- INDICATEURS[ids]
  if (!is.null(familles)) {
    liste <- liste[vapply(liste, function(i) i$famille %in% familles, logical(1))]
  }
  liste
}

#' Colonnes journalieres dont chaque indicateur ne peut pas se passer.
#'
#' POURQUOI CETTE TABLE
#' --------------------
#' SAFRAN renseigne presque tout, une station beaucoup moins : ni SWI, ni
#' humidite, souvent pas de neige distincte de la pluie. Or les agregations
#' annuelles font `sum(x, na.rm = TRUE)` : sur une colonne entierement vide,
#' elles renvoient ZERO et non NA. Sans garde-fou, une station sans mesure de
#' temperature afficherait « 0 jour de gel » chaque annee — un resultat faux
#' et plausible, le pire cas. Un indicateur dont une colonne requise est vide
#' n'est donc PAS calcule : il apparait dans la liste des indicateurs
#' indisponibles, avec le nom de la variable manquante.
#'
#' Maintenir cette table en regard d'une nouvelle entree du registre : le test
#' « REQUIS couvre le registre » echoue si on l'oublie.
REQUIS <- list(
  bh = c("p_tot", "etp"),         petp = c("p_tot", "etp"),
  swi = "swi",                    dry = "p_tot",
  tmp = "t_moy",                  jja = c("t_moy", "p_tot"),
  can = "t_max",                  gel = "t_min",
  pre = "p_tot",                  neige = "p_sol",
  mart = c("p_tot", "t_moy"),     gdd = "t_moy",
  veg = "t_moy",                  ombro = c("p_tot", "t_moy"),
  pre_mois = "p_tot",             tmp_mois = "t_moy",
  tmp_jour = "t_moy",             neige_mois = "p_sol",
  vpd = "vpd",                    vpd_mois = "vpd",
  vent = "ff",                    vent_mois = "ff",
  vent_soutenu = "ff",            vent_sol = c("ff", "swi")
)
for (.id in names(REQUIS)) {
  if (!is.null(INDICATEURS[[.id]])) INDICATEURS[[.id]]$requiert <- REQUIS[[.id]]
}
rm(.id)

#' Groupe de variables d'une colonne, pour la validite annuelle des stations.
#' etp suit les temperatures (elle en est calculee).
#' @keywords internal
groupe_colonne <- function(colonne) {
  if (colonne %in% c("p_tot", "p_liq", "p_sol")) "p"
  else if (colonne %in% c("t_moy", "t_min", "t_max", "etp")) "t"
  else if (colonne == "ff") "ff"
  else NA_character_
}

#' Libelle lisible d'une colonne manquante.
#' @keywords internal
libelle_colonne <- function(colonne) {
  derivees <- c(vpd = "Humidité relative (nécessaire au déficit de pression de vapeur)",
                etp = "Températures (nécessaires au calcul de l'ETP)")
  if (!is.na(derivees[colonne])) return(unname(derivees[colonne]))
  v <- CONFIG$variables[[colonne]]
  if (is.null(v)) colonne else v$lib
}

#' Colonnes requises par un indicateur et absentes (ou vides) du tableau.
#' @return vecteur de noms de colonnes, character(0) si tout est present.
#' @export
colonnes_manquantes <- function(ind, don) {
  req <- ind$requiert
  if (is.null(req) || is.null(don)) return(character(0))
  req[vapply(req, function(c) is.null(don[[c]]) || all(is.na(don[[c]])), logical(1))]
}

#' Attributs d'une source de donnees, a conserver quand on en extrait des lignes.
#' `[.data.frame` les perd silencieusement : sans cette liste, une fois les
#' annees invalides retirees, l'indicateur ne saurait plus d'ou viennent ses
#' donnees (station ou SAFRAN, quelle ETP, quelle definition de la temperature).
#' @keywords internal
ATTRIBUTS_SOURCE <- c("annees_valides", "qualite_annuelle", "source", "etp_source",
                      "derniere_mesure", "definition_tmoy", "station", "point",
                      "altitude_point", "extrait")

#' Recopie les attributs de source de `de` vers `vers`.
#' @keywords internal
copier_attributs_source <- function(vers, de) {
  for (nm in ATTRIBUTS_SOURCE) {
    v <- attr(de, nm, exact = TRUE)
    if (!is.null(v)) attr(vers, nm) <- v
  }
  vers
}

#' Le tableau journalier tel que l'indicateur doit le voir.
#'
#' Pour une SOURCE QUI A DES LACUNES (une station), `annees_valides` liste, par
#' groupe de variables, les annees assez renseignees. L'indicateur ne recoit
#' que les annees valides pour TOUTES ses colonnes requises : une annee trop
#' lacunaire est absente, jamais comptee comme zero. SAFRAN n'a pas de lacunes
#' et ne porte pas cet attribut : le tableau est rendu tel quel.
#'
#' Les annees retirees sont notees dans l'attribut "annees_exclues", pour que le
#' graphique puisse dire POURQUOI il a un trou (au lieu de le laisser deviner).
#' @export
donnees_pour_indicateur <- function(don, ind) {
  valides <- attr(don, "annees_valides")
  if (is.null(valides) || is.null(ind$requiert)) return(don)
  groupes <- unique(stats::na.omit(vapply(ind$requiert, groupe_colonne, character(1))))
  if (!length(groupes)) return(don)
  ok <- Reduce(intersect, lapply(groupes, function(g) valides[[g]]))
  d <- copier_attributs_source(don[don$annee %in% ok, , drop = FALSE], don)
  attr(d, "annees_exclues") <- sort(setdiff(unique(don$annee), ok))
  d
}

#' Metadonnees de la source, attachees a chaque resultat d'indicateur.
#' Les graphiques et les commentaires en tirent leurs notes (source de l'ETP,
#' annees sans valeur, derniere date connue, definition de la temperature).
#' @keywords internal
meta_donnees <- function(don) {
  list(
    source          = attr(don, "source") %||% "safran",
    etp             = attr(don, "etp_source"),
    definition_tmoy = attr(don, "definition_tmoy"),
    derniere_date   = if (nrow(don)) as.Date(attr(don, "derniere_mesure") %||% max(don$date)) else as.Date(NA),
    annees_exclues  = attr(don, "annees_exclues"),
    altitude_point  = attr(don, "altitude_point"),
    station         = attr(don, "station")
  )
}

#' Calcule UN indicateur, et dit pourquoi il est vide s'il l'est.
#'
#' Trois situations que la v11 confondait sous « Données indisponibles » :
#'   - la source ne mesure pas une variable requise (station sans humidite...) ;
#'   - aucune annee n'est assez renseignee (95 % des jours) ;
#'   - le calcul a echoue : une erreur, qui se corrige, et qui n'est pas une
#'     donnee manquante.
#' Le resultat vide porte alors les attributs "raison" et "message" (phrase
#' prete a afficher). Un resultat non vide porte l'attribut "meta".
#'
#' @param don tableau journalier complet (avec ses attributs de source).
#' @param ind entree du registre.
#' @param ctx contexte de calcul.
#' @export
calculer_un_indicateur <- function(don, ind, ctx) {
  # `detail` : le message d'erreur R, pour la seule raison « erreur ». Il etait
  # consigne dans le journal et nulle part ailleurs : l'utilisateur lisait
  # « erreur consignee » sans rien pouvoir transmettre (audit, S5). La carte
  # l'affiche, nettoye par erreur_lisible().
  vide <- function(raison, message, detail = NULL) {
    r <- data.frame()
    attr(r, "raison") <- raison
    attr(r, "message") <- message
    if (!is.null(detail)) attr(r, "detail") <- detail
    r
  }
  if (is.null(ind)) return(vide("inconnu", "Indicateur inconnu."))
  if (is.null(don) || !nrow(don)) {
    return(vide("aucune_donnee", "Aucune donnée chargée pour ce point."))
  }

  manque <- colonnes_manquantes(ind, don)
  if (length(manque)) {
    return(vide("variable_absente", sprintf(
      "Indicateur non calculable avec cette source : %s non mesuré(e).",
      paste(vapply(manque, libelle_colonne, character(1)), collapse = " ; "))))
  }

  d <- donnees_pour_indicateur(don, ind)
  if (!nrow(d)) {
    return(vide("aucune_annee_valide", sprintf(
      "Aucune année n'est assez renseignée (au moins %.0f %% des jours) pour cet indicateur.",
      100 * CONFIG$stations$seuil_annee)))
  }

  res <- tryCatch(ind$calc(d, ctx), error = function(e) {
    warning(sprintf("Indicateur '%s' en échec : %s", ind$id, conditionMessage(e)),
            call. = FALSE)
    vide("erreur", paste(
      "Le calcul de cet indicateur a échoué (erreur consignée dans le journal de",
      "l'application). Les autres indicateurs ne sont pas affectés."),
      detail = conditionMessage(e))
  })
  if (!is.data.frame(res) || !nrow(res)) {
    if (!is.null(attr(res, "raison"))) return(res)
    return(vide("vide", "Aucun résultat pour cet indicateur sur la période choisie."))
  }
  attr(res, "meta") <- meta_donnees(d)
  res
}

#' Calcule tous les indicateurs demandes en une passe.
#' Chaque calcul est isole : l'echec d'un indicateur ne fait pas tomber le reste.
#' Un indicateur dont une colonne requise est vide n'est pas calcule (voir REQUIS).
#' @return liste nommee de data.frame (data.frame vide, portant les attributs
#'   "raison" et "message", si l'indicateur n'a pas pu etre calcule).
#' @export
calculer_indicateurs <- function(don, ctx, ids = ORDRE_INDICATEURS) {
  stats::setNames(lapply(ids, function(id) {
    calculer_un_indicateur(don, INDICATEURS[[id]], ctx)
  }), ids)
}

#' Resultat d'un indicateur dans la liste de calculer_indicateurs(), ou table vide
#' s'il est absent.
#'
#' Ne pas ecrire `res[[cle]] %||% data.frame()` : `%||%` traite une table VIDE
#' (0 colonne) comme absente et la remplace par une table neuve, sans les
#' attributs « raison », « message » et « detail ». La carte affichait alors le
#' message generique au lieu de la cause reelle (audit, S5).
#' @export
resultat_indicateur <- function(res, cle) {
  r <- res[[cle]]
  if (is.null(r)) data.frame() else r
}

#' Commentaire automatique d'un indicateur, SANS jamais lever d'erreur.
#'
#' Audit, C3 : une erreur dans `ind$resume` devenait une chaine vide, donc un
#' commentaire absent qui ressemblait a « rien a signaler ». Ici l'echec est
#' consigne au journal et porte par l'attribut « erreur » du texte rendu (qui est
#' alors vide), pour que l'appelant puisse le montrer. Un signal reactif (`req`)
#' n'est pas une erreur : il rend un texte vide, sans attribut.
#'
#' @param ind entree du registre ; @param d resultat de l'indicateur ; @param ctx contexte.
#' @return chaine de longueur 1 ; attribut « erreur » (message) si le calcul a echoue.
#' @export
commentaire_indicateur <- function(ind, d, ctx) {
  if (is.null(ind) || is.null(ind$resume)) return("")
  tryCatch(
    ind$resume(d, ctx) %||% "",
    shiny.silent.error = function(e) "",
    error = function(e) {
      journaliser_erreur(sprintf("Commentaire de '%s'", ind$id %||% "?"), e)
      txt <- ""
      attr(txt, "erreur") <- conditionMessage(e)
      txt
    })
}

#' Indicateurs que la source de donnees ne permet pas de calculer.
#' @return data.frame(id, titre, manque) ou manque est une phrase.
#' @export
indicateurs_indisponibles <- function(don, ids = ORDRE_INDICATEURS) {
  lignes <- lapply(ids, function(id) {
    ind <- INDICATEURS[[id]]
    if (is.null(ind)) return(NULL)
    m <- colonnes_manquantes(ind, don)
    if (!length(m)) return(NULL)
    data.frame(id = id, titre = ind$titre,
               manque = paste(vapply(m, libelle_colonne, character(1)), collapse = " ; "),
               stringsAsFactors = FALSE)
  })
  lignes <- Filter(Negate(is.null), lignes)
  if (!length(lignes)) {
    return(data.frame(id = character(0), titre = character(0), manque = character(0),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, lignes)
}
