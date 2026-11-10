# ==============================================================================
# utils_calc.R — COEUR DE CALCUL DES INDICATEURS BIOCLIMATIQUES
# ------------------------------------------------------------------------------
# Toutes les fonctions suivent le meme contrat, ce qui permet au registre
# (registre_indicateurs.R) de les appeler de maniere totalement generique :
#
#     calc_xxx(don, ctx) -> data.frame "tidy"
#
#   don : tableau journalier normalise issu de safran_charger()
#   ctx : liste de contexte, construite par ctx_creer() :
#         $ref       vecteur des annees de la periode de reference
#         $affichage vecteur des annees a afficher
#         $courante  annee mise en avant dans les comparaisons
#         $fenetre   c(mois_debut, mois_fin) pour les indicateurs saisonniers
#         $seuils    copie (eventuellement modifiee) de CONFIG$seuils
#
# Aucune fonction ici ne connait Shiny ni ggplot : elles sont testables seules.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Contexte et helpers transverses
# ------------------------------------------------------------------------------

#' Construit le contexte de calcul partage par tous les indicateurs.
#' @export
ctx_creer <- function(ref = CONFIG$references[[CONFIG$reference_defaut]],
                      affichage = NULL,
                      courante = as.integer(format(Sys.Date(), "%Y")),
                      fenetre = CONFIG$fenetre_defaut,
                      seuils = CONFIG$seuils) {
  ref_annees <- seq.int(min(ref), max(ref))
  list(
    ref       = ref_annees,
    ref_label = sprintf("%d-%d", min(ref), max(ref)),
    affichage = affichage %||% ref_annees,
    courante  = as.integer(courante),
    fenetre   = as.integer(fenetre),
    seuils    = seuils
  )
}

#' Sous-ensemble d'un tableau journalier sur une liste d'annees.
#' @keywords internal
sel_annees <- function(don, annees) don[don$annee %in% annees, , drop = FALSE]

#' Sous-ensemble sur une fenetre de mois.
#' Gere le cas d'une fenetre a cheval sur deux annees civiles (ex. nov -> mars) :
#' les mois de debut d'annee sont alors rattaches a l'annee precedente, de sorte
#' qu'une "saison" reste une unite d'analyse coherente.
#' @keywords internal
sel_fenetre <- function(don, fenetre) {
  m1 <- fenetre[1]; m2 <- fenetre[2]
  if (m1 <= m2) {
    out <- don[don$mois >= m1 & don$mois <= m2, , drop = FALSE]
    out$annee_saison <- out$annee
  } else {
    out <- don[don$mois >= m1 | don$mois <= m2, , drop = FALSE]
    out$annee_saison <- ifelse(out$mois >= m1, out$annee, out$annee - 1L)
  }
  out
}

#' Libelle lisible d'une fenetre de mois.
#' @export
libelle_fenetre <- function(fenetre) {
  sprintf("%s a %s", MOIS_LONG[fenetre[1]], MOIS_LONG[fenetre[2]])
}

#' Moyenne mobile d'une serie ANNUELLE.
#'
#' Ce qui la distingue d'une moyenne mobile sur des lignes (stats::filter) :
#'  - la fenetre se compte en ANNEES, pas en lignes. Quand une annee est absente
#'    (station : annee trop lacunaire) la fenetre de 10 lignes couvrait 11 ans ;
#'  - les annees listees dans `exclure` (annee en cours, incomplete) ne
#'    participent pas : une valeur partielle ne doit pas creer de bosse ;
#'  - une fenetre est calculee des qu'au moins 80 % de ses annees sont
#'    renseignees, sur les annees renseignees, au lieu de disparaitre au premier
#'    trou.
#' La fenetre reste entierement contenue dans la plage des annees : pas de
#' demi-fenetre aux extremites (meme convention que stats::filter, qui place,
#' pour k pair, une annee de plus devant que derriere).
#' @export
lissage_annuel <- function(annees, valeurs, k = CONFIG$graphiques$lissage_annees,
                           exclure = NULL) {
  n <- length(annees)
  sortie <- rep(NA_real_, n)
  if (!n) return(sortie)
  v <- as.numeric(valeurs)
  if (!is.null(exclure)) v[exclure %in% TRUE] <- NA_real_
  tout <- seq.int(min(annees), max(annees))
  if (length(tout) < k) return(sortie)
  x <- rep(NA_real_, length(tout))
  x[match(annees, tout)] <- v
  arriere <- (k - 1L) %/% 2L
  avant   <- k - 1L - arriere
  min_ok  <- ceiling(0.8 * k)
  lisse <- rep(NA_real_, length(tout))
  for (i in seq_along(tout)) {
    a <- i - arriere; b <- i + avant
    if (a < 1L || b > length(tout)) next
    w <- x[a:b]
    if (sum(is.finite(w)) >= min_ok) lisse[i] <- mean(w, na.rm = TRUE)
  }
  lisse[match(annees, tout)]
}


# ------------------------------------------------------------------------------
# Mise en forme des nombres, a la francaise (virgule decimale)
# ------------------------------------------------------------------------------

#' Nombre en francais : virgule decimale, signe explicite si demande.
#' Vectorisee. NA / infini -> "n.d.".
#' @keywords internal
fmt_fr <- function(x, dec = 1, signe = FALSE) {
  vapply(x, function(v) {
    if (!is.finite(v)) return("n.d.")
    s <- formatC(abs(v), format = "f", digits = dec, decimal.mark = ",")
    # Un zero arrondi ne porte pas de signe : « -0,0 » n'a pas de sens.
    if (as.numeric(sub(",", ".", s)) == 0) return(s)
    if (v < 0) paste0("−", s) else if (signe) paste0("+", s) else s
  }, character(1))
}

#' Valeur de p, a la francaise.
#' @keywords internal
fmt_p <- function(p) {
  if (!is.finite(p)) return("n.d.")
  if (p < 0.001) "p < 0,001" else paste0("p = ", fmt_fr(p, 3))
}


# ------------------------------------------------------------------------------
# Tendance : pente de Sen, test de Mann-Kendall corrige de l'autocorrelation
# ------------------------------------------------------------------------------
# POURQUOI PAS UNE REGRESSION LINEAIRE
# ------------------------------------
# La regression par moindres carres (v1.1) suppose des ecarts independants d'une
# annee a l'autre. Les series climatiques ne le sont pas : une annee chaude en
# precede souvent une autre. L'independance supposee fait paraitre l'information
# plus abondante qu'elle n'est, et les p-values trop petites : sur des series
# sans aucune tendance et une autocorrelation de 0,3 a 0,5, 14 a 26 % de faux
# positifs au lieu de 5 % (mesure de l'audit de v11).
#
# CE QUI EST FAIT ICI
#  - pente : mediane des pentes entre toutes les paires d'annees (Theil-Sen).
#    Robuste a une annee extreme, ce que la regression n'est pas ;
#  - test : Mann-Kendall, non parametrique ;
#  - autocorrelation : la variance du test est multipliee par (1 + r) / (1 - r),
#    r etant l'autocorrelation d'ordre 1 des residus, corrigee de son biais.
#    On ne corrige que dans le sens prudent (r > 0) ;
#  - intervalle de confiance de la pente : methode de Sen, construit avec la
#    meme variance corrigee, donc elargi comme la p-value ;
#  - aucune tendance n'est estimee sous CONFIG$tendance$n_min annees COMPLETES :
#    une pente sur dix ans est surtout de la variabilite.
#
# La correction est une approximation, pas une garantie : elle ramene les faux
# positifs pres du niveau nominal pour une autocorrelation moderee, moins bien
# pour une forte autocorrelation sur serie courte. Ces taux ont ete mesures par
# simulation (voir tests/simulation_tendance.R).

#' Test de rupture de Pettitt (rupture unique de niveau) — INDICATIF.
#' @return NULL, ou list(annee, ecart, p) si p < `seuil_p`.
#' @keywords internal
rupture_pettitt <- function(x, y, seuil_p = 0.01) {
  n <- length(y)
  if (n < 20L) return(NULL)
  o <- order(x); x <- x[o]; y <- y[o]
  # U_k = somme_{i <= k} somme_{j > k} signe(y_i - y_j)
  sg <- sign(outer(y, y, "-"))
  U <- vapply(seq_len(n - 1L), function(k) sum(sg[seq_len(k), (k + 1L):n]), numeric(1))
  k <- which.max(abs(U))
  p <- min(1, 2 * exp(-6 * max(abs(U))^2 / (n^3 + n^2)))
  if (!is.finite(p) || p >= seuil_p) return(NULL)
  list(annee = x[k], ecart = mean(y[(k + 1L):n]) - mean(y[seq_len(k)]), p = p)
}

#' Tendance d'une serie annuelle (Sen + Mann-Kendall corrige).
#'
#' @param annees,valeurs series de meme longueur.
#' @param unite unite de la grandeur, pour le libelle.
#' @param exclure vecteur logique : annees a ecarter (annee en cours incomplete).
#' @param dec decimales de la pente.
#' @return liste. `ok` indique si une tendance a ete estimee ; sinon `libelle`
#'   dit pourquoi. Champs : n, pente_dec, ic_bas_dec, ic_haut_dec, p, r1,
#'   n_eff, ordonnee, centre, significative, rupture, libelle, libelle_court.
#' @export
tendance <- function(annees, valeurs, unite = "", exclure = NULL, dec = 2,
                     n_min = CONFIG$tendance$n_min,
                     niveau = CONFIG$tendance$niveau) {
  x <- as.numeric(annees); y <- as.numeric(valeurs)
  ok <- is.finite(x) & is.finite(y)
  if (!is.null(exclure)) ok <- ok & !(exclure %in% TRUE)
  x <- x[ok]; y <- y[ok]
  o <- order(x); x <- x[o]; y <- y[o]
  n <- length(y)

  vide <- function(motif) list(
    ok = FALSE, n = n, pente_dec = NA_real_, ic_bas_dec = NA_real_,
    ic_haut_dec = NA_real_, p = NA_real_, r1 = NA_real_, n_eff = NA_real_,
    ordonnee = NA_real_, centre = c(NA_real_, NA_real_), significative = NA,
    rupture = NULL, libelle = motif, libelle_court = motif)

  if (n < n_min) {
    return(vide(sprintf(
      "Tendance non estimée : %d année(s) complète(s) retenue(s), %d au minimum.",
      n, n_min)))
  }

  # -- Pente de Sen ----------------------------------------------------------
  paires <- which(upper.tri(matrix(FALSE, n, n)), arr.ind = TRUE)
  i <- paires[, 1]; j <- paires[, 2]
  dx <- x[j] - x[i]
  garde <- dx != 0
  pentes <- (y[j] - y[i])[garde] / dx[garde]
  b <- stats::median(pentes)
  a <- stats::median(y - b * x)

  # -- Mann-Kendall ----------------------------------------------------------
  S <- sum(sign(y[j] - y[i]))
  t_ex <- as.numeric(table(y)); t_ex <- t_ex[t_ex > 1]
  var0 <- (n * (n - 1) * (2 * n + 5) - sum(t_ex * (t_ex - 1) * (2 * t_ex + 5))) / 18

  # -- Autocorrelation d'ordre 1 des residus, corrigee de son biais ----------
  e <- y - (a + b * x)
  e <- e - mean(e)
  r1 <- sum(e[-1] * e[-n]) / sum(e^2)
  if (!is.finite(r1)) r1 <- 0
  r1c <- min(max(r1 + (1 + 3 * r1) / n, 0), 0.9)
  vif <- (1 + r1c) / (1 - r1c)
  variance <- var0 * vif

  z <- if (S == 0) 0 else (S - sign(S)) / sqrt(variance)
  p <- 2 * stats::pnorm(-abs(z))

  # -- Intervalle de confiance de Sen ---------------------------------------
  N <- length(pentes)
  C <- stats::qnorm(1 - (1 - niveau) / 2) * sqrt(variance)
  tri <- sort(pentes)
  i_bas  <- max(1L, floor((N - C) / 2))
  i_haut <- min(N, ceiling((N + C) / 2 + 1))
  ic <- c(tri[i_bas], tri[i_haut])

  pente_dec <- 10 * b
  ic_dec    <- 10 * ic
  sig <- p < (1 - niveau)

  corps <- sprintf("%s %s/décennie [IC %.0f %% : %s à %s]",
                   fmt_fr(pente_dec, dec, signe = TRUE), unite, 100 * niveau,
                   fmt_fr(ic_dec[1], dec, signe = TRUE),
                   fmt_fr(ic_dec[2], dec, signe = TRUE))
  list(
    ok = TRUE, n = n,
    pente_dec = pente_dec, ic_bas_dec = ic_dec[1], ic_haut_dec = ic_dec[2],
    p = p, r1 = r1c, n_eff = n / vif,
    ordonnee = a, centre = c(stats::median(x), stats::median(y)),
    pentes_ic = ic,
    significative = sig,
    rupture = rupture_pettitt(x, y - b * x),
    libelle = sprintf(
      "Tendance (pente de Sen) : %s ; test de Mann-Kendall corrigé de l'autocorrélation : %s, %s ; %d années complètes.",
      corps, fmt_p(p), if (sig) "tendance significative" else "non significative", n),
    libelle_court = sprintf("%s, %s", corps, fmt_p(p))
  )
}


# ------------------------------------------------------------------------------
# Gel tardif et altitude (audit v1.1, M7)
# ------------------------------------------------------------------------------

#' Retard du debourrement avec l'altitude, en jours par 100 m (fourchette).
#' Vitasse, Signarbieux & Fu (2018a) : environ 5 semaines par 1 000 m au debut des
#' annees 1960, au plus 3 semaines aujourd'hui, soit 3,5 a 2,1 jours par 100 m.
#' C'est un ORDRE DE GRANDEUR de terrain europeen, pas une loi : il depend de
#' l'essence, de l'annee et du massif.
#' @keywords internal
GEL_RETARD_J_PAR_100M <- c(bas = 2, haut = 3.5)

#' Altitude (m) au-dela de laquelle on signale que la fin de la saison de gel tardif
#' par defaut (30 juin) risque d'etre trop precoce. En dessous, le decalage attendu
#' (moins d'une a deux semaines) reste dans la marge de la saison.
#' @keywords internal
GEL_ALTITUDE_ALERTE_M <- 400

#' Conseil sur la fin de la saison de gel tardif, selon l'altitude du point.
#'
#' Le debourrement est estime a partir de la temperature du point (calc_gel_tardif) :
#' il y est donc retardé, comme le gel. Mais la FIN de la saison de gel tardif est une
#' date fixe (30 juin par defaut). L'application connait l'altitude du point ; elle s'en
#' sert pour dire, SANS modifier le calcul, que cette date peut etre trop precoce en
#' montagne.
#'
#' @param alt altitude du point, m.
#' @param fin_saison jour julien de fin de la saison de gel tardif en vigueur.
#' @return une phrase, ou NULL si l'altitude est inconnue ou inferieure a
#'   GEL_ALTITUDE_ALERTE_M.
#' @export
gel_tardif_conseil <- function(alt, fin_saison = CONFIG$seuils$gel_tardif_fin) {
  if (length(alt) != 1L || !is.finite(alt) || alt < GEL_ALTITUDE_ALERTE_M) return(NULL)
  d <- round(alt / 100 * GEL_RETARD_J_PAR_100M)
  sprintf(paste(
    "Gel tardif : le point est à %.0f m. Le débourrement y est retardé d'environ",
    "%d à %d jours par rapport à la plaine (de 2 à 3,5 jours par 100 m,",
    "Vitasse et al. 2018a) et le débourrement estimé suit la température du point ;",
    "mais la saison de gel tardif s'arrête toujours au jour %d de l'année.",
    "Reculer cette date dans Paramètres si le peuplement débourre tard ; le calcul",
    "n'est pas modifié."),
    alt, d[["bas"]], d[["haut"]], as.integer(fin_saison))
}


# ------------------------------------------------------------------------------
# Completude : quelles annees sont entieres, combien de jours manquent
# ------------------------------------------------------------------------------

#' Dernier jour d'un mois.
#' @keywords internal
fin_de_mois <- function(annee, mois) {
  m <- mois + 1L
  a <- annee + (m > 12L)
  m <- ifelse(m > 12L, 1L, m)
  as.Date(sprintf("%d-%02d-01", a, m)) - 1L
}

#' Completude par annee (ou par saison) d'un tableau journalier.
#'
#' DEUX NOTIONS DISTINCTES, QUE LA v11 CONFONDAIT
#'  - une annee est PARTIELLE si le tableau s'arrete avant sa fin (annee en
#'    cours) : on ne doit pas la comparer a une normale annuelle ;
#'  - des jours sont MANQUANTS si le tableau couvre la periode mais que la
#'    mesure est absente (station) : le cumul est alors sous-estime. Ils sont
#'    comptes et signales, jamais compenses : toute compensation serait une
#'    hypothese sur le temps qu'il a fait ces jours-la.
#'
#' @param fenetre NULL pour l'annee civile, sinon c(mois_debut, mois_fin) : la
#'   completude est alors celle de la SAISON (ete, fenetre du P - ETP...).
#' @param vars colonnes devant etre renseignees pour qu'un jour compte. NULL :
#'   un jour compte des qu'une des variables principales est renseignee.
#' @return data.frame(annee, n_jours, attendu, couverts, n_manquants, partielle,
#'   dernier_jour).
#' @keywords internal
completude <- function(don, fenetre = NULL, vars = NULL) {
  vide <- data.frame(annee = integer(0), n_jours = integer(0), attendu = integer(0),
                     couverts = integer(0), n_manquants = integer(0),
                     partielle = logical(0), dernier_jour = as.Date(character(0)))
  if (is.null(don) || !nrow(don)) return(vide)

  if (is.null(fenetre)) {
    cle <- don$annee; dates <- don$date; d <- don
  } else {
    d <- sel_fenetre(don, fenetre)
    if (!nrow(d)) return(vide)
    cle <- d$annee_saison; dates <- d$date
  }

  if (is.null(vars)) {
    cols <- intersect(c("t_moy", "t_min", "t_max", "p_tot"), names(d))
    ren <- if (length(cols)) Reduce(`|`, lapply(cols, function(v) !is.na(d[[v]]))) else rep(TRUE, nrow(d))
  } else {
    cols <- intersect(vars, names(d))
    ren <- if (length(cols)) Reduce(`&`, lapply(cols, function(v) !is.na(d[[v]]))) else rep(TRUE, nrow(d))
  }

  ann <- sort(unique(cle))
  f <- factor(cle, levels = ann)
  n_j  <- as.integer(table(f))
  n_r  <- as.integer(tapply(ren, f, sum))
  dern <- as.Date(as.numeric(tapply(as.numeric(dates), f, max)), origin = "1970-01-01")

  if (is.null(fenetre)) {
    debut <- as.Date(sprintf("%d-01-01", ann)); fin <- as.Date(sprintf("%d-12-31", ann))
  } else {
    m1 <- fenetre[1]; m2 <- fenetre[2]
    debut <- as.Date(sprintf("%d-%02d-01", ann, m1))
    fin   <- fin_de_mois(if (m1 <= m2) ann else ann + 1L, m2)
  }
  attendu  <- as.integer(fin - debut) + 1L
  couverts <- ifelse(dern < fin, as.integer(dern - debut) + 1L, attendu)

  data.frame(annee = as.integer(ann), n_jours = n_j, attendu = attendu,
             couverts = couverts,
             n_manquants = pmax(0L, couverts - n_r),
             # Deux jours de retard tolerés : SAFRAN publie avec un decalage.
             partielle = n_j < attendu - 2L,
             dernier_jour = dern)
}

#' Ajoute a un tableau annuel les colonnes de completude et la normale de
#' reference d'une variable donnee.
#'
#' Colonnes ajoutees : n_jours, n_manquants, partielle, dernier_jour, normale,
#' ecart_type_ref (ecart-type inter-annuel des annees de reference), anomalie.
#' L'anomalie d'une annee PARTIELLE est NA : comparer huit mois a une normale de
#' douze mois n'a pas de sens. La valeur brute reste affichee, signalee comme
#' incomplete ; la comparaison a periode egale est faite par calc_synthese().
#'
#' @param fenetre,vars voir completude().
#' @param comp completude deja calculee (cas ou l'appelant la precise).
#' @keywords internal
enrichir_annuel <- function(res, don, ctx, colonne, fenetre = NULL, vars = NULL,
                            comp = NULL) {
  if (is.null(comp)) comp <- completude(don, fenetre, vars)
  cols <- c("annee", "n_jours", "n_manquants", "partielle", "dernier_jour")

  if (!nrow(res)) {
    for (nm in c("n_jours", "n_manquants")) res[[nm]] <- integer(0)
    res$partielle <- logical(0); res$dernier_jour <- as.Date(character(0))
    for (nm in c("normale", "ecart_type_ref", "anomalie")) res[[nm]] <- numeric(0)
    attr(res, "ref_n") <- c(utilisees = 0L, attendues = length(ctx$ref))
    return(res)
  }

  res <- merge(res, comp[, cols], by = "annee", all.x = TRUE)
  res$partielle <- res$partielle %in% TRUE
  res$n_manquants[is.na(res$n_manquants)] <- 0L

  st <- stats_ref(res, colonne, ctx)
  res$normale <- st$normale
  res$ecart_type_ref <- st$sd
  res$anomalie <- ifelse(res$partielle, NA_real_, res[[colonne]] - res$normale)

  res <- res[order(res$annee), , drop = FALSE]
  rownames(res) <- NULL
  attr(res, "ref_n") <- c(utilisees = st$n, attendues = length(ctx$ref))
  res
}

#' Normale, ecart-type et effectif d'une colonne annuelle sur la reference.
#' Seules comptent les annees de reference COMPLETES et renseignees.
#' @keywords internal
stats_ref <- function(res, colonne, ctx) {
  ok <- res$annee %in% ctx$ref & !(res$partielle %in% TRUE) & is.finite(res[[colonne]])
  v <- res[[colonne]][ok]
  list(normale = if (length(v)) mean(v) else NA_real_,
       sd      = if (length(v) >= 3L) stats::sd(v) else NA_real_,
       n       = length(v))
}

#' Classe d'un ecart a la normale : "haut", "bas" ou "neutre".
#'
#' Un ecart n'est colore que s'il depasse `bande` fois l'ecart-type des annees de
#' reference. Colorer +0,01 °C comme +2 °C serait une fausse precision.
#' Sans ecart-type (reference trop courte) on ne juge pas : "neutre".
#' @param anomalie,sd_ref vecteurs de meme longueur (sd_ref peut etre scalaire).
#' @export
classe_ecart <- function(anomalie, sd_ref, bande = CONFIG$tendance$bande_neutre) {
  sd_ref <- rep_len(sd_ref, length(anomalie))
  out <- ifelse(!is.finite(anomalie) | !is.finite(sd_ref), "neutre",
         ifelse(abs(anomalie) < bande * sd_ref | abs(anomalie) < 1e-9, "neutre",
         ifelse(anomalie > 0, "haut", "bas")))
  out
}

#' Agregation annuelle generique.
#' @param fun "somme" ou "moyenne"
#' @keywords internal
agreger_annuel <- function(don, colonne, fun = c("somme", "moyenne"),
                           cle = "annee", nom = "valeur") {
  fun <- match.arg(fun)
  if (!nrow(don)) {
    res <- data.frame(annee = integer(0), v = numeric(0))
    names(res) <- c("annee", nom)
    return(res)
  }
  f <- if (fun == "somme") function(x) sum(x, na.rm = TRUE) else function(x) mean(x, na.rm = TRUE)
  res <- stats::aggregate(list(v = don[[colonne]]), by = list(k = don[[cle]]), FUN = f)
  names(res) <- c("annee", nom)
  res$annee <- as.integer(res$annee)
  res
}


# ------------------------------------------------------------------------------
# 2. INDICATEURS MENSUELS (profil moyen : reference vs annee mise en avant)
# ------------------------------------------------------------------------------

#' Nombre de jours d'un mois.
#' @keywords internal
jours_du_mois <- function(annee, mois) {
  as.integer(fin_de_mois(annee, mois) - as.Date(sprintf("%d-%02d-01", annee, mois))) + 1L
}

#' Profil mensuel (12 valeurs) d'un sous-ensemble de jours, sur une ou plusieurs
#' annees.
#'
#' Une valeur est d'abord calculee par (annee, mois), puis moyennee entre les
#' annees : on ne moyenne jamais des cumuls journaliers.
#'
#' Un mois est COMPLET s'il compte au moins 90 % de ses jours renseignes. Pour la
#' REFERENCE, les mois incomplets sont ecartes (un cumul de 18 jours n'est pas un
#' cumul mensuel). Pour l'ANNEE COMPAREE, ils sont conserves — c'est tout ce
#' qu'on sait — mais signales : le dernier mois de l'annee en cours est presque
#' toujours incomplet.
#'
#' @param colonne colonne agregee ; @param vars colonnes devant etre renseignees.
#' @param strict TRUE pour ecarter les mois incomplets (reference).
#' @return list(v = 12 valeurs, partiel = 12 logiques).
#' @keywords internal
profil_mois <- function(d, colonne, fun = c("somme", "moyenne"), vars = colonne,
                        strict = FALSE) {
  fun <- match.arg(fun)
  sortie <- list(v = rep(NA_real_, 12), partiel = rep(FALSE, 12))
  if (is.null(d) || !nrow(d)) return(sortie)

  ren <- Reduce(`&`, lapply(intersect(vars, names(d)), function(v) !is.na(d[[v]])))
  f <- if (fun == "somme") function(x) sum(x, na.rm = TRUE) else function(x) mean(x, na.rm = TRUE)
  cum <- stats::aggregate(list(v = d[[colonne]]),
                          by = list(annee = d$annee, mois = d$mois), FUN = f)
  nb  <- stats::aggregate(list(n = ren), by = list(annee = d$annee, mois = d$mois), FUN = sum)
  cum <- merge(cum, nb, by = c("annee", "mois"))
  cum$complet <- cum$n >= 0.9 * jours_du_mois(cum$annee, cum$mois)

  garde <- if (strict) cum[cum$complet, , drop = FALSE] else cum[cum$n > 0, , drop = FALSE]
  if (!nrow(garde)) return(sortie)
  moy <- stats::aggregate(list(v = garde$v), by = list(mois = garde$mois), FUN = mean)
  sortie$v[moy$mois] <- moy$v
  inc <- garde[!garde$complet, , drop = FALSE]
  sortie$partiel[unique(inc$mois)] <- TRUE
  sortie
}

#' Normale mensuelle (12 valeurs) d'une colonne sur les mois COMPLETS de la reference.
#'
#' Mode « base precalculee » (utils_base.R) : la reference journaliere n'est pas chargee,
#' la normale vient du jeu publie (`ctx$base`, lu a la demande) ; le reste du calcul est
#' inchange. Les deux chemins rendent la meme chose (tests/test_v22.R).
#' @keywords internal
ref_mensuelle <- function(don, ctx, colonne, fun, vars = colonne) {
  b <- ctx$base[[paste0("mois_", colonne)]]
  if (!is.null(b)) return(list(v = as.numeric(b), partiel = rep(FALSE, 12)))
  profil_mois(sel_annees(don, ctx$ref), colonne, fun, vars, strict = TRUE)
}

#' 01 — Bilan hydrique mensuel P - ETP.
#' Cumul mensuel des precipitations totales moins cumul mensuel d'ETP.
#' Un solde negatif signale un mois ou la demande evaporative excede l'apport :
#' la reserve utile du sol est sollicitee.
#' @export
calc_bilan_hydrique <- function(don, ctx) {
  v <- c("p_tot", "etp")
  rp <- ref_mensuelle(don, ctx, "p_tot", "somme", v)
  re <- ref_mensuelle(don, ctx, "etp",   "somme", v)
  cp <- profil_mois(sel_annees(don, ctx$courante), "p_tot", "somme", v)
  ce <- profil_mois(sel_annees(don, ctx$courante), "etp",   "somme", v)

  data.frame(
    mois      = 1:12,
    mois_lib  = factor(MOIS_COURT, levels = MOIS_COURT),
    p_ref     = rp$v,   etp_ref = re$v, bh_ref = rp$v - re$v,
    p_cur     = cp$v,   etp_cur = ce$v, bh_cur = cp$v - ce$v,
    cur_partiel = cp$partiel,
    stringsAsFactors = FALSE
  )
}

#' 02 — Diagramme ombrothermique de Bagnouls & Gaussen.
#' Convention : precipitations (mm) sur un axe deux fois plus dilate que les
#' temperatures (degC). Un mois est dit "sec" lorsque P < 2T.
#' Le mois incomplet de l'annee comparee n'est pas classe sec ou humide : son
#' cumul de pluie est tronque, il paraitrait sec a tort.
#' @export
calc_ombrothermique <- function(don, ctx) {
  cur <- sel_annees(don, ctx$courante)
  rp <- ref_mensuelle(don, ctx, "p_tot", "somme",   "p_tot")
  rt <- ref_mensuelle(don, ctx, "t_moy", "moyenne", "t_moy")
  cp <- profil_mois(cur, "p_tot", "somme",   "p_tot")
  ct <- profil_mois(cur, "t_moy", "moyenne", "t_moy")
  partiel <- cp$partiel | ct$partiel

  sec_cur <- cp$v < 2 * ct$v
  sec_cur[partiel] <- NA

  data.frame(
    mois     = 1:12,
    mois_lib = factor(MOIS_COURT, levels = MOIS_COURT),
    p_ref = rp$v, t_ref = rt$v,
    p_cur = cp$v, t_cur = ct$v,
    sec_ref = rp$v < 2 * rt$v,
    sec_cur = sec_cur,
    cur_partiel = partiel,
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 3. INDICATEURS ANNUELS
# ------------------------------------------------------------------------------
# Tous passent par enrichir_annuel() : normale calculee sur les annees de
# reference COMPLETES, anomalie vide pour une annee partielle, jours manquants
# comptes. `vars` nomme les colonnes dont dependent le comptage ou le cumul, pour
# que « jour manquant » s'entende de la bonne variable (un jour sans temperature
# n'est pas manquant pour la pluie).

#' 03 — Evolution de la temperature moyenne annuelle.
#' @export
calc_temperature_annuelle <- function(don, ctx) {
  # Aucune ligne : pas d'indicateur (et surtout pas d'erreur dans aggregate()).
  if (is.null(don) || !nrow(don)) return(data.frame())
  res <- agreger_annuel(don, "t_moy", "moyenne", nom = "t_moy")
  res <- enrichir_annuel(res, don, ctx, "t_moy", vars = "t_moy")
  res$lissee <- lissage_annuel(res$annee, res$t_moy, exclure = res$partielle)
  res
}

#' 04 — Jours de gel : trois seuils (0, -5 et -10 degC par defaut) et date du dernier gel.
#' Un jour de gel est un jour dont la temperature minimale, sous abri, passe SOUS le
#' seuil (inegalite stricte, comme l'indice FD de Zhang et al. 2011). Le gel TARDIF
#' (apres le debourrement) a son propre indicateur : calc_gel_tardif().
#' @export
calc_gel <- function(don, ctx) {
  # Aucune ligne : pas d'indicateur (et surtout pas d'erreur dans aggregate()).
  if (is.null(don) || !nrow(don)) return(data.frame())
  s <- ctx$seuils
  don$gel         <- don$t_min < s$gel_t
  don$gel_severe  <- don$t_min < s$gel_severe_t
  don$gel_extreme <- don$t_min < s$gel_extreme_t

  cpt <- function(col, nom) {
    a <- stats::aggregate(list(n = don[[col]]), by = list(annee = don$annee),
                          FUN = function(x) sum(x, na.rm = TRUE))
    a$annee <- as.integer(a$annee)
    names(a)[2] <- nom
    a
  }
  res <- Reduce(function(x, y) merge(x, y, by = "annee"),
                list(cpt("gel", "n_gel"), cpt("gel_severe", "n_gel_severe"),
                     cpt("gel_extreme", "n_gel_extreme")))

  # Date du dernier gel de printemps (jour julien), indicateur de risque direct
  gels_print <- don[don$gel & don$doy <= CONFIG$methode$gel_printemps_doy_max &
                      !is.na(don$gel), , drop = FALSE]
  if (nrow(gels_print)) {
    dg <- stats::aggregate(list(doy_dernier_gel = gels_print$doy),
                           by = list(annee = gels_print$annee), FUN = max)
    dg$annee <- as.integer(dg$annee)
    res <- merge(res, dg, by = "annee", all.x = TRUE)
  } else {
    res$doy_dernier_gel <- NA_real_
  }

  enrichir_annuel(res, don, ctx, "n_gel", vars = "t_min")
}

#' Jour de l'annee du debourrement ESTIME, par annee (modele de forcage thermique).
#'
#' Le debourrement n'est pas mesure : il est estime comme le premier jour ou la somme
#' des degres-jours max(0 ; min(T moy ; plafond) - base), cumulee depuis le 1er janvier,
#' atteint `s$gel_tardif_gdd`. C'est l'approche classique des modeles de phenologie
#' printaniere (somme de temperatures au-dessus d'un seuil, a partir d'une date fixe :
#' Delpierre et al. 2016), reduite a son plus simple. Un jour sans temperature moyenne
#' n'ajoute rien a la somme : le debourrement est alors estime plus tard qu'il ne l'est.
#'
#' @param don tableau journalier (annee, doy, t_moy) ; @param s seuils.
#' @return data.frame(annee, doy_debourrement) ; NA si la somme n'est pas atteinte.
#' @keywords internal
debourrement_estime <- function(don, s) {
  vide <- data.frame(annee = integer(0), doy_debourrement = numeric(0))
  if (is.null(don) || !nrow(don)) return(vide)
  d <- don[order(don$annee, don$doy), c("annee", "doy", "t_moy"), drop = FALSE]
  gdd_j <- pmax(0, pmin(d$t_moy, s$gdd_plafond) - s$gdd_base)
  gdd_j[is.na(gdd_j)] <- 0
  cumul <- stats::ave(gdd_j, d$annee, FUN = cumsum)
  doy <- ifelse(cumul >= s$gel_tardif_gdd, d$doy, NA_real_)
  deb <- tapply(doy, d$annee, function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE))
  data.frame(annee = as.integer(names(deb)), doy_debourrement = as.numeric(deb))
}

#' 04 bis — Gel tardif : jours de gel survenant apres le debourrement estime.
#'
#' DEFINITION. Un jour de gel tardif est un jour qui verifie les trois conditions :
#'  1. sa temperature minimale, sous abri, est inferieure a `gel_tardif_t` ;
#'  2. il est POSTERIEUR au debourrement estime de l'annee (debourrement_estime()) ;
#'  3. il est au plus le jour `gel_tardif_fin` de l'annee (30 juin).
#' Avant le debourrement, un gel ne touche pas de tissus fragiles ; apres, les jeunes
#' feuilles et pousses peuvent geler. Une annee dont le debourrement estime n'est pas
#' atteint avant la fin de la saison compte zero jour de gel tardif.
#' @export
calc_gel_tardif <- function(don, ctx) {
  # Aucune ligne : pas d'indicateur (et surtout pas d'erreur dans aggregate()).
  if (is.null(don) || !nrow(don)) return(data.frame())
  s <- ctx$seuils
  deb <- debourrement_estime(don, s)
  res <- stats::aggregate(list(n_gel_tardif = gel_tardif_jours(don, s, deb)),
                          by = list(annee = don$annee),
                          FUN = function(x) sum(x, na.rm = TRUE))
  res$annee <- as.integer(res$annee)
  res <- merge(res, deb, by = "annee", all.x = TRUE)
  enrichir_annuel(res, don, ctx, "n_gel_tardif", vars = c("t_min", "t_moy"))
}

#' Quels jours sont des jours de gel tardif ? (un booleen par ligne de `don`)
#' La definition vit ICI, pour que l'indicateur annuel (calc_gel_tardif) et la synthese
#' a periode egale (calc_synthese) comptent la meme chose.
#' @param don tableau journalier (annee, doy, t_min, t_moy) ; @param s seuils ;
#'   @param deb debourrement_estime(don, s), si deja calcule.
#' @return logical, NA quand la temperature minimale manque un jour candidat.
#' @keywords internal
gel_tardif_jours <- function(don, s, deb = debourrement_estime(don, s)) {
  d_deb <- deb$doy_debourrement[match(don$annee, deb$annee)]
  !is.na(d_deb) & don$doy > d_deb & don$doy <= s$gel_tardif_fin & don$t_min < s$gel_tardif_t
}

#' 05 — Jours de chaleur (quatre seuils cumulatifs sur la temperature maximale).
#' Un jour est compte quand sa T max est STRICTEMENT superieure au seuil (25, 30, 35 et
#' 40 degC par defaut) ; 25 degC est le « jour d'ete » de l'indice SU (Zhang et al. 2011).
#' @export
calc_chaleur <- function(don, ctx) {
  # Aucune ligne : pas d'indicateur (et surtout pas d'erreur dans aggregate()).
  if (is.null(don) || !nrow(don)) return(data.frame())
  s <- ctx$seuils
  cpt <- function(seuil, nom) {
    a <- stats::aggregate(list(n = don$t_max > seuil), by = list(annee = don$annee),
                          FUN = function(x) sum(x, na.rm = TRUE))
    a$annee <- as.integer(a$annee)
    names(a)[2] <- nom
    a
  }
  res <- Reduce(function(x, y) merge(x, y, by = "annee"),
                list(cpt(s$chaleur_t1, "n_t1"),
                     cpt(s$chaleur_t2, "n_t2"),
                     cpt(s$chaleur_t3, "n_t3"),
                     cpt(s$chaleur_t4, "n_t4")))
  enrichir_annuel(res, don, ctx, "n_t2", vars = "t_max")
}

#' 05 ter — Premier jour de l'annee a plus de 25 degC, et a plus de 30 degC.
#' Date (jour de l'annee) du PREMIER jour dont la T max depasse chaque seuil
#' (`chaleur_t1` et `chaleur_t2`, 25 et 30 degC par defaut). NA si le seuil n'est pas
#' atteint dans l'annee : pour l'annee en cours, cela veut dire « pas encore ». Un jour
#' sans mesure ne peut pas etre le premier jour : sur une annee a lacunes, la date est
#' une borne TARDIVE (n_manquants dit combien de jours manquent).
#' @export
calc_premiers_jours_chauds <- function(don, ctx) {
  # Aucune ligne : pas d'indicateur (et surtout pas d'erreur dans aggregate()).
  if (is.null(don) || !nrow(don)) return(data.frame())
  s <- ctx$seuils
  premier <- function(seuil, nom) {
    d <- don[!is.na(don$t_max) & don$t_max > seuil, c("annee", "doy"), drop = FALSE]
    if (!nrow(d)) {
      a <- data.frame(annee = integer(0), v = numeric(0))
    } else {
      a <- stats::aggregate(list(v = d$doy), by = list(annee = d$annee), FUN = min)
      a$annee <- as.integer(a$annee)
    }
    names(a)[2] <- nom
    a
  }
  res <- data.frame(annee = sort(unique(as.integer(don$annee))))
  res <- merge(res, premier(s$chaleur_t1, "doy_t1"), by = "annee", all.x = TRUE)
  res <- merge(res, premier(s$chaleur_t2, "doy_t2"), by = "annee", all.x = TRUE)
  enrichir_annuel(res, don, ctx, "doy_t1", vars = "t_max")
}

#' Jour de l'annee -> « 12 avril » (annee non bissextile : le jour 60 est le 1er mars,
#' comme sur les axes des graphiques ; le jour 366 est ramene au 31 decembre).
#' @param doy jour julien, vecteur. @return chaines, NA pour une valeur non finie.
#' @export
doy_date_fr <- function(doy) {
  d <- as.integer(round(pmin(doy, 365)))
  ok <- is.finite(doy) & !is.na(d) & d >= 1L
  out <- rep(NA_character_, length(doy))
  if (any(ok)) {
    dt <- as.Date(d[ok] - 1L, origin = "2001-01-01")
    out[ok] <- sprintf("%d %s", as.integer(format(dt, "%d")), MOIS_LONG[as.integer(format(dt, "%m"))])
  }
  out
}

#' Les 366 jours du calendrier, « MM-JJ », 29 fevrier compris.
#' Une annee bissextile sert de modele : la normale d'un jour se lit par sa date
#' civile, pas par son rang dans l'annee (le rang decale d'un jour apres fevrier).
#' @keywords internal
CLES_JOUR <- format(seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "day"), "%m-%d")

#' Normale de la temperature maximale pour chaque jour du calendrier.
#'
#' Moyenne, sur les annees de reference, de la T max de ce jour, lissee sur une
#' fenetre centree de cinq jours (Frich et al. 2002). Sans lissage, trente valeurs
#' par jour donneraient une courbe en dents de scie, et les ecarts a cette courbe
#' autant de « jours chauds » qui n'en sont pas.
#' @param don tableau journalier (date, annee, t_max).
#' @return vecteur de 366 valeurs nomme par CLES_JOUR, ou NULL si la reference
#'   compte moins d'annees renseignees que CONFIG$methode$vague_ref_annees_min.
#' @keywords internal
vague_normale_jour <- function(don, ctx) {
  # Mode « base precalculee » (utils_base.R) : la normale vient du jeu publie, car la
  # reference journaliere n'est pas chargee. NA partout = reference trop courte.
  if (!is.null(ctx$base$normale_jour_tmax)) {
    v <- ctx$base$normale_jour_tmax
    return(if (all(is.na(v))) NULL else stats::setNames(as.numeric(v), CLES_JOUR))
  }
  ref <- don[don$annee %in% ctx$ref & is.finite(don$t_max), c("date", "annee", "t_max"),
             drop = FALSE]
  if (!nrow(ref)) return(NULL)
  n_an <- tabulate(match(ref$annee, sort(unique(ref$annee))))
  if (sum(n_an >= 300L) < CONFIG$methode$vague_ref_annees_min) return(NULL)

  moy <- as.numeric(tapply(ref$t_max, factor(format(ref$date, "%m-%d"), levels = CLES_JOUR),
                           mean))
  n <- length(moy)
  demi <- (CONFIG$methode$vague_fenetre_normale - 1L) %/% 2L
  # Fenetre circulaire : le 31 decembre voisine du 1er janvier. Un jour sans
  # valeur (29 fevrier, hors annee bissextile de la reference) est lisse par ses
  # voisins.
  voisins <- vapply(seq.int(-demi, demi), function(k) moy[((seq_len(n) - 1L + k) %% n) + 1L],
                    numeric(n))
  lisse <- rowMeans(voisins, na.rm = TRUE)
  lisse[is.nan(lisse)] <- NA_real_
  stats::setNames(lisse, CLES_JOUR)
}

#' Jours de vague de chaleur : ecart a la normale du jour et appartenance a une suite.
#'
#' DEFINITION (indice HWDI, Frich et al. 2002, tel que le reprend l'application) :
#' un jour est « chaud » si sa T max depasse d'au moins `vague_ecart` degres la
#' normale du jour ; une VAGUE est une suite d'au moins `vague_duree` jours chauds
#' consecutifs. Deux choix propres a l'application :
#'  - la saison : seuls les mois de CONFIG$methode$vague_mois comptent (mai a
#'    septembre). En hiver, +5 degC pendant cinq jours est un redoux ;
#'  - le DENOMBREMENT : l'indice d'origine rend la duree de la plus longue vague ;
#'    ici on compte les vagues, et on garde la duree maximale a cote.
#' Une suite est rompue par un jour froid, un jour SANS MESURE ou un trou dans
#' les dates : un jour inconnu n'est pas compte comme chaud.
#'
#' @param don tableau journalier (date, annee, t_max).
#' @return data.frame(date, annee, ecart, chaud, vague, debut, id) ou id numerote
#'   chaque vague (0 hors vague) ; NULL si la normale du jour n'est pas calculable.
#' @keywords internal
vagues_detecter <- function(don, ctx) {
  if (is.null(don) || !nrow(don) || is.null(don$t_max)) return(NULL)
  don <- don[order(don$date), c("date", "annee", "t_max"), drop = FALSE]
  normale <- vague_normale_jour(don, ctx)
  if (is.null(normale)) return(NULL)

  # Un contexte construit a la main peut ne pas porter ces deux reglages.
  ecart_min <- ctx$seuils$vague_ecart %||% CONFIG$seuils$vague_ecart
  duree_min <- ctx$seuils$vague_duree %||% CONFIG$seuils$vague_duree
  mois <- as.integer(format(don$date, "%m"))
  ecart <- don$t_max - normale[match(format(don$date, "%m-%d"), CLES_JOUR)]
  m <- CONFIG$methode$vague_mois
  chaud <- is.finite(ecart) & ecart >= ecart_min & mois >= m[1] & mois <= m[2]

  # Debut d'une suite de jours chauds : jour chaud dont la veille ne l'est pas, ou
  # n'est pas la veille calendaire.
  veille_chaude <- c(FALSE, chaud[-length(chaud)]) & c(FALSE, diff(as.integer(don$date)) == 1L)
  debut_suite <- chaud & !veille_chaude
  suite <- cumsum(debut_suite)
  longueur <- tabulate(suite[chaud], nbins = max(suite, 0L))
  vague <- chaud & (longueur[pmax(suite, 1L)] >= duree_min) %in% TRUE

  id <- integer(nrow(don))
  id[vague] <- match(suite[vague], unique(suite[vague]))
  data.frame(date = don$date, annee = as.integer(don$annee), ecart = ecart,
             chaud = chaud, vague = vague, debut = vague & debut_suite, id = id)
}

#' Les vagues une a une : debut, fin, duree, ecart moyen et maximal a la normale.
#' @param det resultat de vagues_detecter().
#' @return data.frame(annee, debut, fin, duree, ecart_moy, ecart_max), une ligne par
#'   vague, dans l'ordre du temps ; zero ligne s'il n'y en a pas. Les vagues ne
#'   franchissent jamais une fin d'annee : la saison s'arrete en septembre.
#' @keywords internal
vagues_episodes <- function(det) {
  v <- det[det$vague, , drop = FALSE]
  if (!nrow(v)) {
    return(data.frame(annee = integer(0), debut = as.Date(character(0)),
                      fin = as.Date(character(0)), duree = integer(0),
                      ecart_moy = numeric(0), ecart_max = numeric(0)))
  }
  i1 <- !duplicated(v$id)
  i2 <- !duplicated(v$id, fromLast = TRUE)
  rang <- match(v$id, v$id[i1])
  data.frame(annee = v$annee[i1], debut = v$date[i1], fin = v$date[i2],
             duree = tabulate(rang),
             ecart_moy = as.numeric(rowsum(v$ecart, rang)) / tabulate(rang),
             ecart_max = as.numeric(tapply(v$ecart, rang, max)),
             stringsAsFactors = FALSE)
}

#' 05 bis — Vagues de chaleur : nombre, jours cumules, duree et intensite.
#' @export
calc_vagues <- function(don, ctx) {
  if (is.null(don) || !nrow(don)) return(data.frame())
  det <- vagues_detecter(don, ctx)
  if (is.null(det)) {
    r <- data.frame()
    attr(r, "raison") <- "reference_insuffisante"
    attr(r, "message") <- sprintf(paste(
      "Normale du jour non calculable : la référence %s compte moins de %d années",
      "renseignées dans les données chargées."),
      ctx$ref_label, CONFIG$methode$vague_ref_annees_min)
    return(r)
  }
  ep <- vagues_episodes(det)
  annees <- sort(unique(as.integer(don$annee)))
  f <- factor(ep$annee, levels = annees)
  zero <- function(x) { x[is.na(x)] <- 0; x }

  res <- data.frame(
    annee         = annees,
    n_vagues      = as.integer(tabulate(f, nbins = length(annees))),
    n_jours_vague = as.integer(zero(as.numeric(tapply(ep$duree, f, sum)))),
    duree_max     = as.integer(zero(as.numeric(tapply(ep$duree, f, max)))),
    # Sans vague, pas d'ecart : NA et non 0, qui serait une intensite.
    ecart_max     = as.numeric(tapply(ep$ecart_max, f, max)))
  res <- enrichir_annuel(res, don, ctx, "n_vagues", vars = "t_max")
  res$lissee <- lissage_annuel(res$annee, res$n_vagues, exclure = res$partielle)
  res
}

#' 06 — Precipitations annuelles, decomposees liquide / solide.
#' RAPPEL : le total est la somme des deux phases (cf. utils_api.R).
#' @export
calc_precipitations <- function(don, ctx) {
  # Aucune ligne : pas d'indicateur (et surtout pas d'erreur dans aggregate()).
  if (is.null(don) || !nrow(don)) return(data.frame())
  # Une STATION ne separe pas la pluie de la neige : p_liq et p_sol sont vides.
  # Les sommer donnerait « 0 mm de neige » et « 0 mm de pluie » alors que le
  # total est renseigne. Dans ce cas on rend le total seul, la phase solide
  # reste NA et la part de neige n'est pas definie.
  phases <- !colonne_vide(don, "p_liq") && !colonne_vide(don, "p_sol")
  if (phases) {
    res <- stats::aggregate(
      cbind(p_liq = don$p_liq, p_sol = don$p_sol, p_tot = don$p_tot),
      by = list(annee = don$annee), FUN = function(x) sum(x, na.rm = TRUE)
    )
  } else {
    res <- stats::aggregate(
      cbind(p_tot = don$p_tot),
      by = list(annee = don$annee), FUN = function(x) sum(x, na.rm = TRUE)
    )
    res$p_liq <- res$p_tot
    res$p_sol <- NA_real_
    res <- res[, c("annee", "p_liq", "p_sol", "p_tot")]
  }
  res$annee <- as.integer(res$annee)
  res$part_neige <- if (phases) 100 * res$p_sol / pmax(res$p_tot, 1e-9) else NA_real_
  res <- enrichir_annuel(res, don, ctx, "p_tot", vars = "p_tot")
  res$lissee <- lissage_annuel(res$annee, res$p_tot, exclure = res$partielle)
  res
}

#' 07 — Ete meteorologique (juin-juillet-aout) : temperature et precipitations.
#' La completude est celle de la SAISON : un ete n'est entier que le 31 aout.
#' Tant qu'il ne l'est pas, il n'est pas compare a la normale.
#' @export
calc_ete <- function(don, ctx) {
  jja <- don[don$mois %in% 6:8, , drop = FALSE]
  if (!nrow(jja)) return(data.frame())
  t <- agreger_annuel(jja, "t_moy", "moyenne", nom = "t_jja")
  p <- agreger_annuel(jja, "p_tot", "somme",   nom = "p_jja")
  res <- merge(t, p, by = "annee")
  enrichir_ete(res, completude(don, c(6L, 8L), c("t_moy", "p_tot")), ctx)
}

#' Completude de la saison, normales et anomalies de l'ete (temperature ET pluie).
#' Separee de calc_ete() pour que la base precalculee (utils_base.R) l'appelle telle
#' quelle sur des valeurs lues au lieu de les recalculer.
#' @param res data.frame(annee, t_jja, p_jja) ; @param comp completude() de la saison.
#' @keywords internal
enrichir_ete <- function(res, comp, ctx) {
  res <- merge(res, comp[, c("annee", "n_jours", "n_manquants", "partielle", "dernier_jour")],
               by = "annee", all.x = TRUE)
  res$partielle <- res$partielle %in% TRUE
  res$n_manquants[is.na(res$n_manquants)] <- 0L

  st <- stats_ref(res, "t_jja", ctx); sp <- stats_ref(res, "p_jja", ctx)
  res$t_normale <- st$normale; res$p_normale <- sp$normale
  res$t_ecart_type_ref <- st$sd; res$p_ecart_type_ref <- sp$sd
  res$t_anomalie <- ifelse(res$partielle, NA_real_, res$t_jja - res$t_normale)
  res$p_anomalie <- ifelse(res$partielle, NA_real_, res$p_jja - res$p_normale)
  res <- res[order(res$annee), , drop = FALSE]
  rownames(res) <- NULL
  attr(res, "ref_n") <- c(utilisees = st$n, attendues = length(ctx$ref))
  res
}

#' 08 — SWI estival (Soil Wetness Index).
#' Le SWI SAFRAN-ISBA exprime le remplissage du reservoir racinaire :
#' 0 = point de fletrissement, 1 = capacite au champ. En dessous de ~0,5 la
#' contrainte hydrique devient perceptible pour la vegetation forestiere.
#' @export
calc_swi <- function(don, ctx) {
  if (!"swi" %in% names(don) || all(is.na(don$swi))) return(data.frame())
  jja <- don[don$mois %in% 6:8, , drop = FALSE]
  if (!nrow(jja)) return(data.frame())

  res <- stats::aggregate(list(swi_moy = jja$swi), by = list(annee = jja$annee),
                          FUN = function(x) mean(x, na.rm = TRUE))
  mini <- stats::aggregate(list(swi_min = jja$swi), by = list(annee = jja$annee),
                           FUN = function(x) suppressWarnings(min(x, na.rm = TRUE)))
  stress <- stats::aggregate(list(n_stress = jja$swi < ctx$seuils$swi_stress),
                             by = list(annee = jja$annee),
                             FUN = function(x) sum(x, na.rm = TRUE))
  res <- merge(merge(res, mini, by = "annee"), stress, by = "annee")
  res$annee <- as.integer(res$annee)
  res$swi_min[!is.finite(res$swi_min)] <- NA_real_
  enrichir_annuel(res, don, ctx, "swi_moy", fenetre = c(6L, 8L), vars = "swi")
}

#' 09 — Sequences seches consecutives.
#' Duree maximale, par annee, d'une suite ininterrompue de journees dont le
#' cumul de precipitations reste sous le seuil configure (1 mm par defaut).
#' Un jour sans mesure interrompt la sequence : sur une station lacunaire la
#' duree est donc plutot sous-estimee.
#' @export
calc_secheresse <- function(don, ctx) {
  # Aucune ligne : pas d'indicateur (et surtout pas d'erreur dans aggregate()).
  if (is.null(don) || !nrow(don)) return(data.frame())
  s <- ctx$seuils
  don <- don[order(don$date), , drop = FALSE]

  par_annee <- function(sous_ens) {
    sec <- sous_ens$p_tot < s$sec_seuil_mm
    sec[is.na(sec)] <- FALSE
    r <- rle(sec)
    longueurs <- r$lengths[r$values]
    data.frame(
      duree_max    = if (length(longueurs)) max(longueurs) else 0L,
      n_sequences  = sum(longueurs >= s$sec_duree_alerte),
      n_jours_secs = sum(sec)
    )
  }

  annees <- sort(unique(don$annee))
  liste <- lapply(annees, function(a) par_annee(don[don$annee == a, , drop = FALSE]))
  res <- do.call(rbind, liste)
  res$annee <- annees
  res <- res[, c("annee", "duree_max", "n_sequences", "n_jours_secs")]
  enrichir_annuel(res, don, ctx, "duree_max", vars = "p_tot")
}

#' 10 — Indice d'aridite de De Martonne.
#'   annuel  : I = P / (T + 10)
#'   estival : I = 4 x P_JJA / (T_JJA + 10)   (transposition trimestrielle de la
#'             formule mensuelle I = 12 p / (t + 10))
#' Lecture : I < 5 hyperaride, 5-10 aride, 10-20 semi-aride, 20-30 subhumide,
#' 30-60 humide, > 60 tres humide.
#' @export
calc_martonne <- function(don, ctx) {
  # Aucune ligne : pas d'indicateur (et surtout pas d'erreur dans aggregate()).
  if (is.null(don) || !nrow(don)) return(data.frame())
  p <- agreger_annuel(don, "p_tot", "somme",   nom = "p_an")
  t <- agreger_annuel(don, "t_moy", "moyenne", nom = "t_an")
  res <- merge(p, t, by = "annee")
  res$i_annuel <- res$p_an / (res$t_an + 10)

  jja <- don[don$mois %in% 6:8, , drop = FALSE]
  if (nrow(jja)) {
    pj <- agreger_annuel(jja, "p_tot", "somme",   nom = "p_jja")
    tj <- agreger_annuel(jja, "t_moy", "moyenne", nom = "t_jja")
    e <- merge(pj, tj, by = "annee")
    e$i_estival <- 4 * e$p_jja / (e$t_jja + 10)
    # Un ete incomplet donnerait un indice estival tronque, donc trop sec.
    cj <- completude(don, c(6L, 8L), c("t_moy", "p_tot"))
    e <- merge(e, cj[, c("annee", "partielle")], by = "annee", all.x = TRUE)
    e$i_estival[e$partielle %in% TRUE] <- NA_real_
    res <- merge(res, e[, c("annee", "i_estival")], by = "annee", all.x = TRUE)
  } else {
    res$i_estival <- NA_real_
  }

  res$classe <- cut(
    res$i_annuel,
    breaks = c(-Inf, 5, 10, 20, 30, 60, Inf),
    labels = c("Hyperaride", "Aride", "Semi-aride", "Subhumide", "Humide", "Tres humide")
  )
  res <- enrichir_annuel(res, don, ctx, "i_annuel", vars = c("p_tot", "t_moy"))
  # La classe d'une annee partielle (cumul tronque) n'a pas de sens.
  res$classe[res$partielle] <- NA
  res
}

#' 11 — Degres-jours de croissance (Growing Degree Days).
#' Cumul annuel de max(0, min(T_moy, plafond) - base). Base 5 degC : seuil
#' classiquement retenu pour les essences forestieres temperees.
#' @export
calc_gdd <- function(don, ctx) {
  # Aucune ligne : pas d'indicateur (et surtout pas d'erreur dans aggregate()).
  if (is.null(don) || !nrow(don)) return(data.frame())
  s <- ctx$seuils
  don$gdd_j <- pmax(0, pmin(don$t_moy, s$gdd_plafond) - s$gdd_base)
  res <- agreger_annuel(don, "gdd_j", "somme", nom = "gdd")
  res <- enrichir_annuel(res, don, ctx, "gdd", vars = "t_moy")
  res$lissee <- lissage_annuel(res$annee, res$gdd, exclure = res$partielle)
  res
}

#' 12 — Saison de vegetation.
#' Debut  : premier jour d'une serie de N jours consecutifs a T_moy > seuil,
#'          recherchee a partir du 1er fevrier.
#' Fin    : premier jour d'une serie de N jours consecutifs a T_moy < seuil,
#'          recherchee a partir du 1er aout.
#' Duree  : nombre de jours separant les deux dates.
#'
#' Une annee en cours n'est « partielle » ici que tant que la fin de saison n'est
#' pas observee : debut et fin sont des PREMIERS passages, definitifs des qu'ils
#' sont constates. Une annee arretee fin octobre, dont la fin de saison est deja
#' passee, est donc exploitable.
#' @export
calc_saison_vegetation <- function(don, ctx) {
  # Aucune ligne : pas d'indicateur (et surtout pas d'erreur dans aggregate()).
  if (is.null(don) || !nrow(don)) return(data.frame())
  s <- ctx$seuils
  n <- s$veg_n_jours

  # Recherche du premier indice, a partir de `depuis`, ou n valeurs consecutives
  # verifient `cond`. Renvoie NA si aucun bloc suffisamment long n'existe.
  premier_bloc <- function(cond, depuis) {
    if (is.na(depuis) || length(cond) < depuis + n - 1L) return(NA_integer_)
    cond[is.na(cond)] <- FALSE
    r      <- rle(cond)
    fins   <- cumsum(r$lengths)
    debuts <- fins - r$lengths + 1L
    # Longueur utile = partie du bloc situee a partir de `depuis`
    utile  <- fins - pmax(debuts, depuis) + 1L
    cands  <- which(r$values & utile >= n)
    if (!length(cands)) return(NA_integer_)
    max(debuts[cands[1]], depuis)
  }

  annees <- sort(unique(don$annee))
  lignes <- lapply(annees, function(a) {
    d <- don[don$annee == a, , drop = FALSE]
    d <- d[order(d$doy), , drop = FALSE]
    if (nrow(d) < 60) {
      return(data.frame(annee = a, doy_debut = NA_real_, doy_fin = NA_real_, duree = NA_real_))
    }
    i_deb <- premier_bloc(d$t_moy > s$veg_seuil_t, which(d$doy >= s$veg_doy_min)[1])
    i_fin <- premier_bloc(d$t_moy < s$veg_seuil_t, which(d$doy >= s$veg_doy_split)[1])
    doy_deb <- if (is.na(i_deb)) NA_real_ else d$doy[i_deb]
    doy_fin <- if (is.na(i_fin)) NA_real_ else d$doy[i_fin]
    data.frame(annee = a, doy_debut = doy_deb, doy_fin = doy_fin,
               duree = doy_fin - doy_deb)
  })
  res <- do.call(rbind, lignes)

  comp <- completude(don, vars = "t_moy")
  comp <- merge(comp, res[, c("annee", "duree")], by = "annee", all.x = TRUE)
  comp$partielle <- comp$partielle & !is.finite(comp$duree)
  enrichir_annuel(res, don, ctx, "duree", comp = comp[, setdiff(names(comp), "duree")])
}

#' 13 — P - ETP sur fenetre mensuelle configurable.
#' Indicateur cle du deficit hydrique climatique. La fenetre est choisie par
#' l'utilisateur (par ex. juin-aout pour le coeur d'ete, avril-octobre pour la
#' saison de vegetation).
#' La completude est celle de la fenetre : une saison est partielle tant que sa
#' derniere journee n'est pas atteinte (et non plus par comparaison a la mediane
#' des autres saisons, qui laissait passer une saison amputee de 4 %).
#' @export
calc_petp <- function(don, ctx) {
  f <- sel_fenetre(don, ctx$fenetre)
  if (!nrow(f)) return(data.frame())

  res <- stats::aggregate(
    cbind(p = f$p_tot, etp = f$etp),
    by = list(annee = f$annee_saison), FUN = function(x) sum(x, na.rm = TRUE)
  )
  res$annee <- as.integer(res$annee)
  res$petp  <- res$p - res$etp

  res <- enrichir_annuel(res, don, ctx, "petp", fenetre = ctx$fenetre,
                         vars = c("p_tot", "etp"))
  res$lissee <- lissage_annuel(res$annee, res$petp, exclure = res$partielle)
  res
}

#' 14 — Precipitations neigeuses.
#' Cumul annuel de la phase solide (PRENEI_Q), part dans le total et nombre de
#' jours de chute de neige. Indicateur de risque "neige lourde" et de
#' disponibilite en eau de fonte au printemps.
#' @export
calc_neige <- function(don, ctx) {
  # Aucune ligne : pas d'indicateur (et surtout pas d'erreur dans aggregate()).
  if (is.null(don) || !nrow(don)) return(data.frame())
  res <- stats::aggregate(
    cbind(p_sol = don$p_sol, p_tot = don$p_tot),
    by = list(annee = don$annee), FUN = function(x) sum(x, na.rm = TRUE)
  )
  res$annee <- as.integer(res$annee)
  res$part_neige <- 100 * res$p_sol / pmax(res$p_tot, 1e-9)

  nj <- stats::aggregate(list(n_jours_neige = don$p_sol > 0.5),
                         by = list(annee = don$annee),
                         FUN = function(x) sum(x, na.rm = TRUE))
  nj$annee <- as.integer(nj$annee)
  res <- merge(res, nj, by = "annee", all.x = TRUE)

  # Cumul maximal sur 3 jours glissants : proxy de l'alea "neige lourde"
  don <- don[order(don$date), , drop = FALSE]
  roul3 <- as.numeric(stats::filter(ifelse(is.na(don$p_sol), 0, don$p_sol),
                                    rep(1, 3), sides = 1))
  mx <- stats::aggregate(list(neige_3j_max = roul3), by = list(annee = don$annee),
                         FUN = function(x) suppressWarnings(max(x, na.rm = TRUE)))
  mx$annee <- as.integer(mx$annee)
  mx$neige_3j_max[!is.finite(mx$neige_3j_max)] <- NA_real_
  res <- merge(res, mx, by = "annee", all.x = TRUE)

  enrichir_annuel(res, don, ctx, "p_sol", vars = "p_sol")
}


# ------------------------------------------------------------------------------
# 4. Synthese chiffree (bandeau d'indicateurs cles + rapport)
# ------------------------------------------------------------------------------

#' Produit la synthese comparant l'annee mise en avant a la periode de reference.
#'
#' La comparaison est faite A PERIODE EGALE : si l'annee comparee s'arrete le
#' 27 octobre, la normale est calculee sur les 27 octobre de chaque annee de
#' reference. C'est ici, et non dans les graphiques annuels, que l'annee en cours
#' est comparee a la normale.
#'
#' @return data.frame (indicateur, unite, valeur, normale, ecart, sd_ref, n_ref,
#'   classe, sens, format). `classe` vaut "haut", "bas" ou "neutre" selon que
#'   l'ecart depasse ou non la moitie de l'ecart-type de la reference : un ecart
#'   plus faible n'est pas colore. `sens` dit quel cote est defavorable ("chaud",
#'   "sec") ou "neutre" quand aucun cote ne l'est (le gel : moins de jours de gel
#'   peut aussi vouloir dire un debourrement plus precoce, donc plus exposé au
#'   gel tardif). Attributs : doy_max, derniere_date, manquants.
#' @export
calc_synthese <- function(don, ctx) {

  # `famille` : la famille du registre (FAMILLES) ou se trouvent les graphiques
  # qui detaillent cette ligne. Elle sert au bloc « A retenir » du bilan, pour
  # renvoyer vers la bonne section (audit, S1).
  ligne <- function(lib, unite, val, st, sens = "neutre", format = "%.1f",
                    famille = NA_character_) {
    data.frame(indicateur = lib, unite = unite, valeur = val, normale = st[["moy"]],
               ecart = val - st[["moy"]], sd_ref = st[["sd"]], n_ref = st[["n"]],
               sens = sens, format = format, famille = famille,
               stringsAsFactors = FALSE)
  }

  cur <- sel_annees(don, ctx$courante)
  ref <- sel_annees(don, ctx$ref)
  if (!nrow(cur) || !nrow(ref)) return(data.frame())

  # Comparaison a periode egale : on tronque la reference au dernier jour
  # disponible de l'annee en cours, sinon on comparerait 8 mois a 12 mois.
  doy_max <- max(cur$doy, na.rm = TRUE)
  ref_tronq <- ref[ref$doy <= doy_max, , drop = FALSE]

  # -- Sources lacunaires (stations) -----------------------------------------
  # `annees_valides` liste, par groupe de variables, les annees assez bien
  # renseignees. La reference n'utilise que celles-ci, et l'annee comparee est
  # laissee vide si elle n'en fait pas partie. SAFRAN n'a pas cet attribut :
  # rien ne change pour lui.
  valides <- attr(don, "annees_valides")
  an_ok <- function(groupes, annees) {
    if (is.null(valides)) return(rep(TRUE, length(annees)))
    Reduce(`&`, lapply(groupes, function(g) annees %in% valides[[g]]))
  }
  ref_de <- function(...) ref_tronq[an_ok(c(...), ref_tronq$annee), , drop = FALSE]
  cur_ok <- function(...) isTRUE(an_ok(c(...), ctx$courante))
  ou_na  <- function(ok, x) if (ok) x else NA_real_

  # Moyenne, ecart-type et effectif des valeurs annuelles de la reference.
  stat_an <- function(d, col, fun) {
    nul <- c(moy = NA_real_, sd = NA_real_, n = 0)
    if (!nrow(d)) return(nul)
    v <- stats::aggregate(list(v = d[[col]]), by = list(annee = d$annee), FUN = fun)$v
    v <- v[is.finite(v)]
    if (!length(v)) return(nul)
    c(moy = mean(v), sd = if (length(v) >= 3L) stats::sd(v) else NA_real_, n = length(v))
  }
  som <- function(x) sum(x, na.rm = TRUE)
  moy <- function(x) mean(x, na.rm = TRUE)

  ref_t  <- ref_de("t");       ref_p <- ref_de("p");  ref_pt <- ref_de("p", "t")

  st_petp <- {
    fr <- sel_fenetre(ref_pt, ctx$fenetre)
    if (!nrow(fr)) c(moy = NA_real_, sd = NA_real_, n = 0) else {
      a <- stats::aggregate(cbind(p = fr$p_tot, e = fr$etp),
                            by = list(annee = fr$annee_saison), FUN = som)
      v <- (a$p - a$e)[is.finite(a$p - a$e)]
      c(moy = if (length(v)) mean(v) else NA_real_,
        sd = if (length(v) >= 3L) stats::sd(v) else NA_real_, n = length(v))
    }
  }

  # Vagues de chaleur : detectees sur toute la serie (la normale du jour vient de la
  # reference entiere), puis comptees a periode egale comme les autres lignes. Une
  # normale non calculable (reference trop courte) retire la ligne, sans erreur.
  vag <- tryCatch(vagues_detecter(don, ctx), error = function(e) NULL)
  st_vag <- c(moy = NA_real_, sd = NA_real_, n = 0)
  if (!is.null(vag)) {
    vag$doy <- as.integer(format(vag$date, "%j"))
    ans <- sort(intersect(unique(ref_t$annee), vag$annee))
    v <- vapply(ans, function(a) sum(vag$debut[vag$annee == a & vag$doy <= doy_max]),
                numeric(1))
    if (length(v)) st_vag <- c(moy = mean(v), sd = if (length(v) >= 3L) stats::sd(v) else NA_real_,
                               n = length(v))
  }

  # Une ligne par seuil de CONFIG$seuils nomme « <prefixe>_t1 », « _t2 »... (chaleur) ou
  # « gel_t », « gel_severe_t », « gel_extreme_t » (gel) : meme comptage que calc_chaleur()
  # et calc_gel(), a periode egale. `sens` : « chaud » colore l'ecart, « neutre » non.
  lignes_seuils <- function(type, modele, col, op, sens, famille) {
    noms <- if (type == "chaleur") paste0("chaleur_t", 1:4) else c("gel_t", "gel_severe_t", "gel_extreme_t")
    do.call(rbind, lapply(noms, function(nm) {
      seuil <- ctx$seuils[[nm]]
      test <- if (op == ">") function(x) x > seuil else function(x) x < seuil
      ligne(sprintf(modele, seuil), "j",
            ou_na(cur_ok("t"), som(test(cur[[col]]))),
            stat_an(ref_t, col, function(x) sum(test(x), na.rm = TRUE)),
            sens, "%.0f", famille = famille)
    }))
  }

  res <- rbind(
    ligne("Température moyenne", "°C",
          ou_na(cur_ok("t"), moy(cur$t_moy)), stat_an(ref_t, "t_moy", moy), "chaud",
          famille = "thermique"),
    ligne(if (colonne_vide(don, "p_sol")) "Précipitations totales"
          else "Précipitations totales (pluie + neige)", "mm",
          ou_na(cur_ok("p"), som(cur$p_tot)), stat_an(ref_p, "p_tot", som), "sec", "%.0f",
          famille = "hydrique"),
    if (!colonne_vide(don, "p_sol"))
      ligne("dont précipitations neigeuses", "mm",
            ou_na(cur_ok("p"), som(cur$p_sol)), stat_an(ref_p, "p_sol", som), "neutre", "%.0f",
            famille = "neige"),
    ligne(sprintf("P - ETP (%s)", libelle_fenetre(ctx$fenetre)), "mm",
          ou_na(cur_ok("p", "t"), {
            fc <- sel_fenetre(cur, ctx$fenetre); som(fc$p_tot) - som(fc$etp)
          }), st_petp, "sec", "%.0f", famille = "hydrique"),
    # Quatre seuils de T max (25, 30, 35 et 40 degC par defaut), comptes comme dans calc_chaleur().
    lignes_seuils("chaleur", "Jours à T max > %g °C", "t_max", ">", "chaud", "thermique"),
    if (!is.null(vag) && st_vag[["n"]] > 0)
      ligne("Vagues de chaleur", "vagues",
            ou_na(cur_ok("t"), sum(vag$debut[vag$annee == ctx$courante])),
            st_vag, "chaud", "%.0f", famille = "thermique"),
    # Le gel n'a pas de bon sens : moins de gel, c'est un hiver plus doux, donc un
    # debourrement plus precoce et, souvent, davantage de gel tardif.
    # Trois seuils de T min (0, -5 et -10 degC par defaut), comptes comme dans calc_gel().
    lignes_seuils("gel", "Jours de gel (T min < %g °C)", "t_min", "<", "neutre", "gel"),
    ligne("Jours de gel tardif", "j",
          ou_na(cur_ok("t"), som(gel_tardif_jours(cur, ctx$seuils))),
          stat_an(transform(ref_t, gt = gel_tardif_jours(ref_t, ctx$seuils)), "gt", som),
          "neutre", "%.0f", famille = "gel")
  )
  # Lignes dont ni la valeur ni la normale ne sont evaluables (variable absente
  # de la source) : inutiles, et un « 0 » y serait faux.
  res <- res[!(is.na(res$valeur) & is.na(res$normale)), , drop = FALSE]
  if (!nrow(res)) return(data.frame())

  if ("swi" %in% names(cur) && !all(is.na(cur$swi))) {
    res <- rbind(res, ligne("SWI moyen", "-", moy(cur$swi),
                            stat_an(ref_tronq, "swi", moy), "sec", "%.2f",
                            famille = "hydrique"))
  }

  res$classe <- classe_ecart(res$ecart, res$sd_ref)
  rownames(res) <- NULL

  attr(res, "doy_max") <- doy_max
  attr(res, "derniere_date") <- max(cur$date, na.rm = TRUE)
  # Jours sans mesure de l'annee comparee : les cumuls et comptages ci-dessus les
  # ignorent (ils sont donc sous-estimes), la valeur ne les compense pas.
  attr(res, "manquants") <- c(
    temperature = sum(is.na(cur$t_min) | is.na(cur$t_max)),
    pluie       = sum(is.na(cur$p_tot)))
  res
}


# ------------------------------------------------------------------------------
# Mode rapide (v2.2) : les trois petits predicats que le calcul et l'interface partagent.
# Ils vivent ICI et non dans utils_base.R : diagnostic_stations.R ne charge que ce fichier-ci
# et ne doit pas dependre de la lecture de la base (voir tests/test_v171.R).
# ------------------------------------------------------------------------------

#' Les donnees chargees portent-elles la base (mode rapide) ?
#' @keywords internal
base_active <- function(don) !is.null(attr(don, "base"))

#' Les annees que SAFRAN a livrees pour ce point (celles que le jeu ne porte pas, et
#' l'annee comparee).
#' @keywords internal
base_annees_vives <- function(don) sort(unique(as.integer(don$annee)))

#' « série 1959-2026 » en mode complet ; « base 1959-2025 + 2026 » en mode rapide.
#' @keywords internal
serie_libelle <- function(don) {
  rap <- attr(don, "base")
  if (is.null(rap)) return(sprintf("série %d-%d", min(don$annee), max(don$annee)))
  sprintf("base précalculée %d-%d + %s", min(rap$jeu$annees), max(rap$jeu$annees),
          paste(base_annees_vives(don), collapse = ", "))
}


#' Synthese chiffree, SANS jamais lever d'erreur.
#'
#' Audit, C3 : quand `calc_synthese` echouait, le Bilan comme le rapport
#' remplacaient le resultat par un tableau vide, et le bloc disparaissait sans
#' un mot (avec lui, les « points d'attention » sur l'annee incomplete). L'echec
#' est consigne au journal et porte par l'attribut « erreur » du tableau vide.
#' @export
synthese_securisee <- function(don, ctx) {
  # Mode rapide : la comparaison « a periode egale » exige la serie journaliere de la reference.
  if (base_active(don)) {
    s <- data.frame()
    attr(s, "raison") <- "mode_rapide"
    return(s)
  }
  tryCatch(calc_synthese(don, ctx), error = function(e) {
    journaliser_erreur("Synthese chiffree", e)
    s <- data.frame()
    attr(s, "erreur") <- conditionMessage(e)
    s
  })
}


#' Ce qui s'ecarte le plus de la normale, en trois phrases au plus.
#'
#' Audit, S1 : le bilan alignait six a huit cartes de synthese puis quatorze a
#' vingt-quatre graphiques sans dire par quoi commencer. Ce bloc classe les
#' lignes de calc_synthese() par ecart RAPPORTE A L'ECART-TYPE de la reference
#' (un ecart en mm de pluie et un ecart en jours de gel ne se comparent pas
#' autrement) et ne retient que celles qui sortent de la bande neutre : meme
#' regle que la coloration des cartes. Une ligne sans ecart-type (reference de
#' moins de trois annees valides) n'est jamais retenue : on ne la juge pas.
#'
#' @param s resultat de calc_synthese().
#' @param n nombre maximal de lignes retenues.
#' @return data.frame(indicateur, famille, ecart_reduit, texte), eventuellement
#'   vide, du plus marque au moins marque.
#' @export
synthese_a_retenir <- function(s, n = 3L) {
  vide <- data.frame(indicateur = character(0), famille = character(0),
                     ecart_reduit = numeric(0), texte = character(0),
                     stringsAsFactors = FALSE)
  if (is.null(s) || !nrow(s) || !"classe" %in% names(s)) return(vide)
  z <- s$ecart / s$sd_ref
  ok <- s$classe %in% c("haut", "bas") & is.finite(z)
  # Un ecart qui s'affiche « 0 » a la precision de la carte n'est pas un constat.
  dec <- suppressWarnings(as.integer(sub("^%\\.(\\d+)f$", "\\1", s$format)))
  dec[is.na(dec)] <- 1L
  ok <- ok & round(abs(s$ecart), dec) > 0
  if (!any(ok)) return(vide)
  idx <- which(ok)
  idx <- idx[order(-abs(z[idx]))]
  idx <- utils::head(idx, n)

  texte <- vapply(idx, function(i) {
    unite <- if (identical(s$unite[i], "-")) "" else paste0(" ", s$unite[i])
    sprintf("%s : %s%s, soit %s%s de %s que la normale (%s%s).",
            s$indicateur[i], fmt_fr(s$valeur[i], dec[i]), unite,
            fmt_fr(abs(s$ecart[i]), dec[i]), unite,
            if (s$classe[i] == "haut") "plus" else "moins",
            fmt_fr(s$normale[i], dec[i]), unite)
  }, character(1))
  data.frame(indicateur = s$indicateur[idx],
             famille = if ("famille" %in% names(s)) s$famille[idx] else NA_character_,
             ecart_reduit = z[idx], texte = texte, stringsAsFactors = FALSE)
}


# ------------------------------------------------------------------------------
# 5. PROFILS MENSUELS ET JOURNALIERS D'UNE ANNEE CHOISIE
# ------------------------------------------------------------------------------
# Ces indicateurs repondent tous a la meme question : « a quoi a ressemble
# l'annee X, mois par mois ou jour par jour, comparee a la normale ? ».
#
# POINT DE METHODE — L'ORDRE DES OPERATIONS N'EST PAS INDIFFERENT
# ---------------------------------------------------------------
# Pour un CUMUL (pluie, neige), la normale mensuelle est la moyenne des cumuls
# mensuels annee par annee, et non la somme des valeurs journalieres divisee par
# le nombre d'annees. Les deux donnent le meme resultat quand toutes les annees
# sont completes, mais divergent des qu'une annee l'est moins — et la reanalyse
# en compte. Pour une MOYENNE (temperature), la question ne se pose pas de la
# meme facon, mais on procede pareil par coherence.
#
# L'annee comparee vient du contexte (ctx$courante). C'est le selecteur place
# dans l'en-tete de chaque carte qui la fait varier, en reconstruisant un
# contexte local : ces fonctions n'ont pas a savoir d'ou vient l'annee.

#' Profil mensuel generique : normale de reference contre annee comparee.
#'
#' @param colonne Colonne journaliere a agreger (\"p_tot\", \"t_moy\", \"p_sol\"...).
#' @param fun \"somme\" pour un cumul, \"moyenne\" pour une moyenne.
#' @return data.frame(mois, mois_lib, val_ref, val_cur) — noms de colonnes
#'   volontairement neutres, pour que graphe_mensuel() les consomme sans
#'   connaitre la variable tracee.
#' @export
calc_profil_mensuel <- function(don, ctx, colonne, fun = c("somme", "moyenne")) {
  fun <- match.arg(fun)
  # Etape 1 : une valeur par (annee, mois) ; etape 2 : moyenne inter-annuelle.
  # Reference : mois complets seulement. Annee comparee : tous les mois, le mois
  # incomplet etant signale dans `cur_partiel`.
  ref <- ref_mensuelle(don, ctx, colonne, fun, colonne)
  cur <- profil_mois(sel_annees(don, ctx$courante), colonne, fun)

  data.frame(
    mois     = 1:12,
    mois_lib = factor(MOIS_COURT, levels = MOIS_COURT),
    val_ref  = ref$v,
    val_cur  = cur$v,
    cur_partiel = cur$partiel,
    stringsAsFactors = FALSE
  )
}

#' 15 — Precipitations totales mois par mois.
#' @export
calc_precip_mensuel <- function(don, ctx) calc_profil_mensuel(don, ctx, "p_tot", "somme")

#' 16 — Temperature moyenne mois par mois.
#' @export
calc_temperature_mensuelle <- function(don, ctx) calc_profil_mensuel(don, ctx, "t_moy", "moyenne")

#' 17 — Precipitations neigeuses mois par mois.
#' @export
calc_neige_mensuelle <- function(don, ctx) calc_profil_mensuel(don, ctx, "p_sol", "somme")


#' Lissage circulaire d'une serie indexee sur le jour de l'annee.
#'
#' La normale journaliere brute est bruitee : la moyenne d'un 14 mars sur trente
#' ans reste une moyenne de trente valeurs, et le passage d'un jour a l'autre
#' fait des dents de scie qui n'ont aucun sens climatique. On lisse donc sur une
#' fenetre centree de quinze jours.
#'
#' Le lissage est CIRCULAIRE : le 1er janvier a pour voisins les derniers jours
#' de decembre. Un lissage classique laisserait les deux extremites de la courbe
#' calculees sur une demi-fenetre, donc plus bruitees que le reste — et c'est
#' precisement la periode des gels, ou l'on regarde le graphique de pres.
#'
#' @keywords internal
lisser_circulaire <- function(x, k = 15) {
  n <- length(x)
  if (n < k) return(x)
  demi <- (k - 1) %/% 2
  vapply(seq_len(n), function(i) {
    idx <- ((i - demi - 1):(i + demi - 1)) %% n + 1
    m <- mean(x[idx], na.rm = TRUE)
    if (is.finite(m)) m else NA_real_
  }, numeric(1))
}

#' 18 — Temperature journaliere de l'annee comparee contre la normale.
#'
#' @return data.frame(doy, date_axe, t_cur, t_norm, ecart). `date_axe` porte des
#'   dates d'une annee bissextile fictive : c'est ce qui permet a ggplot de
#'   placer les graduations sur les mois reels plutot que sur des numeros de
#'   jour, sans que l'annee affichee ait de sens.
#' @export
calc_temperature_journaliere <- function(don, ctx) {
  if (is.null(don) || !nrow(don)) return(data.frame())

  don$doy <- as.integer(format(don$date, "%j"))

  ref <- sel_annees(don, ctx$ref)
  cur <- sel_annees(don, ctx$courante)
  nb <- ctx$base$normale_jour_tmoy         # base precalculee : voir utils_base.R
  if ((is.null(nb) && !nrow(ref)) || !nrow(cur)) return(data.frame())

  norm <- if (!is.null(nb)) data.frame(doy = 1:366, t_norm = as.numeric(nb))
          else stats::aggregate(list(t_norm = ref$t_moy), by = list(doy = ref$doy),
                                FUN = function(x) mean(x, na.rm = TRUE))
  jour <- stats::aggregate(list(t_cur = cur$t_moy), by = list(doy = cur$doy),
                           FUN = function(x) mean(x, na.rm = TRUE))

  out <- merge(data.frame(doy = 1:366), norm, by = "doy", all.x = TRUE)
  out <- merge(out, jour, by = "doy", all.x = TRUE)
  out <- out[order(out$doy), , drop = FALSE]

  out$t_norm <- lisser_circulaire(out$t_norm, 15)

  # 2000 est bissextile : les 366 jours ont une date, y compris le 29 fevrier.
  out$date_axe <- as.Date(out$doy - 1, origin = "2000-01-01")
  out$ecart    <- out$t_cur - out$t_norm

  # On s'arrete au dernier jour renseigne : prolonger jusqu'au 31 decembre
  # dessinerait une annee en cours comme si elle etait terminee.
  out[!is.na(out$t_cur), , drop = FALSE]
}


# ------------------------------------------------------------------------------
# 6. VENT ET DEFICIT DE PRESSION DE VAPEUR
# ------------------------------------------------------------------------------
# Deux familles d'aleas jusqu'ici absentes de l'application. Toutes deux
# reposent sur des variables SAFRAN facultatives : si la collection ne les
# expose pas, les fonctions renvoient un tableau vide et les cartes affichent un
# message, sans perturber le reste du bilan.

#' Renvoie TRUE si la colonne est absente ou entierement vide.
#' @keywords internal
colonne_vide <- function(don, colonne) {
  is.null(don[[colonne]]) || all(is.na(don[[colonne]]))
}

#' 19 — Vent moyen annuel et journees ventees.
#'
#' @return data.frame(annee, ff_moy, ff_max, n_ventes, normale, ecart, ...)
#' @export
calc_vent <- function(don, ctx) {
  if (is.null(don) || !nrow(don) || colonne_vide(don, "ff")) return(data.frame())

  seuil <- ctx$seuils$vent_fort_ms %||% CONFIG$seuils$vent_fort_ms

  moy <- agreger_annuel(don, "ff", "moyenne", nom = "ff_moy")
  mx  <- stats::aggregate(list(ff_max = don$ff), by = list(annee = don$annee),
                          FUN = function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE))
  mx$annee <- as.integer(mx$annee)
  n_v <- stats::aggregate(list(n_ventes = don$ff >= seuil),
                          by = list(annee = don$annee),
                          FUN = function(x) sum(x, na.rm = TRUE))
  n_v$annee <- as.integer(n_v$annee)

  out <- merge(merge(moy, mx, by = "annee"), n_v, by = "annee")
  enrichir_annuel(out, don, ctx, "ff_moy", vars = "ff")
}

#' 20 — Vent moyen mois par mois.
#' @export
calc_vent_mensuel <- function(don, ctx) {
  if (is.null(don) || !nrow(don) || colonne_vide(don, "ff")) return(data.frame())
  calc_profil_mensuel(don, ctx, "ff", "moyenne")
}

#' 21 — Deficit de pression de vapeur, moyennes annuelle et estivale.
#'
#' La colonne journaliere `vpd` est calculee en amont, dans utils_api.R : c'est
#' une grandeur journaliere au meme titre que p_tot, et la methode FAO-56 qui la
#' produit y est documentee.
#'
#' @return data.frame(annee, vpd_moy, vpd_jja, n_stress, normale, ecart, ...)
#' @export
calc_vpd <- function(don, ctx) {
  if (is.null(don) || !nrow(don) || colonne_vide(don, "vpd")) return(data.frame())

  seuil <- ctx$seuils$vpd_stress_kpa %||% CONFIG$seuils$vpd_stress_kpa

  moy <- agreger_annuel(don, "vpd", "moyenne", nom = "vpd_moy")

  # Juin a aout : c'est la periode ou la demande evaporative decide de la
  # croissance, et ou une moyenne annuelle noierait le signal sous huit mois
  # d'hiver a VPD quasi nul.
  ete <- don[don$mois %in% 6:8, , drop = FALSE]
  jja <- if (nrow(ete)) agreger_annuel(ete, "vpd", "moyenne", nom = "vpd_jja")
         else data.frame(annee = moy$annee, vpd_jja = NA_real_)

  n_s <- stats::aggregate(list(n_stress = don$vpd >= seuil),
                          by = list(annee = don$annee),
                          FUN = function(x) sum(x, na.rm = TRUE))
  n_s$annee <- as.integer(n_s$annee)

  out <- merge(merge(moy, jja, by = "annee", all.x = TRUE), n_s, by = "annee")
  out <- enrichir_annuel(out, don, ctx, "vpd_moy", vars = "vpd")
  # La moyenne d'ete d'un ete inacheve n'est pas celle de l'ete.
  cj <- completude(don, c(6L, 8L), "vpd")
  inc <- cj$annee[cj$partielle]
  out$vpd_jja[out$annee %in% inc] <- NA_real_
  out
}

#' 22 — Deficit de pression de vapeur mois par mois.
#' @export
calc_vpd_mensuel <- function(don, ctx) {
  if (is.null(don) || !nrow(don) || colonne_vide(don, "vpd")) return(data.frame())
  calc_profil_mensuel(don, ctx, "vpd", "moyenne")
}


# ------------------------------------------------------------------------------
# 7. VENT SOUTENU — SEUIL RELATIF ET COMBINAISON AVEC L'ETAT DU SOL
# ------------------------------------------------------------------------------
# POURQUOI UN SEUIL RELATIF PLUTOT QU'ABSOLU
# ------------------------------------------
# Compter les journees au-dessus de 8 m/s a un sens sur un point donne, aucun
# entre deux points. Huit metres par seconde de moyenne journaliere, c'est
# exceptionnel dans le Bourbonnais et ordinaire sur une crete cevenole : le
# meme comptage donnera deux jours ici et soixante la, sans rien dire de
# l'exposition RELATIVE du peuplement.
#
# Le percentile de reference resout cela. Le seuil est calcule sur la periode de
# reference DU POINT ANALYSE : compter les jours qui le depassent revient a
# demander « cette annee a-t-elle ete plus ventee que d'habitude ICI ». La
# question est sensee, et le resultat comparable d'un point a l'autre.
#
# Comparaison STRICTE, et ce n'est pas un detail : le vent moyen journalier est
# fortement discretise, et un test large compterait toutes les journees egales
# au percentile — soit, sur un point peu vente, la quasi-totalite de l'annee.

#' Seuil de vent soutenu, calcule sur la periode de reference du point.
#' @keywords internal
seuil_vent_reference <- function(don, ctx) {
  if (!is.null(ctx$base$seuil_vent)) return(as.numeric(ctx$base$seuil_vent))   # voir utils_base.R
  q <- ctx$seuils$vent_percentile %||% CONFIG$seuils$vent_percentile
  ref <- sel_annees(don, ctx$ref)
  v <- ref$ff[is.finite(ref$ff)]
  if (!length(v)) return(NA_real_)
  as.numeric(stats::quantile(v, probs = q, na.rm = TRUE, names = FALSE))
}

#' 23 — Journees de vent soutenu, au sens du point analyse.
#'
#' @return data.frame(annee, n_p95, seuil_p95, normale, anomalie, ...)
#' @export
calc_vent_soutenu <- function(don, ctx) {
  if (is.null(don) || !nrow(don) || colonne_vide(don, "ff")) return(data.frame())

  seuil <- seuil_vent_reference(don, ctx)
  if (!is.finite(seuil)) return(data.frame())

  res <- stats::aggregate(list(n_p95 = don$ff > seuil),
                          by = list(annee = don$annee),
                          FUN = function(x) sum(x, na.rm = TRUE))
  res$annee <- as.integer(res$annee)
  res$seuil_p95 <- seuil
  enrichir_annuel(res, don, ctx, "n_p95", vars = "ff")
}

#' 24 — Journees de vent soutenu sur sol gorge d'eau.
#'
#' POURQUOI CROISER LES DEUX
#' -------------------------
#' Un arbre ne tombe pas seulement parce qu'il vente : il tombe parce qu'il
#' vente ALORS QUE son ancrage est reduit. Un sol sature d'eau retient moins bien
#' les racines (Gardiner et al. 2016) : un vent qui n'aurait rien produit sur sol
#' ressuye peut alors dechausser des arbres. Les grandes tempetes a chablis
#' d'Europe surviennent surtout en hiver, saison ou les sols sont souvent
#' satures.
#'
#' PORTEE : cet indicateur mesure une EXPOSITION, pas un risque de chablis. Il
#' repose toujours sur des moyennes journalieres, jamais sur des rafales. Il dit
#' combien de journees ont reuni les conditions de fond favorables au
#' dechaussement ; il ne dit pas si une rafale est survenue ce jour-la.
#'
#' @return data.frame(annee, n_combo, n_sature, seuil_p95, normale, anomalie, ...)
#' @export
calc_vent_sol_sature <- function(don, ctx) {
  if (is.null(don) || !nrow(don) ||
      colonne_vide(don, "ff") || colonne_vide(don, "swi")) return(data.frame())

  seuil_v <- seuil_vent_reference(don, ctx)
  seuil_s <- ctx$seuils$swi_sature %||% CONFIG$seuils$swi_sature
  if (!is.finite(seuil_v)) return(data.frame())

  combo   <- don$ff > seuil_v & don$swi >= seuil_s
  res <- stats::aggregate(list(n_combo = combo), by = list(annee = don$annee),
                          FUN = function(x) sum(x, na.rm = TRUE))
  sat <- stats::aggregate(list(n_sature = don$swi >= seuil_s),
                          by = list(annee = don$annee),
                          FUN = function(x) sum(x, na.rm = TRUE))
  res$annee <- as.integer(res$annee)
  sat$annee <- as.integer(sat$annee)

  out <- merge(res, sat, by = "annee")
  out$seuil_p95 <- seuil_v
  out$seuil_swi <- seuil_s
  enrichir_annuel(out, don, ctx, "n_combo", vars = c("ff", "swi"))
}
