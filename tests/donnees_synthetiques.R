# ==============================================================================
# tests/donnees_synthetiques.R — JEUX D'ESSAI
# ------------------------------------------------------------------------------
# PRINCIPE : ne jamais tester une fonction de calcul contre une donnee reelle.
# Une serie SAFRAN ne fournit aucune verite de reference — on ne saurait dire si
# le resultat obtenu est juste, seulement s'il est plausible, ce qui laisse
# passer exactement les erreurs de methode que l'on cherche a attraper.
#
# Les jeux ci-dessous sont construits pour que le resultat attendu se CALCULE A
# LA MAIN. Exemple : une annee ou il pleut 2 mm tous les jours doit donner un
# cumul de 730 mm, un janvier a 62 mm, et rien d'autre.
# ==============================================================================

#' Serie journaliere synthetique.
#'
#' @param annees Vecteur d'annees a produire.
#' @param t_moy Temperature moyenne constante, ou fonction(doy, annee).
#' @param p_jour Precipitation liquide quotidienne constante.
#' @param amplitude Ecart entre t_min/t_max et t_moy.
#' @return data.frame aux colonnes du tableau produit par safran_charger().
serie_synthetique <- function(annees = 2000:2002,
                              t_moy = 10,
                              p_jour = 2,
                              amplitude = 5,
                              hu = 70,
                              ff = 3,
                              swi = 0.6,
                              etp = 1) {
  morceaux <- lapply(annees, function(a) {
    dates <- seq(as.Date(sprintf("%d-01-01", a)),
                 as.Date(sprintf("%d-12-31", a)), by = "day")
    doy <- seq_along(dates)
    tm <- if (is.function(t_moy)) t_moy(doy, a) else rep(t_moy, length(dates))
    data.frame(
      date  = dates,
      annee = a,
      mois  = as.integer(format(dates, "%m")),
      jour  = as.integer(format(dates, "%d")),
      doy   = doy,
      t_moy = tm,
      t_min = tm - amplitude,
      t_max = tm + amplitude,
      p_liq = rep(p_jour, length(dates)),
      p_sol = rep(0, length(dates)),
      etp   = rep(etp, length(dates)),
      swi   = rep(swi, length(dates)),
      hu    = rep(hu, length(dates)),
      ff    = rep(ff, length(dates)),
      stringsAsFactors = FALSE
    )
  })
  don <- do.call(rbind, morceaux)

  # Colonnes derivees : MEME fonction que l'application (safran_deriver), et non
  # une copie de sa formule — v1.4 et avant, la copie reproduisait l'erreur du
  # VPD « a l'identique » et les tests ne pouvaient pas la voir (audit M8). Les
  # valeurs attendues sont, elles, ecrites en dur dans tests/test_v15.R.
  don <- safran_deriver(don)

  don[order(don$date), , drop = FALSE]
}

#' Contexte de calcul pour les tests.
ctx_test <- function(ref = 2000:2001, courante = 2002,
                     affichage = 2000:2002, fenetre = c(4, 10)) {
  ctx_creer(ref = ref, affichage = affichage, courante = courante,
            fenetre = fenetre, seuils = CONFIG$seuils)
}
