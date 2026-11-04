# ==============================================================================
# simulation_vpd.R — MESURE DE L'ECART DE VPD (audit v1.1, M1)
# ------------------------------------------------------------------------------
# A lancer depuis la racine du projet :   Rscript tests/simulation_vpd.R
#
# Compare, pour les memes jours simules, le VPD calcule par les versions <= 1.4
# (pression de vapeur reelle prise sur es(Tmoy)) et par la v1.5 (FAO-56, eq. 12
# et 19), puis le nombre de jours au-dessus du seuil de CONFIG.
#
# Hypotheses de l'audit : etes simules (juin-aout, 92 jours), temperature
# moyenne 20,5 +/- 3,2 degC, amplitude 7 +/- 2 degC, humidite 64 +/- 11 %,
# tirages independants d'un jour a l'autre. L'« amplitude » est ici la DEMI-
# amplitude, comme dans tests/donnees_synthetiques.R : Tmin = Tmoy - amplitude,
# Tmax = Tmoy + amplitude, soit un ecart jour-nuit de 14 degC, courant en ete.
# Ce n'est pas un climat reel : ce sont des conditions d'ete plausibles pour
# mesurer un ECART de formule.
# ==============================================================================

suppressMessages(source("global.R"))

vpd_ancien <- function(t_min, t_max, t_moy, hu) {   # v1.4 et avant
  pmax((pression_saturante(t_max) + pression_saturante(t_min)) / 2 -
         pression_saturante(t_moy) * hu / 100, 0)
}

# -- 1. Les cinq situations du tableau de l'audit -------------------------------
cas <- data.frame(
  situation = c("Hiver", "Printemps", "Ete frais", "Ete chaud", "Canicule"),
  tn = c(-2, 5, 12, 16, 22), tx = c(6, 17, 28, 34, 38), hu = c(85, 65, 55, 45, 38))
cas$vpd_ancien <- with(cas, vpd_ancien(tn, tx, (tn + tx) / 2, hu))
cas$vpd_fao56  <- with(cas, vpd_fao56(tn, tx, hu))
cas$ecart_pct  <- 100 * (cas$vpd_ancien / cas$vpd_fao56 - 1)
cat("\nVPD en kPa, cinq situations (Tn, Tx en degC, HU en %) :\n")
print(format(cas, digits = 3, nsmall = 3), row.names = FALSE)

# -- 2. Etes simules --------------------------------------------------------------
simuler <- function(graine, n_etes = 30L, n_jours = 92L) {
  set.seed(graine)
  n <- n_etes * n_jours
  tm  <- rnorm(n, 20.5, 3.2)
  amp <- pmax(rnorm(n, 7, 2), 0.5)
  hu  <- pmin(pmax(rnorm(n, 64, 11), 5), 100)
  tn <- tm - amp; tx <- tm + amp
  ete <- rep(seq_len(n_etes), each = n_jours)
  seuil <- CONFIG$seuils$vpd_stress_kpa
  va <- vpd_ancien(tn, tx, tm, hu); vn <- vpd_fao56(tn, tx, hu)
  c(moy_ancien = mean(va), moy_fao56 = mean(vn),
    jours_ancien = sum(va >= seuil) / n_etes, jours_fao56 = sum(vn >= seuil) / n_etes)
}
res <- t(vapply(1:200, simuler, numeric(4)))
m <- colMeans(res)
cat(sprintf("\n30 etes simules (moyenne de 200 tirages), seuil %.1f kPa :\n", CONFIG$seuils$vpd_stress_kpa))
cat(sprintf("  VPD moyen        : %.2f kPa (v1.4)  ->  %.2f kPa (v1.5)\n", m["moy_ancien"], m["moy_fao56"]))
cat(sprintf("  jours >= seuil   : %.1f par ete (v1.4)  ->  %.1f par ete (v1.5)\n", m["jours_ancien"], m["jours_fao56"]))
cat(sprintf("  rapport des jours: x %.2f\n", m["jours_ancien"] / m["jours_fao56"]))
