# ==============================================================================
# simulation_tendance.R — TAUX DE FAUX POSITIFS DU TEST DE TENDANCE
# ------------------------------------------------------------------------------
# Mesure, sur des series SANS AUCUNE tendance mais autocorrelees (AR(1)), la
# frequence a laquelle chaque methode conclut a tort a une tendance significative
# au seuil de 5 %. Un test juste en donne 5 %.
#
#   Rscript tests/simulation_tendance.R [nombre_de_simulations]
#
# Compare : la regression lineaire de la v11 (p-value de Fisher, independance
# supposee) et la methode de la v12 (Mann-Kendall corrige, tendance()).
# Mesure aussi la puissance (detection d'une vraie tendance) pour verifier que la
# correction ne rend pas le test aveugle.
# ==============================================================================
args <- commandArgs(trailingOnly = TRUE)
B <- if (length(args)) as.integer(args[1]) else 2000L
source("src/config.R"); source("src/utils_calc.R")

ar1 <- function(n, phi) {
  # phi = 0 : bruit blanc (arima.sim avec un ordre nul emet des avertissements)
  if (phi == 0) return(stats::rnorm(n))
  as.numeric(stats::arima.sim(list(ar = phi), n = n))
}

essai <- function(n, phi, pente_dec = 0, B) {
  x <- seq_len(n)
  p_ols <- p_mk <- numeric(B)
  for (b in seq_len(B)) {
    y <- ar1(n, phi) + pente_dec / 10 * x
    p_ols[b] <- summary(stats::lm(y ~ x))$coefficients[2, 4]
    p_mk[b]  <- tendance(x, y, n_min = 5L)$p
  }
  c(ols = mean(p_ols < 0.05), mk_corrige = mean(p_mk < 0.05))
}

set.seed(2026)
grille <- expand.grid(n = c(30L, 60L), phi = c(0, 0.3, 0.5, 0.7))
res <- do.call(rbind, lapply(seq_len(nrow(grille)), function(i) {
  r <- essai(grille$n[i], grille$phi[i], 0, B)
  data.frame(n = grille$n[i], phi = grille$phi[i], t(round(100 * r, 1)))
}))
cat(sprintf("\nFAUX POSITIFS (%% de series sans tendance declarees significatives, seuil 5 %%, %d simulations)\n", B))
print(res, row.names = FALSE)

# Puissance : tendance reelle de 0,3 ecart-type par decennie... soit 0,03 par an
cat("\nPUISSANCE (tendance vraie de 0,3 ecart-type de bruit blanc par decennie)\n")
pu <- do.call(rbind, lapply(c(30L, 60L), function(n) {
  r <- essai(n, 0.3, pente_dec = 0.3, B = min(B, 1000L))
  data.frame(n = n, phi = 0.3, t(round(100 * r, 1)))
}))
print(pu, row.names = FALSE)
