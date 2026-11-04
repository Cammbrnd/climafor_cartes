# ==============================================================================
# tests/harnais.R — HARNAIS DE TEST MINIMAL
# ------------------------------------------------------------------------------
# POURQUOI PAS testthat
# ---------------------
# testthat serait le choix par defaut d'un paquet R. Ici l'application est
# deployee sur shinyapps.io a partir d'un simple dossier : chaque dependance
# supplementaire est un paquet de plus a installer sur l'instance, pour un
# besoin qui tient en quarante lignes. Le harnais ci-dessous n'utilise que la
# base R. Si le projet migre un jour vers une structure de paquet, la
# conversion vers testthat est mecanique : `verifier()` devient `expect_true()`,
# `presque_egal()` devient `expect_equal()`.
#
# CONVENTION DE SORTIE
# --------------------
# Le script se termine avec un code de sortie non nul si un test echoue : c'est
# ce qui permet de le brancher tel quel sur une integration continue.
# ==============================================================================

.RESULTATS <- new.env(parent = emptyenv())
.RESULTATS$ok <- 0L
.RESULTATS$ko <- character(0)
.RESULTATS$groupe <- ""

#' Ouvre un groupe de tests.
groupe <- function(titre) {
  .RESULTATS$groupe <- titre
  cat(sprintf("\n%s\n%s\n", titre, strrep("-", nchar(titre))))
}

#' Verifie qu'une condition est vraie.
#'
#' @param condition Expression logique de longueur 1.
#' @param intitule Ce que le test etablit, redige a l'affirmative.
verifier <- function(condition, intitule) {
  reussi <- isTRUE(tryCatch(condition, error = function(e) {
    cat(sprintf("   erreur : %s\n", conditionMessage(e)))
    FALSE
  }))
  if (reussi) {
    .RESULTATS$ok <- .RESULTATS$ok + 1L
    cat(sprintf("  OK   %s\n", intitule))
  } else {
    .RESULTATS$ko <- c(.RESULTATS$ko, sprintf("%s / %s", .RESULTATS$groupe, intitule))
    cat(sprintf("  ECHEC %s\n", intitule))
  }
  invisible(reussi)
}

#' Egalite numerique a une tolerance pres.
#'
#' Comparer des flottants avec == est une erreur : 0,1 + 0,2 ne vaut pas 0,3 en
#' arithmetique binaire. La tolerance par defaut est large devant l'erreur
#' d'arrondi et etroite devant toute erreur de methode, qui est ce que ces
#' tests cherchent a attraper.
presque_egal <- function(observe, attendu, tolerance = 1e-8) {
  if (length(observe) != length(attendu)) return(FALSE)
  fini <- is.finite(observe) & is.finite(attendu)
  if (!all(fini == (is.finite(observe) | is.finite(attendu)))) return(FALSE)
  all(abs(observe[fini] - attendu[fini]) < tolerance)
}

#' Bilan final et code de sortie.
bilan <- function() {
  cat(sprintf("\n%s\n", strrep("=", 60)))
  n_ko <- length(.RESULTATS$ko)
  cat(sprintf("%d test(s) reussi(s), %d echec(s)\n", .RESULTATS$ok, n_ko))
  if (n_ko) {
    cat("\nEchecs :\n")
    for (e in .RESULTATS$ko) cat(sprintf("  - %s\n", e))
  }
  cat(sprintf("%s\n", strrep("=", 60)))
  invisible(n_ko)
}
