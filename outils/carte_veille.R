# ==============================================================================
# carte_veille.R — FAUT-IL RECONSTRUIRE LA CARTE ?
# ------------------------------------------------------------------------------
# Lance une fois par mois par le workflow « Veille ». Compare :
#   - la derniere annee du jeu PUBLIE (meta.json en ligne) ;
#   - la derniere annee COMPLETE disponible dans SAFRAN (une seule requete, sur un
#     point d'essai : Toulouse).
# S'il y a une annee de plus, le workflow relance la construction.
#
# Ne declenche JAMAIS le premier calcul : tant qu'aucun jeu n'est publie, la
# reponse est « non » (le premier calcul se lance a la main, apres avoir verifie
# que le service accepte la charge).
#
#   Rscript outils/carte_veille.R url=https://<compte>.github.io/<depot>/
# Ecrit « declencher=true|false » (et les annees) dans le fichier $GITHUB_OUTPUT
# (a defaut, sur la sortie standard) : on n'utilise pas la sortie standard dans
# GitHub Actions, car le chargement de l'application y ecrit aussi.
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
url <- sub("^url=", "", grep("^url=", args, value = TRUE)[1])
suppressMessages(source("global.R"))

publie <- NA_integer_
if (!is.na(url) && carte_url_sure(sub("/*$", "/", url))) {
  publie <- tryCatch({
    rep <- httr2::req_perform(httr2::req_error(httr2::req_timeout(
      httr2::request(paste0(sub("/*$", "/", url), "meta.json")), 30), is_error = function(r) FALSE))
    if (httr2::resp_status(rep) >= 400) NA_integer_ else {
      m <- httr2::resp_body_json(rep, simplifyVector = FALSE)
      max(as.integer(unlist(m$annees)))
    }
  }, error = function(e) NA_integer_)
}

an <- as.integer(format(Sys.Date(), "%Y")) - 1L
don <- tryCatch(safran_charger(1.4442, 43.6047, an), error = function(e) NULL)
# Une annee est complete quand SAFRAN a publie (presque) tous ses jours.
disponible <- if (is.data.frame(don) && nrow(don) >= 363L) an else an - 1L

declencher <- !is.na(publie) && disponible > publie
sortie <- Sys.getenv("GITHUB_OUTPUT")
texte <- sprintf("publie=%s\ndisponible=%d\ndeclencher=%s\n",
                 if (is.na(publie)) "aucun" else publie, disponible, tolower(declencher))
cat(texte)
if (nzchar(sortie)) cat(texte, file = sortie, append = TRUE)
