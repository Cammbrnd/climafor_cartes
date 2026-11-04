# ==============================================================================
# generer_rapport.R — GENERATION DU RAPPORT DANS UN PROCESSUS SEPARE
# ------------------------------------------------------------------------------
# Lance par src/utils_rapport.R :: lancer_rapport_arriere_plan(), jamais a la
# main :
#
#     Rscript rapport/generer_rapport.R <dossier_de_travail> <html|docx> <racine>
#
# Le processus de l'application depose dans <dossier_de_travail> un fichier
# entree.rds (donnees, resultats, contexte...) et un fichier etat.txt valant
# « en_cours ». Ce script charge le code de l'application, genere le document,
# puis ecrit dans etat.txt « ok:<chemin du fichier> » ou « erreur:<message> ».
# L'application, de son cote, se contente de lire etat.txt.
#
# Pourquoi un processus a part : un rapport complet prend de 6 a 16 secondes, et
# pendant ce temps le processus Shiny n'avait aucune main pour les autres
# sessions qu'il sert.
# ==============================================================================
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) stop("usage : Rscript generer_rapport.R <dossier> <format> <racine>")
dossier <- args[1]; format <- args[2]; racine <- args[3]
etat <- file.path(dossier, "etat.txt")

ecrire_etat <- function(txt) writeLines(gsub("[\r\n]+", " ", txt), etat)
# Le processus de l'application peut ainsi l'arreter si la generation depasse
# le delai maximal.
writeLines(as.character(Sys.getpid()), file.path(dossier, "pid.txt"))

tryCatch({
  setwd(racine)
  # Insensible a la casse : certains partages reseau passent tout en minuscules
  # (global.r), ce que Linux distingue de global.R.
  global <- list.files(".", pattern = "^global\\.r$", ignore.case = TRUE)[1]
  if (is.na(global)) stop("global.R introuvable dans ", racine)
  suppressMessages(suppressWarnings(source(global, encoding = "UTF-8")))
  entree <- readRDS(file.path(dossier, "entree.rds"))
  fichier <- rendre_rapport(dossier, entree, format)
  ecrire_etat(paste0("ok:", normalizePath(fichier, winslash = "/")))
}, error = function(e) {
  ecrire_etat(paste0("erreur:", conditionMessage(e)))
  quit(status = 1L)
})
