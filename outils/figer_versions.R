# ==============================================================================
# outils/figer_versions.R — verrouille les versions des paquets (audit, C6)
# ------------------------------------------------------------------------------
# Usage, a la racine du projet, sur LA machine ou l'application tourne bien :
#
#     source("outils/figer_versions.R")
#
# Ecrit renv.lock : la liste des paquets de DESCRIPTION (et de leurs
# dependances) avec la version exacte installee ici. Pour retrouver cet etat sur
# une autre machine, ou apres une mise a jour qui casserait quelque chose :
#
#     renv::restore(lockfile = "renv.lock")
#
# POURQUOI CE N'EST PAS DEJA FAIT DANS L'ARCHIVE : un verrou decrit les paquets
# d'UNE machine. Celui d'une autre machine ne dirait pas ce qui marche chez vous.
#
# Au deploiement sur shinyapps.io, rsconnect enregistre deja les versions
# utilisees ; ce verrou sert surtout en local, et a garder une trace de l'etat qui
# a produit un rapport (les versions sont aussi inscrites dans son annexe).
# Le fichier renv.lock est a versionner (ou a archiver) avec le code.
# ==============================================================================
if (!file.exists("DESCRIPTION") && !file.exists("description")) {
  stop("A lancer depuis la racine du projet (DESCRIPTION introuvable).")
}
if (!requireNamespace("renv", quietly = TRUE)) {
  stop("Installez d'abord renv : install.packages(\"renv\")")
}

# type = "explicit" : seuls les paquets declares dans DESCRIPTION (et leurs
# dependances) entrent dans le verrou, pas tout ce qui traine dans la session.
# prompt = FALSE : pas de question, le fichier est simplement ecrit.
renv::snapshot(project = ".", lockfile = "renv.lock", type = "explicit",
               prompt = FALSE)
message("renv.lock ecrit. Archivez-le avec le code.")
