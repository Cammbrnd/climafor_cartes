# ==============================================================================
# corriger_casse.R — VERIFIE (et corrige) LA CASSE DES DOSSIERS ET FICHIERS
# ------------------------------------------------------------------------------
# A lancer depuis la racine du projet :   source("corriger_casse.R")
#
# POURQUOI
# --------
# Windows ne distingue pas "src" de "Src". Le serveur de deploiement (Linux) si.
# Un dossier nomme "Src" sur votre disque fait fonctionner source("src/config.R")
# chez vous, puis planter l'application au demarrage une fois publiee (« The
# application failed to start. exit status 1 »). RStudio le signale par
# l'avertissement « Filepaths are case-sensitive on deployment server ».
#
# Ce script compare chaque chemin attendu a ce qui existe REELLEMENT sur disque.
# Par defaut il ne modifie rien : il liste les ecarts. Pour renommer, mettre
# APPLIQUER <- TRUE puis relancer.
# ==============================================================================

APPLIQUER <- FALSE

attendus <- c(
  ".Rprofile",
  ".gitignore",
  ".github",
  ".github/workflows",
  ".github/workflows/construire-carte.yml",
  ".github/workflows/apercu-hors-ligne.yml",
  ".github/workflows/diagnostic-geosas.yml",
  ".github/workflows/tester-apercu.yml",
  ".github/workflows/tester.yml",
  ".github/workflows/verifier-publication.yml",
  ".github/workflows/veille.yml",
  ".rscignore",
  "DESCRIPTION",
  "LICENSE",
  "NEWS.md",
  "README.md",
  "app.R",
  "config",
  "config/acces.csv",
  "deploy.R",
  "donnees_grille",
  "donnees_grille/LISEZMOI.md",
  "donnees_limites",
  "donnees_limites/LISEZMOI.md",
  "donnees_limites/departements.geojson",
  "donnees_limites/regions.geojson",
  "docs",
  "docs/maquette_interface.html",
  "diagnostic.R",
  "diagnostic_stations.R",
  "global.R",
  "outils",
  "outils/apercu_carte.py",
  "outils/apercu_carte_ui.py",
  "outils/apercu_tester.js",
  "outils/carte_cli.R",
  "outils/carte_demo.R",
  "outils/carte_diagnostic_zone.R",
  "outils/carte_diagnostic_grille.R",
  "outils/carte_diagnostic_sim.R",
  "outils/carte_diagnostic_coord.R",
  "outils/carte_format.R",
  "outils/carte_geo.R",
  "outils/carte_precalcul.R",
  "outils/carte_sim.R",
  "outils/carte_sources.R",
  "outils/carte_veille.R",
  "outils/construire_modele_docx.py",
  "outils/couverture.R",
  "outils/figer_versions.R",
  "outils/limites_construire.R",
  "outils/stations_cli.R",
  "outils/stations_demo.R",
  "outils/stations_precalcul.R",
  "rapport",
  "rapport/generer_rapport.R",
  "rapport/images",
  "rapport/images/logo_dsf.png",
  "rapport/images/republique_francaise.png",
  "rapport/modele_dsfr.docx",
  "rapport/rapport_climat.Rmd",
  "rapport/styles_rapport.css",
  "src",
  "src/_gabarit_extension.R",
  "src/carte_couches.R",
  "src/carte_couches_base.R",
  "src/utils_base.R",
  "src/carte_couches_base_stations.R",
  "src/config.R",
  "src/glossaire.R",
  "src/mod_bilan.R",
  "src/mod_carte.R",
  "src/mod_comparer.R",
  "src/mod_graphique.R",
  "src/mod_localisation.R",
  "src/mod_parametres.R",
  "src/mod_periode.R",
  "src/mod_rapport.R",
  "src/mod_sources.R",
  "src/mod_station.R",
  "src/mod_synthese.R",
  "src/mod_terrain.R",
  "src/references.R",
  "src/registre_indicateurs.R",
  "src/serveur_dsf.R",
  "src/ui_page.R",
  "src/utils_api.R",
  "src/utils_cache.R",
  "src/utils_calc.R",
  "src/utils_carte.R",
  "src/utils_comparer.R",
  "src/utils_comparer_duo.R",
  "src/utils_envoi.R",
  "src/mod_lieu_choix.R",
  "src/utils_fiches.R",
  "src/utils_limites.R",
  "src/utils_partage.R",
  "src/utils_plot.R",
  "src/utils_profils.R",
  "src/utils_rapport.R",
  "src/utils_stations.R",
  "src/utils_stations_publiees.R",
  "src/utils_synthese.R",
  "src/utils_synthese_rendu.R",
  "src/utils_territoires.R",
  "src/utils_ui.R",
  "tests",
  "tests/donnees_stations.R",
  "tests/donnees_synthetiques.R",
  "tests/harnais.R",
  "tests/carte_verif_js.js",
  "tests/lancer_tests.R",
  "tests/simulation_tendance.R",
  "tests/simulation_vpd.R",
  "tests/test_stations.R",
  "tests/test_v12.R",
  "tests/test_v13.R",
  "tests/test_v14.R",
  "tests/test_v15.R",
  "tests/test_v16.R",
  "tests/test_v17.R",
  "tests/test_v171.R",
  "tests/test_v18.R",
  "tests/test_v19.R",
  "tests/test_v19_comparer.R",
  "tests/test_v20.R",
  "tests/test_v21.R",
  "tests/test_v22.R",
  "tests/test_v22_stations.R",
  "tests/test_v24.R",
  "www",
  "www/app.js",
  "www/carte",
  "www/carte/carte.js",
  "www/carte/lib",
  "www/carte/lib/LICENSE-maplibre-gl.txt",
  "www/carte/lib/maplibre-gl.css",
  "www/carte/lib/maplibre-gl.js",
  "www/climafor.css",
  "www/favicon.png",
  "www/fonts",
  "www/fonts/LICENCE-OpenSans-OFL.txt",
  "www/fonts/open-sans-latin-400-normal.woff2",
  "www/fonts/open-sans-latin-600-normal.woff2",
  "www/fonts/open-sans-latin-700-normal.woff2",
  "www/logo_dsf.png"
)

# Entree de `parent` qui porte ce nom a la casse pres (vecteur vide si aucune)
entrees_proches <- function(parent, nom) {
  ls <- list.files(parent, all.files = TRUE, no.. = TRUE)
  ls[tolower(ls) == tolower(nom)]
}

# Renommage en deux temps : sous Windows, file.rename("Src", "src") est ignore
# car les deux noms designent le meme element.
renommer <- function(dossier, ancien, nouveau) {
  tmp <- paste0(nouveau, ".tmp_casse")
  ok1 <- file.rename(file.path(dossier, ancien), file.path(dossier, tmp))
  ok1 && file.rename(file.path(dossier, tmp), file.path(dossier, nouveau))
}

ecarts <- list(); manquants <- character(0); deja <- character(0)

for (chemin in attendus) {
  cur <- "."
  for (p in strsplit(chemin, "/", fixed = TRUE)[[1]]) {
    trouve <- entrees_proches(cur, p)
    if (!length(trouve)) { manquants <- c(manquants, chemin); break }
    if (p %in% trouve) { cur <- file.path(cur, p); next }
    # casse differente
    cle <- file.path(cur, trouve[1])
    if (!cle %in% deja) {
      deja <- c(deja, cle)
      ecarts[[length(ecarts) + 1L]] <- list(dossier = cur, reel = trouve[1], attendu = p)
    }
    if (APPLIQUER) {
      ok <- renommer(cur, trouve[1], p)
      if (!ok) { cat("  ECHEC du renommage de ", cle, "\n", sep = ""); break }
    }
    cur <- file.path(cur, if (APPLIQUER) p else trouve[1])
  }
}

cat("\n", strrep("=", 70), "\n", sep = "")
if (!length(ecarts)) {
  cat("OK : la casse de tous les dossiers et fichiers correspond.\n")
} else {
  cat(length(ecarts), " ecart(s) de casse :\n", sep = "")
  for (e in ecarts) {
    cat(sprintf("  %-34s  sur disque : %-18s  attendu : %s\n",
                e$dossier, e$reel, e$attendu))
  }
  if (APPLIQUER) {
    cat("\nRenommes. Relancer le script pour confirmer, puis republier.\n")
  } else {
    cat("\nPour corriger : APPLIQUER <- TRUE en tete de ce script, puis relancer.\n")
  }
}
if (length(manquants)) {
  cat("\nFichiers ou dossiers ABSENTS (a recopier depuis l'archive) :\n")
  cat(paste(" -", unique(manquants)), sep = "\n")
}
cat(strrep("=", 70), "\n", sep = "")
