# ==============================================================================
# outils/couverture.R — couverture de lignes de la suite de tests (audit, C8)
# ------------------------------------------------------------------------------
# Usage, a la racine du projet :   Rscript outils/couverture.R
# Necessite le paquet covr (non requis par l'application).
#
# La suite de tests charge elle-meme global.R ; ici on charge le code AVANT, on
# le fait instrumenter par covr, puis on rejoue la suite en retirant son propre
# chargement (sinon elle redefinirait les fonctions instrumentees).
# Sortie : couverture par fichier, et totale, en lignes executees.
# ==============================================================================
if (!requireNamespace("covr", quietly = TRUE)) {
  stop("Le paquet 'covr' est necessaire : install.packages(\"covr\")")
}
if (!file.exists("global.R")) stop("A lancer depuis la racine du projet.")

options(keep.source = TRUE)             # covr a besoin des references de source
suppressMessages(source("global.R"))

lignes <- readLines("tests/lancer_tests.R", encoding = "UTF-8")
chargement <- grepl('^suppressMessages\\(source\\("global.R"\\)\\)', lignes)
if (sum(chargement) != 1L) stop("lancer_tests.R a change : adapter outils/couverture.R")
lignes[chargement] <- "invisible(NULL)"
# La suite se termine par quit() : il couperait le script avant le calcul.
fin <- grepl("^quit\\(status", lignes)
if (sum(fin) != 1L) stop("lancer_tests.R a change : adapter outils/couverture.R")
lignes[fin] <- "invisible(n_echecs)"
suite <- tempfile(fileext = ".R")
writeLines(lignes, suite, useBytes = TRUE)

cov <- covr::environment_coverage(globalenv(), test_files = suite)
d <- as.data.frame(cov)
d <- d[grepl("^(src/|app\\.R|global\\.R)", d$filename), , drop = FALSE]

par_fichier <- do.call(rbind, lapply(split(d, d$filename), function(x) {
  lignes_x <- unique(x[, c("first_line", "value")])
  data.frame(fichier = x$filename[1],
             lignes = nrow(lignes_x), couvertes = sum(lignes_x$value > 0),
             stringsAsFactors = FALSE)
}))
par_fichier$pct <- round(100 * par_fichier$couvertes / par_fichier$lignes, 1)
par_fichier <- par_fichier[order(par_fichier$pct), ]
print(par_fichier, row.names = FALSE)
cat(sprintf("\nTOTAL : %d / %d lignes executees (%.1f %%)\n",
            sum(par_fichier$couvertes), sum(par_fichier$lignes),
            100 * sum(par_fichier$couvertes) / sum(par_fichier$lignes)))
