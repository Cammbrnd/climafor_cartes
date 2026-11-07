# ==============================================================================
# stations_cli.R — LIGNE DE COMMANDE DU PRECALCUL DES STATIONS (GitHub Actions)
# ------------------------------------------------------------------------------
# A lancer depuis la RACINE du projet :
#
#   Rscript outils/stations_cli.R fragment source=mf|demo fragment=1 fragments=4 \
#           annee_debut=1959 annee_fin=2025 sortie=parts_stations
#   Rscript outils/stations_cli.R assembler source_texte="..." annee_debut=1959 annee_fin=2025 \
#           parts=parts_stations sortie=public n_min=300
#   Rscript outils/stations_cli.R verifier dossier=public
#
# fragment  : traite les departements dont le rang tombe dans ce fragment (entrelaces, pour
#             equilibrer la charge) et ecrit parts_stations/part_<k>.rds ;
# assembler : reunit les fragments et ecrit stations.json et s_<grandeur>.bin.gz dans le
#             dossier de la carte (a cote de meta.json) ;
# verifier  : relit les fichiers avec la validation de l'application (code de sortie non nul
#             s'ils sont invalides).
#
# Les annees DOIVENT etre celles de la carte (meme annee_debut et annee_fin) : stations et
# mailles se comparent annee par annee.
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Commande attendue : fragment | assembler | verifier", call. = FALSE)
commande <- args[1]
opt <- list()
for (a in args[-1]) {
  k <- regexpr("=", a, fixed = TRUE)
  if (k < 2) stop(sprintf("Argument mal forme (cle=valeur attendu) : %s", a), call. = FALSE)
  opt[[substr(a, 1, k - 1)]] <- substring(a, k + 1)
}
exiger <- function(...) {
  m <- setdiff(c(...), names(opt))
  if (length(m)) stop(sprintf("Argument(s) manquant(s) : %s", paste(m, collapse = ", ")), call. = FALSE)
}
entier <- function(nom, defaut = NULL) {
  v <- opt[[nom]] %||% defaut
  x <- suppressWarnings(as.integer(v))
  if (length(x) != 1L || is.na(x)) stop(sprintf("%s : entier attendu (recu : %s)", nom, as.character(v)), call. = FALSE)
  x
}

suppressMessages(source("global.R"))
for (f in c("outils/carte_format.R", "outils/carte_precalcul.R",
            "outils/stations_precalcul.R", "outils/stations_demo.R")) source(f)

annees_demandees <- function() {
  fin <- if (nzchar(opt$annee_fin %||% "")) entier("annee_fin") else as.integer(format(Sys.Date(), "%Y")) - 1L
  debut <- entier("annee_debut", 1959L)
  if (debut >= fin || debut < 1950L || fin > as.integer(format(Sys.Date(), "%Y"))) {
    stop(sprintf("Annees invalides : %d a %d.", debut, fin), call. = FALSE)
  }
  seq.int(debut, fin)
}

if (commande == "fragment") {
  exiger("source", "fragment", "fragments", "sortie")
  annees <- annees_demandees()
  k <- entier("fragment"); nf <- entier("fragments")
  if (nf < 1L || k < 1L || k > nf) stop("fragment doit etre compris entre 1 et fragments.", call. = FALSE)
  depts <- switch(opt$source,
    mf   = stations_departements(),
    demo = sprintf("%02d", setdiff(1:95, 20L)),
    stop("source : mf ou demo", call. = FALSE))
  if (!length(depts)) stop("Aucun departement trouve (liste des fichiers Meteo-France indisponible ?).", call. = FALSE)
  depts <- depts[seq(k, length(depts), by = nf)]
  lire <- switch(opt$source,
    mf   = stations_source_mf(annees),
    demo = stations_demo_source(annees))
  cat(sprintf("Fragment %d/%d : %d departements, annees %d-%d, source %s\n", k, nf, length(depts),
              min(annees), max(annees), opt$source))
  dir.create(opt$sortie, recursive = TRUE, showWarnings = FALSE)
  part <- stations_construire(lire, depts, annees,
                              fichier = file.path(opt$sortie, sprintf("part_%03d.rds", k)))
  cat(sprintf("Termine : %d station(s) retenue(s), %d departement(s) sans donnees.\n",
              if (is.null(part$stations)) 0L else nrow(part$stations), length(part$echecs)))

} else if (commande == "assembler") {
  exiger("source_texte", "parts", "sortie")
  annees <- annees_demandees()
  fichiers <- list.files(opt$parts, pattern = "^part_[0-9]+\\.rds$", full.names = TRUE, recursive = TRUE)
  if (!length(fichiers)) stop("Aucun fragment dans ", opt$parts, call. = FALSE)
  cat(sprintf("%d fragment(s) a reunir.\n", length(fichiers)))
  r <- stations_assembler(fichiers, annees, opt$source_texte, opt$sortie, n_min = entier("n_min", 1L))
  cat(sprintf("Stations ecrites dans %s : %d stations, %d grandeurs, %d-%d, %d Ko.\n",
              r$dossier, r$n, length(r$couches), min(r$annees), max(r$annees), r$taille_octets %/% 1024))
  if (length(r$departements_sans_donnees)) {
    cat(sprintf("Departement(s) sans donnees : %s\n", paste(r$departements_sans_donnees, collapse = ", ")))
  }

} else if (commande == "verifier") {
  exiger("dossier")
  lu <- stations_lire_meta_locale(opt$dossier)
  if (is.null(lu$meta)) {
    cat("INVALIDE :\n"); cat(paste0(" - ", lu$erreurs, "\n"), sep = "")
    quit(status = 1L)
  }
  cat(sprintf("Valide : %d stations, %d-%d, %d grandeurs, genere le %s, source : %s\n",
              lu$meta$n_stations, min(unlist(lu$meta$annees)), max(unlist(lu$meta$annees)),
              length(lu$meta$couches), lu$meta$genere_le, lu$meta$source))
} else {
  stop(sprintf("Commande inconnue : %s", commande), call. = FALSE)
}
