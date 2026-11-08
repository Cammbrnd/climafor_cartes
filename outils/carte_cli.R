# ==============================================================================
# carte_cli.R — LIGNE DE COMMANDE DU PRECALCUL (utilisee par GitHub Actions)
# ------------------------------------------------------------------------------
# A lancer depuis la RACINE du projet :
#
#   Rscript outils/carte_cli.R fragment  source=sim|position|demo mailles=<sim|csv|demo:32> \
#           annee_debut=1959 annee_fin=2025 fragment=1 fragments=20 sortie=parts
#   Rscript outils/carte_cli.R assembler source_texte="..." mailles=<csv|demo:32> \
#           annee_debut=1959 annee_fin=2025 parts=parts sortie=public
#   Rscript outils/carte_cli.R verifier dossier=public
#
# fragment  : calcule les mailles dont le rang tombe dans ce fragment (entrelaces :
#             le fragment k prend les rangs k, k+n, k+2n... pour equilibrer la charge)
#             et ecrit parts/part_<k>.rds ;
# assembler : reunit les fragments, ecrit les fichiers publies, les verifie ;
# verifier  : relit un jeu publie avec la validation de l'application (code de sortie
#             non nul s'il est invalide).
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
for (f in c("outils/carte_geo.R", "outils/carte_format.R", "outils/carte_precalcul.R",
            "outils/carte_demo.R", "outils/carte_sim.R", "outils/carte_sources.R")) source(f)

# La base precalculee du bilan (src/carte_couches_base.R) se calcule avec la carte : `base=non`
# la retire (le site publie n'a alors pas de mode rapide).
base_demandee <- function() if (identical(opt$base %||% "oui", "non")) list() else BASE_COUCHES

annees_demandees <- function() {
  fin <- if (nzchar(opt$annee_fin %||% "")) entier("annee_fin") else as.integer(format(Sys.Date(), "%Y")) - 1L
  debut <- entier("annee_debut", 1959L)
  if (debut >= fin || debut < 1950L || fin > as.integer(format(Sys.Date(), "%Y"))) {
    stop(sprintf("Annees invalides : %d a %d.", debut, fin), call. = FALSE)
  }
  seq.int(debut, fin)
}

if (commande == "fragment") {
  exiger("source", "mailles", "fragment", "fragments", "sortie")
  annees <- annees_demandees()
  m <- carte_lire_mailles(opt$mailles)
  k <- entier("fragment"); nf <- entier("fragments")
  if (nf < 1L || k < 1L || k > nf) stop("fragment doit etre compris entre 1 et fragments.", call. = FALSE)
  indices <- seq(k, nrow(m), by = nf)
  lire <- switch(opt$source,
    demo     = carte_demo_source(annees),
    position = carte_source_position(annees, pause_s = as.numeric(opt$pause %||% 0.5)),
    sim      = {
      # CARTE_SIM_LOCAL : dossier de fichiers QUOT_SIM2_AAAA.csv.gz deja presents (tests).
      local <- Sys.getenv("CARTE_SIM_LOCAL", "")
      carte_source_sim(annees, m, indices,
        fournisseur = if (nzchar(local)) carte_sim_fournisseur_local(local) else carte_sim_fournisseur_http(),
        supprimer = !nzchar(local),
        parametres_api = if (nzchar(Sys.getenv("CARTE_SIM_PARAMETRES", ""))) strsplit(Sys.getenv("CARTE_SIM_PARAMETRES"), ",")[[1]]
                         else safran_parametres_utiles()$api)
    },
    stop("source : demo, position ou sim", call. = FALSE))
  cat(sprintf("Fragment %d/%d : %d mailles, annees %d-%d, source %s\n", k, nf, length(indices),
              min(annees), max(annees), opt$source))
  dir.create(opt$sortie, recursive = TRUE, showWarnings = FALSE)
  part <- carte_construire(lire, m$lon, m$lat, annees, indices,
                           fichier = file.path(opt$sortie, sprintf("part_%03d.rds", k)),
                           base = base_demandee())
  cat(sprintf("Termine : %d maille(s) sans donnees sur %d.\n", length(part$echecs), length(indices)))

} else if (commande == "assembler") {
  exiger("source_texte", "mailles", "parts", "sortie")
  annees <- annees_demandees()
  m <- carte_lire_mailles(opt$mailles)
  fichiers <- list.files(opt$parts, pattern = "^part_[0-9]+\\.rds$", full.names = TRUE, recursive = TRUE)
  if (!length(fichiers)) stop("Aucun fragment dans ", opt$parts, call. = FALSE)
  cat(sprintf("%d fragment(s) a reunir.\n", length(fichiers)))
  l2e <- if (all(c("lambx", "lamby") %in% names(m))) list(x = m$lambx * 100, y = m$lamby * 100) else NULL
  # Seule limite connue de la source sim : l'ETP du fichier SIM n'est pas l'ETP_Q de GeoSAS
  # (ecart de quelques %, mesure par outils/carte_diagnostic_sim.R). Elle est dite a l'utilisateur.
  note_etp <- if (!is.null(l2e) && grepl("SIM", opt$source_texte, fixed = TRUE))
    paste("L'ETP vient du fichier SIM de Météo-France : elle s'écarte de quelques % de l'ETP lue par",
          "l'analyse d'une maille, donc ce bilan peut différer un peu de celui de l'analyse.") else ""
  r <- carte_assembler(fichiers, m$lon, m$lat, annees, opt$source_texte, opt$sortie, l2e = l2e, note_etp = note_etp,
                       base = base_demandee())
  cat(sprintf("Jeu ecrit dans %s : %d mailles, %d-%d, %d Ko, %d maille(s) sans donnees.\n",
              r$dossier, r$n, min(r$annees), max(r$annees), r$taille_octets %/% 1024, length(r$echecs)))
  if (length(r$annees_retirees)) {
    cat(sprintf("Annee(s) retiree(s), pas encore complete(s) dans SAFRAN : %s\n", paste(r$annees_retirees, collapse = ", ")))
  }

} else if (commande == "verifier") {
  exiger("dossier")
  lu <- carte_lire_meta_locale(opt$dossier)
  if (is.null(lu$meta)) {
    cat("INVALIDE :\n"); cat(paste0(" - ", lu$erreurs, "\n"), sep = "")
    quit(status = 1L)
  }
  cat(sprintf("Valide : %d mailles, %d-%d, %d couches, %d entrees de base, genere le %s, source : %s\n",
              lu$meta$n_mailles, min(unlist(lu$meta$annees)), max(unlist(lu$meta$annees)),
              length(lu$meta$couches), length(lu$meta[["base"]]), lu$meta$genere_le, lu$meta$source))
} else {
  stop(sprintf("Commande inconnue : %s", commande), call. = FALSE)
}
