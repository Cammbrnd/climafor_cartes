# ==============================================================================
# diagnostic.R — CONTROLE DE L'ACCES AUX DONNEES
# ------------------------------------------------------------------------------
# A lancer depuis la racine du projet quand l'application repond
# "Aucune donnee pour ce point" :
#
#     source("diagnostic.R")
#
# Le script interroge l'API pas a pas et affiche a chaque etape ce qui est
# reellement envoye et reellement recu. Il ne modifie rien.
#
# Point de test par defaut : Limoise (03), 46,6876 N / 3,0541 E.
# ==============================================================================

LAT <- 46.6876
LON <- 3.0541
ANNEE <- 2020

# Resolveur de casse (voir global.R) : le partage peut avoir renomme .R en .r
ci <- function(chemin) {
  cur <- "."
  for (p in strsplit(chemin, "/", fixed = TRUE)[[1]]) {
    ls <- list.files(cur, all.files = TRUE, no.. = TRUE); h <- ls[tolower(ls) == tolower(p)]
    cur <- file.path(cur, if (p %in% h || !length(h)) p else h[1])
  }
  sub("^\\./", "", cur)
}
library(httr2)
source(ci("src/config.R"), encoding = "UTF-8")
source(ci("src/utils_cache.R"), encoding = "UTF-8")   # v1.7 : utils_api.R en depend
source(ci("src/utils_api.R"), encoding = "UTF-8")

titre <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n",
                         strrep("=", 78), "\n", sep = "")

# ------------------------------------------------------------------------------
titre("1. Metadonnees de la collection")
# ------------------------------------------------------------------------------
url_collection <- sub("/position/?$", "", CONFIG$api$edr_base)
cat("URL : ", url_collection, "\n\n", sep = "")

rep_col <- tryCatch(
  httr2::req_perform(httr2::req_error(
    httr2::req_timeout(httr2::request(url_collection), 25),
    is_error = function(r) FALSE)),
  error = function(e) { cat("ECHEC RESEAU : ", conditionMessage(e), "\n"); NULL })

if (!is.null(rep_col)) {
  cat("Statut HTTP : ", httr2::resp_status(rep_col), "\n", sep = "")
  if (httr2::resp_status(rep_col) < 400) {
    j <- httr2::resp_body_json(rep_col, simplifyVector = FALSE)
    pn <- j$parameter_names %||% j$`parameter-names`
    if (length(pn)) {
      cat("\nParametres exposes par la collection (", length(pn), ") :\n", sep = "")
      cat(paste(" -", names(pn)), sep = "\n")
    } else {
      cat("\nLa reponse ne contient pas de champ parameter_names.\n")
      cat("Cles de premier niveau : ", paste(names(j), collapse = ", "), "\n", sep = "")
    }
  } else {
    cat("Corps : ",
        substr(httr2::resp_body_string(rep_col), 1, 500), "\n", sep = "")
  }
}

# ------------------------------------------------------------------------------
titre("2. Variables que l'application va demander")
# ------------------------------------------------------------------------------
neg <- safran_parametres_utiles()
cat("Source           : ", neg$source, "\n", sep = "")
cat("Demandees        : ", paste(neg$api, collapse = ", "), "\n", sep = "")
cat("Ecartees         : ",
    if (length(neg$absents)) paste(neg$absents, collapse = ", ") else "aucune",
    "\n", sep = "")

# ------------------------------------------------------------------------------
titre("3. Requete minimale (les 4 variables du prototype qui fonctionnait)")
# ------------------------------------------------------------------------------
# Ce jeu est la reference : s'il echoue, le probleme n'est pas le choix des
# variables mais l'acces au service lui-meme (reseau, proxy, pare-feu).
minimal <- c("T_Q", "TSUP_H_Q", "PRELIQ_Q", "PRENEI_Q")
req_min <- safran_requete(LON, LAT, ANNEE, minimal)
cat("URL complete :\n", req_min$url, "\n\n", sep = "")

rep_min <- tryCatch(httr2::req_perform(req_min),
                    error = function(e) {
                      cat("ECHEC RESEAU : ", conditionMessage(e), "\n"); NULL })

if (!is.null(rep_min)) {
  cat("Statut HTTP : ", httr2::resp_status(rep_min), "\n", sep = "")
  if (httr2::resp_status(rep_min) >= 400) {
    cat("Corps de l'erreur :\n",
        substr(httr2::resp_body_string(rep_min), 1, 800), "\n", sep = "")
  } else {
    res <- safran_parser(rep_min, ANNEE)
    if (is.null(res$erreur)) {
      cat("OK — ", nrow(res$donnees), " jours recus.\n", sep = "")
      cat("\nApercu :\n"); print(utils::head(res$donnees[, c("date", "t_moy",
                                                             "t_max", "p_liq",
                                                             "p_sol", "p_tot")], 3))
    } else {
      cat("Parsing en echec : ", res$erreur, "\n", sep = "")
    }
  }
}

# ------------------------------------------------------------------------------
titre("4. Requete complete (toutes les variables souhaitees)")
# ------------------------------------------------------------------------------
# Si l'etape 3 passe et pas celle-ci, c'est bien une variable de trop.
req_full <- safran_requete(LON, LAT, ANNEE, cfg_params_api())
cat("Variables : ", paste(cfg_params_api(), collapse = ", "), "\n\n", sep = "")

rep_full <- tryCatch(httr2::req_perform(req_full),
                     error = function(e) {
                       cat("ECHEC RESEAU : ", conditionMessage(e), "\n"); NULL })

if (!is.null(rep_full)) {
  cat("Statut HTTP : ", httr2::resp_status(rep_full), "\n", sep = "")
  if (httr2::resp_status(rep_full) >= 400) {
    cat("Corps de l'erreur :\n",
        substr(httr2::resp_body_string(rep_full), 1, 800), "\n", sep = "")
    cat("\n>> C'est la confirmation : une des variables demandees n'existe pas",
        "\n   dans cette collection. L'application les negocie desormais",
        "\n   automatiquement, elle fonctionnera malgre tout.\n")
  } else {
    cat("OK — la collection accepte l'ensemble des variables.\n")
  }
}

# ------------------------------------------------------------------------------
titre("5. Variable par variable")
# ------------------------------------------------------------------------------
# Identifie precisement laquelle pose probleme.
for (p in cfg_params_api()) {
  r <- tryCatch(httr2::req_perform(safran_requete(LON, LAT, ANNEE, p)),
                error = function(e) NULL)
  statut <- if (is.null(r)) "reseau KO" else as.character(httr2::resp_status(r))
  n_val <- NA
  if (!is.null(r) && httr2::resp_status(r) < 400) {
    res <- safran_parser(r, ANNEE)
    n_val <- if (is.null(res$erreur)) sum(!is.na(res$donnees$t_moy) |
                                            !is.na(res$donnees$p_tot)) else NA
  }
  cat(sprintf("  %-16s statut %-10s\n", p, statut))
}

# ------------------------------------------------------------------------------
titre("6. Chargement complet via l'application")
# ------------------------------------------------------------------------------
don <- safran_charger(LON, LAT, (ANNEE - 2):ANNEE,
                      progres = function(f, m) cat("   [", round(100 * f), "%] ",
                                                   m, "\n", sep = ""))
cat("\nLignes recues : ", nrow(don), "\n", sep = "")
err <- attr(don, "erreurs")
if (length(err)) {
  cat("\nMessages remontes :\n")
  cat(paste(" -", err), sep = "\n")
} else {
  cat("Aucun message d'erreur.\n")
}
if (nrow(don)) {
  cat("\nSource ETP : ", attr(don, "etp_source"), "\n", sep = "")
  cat("Colonnes renseignees :\n")
  for (v in names(CONFIG$variables)) {
    if (v %in% names(don)) {
      cat(sprintf("  %-8s %5.1f %% de valeurs\n", v,
                  100 * mean(!is.na(don[[v]]))))
    }
  }
}

titre("Diagnostic termine")
cat("Copiez l'integralite de cette sortie si vous devez la transmettre.\n")
