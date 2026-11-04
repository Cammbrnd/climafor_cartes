# ==============================================================================
# carte_diagnostic_zone.R — L'API GeoSAS ACCEPTE-T-ELLE LES REQUETES DE ZONE ?
# ------------------------------------------------------------------------------
# Question : pour precalculer les indicateurs sur ~9 900 mailles SAFRAN, peut-on
# demander plusieurs mailles en UNE requete (zone / cube) au lieu de une requete
# par maille et par annee (~670 000 requetes) ?
#
# A lancer depuis la RACINE du projet ClimaFor (ou par le workflow GitHub « Diagnostic GeoSAS ») :
#
#     source("outils/carte_diagnostic_zone.R")
#
# Le script ne modifie rien. Il envoie quelques requetes de petite taille,
# espacees d'une seconde (usage poli du service), et affiche pour chacune le
# statut, la duree, le poids et le nombre de mailles recues.
#
# Point de test : Toulouse (1,2-1,6 E ; 43,4-43,8 N), parametre T_Q, janvier 2020.
# ==============================================================================

PARAM  <- "T_Q"
LON0   <- 1.2
LAT0   <- 43.4
PAUSE  <- 1          # secondes entre deux requetes
TAILLES <- c(0.4, 1, 2, 4)   # cote de la zone en degres, pour la montee en charge

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
source(ci("src/utils_cache.R"), encoding = "UTF-8")
source(ci("src/utils_api.R"), encoding = "UTF-8")

titre <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n",
                         strrep("=", 78), "\n", sep = "")

url_collection <- sub("/position/?$", "", CONFIG$api$edr_base)

# Compte ce que contient une reponse CoverageJSON, sans rien supposer de sa forme.
resumer_coveragejson <- function(j) {
  out <- list(type = j$type %||% "?", domaine = "?", axes = "?",
              positions = NA_integer_, pas_de_temps = NA_integer_)
  dom <- j$domain
  n_couvertures <- 1L
  if (is.null(dom) && length(j$coverages)) {          # CoverageCollection
    n_couvertures <- length(j$coverages)
    dom <- j$coverages[[1]]$domain
    out$type <- paste0(out$type, " (", n_couvertures, " couvertures)")
  }
  if (!is.null(dom)) {
    out$domaine <- dom$domainType %||% "?"
    ax <- dom$axes
    out$axes <- paste(names(ax), collapse = ",")
    nb <- function(a) if (is.null(a)) NA_integer_ else
      as.integer(a$num %||% length(a$values))
    # Mailles PAR couverture : grille x*y, serie composite, ou point unique.
    par_couverture <- if (!is.null(ax$x) && !is.null(ax$y)) {
      nb(ax$x) * nb(ax$y)
    } else if (!is.null(ax$composite)) {
      as.integer(length(ax$composite$values))
    } else {
      1L
    }
    out$positions <- as.integer(n_couvertures * par_couverture)
    out$pas_de_temps <- nb(ax$t)
  }
  out
}

# Une requete chronometree. Ne leve jamais d'erreur.
essai <- function(etiquette, suffixe, params, timeout = 120) {
  cat(sprintf("\n-- %s\n", etiquette))
  req <- requete_avec_params(paste0(url_collection, "/", suffixe), params, timeout)
  req <- httr2::req_user_agent(req, sprintf("%s/%s diagnostic_zone (%s)",
    CONFIG$app$nom, CONFIG$app$version, CONFIG$app$organisme))
  req <- httr2::req_error(req, is_error = function(r) FALSE)
  cat("   URL : ", req$url, "\n", sep = "")
  t0 <- Sys.time()
  rep <- tryCatch(httr2::req_perform(req), error = function(e) {
    cat("   ECHEC RESEAU : ", conditionMessage(e), "\n", sep = ""); NULL })
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  Sys.sleep(PAUSE)
  if (is.null(rep)) return(list(ok = FALSE, secondes = dt))
  st <- httr2::resp_status(rep)
  octets <- length(httr2::resp_body_raw(rep))
  cat(sprintf("   statut %d  |  %.1f s  |  %.0f Ko\n", st, dt, octets / 1024))
  if (st < 400 && octets < 200) {     # constate : area repond 200 avec un corps vide
    cat("   >> Reponse VIDE malgre le statut ", st, " (inutilisable).\n", sep = "")
    return(list(ok = FALSE, statut = st, secondes = dt, octets = octets))
  }
  if (st >= 400) {
    cat("   Corps : ", substr(httr2::resp_body_string(rep), 1, 400), "\n", sep = "")
    return(list(ok = FALSE, statut = st, secondes = dt, octets = octets))
  }
  j <- tryCatch(httr2::resp_body_json(rep, simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(j)) {
    cat("   Reponse recue mais illisible en JSON.\n")
    return(list(ok = FALSE, statut = st, secondes = dt, octets = octets))
  }
  r <- resumer_coveragejson(j)
  cat(sprintf("   %s | domaine %s | axes %s\n   mailles : %s | pas de temps : %s\n",
              r$type, r$domaine, r$axes, r$positions, r$pas_de_temps))
  c(list(ok = TRUE, statut = st, secondes = dt, octets = octets, json = j), r)
}

# Decrit un axe CoverageJSON : valeurs explicites ou axe regulier (debut/fin/nombre).
decrire_axe <- function(a) {
  if (is.null(a)) return("absent")
  if (!is.null(a$values)) {
    v <- unlist(a$values)
    pas <- if (is.numeric(v) && length(v) > 1) sprintf(" | pas %.5f", (v[length(v)] - v[1]) / (length(v) - 1)) else ""
    return(sprintf("%d valeurs : %s ... %s%s", length(v), format(v[1]), format(v[length(v)]), pas))
  }
  n <- as.numeric(a$num %||% NA)
  pas <- if (!is.na(n) && n > 1) sprintf(" | pas %.5f", (as.numeric(a$stop) - as.numeric(a$start)) / (n - 1)) else ""
  sprintf("regulier : %s ... %s (%s valeurs)%s", a$start, a$stop, a$num, pas)
}

# Ce que la reponse dit de sa grille : axes x/y, systeme de coordonnees, parametres.
decrire_grille <- function(j) {
  dom <- j$domain %||% j$coverages[[1]]$domain
  if (is.null(dom)) return(invisible(NULL))
  cat("   Axe x : ", decrire_axe(dom$axes$x), "\n", sep = "")
  cat("   Axe y : ", decrire_axe(dom$axes$y), "\n", sep = "")
  for (r in dom$referencing %||% list()) {
    cat("   Reference : ", paste(unlist(r$coordinates), collapse = ","), " -> ",
        r$system$id %||% r$system$type %||% "?", "\n", sep = "")
  }
  rg <- j$ranges %||% j$coverages[[1]]$ranges
  cat("   Parametres recus : ", paste(names(rg), collapse = ", "), "\n", sep = "")
  invisible(NULL)
}

zone_wkt <- function(cote) {
  l1 <- LON0; l2 <- LON0 + cote; a1 <- LAT0; a2 <- LAT0 + cote
  sprintf("POLYGON((%s %s,%s %s,%s %s,%s %s,%s %s))",
          fmt_coord(l1), fmt_coord(a1), fmt_coord(l2), fmt_coord(a1),
          fmt_coord(l2), fmt_coord(a2), fmt_coord(l1), fmt_coord(a2),
          fmt_coord(l1), fmt_coord(a1))
}
zone_bbox <- function(cote) paste(fmt_coord(LON0), fmt_coord(LAT0),
                                  fmt_coord(LON0 + cote), fmt_coord(LAT0 + cote), sep = ",")

# ------------------------------------------------------------------------------
titre("1. Ce que la collection declare supporter")
# ------------------------------------------------------------------------------
cat("URL : ", url_collection, "\n", sep = "")
rep_col <- tryCatch(
  httr2::req_perform(httr2::req_error(
    httr2::req_timeout(httr2::request(url_collection), 25),
    is_error = function(r) FALSE)),
  error = function(e) { cat("ECHEC RESEAU : ", conditionMessage(e), "\n"); NULL })

dq <- NULL
if (!is.null(rep_col) && httr2::resp_status(rep_col) < 400) {
  j <- httr2::resp_body_json(rep_col, simplifyVector = FALSE)
  dq <- names(j$data_queries %||% j$`data-queries`)
  cat("Requetes de donnees declarees (data_queries) : ",
      if (length(dq)) paste(dq, collapse = ", ") else "aucune information", "\n", sep = "")
  bb <- j$extent$spatial$bbox
  if (length(bb)) cat("Emprise spatiale : ", paste(unlist(bb[[1]]), collapse = ", "), "\n", sep = "")
  fmts <- unlist(j$output_formats %||% j$`output-formats`)
  if (length(fmts)) cat("Formats de sortie : ", paste(fmts, collapse = ", "), "\n", sep = "")
} else if (!is.null(rep_col)) {
  cat("Statut HTTP ", httr2::resp_status(rep_col), "\n", sep = "")
}

# ------------------------------------------------------------------------------
titre("2. Requete de zone (area) et de cube, petite emprise")
# ------------------------------------------------------------------------------
base <- list(`parameter-name` = PARAM, crs = CONFIG$api$edr_crs,
             datetime = "2020-01-01/2020-01-31", f = CONFIG$api$edr_format)

r_area <- essai("area (polygone 0,4 deg)", "area",
                c(list(coords = zone_wkt(0.4)), base))
r_cube <- essai("cube (bbox 0,4 deg)", "cube",
                c(list(bbox = zone_bbox(0.4)), base))
if (isTRUE(r_cube$ok)) {
  cat("\nGrille renvoyee par le cube (les mailles reelles ? coordonnees natives ?) :\n")
  decrire_grille(r_cube$json)
}

# ------------------------------------------------------------------------------
titre("3. Montee en charge (une annee entiere, un parametre)")
# ------------------------------------------------------------------------------
# Le cube est prefere : il renvoie une vraie grille (area ne renvoie qu'une serie,
# et des corps vides au-dela de ~1 deg).
retenu <- if (isTRUE(r_cube$ok)) "cube" else if (isTRUE(r_area$ok)) "area" else NULL
tab <- NULL
if (is.null(retenu)) {
  cat("Ni area ni cube n'ont repondu correctement : etape sautee.\n")
} else {
  annee <- modifyList(base, list(datetime = "2020-01-01/2020-12-31"))
  for (cote in TAILLES) {
    par <- if (retenu == "area") c(list(coords = zone_wkt(cote)), annee)
           else c(list(bbox = zone_bbox(cote)), annee)
    r <- essai(sprintf("%s, %.1f deg, annee 2020", retenu, cote), retenu, par, timeout = 180)
    tab <- rbind(tab, data.frame(cote_deg = cote, ok = isTRUE(r$ok),
      mailles = r$positions %||% NA, secondes = round(r$secondes %||% NA, 1),
      ko = round((r$octets %||% NA) / 1024)))
    if (!isTRUE(r$ok) || (r$secondes %||% 0) > 60) {
      cat("   >> Arret de la montee en charge (echec ou plus de 60 s).\n"); break
    }
  }
  cat("\nRecapitulatif :\n"); print(tab, row.names = FALSE)
  ok <- tab[tab$ok & !is.na(tab$mailles) & tab$mailles > 0, ]
  if (nrow(ok)) {
    d <- ok[nrow(ok), ]
    cat(sprintf(paste0("\nOrdre de grandeur (derniere taille reussie) : %.1f mailles par seconde.\n",
                       "Pour 9 892 mailles, un parametre, une annee : environ %.0f s si cela se maintient.\n"),
                d$mailles / d$secondes, 9892 / (d$mailles / d$secondes)))
  }

  # --- 3b. Plusieurs parametres dans une seule requete ? (boite de 1 deg, annee 2020)
  titre("3b. Plusieurs parametres en une requete (cube 1 deg, annee 2020)")
  tout <- c("T_Q", "PRELIQ_Q", "ETP_Q")
  r3 <- essai("cube 1 deg, 3 parametres", "cube",
              c(list(bbox = zone_bbox(1)),
                modifyList(annee, list(`parameter-name` = paste(tout, collapse = ",")))),
              timeout = 180)
  if (isTRUE(r3$ok)) decrire_grille(r3$json)

  # --- 3c. Plusieurs annees en une requete ? (boite de 1 deg, 2000-2020, un parametre)
  titre("3c. Periode longue (cube 1 deg, 2000-2020, un parametre)")
  r4 <- essai("cube 1 deg, 21 ans", "cube",
              c(list(bbox = zone_bbox(1)),
                modifyList(annee, list(datetime = "2000-01-01/2020-12-31"))),
              timeout = 240)
  if (isTRUE(r4$ok)) {
    cat(sprintf("   => %.0f mailles x %s jours en %.1f s (%.0f Ko)\n",
                r4$positions, r4$pas_de_temps, r4$secondes, r4$octets / 1024))
  }
}

# ------------------------------------------------------------------------------
titre("4. Piste de repli : fichiers SIM en masse sur data.gouv.fr (exploration)")
# ------------------------------------------------------------------------------
rep_dg <- tryCatch(
  httr2::req_perform(httr2::req_error(httr2::req_timeout(httr2::request(
    "https://www.data.gouv.fr/api/1/datasets/?q=SIM%20quotidien&page_size=6"), 30),
    is_error = function(r) FALSE)),
  error = function(e) { cat("ECHEC RESEAU : ", conditionMessage(e), "\n"); NULL })
if (!is.null(rep_dg) && httr2::resp_status(rep_dg) < 400) {
  d <- httr2::resp_body_json(rep_dg, simplifyVector = FALSE)$data
  for (x in d) cat(sprintf(" - %s\n   id %s | %d ressources | modifie %s\n",
                           x$title, x$id, length(x$resources), substr(x$last_modified %||% "?", 1, 10)))
  cat("\nAucun fichier n'est telecharge ici : il s'agit seulement de reperer le jeu de donnees.\n")
} else if (!is.null(rep_dg)) {
  cat("Statut HTTP ", httr2::resp_status(rep_dg), "\n", sep = "")
}

titre("Diagnostic termine")
cat("Copiez l'integralite de cette sortie.\n")
