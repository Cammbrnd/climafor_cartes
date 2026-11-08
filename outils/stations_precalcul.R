# ==============================================================================
# stations_precalcul.R — CALCUL DES GRANDEURS ANNUELLES DES STATIONS METEO-FRANCE
# ------------------------------------------------------------------------------
# PRINCIPE. Comme pour les mailles (carte_precalcul.R), rien n'est recalcule a part :
# la serie journaliere d'une station est mise en forme par station_depuis_brut() — la
# fonction MEME de l'analyse d'une station — puis chaque grandeur est calculee par
# calculer_un_indicateur(), celle de l'application. Une valeur publiee est donc la valeur
# que l'onglet Bilan climatique afficherait pour cette station.
#
# CE QUI DIFFERE DES MAILLES.
#   - Une station a des LACUNES : une annee n'est retenue que si 95 % de ses jours sont
#     renseignes (CONFIG$stations$seuil_annee), variable par variable ; sinon elle est
#     ABSENTE, jamais comptee comme zero.
#   - Une station ne mesure pas tout : pas de SWI, de neige distincte, d'humidite. Un
#     pluviometre n'a pas de gel. Une grandeur qu'une station ne peut pas donner reste vide
#     pour elle (calculer_un_indicateur la refuse) ; elle n'est publiee que si au moins une
#     station la donne.
#   - Toutes les stations ne sont pas publiees : au moins CONFIG$stations$publie$annees_min
#     annees valides, et une derniere mesure de moins de inactive_max_j jours.
#
# DEUX ETAPES, comme pour les mailles, pour repartir le travail :
#   1. stations_construire()  traite des DEPARTEMENTS et ecrit une part (.rds) ;
#   2. stations_assembler()   reunit les parts, ecrit stations.json et s_<cle>.bin.gz, les
#                             relit avec la validation de l'application.
#
# La lecture des fichiers de Meteo-France est une fonction `source_dept(dept)` que l'on
# fournit (tableau brut de mf_lire_fichier(), toutes stations du departement) : le precalcul
# reel utilise stations_source_mf() ; les tests et la demonstration, une source fictive.
# ==============================================================================

STATIONS_PRECALCUL_DEPT_RE <- "^(0[1-9]|[1-8][0-9]|9[0-5]|2A|2B)$"

#' Departements metropolitains pour lesquels Meteo-France publie des fichiers.
#' @param index mf_index_fichiers() (injectable).
stations_departements <- function(index = mf_index_fichiers()) {
  d <- sort(unique(as.character(index$dept)))
  d[grepl(STATIONS_PRECALCUL_DEPT_RE, d)]
}

#' Source reelle : les fichiers departementaux de Meteo-France (data.gouv.fr), tous les
#' fichiers qui couvrent les annees demandees, toutes stations.
#' @param annees annees a couvrir.
#' @return function(dept) -> tableau brut (mf_lire_fichier) ou NULL.
stations_source_mf <- function(annees) {
  function(dept) {
    f <- mf_fichiers_dept(dept, annees = annees)
    if (!nrow(f)) return(NULL)
    morceaux <- lapply(seq_len(nrow(f)), function(i) {
      chemin <- mf_telecharger(f$url[i])
      on.exit(unlink(chemin), add = TRUE)
      mf_lire_fichier(chemin)
    })
    do.call(rbind, morceaux)
  }
}

#' Calcul d'un indicateur pour une station : calculer_un_indicateur() de l'application, qui
#' ecarte les annees lacunaires et refuse un indicateur dont la variable n'est pas mesuree.
#' @keywords internal
stations_calculer_indicateur <- function(source, don, ctx) {
  r <- calculer_un_indicateur(don, INDICATEURS[[source]], ctx)
  if (!is.data.frame(r) || !nrow(r)) NULL else r
}

#' Une ligne de la table publiee pour une station, ou NULL si elle ne se publie pas.
#' @param resume ligne de mf_resumer_stations() ; @param don tableau journalier de la station.
#' @keywords internal
stations_ligne <- function(resume, don, annees, maintenant = Sys.Date()) {
  cfg <- CONFIG$stations$publie
  v <- attr(don, "annees_valides")
  vp <- intersect(v$p, annees); vt <- intersect(v$t, annees)
  n_p <- length(vp); n_t <- length(vt)
  if (max(n_p, n_t) < cfg$annees_min) return(NULL)
  if (!is.finite(as.numeric(resume$derniere_obs)) ||
      as.numeric(as.Date(maintenant) - as.Date(resume$derniere_obs)) > cfg$inactive_max_j) return(NULL)
  data.frame(
    id = resume$id, nom = resume$nom, dept = resume$dept,
    lon = round(resume$lon, 4), lat = round(resume$lat, 4),
    alt = if (is.finite(resume$alt)) round(resume$alt) else NA_real_,
    derniere_obs = format(resume$derniere_obs, "%Y-%m-%d"),
    fr_p = round(resume$fr_p, 3), fr_t = round(resume$fr_t, 3),
    a_pluie = n_p > 0L, a_temp = n_t > 0L,
    premiere = min(c(vp, vt)), n_p = n_p, n_t = n_t,
    stringsAsFactors = FALSE)
}

#' Calcule une part : les departements `depts`.
#'
#' @param source_dept function(dept) -> tableau brut des stations du departement, ou NULL.
#' @param annees annees cartographiees (les annees COMPLETES : la derniere annee partielle
#'   n'est pas publiee, comme pour les mailles).
#' @param fichier si non NULL, la part est ecrite en .rds a cet endroit.
#' @param base entrees de la base precalculee d'une station (STATIONS_BASE_COUCHES), calculees avec
#'   les grandeurs mais publiees a part ; list() = pas de base.
#' @return la part (liste), de facon invisible : `stations` (table), `valeurs`, `normale`,
#'   `pente`, `signif` par grandeur, `echecs` (departements sans donnees).
stations_construire <- function(source_dept, depts, annees, couches = CARTE_COUCHES,
                                fichier = NULL, maintenant = Sys.Date(), silencieux = FALSE,
                                base = list()) {
  ctx <- carte_ctx(annees)
  na <- length(annees)
  couches <- c(couches, base)
  lignes <- list()
  vals <- lapply(couches, function(k) list())     # grandeur -> liste de colonnes (une par station)
  nor  <- lapply(couches, function(k) numeric(0))
  pen  <- lapply(couches, function(k) numeric(0))
  sig  <- lapply(couches, function(k) logical(0))
  echecs <- character(0)

  for (dept in depts) {
    brut <- tryCatch(source_dept(dept), error = function(e) {
      if (!silencieux) cat(sprintf("  departement %s : %s\n", dept, conditionMessage(e)))
      NULL
    })
    if (is.null(brut) || !is.data.frame(brut) || !nrow(brut)) { echecs <- c(echecs, dept); next }
    brut <- brut[!is.na(brut$date) & !is.na(brut$id), , drop = FALSE]
    # Resume recent (fraicheur, completude des 12 derniers mois) : sur les lignes dont les
    # codes qualite douteux sont ecartes, comme le fait la recherche en direct.
    resume <- mf_resumer_stations(mf_appliquer_qualite(brut), dept)
    par_id <- split(brut, brut$id)
    n_dept <- 0L
    for (i in seq_len(nrow(resume))) {
      r <- resume[i, ]
      b <- par_id[[r$id]]
      if (is.null(b) || !nrow(b)) next
      don <- tryCatch(
        station_depuis_brut(b, list(id = r$id, nom = r$nom, dept = dept, lat = r$lat, lon = r$lon, alt = r$alt),
                            r$lon, r$lat, annees),
        error = function(e) NULL)
      if (is.null(don) || !nrow(don)) next
      ligne <- stations_ligne(r, don, annees, maintenant)
      if (is.null(ligne)) next
      out <- carte_calculer_maille(don, ctx, annees, couches,
                                   calculer = stations_calculer_indicateur, garde_jours = FALSE)
      if (!any(vapply(out, function(o) any(is.finite(o$valeurs)), TRUE))) next
      lignes[[length(lignes) + 1L]] <- ligne
      for (cle in names(couches)) {
        vals[[cle]][[length(vals[[cle]]) + 1L]] <- out[[cle]]$valeurs
        nor[[cle]] <- c(nor[[cle]], out[[cle]]$normale)
        pen[[cle]] <- c(pen[[cle]], out[[cle]]$pente)
        sig[[cle]] <- c(sig[[cle]], out[[cle]]$signif)
      }
      n_dept <- n_dept + 1L
    }
    if (!silencieux) cat(sprintf("  departement %s : %d station(s) retenue(s) sur %d\n", dept, n_dept, nrow(resume)))
  }

  m <- length(lignes)
  part <- list(
    annees = annees, depts = depts, echecs = echecs,
    stations = if (m) do.call(rbind, lignes) else NULL,
    valeurs = stats::setNames(lapply(names(vals), function(cle) {
      if (m) do.call(cbind, vals[[cle]]) else matrix(NA_real_, base_nb_lignes(couches[[cle]], na), 0)
    }), names(vals)),
    normale = nor, pente = pen, signif = sig)
  if (!is.null(fichier)) saveRDS(part, fichier)
  invisible(part)
}

#' Reunit les parts et ecrit stations.json et s_<cle>.bin.gz.
#'
#' @param parts liste de parts (stations_construire) ou vecteur de chemins .rds.
#' @param source texte affiche a l'utilisateur (origine des donnees).
#' @param n_min nombre minimal de stations : en deca, la publication est refusee (un
#'   telechargement partiel de Meteo-France ne doit pas ecraser un jeu complet).
#' @param base entrees de la base precalculee (celles de stations_construire). Une entree
#'   qu'aucune station ne renseigne n'est pas ecrite : stations.json la nomme dans
#'   `base_parametres$vides`, et sa lecture rend des NA.
#' @return list(dossier, n, annees, couches, departements_sans_donnees, taille_octets).
stations_assembler <- function(parts, annees, source, dossier, couches = CARTE_COUCHES,
                               n_min = 1L, genere_le = format(Sys.Date()), base = list()) {
  if (is.character(parts)) parts <- lapply(parts, readRDS)
  cfg <- CONFIG$stations$publie
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  na <- length(annees)
  for (p in parts) {
    if (!identical(as.integer(p$annees), as.integer(annees))) {
      stop("Les parts ne portent pas les memes annees.", call. = FALSE)
    }
  }
  parts <- Filter(function(p) !is.null(p$stations) && nrow(p$stations) > 0L, parts)
  if (!length(parts)) stop("Aucune station dans les parts : publication refusee.", call. = FALSE)

  st <- do.call(rbind, lapply(parts, function(p) p$stations))
  garde <- !duplicated(st$id)
  st <- st[garde, , drop = FALSE]
  ordre <- order(st$id)
  if (nrow(st) < n_min) {
    stop(sprintf("%d station(s) seulement (%d attendues au moins) : publication refusee.", nrow(st), n_min),
         call. = FALSE)
  }
  if (nrow(st) > cfg$stations_max) {
    stop(sprintf("%d stations : plus que la limite de lecture (%d).", nrow(st), cfg$stations_max), call. = FALSE)
  }
  st$nom <- vapply(st$nom, cmp_nom_propre, "", defaut = "Station")

  publiees <- list()
  for (cle in names(couches)) {
    k <- couches[[cle]]
    v <- do.call(cbind, lapply(parts, function(p) p$valeurs[[cle]]))[, garde, drop = FALSE]
    nor <- unlist(lapply(parts, function(p) p$normale[[cle]]))[garde]
    pen <- unlist(lapply(parts, function(p) p$pente[[cle]]))[garde]
    sig <- unlist(lapply(parts, function(p) p$signif[[cle]]))[garde]
    v <- v[, ordre, drop = FALSE]; nor <- nor[ordre]; pen <- pen[ordre]; sig <- sig[ordre]
    # Une grandeur qu'aucune station ne donne n'est pas publiee.
    if (!any(is.finite(v))) next
    carte_ecrire_couche(dossier, k, nor, pen, sig, v, prefixe = "s")
    publiees[[length(publiees) + 1L]] <- list(
      cle = k$cle, fichier = sprintf("s_%s.bin.gz", k$cle), stock = k$stock,
      n_stations = sum(colSums(is.finite(v)) > 0L))
  }
  st <- st[ordre, , drop = FALSE]
  if (!length(publiees)) stop("Aucune grandeur calculable : publication refusee.", call. = FALSE)

  # La base : meme ecriture que les grandeurs, mais aucun usage cartographique ni meta de carte.
  meta_base <- list(); vides <- character(0)
  for (cle in names(base)) {
    k <- base[[cle]]
    v <- do.call(cbind, lapply(parts, function(p) p$valeurs[[cle]]))[, garde, drop = FALSE][, ordre, drop = FALSE]
    if (!any(is.finite(v))) { vides <- c(vides, cle); next }
    nan <- rep(NA_real_, ncol(v))
    carte_ecrire_couche(dossier, k, nan, nan, rep(FALSE, ncol(v)), v, prefixe = "s")
    meta_base[[length(meta_base) + 1L]] <- c(
      list(cle = k$cle, fichier = sprintf("s_%s.bin.gz", k$cle), type = k$type, stock = k$stock),
      if (identical(k$type, "ref")) list(lignes = k$lignes))
  }

  ctx <- carte_ctx(annees)
  meta <- list(
    schema = cfg$schema, annees = annees, n_stations = nrow(st),
    reference = ctx$ref_label, genere_le = genere_le, source = source,
    couches = publiees,
    stations = as.list(st[, STATIONS_PUBLIEES_COLONNES]),
    base = if (length(base)) meta_base,
    # Reglages dont depend la base : voir base_applicable() (src/utils_base.R).
    base_parametres = if (length(base)) list(empreinte = base_empreinte(ctx),
                                             attendues = as.list(names(base)), vides = as.list(vides)))
  jsonlite::write_json(meta, file.path(dossier, cfg$fichier), auto_unbox = TRUE, null = "null",
                       na = "null", digits = NA, pretty = FALSE)

  # Controle final : ce qui est ecrit doit passer la validation de l'application.
  lu <- stations_lire_meta_locale(dossier)
  if (is.null(lu$meta)) {
    stop(paste("Les fichiers de stations produits ne passent pas la validation :",
               paste(lu$erreurs, collapse = " ")), call. = FALSE)
  }
  fichiers <- list.files(dossier, pattern = "^(s_.*\\.bin\\.gz|stations\\.json)$", full.names = TRUE)
  list(dossier = dossier, n = nrow(st), annees = annees,
       couches = vapply(publiees, function(k) k$cle, ""),
       departements_sans_donnees = sort(unique(unlist(lapply(parts, function(p) p$echecs)))),
       taille_octets = sum(file.size(fichiers)))
}


# ------------------------------------------------------------------------------
# Relecture d'un jeu de stations ecrit sur disque (precalcul, assemblage, tests)
# ------------------------------------------------------------------------------

#' Lit et valide un jeu de stations dans un dossier local (precalcul, tests).
#' @return list(meta = liste ou NULL, erreurs).
#' @keywords internal
stations_lire_meta_locale <- function(dossier) {
  f <- file.path(dossier, CONFIG$stations$publie$fichier)
  if (!file.exists(f)) return(list(meta = NULL, erreurs = "Aucun jeu de stations installé."))
  if (isTRUE(file.size(f) > 8 * 1024^2)) return(list(meta = NULL, erreurs = "stations.json trop volumineux."))
  meta <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  v <- stations_meta_valider(meta)
  if (!v$ok) return(list(meta = NULL, erreurs = v$erreurs))
  fichiers <- vapply(c(meta$couches, meta[["base"]]), function(k) as.character(k$fichier)[1], "")
  absents <- fichiers[!file.exists(file.path(dossier, fichiers))]
  if (length(absents)) {
    return(list(meta = NULL, erreurs = sprintf("Fichier(s) manquant(s) : %s.", paste(absents, collapse = ", "))))
  }
  list(meta = meta, erreurs = character(0))
}
