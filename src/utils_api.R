# ==============================================================================
# utils_api.R — COUCHE D'ACCES AUX DONNEES
# ------------------------------------------------------------------------------
# Responsabilite unique : aller chercher la donnee brute et la rendre sous forme
# d'un tableau journalier normalise. Aucun calcul d'indicateur ici.
#
# DEUX PRINCIPES, appris a nos depens :
#
#   1. NEGOCIER LES VARIABLES. Une collection EDR n'expose pas forcement tous
#      les parametres SIM2. Demander un parametre absent fait echouer TOUTE la
#      requete, pas seulement ce parametre. On interroge donc d'abord les
#      metadonnees de la collection et on ne demande que l'intersection.
#
#   2. NE JAMAIS AVALER UNE ERREUR. Chaque echec est collecte avec son code
#      HTTP et le debut du corps de reponse, puis attache au resultat sous
#      l'attribut "erreurs". L'interface les affiche. Un "aucune donnee" muet
#      est indiagnosticable, pour l'utilisateur comme pour le mainteneur.
#
# Ce contrat est aussi celui de station_charger() (utils_stations.R) : les
# indicateurs ne savent pas d'ou vient la donnee. L'attribut "source" vaut
# "safran" ou "station".
#
# Contrat de sortie de safran_charger() — un data.frame avec les colonnes :
#   date (Date), annee, mois, jour, doy (int)
#   t_moy, t_min, t_max (degC) ; p_liq, p_sol, p_tot (mm)
#   etp (mm), swi (-), neig_h (m), neig_e (mm)
# Les variables non exposees par la collection sont presentes mais a NA.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Utilitaires generaux
# ------------------------------------------------------------------------------

#' Operateur "valeur par defaut si NULL".
#' Defini en tete de fichier car utilise par tout ce qui suit.
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Convertit un element JSON (vecteur OU liste contenant des NULL) en numerique.
#' L'API renvoie `null` pour les valeurs manquantes ; jsonlite les transforme
#' alors en liste, ce qui casse toute arithmetique directe.
#' @keywords internal
as_num_json <- function(x, n = NULL) {
  if (is.null(x) || length(x) == 0) {
    return(if (is.null(n)) numeric(0) else rep(NA_real_, n))
  }
  if (is.list(x)) {
    x[vapply(x, is.null, logical(1))] <- NA
    x <- unlist(x, use.names = FALSE)
  }
  suppressWarnings(as.numeric(x))
}

#' Corrige l'horodatage non conforme renvoye par la collection
#' ("2020-01-01T00-00-00Z" au lieu de "2020-01-01T00:00:00Z").
#' @keywords internal
corriger_horodatage <- function(x) sub("T00-00-00Z$", "T00:00:00Z", x)

#' Formate une coordonnee sans notation scientifique ni decimales parasites.
#' sprintf("%s", 3.0541) peut produire "3.05410000000001" selon la plateforme,
#' ce que le parseur WKT du serveur peut refuser.
#' @keywords internal
fmt_coord <- function(x) {
  format(round(as.numeric(x), 6), scientific = FALSE, trim = TRUE)
}

#' Construit une requete httr2 en ajoutant les parametres par do.call
#' (evite la dependance au splicing rlang).
#' @keywords internal
requete_avec_params <- function(url, params, timeout = 30) {
  req <- do.call(httr2::req_url_query,
                 c(list(.req = httr2::request(url)), params))
  httr2::req_timeout(req, timeout)
}


# ------------------------------------------------------------------------------
# 1. Cache disque
# ------------------------------------------------------------------------------

#' Le cache disque est-il actif POUR CETTE SESSION ?
#'
#' Audit, C2 : l'option « Utiliser le cache local » modifiait `CONFIG` avec `<<-`,
#' c'est-a-dire la configuration de tout le processus : un utilisateur qui la
#' decochait la desactivait pour toutes les sessions servies par ce processus.
#' Le choix est maintenant garde dans `session$userData` (voir mod_parametres.R) ;
#' on le lit par la session courante (`getDefaultReactiveDomain()`), ce qui evite
#' de le faire passer en argument dans toute la chaine d'appels (SAFRAN, stations).
#' Hors de toute session (tests, scripts), c'est `CONFIG$cache$actif` qui decide.
#'
#' @param session session Shiny ; par defaut, celle qui execute le code.
#' @keywords internal
cache_est_actif <- function(session = shiny::getDefaultReactiveDomain()) {
  choix <- if (!is.null(session)) session$userData$cache_actif else NULL
  if (is.null(choix)) isTRUE(CONFIG$cache$actif) else isTRUE(choix)
}

#' Chemin du fichier de cache d'une annee SAFRAN.
#'
#' Le numero de schema est dans le NOM : une entree ecrite par une version
#' anterieure du format n'est tout simplement jamais retrouvee, au lieu d'etre
#' lue puis mal interpretee (CONFIG$cache$schema). Le nom est assaini
#' (cache_nom_sur) : il ne peut contenir que des chiffres, des points, des tirets
#' et des soulignements.
#' @keywords internal
cache_chemin <- function(lon, lat, annee) {
  p <- CONFIG$cache$precision
  nom <- sprintf("safran_%s_%s_%s_s%d%s",
                 cache_nom_sur(round(lat, p)), cache_nom_sur(round(lon, p)),
                 cache_nom_sur(as.integer(annee)),
                 as.integer(CONFIG$cache$schema %||% 1L), CACHE_EXT)
  file.path(CONFIG$cache$repertoire, nom)
}

#' @keywords internal
cache_lire <- function(lon, lat, annee) {
  if (!cache_est_actif()) return(NULL)
  f <- cache_chemin(lon, lat, annee)
  if (!file.exists(f)) return(NULL)
  age_j <- as.numeric(difftime(Sys.time(), file.mtime(f), units = "days"))
  if (!is.finite(age_j) || age_j > CONFIG$cache$duree_j) return(NULL)
  cache_lire_table(f)
}

#' Ecrit une annee en cache. Echoue silencieusement : sur certains hebergements
#' le systeme de fichiers est en lecture seule, ce n'est pas une raison de
#' faire echouer l'analyse. Le format est un tableau « sans code » controle a la
#' relecture (utils_cache.R) ; le dossier est purge au plus toutes les
#' `purge_intervalle_min` minutes.
#' @keywords internal
cache_ecrire <- function(lon, lat, annee, df) {
  if (!cache_est_actif() || is.null(df) || !nrow(df)) {
    return(invisible(FALSE))
  }
  tryCatch({
    cache_ecrire_table(df, cache_chemin(lon, lat, annee))
    cache_purger_si_besoin()
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}


#' Ce qu'il faudra telecharger pour une serie SAFRAN, et pour combien de temps.
#'
#' Audit, S4 : jusqu'a 68 requetes annuelles partaient sans que l'utilisateur
#' sache a quoi s'attendre, hormis une phrase generale dans l'onglet Parametres.
#' Cette fonction compte, sans aucun acces reseau, les annees deja presentes
#' dans le cache disque (memes regles que cache_lire : cache actif, schema
#' courant, entree plus recente que `duree_j`) ; l'annee en cours n'est jamais
#' mise en cache, elle est donc toujours a telecharger.
#'
#' LA DUREE EST UNE ESTIMATION, PAS UNE MESURE : un lot de `n_paralleles`
#' requetes est suppose durer `CONFIG$api$duree_lot_s` secondes. Le texte affiche
#' ne la presente donc jamais comme une valeur precise.
#'
#' @param annees vecteur d'annees.
#' @return liste(n_total, n_cache, n_telecharger, duree_s).
#' @export
estimer_chargement <- function(lon, lat, annees) {
  annees <- sort(unique(as.integer(annees)))
  annees <- annees[is.finite(annees)]
  n_total <- length(annees)
  en_cache <- vapply(annees, function(a) {
    if (!cache_est_actif()) return(FALSE)
    f <- cache_chemin(lon, lat, a)
    if (!file.exists(f)) return(FALSE)
    age_j <- as.numeric(difftime(Sys.time(), file.mtime(f), units = "days"))
    isTRUE(age_j <= CONFIG$cache$duree_j)
  }, logical(1))
  n_cache <- sum(en_cache)
  n_tel <- n_total - n_cache
  lots <- ceiling(n_tel / max(1L, CONFIG$api$n_paralleles))
  list(n_total = n_total, n_cache = n_cache, n_telecharger = n_tel,
       duree_s = lots * CONFIG$api$duree_lot_s)
}

#' Duree annoncee en mots : un ordre de grandeur, jamais une seconde pres.
#' @keywords internal
phrase_duree <- function(s) {
  if (!is.finite(s) || s < 15) return("quelques secondes")
  if (s < 90) return("environ une minute")
  sprintf("environ %d minutes", as.integer(round(s / 60)))
}


# ------------------------------------------------------------------------------
# 2. Negociation des variables exposees par la collection
# ------------------------------------------------------------------------------

# Memoire de session : la liste des parametres ne change pas en cours d'usage.
.ETAT_API <- new.env(parent = emptyenv())

#' Interroge les metadonnees de la collection EDR et retourne les noms de
#' parametres reellement exposes.
#'
#' @param forcer TRUE pour ignorer la valeur memorisee.
#' @return vecteur de noms (ex. "T_Q", "PRELIQ_Q", ...), ou NULL si les
#'   metadonnees sont inaccessibles.
#' @export
safran_parametres_collection <- function(forcer = FALSE) {

  if (!forcer && !is.null(.ETAT_API$params)) return(.ETAT_API$params)

  # L'URL de la collection est celle du point d'entree /position, amputee du
  # dernier segment.
  url_collection <- sub("/position/?$", "", CONFIG$api$edr_base)

  res <- tryCatch({
    rep <- httr2::req_perform(
      httr2::req_error(
        httr2::req_timeout(httr2::request(url_collection), 25),
        is_error = function(r) FALSE))
    if (httr2::resp_status(rep) >= 400) return(NULL)
    httr2::resp_body_json(rep, simplifyVector = FALSE)
  }, error = function(e) NULL)

  if (is.null(res)) return(NULL)

  # Selon les implementations, la cle est parameter_names ou parameter-names.
  pn <- res$parameter_names %||% res$`parameter-names`
  if (is.null(pn) || !length(pn)) return(NULL)

  noms <- names(pn)
  if (is.null(noms) || !length(noms)) return(NULL)

  .ETAT_API$params <- noms
  noms
}

#' Determine les parametres a demander : intersection entre ce que
#' l'application sait exploiter et ce que la collection expose.
#'
#' @return liste : $api (noms a demander), $absents (non exposes),
#'   $source ("metadonnees" ou "repli").
#' @export
safran_parametres_utiles <- function() {

  souhaites   <- cfg_params_api()
  obligatoire <- vapply(CONFIG$variables, function(v) isTRUE(v$obligatoire),
                        logical(1))
  minimum     <- vapply(CONFIG$variables[obligatoire], function(v) v$api,
                        character(1), USE.NAMES = FALSE)

  disponibles <- safran_parametres_collection()

  if (is.null(disponibles)) {
    # Metadonnees inaccessibles : on se limite au jeu minimal, celui dont on
    # sait par l'usage qu'il fonctionne.
    return(list(api = minimum, absents = setdiff(souhaites, minimum),
                source = "repli"))
  }

  retenus <- intersect(souhaites, disponibles)
  if (!length(retenus)) retenus <- minimum

  list(api = retenus, absents = setdiff(souhaites, disponibles),
       source = "metadonnees")
}


# ------------------------------------------------------------------------------
# 3. Requete et lecture d'une annee
# ------------------------------------------------------------------------------

#' Construit la requete pour une annee de donnees SAFRAN en un point.
#' @keywords internal
safran_requete <- function(lon, lat, annee, params_api) {
  req <- requete_avec_params(
    CONFIG$api$edr_base,
    list(
      `parameter-name` = paste(params_api, collapse = ","),
      coords           = sprintf("POINT(%s %s)", fmt_coord(lon), fmt_coord(lat)),
      crs              = CONFIG$api$edr_crs,
      datetime         = sprintf("%d-01-01/%d-12-31", annee, annee),
      f                = CONFIG$api$edr_format
    ),
    timeout = CONFIG$api$timeout_s)

  req <- httr2::req_retry(req, max_tries = CONFIG$api$n_tentatives + 1)
  req <- httr2::req_user_agent(req, sprintf(
    "%s/%s (%s)", CONFIG$app$nom, CONFIG$app$version, CONFIG$app$organisme))
  # On intercepte nous-memes les codes d'erreur pour pouvoir lire le corps de
  # la reponse, qui nomme souvent le parametre fautif.
  httr2::req_error(req, is_error = function(resp) FALSE)
}

#' Pression de vapeur saturante, en kPa (FAO-56, equation 11).
#' @param t temperature de l'air, degC.
#' @export
pression_saturante <- function(t) 0.6108 * exp(17.27 * t / (t + 237.3))

#' Deficit de pression de vapeur journalier, en kPa (FAO-56).
#'
#'   es  = [es(Tmax) + es(Tmin)] / 2        saturation du jour      (eq. 12)
#'   ea  = es * HU / 100                    pression reelle         (eq. 19)
#'   VPD = es - ea = es * (1 - HU / 100)
#'
#' POURQUOI LA MOYENNE DES DEUX EXTREMES, POUR es COMME POUR ea
#' ------------------------------------------------------------
#' es croit de facon EXPONENTIELLE avec la temperature : calculee sur la seule
#' temperature moyenne, elle serait sous-estimee, d'autant plus que l'amplitude
#' thermique est forte. La FAO-56 (eq. 19) applique l'humidite relative moyenne
#' a CETTE saturation-la. Jusqu'a la v1.4, es etait bien calculee sur les
#' extremes, mais ea sur es(Tmoy) : ea etait trop faible, donc le VPD trop fort
#' de 5 a 20 % (audit v1.1, M1). Exemple de la FAO-56 : Tn 18, Tx 25 degC,
#' HU 68 % donne es = 2,616 kPa, ea = 1,779 kPa.
#'
#' LIMITE ASSUMEE : HU_Q est une moyenne journaliere. Le VPD obtenu est un VPD
#' MOYEN, plus faible que celui du milieu d'apres-midi, que subit reellement la
#' plante (voir le seuil de VPD, CONFIG$seuils$vpd_stress_kpa). Les niveaux ne
#' sont pas comparables a ceux d'une station horaire ; les ecarts entre annees,
#' eux, le sont.
#'
#' @param t_min,t_max temperatures extremes du jour, degC.
#' @param hu humidite relative moyenne du jour, %.
#' @return vecteur numerique en kPa, jamais negatif (une humidite de plus de
#'   100 % donne 0), NA si une entree manque.
#' @export
vpd_fao56 <- function(t_min, t_max, hu) {
  es <- (pression_saturante(t_max) + pression_saturante(t_min)) / 2
  pmax(es * (1 - hu / 100), 0)
}

#' Colonnes DERIVEES du tableau SAFRAN : precipitations totales et VPD.
#'
#' Calculees a partir des colonnes brutes, a CHAQUE lecture, y compris pour les
#' annees servies par le cache. Ce n'est pas un detail : si ces colonnes etaient
#' figees dans le cache, corriger une formule ne changerait rien pour les annees
#' deja telechargees pendant toute la duree de validite du cache (30 jours), et
#' deux annees d'une meme serie pourraient avoir ete calculees avec deux formules
#' differentes. Les ecrire dans le cache reste possible (cela ne coute rien) mais
#' elles y sont toujours ECRASEES au chargement.
#'
#' @param df tableau journalier avec p_liq, p_sol, t_min, t_max, hu.
#' @return le meme tableau, colonnes p_tot et vpd recalculees.
#' @export
safran_deriver <- function(df) {
  # -- REGLE METIER CENTRALE -------------------------------------------------
  # Les precipitations totales sont TOUJOURS la somme du liquide et du solide.
  # SAFRAN fournit PRELIQ_Q (pluie) et PRENEI_Q (neige) separement : n'utiliser
  # que PRELIQ_Q sous-estimerait fortement les cumuls en montagne et en hiver.
  df$p_tot <- rowSums(cbind(df$p_liq, df$p_sol), na.rm = TRUE)
  vide <- is.na(df$p_liq) & is.na(df$p_sol)
  df$p_tot[vide] <- NA_real_

  # -- DEFICIT DE PRESSION DE VAPEUR (VPD), en kPa ---------------------------
  # Derive ici et non dans utils_calc.R, au meme titre que p_tot : c'est une
  # grandeur JOURNALIERE, elle appartient au tableau de base et doit etre
  # disponible pour toute agregation ulterieure. Formule : vpd_fao56().
  df$vpd <- vpd_fao56(df$t_min, df$t_max, df$hu)

  df
}

#' Transforme une reponse CoverageJSON en data.frame journalier normalise.
#' @return liste : $donnees (data.frame ou NULL), $erreur (chaine ou NULL).
#' @keywords internal
safran_parser <- function(reponse, annee = NA) {

  ko <- function(msg) list(donnees = NULL, erreur = sprintf("%s : %s", annee, msg))

  if (is.null(reponse)) return(ko("aucune reponse du serveur"))

  statut <- httr2::resp_status(reponse)
  if (statut >= 400) {
    # Le corps d'erreur EDR nomme generalement le parametre ou l'argument
    # refuse : c'est l'information la plus utile, on la remonte.
    detail <- tryCatch(
      substr(gsub("\\s+", " ", httr2::resp_body_string(reponse)), 1, 300),
      error = function(e) "")
    return(ko(sprintf("HTTP %d%s", statut,
                      if (nzchar(detail)) paste0(" — ", detail) else "")))
  }

  j <- tryCatch(httr2::resp_body_json(reponse, simplifyVector = TRUE),
                error = function(e) NULL)
  if (is.null(j)) return(ko("reponse illisible (JSON invalide)"))

  t_vals <- j$domain$axes$t$values
  if (is.null(t_vals) || length(t_vals) == 0) {
    return(ko("aucun pas de temps renvoye (point probablement hors emprise)"))
  }

  dates <- as.Date(substr(corriger_horodatage(
    unlist(t_vals, use.names = FALSE)), 1, 10))
  n <- length(dates)

  # Extraction par nom interne ; une variable absente du flux devient NA, ce
  # qui n'empeche pas les autres indicateurs de fonctionner.
  corresp <- cfg_table_variables()          # nom API -> nom interne
  cols <- lapply(names(corresp), function(nom_api) {
    as_num_json(j$ranges[[nom_api]]$values, n = n)
  })
  names(cols) <- unname(corresp)
  cols <- lapply(cols, function(v) if (length(v) == n) v else rep(NA_real_, n))

  df <- data.frame(date = dates, cols, stringsAsFactors = FALSE)

  lt       <- as.POSIXlt(df$date)
  df$annee <- lt$year + 1900L
  df$mois  <- lt$mon + 1L
  df$jour  <- lt$mday
  df$doy   <- lt$yday + 1L

  df <- safran_deriver(df)

  if (all(is.na(df$t_moy)) && all(is.na(df$p_tot))) {
    return(ko("pas de temps presents mais toutes les valeurs sont vides"))
  }

  list(donnees = df[order(df$date), , drop = FALSE], erreur = NULL)
}


# ------------------------------------------------------------------------------
# 4. Chargement d'une plage d'annees
# ------------------------------------------------------------------------------

#' Charge la serie journaliere SAFRAN pour un point et une plage d'annees.
#'
#' @param lon,lat Coordonnees WGS84 (EPSG:4326), degres decimaux.
#' @param annees Vecteur d'annees entieres.
#' @param progres Fonction optionnelle `progres(fraction, message)`.
#' @return data.frame journalier normalise, avec les attributs "erreurs",
#'   "parametres", "absents", "etp_source", "point", "extrait". Un data.frame
#'   vide signifie qu'aucune annee n'a pu etre recuperee : les messages sont
#'   alors dans l'attribut "erreurs".
#' @export
safran_charger <- function(lon, lat, annees, progres = NULL) {

  stopifnot(is.numeric(lon), is.numeric(lat), length(annees) > 0)
  annees <- sort(unique(as.integer(annees)))
  notifier <- function(f, m) if (is.function(progres)) progres(f, m)
  erreurs <- character(0)

  # -- 1) Negociation des variables ----------------------------------------
  notifier(0.02, "Lecture des variables disponibles...")
  neg <- safran_parametres_utiles()
  if (identical(neg$source, "repli")) {
    erreurs <- c(erreurs, paste(
      "Metadonnees de la collection inaccessibles : la requete a ete limitee",
      "aux variables essentielles."))
  }
  if (length(neg$absents)) {
    erreurs <- c(erreurs, sprintf(
      "Variables non exposees par la collection, ignorees : %s.",
      paste(neg$absents, collapse = ", ")))
  }

  # -- 2) Cache -------------------------------------------------------------
  depuis_cache <- stats::setNames(
    lapply(annees, function(a) cache_lire(lon, lat, a)), as.character(annees))
  a_telecharger <- annees[vapply(depuis_cache, is.null, logical(1))]

  notifier(0.05, sprintf("%d annee(s) en cache, %d a telecharger",
                         length(annees) - length(a_telecharger),
                         length(a_telecharger)))

  # -- 3) Telechargement par lots ------------------------------------------
  if (length(a_telecharger) > 0) {
    lots <- split(a_telecharger,
                  ceiling(seq_along(a_telecharger) / CONFIG$api$n_paralleles))

    for (i in seq_along(lots)) {
      lot <- lots[[i]]
      notifier(0.05 + 0.9 * (i - 1) / length(lots),
               sprintf("Telechargement SAFRAN %d-%d...", min(lot), max(lot)))

      requetes <- lapply(lot, function(a) safran_requete(lon, lat, a, neg$api))

      reponses <- tryCatch(
        httr2::req_perform_parallel(requetes, on_error = "continue"),
        # Repli sequentiel : httr2 < 1.0, ou parallelisme bloque par un proxy
        error = function(e) lapply(requetes, function(r) {
          tryCatch(httr2::req_perform(r), error = function(e2) NULL)
        }))

      for (k in seq_along(lot)) {
        rep_k <- if (k <= length(reponses)) reponses[[k]] else NULL
        # req_perform_parallel(on_error = "continue") peut renvoyer une
        # condition a la place d'une reponse : on la rend lisible.
        if (inherits(rep_k, "condition")) {
          erreurs <- c(erreurs,
                       sprintf("%d : %s", lot[k], conditionMessage(rep_k)))
          next
        }
        res <- safran_parser(rep_k, annee = lot[k])
        if (!is.null(res$erreur)) {
          erreurs <- c(erreurs, res$erreur)
          next
        }
        depuis_cache[[as.character(lot[k])]] <- res$donnees
        # L'annee en cours est incomplete : on ne la met jamais en cache.
        if (lot[k] < as.integer(format(Sys.Date(), "%Y"))) {
          cache_ecrire(lon, lat, lot[k], res$donnees)
        }
      }
    }
  }

  notifier(0.97, "Assemblage de la serie...")

  valides <- Filter(Negate(is.null), depuis_cache)
  if (length(valides) == 0) {
    vide <- safran_tableau_vide()
    attr(vide, "source")     <- "safran"
    attr(vide, "erreurs")    <- erreurs
    attr(vide, "parametres") <- neg$api
    attr(vide, "absents")    <- neg$absents
    return(vide)
  }

  df <- do.call(rbind, valides)
  rownames(df) <- NULL
  df <- df[order(df$date), , drop = FALSE]
  # Colonnes derivees recalculees pour TOUTES les annees, cache compris.
  df <- safran_deriver(df)

  # -- 4) ETP de secours si ETP_Q absente ou vide ---------------------------
  if (!"etp" %in% names(df) || all(is.na(df$etp))) {
    df$etp <- etp_hargreaves(df$t_moy, df$t_min, df$t_max, df$doy, lat)
    attr(df, "etp_source") <- "Hargreaves-Samani (ETP_Q indisponible)"
  } else {
    attr(df, "etp_source") <- "SAFRAN ETP_Q (Penman-Monteith)"
  }

  # -- 5) T min de secours ---------------------------------------------------
  # Sans TINF_H_Q les indicateurs de gel seraient muets. Une estimation
  # grossiere vaut mieux qu'un graphique vide, a condition de le signaler.
  if (all(is.na(df$t_min)) && !all(is.na(df$t_moy)) && !all(is.na(df$t_max))) {
    df$t_min <- 2 * df$t_moy - df$t_max
    erreurs <- c(erreurs, paste(
      "TINF_H_Q indisponible : la temperature minimale est estimee par",
      "symetrie (2 x T moyenne - T maximale). Les indicateurs de gel sont",
      "donc indicatifs."))
  }

  attr(df, "erreurs")    <- erreurs
  attr(df, "source")     <- "safran"
  attr(df, "parametres") <- neg$api
  attr(df, "absents")    <- neg$absents
  attr(df, "point")      <- c(lon = lon, lat = lat)
  attr(df, "extrait")    <- Sys.time()
  notifier(1, "Termine")
  df
}

#' Structure vide respectant le contrat de sortie.
#' @keywords internal
safran_tableau_vide <- function() {
  base <- data.frame(
    date = as.Date(character(0)),
    annee = integer(0), mois = integer(0), jour = integer(0), doy = integer(0),
    p_tot = numeric(0))
  for (v in names(CONFIG$variables)) base[[v]] <- numeric(0)
  base
}


# ------------------------------------------------------------------------------
# 5. ETP de secours — Hargreaves-Samani (1985)
# ------------------------------------------------------------------------------

#' Evapotranspiration potentielle journaliere par Hargreaves-Samani.
#'
#' Utilisee si SAFRAN ne renvoie pas ETP_Q. Moins precise que Penman-Monteith
#' (elle ignore vent et humidite) mais robuste : elle ne demande que les
#' temperatures et la latitude.
#' Reference : Hargreaves & Samani (1985), Applied Eng. in Agriculture 1(2).
#' @export
etp_hargreaves <- function(t_moy, t_min, t_max, doy, lat) {
  phi   <- lat * pi / 180
  dr    <- 1 + 0.033 * cos(2 * pi * doy / 365)      # distance Terre-Soleil
  delta <- 0.409 * sin(2 * pi * doy / 365 - 1.39)   # declinaison solaire

  x  <- pmin(pmax(-tan(phi) * tan(delta), -1), 1)   # borne : evite les NaN
  ws <- acos(x)

  # Rayonnement extraterrestre Ra (MJ.m-2.j-1), Gsc = 0.0820 MJ.m-2.min-1
  ra <- (24 * 60 / pi) * 0.0820 * dr *
    (ws * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(ws))

  amplitude <- pmax(t_max - t_min, 0)
  pmax(0.0023 * (t_moy + 17.8) * sqrt(amplitude) * ra * 0.408, 0)
}


# ------------------------------------------------------------------------------
# 6. Geocodage — API Geo
# ------------------------------------------------------------------------------

#' Recherche de communes par nom ou code postal.
#' @return data.frame (nom, code_insee, cp, departement, lon, lat, population)
#'   trie par population decroissante ; data.frame vide en cas d'echec.
#' @export
geo_chercher_communes <- function(requete, limite = 10) {

  requete <- trimws(requete %||% "")
  vide <- data.frame(
    nom = character(0), code_insee = character(0), cp = character(0),
    departement = character(0), lon = numeric(0), lat = numeric(0),
    population = integer(0), stringsAsFactors = FALSE)
  if (nchar(requete) < 2) return(vide)

  # Cinq chiffres = code postal, sinon recherche par nom
  champ <- if (grepl("^[0-9]{5}$", requete)) "codePostal" else "nom"
  params <- list(
    fields = "nom,code,codesPostaux,centre,population,departement",
    format = "json", limit = limite)
  params[[champ]] <- requete
  if (champ == "nom") params$boost <- "population"

  res <- tryCatch({
    rep <- httr2::req_perform(
      requete_avec_params(CONFIG$api$geo_communes, params, timeout = 20))
    httr2::resp_body_json(rep, simplifyVector = FALSE)
  }, error = function(e) NULL)

  if (is.null(res) || length(res) == 0) return(vide)

  extraire <- function(c) {
    coords <- c$centre$coordinates
    if (is.null(coords) || length(coords) < 2) return(NULL)
    data.frame(
      nom         = c$nom %||% NA_character_,
      code_insee  = c$code %||% NA_character_,
      cp          = (c$codesPostaux %||% list(NA_character_))[[1]],
      departement = c$departement$nom %||% NA_character_,
      lon         = as.numeric(coords[[1]]),
      lat         = as.numeric(coords[[2]]),
      population  = as.integer(c$population %||% NA_integer_),
      stringsAsFactors = FALSE)
  }

  lignes <- Filter(Negate(is.null), lapply(res, extraire))
  if (length(lignes) == 0) return(vide)
  out <- do.call(rbind, lignes)
  pop <- ifelse(is.na(out$population), 0L, out$population)
  out[order(-pop), , drop = FALSE]
}

#' Geocodage inverse : commune la plus proche d'un couple de coordonnees.
#' @return liste (nom, code_insee, departement, cp) ou NULL.
#' @export
geo_commune_par_point <- function(lon, lat) {
  res <- tryCatch({
    rep <- httr2::req_perform(requete_avec_params(
      CONFIG$api$geo_communes,
      list(lon = lon, lat = lat, format = "json",
           fields = "nom,code,departement,codesPostaux"),
      timeout = 15))
    httr2::resp_body_json(rep, simplifyVector = FALSE)
  }, error = function(e) NULL)

  if (is.null(res) || length(res) == 0) return(NULL)
  c1 <- res[[1]]
  list(nom         = c1$nom %||% NA_character_,
       code_insee  = c1$code %||% NA_character_,
       departement = c1$departement$nom %||% NA_character_,
       dep_code    = c1$departement$code %||% NA_character_,
       cp          = (c1$codesPostaux %||% list(NA_character_))[[1]])
}
