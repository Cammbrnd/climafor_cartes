# ==============================================================================
# utils_profils.R — PROFILS ET ACCES (v2.0)
# ------------------------------------------------------------------------------
# Trois profils, une seule application :
#
#   terrain  consultation sur telephone : « autour de moi », la carte ;
#   co       correspondant-observateur : la carte et un bilan synthetique, avec les
#            grandeurs les plus parlantes ;
#   dsf      permanents du DSF : tous les onglets, tous les indicateurs, la
#            comparaison, l'import de fiches, les bilans par territoire.
#
# Un profil est une CONFIGURATION (onglets visibles, echelles de bilan, grandeurs
# offertes), pas un autre code : le meme module de bilan sert les trois.
#
# COMMENT ON ARRIVE DANS UN PROFIL
# --------------------------------
# Par l'adresse : ?id=xxx. La table des identifiants est un fichier CSV
# (config/acces.csv, plus config/acces_local.csv s'il existe). Sans identifiant
# reconnu, la page d'entree demande l'identifiant ; si CONFIG$acces$demo est vrai,
# elle propose aussi les trois profils (?profil=co) pour une demonstration.
#
# CE N'EST PAS UNE AUTHENTIFICATION
# ---------------------------------
# L'identifiant figure dans l'adresse ; quiconque le connait obtient le profil.
# Le mecanisme oriente chaque personne vers la bonne interface, rien de plus. Quand
# l'API d'identification sera disponible, seule profil_resoudre() change : le reste
# de l'application ne lit que le profil qu'elle rend.
#
# Le texte de la requete est une ENTREE NON FIABLE : l'identifiant est borne a
# [a-z0-9_-], 40 caracteres, avant toute comparaison.
# ==============================================================================

#' Les profils. `onglets` : valeurs des onglets, dans l'ordre d'affichage (le
#' premier s'ouvre d'abord). `echelles` : echelles de bilan offertes. `multi` : le
#' bilan peut-il comparer plusieurs territoires. `restreint` : seules les grandeurs
#' de CONFIG$synthese$couches_co sont offertes.
#' @export
PROFILS <- list(
  terrain = list(
    id = "terrain", libelle = "Terrain",
    resume = "Le climat autour de moi, sur téléphone : valeurs de l'année, mesures des stations proches et synthèse à envoyer par courriel.",
    onglets = c("terrain", "carte"),
    echelles = "point", multi = FALSE, restreint = TRUE),
  co = list(
    id = "co", libelle = "Correspondant-observateur",
    resume = "La carte et un bilan synthétique d'un point (maille et stations proches) ou d'un département, avec les grandeurs les plus parlantes.",
    onglets = c("carte", "synthese", "sources"),
    echelles = c("point", "departement"), multi = FALSE, restreint = TRUE),
  dsf = list(
    id = "dsf", libelle = "Permanent DSF",
    resume = "Tous les indicateurs, la comparaison de deux lieux (maille, station, département) ou de lieux importés, l'import de fiches et les bilans par territoire.",
    onglets = c("accueil", "carte", "comparer", "synthese", "bilan", "rapport", "parametres", "sources"),
    echelles = c("departement", "region", "fiches", "point"), multi = TRUE, restreint = FALSE)
)

# ------------------------------------------------------------------------------
# Table des identifiants
# ------------------------------------------------------------------------------

#' Identifiant assaini : minuscules, [a-z0-9_-], 3 a 40 caracteres, sinon NA.
#' @keywords internal
profil_id_sur <- function(x, max = CONFIG$acces$id_long_max) {
  x <- tolower(trimws(as.character(x %||% "")[1]))
  if (is.na(x) || !grepl(sprintf("^[a-z0-9_-]{3,%d}$", as.integer(max)), x)) return(NA_character_)
  x
}

#' Lit un fichier d'acces : lignes `id;profil;nom`, commentaires `#` ignores.
#' Une ligne illisible, ou dont le profil n'existe pas, est ecartee.
#' @return data.frame(id, profil, nom).
#' @keywords internal
profil_lire_fichier <- function(chemin) {
  vide <- data.frame(id = character(0), profil = character(0), nom = character(0),
                     stringsAsFactors = FALSE)
  if (!is.character(chemin) || !length(chemin) || !file.exists(chemin)) return(vide)
  txt <- tryCatch(readLines(chemin, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  txt <- sub("^﻿", "", txt)
  txt <- txt[nzchar(trimws(txt)) & !grepl("^\\s*#", txt)]
  if (length(txt) < 2L) return(vide)
  sep <- if (lengths(regmatches(txt[1], gregexpr(";", txt[1], fixed = TRUE))) >= 1L) ";" else ","
  champs <- strsplit(txt[-1], sep, fixed = TRUE)
  lignes <- lapply(champs, function(f) {
    f <- trimws(f)
    id <- profil_id_sur(f[1]); pr <- tolower(f[2] %||% "")
    if (is.na(id) || !pr %in% names(PROFILS)) return(NULL)
    nom <- if (length(f) >= 3L && !is.na(f[3])) f[3] else ""
    data.frame(id = id, profil = pr, nom = substr(gsub("[[:cntrl:]]", " ", nom), 1L, 80L),
               stringsAsFactors = FALSE)
  })
  lignes <- Filter(Negate(is.null), lignes)
  if (!length(lignes)) return(vide)
  do.call(rbind, lignes)
}

#' Table des identifiants : le fichier local d'abord (il l'emporte), puis le fichier
#' versionne. Un identifiant present deux fois garde sa premiere ligne.
#' @keywords internal
profil_acces <- function() {
  f1 <- tryCatch(chemin_ci(CONFIG$acces$fichier_local), error = function(e) "")
  f2 <- tryCatch(chemin_ci(CONFIG$acces$fichier), error = function(e) "")
  t <- rbind(profil_lire_fichier(f1), profil_lire_fichier(f2))
  t[!duplicated(t$id), , drop = FALSE]
}

# ------------------------------------------------------------------------------
# Resolution : de l'adresse au profil
# ------------------------------------------------------------------------------

#' Parametres d'une requete, lus un par un : un morceau qui ne se decode pas (« %00 ») est
#' ecarte et signale, il ne fait pas echouer les autres. Le premier de deux parametres de meme
#' nom l'emporte.
#' @return list(q = parametres lus, illisibles = noms des parametres ecartes).
#' @keywords internal
profil_lire_requete <- function(requete) {
  txt <- sub("^\\?", "", as.character(requete %||% "")[1])
  q <- list(); ill <- character(0)
  if (is.na(txt) || !nzchar(txt)) return(list(q = q, illisibles = ill))
  for (m in strsplit(txt, "&", fixed = TRUE)[[1]]) {
    if (!nzchar(m)) next
    r <- tryCatch(shiny::parseQueryString(m), error = function(e) NULL)
    if (is.null(r)) { ill <- c(ill, tolower(sub("=.*$", "", m))); next }
    for (nom in names(r)) if (is.null(q[[nom]])) q[[nom]] <- r[[nom]]
  }
  list(q = q, illisibles = ill)
}

#' Le profil demande par une adresse.
#'
#' @param requete texte de la requete (« ?id=demo-co&x=1.4 ») ou NULL.
#' @param acces table des identifiants (profil_acces()).
#' @param demo les profils s'ouvrent-ils sans identifiant (?profil=co) ?
#' @return list(ok, profil, id, nom, inconnu) ; `profil` est NULL quand ok est FAUX,
#'   et `inconnu` vaut VRAI quand un identifiant a ete donne mais n'est pas dans la table.
#' @export
profil_resoudre <- function(requete, acces = profil_acces(), demo = isTRUE(CONFIG$acces$demo)) {
  rien <- list(ok = FALSE, profil = NULL, id = NA_character_, nom = "", inconnu = FALSE)
  lu <- profil_lire_requete(requete)
  q <- lu$q
  # Un parametre illisible est ignore (une adresse n'est pas rejetee pour un « junk=%00 »),
  # sauf l'identifiant : donne mais illisible, il est inconnu, et la page d'entree le dit.
  if ("id" %in% lu$illisibles) return(modifyList(rien, list(inconnu = TRUE)))
  if (!is.null(q$id)) {
    id <- profil_id_sur(q$id)
    i <- if (is.na(id)) integer(0) else which(acces$id == id)
    if (!length(i)) return(modifyList(rien, list(inconnu = TRUE)))
    return(list(ok = TRUE, profil = PROFILS[[acces$profil[i[1]]]], id = id,
                nom = acces$nom[i[1]], inconnu = FALSE))
  }
  p <- tolower(trimws(as.character(q$profil %||% "")[1]))
  if (isTRUE(demo) && !is.na(p) && p %in% names(PROFILS)) {
    return(list(ok = TRUE, profil = PROFILS[[p]], id = NA_character_, nom = "", inconnu = FALSE))
  }
  rien
}

#' Morceau d'adresse qui rappelle le profil (« id=demo-co » ou « profil=co »), a
#' placer en tete de la requete quand l'application la reecrit, sinon recharger la
#' page ferait perdre le profil.
#' @keywords internal
profil_prefixe_requete <- function(res) {
  if (is.null(res) || !isTRUE(res$ok)) return("")
  if (!is.na(res$id)) sprintf("id=%s", res$id) else sprintf("profil=%s", res$profil$id)
}

#' Le profil d'une adresse, avec la compatibilite des liens partages d'avant la v2.0.
#'
#' Un lien de partage ancien (?x=...&y=..., sans profil) ouvrait l'interface complete :
#' il continue de l'ouvrir, au lieu d'envoyer son destinataire sur la page d'entree. Cela
#' n'ouvre rien de plus qu'avant. Un identifiant donne mais inconnu n'est PAS rattrape.
#' @return comme profil_resoudre(), avec `ancien = TRUE` pour un lien d'avant la v2.0.
#' @keywords internal
profil_pour_requete <- function(requete) {
  res <- profil_resoudre(requete)
  if (isTRUE(res$ok) || isTRUE(res$inconnu)) return(res)
  etat <- tryCatch(partage_decoder(as.character(requete %||% "")[1]), error = function(e) NULL)
  if (is.null(etat)) return(res)
  modifyList(res, list(ok = TRUE, profil = PROFILS$dsf, ancien = TRUE))
}

#' Le profil, dans une session Shiny.
#'
#' L'adresse d'ouverture se lit dans `clientData$url_search` (envoyee par le navigateur a
#' l'initialisation de la session), PAS dans `session$request$QUERY_STRING` : cette derniere
#' decrit la requete WebSocket, dont l'adresse n'a plus de requete.
#' @keywords internal
profil_de_session <- function(session) {
  q <- tryCatch(shiny::isolate(session$clientData$url_search), error = function(e) NULL)
  if (is.null(q) || !nzchar(q)) q <- tryCatch(session$request$QUERY_STRING, error = function(e) NULL)
  profil_pour_requete(q %||% "")
}

#' Debut d'adresse a conserver quand l'application reecrit la sienne (« id=xxx& »), ou
#' rien : sans lui, recharger la page ou envoyer le lien ferait perdre le profil.
#' @keywords internal
profil_prefixe_adresse <- function(res) {
  p <- profil_prefixe_requete(res)
  if (nzchar(p) && !isTRUE(res$ancien)) paste0(p, "&") else ""
}
