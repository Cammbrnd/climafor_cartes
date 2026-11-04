# ==============================================================================
# src/utils_limites.R — Limites d'usage par fenetre glissante
# ------------------------------------------------------------------------------
# Audit v1.7, partie V2.
#
# Un seul processus R sert toutes les sessions. Une rafale de demandes (un
# clic repete, un script qui rejoue le bouton « Analyser ») ralentit tout le
# monde et envoie jusqu'a 68 requetes par analyse vers des services tiers. Ce
# fichier fournit le compteur ; CONFIG$limites fixe les seuils.
#
# CE QUI EST LIMITE
#   - par SESSION  : le nombre d'analyses et de rapports sur une fenetre de
#                    `fenetre_min` minutes ;
#   - par PROCESSUS : le nombre d'analyses qui interrogent un service distant
#                    (donc hors cache), toutes sessions confondues.
#
# CE QUI NE L'EST PAS
#   - l'ADRESSE IP : derriere le mandataire d'un hebergeur, le serveur
#     d'application ne la voit pas de facon fiable (un en-tete se falsifie). Une
#     limite par adresse releve du mandataire ou de l'hebergeur, pas de ce code.
#   - un script qui ouvre une nouvelle session a chaque requete contourne la
#     limite par session ; seule la limite par processus le freine alors.
#
# Fenetre glissante : on garde les instants des demandes recentes ; une demande
# est refusee si le nombre d'instants de la fenetre atteint le maximum, et on dit
# a l'utilisateur dans combien de temps la prochaine passera. Les instants sont
# des parametres (`maintenant`) : les tests n'attendent jamais.
# ==============================================================================

.ETAT_LIMITES <- new.env(parent = emptyenv())

#' Un maximum est-il actif ? (NULL, NA, 0 ou negatif : pas de limite)
#' @keywords internal
limite_active <- function(max) {
  length(max) == 1L && is.numeric(max) && is.finite(max) && max > 0
}

#' Nouveau journal vide (environnement : modifiable sur place).
#' @keywords internal
limite_journal <- function() {
  j <- new.env(parent = emptyenv())
  j$instants <- numeric(0)
  j
}

#' Journal d'une session pour un compteur donne (cree a la demande).
#' @param nom « analyses » ou « rapports ».
#' @keywords internal
limite_journal_session <- function(session, nom) {
  if (is.null(session)) return(limite_journal())
  if (is.null(session$userData$limites)) session$userData$limites <- new.env(parent = emptyenv())
  if (is.null(session$userData$limites[[nom]])) session$userData$limites[[nom]] <- limite_journal()
  session$userData$limites[[nom]]
}

#' Journal de tout le processus pour un compteur donne.
#' @keywords internal
limite_journal_processus <- function(nom) {
  if (is.null(.ETAT_LIMITES[[nom]])) .ETAT_LIMITES[[nom]] <- limite_journal()
  .ETAT_LIMITES[[nom]]
}

#' Secondes a attendre avant que la prochaine demande soit acceptee (0 = tout
#' de suite). Ne modifie pas le journal.
#'
#' @param journal journal (limite_journal).
#' @param max nombre maximal de demandes dans la fenetre ; inactif si non positif.
#' @param fenetre_min longueur de la fenetre, en minutes.
#' @param maintenant instant de reference (numerique, secondes).
#' @keywords internal
limite_attente_s <- function(journal, max, fenetre_min = CONFIG$limites$fenetre_min,
                             maintenant = as.numeric(Sys.time())) {
  if (!limite_active(max) || !limite_active(fenetre_min)) return(0)
  fenetre_s <- fenetre_min * 60
  recents <- sort(journal$instants[journal$instants > maintenant - fenetre_s])
  if (length(recents) < max) return(0)
  # La plus ancienne des demandes qui comptent sort de la fenetre en premier :
  # il faut qu'il en sorte (longueur - max + 1) pour repasser sous le maximum.
  sortie <- recents[length(recents) - max + 1L] + fenetre_s
  max(0, sortie - maintenant)
}

#' Note une demande acceptee (et oublie les instants sortis de la fenetre).
#' @keywords internal
limite_noter <- function(journal, fenetre_min = CONFIG$limites$fenetre_min,
                         maintenant = as.numeric(Sys.time())) {
  fenetre_s <- if (limite_active(fenetre_min)) fenetre_min * 60 else 600
  journal$instants <- c(journal$instants[journal$instants > maintenant - fenetre_s],
                        maintenant)
  invisible(journal)
}

#' Delai d'attente en mots : « moins d'une minute », « environ 3 minutes ».
#' @keywords internal
limite_phrase_delai <- function(s) {
  if (!is.finite(s) || s <= 60) return("moins d'une minute")
  m <- ceiling(s / 60)
  sprintf("environ %d minute%s", m, if (m > 1) "s" else "")
}

#' Message affiche quand une demande est refusee. Dit ce qui s'est passe, quand
#' reessayer, et que rien n'est perdu.
#' @param quoi « analyses » ou « rapports ».
#' @param portee « session » ou « processus ».
#' @param max nombre de demandes autorisees dans la fenetre (portee « session »).
#' @keywords internal
limite_message <- function(quoi, portee, attente_s,
                           fenetre_min = CONFIG$limites$fenetre_min, max = NA) {
  fenetre <- sprintf("%s minutes", format(fenetre_min))
  debut <- if (identical(portee, "processus")) {
    "Le service reçoit beaucoup d'analyses avec téléchargement en ce moment."
  } else {
    n <- if (is.finite(max)) format(max) else "plusieurs"
    if (identical(quoi, "rapports")) {
      sprintf("Vous avez demandé %s rapports en %s, c'est la limite.", n, fenetre)
    } else {
      sprintf("Vous avez lancé %s analyses en %s, c'est la limite.", n, fenetre)
    }
  }
  paste(debut, sprintf("Réessayez dans %s ; vos résultats actuels restent affichés.",
                       limite_phrase_delai(attente_s)))
}

#' Verifie les limites d'une demande et, si elle passe, la note partout.
#'
#' Les deux compteurs sont verifies AVANT de rien noter : une demande refusee par
#' la limite du processus ne consomme pas le quota de la session.
#'
#' @param quoi « analyses » ou « rapports ».
#' @param session session Shiny (ou NULL hors session).
#' @param distant la demande interroge-t-elle un service distant ? (seulement
#'   alors, le compteur du processus s'applique pour les analyses.)
#' @param maintenant instant de reference, en secondes (injectable).
#' @return liste(ok, message, attente_s, portee).
#' @keywords internal
limite_controler <- function(quoi, session = shiny::getDefaultReactiveDomain(),
                             distant = FALSE, maintenant = as.numeric(Sys.time())) {
  L <- CONFIG$limites
  max_session <- switch(quoi, analyses = L$analyses_session,
                        rapports = L$rapports_session, NA)
  j_session <- limite_journal_session(session, quoi)
  att_s <- limite_attente_s(j_session, max_session, L$fenetre_min, maintenant)
  if (att_s > 0) {
    return(list(ok = FALSE, attente_s = att_s, portee = "session",
                message = limite_message(quoi, "session", att_s, L$fenetre_min, max_session)))
  }

  j_proc <- NULL
  if (isTRUE(distant) && identical(quoi, "analyses")) {
    j_proc <- limite_journal_processus("telechargements")
    att_p <- limite_attente_s(j_proc, L$telechargements_proc, L$fenetre_min, maintenant)
    if (att_p > 0) {
      return(list(ok = FALSE, attente_s = att_p, portee = "processus",
                  message = limite_message(quoi, "processus", att_p, L$fenetre_min)))
    }
  }

  if (limite_active(max_session)) limite_noter(j_session, L$fenetre_min, maintenant)
  if (!is.null(j_proc) && limite_active(L$telechargements_proc)) {
    limite_noter(j_proc, L$fenetre_min, maintenant)
  }
  list(ok = TRUE, attente_s = 0, portee = NA_character_, message = "")
}
