# ==============================================================================
# utils_envoi.R — LA SYNTHESE DU TERRAIN, A ENVOYER PAR COURRIEL (v2.1)
# ------------------------------------------------------------------------------
# Sur le terrain, on veut pouvoir dire « voici ce que dit le climat ici » a un collegue,
# un proprietaire, le bureau : une page courte, lisible sur un telephone, qui part par
# courriel.
#
# CE QUE FAIT L'APPLICATION. Elle prepare :
#   - un OBJET et un CORPS de message, en texte (les premieres lignes de la synthese) ;
#   - la synthese COMPLETE en texte, a copier ;
#   - la synthese COMPLETE en page HTML autonome, a enregistrer, a joindre ou a partager.
# Le bouton « Envoyer par courriel » ouvre la messagerie de l'utilisateur, message deja
# redige (lien mailto:). Le bouton « Partager » ouvre le partage de l'appareil, avec la
# page en fichier joint quand l'appareil le permet.
#
# CE QUE L'APPLICATION NE FAIT PAS, ET POURQUOI.
#   - Elle n'envoie aucun courriel elle-meme : il faudrait un serveur de messagerie, un
#     compte, et le mot de passe qui va avec ; et le message partirait au nom de
#     l'application, pas du forestier. Avec mailto, il part de sa messagerie, sous son
#     nom, et il le relit avant d'envoyer.
#   - Elle ne demande pas de destinataire : on ne saisit pas l'adresse de quelqu'un dans
#     une page web pour la lui faire parvenir a un serveur ; la messagerie la connait.
#   - Un lien mailto ne porte pas de piece jointe (aucun navigateur ne le permet) : la
#     page complete se joint a la main apres « Enregistrer », ou part par « Partager ».
#   - Le message ne contient pas les coordonnees du point : le nom du lieu suffit.
#
# LONGUEUR. Un lien mailto trop long est tronque, ou refuse, par certaines messageries
# (environ 2000 caracteres une fois encode). Le corps du message est donc construit pour
# tenir sous ENVOI_BUDGET_MAILTO : on retire des lignes de grandeurs, jamais le pied.
# ==============================================================================

ENVOI_BUDGET_MAILTO <- 1900L

#' Encodage en pour-cent (UTF-8), independant de la locale : chaque octet hors
#' [A-Za-z0-9-._~] devient %XX.
#' @param x texte (un seul element).
#' @keywords internal
envoi_encoder <- function(x) {
  b <- charToRaw(enc2utf8(as.character(x)[1]))
  if (!length(b)) return("")
  v <- as.integer(b)
  ok <- (v >= 48L & v <= 57L) | (v >= 65L & v <= 90L) | (v >= 97L & v <= 122L) | v %in% c(45L, 46L, 95L, 126L)
  paste0(ifelse(ok, vapply(v, intToUtf8, ""), sprintf("%%%02X", v)), collapse = "")
}

#' Lien « Envoyer par courriel » : la messagerie s'ouvre sur un message deja redige, sans
#' destinataire. Les sauts de ligne sont ceux des courriels (CRLF).
#' @param objet,corps textes.
#' @return adresse « mailto:?subject=...&body=... ».
#' @export
envoi_mailto <- function(objet, corps) {
  paste0("mailto:?subject=", envoi_encoder(objet),
         "&body=", envoi_encoder(gsub("\n", "\r\n", corps, fixed = TRUE)))
}

#' Nom de chaque lieu du bilan pour ce qu'il est : la station porte son nom, une maille
#' « Maille SAFRAN (modèle) ».
#' @keywords internal
envoi_etiquettes <- function(res) {
  stats::setNames(ifelse(res$tl$source == "station", res$tl$lieu, stations_libelle_maille()), res$tl$lieu)
}

#' Une valeur, son ecart et sa lecture, en une phrase courte.
#' @param r une ligne de cmp_bilan().
#' @keywords internal
envoi_valeur <- function(r, court = FALSE) {
  if (!is.finite(r$valeur)) return("pas de valeur")
  u <- if (nzchar(r$unite)) r$unite else ""
  v <- fmt_val(r$valeur, u, r$decimales)
  if (!is.finite(r$ecart)) return(v)
  if (court) return(sprintf("%s (%s)", v, fmt_ecart(r$ecart, u, r$decimales)))
  sprintf("%s, %s (%s)", v, fmt_ecart(r$ecart, u, r$decimales),
          sub(" de la normale", "", synthese_lecture_ecart(r$classe, r$ecart, r$ecart_type_ref)))
}

#' Libelle court d'une source pour le message : « maille », « station », ou le nom de la
#' station quand il y en a plusieurs.
#' @keywords internal
envoi_libelle_court <- function(res) {
  n_st <- sum(res$tl$source == "station")
  s <- ifelse(res$tl$source == "station",
              if (n_st > 1L) substr(sub(" \\(mesure\\)$", "", res$tl$lieu), 1L, 24L) else "station", "maille")
  stats::setNames(s, res$tl$lieu)
}

#' Les grandeurs du bilan, une ligne chacune, dans l'ordre de la carte.
#' @return data.frame(cle, titre, long, court).
#' @keywords internal
envoi_grandeurs <- function(res) {
  b <- res$bilan[res$bilan$cle %in% res$cles_t, , drop = FALSE]
  cles <- unique(b$cle)
  cles <- cles[order(match(cles, names(CARTE_COUCHES)))]
  eti <- envoi_etiquettes(res); cou <- envoi_libelle_court(res)
  multi <- nrow(res$tl) > 1L
  l <- lapply(cles, function(cle) {
    bc <- b[b$cle == cle & is.finite(b$valeur), , drop = FALSE]
    bc <- bc[order(match(bc$lieu, res$tl$lieu)), , drop = FALSE]
    if (!nrow(bc)) return(NULL)
    long <- if (multi) paste(vapply(seq_len(nrow(bc)), function(i)
      sprintf("%s : %s", eti[[bc$lieu[i]]], envoi_valeur(bc[i, ])), ""), collapse = " ; ")
      else envoi_valeur(bc[1, ])
    court <- if (multi) paste(vapply(seq_len(nrow(bc)), function(i)
      sprintf("%s %s", cou[[bc$lieu[i]]], envoi_valeur(bc[i, ], court = TRUE)), ""), collapse = " ; ")
      else envoi_valeur(bc[1, ], court = TRUE)
    data.frame(cle = cle, titre = bc$titre[1], long = long, court = court, stringsAsFactors = FALSE)
  })
  l <- Filter(Negate(is.null), l)
  if (!length(l)) return(data.frame(cle = character(0), titre = character(0), long = character(0),
                                    court = character(0), stringsAsFactors = FALSE))
  do.call(rbind, l)
}

#' Le tableau de la synthese : une ligne par grandeur et par source.
#' @return data.frame(Grandeur, Source, Valeur, Normale, Écart, Lecture) en texte, ou NULL.
#' @keywords internal
envoi_tableau <- function(res) {
  d <- synthese_tableau(res)
  if (is.null(d)) return(NULL)
  eti <- envoi_etiquettes(res)
  d <- data.frame(Grandeur = d$Grandeur, Source = unname(eti[d$Lieu]), Valeur = d$Valeur,
                  Normale = d$Normale, `Écart` = d$`Écart`, Lecture = d$Lecture,
                  check.names = FALSE, stringsAsFactors = FALSE)
  d <- d[d$Valeur != "n.d.", , drop = FALSE]
  rownames(d) <- NULL
  d
}

#' Les sources de la synthese, en mots : d'ou vient chaque valeur.
#' @keywords internal
envoi_sources <- function(res) {
  det <- res$details %||% character(0)
  eti <- envoi_etiquettes(res)
  ret <- res$tl[res$tl$retenu, , drop = FALSE]
  vapply(seq_len(nrow(ret)), function(i) {
    l <- ret$lieu[i]
    d <- if (l %in% names(det)) det[[l]] else ret$note[i]
    d <- if (nzchar(d %||% "")) d else if (ret$source[i] == "station") "station Météo-France" else "maille de 8 km la plus proche"
    sprintf("%s — %s", eti[[l]], d)
  }, "")
}

#' La mise en garde de lecture, selon ce que la synthese contient.
#' @keywords internal
envoi_prudence <- function(res) {
  if (any(res$tl$source == "station")) {
    paste("Une station est une mesure faite à son emplacement ; la maille SAFRAN (8 km) est un modèle qui lisse le relief.",
          "Les deux ne disent pas exactement la même chose, et leur écart n'est pas une erreur de l'une ou de l'autre.",
          "Ce bilan décrit un climat, il ne remplace pas un diagnostic stationnel.")
  } else {
    paste("La valeur est celle de la maille SAFRAN de 8 km (un modèle), pas la mesure d'une station :",
          "en terrain accidenté elle peut s'en écarter. Ce bilan décrit un climat, il ne remplace pas un diagnostic stationnel.")
  }
}

#' Prepare la synthese a envoyer : tout ce que les boutons utilisent.
#'
#' @param res synthese_preparer("point", ...) du Terrain ; `res$point_nom` est le nom du lieu,
#'   `res$details` (facultatif) le detail de chaque source.
#' @param maintenant date de l'etablissement (testable).
#' @return list(objet, titre, lieu, resume, grandeurs, tableau, sources, prudence, pied,
#'   texte_court, mailto, texte_complet, nom_fichier, tronque).
#'   `tronque` : nombre de grandeurs absentes du message court.
#' @export
envoi_synthese <- function(res, maintenant = Sys.Date()) {
  lieu <- res$point_nom %||% synthese_libelle(res)
  titre <- sprintf("Bilan climatique %d — %s", res$annee, lieu)
  objet <- sprintf("%s — %s", CONFIG$app$nom, titre)
  resume <- utils::head(synthese_phrases_generales(res), 3L)
  gr <- envoi_grandeurs(res)
  tab <- envoi_tableau(res)
  src <- envoi_sources(res)
  prudence <- envoi_prudence(res)
  date <- format(maintenant, "%d/%m/%Y")
  pied <- sprintf("%s %s — %s — établi le %s — Licence ouverte Etalab 2.0 (Météo-France)",
                  CONFIG$app$nom, CONFIG$app$version, CONFIG$app$organisme, date)

  # -- Texte complet : tout, sans limite de longueur
  complet <- c(
    titre, sprintf("Normale %s · établi le %s", res$ctx$ref_label, date), "",
    "EN BREF", paste0("- ", resume), "",
    "GRANDEURS", if (nrow(gr)) paste0("• ", gr$titre, " : ", gr$long) else "Aucune valeur exploitable.", "",
    "SOURCES", paste0("- ", src), "",
    "À LIRE AVEC PRUDENCE", prudence, "",
    pied)

  # -- Texte court : sous le budget du lien mailto
  fin <- c("", "La synthèse complète (tableau, sources, méthode) est dans le fichier joint ou dans ClimaFor.", pied)
  construire <- function(n) {
    c(titre, sprintf("Normale %s", res$ctx$ref_label), "",
      paste0("- ", utils::head(resume, if (n >= 3L) 2L else 1L)), "",
      if (n > 0L) paste0("• ", gr$titre[seq_len(n)], " : ", gr$court[seq_len(n)]),
      if (n < nrow(gr)) sprintf("… et %d autre%s grandeur%s", nrow(gr) - n, if (nrow(gr) - n > 1L) "s" else "", if (nrow(gr) - n > 1L) "s" else ""),
      fin)
  }
  n <- nrow(gr)
  corps <- paste(construire(n), collapse = "\n")
  while (nchar(envoi_mailto(objet, corps), type = "bytes") > ENVOI_BUDGET_MAILTO && n > 0L) {
    n <- n - 1L
    corps <- paste(construire(n), collapse = "\n")
  }
  list(objet = objet, titre = titre, lieu = lieu, annee = res$annee, ref = res$ctx$ref_label, date = date,
       resume = resume, grandeurs = gr, tableau = tab, sources = src, prudence = prudence, pied = pied,
       texte_court = corps, mailto = envoi_mailto(objet, corps),
       texte_complet = paste(complet, collapse = "\n"),
       nom_fichier = nom_fichier_export("synthese", list(nom = lieu), "html"),
       tronque = nrow(gr) - n)
}


# ------------------------------------------------------------------------------
# La page HTML autonome
# ------------------------------------------------------------------------------

#' La synthese en page HTML autonome : un seul fichier, sans script ni ressource externe,
#' lisible sur un telephone et imprimable.
#' @param env envoi_synthese().
#' @return le document, en texte.
#' @export
envoi_html <- function(env) {
  tete <- paste0(
    "<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", htmltools::htmlEscape(env$titre), "</title>\n",
    "<style>", SYNTHESE_STYLE_HTML, "</style>\n</head>\n")
  corps <- htmltools::tags$body(
    htmltools::tags$h1(env$titre),
    htmltools::tags$p(class = "meta", sprintf("Normale %s · établi le %s", env$ref, env$date)),
    htmltools::tags$h2("En bref"),
    htmltools::tags$div(class = "bref", lapply(env$resume, htmltools::tags$p)),
    if (!is.null(env$tableau)) htmltools::tagList(
      htmltools::tags$h2("Tableau de synthèse"),
      htmltools::tags$div(style = "overflow-x:auto", synthese_table_simple(env$tableau))),
    htmltools::tags$h2("Sources"),
    htmltools::tags$ul(lapply(env$sources, htmltools::tags$li)),
    htmltools::tags$h2("À lire avec prudence"),
    htmltools::tags$div(class = "limites", htmltools::tags$p(env$prudence)),
    htmltools::tags$p(class = "meta", env$pied))
  paste0("<!doctype html>\n<html lang=\"fr\">\n", tete, as.character(corps), "\n</html>")
}

#' Ecrit la page HTML de la synthese.
#' @return `file`, invisiblement.
#' @export
envoi_ecrire_html <- function(env, file) {
  writeLines(envoi_html(env), file, useBytes = TRUE)
  invisible(file)
}

#' Les donnees des boutons « Partager » et « Copier », en JSON dans la page : le navigateur
#' les lit au clic, sans repasser par le serveur (l'autorisation de partager ne dure que
#' quelques secondes apres le geste).
#' @param env envoi_synthese().
#' @return balise <script type="application/json">.
#' @keywords internal
envoi_donnees_ui <- function(env) {
  j <- jsonlite::toJSON(list(objet = env$objet, texte = env$texte_complet, court = env$texte_court,
                             fichier = env$nom_fichier, html = envoi_html(env)),
                        auto_unbox = TRUE)
  # « </script> » dans le JSON fermerait la balise : on echappe le « / ».
  j <- gsub("</", "<\\/", as.character(j), fixed = TRUE)
  htmltools::tags$script(type = "application/json", class = "cf-envoi-donnees", htmltools::HTML(j))
}
