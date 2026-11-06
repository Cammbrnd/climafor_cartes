# ==============================================================================
# utils_ui.R — PETITS OUTILS D'INTERFACE
# ------------------------------------------------------------------------------
# Fichier purement presentationnel : aucune logique metier, aucune dependance
# a CONFIG en dehors de ce qui est passe en argument.
#
# POURQUOI DES SVG EN DUR PLUTOT QUE shiny::icon()
# ------------------------------------------------
# shiny::icon() embarque Font Awesome : une police de plus de 100 ko chargee
# pour une dizaine de glyphes, plus une dependance externe dont la version
# varie avec celle de Shiny. Les traces ci-dessous sont des SVG de 24x24 au
# trait, herites de la couleur du texte (stroke: currentColor dans la feuille
# de style) : rien a telecharger, rien a mettre a jour, et le contraste suit
# automatiquement celui du contexte (bouton bleu, bandeau blanc, etc.).
#
# Les icones sont DECORATIVES : aria-hidden="true" et focusable="false". Elles
# accompagnent toujours un libelle textuel, jamais ne le remplacent — sauf
# pour le bouton de fermeture du tiroir, qui porte alors un title et un
# aria-label explicites.
# ==============================================================================

#' Traces des icones. Cle = nom d'usage, valeur = contenu du <svg>.
#' @keywords internal
CF_ICONES <- list(
  # Navigation principale
  accueil    = '<path d="M4 11l8-7 8 7"/><path d="M6 10v10h12V10"/><path d="M10 20v-6h4v6"/>',
  synthese   = '<path d="M5 20V11M12 20V4M19 20v-6"/><path d="M3 20h18"/>',
  comparer   = '<path d="M4 8h14M14 4l4 4-4 4"/><path d="M20 16H6M10 12l-4 4 4 4"/>',
  bilan      = '<path d="M3 3v18h18"/><path d="M7 15l3.5-4.5 3 3L19 6"/>',
  rapport    = '<path d="M14 3H7a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V8z"/><path d="M14 3v5h5"/><path d="M9 13h6M9 17h4"/>',
  parametres = '<path d="M4 6h10M18 6h2M4 12h4M12 12h8M4 18h8M16 18h4"/><circle cx="16" cy="6" r="2"/><circle cx="10" cy="12" r="2"/><circle cx="14" cy="18" r="2"/>',
  sources    = '<path d="M4 5.5A2.5 2.5 0 0 1 6.5 3H19v16H6.5A2.5 2.5 0 0 0 4 21.5z"/><path d="M4 19.5V5.5"/>',

  # Actions
  reglages   = '<path d="M4 6h10M18 6h2M4 12h4M12 12h8M4 18h8M16 18h4"/><circle cx="16" cy="6" r="2"/><circle cx="10" cy="12" r="2"/><circle cx="14" cy="18" r="2"/>',
  croix      = '<path d="M6 6l12 12M18 6L6 18"/>',
  lancer     = '<path d="M5 4l14 8-14 8z"/>',
  recherche  = '<circle cx="11" cy="11" r="7"/><path d="M20 20l-3.5-3.5"/>',
  telecharger= '<path d="M12 4v11"/><path d="M8 12l4 4 4-4"/><path d="M4 19h16"/>',
  image      = '<rect x="3" y="4" width="18" height="16"/><circle cx="8.5" cy="9.5" r="1.5"/><path d="M4 18l5-5 4 4 3-3 4 4"/>',
  tableau    = '<rect x="3" y="4" width="18" height="16"/><path d="M3 10h18M9 10v10M15 10v10"/>',

  # Reperes
  position   = '<path d="M12 21s7-6.3 7-11a7 7 0 1 0-14 0c0 4.7 7 11 7 11z"/><circle cx="12" cy="10" r="2.6"/>',
  calendrier = '<rect x="3" y="5" width="18" height="16"/><path d="M3 10h18M8 3v4M16 3v4"/>',
  cible      = '<circle cx="12" cy="12" r="7"/><path d="M12 2v3M12 19v3M2 12h3M19 12h3"/>',
  station    = '<path d="M10 14.5V5a2 2 0 1 1 4 0v9.5a4 4 0 1 1-4 0z"/><path d="M12 9v7"/>',
  carte      = '<path d="M9 4L3 6v14l6-2 6 2 6-2V4l-6 2z"/><path d="M9 4v14M15 6v14"/>',
  coche      = '<path d="M4 12.5l5 5L20 6.5"/>',
  info       = '<circle cx="12" cy="12" r="9"/><path d="M12 11v6M12 7.5v.01"/>',
  lien       = '<path d="M10 14a4 4 0 0 0 5.7 0l3-3a4 4 0 0 0-5.7-5.7l-1 1"/><path d="M14 10a4 4 0 0 0-5.7 0l-3 3a4 4 0 0 0 5.7 5.7l1-1"/>'
)

#' Icone SVG en ligne, decorative.
#'
#' @param nom Cle de CF_ICONES.
#' @param classe Classe CSS appliquee au <svg>.
#' @return htmltools::HTML, ou NULL si le nom est inconnu (une icone manquante
#'   ne doit jamais faire tomber une page).
#' @export
cf_icone <- function(nom, classe = "cf-icone") {
  trace <- CF_ICONES[[nom]]
  if (is.null(trace)) return(NULL)
  htmltools::HTML(sprintf(
    '<svg class="%s" viewBox="0 0 24 24" aria-hidden="true" focusable="false">%s</svg>',
    classe, trace))
}

#' Libelle d'onglet : icone + texte.
#' @export
cf_onglet_titre <- function(icone, texte) {
  htmltools::tagList(cf_icone(icone), texte)
}

#' En-tete d'etape numerotee, dans le tiroir de reglages.
#'
#' Le numero n'est pas un ornement : il indique qu'il existe un ordre de
#' remplissage et combien d'etapes restent avant de pouvoir lancer l'analyse.
#'
#' @param numero Chiffre affiche.
#' @param titre Intitule de l'etape.
#' @param note Precision facultative, en gris, sur la meme ligne.
#' @export
cf_etape <- function(numero, titre, note = NULL) {
  shiny::div(
    class = "cf-etape",
    shiny::span(class = "cf-etape__num", `aria-hidden` = "true", numero),
    shiny::span(
      class = "cf-etape__titre",
      titre,
      if (!is.null(note)) shiny::span(class = "cf-etape__note", " — ", note)
    )
  )
}

#' Bloc replie pour les reglages secondaires.
#'
#' @param titre Intitule affiche sur la ligne cliquable.
#' @param ... Contenu du bloc.
#' @param ouvert TRUE pour afficher le bloc deplie au chargement.
#' @export
cf_repli <- function(titre, ..., ouvert = FALSE) {
  shiny::tags$details(
    class = "cf-repli", open = if (isTRUE(ouvert)) NA else NULL,
    shiny::tags$summary(titre),
    shiny::div(class = "cf-repli__corps", ...)
  )
}

#' Consigne une erreur dans le journal du serveur, avec son contexte.
#'
#' Audit, C3 : seize `tryCatch` repliaient sur NULL, "" ou un tableau vide sans
#' rien dire, si bien qu'un bug de calcul ressemblait a une donnee absente. Tout
#' repli qui masque une ERREUR passe maintenant par ici : le journal garde le
#' contexte (quel indicateur, quelle etape) et le message d'origine.
#' @param contexte ce qui echoue, en une locution (« Synthese chiffree »).
#' @param e condition R, ou texte.
#' @return le message, de facon invisible (pour le garder dans un attribut).
#' @export
journaliser_erreur <- function(contexte, e) {
  msg <- if (inherits(e, "condition")) conditionMessage(e) else as.character(e)[1]
  message(sprintf("[ClimaFor] %s en échec : %s", contexte, msg))
  invisible(msg)
}

#' Message d'erreur R rendu montrable a l'ecran.
#'
#' Un message brut peut contenir un chemin du serveur, des retours a la ligne ou
#' un texte tres long : il est donc ramene sur UNE ligne, ses chemins absolus
#' sont remplaces par « … », et il est tronque. Ce qui reste suffit a
#' l'identifier et a le transmettre au mainteneur ; le journal garde l'original.
#' @param msg chaine (ou condition).
#' @param max_car longueur maximale.
#' @export
erreur_lisible <- function(msg, max_car = 300L) {
  if (inherits(msg, "condition")) msg <- conditionMessage(msg)
  msg <- as.character(msg %||% "")[1]
  if (is.na(msg)) msg <- ""
  msg <- gsub("[[:space:]]+", " ", msg)
  # Chemins Unix (/srv/app/src/x.R) et Windows (C:\\Users\\...)
  msg <- gsub("(?:[A-Za-z]:\\\\|/)(?:[^ '\"/\\\\:]+[/\\\\])+[^ '\"/\\\\:]*", "\u2026", msg, perl = TRUE)
  msg <- trimws(msg)
  if (nchar(msg) > max_car) msg <- paste0(substr(msg, 1L, max_car - 1L), "\u2026")
  msg
}

#' Bloc « Detail technique » d'une erreur : replie, copiable, avec identifiant.
#'
#' @param id identifiant de l'indicateur (donne au mainteneur).
#' @param detail message d'erreur brut (nettoye ici).
#' @return un tag, ou NULL si le detail est desactive (CONFIG$app$detail_erreurs)
#'   ou vide.
#' @export
cf_erreur_detail <- function(id, detail) {
  if (!isTRUE(CONFIG$app$detail_erreurs)) return(NULL)
  txt <- erreur_lisible(detail)
  if (!nzchar(txt)) return(NULL)
  shiny::tags$details(
    class = "cf-details cf-erreur-detail",
    shiny::tags$summary("Détail technique"),
    shiny::div(
      class = "cf-details__corps",
      shiny::tags$dl(
        class = "cf-liste-def",
        shiny::tags$dt("Indicateur"), shiny::tags$dd(shiny::tags$code(id)),
        shiny::tags$dt("Message"),    shiny::tags$dd(shiny::tags$code(txt)),
        shiny::tags$dt("Version"),    shiny::tags$dd(CONFIG$app$version)),
      shiny::div(class = "cf-aide",
                 "À joindre à votre signalement : l'identifiant et le message suffisent.")
    )
  )
}

#' Un indicateur doit-il etre rendu en interactif ?
#'
#' Deux conditions : l'application est en mode interactif, ET l'entree du
#' registre ne s'y oppose pas. Le second cas vise les traces que ggplotly ne
#' convertit pas fidelement — voir la note en tete de la section 8 de
#' utils_plot.R.
#'
#' @param ind Entree du registre INDICATEURS.
#' @export
graphe_est_interactif <- function(ind) {
  isTRUE(CONFIG$graphiques$interactif) && !identical(ind$interactif, FALSE)
}

# ------------------------------------------------------------------------------
# Blocs de l'application, sans reactif (audit, C7)
# ------------------------------------------------------------------------------
# Le serveur d'app.R faisait 346 lignes dont quatre grands renderUI qui melaient
# lecture des reactifs et construction du HTML. La construction est ici, en
# fonctions qui prennent des valeurs et rendent des balises : on les teste sans
# lancer Shiny, et le serveur ne garde que ce qu'il est seul a savoir faire.

#' Barre de contexte : quel point, quelle reference, quelle annee.
#'
#' Sans elle, on parcourt quatre ecrans de graphiques sans plus savoir a quoi ils
#' se rapportent.
#' @param don donnees chargees ; @param p point (nom, lon, lat) ; @param c0 contexte.
#' @keywords internal
contexte_ui <- function(don, p, c0) {
  st <- attr(don, "station")
  alt_pt <- attr(don, "altitude_point")
  origine <- if (!is.null(st))
    sprintf("station <b>%s</b> (%s)", htmltools::htmlEscape(st$nom), mf_km(st$distance_km %||% NA_real_))
  else sprintf("grille <b>SAFRAN</b> 8 km%s",
               if (!is.null(alt_pt) && is.finite(alt_pt))
                 sprintf(" (point à %.0f m)", alt_pt) else "")
  shiny::div(
    class = "cf-contexte",
    shiny::div(
      shiny::div(class = "cf-contexte__lieu", cf_icone("position"), p$nom),
      shiny::div(class = "cf-contexte__meta", shiny::HTML(sprintf(
        paste("%s · référence <b>%s</b> · année comparée <b>%d</b>",
              "· série %d-%d · données jusqu'au %s"),
        origine, c0$ref_label, c0$courante,
        min(don$annee), max(don$annee),
        format(attr(don, "derniere_mesure") %||% max(don$date), "%d/%m/%Y"))))
    ),
    shiny::div(
      class = "cf-contexte__actions",
      # Copie l'adresse de la page, que app.R tient a jour apres chaque analyse
      # (updateQueryString). Bouton natif : il n'a pas a passer par le serveur, et
      # le presse-papiers exige un geste de l'utilisateur.
      shiny::tags$button(
        type = "button", class = "cf-btn cf-btn--clair", `data-copier-lien` = "1",
        cf_icone("lien"), shiny::span(class = "cf-lien-lib", "Copier le lien")),
      shiny::actionButton(
        "rouvrir_reglages",
        label = shiny::tagList(cf_icone("reglages"), "Modifier"),
        class = "cf-btn cf-btn--clair"),
      shiny::span(class = "cf-sr-only", role = "status", `aria-live` = "polite",
                  `data-lien-etat` = "1")
    )
  )
}

#' Messages du service de donnees : ce qui rate doit etre visible.
#'
#' Un « aucune donnee » sans explication est indiagnosticable, pour l'utilisateur
#' comme pour le mainteneur.
#' @param msg vecteur de messages ; @param bloquant TRUE si rien n'a pu etre charge.
#' @keywords internal
messages_service_ui <- function(msg, bloquant) {
  shiny::div(
    class = paste("cf-alerte",
                  if (bloquant) "cf-alerte--erreur" else "cf-alerte--attention"),
    shiny::span(class = "cf-alerte__titre",
                if (bloquant) "Le service de données n'a rien renvoyé"
                else "Remarques sur les données reçues"),
    shiny::tags$ul(lapply(utils::head(msg, 12), shiny::tags$li)),
    if (length(msg) > 12)
      shiny::div(class = "cf-mention",
                 sprintf("… et %d autre(s) message(s).", length(msg) - 12)),
    if (bloquant)
      shiny::div(class = "cf-aide", paste(
        "Pour un diagnostic détaillé, lancez source(\"diagnostic.R\") depuis",
        "la racine du projet : le script interroge l'API pas à pas et",
        "affiche ce qui est réellement envoyé et reçu."))
  )
}

#' Phrase qui annonce l'attente avant l'analyse.
#'
#' Station : un fichier par departement, de quelques secondes a une minute.
#' Grille SAFRAN : une requete par annee ; on compte celles deja en cache et on
#' annonce le reste (audit, S4). La duree est une estimation
#' (`CONFIG$api$duree_lot_s`), jamais presentee comme une mesure.
#' @param en_station TRUE pour une station Meteo-France.
#' @param est sortie de estimer_chargement(), ou NULL si elle n'a pu etre faite.
#' @keywords internal
phrase_attente <- function(en_station, est) {
  if (en_station) return("Premier chargement : de quelques secondes à une minute.")
  if (is.null(est)) return("Premier chargement : environ une minute.")
  if (est$n_telecharger == 0L) {
    return(sprintf("%d années déjà en cache : chargement immédiat.", est$n_total))
  }
  sprintf("%d année%s à télécharger (%s)%s.",
          est$n_telecharger, if (est$n_telecharger > 1L) "s" else "",
          phrase_duree(est$duree_s),
          if (est$n_cache > 0L) sprintf(", %d déjà en cache", est$n_cache) else "")
}

#' État affiché dans la barre d'action.
#'
#' Avant l'analyse, il dit ce qui manque ou ce qui est pret ; apres, ce qui a ete
#' charge. Le bouton n'est jamais actionne a l'aveugle.
#' @param don donnees (ou NULL) ; @param p point (ou NULL) ; @param src source
#'   choisie (liste type/station, ou NULL) ; @param perimee TRUE si la selection a
#'   change depuis l'analyse ; @param est estimation de chargement (ou NULL).
#' @keywords internal
etat_chargement_ui <- function(don, p, src, perimee, est) {
  if (is.null(don) || !nrow(don) || perimee) {
    if (is.null(p)) {
      return(shiny::span(class = "cf-mention",
                         "Aucun point sélectionné pour le moment."))
    }
    en_station <- !is.null(src) && identical(src$type, "station")
    origine <- if (en_station) sprintf(" · station %s", src$station$nom)
               else if (!is.null(src)) " · grille SAFRAN" else ""
    return(shiny::span(class = "cf-mention",
                       sprintf("Prêt : %s%s. %s", p$nom, origine,
                               phrase_attente(en_station, est))))
  }
  shiny::div(
    class = "cf-etat cf-etat--ok",
    shiny::span(class = "cf-etat__titre", cf_icone("coche"),
                sprintf("%s jours chargés, de %d à %d.",
                        format(nrow(don), big.mark = " "),
                        min(don$annee), max(don$annee))),
    shiny::span(class = "cf-etat__detail",
                sprintf("ETP : %s", attr(don, "etp_source") %||% "SAFRAN")))
}
