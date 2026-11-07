# ==============================================================================
# mod_terrain.R — ECRAN « AUTOUR DE MOI » (v2.0, sources et envoi : v2.1)
# ------------------------------------------------------------------------------
# Le profil terrain tient en un ecran de telephone : on designe un lieu (commune ou
# position de l'appareil, par le module de localisation place au-dessus), et l'ecran
# donne, pour l'annee la plus recente du jeu publie, les quelques grandeurs qui
# parlent en tournee : valeur, ecart a la normale, lecture en mots. Un graphique, au
# choix, montre d'ou vient l'ecart.
#
# SOURCES (v2.1). La valeur d'un point est celle de sa maille SAFRAN (un modele). Quand le
# jeu publie des stations Meteo-France (des mesures), les plus proches du point sont
# proposees : on les coche, et chaque grandeur montre la mesure a cote du modele. Rien ne
# s'affiche de plus quand le jeu n'a pas de stations.
#
# SYNTHESE A ENVOYER (v2.1). Le bas de l'ecran prepare une page courte, a envoyer par
# courriel : voir src/utils_envoi.R pour ce que fait, et ne fait pas, l'application.
#
# CE QUE L'ECRAN NE FAIT PAS. Il ne saisit pas de fiche : c'est le role de
# l'application de terrain (Sylvasan). Quand CONFIG$sylvasan$url_saisie est renseignee,
# un bouton y renvoie ; sinon il n'y a pas de bouton.
#
# Les chiffres sont ceux du bilan synthetique (src/utils_synthese.R) : un seul calcul.
#
# Parametres du serveur : voir mod_synthese_server (profil, lieu, transport, mobile,
# executer) ; `altitude` : function(lon, lat) -> metres ou NA (injectable : les tests n'ont
# pas de reseau).
# ==============================================================================

#' @export
mod_terrain_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("indisponible")),
    shiny::uiOutput(ns("resultat")),
    shiny::uiOutput(ns("sources")),
    shiny::uiOutput(ns("grandeurs")),
    shiny::uiOutput(ns("envoi")),
    shiny::uiOutput(ns("saisie"))
  )
}

#' Une carte de grandeur : valeur, ecart, lecture en mots. Pas de couleur de jugement : un
#' ecart positif est bon pour la pluie, mauvais pour la chaleur.
#'
#' Une seule source : la valeur en grand. Plusieurs (maille et stations) : une ligne par
#' source, nommee pour ce qu'elle est, mesure ou modele.
#' @param r lignes de cmp_bilan() d'UNE grandeur (une par source) ; @param etiquettes nom
#'   affiche de chaque lieu (envoi_etiquettes()).
#' @keywords internal
terrain_carte_ui <- function(r, ctx, etiquettes = NULL) {
  ligne <- function(x) {
    unite <- x$unite
    lecture <- if (is.finite(x$ecart)) synthese_lecture_ecart(x$classe, x$ecart, x$ecart_type_ref) else "normale non calculable"
    list(valeur = fmt_val(x$valeur, unite, x$decimales),
         ecart = if (is.finite(x$ecart)) sprintf("%s par rapport à la normale %s", fmt_ecart(x$ecart, unite, x$decimales), ctx$ref_label) else lecture,
         lecture = if (is.finite(x$ecart)) lecture)
  }
  if (nrow(r) == 1L) {
    l <- ligne(r[1, ])
    return(shiny::tags$li(
      class = "cf-terrain__carte",
      shiny::div(class = "cf-terrain__titre", r$titre[1]),
      shiny::div(class = "cf-terrain__valeur", l$valeur),
      shiny::div(class = "cf-terrain__ecart", l$ecart),
      if (!is.null(l$lecture)) shiny::div(class = "cf-terrain__lecture", l$lecture)))
  }
  shiny::tags$li(
    class = "cf-terrain__carte cf-terrain__carte--sources",
    shiny::div(class = "cf-terrain__titre", r$titre[1]),
    shiny::tags$ul(class = "cf-terrain__sources", lapply(seq_len(nrow(r)), function(i) {
      l <- ligne(r[i, ])
      nom <- if (!is.null(etiquettes) && r$lieu[i] %in% names(etiquettes)) etiquettes[[r$lieu[i]]] else r$lieu[i]
      shiny::tags$li(
        shiny::div(class = "cf-terrain__source", nom),
        shiny::div(class = "cf-terrain__valeur cf-terrain__valeur--source", l$valeur),
        shiny::div(class = "cf-terrain__ecart", l$ecart),
        if (!is.null(l$lecture)) shiny::div(class = "cf-terrain__lecture", l$lecture))
    })))
}

#' @export
mod_terrain_server <- function(id, profil, lieu,
                               transport = shiny::reactive(carte_transport()),
                               mobile = shiny::reactive(FALSE),
                               executer = httr2::req_perform,
                               altitude = mf_altitude) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    avec_sf <- requireNamespace("sf", quietly = TRUE)

    jeu_brut <- shiny::reactive({
      tryCatch(cmp_jeu(transport(), executer = executer), error = function(e) {
        journaliser_erreur("Terrain : fichiers de la carte", e)
        e
      })
    })
    jeu <- shiny::reactive({ j <- jeu_brut(); if (inherits(j, "error")) NULL else j })

    output$indisponible <- shiny::renderUI({
      if (!avec_sf) {
        return(shiny::div(class = "cf-alerte cf-alerte--attention", role = "status",
          shiny::span(class = "cf-alerte__titre", "Écran indisponible sur ce serveur"),
          "Le paquet R « sf » n'y est pas installé."))
      }
      if (!inherits(jeu_brut(), "error")) return(NULL)
      shiny::div(class = "cf-alerte cf-alerte--info", role = "status",
        shiny::span(class = "cf-alerte__titre", "Les données ne sont pas disponibles"),
        "Les valeurs précalculées ne se chargent pas pour le moment.")
    })

    # -- Les sources du point : la maille, et les stations publiees proches ---------------
    src_pt <- shiny::reactive({
      p <- lieu(); j <- jeu()
      shiny::req(p, j, avec_sf)
      # L'altitude sert a juger l'ecart avec chaque station ; sans elle (service IGN
      # injoignable), ce critere est ignore. On ne la demande que si le jeu a des stations.
      alt <- if (isTRUE(cmp_stations(j, executer = executer)$ok))
        tryCatch(altitude(p$lon, p$lat), error = function(e) NA_real_) else NA_real_
      tryCatch(sources_point(j, p$lon, p$lat, alt, executer),
               error = function(e) {
                 journaliser_erreur("Terrain : stations proches", e)
                 list(stations = NULL, publiees = FALSE, raison = "invalide")
               })
    })

    output$sources <- shiny::renderUI({
      if (is.null(lieu())) return(NULL)
      s <- tryCatch(src_pt(), error = function(e) NULL)
      if (is.null(s) || !isTRUE(s$publiees) || is.null(s$stations) || !nrow(s$stations)) return(NULL)
      shiny::div(class = "cf-carte",
                 sources_choix_ui(ns("sources_choix"), s, shiny::isolate(input$sources_choix)))
    })

    # Le bilan du point, pour l'annee la plus recente du jeu.
    res <- shiny::reactive({
      p <- lieu(); j <- jeu()
      shiny::req(p, j, avec_sf)
      src <- src_pt()
      cles <- intersect(CONFIG$synthese$base$terrain$t, names(synthese_catalogue(j, profil)))
      a <- max(j$annees)
      lieux <- sources_lieux(p, input$sources_choix %||% sources_defaut(src), src)
      r <- tryCatch(
        synthese_preparer("point", lieux, NULL, j, cles, cles, ctx_creer(affichage = j$annees, courante = a), a, profil, executer),
        error = function(e) { journaliser_erreur("Terrain : calcul", e); NULL })
      if (!is.null(r)) {
        r$point_nom <- sources_nom_point(p)
        r$details <- sources_details(lieux, src)
      }
      r
    })

    # Le message d'erreur, ou NULL quand le bilan est affichable.
    probleme <- shiny::reactive({
      if (is.null(lieu())) {
        return(shiny::div(class = "cf-carte",
          shiny::p("Désignez un lieu ci-dessus : une commune, ou « Utiliser ma position »."),
          shiny::p(class = "cf-mention", "Vous pouvez aussi toucher une maille sur la carte.")))
      }
      shiny::req(jeu())
      r <- res()
      if (is.null(r)) {
        return(shiny::div(class = "cf-alerte cf-alerte--attention", role = "status",
          shiny::span(class = "cf-alerte__titre", "Le calcul n'a pas pu aboutir"),
          "L'erreur est consignée dans le journal."))
      }
      if (!any(r$tl$retenu)) {
        return(shiny::div(class = "cf-alerte cf-alerte--attention", role = "status",
          shiny::span(class = "cf-alerte__titre", "Lieu hors de la grille SAFRAN"),
          "Aucune maille à proximité : essayez un lieu en France métropolitaine."))
      }
      NULL
    })

    output$resultat <- shiny::renderUI({
      pb <- probleme()
      if (!is.null(pb)) return(pb)
      r <- res()
      shiny::div(
        class = "cf-carte",
        shiny::h2(class = "cf-carte__titre", sprintf("%s — %d", r$point_nom, r$annee)),
        synthese_bref_ui(utils::head(synthese_phrases_generales(r), 2L)),
        synthese_remarques_ui(r$tl))
    })

    output$grandeurs <- shiny::renderUI({
      if (!is.null(probleme())) return(NULL)
      r <- res()
      eti <- envoi_etiquettes(r)
      b <- r$bilan[r$bilan$cle %in% r$cles_t, , drop = FALSE]
      cles <- unique(b$cle)
      cles <- cles[order(match(cles, names(CARTE_COUCHES)))]
      b <- b[order(match(b$lieu, r$tl$lieu)), , drop = FALSE]
      mention <- if (any(r$tl$source == "station")) {
        "Les courbes montrent la mesure des stations à côté du modèle (maille de 8 km) : elles ne sont pas censées coïncider."
      } else {
        sprintf("Valeur de la maille de 8 km (%s), comparée à la normale %s. Ce n'est pas la mesure d'une station.",
                r$tl$note[1], r$ctx$ref_label)
      }
      cg <- cles[cles %in% r$cles_g]
      shiny::tagList(
        shiny::tags$ul(class = "cf-terrain__liste", lapply(cles, function(k) terrain_carte_ui(b[b$cle == k, , drop = FALSE], r$ctx, eti))),
        shiny::div(
          class = "cf-carte",
          shiny::h3(class = "cf-carte__titre", "Depuis 1959"),
          shiny::selectInput(ns("cle"), "Grandeur", selectize = FALSE, width = "100%",
                             choices = stats::setNames(cg, vapply(cg, function(k) CARTE_COUCHES[[k]]$titre, "")),
                             selected = shiny::isolate(input$cle) %||% cg[1]),
          shiny::plotOutput(ns("graphe"), height = "300px"),
          shiny::p(class = "cf-mention", mention)))
    })

    output$graphe <- shiny::renderPlot({
      r <- res(); cle <- input$cle
      shiny::req(r, cle, cle %in% r$cles_g)
      synthese_graphe_serie(r, cle, mobile = mobile())
    }, res = 96)

    # -- La synthese a envoyer ---------------------------------------------------------------
    env <- shiny::reactive({
      shiny::req(is.null(probleme()))
      tryCatch(envoi_synthese(res()), error = function(e) { journaliser_erreur("Terrain : synthèse à envoyer", e); NULL })
    })

    output$envoi <- shiny::renderUI({
      if (!is.null(probleme())) return(NULL)
      e <- env()
      if (is.null(e)) return(NULL)
      shiny::div(
        class = "cf-carte cf-envoi", `data-envoi-zone` = "1",
        shiny::h3(class = "cf-carte__titre", "Synthèse à envoyer"),
        shiny::p("Une page courte : le lieu, les valeurs, leurs sources et les précautions de lecture. ",
                 "Elle ne contient pas les coordonnées du point, seulement son nom."),
        synthese_bref_ui(e$resume),
        shiny::div(
          class = "cf-envoi__actions",
          shiny::tags$a(class = "cf-btn cf-btn--principal", href = e$mailto, "Envoyer par courriel"),
          shiny::tags$button(type = "button", class = "cf-btn cf-btn--sec", `data-envoi` = "partager", hidden = NA, "Partager…"),
          shiny::downloadButton(ns("html"), "Enregistrer la page", class = "cf-btn cf-btn--sec"),
          shiny::tags$button(type = "button", class = "cf-btn cf-btn--sec", `data-envoi` = "copier", "Copier le texte")),
        shiny::div(class = "cf-envoi__etat cf-mention", role = "status", `aria-live` = "polite", `data-envoi-etat` = "1"),
        shiny::p(class = "cf-mention", paste(
          "« Envoyer par courriel » ouvre votre messagerie sur un message déjà rédigé : ajoutez le destinataire.",
          "Aucun message ne part de ClimaFor. Un lien ne peut pas porter de pièce jointe : pour joindre la page complète,",
          "enregistrez-la puis ajoutez-la au message, ou utilisez « Partager » si votre appareil le propose.")),
        if (e$tronque > 0L) shiny::p(class = "cf-mention", sprintf(
          "Le message court contient les premières grandeurs (%d de moins que la page) ; la page enregistrée les contient toutes.", e$tronque)),
        shiny::tags$details(
          class = "cf-details",
          shiny::tags$summary("Voir le message"),
          shiny::div(class = "cf-details__corps",
                     shiny::p(shiny::strong("Objet : "), e$objet),
                     shiny::tags$pre(class = "cf-envoi__texte", e$texte_court))),
        if (!is.null(e$tableau)) shiny::tags$details(
          class = "cf-details",
          shiny::tags$summary("Voir le tableau de la synthèse"),
          shiny::div(class = "cf-details__corps",
                     synthese_table_tag(e$tableau, sprintf("Valeurs de %d et écart à la normale", e$annee)))),
        envoi_donnees_ui(e))
    })

    output$html <- shiny::downloadHandler(
      filename = function() { e <- env(); shiny::req(e); e$nom_fichier },
      content = function(file) { e <- env(); shiny::req(e); envoi_ecrire_html(e, file) },
      contentType = "text/html")

    output$saisie <- shiny::renderUI({
      u <- CONFIG$sylvasan$url_saisie
      if (!nzchar(u)) return(NULL)
      shiny::div(class = "cf-carte",
        shiny::tags$a(class = "cf-btn cf-btn--principal", href = u, target = "_blank", rel = "noopener",
                      "Saisir une fiche dans l'application de terrain"))
    })

    invisible(NULL)
  })
}
