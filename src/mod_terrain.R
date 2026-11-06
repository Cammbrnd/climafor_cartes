# ==============================================================================
# mod_terrain.R — ECRAN « AUTOUR DE MOI » (v2.0)
# ------------------------------------------------------------------------------
# Le profil terrain tient en un ecran de telephone : on designe un lieu (commune ou
# position de l'appareil, par le module de localisation place au-dessus), et l'ecran
# donne, pour l'annee la plus recente du jeu publie, les quelques grandeurs qui
# parlent en tournee : valeur, ecart a la normale, lecture en mots. Un graphique, au
# choix, montre d'ou vient l'ecart.
#
# CE QUE L'ECRAN NE FAIT PAS. Il ne saisit pas de fiche : c'est le role de
# l'application de terrain (Sylvasan). Quand CONFIG$sylvasan$url_saisie est renseignee,
# un bouton y renvoie ; sinon il n'y a pas de bouton.
#
# Les chiffres sont ceux du bilan synthetique (src/utils_synthese.R) : un seul calcul.
#
# Parametres du serveur : voir mod_synthese_server (profil, lieu, transport, mobile,
# executer).
# ==============================================================================

#' @export
mod_terrain_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("indisponible")),
    shiny::uiOutput(ns("resultat")),
    shiny::uiOutput(ns("saisie"))
  )
}

#' Une carte de grandeur : valeur, ecart, lecture en mots. Pas de couleur de jugement : un
#' ecart positif est bon pour la pluie, mauvais pour la chaleur.
#' @param r une ligne de cmp_bilan().
#' @keywords internal
terrain_carte_ui <- function(r, ctx) {
  unite <- r$unite
  lecture <- if (is.finite(r$ecart)) synthese_lecture_ecart(r$classe, r$ecart, r$ecart_type_ref) else "normale non calculable"
  shiny::tags$li(
    class = "cf-terrain__carte",
    shiny::div(class = "cf-terrain__titre", r$titre),
    shiny::div(class = "cf-terrain__valeur", fmt_val(r$valeur, unite, r$decimales)),
    shiny::div(class = "cf-terrain__ecart", if (is.finite(r$ecart))
      sprintf("%s par rapport à la normale %s", fmt_ecart(r$ecart, unite, r$decimales), ctx$ref_label) else lecture),
    if (is.finite(r$ecart)) shiny::div(class = "cf-terrain__lecture", lecture))
}

#' @export
mod_terrain_server <- function(id, profil, lieu,
                               transport = shiny::reactive(carte_transport()),
                               mobile = shiny::reactive(FALSE),
                               executer = httr2::req_perform) {
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

    # Le bilan du point, pour l'annee la plus recente du jeu.
    res <- shiny::reactive({
      p <- lieu(); j <- jeu()
      shiny::req(p, j, avec_sf)
      cles <- intersect(CONFIG$synthese$base$terrain$t, names(synthese_catalogue(j, profil)))
      a <- max(j$annees)
      tryCatch(
        synthese_preparer("point", list(cmp_lieu_point(cmp_nom_propre(p$nom %||% "Point choisi", "Point choisi"), p$lon, p$lat)),
                          NULL, j, cles, cles, ctx_creer(affichage = j$annees, courante = a), a, profil, executer),
        error = function(e) { journaliser_erreur("Terrain : calcul", e); NULL })
    })

    output$resultat <- shiny::renderUI({
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
      b <- r$bilan[match(r$cles_t, r$bilan$cle), , drop = FALSE]
      b <- b[!is.na(b$cle), , drop = FALSE]
      shiny::tagList(
        shiny::div(
          class = "cf-carte",
          shiny::h2(class = "cf-carte__titre", sprintf("%s — %d", r$tl$lieu[1], r$annee)),
          synthese_bref_ui(utils::head(synthese_phrases_generales(r), 2L))),
        shiny::tags$ul(class = "cf-terrain__liste", lapply(seq_len(nrow(b)), function(i) terrain_carte_ui(b[i, ], r$ctx))),
        shiny::div(
          class = "cf-carte",
          shiny::h3(class = "cf-carte__titre", "Depuis 1959"),
          shiny::selectInput(ns("cle"), "Grandeur", selectize = FALSE, width = "100%",
                             choices = stats::setNames(b$cle, b$titre),
                             selected = shiny::isolate(input$cle) %||% b$cle[1]),
          shiny::plotOutput(ns("graphe"), height = "300px"),
          shiny::p(class = "cf-mention", sprintf(
            "Valeur de la maille de 8 km (%s), comparée à la normale %s. Ce n'est pas la mesure d'une station.",
            r$tl$note[1], r$ctx$ref_label))))
    })

    output$graphe <- shiny::renderPlot({
      r <- res(); cle <- input$cle
      shiny::req(r, cle, cle %in% r$cles_g)
      synthese_graphe_serie(r, cle, mobile = mobile())
    }, res = 96)

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
