# ==============================================================================
# mod_periode.R — MODULE "QUAND ?"
# ------------------------------------------------------------------------------
# Quatre dimensions temporelles independantes :
#   1. la periode de reference (la normale a laquelle on compare) ;
#   2. l'annee mise en avant ;
#   3. la plage affichee sur les series annuelles ;
#   4. la fenetre mensuelle du P - ETP.
#
# CHOIX D'INTERFACE (v2) : SEULES LES DEUX PREMIERES SONT VISIBLES.
# ----------------------------------------------------------------
# Les deux dernieres sont des reglages de confort : leurs valeurs par defaut
# conviennent dans l'immense majorite des cas, et les modifier ne change pas
# la nature du bilan. Les afficher au meme niveau que la reference laissait
# croire qu'il fallait statuer sur les quatre avant de pouvoir lancer quoi que
# ce soit — c'est la principale source d'hesitation constatee sur la v1.
#
# Elles restent a un clic, dans un bloc replie. Elles ne sont jamais retirees
# du DOM : leurs valeurs sont donc toujours transmises au serveur, replie ou
# non. Le fichier www/app.js emet un evenement "resize" au depliage, sans quoi
# le curseur ion.rangeSlider, construit dans un conteneur masque, s'afficherait
# sur quelques pixels de large.
#
# Contrat de sortie : reactive() -> contexte ctx_creer(). Inchange.
#
# (v1.4) VALEURS INITIALES VENUES D'UN LIEN PARTAGE
# -------------------------------------------------
# `etat` (partage_decoder) fixe les valeurs de depart des champs : le lien d'un
# collegue rouvre la meme periode. Les champs sont ecrits dans le HTML de la
# page, pas modifies apres coup : le serveur lit donc des le premier calcul les
# valeurs du lien, sans etat intermediaire ou l'on verrait les valeurs par
# defaut. Sans `etat`, rien ne change.
# ==============================================================================

mod_periode_ui <- function(id, etat = NULL) {
  ns <- shiny::NS(id)
  annee_max <- as.integer(format(Sys.Date(), "%Y"))

  # -- Valeurs de depart : le lien partage s'il y en a un, sinon les defauts ----
  preset_ref <- if (is.null(etat)) CONFIG$reference_defaut else partage_preset_reference(etat$ref)
  ref_deb <- if (is.null(etat)) 1991L else etat$ref[1]
  ref_fin <- if (is.null(etat)) 2020L else etat$ref[2]
  courante <- if (is.null(etat)) annee_max else etat$courante
  plage <- if (is.null(etat)) c(1970L, annee_max) else etat$plage
  preset_fen <- if (is.null(etat)) "Saison de végétation (avr-oct)"
                else partage_preset_fenetre(etat$fenetre)
  mois <- if (is.null(etat)) c(4L, 10L) else etat$fenetre
  # Le bloc avance s'ouvre si le lien y a deplace un reglage : on ne cache pas
  # a un collegue ce qui differe de ses valeurs habituelles.
  avance_modifie <- !is.null(etat) &&
    (!identical(as.integer(plage), c(1970L, annee_max)) ||
       !identical(as.integer(etat$fenetre), as.integer(CONFIG$fenetre_defaut)))

  shiny::div(
    class = "cf-panneau cf-panneau--simple",
    shiny::div(
      class = "cf-panneau__corps",

      # -- ESSENTIEL : la reference et l'annee qu'on lui compare -------------
      shiny::selectInput(
        ns("preset"), "Période de référence",
        choices = c(names(CONFIG$references), "Personnalisée…" = "custom"),
        selected = preset_ref, width = "100%",
        # Liste courte : un <select> natif est relie a son etiquette, s'ouvre
        # avec le selecteur du telephone et ne charge aucun JavaScript.
        selectize = FALSE),

      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'custom'", ns("preset")),
        shiny::div(
          class = "cf-duo",
          shiny::numericInput(ns("ref_debut"), "Référence — début", value = ref_deb,
                              min = CONFIG$annee_min, max = annee_max, step = 1),
          shiny::numericInput(ns("ref_fin"), "Référence — fin", value = ref_fin,
                              min = CONFIG$annee_min, max = annee_max, step = 1))
      ),

      shiny::numericInput(
        ns("courante"), "Année comparée",
        value = courante, min = CONFIG$annee_min, max = annee_max,
        step = 1, width = "100%"),

      shiny::div(
        class = "cf-aide",
        "La normale 1991-2020 est la référence en vigueur à l'OMM ; 1961-1990 ",
        "sert de repère antérieur à l'essentiel du réchauffement récent (ce n'est ",
        "pas la base pré-industrielle du GIEC)."),

      # -- SECONDAIRE : replie par defaut ------------------------------------
      cf_repli(
        "Réglages avancés — plage affichée, fenêtre du P − ETP",
        ouvert = avance_modifie,

        shiny::sliderInput(
          ns("plage"), "Années affichées sur les graphiques",
          min = CONFIG$annee_min, max = annee_max,
          value = plage, step = 1, sep = "", width = "100%"),

        shiny::selectInput(
          ns("fenetre_preset"), "Fenêtre de calcul du P − ETP",
          choices = c(names(CONFIG$fenetres), "Personnalisée…" = "custom"),
          selected = preset_fen, width = "100%",
          selectize = FALSE),

        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'custom'", ns("fenetre_preset")),
          shiny::div(
            class = "cf-duo",
            shiny::selectInput(ns("mois_debut"), "Du mois de",
                               choices = stats::setNames(1:12, MOIS_LONG), selected = mois[1],
                               selectize = FALSE),
            shiny::selectInput(ns("mois_fin"), "au mois de",
                               choices = stats::setNames(1:12, MOIS_LONG), selected = mois[2],
                               selectize = FALSE))
        ),

        shiny::div(
          class = "cf-aide",
          "Juin-août mesure le stress de pointe, avril-octobre le bilan de la ",
          "saison de végétation. La plage affichée ne modifie pas les calculs : ",
          "elle ne fait que cadrer les graphiques de séries annuelles.")
      )
    )
  )
}

#' @param seuils reactive() -> liste de seuils (fournie par mod_parametres).
#'   Injectee explicitement plutot que lue dans une globale : le module reste
#'   testable isolement.
#' @return reactive() -> contexte ctx_creer()
mod_periode_server <- function(id, seuils = shiny::reactive(CONFIG$seuils)) {
  shiny::moduleServer(id, function(input, output, session) {

    reference <- shiny::reactive({
      if (identical(input$preset, "custom")) {
        d <- input$ref_debut; f <- input$ref_fin
        shiny::validate(shiny::need(
          isTRUE(is.finite(d)) && isTRUE(is.finite(f)) && f >= d,
          "Période de référence invalide : l'année de fin doit suivre l'année de début."))
        c(max(CONFIG$annee_min, d), f)
      } else {
        CONFIG$references[[input$preset]]
      }
    })

    fenetre <- shiny::reactive({
      if (identical(input$fenetre_preset, "custom")) {
        c(as.integer(input$mois_debut), as.integer(input$mois_fin))
      } else {
        CONFIG$fenetres[[input$fenetre_preset]]
      }
    })

    shiny::reactive({
      ctx_creer(
        ref       = reference(),
        affichage = seq.int(input$plage[1], input$plage[2]),
        courante  = input$courante,
        fenetre   = fenetre(),
        seuils    = seuils())
    })
  })
}
