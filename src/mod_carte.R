# ==============================================================================
# mod_carte.R — ONGLET CARTE
# ------------------------------------------------------------------------------
# Affiche, pour toute la France, un indicateur annuel par maille SAFRAN, avec un
# degrade de couleur : sa valeur, son ecart a la normale ou sa tendance, annee
# par annee.
#
# CE QUE FAIT SHINY, ET CE QU'IL NE FAIT PAS
# ------------------------------------------
# Shiny ne sert ici que de squelette (le HTML ci-dessous) et de passerelle : il
# dit au navigateur ou sont les fichiers (carte_transport), puis recoit le clic
# sur une maille. TOUT le reste — chargement des valeurs, choix de l'indicateur,
# curseur d'annee, animation, recoloration — se passe dans le navigateur
# (www/carte/carte.js), sans aller-retour avec le serveur. C'est ce qui rend le
# changement d'annee instantane : une carte Shiny classique redessine ses
# polygones et renvoie leurs donnees par le websocket a chaque changement.
#
# Le navigateur ne charge MapLibre et les fichiers qu'a la PREMIERE ouverture de
# l'onglet : les autres onglets ne paient rien.
#
# CONTRAT
# -------
#   mod_carte_server(id, onglet, ouvrir_maille, ouvrir_station = NULL)
#     onglet          reactive() -> identifiant de l'onglet affiche
#     ouvrir_maille   function(point) appelee avec list(lon, lat, nom, source)
#                     quand l'utilisateur demande l'analyse d'une maille
#     ouvrir_station  function(point, id) appelee quand il demande l'analyse d'une station
#                     (point : sa position, id : son identifiant) ; NULL quand le profil n'a
#                     pas d'analyse de station : la fiche de la station s'affiche sans bouton
#
# STATIONS. carte.js lit stations.json (publie a cote de meta.json) et pose les stations en
# points sur les mailles. Le navigateur ne renvoie que l'IDENTIFIANT d'une station : le
# serveur en retrouve la position et le nom dans sa propre lecture du fichier
# (stations_point_depuis_id), comme il assainit le clic sur une maille.
# ==============================================================================

#' Interface de l'onglet Carte : le squelette que carte.js anime.
#' @export
mod_carte_ui <- function(id) {
  ns <- shiny::NS(id)

  mode_radio <- function(valeur, libelle, coche = FALSE) {
    shiny::tags$label(
      class = "cf-carto__mode",
      shiny::tags$input(type = "radio", name = ns("mode"), value = valeur,
                        `data-role` = "mode", checked = if (coche) NA),
      shiny::span(libelle, `data-mode` = valeur))
  }

  shiny::tagList(
    shiny::h2(class = "cf-titre-section", "Carte des indicateurs"),
    shiny::p(class = "cf-chapeau", paste(
      "Un indicateur climatique forestier pour chaque maille de 8 km de la grille",
      "SAFRAN : sa valeur une année donnée, son écart à la normale ou sa tendance",
      "depuis 1959. Cliquez sur une maille, ou sur une station Météo-France, pour la lire",
      "et l'analyser en détail.")),

    shiny::uiOutput(ns("indisponible")),

    shiny::div(
      id = ns("racine"), class = "cf-carto", `data-cf-carto` = "1", hidden = "hidden",

      # -- Reglages ----------------------------------------------------------
      shiny::div(
        class = "cf-carto__barre",
        shiny::div(
          class = "cf-carto__champ",
          shiny::tags$label(`for` = ns("couche"), "Indicateur"),
          shiny::tags$select(id = ns("couche"), class = "cf-champ", `data-role` = "couche")),
        shiny::tags$fieldset(
          class = "cf-carto__modes",
          shiny::tags$legend("Affichage"),
          mode_radio("valeur", "Valeur de l'année", coche = TRUE),
          mode_radio("ecart", "Écart à la normale"),
          mode_radio("tendance", "Tendance")),
        shiny::div(
          class = "cf-carto__champ",
          shiny::tags$label(`for` = ns("annee"), "Année : ",
                            shiny::tags$output(`data-role` = "annee-sortie")),
          shiny::div(
            class = "cf-carto__annee",
            shiny::tags$input(id = ns("annee"), type = "range", step = 1,
                              `data-role` = "annee"),
            shiny::tags$button(type = "button", class = "cf-btn cf-btn--sec",
                               `data-role` = "lecture", "Lecture")))
      ),

      # -- Carte et fiche de la maille -----------------------------------------
      shiny::div(
        class = "cf-carto__scene",
        shiny::div(class = "cf-carto__carte", `data-role` = "carte",
                   role = "region", `aria-label` = "Carte des mailles SAFRAN"),
        shiny::tags$aside(
          class = "cf-carto__info", `data-role` = "info", `aria-live` = "polite",
          shiny::p(class = "cf-mention",
                   "Cliquez sur une maille pour lire ses valeurs."))
      ),

      # -- Stations Meteo-France : carte.js affiche ce bloc si le jeu publie en porte ----
      shiny::div(
        class = "cf-carto__stations", `data-role` = "stations-bloc", hidden = "hidden",
        shiny::tags$label(
          class = "cf-carto__case",
          shiny::tags$input(type = "checkbox", `data-role` = "stations", checked = NA),
          shiny::span("Afficher les stations Météo-France (clic : fiche)")),
        # Les types de station : une case par ce que la station mesure. carte.js les remplit (effectifs)
        # et les montre quand les stations sont chargees. Les pastilles reprennent le rendu des points.
        shiny::div(
          class = "cf-carto__types", `data-role` = "stations-types", hidden = "hidden",
          role = "group", `aria-label` = "Types de station affichés",
          shiny::tags$label(
            class = "cf-carto__case cf-carto__case--type",
            shiny::tags$input(type = "checkbox", `data-role` = "station-type", `data-type` = "complete", checked = NA),
            shiny::span(class = "cf-carto__pastille cf-carto__pastille--complete", `aria-hidden` = "true"),
            shiny::span("Pluie et températures"),
            shiny::span(class = "cf-mention", `data-count` = "")),
          shiny::tags$label(
            class = "cf-carto__case cf-carto__case--type",
            shiny::tags$input(type = "checkbox", `data-role` = "station-type", `data-type` = "pluie", checked = NA),
            shiny::span(class = "cf-carto__pastille cf-carto__pastille--pluie", `aria-hidden` = "true"),
            shiny::span("Pluie seule"),
            shiny::span(class = "cf-mention", `data-count` = "")),
          shiny::tags$label(
            class = "cf-carto__case cf-carto__case--type",
            shiny::tags$input(type = "checkbox", `data-role` = "station-type", `data-type` = "temp", checked = NA),
            shiny::span(class = "cf-carto__pastille cf-carto__pastille--temp", `aria-hidden` = "true"),
            shiny::span("Températures seules"),
            shiny::span(class = "cf-mention", `data-count` = ""))),
        shiny::span(class = "cf-mention", `data-role` = "stations-note")),

      shiny::div(class = "cf-carto__legende", `data-role` = "legende"),
      shiny::p(class = "cf-mention", `data-role` = "etat", role = "status"),

      # -- Lecture, methode et alternative au clavier --------------------------
      shiny::tags$details(
        class = "cf-details",
        shiny::tags$summary("Lecture et méthode"),
        shiny::div(class = "cf-details__corps", `data-role` = "methode")),
      shiny::tags$details(
        class = "cf-details",
        shiny::tags$summary("Mailles extrêmes (accès au clavier)"),
        shiny::div(class = "cf-details__corps cf-tableau-defile", `data-role` = "liste"))
    )
  )
}

#' Serveur de l'onglet Carte.
#' @export
mod_carte_server <- function(id, onglet, ouvrir_maille, ouvrir_station = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    transport <- shiny::reactiveVal(NULL)
    initialise <- shiny::reactiveVal(FALSE)

    # Premiere ouverture de l'onglet : on cherche les fichiers, puis on previent
    # le navigateur. Avant, rien n'est charge.
    shiny::observeEvent(onglet(), {
      if (!identical(onglet(), "carte") || isTRUE(initialise())) return()
      initialise(TRUE)
      tr <- tryCatch(carte_transport(), error = function(e) {
        list(mode = "absent", base = NULL, erreurs = "Les données de la carte n'ont pas pu être préparées.")
      })
      transport(tr)
      if (!identical(tr$mode, "absent")) {
        session$sendCustomMessage("cf-carte-init", list(
          racine = ns("racine"), entree = ns("clic"), base = tr$base,
          tuiles = CONFIG$api$wmts_ign_plan, schema = CONFIG$carte$schema,
          # "" : pas de stations (recherche de stations desactivee)
          stations = if (isTRUE(CONFIG$stations$actif)) CONFIG$stations$publie$fichier else "",
          schema_stations = CONFIG$stations$publie$schema,
          entree_station = ns("clic_station"), analyse_station = is.function(ouvrir_station)))
      }
    }, ignoreInit = FALSE)

    output$indisponible <- shiny::renderUI({
      tr <- transport()
      if (is.null(tr) || !identical(tr$mode, "absent")) return(NULL)
      shiny::div(
        class = "cf-alerte cf-alerte--info", role = "status",
        shiny::span(class = "cf-alerte__titre", "La carte n'est pas encore disponible"),
        shiny::tags$ul(lapply(utils::head(tr$erreurs, 6), shiny::tags$li)),
        shiny::p(paste(
          "Les valeurs de la carte sont produites à part, par un calcul planifié.",
          "En attendant, l'onglet Accueil analyse n'importe quel point de France.")))
    })

    # Clic sur une maille : le navigateur envoie une entree comme une autre, on
    # ne retient que ce qui passe carte_point_depuis_clic.
    shiny::observeEvent(input$clic, {
      p <- carte_point_depuis_clic(input$clic)
      if (is.null(p)) {
        shiny::showNotification("Cette maille n'a pas pu être ouverte.",
                                type = "warning", duration = 6)
        return()
      }
      ouvrir_maille(p)
    })

    # Clic sur « Analyser cette station » : on ne retient que l'identifiant, et la station doit
    # exister dans la table publiee que LE SERVEUR a lue.
    shiny::observeEvent(input$clic_station, {
      if (!is.function(ouvrir_station)) return()
      id <- if (is.list(input$clic_station)) input$clic_station$id else NULL
      p <- stations_point_depuis_id(id, stations_publiees_table(transport()))
      if (is.null(p)) {
        shiny::showNotification("Cette station n'a pas pu être ouverte.", type = "warning", duration = 6)
        return()
      }
      ouvrir_station(p, p$station)
    })
  })
}

#' Ouverture de l'analyse d'une maille choisie sur la carte.
#'
#' Un clic sur une maille demande l'analyse de SON point, avec la grille SAFRAN
#' (la maille EST une maille SAFRAN : une station voisine n'aurait pas de sens).
#' On pose le point, puis on attend que la recherche de stations l'ait enregistre
#' avant de lancer : lancer tout de suite utiliserait la source de l'ANCIEN point.
#' Meme principe que la restauration d'un lien partage, avec une precaution de
#' plus : `source_donnees()$point` doit etre le nouveau point.
#'
#' A appeler depuis le serveur de l'application, une fois pour toutes.
#'
#' @param lieu reactiveVal du point choisi (mod_localisation_server).
#' @param choix_initial reactiveVal de la source demandee a la prochaine recherche.
#' @param source_donnees reactive de la source en vigueur (mod_station_server).
#' @param lancer function() qui lance l'analyse (lancer_analyse du serveur).
#' @return function(point, source = "safran") : `ouvrir_maille` de mod_carte_server() ; avec
#'   `source` = identifiant d'une station, c'est l'analyse de cette station (`ouvrir_station`).
#' @export
mod_carte_ouverture_server <- function(lieu, choix_initial, source_donnees, lancer) {
  a_ouvrir <- shiny::reactiveVal(NULL)

  shiny::observe({
    p <- a_ouvrir()
    if (is.null(p)) return()
    if (isTRUE(CONFIG$stations$actif)) {
      s <- source_donnees()
      pt <- if (is.null(s)) NULL else s$point
      if (is.null(pt) || !isTRUE(all.equal(c(pt$lon, pt$lat), c(p$lon, p$lat)))) return()
    }
    a_ouvrir(NULL)
    shiny::removeNotification("carte_attente")
    shiny::isolate(lancer())
  })

  function(p, source = "safran") {
    shiny::showNotification(if (identical(source, "safran")) "Analyse de la maille en préparation…"
                            else "Analyse de la station en préparation…",
                            id = "carte_attente", type = "message", duration = 8)
    # Meme point que celui deja choisi : aucune nouvelle recherche de stations ne
    # partira, la demande de source ne serait donc jamais consommee (et s'appliquerait
    # a tort au point suivant). L'analyse part alors avec la source en vigueur.
    if (!identical(shiny::isolate(lieu()), p)) choix_initial(source)
    lieu(p)
    a_ouvrir(p)
  }
}
