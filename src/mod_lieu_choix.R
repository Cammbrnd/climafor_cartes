# ==============================================================================
# mod_lieu_choix.R — CHOISIR UN LIEU : MAILLE, STATION OU DEPARTEMENT (v2.1)
# ------------------------------------------------------------------------------
# Brique du mode « Deux lieux » de l'onglet Comparer : un lieu parmi trois sortes, que le
# bilan sait toutes traiter (utils_comparer.R) :
#
#   maille       un POINT (commune, point de l'accueil, coordonnees) : la valeur de sa
#                maille SAFRAN de 8 km, un MODELE ;
#   station      une station Meteo-France publiee avec le jeu de la carte : une MESURE ;
#   departement  la moyenne des mailles du departement.
#
# Aucun de ces choix ne declenche de telechargement de serie journaliere : les stations
# viennent du jeu publie (utils_stations_publiees.R).
#
# Parametres du serveur
#   jeu           reactive : cmp_jeu() ou NULL
#   lieu_accueil  reactive : point choisi sur l'accueil (nom, lon, lat) ou NULL
#   executer      requete HTTP (injectable)
#   chercher      function(requete) -> data.frame de communes (injectable : geo_chercher_communes)
#   type_defaut   type coche d'emblee
# Retourne list(lieu = reactive du lieu choisi ou NULL, definir_type = function(type)).
# ==============================================================================

DUO_TYPES <- c("Maille SAFRAN (modèle)" = "maille",
               "Station Météo-France (mesure)" = "station",
               "Département" = "departement")

#' @export
mod_lieu_choix_ui <- function(id, titre, type_defaut = "maille") {
  ns <- shiny::NS(id)
  shiny::tags$fieldset(
    class = "cf-lieu-choix",
    shiny::tags$legend(class = "cf-lieu-choix__legende", titre),
    shiny::radioButtons(ns("type"), NULL, choices = DUO_TYPES, selected = type_defaut, inline = TRUE),

    shiny::conditionalPanel(
      "input.type == 'maille'", ns = ns,
      shiny::div(
        class = "cf-lieu-choix__ligne",
        shiny::textInput(ns("commune"), "Commune ou code postal", placeholder = "Ex. Saint-Gaudens", width = "16rem"),
        shiny::actionButton(ns("chercher"), "Chercher", class = "cf-btn cf-btn--sec")),
      shiny::uiOutput(ns("communes")),
      shiny::actionButton(ns("accueil"), "Utiliser le point de l'accueil", class = "cf-btn cf-btn--sec"),
      shiny::tags$details(
        class = "cf-details",
        shiny::tags$summary("Saisir des coordonnées"),
        shiny::div(
          class = "cf-details__corps",
          shiny::numericInput(ns("lon"), "Longitude (°, WGS84)", value = NA, step = 0.01, width = "12rem"),
          shiny::numericInput(ns("lat"), "Latitude (°, WGS84)", value = NA, step = 0.01, width = "12rem"),
          shiny::actionButton(ns("coord"), "Utiliser ces coordonnées", class = "cf-btn cf-btn--sec")))),

    shiny::conditionalPanel("input.type == 'station'", ns = ns, shiny::uiOutput(ns("station_ui"))),

    shiny::conditionalPanel(
      "input.type == 'departement'", ns = ns,
      shiny::selectInput(ns("dep"), "Département",
                         choices = c("Choisir un département" = "", tryCatch(territoires_choix("departement"), error = function(e) character(0))),
                         selectize = FALSE, width = "22rem")),

    shiny::uiOutput(ns("retenu")))
}

#' @export
mod_lieu_choix_server <- function(id, jeu, lieu_accueil, executer = httr2::req_perform,
                                  chercher = geo_chercher_communes, type_defaut = "maille") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Maille : un point -----------------------------------------------------------
    point <- shiny::reactiveVal(NULL)       # list(nom, lon, lat)
    candidats <- shiny::reactiveVal(NULL)   # data.frame de communes
    message_recherche <- shiny::reactiveVal("")

    # Le point de l'accueil, tant que rien d'autre n'a ete choisi.
    shiny::observeEvent(lieu_accueil(), {
      p <- lieu_accueil()
      if (is.null(point()) && !is.null(p) && is.finite(p$lon %||% NA_real_) && is.finite(p$lat %||% NA_real_)) {
        point(list(nom = p$nom %||% "Point de l'accueil", lon = p$lon, lat = p$lat))
      }
    }, ignoreNULL = TRUE)
    shiny::observeEvent(input$accueil, {
      p <- lieu_accueil()
      if (is.null(p) || !is.finite(p$lon %||% NA_real_) || !is.finite(p$lat %||% NA_real_)) {
        message_recherche("Aucun point n'est choisi sur l'accueil.")
        return()
      }
      message_recherche("")
      point(list(nom = p$nom %||% "Point de l'accueil", lon = p$lon, lat = p$lat))
    })

    chercher_commune <- function(q) {
      q <- trimws(q %||% "")
      if (nchar(q) < 2L) { message_recherche("Saisissez au moins deux caractères."); candidats(NULL); return() }
      r <- tryCatch(chercher(q), error = function(e) NULL)
      if (is.null(r) || !nrow(r)) {
        message_recherche(sprintf("Aucune commune ne correspond à « %s ».", q)); candidats(NULL); return()
      }
      message_recherche("")
      candidats(r)
      point(list(nom = r$nom[1], lon = r$lon[1], lat = r$lat[1]))
    }
    shiny::observeEvent(input$chercher, chercher_commune(input$commune))
    shiny::observeEvent(input$commune_entree, chercher_commune(input$commune_entree$valeur))

    shiny::observeEvent(input$commune_choix, {
      r <- candidats(); i <- suppressWarnings(as.integer(input$commune_choix))
      if (!is.null(r) && length(i) == 1L && is.finite(i) && i >= 1L && i <= nrow(r)) {
        point(list(nom = r$nom[i], lon = r$lon[i], lat = r$lat[i]))
      }
    })

    shiny::observeEvent(input$coord, {
      lon <- suppressWarnings(as.numeric(input$lon)); lat <- suppressWarnings(as.numeric(input$lat))
      if (!length(lon) || !length(lat) || !is.finite(lon) || !is.finite(lat) ||
          lon < -180 || lon > 180 || lat < -90 || lat > 90) {
        message_recherche("Coordonnées invalides : longitude entre −180 et 180, latitude entre −90 et 90.")
        return()
      }
      message_recherche("")
      point(list(nom = sprintf("Point %s ; %s", fmt_fr(lat, 3), fmt_fr(lon, 3)), lon = lon, lat = lat))
    })

    output$communes <- shiny::renderUI({
      r <- candidats(); m <- message_recherche()
      shiny::tagList(
        if (nzchar(m)) shiny::p(class = "cf-aide", role = "status", m),
        if (!is.null(r) && nrow(r) > 1L) shiny::selectInput(
          ns("commune_choix"), "Plusieurs communes correspondent",
          choices = stats::setNames(seq_len(nrow(r)), sprintf("%s (%s) — %s", r$nom, r$cp, r$departement)),
          selected = 1L, selectize = FALSE, width = "100%"))
    })

    # -- Station : une mesure publiee ----------------------------------------------------------
    stations <- shiny::reactive({
      j <- jeu()
      if (is.null(j)) return(NULL)
      sj <- cmp_stations(j, executer = executer)
      if (isTRUE(sj$ok)) sj$table else NULL
    })

    output$station_ui <- shiny::renderUI({
      t <- stations()
      if (is.null(t) || !nrow(t)) {
        return(shiny::p(class = "cf-aide", role = "status",
          "Ce jeu de données ne contient pas encore de stations Météo-France : choisissez une maille ou un département."))
      }
      deps <- sort(unique(t$dept))
      etiq <- tryCatch({ tc <- territoires_choix("departement"); stats::setNames(names(tc), tc) }, error = function(e) character(0))
      noms <- ifelse(deps %in% names(etiq), sprintf("%s (%s)", etiq[deps], deps), deps)
      shiny::tagList(
        shiny::selectInput(ns("station_dep"), "Département de la station",
                           choices = c("Choisir un département" = "", stats::setNames(deps, noms)),
                           selected = shiny::isolate(input$station_dep) %||% "", selectize = FALSE, width = "22rem"),
        shiny::uiOutput(ns("station_choix")))
    })

    output$station_choix <- shiny::renderUI({
      t <- stations(); d <- input$station_dep
      if (is.null(t) || is.null(d) || !nzchar(d)) return(NULL)
      s <- t[t$dept == d, , drop = FALSE]
      s <- s[order(s$nom), , drop = FALSE]
      if (!nrow(s)) return(shiny::p(class = "cf-aide", "Aucune station publiée dans ce département."))
      etiq <- sprintf("%s — %s%s", s$nom, ifelse(is.finite(s$alt), sprintf("%.0f m", s$alt), "altitude inconnue"),
                      ifelse(!s$a_temp, " (pluie seule)", ifelse(!s$a_pluie, " (températures seules)", "")))
      shiny::selectInput(ns("station"), "Station", choices = c("Choisir une station" = "", stats::setNames(s$id, etiq)),
                         selected = shiny::isolate(input$station) %||% "", selectize = FALSE, width = "100%")
    })

    # -- Le lieu choisi ---------------------------------------------------------------------------
    lieu <- shiny::reactive({
      switch(input$type %||% type_defaut,
        maille = {
          p <- point()
          if (is.null(p)) NULL else cmp_lieu_point(cmp_nom_propre(p$nom, "Point choisi"), p$lon, p$lat)
        },
        station = {
          t <- stations(); id <- input$station
          if (is.null(t) || is.null(id) || !nzchar(id) || !id %in% t$id) NULL
          else cmp_lieu_station(t[t$id == id, , drop = FALSE][1, ])
        },
        departement = {
          d <- input$dep
          if (is.null(d) || !nzchar(d)) NULL else duo_lieu_departement(d)
        },
        NULL)
    })

    output$retenu <- shiny::renderUI({
      l <- lieu()
      if (is.null(l)) return(shiny::p(class = "cf-mention", role = "status", "Aucun lieu choisi."))
      shiny::p(class = "cf-lieu-choix__retenu", role = "status",
               shiny::strong(l$nom),
               sprintf(" — %s, %s", switch(l$type, station = "station", zone = "département", "maille SAFRAN"), duo_nature(l)))
    })

    list(lieu = lieu,
         definir_type = function(type) {
           if (type %in% DUO_TYPES) shiny::updateRadioButtons(session, "type", selected = type)
         })
  })
}
