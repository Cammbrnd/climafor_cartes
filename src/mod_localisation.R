# ==============================================================================
# mod_localisation.R — MODULE "OU ?"
# ------------------------------------------------------------------------------
# Trois voies vers le meme resultat : commune, position de l'appareil, carte ou
# coordonnees.
#
# CHOIX D'INTERFACE (v3) : UN GESTE, PAS TROIS ONGLETS
# ----------------------------------------------------
# La v2 presentait trois onglets (Commune / Carte / Coordonnees) dont la carte
# Leaflet, hauteur 300 px, et un bouton « Ma position » cache dans le troisieme.
# Or l'usage dominant, surtout en tournee, tient en UN geste : taper une commune,
# ou toucher « Utiliser ma position ». Ces deux voies sont donc visibles
# d'emblee, sur un seul ecran, sans onglet a ouvrir. La carte et les
# coordonnees, qui servent a placer un point precis hors commune, restent a un
# clic dans des blocs replies.
#
# Les composants construits dans un bloc replie mesurent une largeur nulle ;
# www/app.js emet un « resize » au depliage, et la sortie de la carte est
# declaree sans suspension (suspendWhenHidden = FALSE) pour exister des le debut.
#
# Contrat de sortie : reactiveVal -> list(lon, lat, nom, source) ou NULL.
# Le module ne telecharge aucune donnee : il designe un point.
#
# (v1.4) Le retour est un reactiveVal et non un reactive : app.R y pose le point
# d'un lien partage (source « lien partagé ») sans passer par la recherche d'une
# commune. Les autres modules le lisent comme avant, en l'appelant sans argument.
# ==============================================================================

# ------------------------------------------------------------------------------
# Entrees et sorties a controler (audit v1.7, V5)
# ------------------------------------------------------------------------------
# Tout ce qui arrive du navigateur (texte tape, clic, position) ou d'un service
# distant (nom de commune) est une DONNEE NON FIABLE : un client peut envoyer au
# serveur Shiny n'importe quelle valeur, et une reponse amont alteree irait
# s'inserer dans la page de tous les utilisateurs. Les helpers ci-dessous sont
# purs : ils se testent sans navigateur ni reseau.

# Longueur maximale du texte de recherche (le plus long nom de commune francais
# fait moins de 50 caracteres).
LOC_RECHERCHE_MAX <- 100L

#' Texte de recherche nettoye : une seule chaine, sans espaces de bord, au plus
#' LOC_RECHERCHE_MAX caracteres. Renvoie "" si l'entree n'est pas un texte.
#' @keywords internal
loc_recherche_nettoyee <- function(q) {
  if (is.null(q) || is.list(q) || length(q) < 1L) return("")
  q <- as.character(q)[1]
  if (is.na(q)) return("")
  q <- gsub("[[:cntrl:]]", " ", q)
  substr(trimws(q), 1L, LOC_RECHERCHE_MAX)
}

#' Un point est-il utilisable ? Deux nombres finis, dans les limites de la
#' France metropolitaine (celles de la saisie manuelle et des liens partages ;
#' SAFRAN ne couvre rien d'autre). Une valeur forgee (texte, liste, infini) est
#' refusee au lieu de partir vers un service distant.
#' @keywords internal
loc_point_valide <- function(lon, lat) {
  num <- function(x) tryCatch(suppressWarnings(as.numeric(x)), error = function(e) NA_real_)
  lon <- num(lon); lat <- num(lat)
  length(lon) == 1L && length(lat) == 1L && is.finite(lon) && is.finite(lat) &&
    lat >= PARTAGE_BORNES$lat[1] && lat <= PARTAGE_BORNES$lat[2] &&
    lon >= PARTAGE_BORNES$lon[1] && lon <= PARTAGE_BORNES$lon[2]
}

#' Contenu HTML d'un resultat de recherche de commune. Le nom, le code postal et
#' le departement viennent de l'API Geo : ils sont ECHAPPES (un nom contenant
#' « <script> » s'affiche comme du texte). Le gras et l'espacement, eux, sont
#' ecrits ici.
#' @keywords internal
loc_resultat_html <- function(nom, cp, departement) {
  texte <- function(x) {
    x <- as.character(x %||% "")[1]
    if (is.na(x)) x <- ""
    htmltools::htmlEscape(x)
  }
  shiny::HTML(sprintf(
    "<strong>%s</strong><span class='cf-resultat__meta'>%s · %s</span>",
    texte(nom), texte(cp), texte(departement)))
}

mod_localisation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "cf-loc",

    # -- Voie 1 : commune ou code postal -------------------------------------
    shiny::div(
      class = "cf-champ-bouton",
      # Le texte indicatif d'un champ (placeholder) disparait a la saisie et
      # n'est pas un nom accessible fiable : le champ a donc une etiquette,
      # masquee a l'ecran car le titre de l'etape la dit deja.
      shiny::textInput(ns("commune"),
                       label = shiny::span(class = "cf-sr-only", "Commune ou code postal"),
                       width = "100%", placeholder = "Commune ou code postal"),
      shiny::actionButton(
        ns("chercher"),
        label = shiny::tagList(cf_icone("recherche"), "Rechercher"),
        class = "cf-btn cf-btn--sec")
    ),
    shiny::uiOutput(ns("resultats")),

    # -- Voie 2 : position de l'appareil ---------------------------------------
    shiny::actionButton(
      ns("geoloc"),
      label = shiny::tagList(cf_icone("cible"), "Utiliser ma position"),
      class = "cf-btn cf-btn--sec cf-btn--pleine",
      `aria-describedby` = ns("note_position")),
    # Audit V3 : dire, au moment ou la position est demandee, ce qu'il en advient.
    shiny::div(
      id = ns("note_position"), class = "cf-aide",
      "Position arrondie à 10 m près avant envoi ; voir « Position et confidentialité » dans Sources."),

    # Confirmation de ce qui est retenu : c'est le seul retour dont dispose
    # l'utilisateur avant de lancer une requete d'une minute.
    shiny::uiOutput(ns("recapitulatif")),

    # -- Voies 3 et 4 : a un clic ----------------------------------------------
    cf_repli(
      shiny::tagList(cf_icone("carte"), "Placer le point sur la carte"),
      leaflet::leafletOutput(ns("carte"), height = "280px"),
      shiny::div(class = "cf-aide", "Touchez ou cliquez pour placer le point.")
    ),
    cf_repli(
      shiny::tagList(cf_icone("position"), "Saisir des coordonnées"),
      shiny::div(
        class = "cf-duo",
        shiny::numericInput(ns("lat"), "Latitude (WGS 84)", value = NA, step = 0.0001),
        shiny::numericInput(ns("lon"), "Longitude (WGS 84)", value = NA, step = 0.0001)
      ),
      shiny::actionButton(
        ns("valider_xy"),
        label = shiny::tagList(cf_icone("coche"), "Appliquer"),
        class = "cf-btn cf-btn--sec")
    )
  )
}

#' @return reactive(list(lon, lat, nom, source)) ou NULL.
mod_localisation_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    point     <- shiny::reactiveVal(NULL)   # point courant
    candidats <- shiny::reactiveVal(NULL)   # resultats de recherche

    # -- Carte ---------------------------------------------------------------
    output$carte <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addTiles(
          urlTemplate = CONFIG$api$wmts_ign_plan,
          attribution = "IGN — Géoplateforme",
          options = leaflet::tileOptions(minZoom = 5, maxZoom = 16)) |>
        leaflet::setView(lng = 4.85, lat = 45.5, zoom = 7)
    })

    shiny::outputOptions(output, "carte", suspendWhenHidden = FALSE)

    shiny::observeEvent(input$carte_click, {
      clic <- input$carte_click
      if (is.null(clic)) return()
      if (!loc_point_valide(clic$lng, clic$lat)) { hors_france(); return() }
      lon <- round(as.numeric(clic$lng), 4); lat <- round(as.numeric(clic$lat), 4)
      nom <- geo_commune_par_point(lon, lat)
      point(list(
        lon = lon, lat = lat,
        nom = if (is.null(nom)) "Point sélectionné" else nom$nom,
        source = "carte"))
    })

    # -- Géolocalisation ------------------------------------------------------
    shiny::observeEvent(input$geoloc, {
      session$sendCustomMessage("cf_geolocaliser", list(cible = ns("position")))
    })

    shiny::observeEvent(input$position, {
      pos <- input$position
      if (is.null(pos) || isTRUE(pos$erreur)) {
        shiny::showNotification(
          "Position indisponible. Saisissez une commune ou cliquez sur la carte.",
          type = "warning", duration = 6)
        return()
      }
      if (!loc_point_valide(pos$lon, pos$lat)) { hors_france(); return() }
      # Arrondi AVANT tout appel distant : 4 decimales = environ 10 m, bien plus
      # fin que la maille de 8 km ; le service tiers n'a pas besoin de plus.
      lon <- round(as.numeric(pos$lon), 4); lat <- round(as.numeric(pos$lat), 4)
      nom <- geo_commune_par_point(lon, lat)
      point(list(
        lon = lon, lat = lat,
        nom = if (is.null(nom)) "Ma position" else nom$nom,
        source = "géolocalisation"))
    })

    hors_france <- function() {
      shiny::showNotification(
        "Ce point est hors de France métropolitaine : la grille SAFRAN et les stations n'y couvrent rien.",
        type = "warning", duration = 6)
    }

    # -- Recherche de commune ------------------------------------------------
    lancer_recherche <- function(q = input$commune) {
      q <- loc_recherche_nettoyee(q)
      if (nchar(q) < 2) { candidats(NULL); return() }
      res <- geo_chercher_communes(q)
      candidats(res)
      if (!nrow(res)) {
        shiny::showNotification("Aucune commune ne correspond à cette saisie.",
                                type = "message", duration = 5)
      } else if (nrow(res) == 1) {
        selectionner_commune(1)
      }
    }

    selectionner_commune <- function(i) {
      res <- candidats()
      if (is.null(res) || i > nrow(res)) return()
      c1 <- res[i, ]
      if (!loc_point_valide(c1$lon, c1$lat)) {
        # Commune d'outre-mer ou coordonnees aberrantes venues de l'API.
        hors_france(); return()
      }
      point(list(lon = round(c1$lon, 4), lat = round(c1$lat, 4),
                 nom = sprintf("%s (%s)", c1$nom, c1$cp), source = "commune"))
      candidats(NULL)
    }

    shiny::observeEvent(input$chercher, lancer_recherche())
    # La touche Entrée transmet la valeur du champ : voir www/app.js
    shiny::observeEvent(input$commune_entree, {
      lancer_recherche(input$commune_entree$valeur %||% input$commune)
    })

    output$resultats <- shiny::renderUI({
      res <- candidats()
      if (is.null(res) || !nrow(res)) return(NULL)
      shiny::div(
        class = "cf-resultats",
        lapply(seq_len(nrow(res)), function(i) {
          shiny::actionLink(
            ns(paste0("com_", i)),
            label = loc_resultat_html(res$nom[i], res$cp[i], res$departement[i]),
            class = "cf-resultat")
        }))
    })

    # Observateurs crees une seule fois, pour les 10 resultats possibles
    lapply(1:10, function(i) {
      shiny::observeEvent(input[[paste0("com_", i)]],
                          selectionner_commune(i), ignoreInit = TRUE)
    })

    # -- Saisie manuelle ------------------------------------------------------
    shiny::observeEvent(input$valider_xy, {
      lat <- input$lat; lon <- input$lon
      if (!isTRUE(is.finite(lat)) || !isTRUE(is.finite(lon))) {
        shiny::showNotification("Renseignez une latitude et une longitude valides.",
                                type = "warning"); return()
      }
      if (!loc_point_valide(lon, lat)) {
        shiny::showNotification(
          "Ce point est hors de France métropolitaine.",
          type = "warning", duration = 6); return()
      }
      nom <- geo_commune_par_point(lon, lat)
      point(list(lon = lon, lat = lat,
                 nom = if (is.null(nom)) "Point saisi" else nom$nom,
                 source = "coordonnées"))
    })

    # -- Synchronisation de l'affichage sur le point courant -----------------
    placer_marqueur <- function(p) {
      leaflet::leafletProxy("carte") |>
        leaflet::clearMarkers() |>
        leaflet::addCircleMarkers(
          lng = p$lon, lat = p$lat, radius = 8,
          color = CONFIG$couleurs$vert, fillOpacity = 0.85, weight = 2,
          label = htmltools::htmlEscape(p$nom)) |>
        leaflet::setView(lng = p$lon, lat = p$lat, zoom = 11)
    }

    shiny::observeEvent(point(), {
      p <- point(); if (is.null(p)) return()
      shiny::updateNumericInput(session, "lat", value = p$lat)
      shiny::updateNumericInput(session, "lon", value = p$lon)
      # Point venu d'un lien : le champ « commune » reprend son nom, sinon il
      # resterait vide devant un point deja choisi.
      if (identical(p$source, "lien partagé")) {
        shiny::updateTextInput(session, "commune", value = p$nom)
      }
      placer_marqueur(p)
    })

    # Un point pose AVANT que la carte n'existe cote navigateur (cas d'un lien :
    # il est pose au demarrage) ferait perdre l'appel du proxy. La carte annonce
    # ses limites des qu'elle est construite : on y place alors le marqueur.
    shiny::observeEvent(input$carte_bounds, {
      p <- point(); if (!is.null(p)) placer_marqueur(p)
    }, once = TRUE)

    output$recapitulatif <- shiny::renderUI({
      p <- point()
      if (is.null(p)) {
        return(shiny::div(
          class = "cf-etat cf-etat--attente",
          shiny::span(class = "cf-etat__titre", cf_icone("position"),
                      "Aucun point sélectionné")))
      }
      shiny::div(
        class = "cf-etat cf-etat--ok",
        shiny::span(class = "cf-etat__titre", cf_icone("coche"), p$nom),
        shiny::span(class = "cf-etat__detail",
                    sprintf("%s N · %s E · via %s", fmt_fr(p$lat, 4), fmt_fr(p$lon, 4), p$source)))
    })

    point
  })
}
