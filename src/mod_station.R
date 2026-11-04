# ==============================================================================
# mod_station.R — MODULE « SOURCE DES DONNEES »
# ------------------------------------------------------------------------------
# Des qu'un point est designe, le module cherche les stations Meteo-France les
# plus proches (utils_stations.R), en propose une AVEC SA QUALITE, et laisse
# choisir entre les trois meilleures et la grille SAFRAN. Il ne charge aucune
# serie : il designe une source, app.R la charge.
#
# CONTRAT DE SORTIE
# -----------------
# reactive() -> NULL tant qu'aucun point n'est designe, sinon une liste :
#   $type      "station" ou "safran"
#   $station   une ligne de mf_stations_proches() (liste), ou NULL pour SAFRAN
#   $candidats data.frame des stations proposees (peut etre vide)
#   $alt       altitude du point (NA si inconnue)
#   $erreurs   messages de la recherche (Meteo-France injoignable, secours...)
#   $point     le point (lon, lat, nom, source) auquel se rapporte CETTE source.
#              Pendant une nouvelle recherche, la source en vigueur decrit encore
#              le point precedent : $point permet a l'appelant de s'en assurer
#              avant de lancer une analyse (onglet Carte).
#
# POURQUOI UNE STATION PAR DEFAUT
# -------------------------------
# C'est ce qui a ete demande : mesurer, et non modeliser, quand une mesure
# convenable existe. Le defaut est la premiere station proposee SI sa qualite
# est « bonne », « correcte » ou « limitee ». Si elle est « insuffisante », ou
# s'il n'y a aucune station dans le rayon, ou si Meteo-France ne repond pas, la
# grille SAFRAN prend le relais et le module DIT pourquoi. Le choix reste
# toujours ouvert a l'utilisateur.
#
# POURQUOI LA QUALITE N'EST PAS QUE DE LA COULEUR
# -----------------------------------------------
# Le niveau est ecrit en toutes lettres (« Qualité bonne ») et precede d'un
# symbole different pour chaque niveau : une pastille verte ou rouge seule ne se
# lit pas en plein soleil en tournee, ni pour un daltonien.
# ==============================================================================

mod_station_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("carte"))
}

#' @param lieu reactive() -> list(lon, lat, nom, source) ou NULL.
#' @param choix_initial reactiveVal() -> identifiant de source demande par un lien
#'   partage (« safran » ou identifiant de station), ou NULL. Consomme une seule
#'   fois, par la premiere recherche : un point choisi ensuite retrouve la
#'   source proposee par defaut.
#' @return reactive() -> voir l'en-tete.
mod_station_server <- function(id, lieu, choix_initial = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Resultat de la derniere recherche. Un reactiveVal, pas un reactive : la
    # recherche est lente et reseau, elle ne doit se refaire QUE quand le point
    # change, pas quand l'interface se redessine.
    recherche <- shiny::reactiveVal(NULL)

    shiny::observeEvent(lieu(), {
      p <- lieu()
      if (is.null(p) || !CONFIG$stations$actif) { recherche(NULL); return() }

      res <- shiny::withProgress(
        message = "Recherche des stations Météo-France", value = 0.1, {
          alt <- mf_altitude(p$lon, p$lat)
          shiny::setProgress(0.3)
          cand <- tryCatch(mf_stations_proches(p$lon, p$lat, alt),
                           error = function(e) {
                             v <- mf_stations_proches_vide()
                             attr(v, "erreurs") <- paste("Recherche des stations :", conditionMessage(e))
                             v
                           })
          list(candidats = cand, alt = alt, erreurs = attr(cand, "erreurs") %||% character(0),
               point = p)
        })
      # Source demandee par un lien : retenue si elle fait partie des sources
      # proposees, sinon on le dit et la source par defaut s'applique.
      demande <- if (!is.null(choix_initial)) shiny::isolate(choix_initial()) else NULL
      if (!is.null(demande)) {
        choix_initial(NULL)
        valides <- c("safran", res$candidats$id)
        if (demande %in% valides) {
          res$initial <- demande
        } else {
          shiny::showNotification(
            paste("La station indiquée dans le lien n'est pas proposée pour ce point :",
                  "la source par défaut est utilisée."),
            type = "warning", duration = 10)
        }
      }
      recherche(res)
    }, ignoreNULL = FALSE)

    # -- Source proposee par defaut ------------------------------------------
    defaut <- shiny::reactive({
      r <- recherche(); if (is.null(r)) return("safran")
      if (!is.null(r$initial)) return(r$initial)
      cand <- r$candidats
      if (nrow(cand) && cand$niveau[1] <= 3L) cand$id[1] else "safran"
    })

    choix <- shiny::reactive({
      r <- recherche(); shiny::req(r)
      c0 <- input$choix
      valides <- c("safran", r$candidats$id)
      if (is.null(c0) || !c0 %in% valides) defaut() else c0
    })

    # -- Carte « Source des donnees » ---------------------------------------
    output$carte <- shiny::renderUI({
      p <- lieu()
      if (is.null(p)) return(NULL)
      if (!CONFIG$stations$actif) return(NULL)
      r <- recherche()
      if (is.null(r)) return(NULL)
      cand <- r$candidats

      # Liste des sources : les stations proposees, puis SAFRAN
      noms <- lapply(seq_len(nrow(cand)), function(i) {
        shiny::HTML(sprintf(
          "<span class='cf-source__opt'><strong>%s</strong> <span class='cf-source__optmeta'>%s · %s</span> %s</span>",
          htmltools::htmlEscape(cand$nom[i]), mf_km(cand$distance_km[i]),
          if (is.finite(cand$alt[i])) sprintf("%.0f m", cand$alt[i]) else "altitude ?",
          as.character(mf_pastille(cand$niveau[i], cand$libelle[i]))))
      })
      noms <- c(noms, list(shiny::HTML(
        "<span class='cf-source__opt'><strong>Grille SAFRAN</strong> <span class='cf-source__optmeta'>8 km · modélisée, depuis 1959</span></span>")))
      valeurs <- c(cand$id, "safran")

      n_autres <- length(valeurs)
      shiny::div(
        class = "cf-source",
        shiny::div(class = "cf-source__titre", cf_icone("station"), "Source des données"),

        shiny::uiOutput(ns("principale")),

        if (length(r$erreurs))
          shiny::div(class = "cf-aide cf-source__note",
                     paste(utils::head(unique(r$erreurs), 2), collapse = " ")),

        if (n_autres > 1L)
          cf_repli(
            sprintf("Choisir une autre source (%d possibles)", n_autres),
            shiny::radioButtons(ns("choix"), label = NULL,
                                choiceNames = noms, choiceValues = valeurs,
                                selected = shiny::isolate(defaut())),
            shiny::div(class = "cf-aide", mf_aide_qualite()))
      )
    })

    # -- Resume de la source retenue ------------------------------------------
    output$principale <- shiny::renderUI({
      r <- recherche(); shiny::req(r)
      cand <- r$candidats
      id <- choix()

      if (identical(id, "safran")) {
        pourquoi <- if (!nrow(cand)) {
          if (length(r$erreurs)) "Les stations Météo-France n'ont pas pu être consultées."
          else sprintf("Aucune station Météo-France dans un rayon de %d km.", CONFIG$stations$rayon_km)
        } else if (cand$niveau[1] >= 4L) {
          sprintf("La station la plus proche (%s) est de qualité insuffisante.", cand$nom[1])
        } else NULL
        return(shiny::div(
          class = "cf-source__principale",
          shiny::div(class = "cf-source__nom", "Grille SAFRAN",
                     shiny::span(class = "cf-badge cf-badge--neutre", "Modélisée")),
          shiny::div(class = "cf-source__meta",
                     "Réanalyse Météo-France au pas de 8 km, complète depuis 1959."),
          if (!is.null(pourquoi)) shiny::div(class = "cf-source__pourquoi", pourquoi),
          shiny::div(class = "cf-aide",
                     "Toutes les variables : précipitations, températures, vent, humidité, humidité du sol (SWI), neige.")
        ))
      }

      s <- cand[cand$id == id, , drop = FALSE]
      shiny::req(nrow(s) == 1)
      d <- mf_decrire_station(as.list(s))

      puces <- Filter(Negate(is.null), list(
        d$distance, d$altitude, d$ecart_alt, d$variables))
      manque <- c(if (!isTRUE(s$a_temp)) "températures (gel, canicule, bilan hydrique)",
                  "humidité du sol, neige et humidité de l'air")

      shiny::div(
        class = "cf-source__principale",
        shiny::div(class = "cf-source__nom", s$nom,
                   mf_pastille(s$niveau, paste("Qualité", tolower(s$libelle)))),
        shiny::div(class = "cf-chips-info",
                   lapply(puces, function(x) shiny::span(class = "cf-chip-info", x))),
        shiny::div(class = "cf-source__meta",
                   d$fraicheur, " · complétude sur 12 mois : ", d$completude),
        mf_motifs_qualite(s),
        shiny::div(
          class = "cf-aide",
          shiny::tags$strong("Non fournis par cette station : "),
          paste0(paste(manque, collapse = ", "), ". L'ETP est estimée (Hargreaves)."))
      )
    })

    # -- Valeur retournee ------------------------------------------------------
    shiny::reactive({
      r <- recherche()
      if (is.null(r)) return(NULL)
      id <- tryCatch(choix(), error = function(e) "safran")
      if (identical(id, "safran")) {
        return(list(type = "safran", station = NULL, candidats = r$candidats,
                    alt = r$alt, erreurs = r$erreurs, point = r$point))
      }
      s <- r$candidats[r$candidats$id == id, , drop = FALSE]
      if (nrow(s) != 1) {
        return(list(type = "safran", station = NULL, candidats = r$candidats,
                    alt = r$alt, erreurs = r$erreurs, point = r$point))
      }
      list(type = "station", station = as.list(s), candidats = r$candidats,
           alt = r$alt, erreurs = r$erreurs, point = r$point)
    })
  })
}


# ------------------------------------------------------------------------------
# Petits outils de presentation
# ------------------------------------------------------------------------------

#' Tableau de stations vide, de la forme de mf_stations_proches().
#' @keywords internal
mf_stations_proches_vide <- function() {
  out <- mf_resumer_stations(mf_brut_vide(), "")
  out$distance_km <- numeric(0)
  out
}

#' Pastille de qualite : symbole + texte, jamais la couleur seule.
#' @param niveau 1 a 4.
#' @keywords internal
mf_pastille <- function(niveau, texte) {
  symbole <- c("●", "◐", "▲", "✕")[niveau]
  shiny::span(class = paste0("cf-badge cf-badge--n", niveau),
              shiny::span(`aria-hidden` = "true", symbole), " ", texte)
}

#' Les raisons pour lesquelles la qualite n'est pas « bonne » (liste courte).
#' @param s une ligne de mf_qualite_stations().
#' @keywords internal
mf_motifs_qualite <- function(s) {
  m <- character(0)
  if (isTRUE(s$niv_distance >= 3L)) m <- c(m, sprintf("station éloignée (%.0f km)", s$distance_km))
  if (isTRUE(s$niv_altitude >= 3L)) m <- c(m, sprintf("écart d'altitude de %.0f m", s$ecart_alt_m))
  if (isTRUE(s$niv_fraicheur >= 3L)) m <- c(m, sprintf("dernière mesure ancienne (%.0f jours)", s$age_j))
  if (isTRUE(s$niv_donnees >= 3L)) {
    m <- c(m, if (!isTRUE(s$a_pluie) || !isTRUE(s$a_temp)) "une seule famille de variables mesurée"
              else "données incomplètes ces 12 derniers mois")
  }
  if (!length(m)) return(NULL)
  shiny::div(class = "cf-source__pourquoi",
             paste0("Limites : ", paste(m, collapse =  " ; "), "."))
}

#' Rappel des criteres de qualite, d'apres CONFIG.
#' @keywords internal
mf_aide_qualite <- function() {
  s <- CONFIG$stations$seuils
  sprintf(paste(
    "Qualité d'une station = le plus mauvais de quatre critères : distance",
    "(bonne jusqu'à %d km, insuffisante au-delà de %d km), écart d'altitude",
    "(%d m / %d m), dernière mesure (%d jours / %d jours) et part des 12",
    "derniers mois renseignée (%.0f %% / %.0f %%). Une station qui ne mesure",
    "que la pluie ou que les températures n'est jamais mieux que « limitée »."),
    s$distance_km[1], s$distance_km[3], s$ecart_alt_m[1], s$ecart_alt_m[3],
    s$fraicheur_j[1], s$fraicheur_j[3], 100 * s$completude[1], 100 * s$completude[3])
}
