# ==============================================================================
# mod_comparer.R — MODULE « COMPARER » (v1.9, deux lieux : v2.1)
# ------------------------------------------------------------------------------
# Deux facons de comparer, au choix :
#
#   « Deux lieux » (v2.1, par defaut). On choisit deux lieux, chacun une MAILLE SAFRAN
#       (un point, un modele), une STATION Meteo-France (une mesure) ou un DEPARTEMENT,
#       dans toutes les combinaisons : maille-maille, station-station, maille-station,
#       departement-departement, departement-maille, departement-station. La lecture
#       porte sur les ANNEES COMMUNES aux deux lieux (src/utils_comparer_duo.R).
#   « Plusieurs lieux importes ». Des points et des zones importes (CSV de points,
#       GeoJSON, GeoPackage, shapefile zippe), compares sur les mailles precalculees.
#
# Dans les deux cas, le calcul est celui de src/utils_comparer.R ; ce fichier ne fait que
# le brancher a l'interface.
#
# CE QUE LA PAGE DIT, PARCE QUE CELA CHANGE LA LECTURE
# ----------------------------------------------------
#   - les valeurs sont celles des MAILLES de 8 km, pas d'une station ;
#   - elles sont calculees avec les seuils PAR DEFAUT (les reglages de l'onglet
#     Parametres ne s'y appliquent pas) ;
#   - l'annee comparee se choisit ici : la derniere annee du jeu publie n'est pas
#     forcement l'annee en cours.
#
# Parametres du serveur
#   ctx        reactive : contexte de calcul (reference, annees affichees)
#   lieu       reactive : point choisi sur l'accueil (liste nom, lat, lon) ou NULL
#   transport  reactive : ou lire les fichiers de la carte (carte_transport())
#   mobile     reactive : ecran etroit
#   executer   fonction de requete HTTP (injectable : les tests n'ont pas de reseau)
#   chercher   function(requete) -> communes (injectable : geo_chercher_communes)
# ==============================================================================

# ------------------------------------------------------------------------------
# Constructeurs purs
# ------------------------------------------------------------------------------

#' Tableau de comparaison : une ligne par lieu, une colonne par grandeur.
#' Chaque cellule donne la valeur de l'annee comparee et, dessous, son ecart a la
#' normale de la reference avec une lecture en mots (sans couleur : un ecart
#' positif est bon pour la pluie, mauvais pour la chaleur).
#' @param bilan cmp_bilan() ; @param tl cmp_table_lieux() ; @param cles grandeurs, dans l'ordre.
#' @keywords internal
comparer_tableau_ui <- function(bilan, tl, cles, annee) {
  lieux <- tl$lieu[tl$retenu & tl$lieu %in% bilan$lieu]
  cases <- function(lieu) lapply(cles, function(cle) {
    r <- bilan[bilan$lieu == lieu & bilan$cle == cle, ]
    if (!nrow(r) || !is.finite(r$valeur[1])) {
      return(shiny::tags$td(class = "cf-comp__val", "—"))
    }
    r <- r[1, ]
    unite <- if (nzchar(r$unite)) paste0(" ", r$unite) else ""
    lecture <- switch(r$classe, haut = " · au-dessus", bas = " · en dessous",
                      if (is.finite(r$ecart_type_ref)) " · proche de la normale" else "")
    shiny::tags$td(
      class = "cf-comp__val",
      shiny::div(class = "cf-comp__chiffre", paste0(fmt_fr(r$valeur, r$decimales), unite)),
      shiny::div(class = "cf-comp__ecart", if (is.finite(r$ecart))
        paste0(fmt_fr(r$ecart, r$decimales, signe = TRUE), lecture) else "normale non calculable"))
  })
  titres <- vapply(cles, function(k) CARTE_COUCHES[[k]]$titre, "")
  shiny::div(
    class = "cf-comp__defile", role = "region", tabindex = "0",
    `aria-label` = sprintf("Comparaison des lieux pour %d (défile horizontalement)", annee),
    shiny::tags$table(
      class = "cf-tableau cf-comp__tableau",
      shiny::tags$caption(class = "cf-sr-only",
        sprintf("Valeur de l'année %d et écart à la normale, par lieu", annee)),
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(scope = "col", "Lieu"),
        lapply(titres, function(t) shiny::tags$th(scope = "col", cf_terme(t))))),
      shiny::tags$tbody(lapply(lieux, function(l) {
        i <- match(l, tl$lieu)
        shiny::tags$tr(
          shiny::tags$th(
            scope = "row",
            shiny::div(class = "cf-comp__nom", l),
            shiny::div(class = "cf-comp__ecart", if (tl$type[i] == "zone")
              sprintf("zone, %d maille%s", tl$n_mailles[i], if (tl$n_mailles[i] > 1) "s" else "")
              else if (tl$type[i] == "station") "station (mesure)"
              else "point")),
          cases(l))
      }))))
}

#' Bilan d'un lieu : les ecarts les plus marques, en phrases.
#' @keywords internal
comparer_bilan_ui <- function(bilan, tl, ctx, annee) {
  lieux <- tl$lieu[tl$retenu & tl$lieu %in% bilan$lieu]
  lapply(lieux, function(l) {
    ph <- cmp_phrases_bilan(bilan[bilan$lieu == l, ], ctx, annee)
    i <- match(l, tl$lieu)
    shiny::tags$details(
      class = "cf-details",
      shiny::tags$summary(l),
      shiny::div(
        class = "cf-details__corps",
        if (nzchar(tl$note[i])) shiny::p(class = "cf-mention", tl$note[i]),
        if (length(ph)) shiny::tags$ul(class = "cf-comp__bilan", lapply(ph, shiny::tags$li))
        else shiny::p("Aucune valeur exploitable pour cette année et ces grandeurs.")))
  })
}

#' Les deux lieux compares, en une ligne chacun : nom, famille, nature (mesure ou modele).
#' @param res duo_preparer().
#' @keywords internal
comparer_duo_entete_ui <- function(res) {
  ligne <- function(rep, nom, l, i) {
    shiny::tags$li(shiny::strong(paste0(rep, " : ")), duo_nom_court(nom), sprintf(" — %s, %s", switch(l$type, station = "station", zone = "département", "maille SAFRAN"), duo_nature(l)),
                   if (nzchar(res$tl$note[i])) shiny::span(class = "cf-mention", paste0(" (", res$tl$note[i], ")")))
  }
  shiny::tags$ul(class = "cf-duo__lieux", ligne("A", res$a, res$lieux$a, 1L), ligne("B", res$b, res$lieux$b, 2L))
}

#' Messages d'import : une alerte, avec la liste de ce qui a ete ecarte.
#' @keywords internal
comparer_messages_ui <- function(messages) {
  if (!length(messages)) return(NULL)
  shiny::div(
    class = "cf-alerte cf-alerte--attention", role = "status",
    shiny::span(class = "cf-alerte__titre", "À propos de l'import"),
    shiny::tags$ul(lapply(utils::head(messages, 12L), shiny::tags$li)),
    if (length(messages) > 12L)
      shiny::div(class = "cf-aide", sprintf("… et %d autres remarques.", length(messages) - 12L)))
}


# ------------------------------------------------------------------------------
# Interface
# ------------------------------------------------------------------------------

#' @export
mod_comparer_ui <- function(id) {
  ns <- shiny::NS(id)
  presets <- c(p_mm = "Maille ↔ maille", p_ss = "Station ↔ station", p_ms = "Maille ↔ station",
               p_dd = "Département ↔ département", p_dm = "Département ↔ maille", p_ds = "Département ↔ station")
  shiny::tagList(
    shiny::h2(class = "cf-titre-section", "Comparer des lieux"),
    shiny::p(class = "cf-chapeau", paste(
      "Comparez le climat de deux lieux : deux mailles SAFRAN (des points), deux stations Météo-France,",
      "une maille et une station, ou des départements. La maille est un modèle, la station une mesure :",
      "les comparer, c'est voir ce que la mesure dit de plus ou de moins que le modèle.")),

    shiny::uiOutput(ns("indisponible")),

    shiny::div(
      class = "cf-pastilles",
      shiny::radioButtons(ns("mode"), "Comparer",
                          choices = c("Deux lieux" = "duo", "Plusieurs lieux importés" = "liste"),
                          selected = "duo", inline = TRUE)),

    # -- Deux lieux ------------------------------------------------------------
    shiny::conditionalPanel(
      "input.mode == 'duo'", ns = ns,
      shiny::div(
        class = "cf-carte",
        shiny::h3(class = "cf-carte__titre", "1. Vos deux lieux"),
        shiny::div(
          class = "cf-duo__presets", role = "group", `aria-label` = "Combinaisons usuelles",
          lapply(names(presets), function(i) shiny::actionButton(ns(i), presets[[i]], class = "cf-btn cf-btn--sec"))),
        shiny::div(
          class = "cf-duo__choix",
          mod_lieu_choix_ui(ns("lieu_a"), "Lieu A", type_defaut = "maille"),
          mod_lieu_choix_ui(ns("lieu_b"), "Lieu B", type_defaut = "station")))),

    # -- Plusieurs lieux importes ------------------------------------------------
    shiny::conditionalPanel(
      "input.mode == 'liste'", ns = ns,
      shiny::div(
        class = "cf-carte",
        shiny::h3(class = "cf-carte__titre", "1. Vos lieux"),
        shiny::fileInput(
          ns("fichier"), "Importer un fichier",
          accept = c(".csv", ".txt", ".tsv", ".geojson", ".json", ".gpkg", ".zip"),
          buttonLabel = "Parcourir…", placeholder = "Aucun fichier choisi", width = "100%"),
        shiny::div(class = "cf-aide", sprintf(paste(
          "Points : CSV avec une colonne nom, latitude et longitude (WGS84, degrés décimaux).",
          "Zones : GeoJSON, GeoPackage ou shapefile zippé (le .zip contient .shp, .shx, .dbf et .prj).",
          "%d lieux au plus, %s Mo au plus par fichier."),
          CONFIG$comparer$lieux_max, fmt_fr(CONFIG$comparer$fichier_max_mo, 0))),
        shiny::div(
          class = "cf-comp__actions",
          shiny::actionButton(ns("ajouter_point"), "Ajouter le point de l'accueil",
                              class = "cf-btn cf-btn--sec"),
          shiny::downloadButton(ns("modele"), "Modèle de CSV", class = "cf-btn cf-btn--sec"),
          shiny::actionButton(ns("vider"), "Tout effacer", class = "cf-btn cf-btn--sec")),
        shiny::uiOutput(ns("messages")),
        shiny::uiOutput(ns("liste_lieux")))),

    # -- 2. Les grandeurs (communes aux deux modes) -----------------------------
    shiny::div(
      class = "cf-carte",
      shiny::h3(class = "cf-carte__titre", "2. Grandeurs et année"),
      shiny::uiOutput(ns("choix")),
      shiny::p(class = "cf-mention", paste(
        "Les valeurs des mailles sont précalculées avec les seuils par défaut : les réglages de l'onglet",
        "Paramètres ne s'y appliquent pas. La normale est celle de la période de référence choisie",
        "sur l'accueil."))),

    # -- 3. Resultats ----------------------------------------------------------
    shiny::conditionalPanel("input.mode == 'duo'", ns = ns, shiny::uiOutput(ns("duo_resultats"))),
    shiny::conditionalPanel("input.mode == 'liste'", ns = ns, shiny::uiOutput(ns("resultats")))
  )
}


# ------------------------------------------------------------------------------
# Serveur
# ------------------------------------------------------------------------------

#' @export
mod_comparer_server <- function(id, ctx, lieu, transport = shiny::reactive(carte_transport()),
                                mobile = shiny::reactive(FALSE),
                                executer = httr2::req_perform,
                                chercher = geo_chercher_communes) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    avec_sf <- requireNamespace("sf", quietly = TRUE)

    # -- Le jeu de donnees de la carte -----------------------------------------
    # Charge a la premiere ouverture de l'onglet (les sorties d'un onglet masque ne
    # sont pas evaluees), puis conserve en memoire par cmp_jeu().
    jeu_brut <- shiny::reactive({
      tryCatch(cmp_jeu(transport(), executer = executer), error = function(e) {
        journaliser_erreur("Comparer : fichiers de la carte", e)
        e
      })
    })
    jeu <- shiny::reactive({ j <- jeu_brut(); if (inherits(j, "error")) NULL else j })

    output$indisponible <- shiny::renderUI({
      if (!avec_sf) {
        return(shiny::div(
          class = "cf-alerte cf-alerte--attention", role = "status",
          shiny::span(class = "cf-alerte__titre", "Comparaison indisponible sur ce serveur"),
          "Le paquet R « sf », nécessaire pour rattacher les lieux aux mailles, n’y est pas installé."))
      }
      if (!inherits(jeu_brut(), "error")) return(NULL)
      shiny::div(
        class = "cf-alerte cf-alerte--info", role = "status",
        shiny::span(class = "cf-alerte__titre", "Les données de la carte ne sont pas disponibles"),
        shiny::p(paste(
          "La comparaison s'appuie sur les valeurs précalculées de la carte. Elles ne se chargent pas",
          "pour le moment (voir l'onglet Carte) ; l'erreur est consignée dans le journal.")))
    })

    # -- Les lieux --------------------------------------------------------------
    lieux <- shiny::reactiveVal(list())
    selection <- shiny::reactiveVal(character(0))
    messages <- shiny::reactiveVal(character(0))

    ajouter <- function(nouveaux, problemes = character(0)) {
      r <- cmp_ajouter_lieux(lieux(), nouveaux)
      avant <- vapply(lieux(), function(l) l$nom, "")
      lieux(r$lieux)
      apres <- vapply(r$lieux, function(l) l$nom, "")
      selection(union(selection(), setdiff(apres, avant)))
      messages(c(problemes, r$problemes))
    }

    shiny::observeEvent(input$fichier, {
      f <- input$fichier
      shiny::req(f, nrow(f) == 1L)
      r <- cmp_importer(f$datapath[1], f$name[1])
      if (!length(r$lieux)) {
        messages(r$problemes)
      } else {
        ajouter(r$lieux, r$problemes)
      }
    })

    shiny::observeEvent(input$ajouter_point, {
      p <- lieu()
      if (is.null(p) || !is.finite(p$lon %||% NA_real_) || !is.finite(p$lat %||% NA_real_)) {
        messages("Aucun point n'est choisi sur l'accueil.")
        return()
      }
      ajouter(list(cmp_lieu_point(p$nom %||% "Point de l'accueil", p$lon, p$lat)))
    })

    shiny::observeEvent(input$vider, {
      lieux(list()); selection(character(0)); messages(character(0))
    })

    output$messages <- shiny::renderUI(comparer_messages_ui(messages()))

    # Rattachement aux mailles : refait quand la liste ou le jeu change.
    mailles <- shiny::reactive({
      j <- jeu()
      shiny::req(j, avec_sf)
      lapply(lieux(), cmp_mailles, jeu = j, executer = executer)
    })
    tl <- shiny::reactive({
      if (!length(lieux())) return(cmp_table_lieux(list(), list()))
      cmp_table_lieux(lieux(), mailles())
    })

    output$liste_lieux <- shiny::renderUI({
      t <- tl()
      if (!nrow(t)) {
        return(shiny::p(class = "cf-mention", "Aucun lieu pour l'instant."))
      }
      etiquette <- sprintf("%s — %s%s", t$lieu,
        ifelse(t$type == "zone", sprintf("zone, %d maille%s", t$n_mailles, ifelse(t$n_mailles > 1, "s", "")),
               ifelse(t$type == "station", "station (mesure)", "point")),
        ifelse(nzchar(t$note), paste0(" (", t$note, ")"), ""))
      shiny::checkboxGroupInput(
        ns("retenus"), sprintf("Lieux comparés (%d sur %d)", sum(t$retenu & t$lieu %in% selection()), nrow(t)),
        choiceNames = lapply(etiquette, shiny::span), choiceValues = t$lieu,
        selected = shiny::isolate(selection()), width = "100%")
    })
    shiny::observeEvent(input$retenus, selection(input$retenus %||% character(0)),
                        ignoreNULL = FALSE, ignoreInit = TRUE)

    # -- Grandeurs et annee --------------------------------------------------------
    dispo <- shiny::reactive({ j <- jeu(); shiny::req(j); cmp_couches_disponibles(j) })

    output$choix <- shiny::renderUI({
      d <- dispo()
      j <- jeu()
      defaut <- intersect(CONFIG$comparer$couches_defaut, names(d))
      shiny::tagList(
        shiny::selectInput(ns("annee"), "Année comparée", choices = rev(j$annees),
                           selected = max(j$annees), width = "14rem", selectize = FALSE),
        shiny::tags$details(
          class = "cf-details",
          shiny::tags$summary(sprintf("Grandeurs comparées (%d sur %d)", length(defaut), length(d))),
          shiny::div(
            class = "cf-details__corps",
            shiny::checkboxGroupInput(ns("couches"), label = shiny::span(class = "cf-sr-only", "Grandeurs"),
                                      choiceNames = lapply(unname(d), shiny::span),
                                      choiceValues = names(d), selected = defaut, width = "100%"))))
    })

    cles <- shiny::reactive({
      c <- input$couches
      c <- if (is.null(c)) intersect(CONFIG$comparer$couches_defaut, names(dispo())) else c
      intersect(names(dispo()), c)
    })
    annee <- shiny::reactive({
      a <- suppressWarnings(as.integer(input$annee))
      if (length(a) == 1L && is.finite(a)) a else max(jeu()$annees)
    })

    # -- Calcul ----------------------------------------------------------------------
    retenus <- shiny::reactive({
      t <- tl(); t$lieu[t$retenu & t$lieu %in% selection()]
    })
    series <- shiny::reactive({
      j <- jeu(); t <- tl()
      shiny::req(j, nrow(t) > 0, length(retenus()) > 0, length(cles()) > 0)
      garde <- which(t$lieu %in% retenus())
      tryCatch(
        cmp_series(j, t$lieu[garde], mailles()[garde], cles(), executer),
        error = function(e) { journaliser_erreur("Comparer : series", e); NULL })
    })
    tendances <- shiny::reactive({ s <- series(); shiny::req(s); cmp_tendances(s, ctx()) })
    bilan <- shiny::reactive({
      s <- series(); shiny::req(s, nrow(s) > 0)
      cmp_bilan(s, ctx(), annee(), tendances())
    })

    # -- Resultats ---------------------------------------------------------------------
    output$resultats <- shiny::renderUI({
      shiny::req(jeu())
      if (!length(retenus())) return(NULL)
      s <- series()
      if (is.null(s)) {
        return(shiny::div(class = "cf-alerte cf-alerte--attention", role = "status",
          shiny::span(class = "cf-alerte__titre", "La comparaison n'a pas pu être calculée"),
          "Les fichiers de la carte ne se lisent pas ; l'erreur est consignée dans le journal."))
      }
      b <- bilan(); t <- tl(); a <- annee(); k <- cles()
      choix_graphe <- stats::setNames(k, vapply(k, function(x) CARTE_COUCHES[[x]]$titre, ""))
      shiny::tagList(
        shiny::div(
          class = "cf-carte",
          shiny::h3(class = "cf-carte__titre", sprintf("3. Comparaison pour %d", a)),
          comparer_tableau_ui(b, t, k, a),
          shiny::p(class = "cf-mention", sprintf(
            "Écart par rapport à la normale %s de chaque lieu. « Proche de la normale » : moins d'un demi écart-type.",
            ctx()$ref_label))),
        shiny::div(
          class = "cf-carte",
          shiny::h3(class = "cf-carte__titre", "Évolution depuis 1959"),
          shiny::div(
            class = "cf-comp__actions",
            shiny::selectInput(ns("couche_graphe"), "Grandeur", choices = choix_graphe,
                               selected = shiny::isolate(input$couche_graphe), width = "22rem",
                               selectize = FALSE),
            shiny::div(
              class = "cf-pastilles",
              shiny::radioButtons(ns("mode_graphe"), "Lecture",
                                  choices = c("Valeur" = "valeur", "Écart à la normale" = "ecart"),
                                  selected = shiny::isolate(input$mode_graphe) %||% "valeur",
                                  inline = TRUE))),
          shiny::plotOutput(ns("graphe"), height = "380px")),
        shiny::div(
          class = "cf-carte",
          shiny::h3(class = "cf-carte__titre", "Bilan par lieu"),
          shiny::p(class = "cf-mention", sprintf(
            "Les cinq écarts les plus marqués de %d, rapportés à l'écart-type de la référence.", a)),
          comparer_bilan_ui(b, t, ctx(), a)),
        shiny::div(
          class = "cf-comp__actions",
          shiny::downloadButton(ns("csv_comparaison"), "Comparaison (CSV)", class = "cf-btn cf-btn--sec"),
          shiny::downloadButton(ns("csv_series"), "Séries annuelles (CSV)", class = "cf-btn cf-btn--sec")),
        shiny::tags$details(
          class = "cf-details",
          shiny::tags$summary("Méthode et limites"),
          shiny::div(
            class = "cf-details__corps",
            shiny::p(sprintf(paste(
              "Les valeurs viennent des mailles de 8 km de la réanalyse SAFRAN-ISBA (Météo-France),",
              "précalculées le %s. Un point prend la valeur de sa maille la plus proche (à 6 km au plus).",
              "Une zone prend la moyenne des mailles dont le centre est dans la zone ; une zone plus petite",
              "qu'une maille prend la maille la plus proche."),
              format(as.Date(jeu()$meta$genere_le), "%d/%m/%Y"))),
            shiny::p(paste(
              "Une maille lisse le relief : en terrain accidenté, la valeur d'un point peut s'écarter de celle",
              "d'une station. Pour une analyse fine d'un lieu, utilisez l'onglet Accueil.")),
            shiny::p(paste(
              "La normale est la moyenne des années de référence de la série annuelle du lieu (dix années",
              "au moins). La tendance est la pente de Sen avec le test de Mann-Kendall corrigé de",
              "l'autocorrélation, sur vingt années au moins ; « non significative » ne veut pas dire « absente ».")))))
    })

    output$graphe <- shiny::renderPlot({
      s <- series(); b <- bilan()
      cle <- input$couche_graphe
      shiny::req(s, b, cle, cle %in% s$cle)
      cmp_graphe(s, b, cle, input$mode_graphe %||% "valeur", ctx(), annee(), mobile())
    }, res = 96)

    # -- Deux lieux ------------------------------------------------------------------------------
    choix_a <- mod_lieu_choix_server("lieu_a", jeu, lieu, executer, chercher, type_defaut = "maille")
    choix_b <- mod_lieu_choix_server("lieu_b", jeu, lieu, executer, chercher, type_defaut = "station")

    # Combinaisons usuelles : elles ne font que cocher le type de chaque lieu.
    paires <- list(p_mm = c("maille", "maille"), p_ss = c("station", "station"), p_ms = c("maille", "station"),
                   p_dd = c("departement", "departement"), p_dm = c("departement", "maille"),
                   p_ds = c("departement", "station"))
    for (nom in names(paires)) local({
      n <- nom
      shiny::observeEvent(input[[n]], {
        choix_a$definir_type(paires[[n]][1]); choix_b$definir_type(paires[[n]][2])
      }, ignoreInit = TRUE)
    })

    duo <- shiny::reactive({
      j <- jeu(); a <- choix_a$lieu(); b <- choix_b$lieu()
      shiny::req(j, avec_sf, a, b, length(cles()) > 0)
      tryCatch(duo_preparer(a, b, j, cles(), ctx(), annee(), executer),
               error = function(e) { journaliser_erreur("Comparer : deux lieux", e); NULL })
    })

    output$duo_resultats <- shiny::renderUI({
      shiny::req(jeu())
      if (is.null(choix_a$lieu()) || is.null(choix_b$lieu())) {
        return(shiny::div(class = "cf-carte",
          shiny::p("Choisissez les deux lieux ci-dessus : le résultat s'affiche ensuite ici.")))
      }
      r <- duo()
      if (is.null(r)) {
        return(shiny::div(class = "cf-alerte cf-alerte--attention", role = "status",
          shiny::span(class = "cf-alerte__titre", "La comparaison n'a pas pu être calculée"),
          "Les fichiers de la carte ne se lisent pas ; l'erreur est consignée dans le journal."))
      }
      if (!all(r$tl$retenu)) {
        hors <- r$tl[!r$tl$retenu, , drop = FALSE]
        return(shiny::div(class = "cf-alerte cf-alerte--attention", role = "status",
          shiny::span(class = "cf-alerte__titre", "Un lieu ne peut pas être comparé"),
          shiny::tags$ul(lapply(seq_len(nrow(hors)), function(i) shiny::tags$li(sprintf("%s : %s.", hors$lieu[i], hors$note[i]))))))
      }
      k <- intersect(r$cles, unique(r$series$cle[is.finite(r$series$valeur)]))
      choix_graphe <- stats::setNames(k, vapply(k, function(x) CARTE_COUCHES[[x]]$titre, ""))
      tab <- duo_tableau(r)
      shiny::tagList(
        shiny::div(
          class = "cf-carte",
          shiny::h3(class = "cf-carte__titre", sprintf("3. Comparaison pour %d", r$annee)),
          comparer_duo_entete_ui(r),
          synthese_bref_ui(duo_lecture(r)),
          if (!is.null(tab)) synthese_table_tag(tab, sprintf("Valeur de %d pour chaque lieu, écart entre les deux et écart à la normale", r$annee)),
          shiny::p(class = "cf-mention", sprintf(paste(
            "Années communes aux deux lieux seulement ; chacun est rapporté à sa propre normale %s,",
            "calculée sur ces années. « Proche de la normale » : moins d'un demi écart-type."), r$ctx$ref_label))),
        if (length(k)) shiny::div(
          class = "cf-carte",
          shiny::h3(class = "cf-carte__titre", "Évolution sur les années communes"),
          shiny::div(
            class = "cf-comp__actions",
            shiny::selectInput(ns("duo_couche"), "Grandeur", choices = choix_graphe,
                               selected = shiny::isolate(input$duo_couche), width = "22rem", selectize = FALSE),
            shiny::div(
              class = "cf-pastilles",
              shiny::radioButtons(ns("duo_mode"), "Lecture",
                                  choices = c("Valeur" = "valeur", "Écart à la normale" = "ecart"),
                                  selected = shiny::isolate(input$duo_mode) %||% "valeur", inline = TRUE))),
          shiny::plotOutput(ns("duo_graphe"), height = "380px")),
        shiny::div(
          class = "cf-comp__actions",
          shiny::downloadButton(ns("csv_duo"), "Écarts A − B (CSV)", class = "cf-btn cf-btn--sec"),
          shiny::downloadButton(ns("csv_duo_series"), "Séries annuelles (CSV)", class = "cf-btn cf-btn--sec")),
        shiny::tags$details(
          class = "cf-details",
          shiny::tags$summary("Méthode et limites"),
          shiny::div(class = "cf-details__corps", lapply(synthese_methode(r), shiny::p))))
    })

    output$duo_graphe <- shiny::renderPlot({
      r <- duo(); cle <- input$duo_couche
      shiny::req(r, cle, cle %in% r$series$cle)
      cmp_graphe(r$series, r$bilan, cle, input$duo_mode %||% "valeur", r$ctx, r$annee, mobile())
    }, res = 96)

    ecrire_duo <- function(file, d, titre) {
      r <- duo(); shiny::req(r)
      ecrire_csv_avec_metadonnees(d, file, duo_lignes_metadonnees(r, jeu(), titre))
    }
    output$csv_duo <- shiny::downloadHandler(
      filename = function() nom_fichier_export("comparaison_ab", list(nom = "deux_lieux")),
      content = function(file) { r <- duo(); shiny::req(r); ecrire_duo(file, duo_export_ecarts(r), "Comparaison de deux lieux : écarts A − B") })
    output$csv_duo_series <- shiny::downloadHandler(
      filename = function() nom_fichier_export("comparaison_ab_series", list(nom = "deux_lieux")),
      content = function(file) { r <- duo(); shiny::req(r); ecrire_duo(file, cmp_export_series(r$series), "Séries annuelles des deux lieux (années communes)") })

    # -- Exports ---------------------------------------------------------------------------
    output$modele <- shiny::downloadHandler(
      filename = function() "modele_points.csv",
      content = function(file) {
        ecrire_csv_avec_metadonnees(cmp_modele_csv(), file, character(0))
      })

    ecrire <- function(file, d, titre) {
      ecrire_csv_avec_metadonnees(d, file,
                                  cmp_lignes_metadonnees(jeu(), tl()[tl()$lieu %in% retenus(), ], ctx(), annee(), titre))
    }
    output$csv_comparaison <- shiny::downloadHandler(
      filename = function() nom_fichier_export("comparaison", list(nom = "lieux")),
      content = function(file) {
        t <- tl()
        ecrire(file, cmp_export_comparaison(bilan(), t, annee()), "Comparaison de lieux")
      })
    output$csv_series <- shiny::downloadHandler(
      filename = function() nom_fichier_export("comparaison_series", list(nom = "lieux")),
      content = function(file) {
        ecrire(file, cmp_export_series(series()), "Séries annuelles des lieux comparés")
      })

    invisible(NULL)
  })
}
