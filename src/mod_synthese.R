# ==============================================================================
# mod_synthese.R — MODULE « BILAN SYNTHETIQUE » (v2.0)
# ------------------------------------------------------------------------------
# Un bilan court, a quatre echelles : un departement, une region, un ensemble de
# fiches importees, ou un point. On coche les grandeurs dont on veut le graphique et
# celles dont on veut la ligne de tableau ; le bilan s'etablit d'un clic, avec
# quelques phrases d'explication en tete et sous chaque graphique, et s'emporte en
# page HTML, en Word ou en CSV.
#
# Le meme module sert les trois profils : le profil ne change que les echelles
# offertes (CONFIG, utils_profils.R), les grandeurs du catalogue et le jeu coche
# d'emblee. Tout le calcul est dans src/utils_synthese.R.
#
# Parametres du serveur
#   profil      un element de PROFILS
#   lieu        reactiveVal : point choisi (mod_localisation_server) ou NULL
#   naviguer    function(onglet) : change d'onglet (lien « changer de point »)
#   declencheur reactiveVal : un point vient d'etre choisi sur la carte (CO, terrain) ;
#               le bilan du point s'etablit alors sans autre geste
#   transport   reactive : ou lire les fichiers de la carte (carte_transport())
#   mobile      reactive : ecran etroit
#   executer    requete HTTP (injectable : les tests n'ont pas de reseau)
# ==============================================================================

SYNTHESE_AIDE_ECHELLE <- c(
  departement = "La moyenne des mailles de 8 km du département, à poids égaux.",
  region      = "La moyenne des mailles de 8 km de la région, à poids égaux.",
  fiches      = "Vos fiches replacées dans leur contexte climatique : une fiche, la valeur de sa maille l'année de la fiche.",
  point       = "La valeur de la maille de 8 km la plus proche du point.")

# ------------------------------------------------------------------------------
# Interface
# ------------------------------------------------------------------------------

#' @export
mod_synthese_ui <- function(id, profil) {
  ns <- shiny::NS(id)
  ech <- SYNTHESE_ECHELLES[profil$echelles]
  shiny::tagList(
    shiny::h2(class = "cf-titre-section", "Bilan synthétique"),
    shiny::p(class = "cf-chapeau", paste(
      "Un bilan court d'un territoire, d'un point ou de vos fiches : les grandeurs que vous cochez,",
      "en graphique et en tableau, avec quelques phrases pour les lire.",
      "Les valeurs viennent des mailles de 8 km de la carte.")),

    shiny::uiOutput(ns("indisponible")),

    shiny::div(
      class = "cf-carte",
      shiny::h3(class = "cf-carte__titre", "1. Quoi analyser ?"),
      shiny::div(
        class = "cf-pastilles",
        shiny::radioButtons(ns("echelle"), "Échelle", choices = stats::setNames(names(ech), unname(ech)),
                            selected = profil$echelles[1], inline = TRUE)),
      shiny::uiOutput(ns("cible"))),

    shiny::div(
      class = "cf-carte",
      shiny::h3(class = "cf-carte__titre", "2. Année et contenu"),
      shiny::uiOutput(ns("reglages"))),

    shiny::div(
      class = "cf-barre-action cf-synt__action",
      shiny::div(class = "cf-barre-action__etat", shiny::uiOutput(ns("etat"))),
      shiny::actionButton(ns("calculer"),
                          label = shiny::tagList(cf_icone("lancer"), "Établir le bilan"),
                          class = "cf-btn cf-btn--principal cf-btn--large")),

    shiny::uiOutput(ns("resultats"))
  )
}


# ------------------------------------------------------------------------------
# Constructeurs purs de la page de resultats
# ------------------------------------------------------------------------------

#' Les phrases generales : un encadre, une phrase par paragraphe.
#' @keywords internal
synthese_bref_ui <- function(phrases) {
  shiny::div(class = "cf-synt__bref", role = "note", lapply(phrases, shiny::p))
}

#' Remarques sur les lieux ecartes (hors grille, zone trop grande...).
#' @keywords internal
synthese_remarques_ui <- function(tl) {
  r <- tl[nzchar(tl$note) & (!tl$retenu | grepl("plus petite", tl$note)), , drop = FALSE]
  if (!nrow(r)) return(NULL)
  shiny::div(
    class = "cf-alerte cf-alerte--attention", role = "status",
    shiny::span(class = "cf-alerte__titre", "À propos des territoires"),
    shiny::tags$ul(lapply(seq_len(nrow(r)), function(i) shiny::tags$li(sprintf("%s : %s.", r$lieu[i], r$note[i])))))
}

#' Une carte de resultat : titre, phrases, graphique, lecture.
#' @keywords internal
synthese_carte_graphe_ui <- function(ns, id, titre, phrases = character(0), lecture = "") {
  shiny::div(
    class = "cf-carte cf-synt__graphe",
    shiny::h3(class = "cf-carte__titre", titre),
    if (length(phrases)) shiny::tags$ul(class = "cf-synt__phrases", lapply(phrases, shiny::tags$li)),
    shiny::plotOutput(ns(paste0("gr_", id)), height = "340px"),
    if (nzchar(lecture)) shiny::tags$details(
      class = "cf-details",
      shiny::tags$summary("Comment lire ce graphique"),
      shiny::div(class = "cf-details__corps", shiny::p(lecture))))
}


# ------------------------------------------------------------------------------
# Serveur
# ------------------------------------------------------------------------------

#' @export
mod_synthese_server <- function(id, profil, lieu, naviguer = function(onglet) NULL,
                                declencheur = shiny::reactiveVal(NULL),
                                transport = shiny::reactive(carte_transport()),
                                mobile = shiny::reactive(FALSE),
                                executer = httr2::req_perform) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    avec_sf <- requireNamespace("sf", quietly = TRUE)

    # -- Le jeu de donnees de la carte ----------------------------------------------
    jeu_brut <- shiny::reactive({
      tryCatch(cmp_jeu(transport(), executer = executer), error = function(e) {
        journaliser_erreur("Bilan synthétique : fichiers de la carte", e)
        e
      })
    })
    jeu <- shiny::reactive({ j <- jeu_brut(); if (inherits(j, "error")) NULL else j })

    output$indisponible <- shiny::renderUI({
      if (!avec_sf) {
        return(shiny::div(
          class = "cf-alerte cf-alerte--attention", role = "status",
          shiny::span(class = "cf-alerte__titre", "Bilan indisponible sur ce serveur"),
          "Le paquet R « sf », nécessaire pour rattacher les territoires aux mailles, n'y est pas installé."))
      }
      if (!inherits(jeu_brut(), "error")) return(NULL)
      shiny::div(
        class = "cf-alerte cf-alerte--info", role = "status",
        shiny::span(class = "cf-alerte__titre", "Les données de la carte ne sont pas disponibles"),
        shiny::p("Le bilan s'appuie sur les valeurs précalculées de la carte. Elles ne se chargent pas pour le moment ; l'erreur est consignée dans le journal."))
    })

    # -- Catalogue --------------------------------------------------------------------
    dispo <- shiny::reactive({ j <- jeu(); shiny::req(j); synthese_catalogue(j, profil) })
    groupes <- shiny::reactive(synthese_groupes(dispo()))
    catalogue_pret <- shiny::reactiveVal(FALSE)

    # Grandeurs cochees dans un jeu de cases (g_ : graphiques, t_ : tableau). Tant que le
    # catalogue n'est pas affiche, le jeu de base du profil tient lieu de coche.
    cochees <- function(prefixe) {
      d <- names(dispo())
      if (!isTRUE(catalogue_pret())) return(synthese_base(profil$id, d)[[sub("_$", "", prefixe)]])
      v <- unlist(lapply(names(groupes()), function(f) input[[paste0(prefixe, f)]]), use.names = FALSE)
      intersect(d, v)
    }

    output$reglages <- shiny::renderUI({
      j <- jeu(); g <- groupes(); d <- dispo()
      shiny::req(j)
      base <- synthese_base(profil$id, names(d))
      colonne <- function(prefixe, titre, sel) {
        shiny::div(
          class = "cf-synt__colonne",
          shiny::h4(class = "cf-synt__soustitre", titre),
          lapply(names(g), function(f) shiny::checkboxGroupInput(
            ns(paste0(prefixe, f)), label = g[[f]]$libelle,
            choiceNames = lapply(unname(g[[f]]$choix), shiny::span),
            choiceValues = names(g[[f]]$choix),
            selected = intersect(sel, names(g[[f]]$choix)), width = "100%")))
      }
      shiny::isolate(catalogue_pret(TRUE))
      shiny::tagList(
        shiny::div(
          class = "cf-synt__reglages",
          shiny::selectInput(ns("annee"), "Année du bilan", choices = rev(j$annees),
                             selected = shiny::isolate(input$annee) %||% max(j$annees),
                             width = "11rem", selectize = FALSE),
          shiny::selectInput(ns("ref"), "Période de référence (normale)", choices = names(CONFIG$references),
                             selected = shiny::isolate(input$ref) %||% CONFIG$reference_defaut,
                             width = "14rem", selectize = FALSE)),
        shiny::tags$details(
          class = "cf-details", open = if (!isTRUE(profil$restreint)) NA,
          shiny::tags$summary("Grandeurs à inclure"),
          shiny::div(
            class = "cf-details__corps",
            shiny::div(
              class = "cf-synt__actions",
              shiny::actionButton(ns("jeu_base"), "Jeu de base", class = "cf-btn cf-btn--sec"),
              shiny::actionButton(ns("tout"), "Tout cocher", class = "cf-btn cf-btn--sec"),
              shiny::actionButton(ns("aucun"), "Tout décocher", class = "cf-btn cf-btn--sec")),
            shiny::div(
              class = "cf-synt__catalogue",
              colonne("g_", "Graphiques", base$g),
              colonne("t_", "Lignes du tableau de synthèse", base$t)))),
        shiny::p(class = "cf-mention", paste(
          "Valeurs des mailles précalculées, avec les seuils par défaut : les réglages de l'onglet",
          "Paramètres ne s'y appliquent pas.")))
    })

    cocher <- function(g, t) {
      for (f in names(groupes())) {
        ch <- names(groupes()[[f]]$choix)
        shiny::updateCheckboxGroupInput(session, paste0("g_", f), selected = intersect(g, ch))
        shiny::updateCheckboxGroupInput(session, paste0("t_", f), selected = intersect(t, ch))
      }
    }
    shiny::observeEvent(input$jeu_base, { b <- synthese_base(profil$id, names(dispo())); cocher(b$g, b$t) })
    shiny::observeEvent(input$tout,     cocher(names(dispo()), names(dispo())))
    shiny::observeEvent(input$aucun,    cocher(character(0), character(0)))

    # -- Fiches importees ---------------------------------------------------------------
    fiches <- shiny::reactiveVal(fiches_vide())
    fiches_msg <- shiny::reactiveVal(character(0))

    shiny::observeEvent(input$fichier_fiches, {
      f <- input$fichier_fiches
      shiny::req(f, nrow(f) == 1L)
      r <- fiches_lire(f$datapath[1], f$name[1])
      fiches(r$fiches); fiches_msg(r$problemes)
    })
    shiny::observeEvent(input$api_fiches, {
      r <- fiches_lire_api()
      fiches(r$fiches); fiches_msg(r$problemes)
    })
    shiny::observeEvent(input$vider_fiches, { fiches(fiches_vide()); fiches_msg(character(0)) })

    output$modele_fiches <- shiny::downloadHandler(
      filename = function() "modele_fiches.csv",
      content = function(file) ecrire_csv_avec_metadonnees(fiches_modele_csv(), file, character(0)))

    output$fiches_etat <- shiny::renderUI({
      f <- fiches(); m <- fiches_msg()
      shiny::tagList(
        if (nrow(f)) shiny::p(class = "cf-synt__compte", sprintf("%d fiche%s importée%s.", nrow(f),
                                                              if (nrow(f) > 1L) "s" else "", if (nrow(f) > 1L) "s" else "")),
        comparer_messages_ui(m))
    })

    # -- Cible : ce qui est analyse ------------------------------------------------------
    point_defaut_dep <- function() {
      p <- shiny::isolate(lieu())
      d <- if (!is.null(p) && avec_sf) tryCatch(territoires_departement_du_point(p$lon, p$lat),
                                                error = function(e) NA_character_) else NA_character_
      if (is.na(d)) CONFIG$synthese$defaut$departement else d
    }

    output$cible <- shiny::renderUI({
      e <- input$echelle
      shiny::req(e, avec_sf || e %in% c("point"))
      aide <- shiny::div(class = "cf-aide", SYNTHESE_AIDE_ECHELLE[[e]])
      corps <- switch(e,
        departement = {
          ch <- territoires_choix("departement")
          if (isTRUE(profil$multi)) {
            shiny::tagList(
              shiny::selectInput(ns("deps"), "Départements", choices = ch, multiple = TRUE, selectize = FALSE,
                                 selected = shiny::isolate(input$deps) %||% point_defaut_dep(),
                                 size = 8, width = "100%"),
              shiny::div(class = "cf-aide", sprintf("Ctrl (⌘ sur Mac) pour en choisir plusieurs, %d au plus.",
                                                    CONFIG$synthese$territoires_max)))
          } else {
            shiny::selectInput(ns("dep"), "Département", choices = c("Choisir un département" = "", ch),
                               selected = shiny::isolate(input$dep) %||% point_defaut_dep(),
                               selectize = FALSE, width = "22rem")
          }
        },
        region = shiny::div(
          class = "cf-synt__choix",
          shiny::checkboxGroupInput(
            ns("regs"), "Régions", choices = territoires_choix("region"),
            selected = shiny::isolate(input$regs) %||% CONFIG$synthese$defaut$region)),
        fiches = shiny::tagList(
          shiny::fileInput(ns("fichier_fiches"), "Importer des fiches",
                           accept = c(".csv", ".txt", ".tsv", ".geojson", ".json", ".gpkg", ".zip"),
                           buttonLabel = "Parcourir…", placeholder = "Aucun fichier choisi", width = "100%"),
          shiny::div(class = "cf-aide", sprintf(paste(
            "Un point par fiche : CSV avec les colonnes latitude et longitude, et si possible date, essence",
            "et problème ; ou GeoJSON, GeoPackage, shapefile zippé. %d fiches au plus."),
            CONFIG$synthese$fiches_max)),
          shiny::div(
            class = "cf-synt__actions",
            shiny::downloadButton(ns("modele_fiches"), "Modèle de CSV", class = "cf-btn cf-btn--sec"),
            if (nzchar(CONFIG$sylvasan$api_fiches))
              shiny::actionButton(ns("api_fiches"), "Charger depuis l'application de terrain", class = "cf-btn cf-btn--sec"),
            shiny::actionButton(ns("vider_fiches"), "Effacer", class = "cf-btn cf-btn--sec")),
          shiny::uiOutput(ns("fiches_etat")),
          shiny::div(
            class = "cf-synt__choix",
            shiny::checkboxGroupInput(
              ns("fb"), "Répartition des fiches à inclure",
              choices = stats::setNames(names(SYNTHESE_BLOCS_FICHES), unname(SYNTHESE_BLOCS_FICHES)),
              selected = shiny::isolate(input$fb) %||% names(SYNTHESE_BLOCS_FICHES)))),
        point = shiny::uiOutput(ns("point_etat")))
      shiny::tagList(corps, if (!identical(e, "fiches")) aide)
    })

    output$point_etat <- shiny::renderUI({
      p <- lieu()
      if (is.null(p)) {
        return(shiny::p(class = "cf-mention", if ("accueil" %in% profil$onglets)
          "Aucun point choisi : désignez-le dans l'Accueil." else
          "Aucun point choisi : cherchez une commune ou touchez « Utiliser ma position », ci-dessus, ou cliquez une maille sur la carte."))
      }
      shiny::p(class = "cf-synt__compte",
               shiny::strong(p$nom %||% "Point choisi"),
               sprintf(" — %s °N, %s °E ", fmt_fr(p$lat, 4), fmt_fr(p$lon, 4)),
               if ("accueil" %in% profil$onglets) shiny::actionLink(ns("vers_accueil"), "Changer de point"))
    })
    shiny::observeEvent(input$vers_accueil, naviguer("accueil"))

    # -- Calcul --------------------------------------------------------------------------------
    res_val <- shiny::reactiveVal(NULL)

    # Ce qu'il faut analyser a l'echelle donnee : des lieux ou des fiches, ou la raison de
    # ne rien faire.
    cibles <- function(e) {
      vide <- function(m) list(lieux = list(), fiches = NULL, vide = m)
      switch(e,
        departement = {
          codes <- if (isTRUE(profil$multi)) input$deps else input$dep
          codes <- utils::head(codes[nzchar(codes)], CONFIG$synthese$territoires_max)
          if (!length(codes)) return(vide("Choisissez au moins un département."))
          list(lieux = territoires_lieux("departement", codes), fiches = NULL, vide = NULL)
        },
        region = {
          codes <- utils::head(input$regs, CONFIG$synthese$territoires_max)
          if (!length(codes)) return(vide("Choisissez au moins une région."))
          list(lieux = territoires_lieux("region", codes), fiches = NULL, vide = NULL)
        },
        fiches = {
          if (!nrow(fiches())) return(vide("Importez d'abord un fichier de fiches."))
          list(lieux = list(), fiches = fiches(), vide = NULL)
        },
        point = {
          p <- lieu()
          if (is.null(p)) return(vide("Choisissez d'abord un point."))
          list(lieux = list(cmp_lieu_point(cmp_nom_propre(p$nom %||% "Point choisi", "Point choisi"), p$lon, p$lat)),
               fiches = NULL, vide = NULL)
        },
        vide("Échelle inconnue."))
    }

    ctx_synth <- function(j) {
      ref <- CONFIG$references[[input$ref %||% CONFIG$reference_defaut]] %||% CONFIG$references[[CONFIG$reference_defaut]]
      a <- suppressWarnings(as.integer(input$annee %||% max(j$annees)))
      a <- if (length(a) == 1L && is.finite(a) && a %in% j$annees) a else max(j$annees)
      list(ctx = ctx_creer(ref = ref, affichage = j$annees, courante = a), annee = a)
    }

    signature <- function(e) {
      paste(e, paste(sort(c(input$deps, input$dep, input$regs)), collapse = ","), nrow(fiches()),
            input$annee, input$ref, paste(cochees("g_"), collapse = ","), paste(cochees("t_"), collapse = ","),
            sep = "|")
    }

    calculer <- function(e) {
      j <- jeu()
      if (is.null(j) || !avec_sf) return()
      c0 <- cibles(e)
      if (!is.null(c0$vide)) { res_val(list(vide = c0$vide)); return() }
      cx <- ctx_synth(j)
      g <- cochees("g_"); t <- cochees("t_")
      fb <- if (identical(e, "fiches")) (input$fb %||% character(0)) else character(0)
      if (!length(g) && !length(t) && !length(fb)) {
        res_val(list(vide = "Cochez au moins un graphique ou une ligne de tableau.")); return()
      }
      r <- shiny::withProgress(message = "Calcul du bilan", value = 0.3, {
        tryCatch(synthese_preparer(e, c0$lieux, c0$fiches, j, g, t, cx$ctx, cx$annee, profil, executer),
                 error = function(err) { journaliser_erreur("Bilan synthétique : calcul", err); NULL })
      })
      if (!is.null(r)) r$blocs_fiches <- fb
      res_val(list(res = r, signature = signature(e)))
    }

    shiny::observeEvent(input$calculer, calculer(input$echelle))
    shiny::observeEvent(declencheur(), {
      shiny::req(declencheur(), "point" %in% profil$echelles)
      shiny::updateRadioButtons(session, "echelle", selected = "point")
      calculer("point")
    }, ignoreInit = TRUE)

    output$etat <- shiny::renderUI({
      v <- res_val()
      if (is.null(v) || is.null(v$signature)) {
        return(shiny::span(class = "cf-mention", "Choisissez, puis établissez le bilan."))
      }
      if (!identical(v$signature, signature(input$echelle))) {
        return(shiny::span(class = "cf-mention cf-synt__perime", role = "status",
                           "Vos choix ont changé depuis le bilan affiché."))
      }
      shiny::span(class = "cf-mention", "Le bilan affiché correspond à vos choix.")
    })

    # -- Resultats ----------------------------------------------------------------------------
    output$resultats <- shiny::renderUI({
      v <- res_val()
      if (is.null(v)) return(NULL)
      if (!is.null(v$vide)) {
        return(shiny::div(class = "cf-alerte cf-alerte--info", role = "status", shiny::p(v$vide)))
      }
      r <- v$res
      if (is.null(r)) {
        return(shiny::div(class = "cf-alerte cf-alerte--attention", role = "status",
          shiny::span(class = "cf-alerte__titre", "Le bilan n'a pas pu être calculé"),
          "Les fichiers de la carte ne se lisent pas ; l'erreur est consignée dans le journal."))
      }
      fiches_ <- identical(r$echelle, "fiches")
      if (!fiches_ && !any(r$tl$retenu)) {
        return(shiny::tagList(
          shiny::div(class = "cf-alerte cf-alerte--attention", role = "status",
            shiny::span(class = "cf-alerte__titre", "Aucune maille pour ce choix"),
            "Le point est hors de la grille SAFRAN, ou le territoire est trop grand."),
          synthese_remarques_ui(r$tl)))
      }
      tab <- if (fiches_) synthese_tableau_resume_fiches(r) else synthese_tableau(r)
      tab_f <- if (fiches_ && length(r$cles_t)) synthese_tableau_fiches(r) else NULL

      shiny::tagList(
        shiny::div(
          class = "cf-carte",
          shiny::h3(class = "cf-carte__titre", sprintf("Bilan %d — %s", r$annee, synthese_libelle(r))),
          synthese_bref_ui(synthese_phrases_generales(r)),
          synthese_remarques_ui(r$tl)),
        if (!is.null(tab)) shiny::div(
          class = "cf-carte",
          shiny::h3(class = "cf-carte__titre", if (fiches_) "Contexte climatique des fiches" else "Tableau de synthèse"),
          synthese_table_tag(tab, if (fiches_) "Contexte climatique des fiches" else sprintf("Valeur de %d et écart à la normale", r$annee)),
          shiny::p(class = "cf-mention", sprintf(
            "Écart à la normale %s. « Proche de la normale » : moins d'un demi écart-type de cette période.",
            r$ctx$ref_label))),
        lapply(r$cles_g, function(cle) synthese_carte_graphe_ui(
          ns, cle, CARTE_COUCHES[[cle]]$titre, synthese_phrases_cle(r, cle), synthese_lecture_cle(cle))),
        lapply(r$blocs_fiches %||% character(0), function(b) shiny::div(
          class = "cf-carte cf-synt__graphe",
          shiny::h3(class = "cf-carte__titre", SYNTHESE_BLOCS_FICHES[[b]]),
          shiny::plotOutput(ns(paste0("gr_", b)), height = "300px"),
          synthese_table_tag(synthese_tableau_repartition(r, sub("^f_", "", b)), SYNTHESE_BLOCS_FICHES[[b]]))),
        if (!is.null(tab_f)) shiny::tags$details(
          class = "cf-details",
          shiny::tags$summary(sprintf("Tableau des fiches (%d)", nrow(tab_f))),
          shiny::div(class = "cf-details__corps",
                     synthese_table_tag(tab_f, "Fiches et contexte climatique de leur année"))),
        shiny::div(
          class = "cf-synt__actions cf-synt__export",
          shiny::downloadButton(ns("html"), "Bilan à emporter (HTML)", class = "cf-btn cf-btn--sec"),
          if (requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available())
            shiny::downloadButton(ns("docx"), "Bilan Word", class = "cf-btn cf-btn--sec"),
          shiny::downloadButton(ns("csv"), "Tableau (CSV)", class = "cf-btn cf-btn--sec"),
          if (!fiches_) shiny::downloadButton(ns("csv_series"), "Séries annuelles (CSV)", class = "cf-btn cf-btn--sec")),
        shiny::tags$details(
          class = "cf-details",
          shiny::tags$summary("Méthode et limites"),
          shiny::div(class = "cf-details__corps", lapply(synthese_methode(r), shiny::p))))
    })

    # Un graphique par grandeur et par repartition de fiches : declares une fois, evalues
    # seulement s'ils figurent dans le bilan affiche.
    for (cle in c(names(CARTE_COUCHES), names(SYNTHESE_BLOCS_FICHES))) local({
      k <- cle
      output[[paste0("gr_", k)]] <- shiny::renderPlot({
        r <- res_val()$res
        shiny::req(r, k %in% c(r$cles_g, r$blocs_fiches))
        synthese_graphe(r, k, mobile())
      }, res = 96)
    })

    # -- Exports -----------------------------------------------------------------------------------
    res_courant <- function() { r <- res_val()$res; shiny::req(r); r }
    nom_export <- function(prefixe, ext) {
      r <- res_courant()
      nom_fichier_export(prefixe, list(nom = if (identical(r$echelle, "fiches")) "fiches" else synthese_libelle(r)), ext)
    }
    output$html <- shiny::downloadHandler(
      filename = function() nom_export("bilan", "html"),
      content = function(file) { r <- res_courant(); synthese_ecrire_html(r, file, r$blocs_fiches) })
    output$docx <- shiny::downloadHandler(
      filename = function() nom_export("bilan", "docx"),
      content = function(file) { r <- res_courant(); synthese_ecrire_docx(r, file, r$blocs_fiches) })
    output$csv <- shiny::downloadHandler(
      filename = function() nom_export("bilan", "csv"),
      content = function(file) synthese_ecrire_csv(res_courant(), file))
    output$csv_series <- shiny::downloadHandler(
      filename = function() nom_export("bilan_series", "csv"),
      content = function(file) synthese_ecrire_csv_series(res_courant(), file))

    invisible(NULL)
  })
}
