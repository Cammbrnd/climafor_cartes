# ==============================================================================
# app.R — POINT D'ENTREE
# ------------------------------------------------------------------------------
# Assemblage uniquement : ni calcul, ni requete, ni mise en forme metier.
# Chaque onglet delegue a son module, ce qui permettra d'ajouter les modules a
# venir (territoire, ravageurs, aleas) avec deux lignes.
#
# POURQUOI bootstrapPage() ET NON UN SIMPLE tagList()
# ---------------------------------------------------
# A NE PAS SUPPRIMER. Les fonctions de page de Shiny — fluidPage(),
# navbarPage(), bootstrapPage() — ne se contentent pas d'envelopper le contenu :
# ce sont elles qui declarent la DEPENDANCE BOOTSTRAP (CSS et JavaScript).
# tabsetPanel() emet bien le balisage attendu (ul.nav-tabs + div.tab-content),
# mais sans cette dependance il n'en reste qu'une liste a puces : aucune mise en
# forme, et surtout pas le greffon JavaScript qui bascule d'un panneau a
# l'autre. C'est exactement ce qui se produisait quand l'interface etait
# construite avec un tagList() nu. bootstrapPage() est la plus discrete des
# trois fonctions de page : elle n'ajoute ni conteneur ni marge, elle charge
# seulement ce qu'il faut.
#
# ORGANISATION DE L'INTERFACE (version 4)
# ---------------------------------------
# 0. (v4) SOURCE DES DONNEES = STATION METEO-FRANCE OU GRILLE SAFRAN. Des qu'un
#    point est designe, mod_station propose la station la plus proche avec sa
#    qualite ; l'analyse se fait avec la source retenue. Le bandeau
#    « actualites » (pluie 30 jours, prevision, restrictions d'eau), place avant
#    la localisation, a ete supprime : il ne servait pas a l'analyse.

# 1. UN ONGLET ACCUEIL dedie au choix du lieu et de la periode. Le parcours est
#    lineaire : on configure dans l'accueil, on lit dans le bilan. On ne fait
#    pas les deux au meme endroit.
#
# 2. « LANCER L'ANALYSE » BASCULE AUTOMATIQUEMENT SUR L'ONGLET BILAN. Sans cela
#    l'utilisateur reste devant son formulaire, sans rien qui lui dise que le
#    resultat l'attend un onglet plus loin. Le changement d'onglet EST le retour
#    visible de l'action.
#
# 3. LE CHEMIN DU RETOUR EST EXPLICITE. La barre de contexte, en tete du bilan,
#    porte un bouton qui ramene a l'accueil. Les deux onglets conservent leur
#    etat : revenir modifier un reglage ne detruit ni la carte ni les resultats
#    deja calcules.
#
# 4. UNE SEULE BARRE en haut. L'ancienne version empilait un en-tete de 110 px
#    PUIS une barre d'onglets : 170 px consommes avant la premiere information.
#
# 5. UNE SEULE BARRE DE CONTEXTE. Le bandeau du module bilan, qui repetait mot
#    pour mot la meme information, est desormais reserve a l'impression.
#
# 6. (v1.4) L'ANALYSE SE PARTAGE PAR SON ADRESSE. Apres chaque analyse reussie,
#    l'adresse de la page porte le point, la source, les periodes, la fenetre
#    et les seuils modifies (src/utils_partage.R). Recharger la page, ou ouvrir
#    le lien d'un collegue (bouton « Copier le lien » de la barre de contexte),
#    reconstruit les memes reglages et relance l'analyse. L'interface devient
#    pour cela une FONCTION de la requete : les valeurs de depart des champs sont
#    ecrites dans la page elle-meme, pas corrigees apres coup.
#
# Les identifiants d'input et les modules sont inchanges.
# ==============================================================================

# global.R peut s'appeler global.r (partage reseau) : on le retrouve quelle que soit la casse.
if (!exists("CONFIG")) source(list.files(".", pattern = "^global\\.r$", ignore.case = TRUE)[1])


# ==============================================================================
# INTERFACE
# ==============================================================================

# `etat` : etat decode d'un lien partage (partage_decoder), ou NULL.
ui_construire <- function(etat = NULL) shiny::bootstrapPage(

  title = sprintf("%s — %s", CONFIG$app$nom, CONFIG$app$sous_titre),
  # <html lang="fr"> : sans lui, un lecteur d'ecran lit le francais avec la
  # prononciation de sa langue par defaut (WCAG 3.1.1, niveau A).
  lang = "fr",

  shiny::tags$head(
    shiny::tags$meta(name = "viewport",
                     content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    shiny::tags$meta(name = "description",
                     content = "Bilan météorologique et climatique pour la gestion forestière"),
    # Favicon : sans lien declare, le navigateur demande /favicon.ico et recoit
    # une erreur 404 a chaque chargement. Pictogramme du DSF (l'oeil), en PNG
    # transparent de 64 px : le navigateur le reduit a la taille de l'onglet.
    shiny::tags$link(rel = "icon", type = "image/png", sizes = "64x64", href = "favicon.png"),
    shiny::tags$link(rel = "stylesheet", href = "climafor.css"),
    shiny::tags$script(src = "app.js"),
    # Carte (v1.8) : ce petit chargeur n'appelle MapLibre qu'a la premiere
    # ouverture de l'onglet.
    shiny::tags$script(src = "carte/carte.js")
  ),

  # Lien d'evitement : premier element focusable de la page (exigence RGAA).
  shiny::tags$a(class = "cf-evitement", href = "#contenu", "Aller au contenu"),

  # -- Barre d'application ---------------------------------------------------
  # Logo du DSF, puis nom du service, sans filet entre les deux. L'image porte
  # un texte de remplacement : c'est un logo porteur de sens, pas un ornement.
  shiny::tags$header(
    class = "cf-barre",
    shiny::div(
      class = "cf-barre__interieur",
      shiny::div(
        class = "cf-entete-marques",
        shiny::img(class = "cf-logo cf-logo--operateur",
                   src = "logo_dsf.png",
                   alt = "Correspondant observateur — Département de la santé des forêts")
      ),
      shiny::div(
        class = "cf-marque",
        shiny::h1(class = "cf-marque__titre", CONFIG$app$nom),
        shiny::div(class = "cf-marque__soustitre", CONFIG$app$sous_titre)
      )
    )
  ),

  # -- Onglets ---------------------------------------------------------------
  # <main> : repere de contenu principal. Le lien d'evitement pointe dessus ;
  # tabindex = -1 lui permet de recevoir le focus quand on l'active.
  shiny::tags$main(
    class = "cf-onglets", id = "contenu", tabindex = "-1",

    shiny::tabsetPanel(
      id = "onglets", type = "tabs",

      # ---- Onglet 1 : accueil, lieu et periode -----------------------------
      shiny::tabPanel(
        title = cf_onglet_titre("accueil", "Accueil"), value = "accueil",
        shiny::div(
          class = "cf-conteneur",

          shiny::uiOutput("accueil"),

          shiny::div(
            class = "cf-accueil-grille",

            # Colonne 1 : le point
            shiny::div(
              class = "cf-carte",
              cf_etape(1, "Où ?", "le point à analyser"),
              mod_localisation_ui("loc"),
              # Apparait dès qu'un point est désigné : station proposée et qualité
              mod_station_ui("station")
            ),

            # Colonne 2 : la periode
            shiny::div(
              class = "cf-carte",
              cf_etape(2, "Quand ?", "référence et année comparée"),
              mod_periode_ui("periode", etat)
            )
          ),

          # Barre d'action collante : le bouton reste visible pendant qu'on
          # fait defiler les reglages, y compris sur un ecran de telephone.
          shiny::div(
            class = "cf-barre-action",
            shiny::div(class = "cf-barre-action__etat",
                       shiny::uiOutput("etat_chargement")),
            shiny::actionButton(
              "lancer",
              label = shiny::tagList(cf_icone("lancer"), "Lancer l'analyse"),
              class = "cf-btn cf-btn--principal cf-btn--large")
          ),

          # Messages du service de donnees : ils concernent le chargement,
          # donc ils s'affichent la ou l'on a lance le chargement.
          shiny::uiOutput("messages_api")
        )
      ),

      # ---- Onglet 2 : la carte (v1.8) --------------------------------------
      # Un indicateur par maille SAFRAN pour toute la France. Le clic sur une
      # maille ouvre son analyse : l'onglet est une porte d'entree vers le bilan.
      shiny::tabPanel(
        title = cf_onglet_titre("carte", "Carte"), value = "carte",
        shiny::div(class = "cf-conteneur", mod_carte_ui("carte"))),

      # ---- Onglet 3 : le bilan ---------------------------------------------
      shiny::tabPanel(
        title = cf_onglet_titre("bilan", "Bilan climatique"), value = "bilan",
        shiny::div(
          class = "cf-conteneur",
          shiny::uiOutput("recap"),
          shiny::uiOutput("bilan_vide"),
          mod_bilan_ui("bilan")
        )
      ),

      # ---- Onglet 4 : rapport ----------------------------------------------
      shiny::tabPanel(
        title = cf_onglet_titre("rapport", "Rapport"), value = "rapport",
        shiny::div(class = "cf-conteneur", mod_rapport_ui("rapport"))),

      # ---- Onglet 5 : parametres -------------------------------------------
      shiny::tabPanel(
        title = cf_onglet_titre("parametres", "Paramètres"), value = "parametres",
        shiny::div(class = "cf-conteneur", mod_parametres_ui("parametres", etat))),

      # ---- Onglet 6 : sources ----------------------------------------------
      # Libelle court : six onglets doivent tenir sur la largeur d'un
      # telephone. Le titre complet figure en tete de l'onglet lui-meme.
      shiny::tabPanel(
        title = cf_onglet_titre("sources", "Sources"), value = "sources",
        shiny::div(class = "cf-conteneur", mod_sources_ui("sources")))

      # ---- Emplacements des modules a venir --------------------------------
      # shiny::tabPanel(cf_onglet_titre("bilan", "Territoire"), value = "territoire",
      #                 shiny::div(class = "cf-conteneur", mod_territoire_ui("territoire"))),
      # shiny::tabPanel(... "Ravageurs" ...),
      # shiny::tabPanel(... "Aléas" ...)
    )
  ),

  # -- Pied de page ----------------------------------------------------------
  shiny::tags$footer(
    class = "cf-pied",
    shiny::div(
      class = "cf-pied__interieur",
      shiny::p(shiny::strong(sprintf("%s version %s", CONFIG$app$nom,
                                     CONFIG$app$version)),
               " — ", CONFIG$app$organisme),
      shiny::p(paste("Données : stations Météo-France (données climatologiques de",
                     "base) ou réanalyse SAFRAN-ISBA (Météo-France, via GeoSAS INRAE).",
                     "Licence Etalab 2.0.")),
      shiny::p(paste("Ce bilan décrit le climat d'une station ou d'une maille de",
                     "8 km. Il ne se substitue pas à un diagnostic stationnel."))
    )
  )
)

# L'interface est calculee a chaque ouverture de page : l'adresse demandee
# (?x=...&y=...) en fixe les valeurs de depart. Une adresse illisible, ou sans
# point exploitable, donne l'interface habituelle : jamais une erreur.
ui <- function(request) {
  etat <- tryCatch(partage_decoder(request$QUERY_STRING), error = function(e) NULL)
  ui_construire(etat)
}


# ==============================================================================
# SERVEUR
# ==============================================================================

server <- function(input, output, session) {

  # -- Largeur d'écran (pilotée par www/app.js) -----------------------------
  # Sous 900 px, les graphiques passent en version compacte : moins de
  # graduations, typographie réduite.
  mobile <- shiny::reactive({
    l <- input$cf_largeur
    isTRUE(is.numeric(l)) && l < 900
  })

  # Appareil tactile (pointeur grossier) : les graphiques interactifs perdent
  # leur zoom et leur déplacement au doigt, qui volent le défilement de la page.
  tactile <- shiny::reactive(isTRUE(input$cf_tactile))

  # -- Modules de réglage ----------------------------------------------------
  parametres <- mod_parametres_server("parametres")
  lieu       <- mod_localisation_server("loc")
  ctx        <- mod_periode_server("periode", seuils = parametres$seuils)

  # Source des données : station Météo-France proposée, ou grille SAFRAN.
  # `choix_initial` : source demandée par un lien partagé, consommée par la
  # première recherche de stations.
  choix_initial  <- shiny::reactiveVal(NULL)
  source_donnees <- mod_station_server("station", lieu, choix_initial)

  mod_sources_server("sources")

  # -- Onglet Carte (v1.8) : un clic sur une maille ouvre l'analyse de son point --
  mod_carte_server("carte", onglet = shiny::reactive(input$onglets),
                   ouvrir_maille = mod_carte_ouverture_server(
                     lieu, choix_initial, source_donnees, function() lancer_analyse()))

  # -- Navigation ------------------------------------------------------------
  aller_a <- function(onglet) {
    shiny::updateTabsetPanel(session, "onglets", selected = onglet)
  }

  # Retours vers l'accueil : depuis la barre de contexte du bilan, et depuis
  # l'onglet Bilan quand celui-ci est encore vide.
  shiny::observeEvent(input$rouvrir_reglages, aller_a("accueil"))
  shiny::observeEvent(input$revenir_accueil,  aller_a("accueil"))

  # -- Chargement des données ------------------------------------------------
  # Un reactiveVal, pas un eventReactive : il vaut NULL tant qu'aucune analyse
  # n'a été lancée, ce qui permet de distinguer « pas encore demandé » de
  # « demandé sans résultat ». Un eventReactive interromprait silencieusement
  # ses consommateurs.
  donnees  <- shiny::reactiveVal(NULL)
  messages <- shiny::reactiveVal(character(0))

  # Le point et la source AU MOMENT DU CHARGEMENT. Le bilan, le rapport et la
  # barre de contexte doivent nommer ce qui a été analysé, pas ce qui est
  # sélectionné maintenant : on peut changer de point sur l'accueil sans
  # relancer, et les graphiques seraient alors étiquetés avec le mauvais lieu.
  lieu_analyse <- shiny::reactiveVal(NULL)

  cle_selection <- shiny::reactive({
    p <- lieu(); src <- source_donnees()
    if (is.null(p)) return(NA_character_)
    sprintf("%.4f|%.4f|%s", p$lon, p$lat,
            if (!is.null(src) && identical(src$type, "station")) src$station$id else "safran")
  })
  cle_analyse <- shiny::reactiveVal(NA_character_)

  # Années à télécharger : l'union de la référence, de la plage affichée, de
  # l'année comparée et de la plage demandée en Paramètres. Partagée par le
  # lancement et par l'annonce de l'attente (estimer_chargement).
  annees_a_charger <- function() {
    c0 <- ctx()
    toutes <- c(c0$ref, c0$affichage, c0$courante, parametres$annees())
    annees <- seq.int(min(toutes), max(toutes))
    annees[annees >= CONFIG$annee_min &
             annees <= as.integer(format(Sys.Date(), "%Y"))]
  }

  lancer_analyse <- function() {

    p <- lieu()
    if (is.null(p)) {
      shiny::showNotification(
        "Sélectionnez d'abord un point : commune, carte ou coordonnées.",
        type = "warning", duration = 6)
      aller_a("accueil")
      return()
    }

    c0 <- ctx()
    annees <- annees_a_charger()

    src <- source_donnees()
    en_station <- !is.null(src) && identical(src$type, "station")

    # Limites d'usage (audit V2). Le compteur du processus ne s'applique qu'aux
    # analyses qui iront chercher des donnees a distance : SAFRAN dont toutes les
    # annees ne sont pas en cache, ou une station (sa serie n'est pas predictible
    # sans lire les fichiers).
    distant <- en_station || tryCatch(
      estimer_chargement(p$lon, p$lat, annees)$n_telecharger > 0,
      error = function(e) TRUE)
    lim <- limite_controler("analyses", session, distant = distant)
    if (!lim$ok) {
      shiny::showNotification(lim$message, type = "warning", duration = 12)
      return()
    }

    barre <- shiny::Progress$new(session, min = 0, max = 1)
    on.exit(barre$close())
    barre$set(message = if (en_station) sprintf("Station %s", src$station$nom)
                        else "Interrogation de SAFRAN", value = 0)
    progres <- function(f, m) barre$set(value = f, detail = m)

    don <- tryCatch(
      if (en_station) {
        station_charger(src$station, p$lon, p$lat, annees, progres = progres)
      } else {
        safran_charger(p$lon, p$lat, annees, progres = progres)
      },
      error = function(e) {
        messages(paste("Erreur inattendue :", conditionMessage(e)))
        NULL
      })

    if (is.null(don)) return()

    messages(attr(don, "erreurs") %||% character(0))

    if (!nrow(don)) {
      donnees(NULL)
      shiny::showNotification(
        if (en_station)
          paste("Aucune donnée pour cette station. Choisissez une autre source",
                "dans « Source des données », ou la grille SAFRAN.")
        else "Aucune donnée reçue. Le détail figure au bas de cet onglet.",
        type = "error", duration = 10)
      return()
    }

    # Altitude du point (IGN) : la grille SAFRAN et la station n'ont pas la meme
    # altitude que lui. Conservee avec les donnees pour le bilan, le rapport et
    # les exports (D5).
    alt_pt <- suppressWarnings(as.numeric(src$alt %||% NA_real_))
    if (length(alt_pt) == 1L && is.finite(alt_pt)) attr(don, "altitude_point") <- alt_pt

    donnees(don)
    lieu_analyse(p)
    cle_analyse(shiny::isolate(cle_selection()))

    # L'adresse de la page décrit l'analyse AFFICHÉE : recharger la page ou
    # envoyer le lien redonne ce bilan. « replace » : pas d'entrée d'historique
    # par analyse, le bouton « précédent » quitte l'application comme avant.
    etat_partage <- list(
      lon = p$lon, lat = p$lat, nom = p$nom,
      source = if (en_station) src$station$id else "safran",
      ref = range(c0$ref), courante = c0$courante,
      plage = range(c0$affichage), fenetre = c0$fenetre,
      annees_charge = parametres$annees(),
      seuils = partage_seuils_modifies(c0$seuils))
    tryCatch(
      shiny::updateQueryString(paste0("?", partage_encoder(etat_partage)),
                               mode = "replace", session = session),
      error = function(e) message("[ClimaFor] Adresse non mise à jour : ", conditionMessage(e)))

    # Le basculement d'onglet EST le retour visible de l'action : sans lui,
    # l'utilisateur reste devant son formulaire et rien ne lui dit que le
    # résultat l'attend ailleurs. En cas d'échec on reste au contraire sur
    # l'accueil, là où se trouvent les messages d'erreur et les réglages à
    # corriger.
    aller_a("bilan")
  }

  shiny::observeEvent(input$lancer, lancer_analyse())

  # -- Ouverture d'un lien partagé ---------------------------------------------
  # L'interface a déjà reçu les périodes et les seuils du lien (ui_construire).
  # Reste le point et la source : on les pose, puis on attend que la recherche de
  # stations ait désigné la source avant de lancer l'analyse, une seule fois.
  a_restaurer <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(session$clientData$url_search, {
    etat <- tryCatch(partage_decoder(session$clientData$url_search),
                     error = function(e) NULL)
    if (is.null(etat)) return()
    if (length(etat$remarques)) {
      shiny::showNotification(
        paste("Lien partagé :", paste(etat$remarques, collapse = " ")),
        type = "warning", duration = 12)
    }
    choix_initial(etat$source)
    lieu(list(lon = etat$lon, lat = etat$lat, nom = etat$nom, source = "lien partagé"))
    a_restaurer(TRUE)
  }, once = TRUE)

  shiny::observe({
    if (!isTRUE(a_restaurer())) return()
    if (is.null(lieu())) return()
    if (isTRUE(CONFIG$stations$actif) && is.null(source_donnees())) return()
    a_restaurer(FALSE)
    shiny::isolate(lancer_analyse())
  })

  # -- Bilan et rapport ------------------------------------------------------
  resultats <- mod_bilan_server("bilan", donnees, ctx, lieu_analyse, mobile, tactile)
  mod_rapport_server("rapport", donnees, resultats, ctx, lieu_analyse)

  # -- Barre de contexte -----------------------------------------------------
  # Rappelle en permanence quel point, quelle référence et quelle année sont
  # affichés. Sans elle, on parcourt quatre écrans de graphiques sans plus
  # savoir à quoi ils se rapportent.
  output$recap <- shiny::renderUI({
    don <- donnees(); p <- lieu_analyse(); c0 <- ctx()
    if (is.null(don) || !nrow(don) || is.null(p)) return(NULL)
    contexte_ui(don, p, c0)
  })

  # -- Onglet Bilan encore vide ---------------------------------------------
  # Un onglet vide n'apprend rien et laisse croire a une panne. Celui-ci dit ce
  # qui manque et renvoie la ou l'action se fait.
  output$bilan_vide <- shiny::renderUI({
    if (!is.null(donnees())) return(NULL)
    shiny::div(
      class = "cf-carte cf-accueil",
      shiny::h2(class = "cf-carte__titre", "Aucune analyse en cours"),
      shiny::p(paste("Désignez un point et une période dans l'onglet Accueil,",
                     "puis lancez l'analyse : les résultats s'afficheront ici.")),
      shiny::actionButton(
        "revenir_accueil",
        label = shiny::tagList(cf_icone("accueil"), "Aller à l'accueil"),
        class = "cf-btn cf-btn--principal")
    )
  })

  # -- Messages du service de données ---------------------------------------
  # Ce qui rate doit être visible. Un « aucune donnée » sans explication est
  # indiagnosticable, pour l'utilisateur comme pour le mainteneur.
  output$messages_api <- shiny::renderUI({
    msg <- messages()
    if (!length(msg)) return(NULL)
    messages_service_ui(msg, bloquant = is.null(donnees()))
  })

  # -- Chapeau de l'accueil --------------------------------------------------
  # Il explique ce que fait l'application tant qu'aucune analyse n'a tourné,
  # puis s'efface : une fois le premier bilan produit, la consigne n'apprend
  # plus rien et ne fait qu'éloigner les réglages du haut de page.
  output$accueil <- shiny::renderUI({
    if (!is.null(donnees())) return(NULL)
    shiny::p(class = "cf-chapeau", paste(
      "Bilan climatique de n'importe quel point de France métropolitaine, établi",
      "avec la station Météo-France la plus proche ou la grille SAFRAN."))
  })

  # -- État affiché dans la barre d'action ----------------------------------
  # Avant l'analyse, il dit ce qui manque ou ce qui est prêt ; après, ce qui a
  # été chargé. Le bouton n'est jamais actionné à l'aveugle.
  output$etat_chargement <- shiny::renderUI({
    don <- donnees(); p <- lieu(); src <- source_donnees()
    # Une analyse existe mais la sélection a changé depuis : elle n'est plus à jour
    perimee <- !identical(cle_analyse(), cle_selection())
    # Grille SAFRAN : on compte les années déjà en cache pour annoncer le reste
    # (audit, S4). L'estimation n'a de sens qu'avant l'analyse.
    est <- NULL
    en_attente <- is.null(don) || !nrow(don) || perimee
    if (en_attente && !is.null(p) && !identical(src$type, "station")) {
      parametres$cache()  # dependance : cocher/decocher le cache change la phrase
      est <- tryCatch(estimer_chargement(p$lon, p$lat, annees_a_charger()),
                      error = function(e) NULL)
    }
    etat_chargement_ui(don, p, src, perimee, est)
  })

  # -- Alerte si la référence n'est pas couverte ----------------------------
  # Une normale calculée sur une référence tronquée serait fausse sans que
  # l'utilisateur puisse s'en apercevoir : mieux vaut le dire.
  shiny::observe({
    don <- donnees(); c0 <- ctx()
    if (is.null(don) || !nrow(don)) return()
    manquantes <- setdiff(c0$ref, unique(don$annee))
    # Pour une station, la couverture de la référence est jugée par la carte de
    # qualité du bilan (années valides, pas seulement présentes).
    if (length(manquantes) > 2 && is.null(attr(don, "station"))) {
      shiny::showNotification(
        sprintf(paste("%d année(s) de la référence %s ne sont pas chargées.",
                      "Élargissez la série dans l'onglet Paramètres, sinon la",
                      "normale portera sur une base incomplète."),
                length(manquantes), c0$ref_label),
        type = "warning", duration = 12)
    }
  })
}

shiny::shinyApp(ui, server)
