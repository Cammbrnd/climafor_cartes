# ==============================================================================
# serveur_dsf.R — SERVEUR DE L'ANALYSE D'UN POINT (profil permanent DSF)
# ------------------------------------------------------------------------------
# Tout ce que l'application faisait jusqu'a la v1.9 sur le serveur : choisir la
# source (station ou grille SAFRAN), charger les donnees d'un point, produire le
# bilan climatique et le rapport, partager l'analyse par son adresse, comparer des
# lieux importes.
#
# Extrait d'app.R en v2.0 : l'application a maintenant trois profils (utils_profils.R),
# et seul le profil permanent a besoin de cette machinerie (recherche de stations,
# telechargements SAFRAN, rapport). Les profils « co » et « terrain » lisent les
# mailles precalculees de la carte (mod_synthese.R, mod_terrain.R) et n'appellent
# aucun service distant a la selection d'un point. Le code est le MEME : rien n'a
# change dans la logique, seulement son emplacement.
#
# @param input,output,session  ceux du serveur de l'application
# @param lieu            reactiveVal du point choisi (mod_localisation_server)
# @param aller_a         function(onglet) : change d'onglet
# @param mobile,tactile  reactives de largeur et de pointeur
# @param prefixe_adresse debut de l'adresse a conserver (« id=... & » ou « profil=... & »)
# @return function(point) a passer comme `ouvrir_maille` de mod_carte_server()
# ==============================================================================

serveur_dsf <- function(input, output, session, lieu, aller_a, mobile, tactile,
                        prefixe_adresse = "") {

  # -- Modules de réglage ----------------------------------------------------
  parametres <- mod_parametres_server("parametres")
  ctx        <- mod_periode_server("periode", seuils = parametres$seuils)

  # Source des données : station Météo-France proposée, ou grille SAFRAN.
  # `choix_initial` : source demandée par un lien partagé, consommée par la
  # première recherche de stations.
  choix_initial  <- shiny::reactiveVal(NULL)
  source_donnees <- mod_station_server("station", lieu, choix_initial)

  mod_comparer_server("comparer", ctx = ctx, lieu = lieu, mobile = mobile)  # v1.9 : points et zones

  # -- Navigation ------------------------------------------------------------
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

  # `complet` : charger toute la serie meme si le mode rapide est choisi (bouton du bilan).
  lancer_analyse <- function(complet = FALSE) {

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

    # Mode rapide (v2.2) : la base precalculee fournit les annees publiees, SAFRAN (ou Meteo-France,
    # pour une station) ne donne que le reste. Seulement si la base sert pour ces reglages, sinon
    # chargement complet, avec la raison dite.
    rap <- NULL
    if (!isTRUE(complet) && identical(parametres$mode(), "rapide")) {
      ouv <- if (en_station) base_ouvrir_station(src$station$id) else base_ouvrir(p$lon, p$lat)
      ap <- if (isTRUE(ouv$ok)) base_applicable(c0, ouv$jeu) else ouv
      if (isTRUE(ouv$ok) && isTRUE(ap$ok)) rap <- ouv
      else shiny::showNotification(
        paste("Mode rapide indisponible :", ap$message, "Chargement complet."),
        type = "warning", duration = 10)
    }
    if (!is.null(rap)) annees <- base_annees_a_charger(rap$jeu, annees, c0$courante)

    # Limites d'usage (audit V2). Le compteur du processus ne s'applique qu'aux
    # analyses qui iront chercher des donnees a distance : SAFRAN dont toutes les
    # annees ne sont pas en cache, ou une station (sa serie n'est pas predictible
    # sans lire les fichiers).
    distant <- en_station || !is.null(rap) || tryCatch(
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

    charger <- function(annees) tryCatch(
      if (en_station) {
        station_charger(src$station, p$lon, p$lat, annees, progres = progres)
      } else {
        safran_charger(p$lon, p$lat, annees, progres = progres)
      },
      error = function(e) {
        messages(paste("Erreur inattendue :", conditionMessage(e)))
        NULL
      })
    don <- charger(annees)

    # Station en mode rapide sans la moindre mesure des annees que le jeu ne porte pas (station
    # arretee depuis la publication) : rien a ajouter a la base, mais le bilan a besoin de jours.
    # On charge alors toute la serie plutot que d'echouer.
    if (!is.null(rap) && en_station && !is.null(don) && !nrow(don)) {
      shiny::showNotification(
        "Mode rapide : aucune mesure récente pour cette station. Chargement complet.",
        type = "warning", duration = 10)
      rap <- NULL
      don <- charger(annees_a_charger())
    }

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
    if (!is.null(rap)) {
      # Une station garde la qualite de TOUTES ses annees, pas des seules annees chargees.
      if (en_station) don <- base_station_attributs(don, rap)
      attr(don, "base") <- rap[c("jeu", "i", "note")]
    }
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
      shiny::updateQueryString(paste0("?", prefixe_adresse, partage_encoder(etat_partage)),
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
  # Depuis le bilan ou le rapport en mode rapide : recharger la serie entiere.
  shiny::observeEvent(input$charger_complet, lancer_analyse(complet = TRUE))
  shiny::observeEvent(input$charger_complet_rapport, lancer_analyse(complet = TRUE))

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
    etat_chargement_ui(don, p, src, perimee, est,
                       rapide = identical(parametres$mode(), "rapide") && !identical(src$type, "station"))
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
    if (length(manquantes) > 2 && is.null(attr(don, "station")) && !base_active(don)) {
      shiny::showNotification(
        sprintf(paste("%d année(s) de la référence %s ne sont pas chargées.",
                      "Élargissez la série dans l'onglet Paramètres, sinon la",
                      "normale portera sur une base incomplète."),
                length(manquantes), c0$ref_label),
        type = "warning", duration = 12)
    }
  })

  # -- Onglet Carte (v1.8) : un clic sur une maille ouvre l'analyse de son point --
  mod_carte_ouverture_server(lieu, choix_initial, source_donnees, function() lancer_analyse())
}
