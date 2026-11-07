# ==============================================================================
# mod_sources.R — ONGLET SOURCES ET EXPLICATIONS
# ------------------------------------------------------------------------------
# Onglet purement documentaire. Deux sections sont generees automatiquement a
# partir du registre (les fiches methode de chaque indicateur, et la liste des
# variables SAFRAN mobilisees) : elles restent donc exactes quand le registre
# evolue, sans maintenance manuelle.
# ==============================================================================

#' Carte « Position et confidentialite » (audit v1.7, V3).
#'
#' Elle DECRIT CE QUE FAIT LE CODE, rien de plus : quelles donnees quittent le
#' navigateur, vers qui, ce qui reste sur le serveur et combien de temps. Les
#' durees et les arrondis sont lus dans CONFIG : le texte ne peut pas diverger du
#' comportement. Elle ne remplace pas la declaration de l'organisme qui heberge
#' l'application (responsable de traitement, journaux d'acces, cookies de
#' l'hebergeur), et le dit.
#' @keywords internal
confidentialite_ui <- function() {
  # Onglet Carte : le texte depend de l'endroit d'ou le navigateur charge les
  # fichiers (reglage CONFIG$carte$url_donnees), il est donc compose ici.
  url_carte <- CONFIG$carte$url_donnees
  hote_carte <- if (is.character(url_carte) && length(url_carte) == 1L && nzchar(url_carte)) {
    sub("^https://([^/:]+).*$", "\\1", url_carte)
  } else ""
  texte_carte <- paste(
    if (nzchar(hote_carte)) {
      sprintf(paste(
        "L'onglet Carte charge, directement depuis votre navigateur, des fichiers de",
        "résultats précalculés hébergés sur %s : ce service voit l'adresse de votre",
        "appareil et les fichiers demandés, jamais votre position."), hote_carte)
    } else {
      paste("Les fichiers de résultats précalculés de l'onglet Carte sont servis par",
            "l'application elle-même : aucun service tiers n'est contacté pour eux.")
    },
    "Le fond de carte IGN est chargé comme indiqué ci-dessus. La bibliothèque",
    "cartographique (MapLibre) est fournie par l'application, sans service extérieur.",
    "Quand vous demandez « Analyser cette maille », l'application reçoit les",
    "coordonnées du centre de la maille, comme pour tout autre point choisi.")
  shiny::div(
    class = "cf-carte", id = "confidentialite",
    shiny::h3(class = "cf-carte__titre", "Position et confidentialité"),
    shiny::tags$dl(
      class = "cf-liste-def",

      shiny::tags$dt("Votre position"),
      shiny::tags$dd(paste(
        "« Utiliser ma position » ne fonctionne qu'avec l'accord de votre navigateur.",
        "La position est arrondie à 10 m environ dans votre navigateur avant d'être",
        "envoyée à l'application, qui s'en sert pour désigner le point d'analyse.",
        "L'application n'a ni compte ni identifiant d'utilisateur.")),

      shiny::tags$dt("Ce qui part vers des services tiers"),
      shiny::tags$dd(paste(
        "Le serveur de l'application interroge, avec les coordonnées du point :",
        "l'API Géo (geo.api.gouv.fr) pour trouver la commune, GeoSAS (api.geosas.fr)",
        "pour les données SAFRAN, data.gouv.fr pour le catalogue des fichiers de stations,",
        "et le stockage de Météo-France chez OVHcloud (meteofrance.s3.sbg.io.cloud.ovh.net)",
        "pour les fichiers eux-mêmes.",
        "Ces services voient l'adresse du serveur, pas celle de votre appareil.",
        "Les tuiles du fond de carte, elles, sont chargées directement par votre",
        "navigateur auprès de la Géoplateforme de l'IGN (data.geopf.fr) : ce service",
        "voit l'adresse de votre appareil et les zones de carte que vous affichez.")),

      shiny::tags$dt("Onglet Carte"),
      shiny::tags$dd(texte_carte),

      shiny::tags$dt("Ce qui reste sur le serveur"),
      shiny::tags$dd(sprintf(paste(
        "Pour accélérer les rechargements, les données déjà téléchargées sont gardées",
        "sur le disque du serveur. Les noms de ces fichiers contiennent les coordonnées",
        "du point arrondies à 100 m environ et l'année, sans identifiant de personne",
        "ni adresse. Ils sont supprimés après %d jours au plus, ou plus tôt si le",
        "dossier dépasse %d Mo. Le journal technique du serveur reçoit en outre les",
        "messages d'erreur de l'application ; sa durée de conservation dépend de",
        "l'hébergeur."),
        as.integer(CONFIG$cache$duree_j), as.integer(CONFIG$cache$taille_max_mo))),

      shiny::tags$dt("Lien de partage et rapports"),
      shiny::tags$dd(paste(
        "Le lien de partage contient les coordonnées du point, le nom du lieu et vos",
        "réglages : toute personne qui reçoit le lien les voit. Un rapport contient",
        "le point, le lieu et le texte du champ « contexte » que vous avez saisi ; il",
        "n'est gardé sur le serveur que jusqu'à la préparation suivante ou la fin de",
        "votre session.")),

      shiny::tags$dt("Limite de cette rubrique"),
      shiny::tags$dd(paste(
        "Cette rubrique décrit ce que fait le code de l'application. Elle ne remplace",
        "pas la déclaration de confidentialité de l'organisme qui l'héberge et",
        "l'exploite : responsable du traitement, journaux d'accès et cookies de",
        "l'hébergeur n'y figurent pas."))
    )
  )
}

mod_sources_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(

    shiny::h2(class = "cf-titre-section", "Sources et méthodes"),

    # -- Origine des donnees --------------------------------------------------
    shiny::div(
      class = "cf-carte",
      shiny::h3(class = "cf-carte__titre", "D'où viennent les données"),
      shiny::tags$dl(
        class = "cf-liste-def",
        shiny::tags$dt("Réanalyse SAFRAN-ISBA (SIM2), Météo-France"),
        shiny::tags$dd(paste(
          "Réanalyse météorologique quotidienne couvrant la France métropolitaine",
          "depuis 1958 sur une grille régulière de 8 km. SAFRAN reconstitue les",
          "paramètres atmosphériques en combinant observations de surface et",
          "analyses du modèle ARPEGE ; ISBA simule ensuite le bilan hydrique du",
          "sol, d'où proviennent l'ETP et le SWI.")),

        shiny::tags$dt("Service d'accès : API OGC EDR de GeoSAS (INRAE)"),
        shiny::tags$dd(shiny::HTML(paste0(
          "Interrogation ponctuelle du jeu SIM2 au format CoverageJSON. ",
          "Point d'entrée : <code>", CONFIG$api$edr_base, "</code>"))),

        shiny::tags$dt("Référentiel géographique : API Géo"),
        shiny::tags$dd(shiny::HTML(paste0(
          "Recherche de communes et géocodage inverse. ",
          "<code>", CONFIG$api$geo_communes, "</code>"))),

        shiny::tags$dt("Fond cartographique : Géoplateforme IGN"),
        shiny::tags$dd("Plan IGN v2, accessible sans clé depuis l'ouverture des flux en 2024.")
      )
    ),

    # -- Stations Meteo-France -------------------------------------------------
    shiny::div(
      class = "cf-carte",
      shiny::h3(class = "cf-carte__titre", "Stations Météo-France"),
      shiny::tags$dl(
        class = "cf-liste-def",
        shiny::tags$dt("Données climatologiques de base — quotidiennes"),
        shiny::tags$dd(shiny::HTML(paste0(
          "Observations des stations de Météo-France, publiées en données ouvertes ",
          "(licence Etalab 2.0, sans clé), un jeu de fichiers par département. Elles ",
          "sont mises à jour environ une fois par jour : c'est <strong>quasi quotidien, ",
          "pas du temps réel</strong>. Catalogue : <code>data.gouv.fr</code>, jeu ",
          "« Données climatologiques de base - quotidiennes »."))),

        shiny::tags$dt("Choix de la station"),
        shiny::tags$dd(sprintf(paste(
          "Les stations situées à moins de %d km du point sont classées selon la",
          "distance corrigée : chaque mètre d'écart d'altitude compte pour %.0f m",
          "de distance, et une station sans températures, sans pluie ou arrêtée",
          "est pénalisée. Les trois meilleures sont proposées, avec la grille",
          "SAFRAN en alternative."),
          CONFIG$stations$rayon_km, 1000 * CONFIG$stations$poids_alt_km_par_m)),

        shiny::tags$dt("Qualité d'une station (à la proposition)"),
        shiny::tags$dd(mf_aide_qualite()),

        shiny::tags$dt("Qualité de la série (après chargement)"),
        shiny::tags$dd(sprintf(paste(
          "Une année est utilisée si au moins %.0f %% de ses jours sont renseignés",
          "pour les variables de l'indicateur ; sinon elle est absente du graphique",
          "(jamais comptée comme zéro). La part des années de la période de référence",
          "ainsi couvertes donne la qualité de la série : bonne à partir de %.0f %%,",
          "correcte à partir de %.0f %%, limitée à partir de %.0f %%."),
          100 * CONFIG$stations$seuil_annee,
          100 * CONFIG$stations$seuils$annees_ref[1],
          100 * CONFIG$stations$seuils$annees_ref[2],
          100 * CONFIG$stations$seuils$annees_ref[3])),

        shiny::tags$dt("Ce qu'une station ne fournit pas"),
        shiny::tags$dd(paste(
          "Ni humidité du sol (SWI), ni hauteur de neige, ni humidité de l'air, et pas",
          "de séparation pluie / neige. Les indicateurs qui en dépendent ne sont pas",
          "calculés avec une station ; l'ETP est estimée par la formule de",
          "Hargreaves-Samani à partir des températures."))
      )
    ),

    # -- Mesure et modele (v2.1) ----------------------------------------------
    shiny::div(
      class = "cf-carte",
      shiny::h3(class = "cf-carte__titre", "Mesure et modèle, côte à côte"),
      shiny::tags$dl(
        class = "cf-liste-def",
        shiny::tags$dt("La station est une mesure, la maille un modèle"),
        shiny::tags$dd(paste(
          "Une station Météo-France décrit son emplacement : altitude, exposition, abri. La maille SAFRAN de 8 km",
          "est un modèle alimenté par des observations : elle lisse le relief. Les deux ne disent pas exactement",
          "la même chose, et leur écart n'est pas une erreur de l'une ou de l'autre. Les voir côte à côte",
          "(Synthèse d'un point, Autour de moi, Comparer) aide à juger ce que la maille dit de votre peuplement.")),

        shiny::tags$dt("Stations publiées avec la carte"),
        shiny::tags$dd(sprintf(paste(
          "Les indicateurs annuels de chaque station sont précalculés avec ceux des mailles, au même moment, et publiés",
          "avec elles : consulter une station ne télécharge aucune série journalière. N'y figurent que les stations",
          "qui ont au moins %d années valides de pluie ou de températures et une mesure de moins de %d jours.",
          "Les indicateurs qu'une station ne mesure pas (humidité du sol, neige, humidité de l'air) ne sont pas publiés ;",
          "pour une station, l'ETP est estimée (Hargreaves-Samani)."),
          CONFIG$stations$publie$annees_min, CONFIG$stations$publie$inactive_max_j)),

        shiny::tags$dt("Comparer deux lieux"),
        shiny::tags$dd(paste(
          "Deux mailles, deux stations, une maille et une station, ou des départements, dans toutes les combinaisons.",
          "Seules les années où les deux lieux ont une valeur sont comparées, et la normale de chacun est calculée",
          "sur ces années : comparer 1959-2025 à 1975-2025 reviendrait à comparer deux climats.")),

        shiny::tags$dt("Envoyer une synthèse par courriel"),
        shiny::tags$dd(paste(
          "L'écran Autour de moi prépare une synthèse courte. « Envoyer par courriel » ouvre votre messagerie sur un",
          "message déjà rédigé ; ClimaFor n'envoie lui-même aucun courriel, et ne vous demande aucune adresse.",
          "Le message contient le nom du lieu, jamais ses coordonnées. Un lien de courriel ne peut pas porter de pièce",
          "jointe : la page complète s'enregistre, ou se partage depuis l'appareil quand il le permet.")))
    ),

    # -- Variables mobilisees, generees depuis CONFIG -------------------------
    shiny::div(
      class = "cf-carte",
      shiny::h3(class = "cf-carte__titre", "Variables SAFRAN mobilisées (si la source est la grille)"),
      shiny::tableOutput(ns("variables")),
      shiny::div(
        class = "cf-alerte cf-alerte--attention",
        shiny::strong("Point de vigilance sur les précipitations. "),
        paste("SAFRAN distingue deux variables : PRELIQ_Q ne contient que la pluie,",
              "PRENEI_Q que la neige, exprimée en équivalent en eau. Le cumul de",
              "précipitations totales affiché par l'application est toujours la",
              "somme des deux. Retenir PRELIQ_Q seul sous-estimerait fortement les",
              "cumuls hivernaux et de montagne.")
      )
    ),

    # -- Glossaire (v1.4) : meme table que les infobulles des titres ------------
    glossaire_ui(),

    # -- Fiches methode, generees depuis le registre -------------------------
    shiny::div(
      class = "cf-carte",
      shiny::h3(class = "cf-carte__titre", "Fiches méthodologiques des indicateurs"),
      shiny::uiOutput(ns("fiches"))
    ),

    # -- Limites d'interpretation ---------------------------------------------
    shiny::div(
      class = "cf-carte",
      shiny::h3(class = "cf-carte__titre", "Ce que ces chiffres ne disent pas"),
      shiny::tags$ul(
        class = "cf-liste",
        shiny::tags$li(paste(
          "La maille de 8 km lisse le relief. En vallée encaissée, sur une crête ou",
          "en versant sud raide, les conditions réelles s'écartent sensiblement de",
          "la valeur de maille. Ces données décrivent un climat local, pas une",
          "station forestière.")),
        shiny::tags$li(paste(
          "SAFRAN est une réanalyse, pas une mesure. Sa validation sur la France",
          "(Quintana-Seguí et al. 2008) restitue bien la température et l'humidité,",
          "sans biais notable ; les précipitations n'ont pas de biais mais une erreur",
          "quadratique de 2,4 mm par jour environ, liée à l'hétérogénéité à l'intérieur",
          "de zones d'analyse d'environ 1 000 km² ; le vent est restitué avec un biais",
          "de -0,3 m/s ; le rayonnement est parfois biaisé, surtout sur les côtes.",
          "L'application n'affiche aucune incertitude de ce type : les intervalles",
          "tracés sur les tendances traduisent la variabilité d'une année sur l'autre,",
          "pas l'erreur de la réanalyse.")),
        shiny::tags$li(paste(
          "Les tendances sont des pentes de Sen (Sen 1968) testées par Mann-Kendall",
          "(Mann 1945 ; Kendall 1975), avec une correction de l'autocorrélation",
          "d'ordre 1 par un facteur d'inflation de variance — plus simple que la",
          "version de Hamed et Rao 1998, qui utilise plusieurs retards — sur vingt",
          "années complètes au moins. « Non significative » ne veut pas dire « absente »",
          "sur une série courte. Les ruptures d'homogénéité des stations sont repérées",
          "par le test de Pettitt, à titre indicatif (Pettitt 1979).")),
        shiny::tags$li(paste(
          "Le sol modélisé par ISBA est un sol standard. La réserve utile réelle",
          "de votre parcelle — profondeur, charge en cailloux, texture — module",
          "considérablement le stress hydrique effectif. Un sondage à la tarière",
          "reste indispensable.")),
        shiny::tags$li(paste(
          "Une station mesure en un point, pas sur votre parcelle : à plusieurs",
          "kilomètres et parfois plusieurs centaines de mètres d'altitude d'écart,",
          "le climat réel peut différer. La qualité affichée tient compte de ces",
          "écarts, mais ne les corrige pas.")),
        shiny::tags$li(paste(
          "L'ETP Penman-Monteith (SAFRAN) ou Hargreaves-Samani (station) est une",
          "demande évaporative de référence, calée",
          "sur un couvert herbacé. Un peuplement forestier adulte, plus rugueux et",
          "plus profondément enraciné, s'en écarte : les valeurs de P - ETP se",
          "lisent en relatif, entre années et entre stations, plus qu'en absolu.")),
        shiny::tags$li(paste(
          "L'année en cours est incomplète et SAFRAN accuse quelques jours de",
          "délai. Les comparaisons de l'année comparée à la normale sont faites à",
          "période égale, la normale étant tronquée à la même date.")),
        shiny::tags$li(paste(
          "Une normale sur 30 ans décrit un climat, pas une trajectoire. Pour",
          "anticiper, ces diagnostics se complètent des projections régionalisées",
          "(DRIAS, portail Climadiag Forêt de Météo-France)."))
      )
    ),

    # -- Corrections de methode et references (v1.5, audit M6) -----------------
    corrections_ui(),
    references_ui(),

    # -- Position et confidentialite (v1.7, audit V3) --------------------------
    confidentialite_ui(),

    # -- Mentions -------------------------------------------------------------
    shiny::div(
      class = "cf-carte",
      shiny::h3(class = "cf-carte__titre", "Mentions"),
      shiny::p(sprintf("%s version %s — %s.",
                       CONFIG$app$nom, CONFIG$app$version, CONFIG$app$organisme)),
      shiny::p(class = "cf-mention", paste(
        "Les données Météo-France (stations et SAFRAN) sont diffusées sous licence ouverte",
        "Etalab 2.0. Toute réutilisation des sorties de cette application doit",
        "mentionner cette source ainsi que la date d'extraction, qui figure sur",
        "chaque rapport."))
    )
  )
}

mod_sources_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # Table des variables : lue directement dans CONFIG, jamais recopiee
    output$variables <- shiny::renderTable({
      data.frame(
        `Nom interne`  = names(CONFIG$variables),
        `Variable SAFRAN` = vapply(CONFIG$variables, function(v) v$api, character(1)),
        `Description`  = vapply(CONFIG$variables, function(v) v$lib, character(1)),
        `Unité`        = vapply(CONFIG$variables, function(v) v$unite, character(1)),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }, striped = TRUE, width = "100%")

    # Fiches methodologiques : lues directement dans le registre
    output$fiches <- shiny::renderUI({
      shiny::tagList(lapply(indicateurs_ordonnes(), function(ind) {
        shiny::tags$details(
          class = "cf-details",
          shiny::tags$summary(ind$titre),
          shiny::div(
            class = "cf-details__corps",
            shiny::p(shiny::strong("Calcul. "), ind$methode),
            shiny::p(shiny::strong("Lecture. "), ind$lecture)
          )
        )
      }))
    })
  })
}
