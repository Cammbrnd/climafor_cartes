# ==============================================================================
# mod_parametres.R — ONGLET PARAMETRES
# ------------------------------------------------------------------------------
# Expose a l'utilisateur les seuils qui gouvernent les calculs. L'interface est
# generee automatiquement a partir de la table DESCRIPTIF_SEUILS ci-dessous :
# ajouter un seuil dans CONFIG$seuils et une ligne dans cette table suffit a le
# rendre reglable, sans ecrire une seule ligne d'UI supplementaire.
#
# CHOIX D'INTERFACE (v2)
# ----------------------
# 1. LA JUSTIFICATION DE CHAQUE SEUIL EST REPLIEE. C'est le contenu le plus
#    precieux de cet onglet — et le plus encombrant : quinze paragraphes gris
#    sous quinze champs numeriques faisaient de la page un mur de texte, et
#    l'utilisateur qui cherchait un seuil precis devait tout parcourir. Chaque
#    groupe expose desormais ses champs, et un depliant "Pourquoi ces valeurs"
#    donne l'integralite des justifications, mot pour mot.
#
# 2. LE VOLUME TELECHARGE PASSE EN TETE ET EST IDENTIFIE COMME TEL. C'est le
#    seul reglage de cet onglet qui agisse sur le temps de reponse : il ne se
#    confond plus avec les seuils bioclimatiques.
#
# 3. L'ACTION "RETABLIR" REMONTE PRES DES CHAMPS qu'elle reinitialise.
#
# 4. (v1.4) LES DIX-NEUF SEUILS SONT REPLIES. Ouvrir l'onglet montrait vingt et
#    un champs d'un coup (audit, S1), alors que la tres grande majorite des
#    analyses gardent les valeurs par defaut. Le bloc reste a un clic, ET une
#    ligne d'etat dit ce qu'il contient : « valeurs par defaut » ou la liste des
#    seuils modifies. Aucun champ n'est retire : les valeurs sont transmises au
#    serveur, replie ou non. Le bloc s'ouvre de lui-meme quand un lien partage
#    a modifie des seuils.
# ==============================================================================

# id        : nom du seuil dans CONFIG$seuils
# libelle   : intitule affiche
# groupe    : regroupement dans l'interface
# min/max/pas : bornes du champ numerique
# aide      : justification du reglage, affichee sous le champ
# statut    : « convention » ou « indicatif » (voir src/references.R) — ce que
#             vaut le repere : usage courant, ou ordre de grandeur appuye par la
#             litterature citee. Aucun seuil n'est presente comme un resultat.
# refs      : identifiants de REFERENCES, separes par « ; » (vide si aucun)
DESCRIPTIF_SEUILS <- data.frame(
  id = c("gel_t", "gel_severe_t", "gel_extreme_t",
         "gel_tardif_t", "gel_tardif_gdd", "gel_tardif_fin",
         "chaleur_t1", "chaleur_t2", "chaleur_t3", "chaleur_t4", "vague_ecart", "vague_duree",
         "gdd_base", "gdd_plafond",
         "veg_seuil_t", "veg_n_jours",
         "sec_seuil_mm", "sec_duree_alerte",
         "swi_stress", "swi_severe", "swi_sature",
         "vent_fort_ms", "vent_percentile", "vpd_stress_kpa"),
  libelle = c(
    "Seuil de gel (°C)", "Seuil de gel fort (°C)", "Seuil de gel très fort (°C)",
    "Seuil de gel tardif (°C)",
    "Débourrement estimé — degrés-jours depuis le 1er janvier",
    "Fin de la saison de gel tardif (jour julien)",
    "Seuil 1 de chaleur (°C)", "Seuil 2 de chaleur (°C)", "Seuil 3 de chaleur (°C)",
    "Seuil 4 de chaleur (°C)",
    "Vague de chaleur — écart à la normale du jour (°C)",
    "Vague de chaleur — jours consécutifs requis",
    "Température de base des degrés-jours (°C)", "Plafond journalier des degrés-jours (°C)",
    "Seuil thermique de végétation (°C)", "Jours consécutifs requis",
    "Seuil de journée sèche (mm)", "Durée d'alerte sécheresse (jours)",
    "SWI de contrainte", "SWI de stress sévère", "SWI de saturation du sol",
    "Vent fort — seuil absolu (m/s)", "Vent soutenu — percentile de référence",
    "Seuil de VPD élevé (kPa, moyenne journalière)"
  ),
  groupe = c(rep("Gel", 6), rep("Chaleur", 6), rep("Degrés-jours", 2),
             rep("Saison de végétation", 2), rep("Sécheresse", 2), rep("Sol (SWI)", 3),
             rep("Vent", 2), "Air (VPD)"),
  min = c(-10, -20, -30, -10, 0, 1, 15, 20, 25, 30, 1, 3, 0, 20, 0, 2, 0, 3, 0, 0, 0.5, 3, 0.50, 0.5),
  max = c(5, 0, 0, 5, 600, 250, 35, 40, 45, 50, 10, 15, 15, 40, 15, 15, 5, 60, 1, 1, 1, 25, 0.999, 4),
  pas = c(0.5, 0.5, 0.5, 0.5, 10, 1, 1, 1, 1, 1, 0.5, 1, 0.5, 1, 0.5, 1, 0.1, 1, 0.05, 0.05, 0.05, 0.5, 0.01, 0.1),
  aide = c(
    "Un jour de gel est compté lorsque la température minimale passe sous ce seuil (0 °C : indice FD).",
    "Repère des gels destructeurs pour les jeunes plants non aoûtés.",
    "Second repère de gel intense, plus rare : compté en plus des deux précédents.",
    "Un jour de gel tardif est un jour de gel survenant après le débourrement estimé. 0 °C compte l'exposition au gel, pas le dégât : les seuils de dommage des feuilles dépliées varient de -1 à -7,5 °C selon l'essence (Bigler et Bugmann 2018). Mettre -2 ou -3 °C pour approcher le dégât.",
    "200 : somme de degrés-jours (base et plafond des degrés-jours, ci-dessous) depuis le 1er janvier au-delà de laquelle le débourrement est estimé atteint. Valeur par défaut indicative, non spécifique d'une essence : à caler sur les dates de débourrement observées (plus petit = débourrement plus précoce, donc plus de gel tardif).",
    "181 = 30 juin. À reculer en altitude : le débourrement y est retardé d'environ 2 à 3,5 jours par 100 m, et le risque de gel s'y prolonge.",
    "25 °C : jour d'été (indice SU ; Météo-France retient aussi 25 °C).",
    "30 °C : seuil de forte chaleur.",
    "35 °C : repère de chaleur extrême. L'assimilation nette des essences tempérées y diminue le plus souvent (optimum général entre 20 et 30 °C), sans seuil unique.",
    "40 °C : chaleur très extrême, rarement atteinte en France. Repère d'usage.",
    "5 °C au-dessus de la normale du jour (la moyenne de la température maximale de ce jour sur la période de référence) : écart de l'indice HWDI. Seuil relatif : il suit la saison, contrairement aux trois seuils précédents.",
    "5 jours : durée minimale de l'indice HWDI. Les vagues de chaleur ne sont cherchées que de mai à septembre.",
    "5 °C : base usuelle pour les ligneux tempérés. 8 à 10 °C pour les insectes xylophages.",
    "Plafonner évite de créditer d'un gain de croissance des journées caniculaires.",
    "5 °C : seuil usuel de la saison de végétation (l'indice international GSL retient 5 °C pendant 6 jours consécutifs).",
    "Nombre de jours consécutifs au-dessus (ou en dessous) du seuil.",
    "1 mm : seuil usuel d'un jour sans pluie (indice international CDD).",
    "Au-delà, une plantation de printemps non paillée est compromise.",
    "0,5 : repère de contrainte hydrique. La transpiration des peuplements se régule sous environ 0,4 d'eau extractible relative, grandeur voisine du SWI mais pas identique.",
    "0,3 : repère de stress sévère. Les attaques de scolytes sont liées à des déficits hydriques marqués, mais aucun seuil de SWI n'est établi.",
    "0,9 : sol proche de la saturation. L'effet sur l'ancrage des racines dépend du sol et de l'essence.",
    "Seuil absolu, utile sur un point dont on connaît le régime de vent.",
    "Seuil relatif au point analysé : 0,95 compte les 5 % de journées les plus ventées de la référence. Plus robuste qu'un seuil absolu pour comparer deux stations.",
    "1,5 kPa : repère d'air très sec, appliqué à un VPD MOYEN journalier (plus faible que celui de l'après-midi). La fermeture des stomates est progressive et propre à chaque essence."
  ),
  statut = c(
    "convention", "convention", "convention",
    "indicatif", "convention", "convention",
    "convention", "convention", "indicatif", "convention", "convention", "convention",
    "convention", "convention",
    "convention", "convention",
    "convention", "convention",
    "indicatif", "convention", "convention",
    "convention", "convention", "convention"),
  refs = c(
    "zhang2011", "", "",
    "bigler2018;lenz2013;vitasse2018gel", "delpierre2016", "vitasse2018",
    "zhang2011", "", "sage2007", "", "frich2002", "frich2002;zhang2011",
    "", "",
    "zhang2011", "zhang2011",
    "zhang2011", "",
    "granier1999;breda2006", "netherer2015;breda2006", "gardiner2016",
    "", "", "grossiord2020"),
  stringsAsFactors = FALSE
)


#' @param etat etat decode d'un lien partage (partage_decoder), ou NULL : fixe
#'   les valeurs de depart de la plage telechargee et des seuils.
mod_parametres_ui <- function(id, etat = NULL) {
  ns <- shiny::NS(id)
  groupes <- unique(DESCRIPTIF_SEUILS$groupe)
  annee_max <- as.integer(format(Sys.Date(), "%Y"))
  charge <- if (is.null(etat)) c(1970L, annee_max) else etat$annees_charge
  valeur_seuil <- function(id) etat$seuils[[id]] %||% CONFIG$seuils[[id]]

  htmltools::tagList(

    shiny::h2(class = "cf-titre-section", "Paramètres"),
    shiny::p(class = "cf-chapeau",
             paste("Les valeurs par défaut correspondent aux conventions les plus",
                   "répandues en climatologie forestière. Ajustez-les pour coller à",
                   "votre contexte stationnel : tous les graphiques et le rapport",
                   "sont recalculés immédiatement.")),

    # -- Volume de donnees telecharge ----------------------------------------
    # Seul reglage de cet onglet qui agisse sur le temps de reponse : il est
    # isole en tete pour ne pas se confondre avec les seuils bioclimatiques.
    shiny::div(
      class = "cf-carte",
      shiny::h3(class = "cf-carte__titre", "Étendue de la série téléchargée"),
      shiny::sliderInput(
        ns("annees_charge"),
        label = shiny::span(class = "cf-sr-only", "Années à télécharger : première et dernière"),
        min = CONFIG$annee_min, max = annee_max,
        value = charge,
        step = 1, sep = "", width = "100%"
      ),
      shiny::div(class = "cf-aide",
                 paste("Chaque année représente une requête à l'API. Une série de 60 ans",
                       "prend environ une minute au premier chargement, puis quelques",
                       "secondes grâce au cache. La plage doit couvrir la période de",
                       "référence choisie. L'Accueil annonce, avant de lancer l'analyse,",
                       "combien d'années restent à télécharger.")),
      shiny::checkboxInput(ns("cache"), "Utiliser le cache local", value = TRUE),
      # Le mode rapide est le defaut (v2.3) : il ne sert que si la base precalculee est joignable et
      # valable pour les reglages courants ; sinon serveur_dsf.R charge la serie complete et le dit.
      shiny::radioButtons(
        ns("mode_chargement"), "Mode de chargement",
        choices = c("Rapide : base précalculée et années récentes" = "rapide",
                    "Complet : toute la série, année par année (référence)" = "complet"),
        selected = "rapide", width = "100%"),
      shiny::div(class = "cf-aide", paste(
        "Le mode rapide lit dans le jeu publié les années déjà calculées pour la maille SAFRAN ou",
        "la station, et ne demande à SAFRAN (ou à Météo-France) que l'année en cours et l'année",
        "comparée. Il sert tant que la référence, la fenêtre de saison et les seuils sont ceux par",
        "défaut ; sinon, ou si le jeu publié n'est pas joignable, l'application charge la série",
        "complète et le dit. La synthèse chiffrée à période égale et le rapport demandent la série",
        "complète (un bouton la charge)."))
    ),

    # -- Seuils, generes automatiquement --------------------------------------
    shiny::div(
      class = "cf-carte__entete",
      shiny::h3(class = "cf-carte__titre", "Seuils bioclimatiques"),
      shiny::actionLink(ns("reinit"), "Rétablir les valeurs par défaut")
    ),
    # Ce que contient le bloc replie : valeurs par defaut, ou les seuils changes.
    shiny::uiOutput(ns("etat_seuils")),

    cf_repli(
      sprintf("Voir et modifier les %d seuils", nrow(DESCRIPTIF_SEUILS)),
      ouvert = length(etat$seuils) > 0L,

    # (v1.5, audit M6) Ce que vaut un seuil : aucun n'est un resultat etabli.
    shiny::div(
      class = "cf-aide cf-aide--statuts",
      shiny::strong("Aucun seuil n'est un résultat établi. "),
      shiny::strong(paste0(STATUTS_SEUIL[["convention"]], " : ")),
      STATUTS_SEUIL_AIDE[["convention"]], " ",
      shiny::strong(paste0(STATUTS_SEUIL[["indicatif"]], " : ")),
      STATUTS_SEUIL_AIDE[["indicatif"]], " Les références figurent dans Sources."),

    shiny::div(
      class = "cf-grille-parametres",
      lapply(groupes, function(g) {
        lignes <- DESCRIPTIF_SEUILS[DESCRIPTIF_SEUILS$groupe == g, , drop = FALSE]
        shiny::div(
          class = "cf-carte",
          shiny::h3(class = "cf-carte__titre", g),

          # Les champs d'abord, nus : c'est ce que l'on vient chercher.
          lapply(seq_len(nrow(lignes)), function(i) {
            shiny::div(
              class = "cf-champ",
              shiny::numericInput(
                ns(paste0("seuil_", lignes$id[i])), lignes$libelle[i],
                value = valeur_seuil(lignes$id[i]),
                min = lignes$min[i], max = lignes$max[i], step = lignes$pas[i],
                width = "100%"
              )
            )
          }),

          # La justification ensuite, repliee, integrale.
          shiny::tags$details(
            class = "cf-details",
            shiny::tags$summary("Pourquoi ces valeurs"),
            shiny::div(
              class = "cf-details__corps",
              shiny::tags$dl(
                class = "cf-liste-def",
                unlist(lapply(seq_len(nrow(lignes)), function(i) {
                  refs <- ref_courte(lignes$refs[i])
                  list(shiny::tags$dt(lignes$libelle[i]),
                       shiny::tags$dd(
                         lignes$aide[i],
                         shiny::tags$span(
                           class = paste0("cf-statut cf-statut--", lignes$statut[i]),
                           STATUTS_SEUIL[[lignes$statut[i]]]),
                         if (nzchar(refs)) shiny::tags$span(
                           class = "cf-refs", paste("Réf. :", refs))))
                }), recursive = FALSE)
              )
            )
          )
        )
      })
    ),

    shiny::div(
      class = "cf-bloc",
      shiny::actionButton(ns("reinit_bas"), "Rétablir les valeurs par défaut",
                          class = "cf-btn cf-btn--sec")
    )
    )
  )
}

#' Seuils courants a partir des valeurs recues des champs (audit v1.7, V8).
#'
#' Les attributs `min` et `max` d'un champ numerique ne sont qu'une indication
#' du navigateur : le serveur recoit ce que le client envoie, y compris une
#' valeur hors bornes ou d'un autre type (un client peut ecrire directement dans
#' les entrees Shiny). Une valeur absente, vide ou non numerique retombe sur la
#' valeur par defaut, comme avant ; une valeur numerique HORS BORNES retombe aussi
#' sur la valeur par defaut, mais est ALORS SIGNALEE (`refuses`) pour que
#' l'utilisateur sache que son choix n'a pas ete pris en compte. Memes bornes que
#' pour un lien partage (partage_lire_seuils).
#'
#' @param valeurs liste nommee (id du seuil -> valeur recue).
#' @param defauts seuils par defaut.
#' @param descriptif tableau des seuils (id, min, max).
#' @return liste(seuils, refuses) ; `refuses` : identifiants hors bornes.
#' @keywords internal
seuils_depuis_champs <- function(valeurs, defauts = CONFIG$seuils,
                                 descriptif = DESCRIPTIF_SEUILS) {
  s <- defauts
  refuses <- character(0)
  for (i in seq_len(nrow(descriptif))) {
    id <- descriptif$id[i]
    v <- valeurs[[id]]
    if (!is.numeric(v) || length(v) != 1L || !isTRUE(is.finite(v))) next
    if (v < descriptif$min[i] || v > descriptif$max[i]) {
      refuses <- c(refuses, id)
      next
    }
    s[[id]] <- as.numeric(v)
  }
  list(seuils = s, refuses = refuses)
}

#' @return liste de reactives :
#'   $seuils  -> liste de seuils courants
#'   $annees  -> plage d'annees a telecharger
mod_parametres_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reconstruction de la liste de seuils a partir des champs generes.
    # Toute valeur invalide retombe silencieusement sur la valeur par defaut :
    # l'application ne doit jamais casser sur une saisie incomplete.
    lecture_seuils <- shiny::reactive({
      champs <- stats::setNames(
        lapply(DESCRIPTIF_SEUILS$id, function(nom) input[[paste0("seuil_", nom)]]),
        DESCRIPTIF_SEUILS$id)
      seuils_depuis_champs(champs)
    })
    seuils <- shiny::reactive(lecture_seuils()$seuils)

    # -- Ligne d'etat du bloc replie ---------------------------------------------
    output$etat_seuils <- shiny::renderUI({
      m <- partage_seuils_modifies(seuils())
      ref <- lecture_seuils()$refuses
      avertissement <- if (length(ref)) {
        j <- match(ref, DESCRIPTIF_SEUILS$id)
        shiny::div(
          class = "cf-etat-reglage cf-etat-reglage--modifie", role = "status",
          shiny::strong("Valeur hors limites, non prise en compte : "),
          paste0(paste(sprintf("%s (de %s à %s)", DESCRIPTIF_SEUILS$libelle[j],
                               format(DESCRIPTIF_SEUILS$min[j]), format(DESCRIPTIF_SEUILS$max[j])),
                       collapse = " ; "),
                 ". La valeur par défaut est utilisée."))
      }
      if (!length(m)) {
        return(shiny::tagList(
          shiny::div(class = "cf-mention cf-etat-reglage",
                     "Valeurs par défaut (conventions usuelles en climatologie forestière)."),
          avertissement))
      }
      noms <- DESCRIPTIF_SEUILS$libelle[match(names(m), DESCRIPTIF_SEUILS$id)]
      shiny::tagList(
        shiny::div(
          class = "cf-etat-reglage cf-etat-reglage--modifie", role = "status",
          shiny::strong(sprintf("%d seuil%s modifié%s : ", length(m),
                                if (length(m) > 1) "s" else "", if (length(m) > 1) "s" else "")),
          paste(utils::head(noms, 4), collapse = " ; "),
          if (length(noms) > 4) sprintf(" ; … et %d autre(s).", length(noms) - 4L) else "."),
        avertissement)
    })

    # Deux points d'entree (lien en tete, bouton en pied), un seul traitement :
    # la remise a zero doit etre atteignable sans avoir a chercher.
    reinitialiser <- function() {
      for (i in seq_len(nrow(DESCRIPTIF_SEUILS))) {
        shiny::updateNumericInput(session, paste0("seuil_", DESCRIPTIF_SEUILS$id[i]),
                                  value = CONFIG$seuils[[DESCRIPTIF_SEUILS$id[i]]])
      }
      shiny::showNotification("Valeurs par défaut rétablies.", type = "message")
    }

    shiny::observeEvent(input$reinit,     reinitialiser())
    shiny::observeEvent(input$reinit_bas, reinitialiser())

    # Le cache est une option DE SESSION (audit, C2) : le choix est garde dans la
    # session, jamais dans CONFIG (partage par tout le processus). cache_est_actif()
    # le relit. Priorite haute : l'observateur passe avant les sorties de la meme
    # vague, qui lisent donc deja la nouvelle valeur.
    shiny::observe({
      session$userData$cache_actif <- isTRUE(input$cache)
    }, priority = 100)

    list(
      seuils = seuils,
      annees = shiny::reactive(input$annees_charge),
      mode   = shiny::reactive(input$mode_chargement %||% "rapide"),
      # Reactif : les phrases qui annoncent « X annees en cache » en dependent.
      cache  = shiny::reactive(isTRUE(input$cache))
    )
  })
}
