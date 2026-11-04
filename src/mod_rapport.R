# ==============================================================================
# mod_rapport.R — ONGLET RAPPORT
# ------------------------------------------------------------------------------
# Produit un document autoportant, destine a etre transmis tel quel. Le rapport
# reutilise les indicateurs deja calcules par mod_bilan : rien n'est recalcule,
# et le document ne peut pas diverger de ce qui est affiche a l'ecran.
#
# Le niveau de detail est adaptable, car les trois publics vises n'ont pas le
# meme besoin :
#   - decideur  : synthese, messages cles, pas de methode ;
#   - ingenieur : tous les graphiques et les commentaires ;
#   - technique : ajoute les fiches methodologiques et les tableaux de valeurs.
#
# CHOIX D'INTERFACE (v2)
# ----------------------
# 1. QUATRE REGLAGES, QUATRE CARTES NUMEROTEES. En v1, les quatre champs se
#    succedaient dans un unique bloc : rien ne disait ou finissait un choix et
#    ou commencait le suivant, et la liste des quatorze indicateurs ecrasait
#    visuellement les trois autres reglages.
#
# 2. LES INDICATEURS PASSENT EN LISTE CLIQUABLE MULTICOLONNE, avec deux
#    raccourcis "tout / aucun". Quatorze cases a cocher empilees occupaient
#    l'equivalent d'un ecran ; en colonnes, l'ensemble tient d'un regard, et la
#    cible tactile passe du texte a la ligne entiere.
#
# 3. L'APERCU DES MESSAGES CLES EST INTEGRE A LA DERNIERE ETAPE, juste avant
#    le bouton : on lit ce que le rapport va dire avant de decider de le
#    produire, et la sequence numerotee 1-2-3-4 reste ininterrompue.
#
# GENERATION EN DEUX TEMPS (v1.2)
# -------------------------------
# En v11, « Generer le rapport » etait un telechargement dont le contenu se
# fabriquait DANS le processus Shiny : de 6 a 16 secondes pendant lesquelles ce
# processus ne repondait a aucune autre session. Desormais :
#   1. « Preparer le rapport » lance la fabrication dans un processus R separe
#      (src/utils_rapport.R, rapport/generer_rapport.R) et rend la main ;
#   2. l'etat s'affiche (en cours, pret, echec) ;
#   3. quand le fichier est pret, « Telecharger » le sert tel quel.
# Si les reglages changent apres la preparation, le fichier n'est plus offert :
# il ne correspondrait plus a ce qui est affiche.
# Si le processus separe ne peut pas etre lance, la generation se fait dans le
# processus courant, comme avant.
# ==============================================================================

mod_rapport_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(

    shiny::h2(class = "cf-titre-section", "Rapport"),
    shiny::p(class = "cf-chapeau",
             paste("Le rapport reprend le point sélectionné, la période de référence",
                   "retenue et les indicateurs cochés, accompagnés de leur lecture",
                   "commentée. Il s'imprime et se transmet sans retouche.")),

    # -- 1. Public vise --------------------------------------------------------
    shiny::div(
      class = "cf-carte",
      cf_etape(1, "Pour qui ?", "détermine le niveau de détail"),
      shiny::div(
        class = "cf-choix",
        shiny::radioButtons(
          ns("niveau"), label = NULL,
          choices = c(
            "Synthèse décideur — messages clés et graphiques essentiels" = "decideur",
            "Bilan ingénieur — tous les graphiques commentés"            = "ingenieur",
            "Dossier technique — avec méthodes et tableaux de valeurs"   = "technique"
          ),
          selected = CONFIG$rapport$niveau_defaut, width = "100%"
        )
      )
    ),

    # -- 2. Contenu ------------------------------------------------------------
    shiny::div(
      class = "cf-carte",
      shiny::div(
        class = "cf-carte__entete",
        cf_etape(2, "Quoi ?", "indicateurs inclus"),
        shiny::div(
          class = "cf-filtres__raccourcis",
          shiny::actionLink(ns("tous_indicateurs"), "Tout inclure"),
          shiny::actionLink(ns("aucun_indicateur"), "Tout retirer")
        )
      ),
      # (v1.4) Les cases sont repliees : tout inclure est le cas courant, et
      # trente et un champs d'un coup (audit, S1) noyaient les trois autres
      # etapes. La ligne d'etat dit combien sont retenues ; les raccourcis
      # « Tout inclure / Tout retirer » restent visibles.
      shiny::uiOutput(ns("etat_indicateurs")),
      cf_repli(
        "Choisir les indicateurs un par un",
        shiny::div(
          class = "cf-choix cf-choix--colonnes",
          shiny::checkboxGroupInput(
            ns("indicateurs"), label = NULL,
            choices  = stats::setNames(
              vapply(indicateurs_ordonnes(), function(i) i$id, character(1)),
              vapply(indicateurs_ordonnes(), function(i) i$titre, character(1))
            ),
            selected = ORDRE_INDICATEURS
          )
        )
      )
    ),

    # -- 3. Mise en contexte ---------------------------------------------------
    shiny::div(
      class = "cf-carte",
      cf_etape(3, "Dans quel cadre ?", "facultatif"),
      shiny::textAreaInput(
        ns("contexte"), label = NULL,
        placeholder = paste("Ex. : diagnostic préalable au renouvellement de la parcelle 14,",
                            "peuplement d'épicéa de 45 ans, exposition sud, 900 m."),
        width = "100%", rows = 3
      ),
      shiny::div(class = "cf-aide", sprintf(paste(
        "Ce texte figure en introduction du document, comme du texte simple :",
        "la mise en forme, les balises et les liens ne sont pas interprétés.",
        "%s caractères au maximum."),
        format(CONFIG$rapport$contexte_max, big.mark = " "))),
      shiny::uiOutput(ns("compteur_contexte"))
    ),

    # -- 4. Verification et generation ----------------------------------------
    # L'apercu des messages cles est INTEGRE a cette carte, et non place entre
    # deux etapes numerotees : une carte sans numero au milieu d'une sequence
    # 1-2-3-4 rompt le comptage, et l'utilisateur ne sait plus s'il lui reste
    # une etape ou deux. Il tombe ici a sa place logique : on lit ce que le
    # rapport va dire, puis on choisit le format, puis on genere.
    shiny::div(
      class = "cf-carte",
      cf_etape(4, "Vérifier et générer"),

      shiny::uiOutput(ns("apercu")),

      shiny::div(
        class = "cf-bloc",
        # L'etape ne s'appelle plus "Sous quel format ?" : ce choix a donc
        # besoin de son propre intitule, sans quoi deux boutons radio
        # apparaitraient nus sous l'apercu, sans dire de quoi ils traitent.
        # h3 et non h4 : sous le h2 « Rapport », un h4 sautait un niveau
        # (WCAG 1.3.1, regle heading-order). Meme aspect, via .cf-bloc__titre.
        shiny::h3(class = "cf-bloc__titre", "Format du document"),
        shiny::div(
          class = "cf-choix",
          shiny::radioButtons(
            ns("format"), label = NULL,
            choices = c("HTML (autonome, s'ouvre dans un navigateur)" = "html",
                        "Word (.docx, modifiable)" = "docx"),
            selected = "html", width = "100%"
          )
        )
      ),
      # L'option n'a de sens qu'en HTML : un widget n'est pas executable dans
      # un .docx, faute de navigateur pour le faire tourner.
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'html'", ns("format")),
        shiny::div(
          class = "cf-bloc",
          shiny::checkboxInput(
            ns("interactif"),
            "Graphiques interactifs (survol des valeurs, zoom)",
            value = TRUE),
          shiny::div(class = "cf-aide", paste(
            "Les graphiques interactifs embarquent une bibliothèque JavaScript",
            "et alourdissent le document d'environ 3,5 Mo. Décochez cette case",
            "pour un rapport léger, à transmettre par courriel ou à imprimer."))
        )
      ),

      shiny::div(
        class = "cf-bloc",
        shiny::actionButton(
          ns("preparer"), "Préparer le rapport",
          class = "cf-btn cf-btn--principal cf-btn--large"),
        shiny::uiOutput(ns("etat_generation"))
      ),
      shiny::div(class = "cf-aide", paste(
        "La préparation reprend les indicateurs déjà calculés : elle ne relance",
        "aucune requête vers l'API. Elle se fait en arrière-plan : vous pouvez",
        "continuer à consulter les graphiques. Comptez de quelques secondes à une",
        "demi-minute selon le niveau de détail."))
    )
  )
}

#' @param donnees reactive() -> tableau journalier.
#' @param resultats reactive() -> liste des indicateurs calcules (mod_bilan).
#' @param ctx reactive() -> contexte.
#' @param lieu reactive() -> point courant.
mod_rapport_server <- function(id, donnees, resultats, ctx, lieu) {
  shiny::moduleServer(id, function(input, output, session) {

    # -- Raccourcis de selection ---------------------------------------------
    tous_les_ids <- vapply(indicateurs_ordonnes(), function(i) i$id, character(1))

    # Indicateurs coches. Une liste dont toutes les cases sont decochees vaut NULL
    # cote Shiny : NULL veut dire « aucun », jamais « tous ». (Avant la v1.4,
    # apercu et preparation lisaient NULL comme « tous » : apres « Tout
    # retirer », le rapport contenait quand meme les vingt-quatre indicateurs.)
    # La liste est dans l'interface des le depart, donc liee des l'ouverture :
    # NULL ne peut pas signifier « pas encore lue ».
    indicateurs_choisis <- function() intersect(input$indicateurs, names(INDICATEURS))

    shiny::observeEvent(input$tous_indicateurs, {
      shiny::updateCheckboxGroupInput(session, "indicateurs", selected = tous_les_ids)
    })
    shiny::observeEvent(input$aucun_indicateur, {
      shiny::updateCheckboxGroupInput(session, "indicateurs", selected = character(0))
    })

    # -- Ligne d'etat de la liste repliee ------------------------------------------
    output$etat_indicateurs <- shiny::renderUI({
      n <- length(indicateurs_choisis())
      total <- length(tous_les_ids)
      shiny::div(
        class = "cf-mention cf-etat-reglage", role = "status",
        if (n == total) sprintf("Les %d indicateurs sont inclus.", total)
        else if (n == 0L) "Aucun indicateur inclus."
        else sprintf("%d indicateur%s sur %d inclus.", n, if (n > 1) "s" else "", total))
    })

    # -- Apercu des messages cles --------------------------------------------
    # Donne a l'utilisateur la substance du rapport avant de le generer.
    output$apercu <- shiny::renderUI({
      res <- resultats(); c <- ctx()
      shiny::req(res)
      ids <- indicateurs_choisis()

      textes <- lapply(ids, function(cle) {
        commentaire_indicateur(INDICATEURS[[cle]], resultat_indicateur(res, cle), c)
      })
      # Un commentaire qui echoue est nomme, pas passe sous silence (audit, C3).
      en_echec <- ids[vapply(textes, function(t) !is.null(attr(t, "erreur")), logical(1))]
      messages <- Filter(nzchar, vapply(textes, as.character, character(1)))

      if (!length(messages) && !length(en_echec)) return(NULL)
      shiny::div(
        class = "cf-bloc",
        shiny::h3(class = "cf-bloc__titre", "Aperçu des messages clés"),
        shiny::div(class = "cf-aide",
                   "Ce sont les phrases qui figureront dans le document généré."),
        shiny::tags$ul(class = "cf-liste", lapply(messages, shiny::tags$li)),
        if (length(en_echec)) shiny::div(
          class = "cf-alerte cf-alerte--attention", role = "status",
          sprintf("Commentaire non produit pour : %s (erreur consignée dans le journal).",
                  paste(vapply(en_echec, function(i) INDICATEURS[[i]]$titre, character(1)),
                        collapse=", ")))
      )
    })

    # -- Compteur du texte de contexte ------------------------------------------
    output$compteur_contexte <- shiny::renderUI({
      n <- nchar(input$contexte %||% "")
      if (n <= CONFIG$rapport$contexte_max) return(NULL)
      shiny::div(class = "cf-aide", sprintf(
        "%d caractères saisis : le texte sera tronqué à %d dans le document.",
        n, CONFIG$rapport$contexte_max))
    })

    # -- Generation en deux temps -------------------------------------------------
    # `signature` resume tout ce qui determine le contenu du document. Le fichier
    # prepare n'est offert que tant qu'elle n'a pas change.
    signature <- shiny::reactive({
      don <- donnees()
      list(niveau = input$niveau, format = input$format,
           interactif = isTRUE(input$interactif),
           indicateurs = sort(input$indicateurs %||% character(0)),
           contexte = input$contexte %||% "",
           ctx = ctx(), lieu = lieu(),
           nb = if (is.null(don)) 0L else nrow(don),
           fin = if (is.null(don) || !nrow(don)) NA else max(don$date))
    })

    tache <- shiny::reactiveVal(NULL)        # processus lance (ou NULL)
    etat  <- shiny::reactiveVal(list(statut = "aucun"))

    # Les dossiers de travail (plusieurs Mo) ne s'accumulent pas : celui de la
    # preparation precedente est supprime a chaque nouvelle, et a la fin de la
    # session.
    nettoyer <- function() {
      j <- shiny::isolate(tache())
      if (!is.null(j$dossier)) unlink(j$dossier, recursive = TRUE)
    }
    session$onSessionEnded(nettoyer)

    shiny::observeEvent(input$preparer, {
      don <- donnees()
      if (is.null(don) || !nrow(don) || is.null(resultats())) {
        shiny::showNotification("Lancez d'abord une analyse.", type = "warning")
        return()
      }
      if (!length(indicateurs_choisis())) {
        shiny::showNotification(
          "Retenez au moins un indicateur pour préparer le rapport.", type = "warning")
        return()
      }
      sig <- signature()
      entree <- list(
        donnees     = don,
        resultats   = resultats(),
        ctx         = ctx(),
        lieu        = lieu(),
        indicateurs = indicateurs_choisis(),
        niveau      = input$niveau,
        contexte    = contexte_borne(input$contexte),
        interactif  = isTRUE(input$interactif) && !identical(input$format, "docx")
      )
      format <- if (identical(input$format, "docx")) "docx" else "html"

      # Valide ici, pas dans le processus separe : une erreur de reglage se dit
      # tout de suite, sans attendre.
      ok_niveau <- tryCatch({ valider_niveau(entree$niveau); TRUE },
                            error = function(e) {
                              etat(list(statut = "erreur", message = conditionMessage(e)))
                              FALSE })
      if (!ok_niveau) return()

      # Limite par session (audit V2) : un rapport mobilise un processus entier.
      lim <- limite_controler("rapports", session)
      if (!lim$ok) {
        shiny::showNotification(lim$message, type = "warning", duration = 10)
        return()
      }

      nettoyer()
      tache(NULL)
      etat(list(statut = "en_cours", ecoule = 0))
      job <- lancer_rapport_arriere_plan(entree, format)

      if (is.null(job)) {
        # Repli : generation dans ce processus. Plus lente pour les autres
        # sessions, mais le rapport est produit.
        dossier <- tempfile("rapport_")
        res <- tryCatch(
          shiny::withProgress(message = "Génération du rapport", value = 0.3, {
            rendre_rapport(dossier, entree, format)
          }),
          error = function(e) e)
        if (inherits(res, "error")) {
          message("[ClimaFor] Rapport en échec : ", conditionMessage(res))
          etat(list(statut = "erreur", message = conditionMessage(res)))
        } else {
          tache(list(sig = sig, format = format, direct = TRUE, dossier = dossier))
          etat(list(statut = "ok", fichier = res))
        }
        return()
      }
      job$sig <- sig
      tache(job)
    })

    # Suivi du processus : une lecture par seconde, tant qu'il tourne.
    shiny::observe({
      job <- tache()
      if (is.null(job) || isTRUE(job$direct)) return()
      e <- etat_rapport(job)
      shiny::isolate(etat(e))
      if (identical(e$statut, "en_cours")) shiny::invalidateLater(1000, session)
      if (identical(e$statut, "erreur") && !is.null(job$journal) && file.exists(job$journal)) {
        message("[ClimaFor] Rapport en échec : ", e$message)
      }
    })

    output$etat_generation <- shiny::renderUI({
      e <- etat(); job <- tache()
      if (identical(e$statut, "en_cours")) {
        return(shiny::div(class = "cf-etat", role = "status",
                          shiny::span(class = "cf-etat__titre",
                                      sprintf("Préparation en cours… %d s", as.integer(e$ecoule %||% 0)))))
      }
      if (identical(e$statut, "erreur")) {
        return(shiny::div(
          class = "cf-alerte cf-alerte--erreur", role = "alert",
          shiny::span(class = "cf-alerte__titre", "Le rapport n'a pas pu être produit"),
          shiny::div(utils::head(e$message, 1)),
          shiny::div(class = "cf-aide",
                     "Modifiez un réglage ou relancez : les graphiques de l'onglet Bilan ne sont pas affectés.")))
      }
      if (identical(e$statut, "ok") && !is.null(job)) {
        if (!identical(job$sig, signature())) {
          return(shiny::div(class = "cf-alerte cf-alerte--info", role = "status",
                            "Les réglages ont changé depuis la préparation : préparez de nouveau le rapport."))
        }
        taille <- tryCatch(file.size(e$fichier), error = function(err) NA_real_)
        return(shiny::div(
          class = "cf-etat cf-etat--ok", role = "status",
          shiny::span(class = "cf-etat__titre", cf_icone("coche"),
                      sprintf("Rapport prêt%s", if (is.finite(taille))
                        sprintf(" (%s)", format_taille(taille)) else "")),
          shiny::downloadButton(
            session$ns("telecharger"),
            sprintf("Télécharger le rapport (%s)", if (identical(job$format, "docx")) "Word" else "HTML"),
            class = "cf-btn cf-btn--principal")))
      }
      NULL
    })

    output$telecharger <- shiny::downloadHandler(
      filename = function() {
        lieu_txt <- gsub("^_+|_+$", "", gsub("[^A-Za-z0-9]+", "_", (lieu() %||% list(nom = "point"))$nom))
        sprintf("bilan_climatique_%s_%s.%s", lieu_txt, format(Sys.Date(), "%Y%m%d"),
                if (identical(tache()$format, "docx")) "docx" else "html")
      },
      content = function(file) {
        e <- etat()
        shiny::req(identical(e$statut, "ok"), file.exists(e$fichier))
        file.copy(e$fichier, file, overwrite = TRUE)
      }
    )
  })
}
