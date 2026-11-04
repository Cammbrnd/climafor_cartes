# ==============================================================================
# mod_graphique.R — MODULE GENERIQUE D'AFFICHAGE D'UN INDICATEUR
# ------------------------------------------------------------------------------
# Une seule implementation sert tous les graphiques du registre, presents et a
# venir. Le module ne sait rien du contenu metier : il lit l'entree du registre
# qu'on lui passe et se contente de l'afficher.
#
# Chaque instance produit une carte comprenant :
#   - le titre de l'indicateur ;
#   - un selecteur d'annee, si et seulement si le registre le demande ;
#   - le graphique ;
#   - un commentaire automatique confrontant l'annee retenue a la normale ;
#   - un depliant « Méthode et lecture » ;
#   - les valeurs du graphique en tableau (alternative non visuelle) ;
#   - les exports PNG et CSV, chacun portant sa source, sa reference et sa date.
#
# ==============================================================================
# LE SELECTEUR D'ANNEE — CE QU'IL FAIT ET CE QU'IL COUTE
# ==============================================================================
# Les indicateurs portant `selecteur_annee = TRUE` dans le registre affichent un
# menu deroulant dans l'en-tete de leur carte. Choisir une annee ne modifie PAS
# le reglage global de l'application : la carte construit un CONTEXTE LOCAL,
# copie du contexte global dont le seul champ `courante` est remplace, et
# recalcule son indicateur avec.
#
# Pourquoi un contexte local plutot qu'un reglage global
# ------------------------------------------------------
# Parce que la question n'est pas la meme. Le reglage global de l'accueil dit
# « quelle annee ce bilan documente-t-il », et il doit rester le meme partout,
# jusque dans le rapport. Le selecteur d'une carte dit « et si je regardais
# 2003 sur celle-ci ? » : c'est une exploration, elle ne doit contaminer ni les
# autres graphiques, ni la synthese, ni le document produit. Deux besoins
# distincts, deux mecanismes distincts.
#
# Ce que cela coute
# -----------------
# mod_bilan calcule tous les indicateurs en une passe, avec le contexte global.
# Une carte munie d'un selecteur ne peut pas se servir de ce resultat des lors
# que l'annee retenue differe : elle rappelle donc `ind$calc()` sur les donnees
# journalieres brutes. C'est un recalcul par carte et par changement d'annee,
# sur une agregation de quelques dizaines de milliers de lignes — de l'ordre de
# la dizaine de millisecondes. Les cartes sans selecteur, elles, continuent de
# consommer le calcul unique : rien n'est perdu pour les douze autres.
#
# ==============================================================================
# RENDU INTERACTIF OU IMAGE
# ==============================================================================
# La carte affiche un graphique plotly quand DEUX conditions sont reunies :
# CONFIG$graphiques$interactif vaut TRUE, et l'entree du registre ne porte pas
# `interactif = FALSE`. Sinon elle affiche une image.
#
# v1.1 : le choix se fait desormais A L'EXECUTION, selon la largeur de l'ecran.
# Sous 900 px (telephone, tablette etroite) la carte affiche une IMAGE : plotly.js
# (plusieurs Mo) n'est alors jamais envoye au navigateur, et le doigt qui
# glisse sur un graphique fait defiler la page au lieu de le deplacer. Au-dela,
# plotly reste le defaut, sans deplacement ni zoom sur ecran tactile (cf.
# graphe_interactif). Les deux modes n'emettent pas la meme balise, plotlyOutput()
# d'un cote et plotOutput() de l'autre : la carte les echange via renderUI.
#
# Dans les deux cas, ind$plot() produit un ggplot. L'export PNG s'appuie
# toujours sur lui : il reste donc identique, interactif ou non — et disponible
# meme lorsque la conversion plotly echoue.
#
# LE SOUS-TITRE EST DEPLACE, PAS PERDU
# ------------------------------------
# ggplotly ne conserve pas les sous-titres. Or ils portent des informations sans
# lesquelles plusieurs graphiques se lisent de travers : seuil de vent retenu,
# signification du rouge et du bleu, fenetre mensuelle du P - ETP. Le module les
# recupere sur l'objet ggplot et les affiche sous le graphique.
#
# L'EXPORT SUIT LE SELECTEUR
# --------------------------
# PNG et CSV exportent ce qui est A L'ECRAN, donc l'annee choisie dans la carte,
# et le nom de fichier la mentionne. Exporter l'annee globale alors qu'une autre
# est affichee serait un piege silencieux.
# ==============================================================================

mod_graphique_ui <- function(id, ind) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "cf-carte", id = paste0("chart-", ind$id),

    shiny::div(
      class = "cf-carte__entete",
      # Les termes du glossaire (P - ETP, SWI, VPD, GDD...) portent leur
      # definition en infobulle : voir cf_terme() dans src/glossaire.R.
      shiny::h3(class = "cf-carte__titre", cf_terme(ind$titre)),

      # Le selecteur est rendu cote serveur : la liste des annees disponibles
      # depend des donnees chargees, que l'interface ne connait pas encore au
      # moment ou elle est construite.
      if (isTRUE(ind$selecteur_annee)) shiny::uiOutput(ns("selecteur")),

      shiny::div(
        class = "cf-carte__actions",
        shiny::downloadLink(
          ns("png"), shiny::tagList(cf_icone("image"), "Image PNG"),
          class = "cf-lien-outil",
          title = sprintf("Télécharger le graphique « %s » en PNG", ind$titre)),
        shiny::downloadLink(
          ns("csv"), shiny::tagList(cf_icone("tableau"), "Données CSV"),
          class = "cf-lien-outil",
          title = sprintf("Télécharger les données de « %s » en CSV", ind$titre))
      )
    ),

    # Le rendu (plotly ou image) est choisi COTE SERVEUR, selon la largeur de
    # l'ecran : voir la note « RENDU INTERACTIF OU IMAGE » en tete de fichier.
    shiny::div(class = "cf-carte__graphe", shiny::uiOutput(ns("zone_graphe"))),

    # Detail technique d'un echec (calcul ou trace), replie. Vide quand tout va bien.
    shiny::uiOutput(ns("erreur")),

    # Sous-titre et notes du ggplot, reaffiches ici : ggplotly ne conserve ni
    # l'un ni les autres. Les notes disent ce qui change la lecture : annee
    # incomplete, jours sans mesure, annee sans valeur, source de l'ETP, tendance.
    shiny::uiOutput(ns("sous_titre")),

    shiny::uiOutput(ns("commentaire")),

    # Les valeurs du graphique, en tableau : seule alternative non visuelle a un
    # graphique plotly, et verification a l'oeil des chiffres tracees.
    shiny::uiOutput(ns("tableau")),

    shiny::tags$details(
      class = "cf-details",
      shiny::tags$summary("Méthode et lecture"),
      shiny::div(
        class = "cf-details__corps",
        shiny::p(shiny::strong("Comment c'est calculé. "), ind$methode),
        shiny::p(shiny::strong("Comment le lire. "), ind$lecture)
      )
    )
  )
}

#' @param id Identifiant du module (doit etre unique : on utilise ind$id).
#' @param ind Entree du registre INDICATEURS.
#' @param donnees reactive() -> data.frame deja calcule pour cet indicateur,
#'   avec le contexte global. Utilise tel quel si la carte n'a pas de selecteur.
#' @param ctx reactive() -> contexte de calcul global.
#' @param lieu reactive() -> liste du point courant (sert a nommer les exports).
#' @param mobile reactive() -> TRUE si affichage etroit.
#' @param brut reactive() -> tableau journalier SAFRAN. Indispensable aux seules
#'   cartes munies d'un selecteur d'annee, qui recalculent elles-memes.
mod_graphique_server <- function(id, ind, donnees, ctx, lieu,
                                 mobile = shiny::reactive(FALSE),
                                 brut = shiny::reactive(NULL),
                                 tactile = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    avec_selecteur <- isTRUE(ind$selecteur_annee)

    # -- Selecteur d'annee ----------------------------------------------------
    output$selecteur <- shiny::renderUI({
      if (!avec_selecteur) return(NULL)
      d <- brut()
      shiny::req(d, nrow(d) > 0)

      annees <- sort(unique(d$annee), decreasing = TRUE)

      # La valeur par defaut est l'annee comparee du reglage global — donc
      # l'annee en cours tant que l'utilisateur n'en a pas choisi une autre.
      # `isolate` est delibere : sans lui, toute modification du contexte
      # (periode de reference, fenetre) reconstruirait le menu et effacerait le
      # choix de l'utilisateur.
      defaut <- shiny::isolate(ctx()$courante)
      if (!defaut %in% annees) defaut <- max(annees)

      # Etiquette reliee au champ (for) : « Année » a l'ecran, mais le nom
      # accessible precise le graphique, sinon six listes « Année » se
      # ressemblent au lecteur d'ecran. <select> natif : relie a son etiquette,
      # navigable a la frappe, et pas de selectize a initialiser six fois.
      shiny::div(
        class = "cf-carte__reglage",
        shiny::tags$label(
          class = "cf-carte__reglage-lib", `for` = ns("annee"),
          "Année",
          shiny::span(class = "cf-sr-only",
                      sprintf(" comparée du graphique « %s »", ind$titre))),
        shiny::selectInput(ns("annee"), label = NULL, choices = annees,
                           selected = defaut, width = "110px", selectize = FALSE)
      )
    })

    # -- Contexte local -------------------------------------------------------
    # Copie du contexte global, dont seule l'annee comparee est remplacee.
    ctx_local <- shiny::reactive({
      c0 <- ctx()
      if (!avec_selecteur) return(c0)
      # `input$annee` vaut NULL tant que le menu n'a pas ete rendu — c'est-a-dire
      # a chaque premier affichage de la carte. as.integer(NULL) renvoie un
      # vecteur VIDE, et `if (is.finite(integer(0)))` leve une erreur. D'ou le
      # test de longueur, qui n'est pas une precaution de style.
      a <- suppressWarnings(as.integer(input$annee))
      if (length(a) == 1L && is.finite(a)) c0$courante <- a
      c0
    })

    # -- Donnees affichees ----------------------------------------------------
    donnees_local <- shiny::reactive({
      if (!avec_selecteur) return(donnees())
      d <- brut()
      shiny::req(d, nrow(d) > 0)
      # Meme chemin que le calcul global : une variable absente, une source sans
      # annee valide ou une erreur de calcul recoivent chacune leur message.
      calculer_un_indicateur(d, ind, ctx_local())
    })

    # Colonnes exportees et affichees dans le tableau : celles du registre, plus
    # les indicateurs de completude lorsqu'ils existent.
    colonnes_exportees <- function(d) {
      cols <- if (!is.null(ind$colonnes)) intersect(ind$colonnes, names(d)) else names(d)
      cols <- union(cols, intersect(c("partielle", "n_manquants"), names(d)))
      if (!length(cols)) names(d) else cols
    }

    # -- Construction du ggplot ----------------------------------------------
    # Isolee : un indicateur en echec affiche un message explicite au lieu de
    # faire tomber la page entiere.
    # Le message d'erreur R brut (« objet 'x' introuvable »...) n'apprend rien a
    # l'utilisateur et expose des noms internes : il va dans le journal, pas a
    # l'ecran. Un signal reactif (req) n'est pas une erreur et passe tel quel.
    graphe_brut <- shiny::reactive({
      tryCatch(
        ind$plot(donnees_local(), ctx_local(), mobile = mobile()),
        shiny.silent.error = function(e) stop(e),
        error = function(e) {
          journaliser_erreur(sprintf("Graphique '%s'", ind$id), e)
          g <- graphe_message(paste(
            "Ce graphique n'a pas pu être construit.",
            "Les autres indicateurs ne sont pas affectés ;",
            "l'erreur est consignée dans le journal de l'application.", sep = "\n"))
          # Le message d'origine accompagne le graphique de remplacement : la
          # carte l'affiche dans un bloc replie (audit, S5).
          attr(g, "erreur_detail") <- conditionMessage(e)
          g
        }
      )
    })

    # -- Detail technique d'un echec -------------------------------------------
    # Deux sources possibles : le calcul de l'indicateur (attribut "detail" du
    # resultat vide) ou la construction du graphique (attribut "erreur_detail").
    # Un signal reactif (req) n'est pas une erreur : il ne produit rien ici.
    output$erreur <- shiny::renderUI({
      d <- tryCatch(donnees_local(), error = function(e) NULL)
      detail <- if (identical(attr(d, "raison"), "erreur")) attr(d, "detail") else NULL
      if (is.null(detail)) {
        g <- tryCatch(graphe_brut(), error = function(e) NULL)
        detail <- attr(g, "erreur_detail")
      }
      if (is.null(detail)) return(NULL)
      cf_erreur_detail(ind$id, detail)
    })

    # Affichage en image : sous-titre et notes sont replies (sinon ils sont
    # coupes a droite). L'export PNG utilise le graphique non replie.
    graphe <- shiny::reactive({
      g <- graphe_brut()
      etroit <- isTRUE(mobile())
      # Mise en forme seulement : si le repli echoue, on garde le graphique tel
      # quel, mais on le consigne (audit, C3).
      g <- tryCatch(graphe_sous_titre_replie(g, largeur = if (etroit) 50 else 115),
                    error = function(e) {
                      journaliser_erreur(sprintf("Sous-titre de '%s'", ind$id), e); g })
      tryCatch(graphe_notes_repliees(g, largeur = if (etroit) 52 else 115),
               error = function(e) {
                 journaliser_erreur(sprintf("Notes de '%s'", ind$id), e); g })
    })

    # -- Commentaire et texte alternatif -------------------------------------
    commentaire_txt <- shiny::reactive({
      commentaire_indicateur(ind, donnees_local(), ctx_local())
    })

    # Description lue par un lecteur d'ecran : le titre, la phrase de lecture, et
    # un renvoi vers les valeurs. Avant, l'image s'annoncait « Plot object ».
    texte_alt <- shiny::reactive({
      resume <- tryCatch(commentaire_txt(), error = function(e) "")
      resume <- if (nzchar(resume)) paste0(" ", substr(resume, 1L, 500L)) else ""
      sprintf("Graphique : %s.%s Les valeurs sont données dans le tableau sous le graphique.",
              ind$titre, resume)
    })

    # Interactif seulement si l'application et l'indicateur le permettent ET
    # si l'ecran est assez large. Sur telephone, une image est plus legere
    # (plotly.js pese plusieurs Mo) et ne vole pas le defilement au doigt.
    interactif_effectif <- shiny::reactive({
      graphe_est_interactif(ind) && !isTRUE(mobile())
    })

    output$zone_graphe <- shiny::renderUI({
      h <- if (isTRUE(mobile())) CONFIG$graphiques$hauteur_mobile
           else CONFIG$graphiques$hauteur_web
      if (interactif_effectif()) {
        # role="img" + aria-label : le graphique plotly est un SVG sans
        # equivalent textuel ; le tableau des valeurs en tient lieu.
        shiny::div(
          role = "img", `aria-label` = texte_alt(),
          plotly::plotlyOutput(ns("graphe_plotly"), height = paste0(h, "px")))
      } else {
        shiny::plotOutput(ns("graphe"), height = paste0(h, "px"))
      }
    })

    output$graphe <- shiny::renderPlot({ graphe() }, res = 96,
                                       alt = shiny::reactive(texte_alt()))

    output$graphe_plotly <- plotly::renderPlotly({
      graphe_interactif(graphe(), mobile = mobile(), tactile = isTRUE(tactile()))
    })

    # En image, sous-titre et notes sont dessines dans le graphique lui-meme ;
    # en plotly, ils sont reaffiches ici.
    output$sous_titre <- shiny::renderUI({
      if (!interactif_effectif()) return(NULL)
      g <- graphe_brut()
      st <- graphe_sous_titre(g)
      notes <- graphe_notes(g)
      if (is.null(st) && !length(notes)) return(NULL)
      htmltools::tagList(
        if (!is.null(st)) shiny::div(class = "cf-aide cf-sous-titre", st),
        if (length(notes))
          shiny::tags$ul(class = "cf-notes", lapply(notes, shiny::tags$li)))
    })

    output$commentaire <- shiny::renderUI({
      txt <- commentaire_txt()
      # Un commentaire qui ECHOUE n'est pas un commentaire absent : on le dit
      # (audit, C3), avec le detail technique replie comme pour un graphique.
      if (!is.null(attr(txt, "erreur"))) {
        return(htmltools::tagList(
          shiny::div(class = "cf-aide",
                     "Le commentaire automatique n'a pas pu être produit pour cet",
                     "indicateur (erreur consignée dans le journal de l'application)."),
          cf_erreur_detail(ind$id, attr(txt, "erreur"))))
      }
      if (!nzchar(txt)) return(NULL)
      shiny::div(class = "cf-commentaire", txt)
    })

    # -- Tableau des valeurs --------------------------------------------------
    output$tableau <- shiny::renderUI({
      limite <- 400L
      d <- valeurs_formatees(donnees_local(), ind, ctx_local(), limite = limite)
      if (is.null(d)) return(NULL)

      shiny::tags$details(
        class = "cf-details",
        shiny::tags$summary("Valeurs du graphique (tableau)"),
        shiny::div(
          class = "cf-details__corps cf-tableau-defile",
          shiny::tags$table(
            class = "cf-tableau",
            shiny::tags$caption(class = "cf-sr-only",
                                sprintf("Valeurs de « %s »", ind$titre)),
            shiny::tags$thead(shiny::tags$tr(
              lapply(names(d), function(n) shiny::tags$th(scope = "col", n)))),
            shiny::tags$tbody(lapply(seq_len(nrow(d)), function(i)
              shiny::tags$tr(lapply(d, function(cl) shiny::tags$td(cl[i])))))),
          if (isTRUE(attr(d, "tronque")))
            shiny::div(class = "cf-aide",
                       sprintf("Les %d premières lignes seulement : le CSV contient tout.", limite)))
      )
    })

    # -- Exports --------------------------------------------------------------
    # Le nom de fichier porte l'annee affichee : deux exports d'un meme
    # graphique sur deux annees ne doivent pas se recouvrir dans le dossier de
    # telechargements.
    nom_fichier <- function(ext) {
      lieu_txt <- gsub("^_+|_+$", "", gsub("[^A-Za-z0-9]+", "_", (lieu() %||% list(nom = "point"))$nom))
      annee <- if (avec_selecteur) paste0("_", ctx_local()$courante) else ""
      sprintf("%s_%s%s_%s.%s", ind$id, lieu_txt, annee,
              format(Sys.Date(), "%Y%m%d"), ext)
    }

    output$png <- shiny::downloadHandler(
      filename = function() nom_fichier("png"),
      content = function(file) {
        # L'image circule seule : elle porte sa source, sa date et sa licence.
        p <- graphe_avec_source(graphe_brut(), ligne_source(brut()))
        p <- graphe_notes_repliees(graphe_sous_titre_replie(p, largeur = 135), largeur = 135)
        ggplot2::ggsave(
          file, plot = p,
          width = CONFIG$graphiques$largeur_export,
          height = CONFIG$graphiques$hauteur_export,
          dpi = CONFIG$graphiques$dpi_export, bg = "white"
        )
      }
    )

    output$csv <- shiny::downloadHandler(
      filename = function() nom_fichier("csv"),
      content = function(file) {
        dat <- donnees_local()
        d <- dat[, colonnes_exportees(dat), drop = FALSE]
        # Trois decimales : « 72,9690975582194 » n'a aucun sens pour une moyenne
        # de pluies, et alourdit le fichier.
        num <- vapply(d, is.double, logical(1))
        d[num] <- lapply(d[num], round, 3)
        # Separateur point-virgule, virgule decimale, UTF-8 avec BOM : ouverture
        # directe dans Excel en configuration francaise. Les lignes « # » en tete
        # donnent source, reference, seuils, date et licence : un CSV qui
        # circule seul ne perd plus son contexte.
        ecrire_csv_avec_metadonnees(
          d, file,
          lignes_metadonnees_csv(ind, ctx_local(), don = brut(), lieu = lieu(), dat = dat))
      }
    )

    invisible(NULL)
  })
}
