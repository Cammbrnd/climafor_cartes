# ==============================================================================
# mod_bilan.R — MODULE "BILAN CLIMATIQUE"
# ------------------------------------------------------------------------------
# Assemble la page de resultats. Le point important : ce module ne cite AUCUN
# indicateur nommement. Il parcourt le registre et instancie autant de
# mod_graphique que necessaire. Ajouter un indicateur au registre le fait
# apparaitre ici sans toucher a ce fichier.
#
# CHOIX D'INTERFACE (v2)
# ----------------------
# 1. LES GRAPHIQUES SONT REGROUPES PAR THEMATIQUE, sous un titre de section.
#    Quatorze graphiques a la suite forment un mur : sans point de repere, on
#    ne sait plus ou l'on en est apres le sixieme. Les familles existent deja
#    dans le registre — elles ne servaient qu'a filtrer, elles structurent
#    desormais la page. Aucun indicateur n'est cite ici : le regroupement se
#    fait par parcours de FAMILLES.
#
# 2. LE FILTRE PASSE EN PASTILLES, avec deux raccourcis "tout / aucun". Une
#    ligne de six cases a cocher se lit comme un formulaire ; six pastilles se
#    lisent comme des interrupteurs, ce qu'elles sont.
#
# 3. (v3) SUR ECRAN ETROIT, UNE THEMATIQUE A LA FOIS. Sept familles
#    empilees font plus de dix ecrans de telephone. Sous 900 px, le filtre
#    devient une rangee de pastilles a choix unique : seule la famille retenue
#    est construite, ce qui allege aussi le navigateur. Sur grand ecran, le
#    filtre a choix multiples reste inchange.
#
# 4. (v3) QUALITE DE LA SERIE ET INDICATEURS ABSENTS. Quand la source est une
#    station Meteo-France, une carte dit dans quelle mesure la serie couvre la
#    reference choisie (mf_qualite_serie), et une note liste les indicateurs que
#    la station ne permet pas de calculer, avec la variable qui manque. Un
#    indicateur absent n'a pas de carte : une carte vide ne dit rien.
#
# 5. LE BANDEAU D'IDENTIFICATION EST RESERVE A L'IMPRESSION. A l'ecran il
#    repetait mot pour mot la barre de contexte affichee par app.R. Sur papier,
#    ou cette barre disparait, il redevient indispensable : la feuille doit
#    dire de quel point et de quelle reference elle parle.
#
# 6. (v1.4) PAR OU COMMENCER. Six a huit cartes de synthese puis jusqu'a vingt-
#    quatre graphiques sans consigne de lecture : le bloc « A retenir » nomme
#    les trois ecarts les plus marques de l'annee comparee (rapportes a
#    l'ecart-type de la reference) et renvoie a la section qui les detaille.
#    Les termes techniques des titres portent une infobulle ; le glossaire est
#    dans l'onglet Sources. Un indicateur dont le calcul a echoue est signale
#    en tete de page, son detail technique sur sa carte.
# ==============================================================================

mod_bilan_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(

    # -- Bandeau d'identification (visible a l'impression seulement) ---------
    shiny::uiOutput(ns("bandeau")),

    # -- Qualite de la serie (station) ---------------------------------------
    shiny::uiOutput(ns("qualite")),

    # -- Indicateurs dont le calcul a echoue ------------------------------------
    shiny::uiOutput(ns("erreurs_calcul")),

    # -- Synthese chiffree ----------------------------------------------------
    shiny::uiOutput(ns("synthese")),

    # -- Export du bilan (CSV) ---------------------------------------------------
    shiny::uiOutput(ns("export")),

    # -- Filtre par famille thematique ---------------------------------------
    shiny::uiOutput(ns("filtres")),

    # -- Graphiques, groupes par thematique -----------------------------------
    shiny::uiOutput(ns("graphiques")),

    # -- Indicateurs que la source ne permet pas de calculer -------------------
    shiny::uiOutput(ns("indisponibles"))
  )
}

# ------------------------------------------------------------------------------
# Synthese chiffree : constructeurs purs (audit, C7)
# ------------------------------------------------------------------------------
# Le serveur du module portait 130 lignes de construction de HTML dans un seul
# renderUI, impossibles a tester sans lancer Shiny. Elles sont ici, en fonctions
# qui ne dependent ni de reactifs ni de session : on leur donne des valeurs, elles
# rendent des balises ou des phrases.

#' Une carte de la synthese (indicateur, valeur, ecart a la normale).
#'
#' Couleur : seulement si l'ecart depasse la bande neutre (une demi ecart-type de
#' la reference) ET que l'indicateur a un cote defavorable. En deca, ou pour le
#' gel et la neige (aucun sens favorable), la carte reste grise : colorer un ecart
#' de 0,1 degC en rouge serait une affirmation que les donnees ne soutiennent pas.
#'
#' @param s tableau de calc_synthese() ; @param i numero de la ligne.
#' @keywords internal
synthese_carte <- function(s, i) {
  dec   <- suppressWarnings(as.integer(sub("^%\\.(\\d+)f$", "\\1", s$format[i])))
  if (is.na(dec)) dec <- 1L
  val   <- fmt_fr(s$valeur[i], dec)
  ecart <- s$ecart[i]

  juge <- switch(s$sens[i],
                 "sec"   = c(haut = "ok",     bas = "alerte"),
                 "chaud" = c(haut = "alerte", bas = "ok"),
                 NULL)
  sd_ok <- is.finite(s$sd_ref[i])
  # Un ecart qui s'affiche « 0,00 » ne se colore ni ne se qualifie : il est nul a
  # la precision de la carte.
  nul <- is.finite(ecart) && round(ecart, dec) == 0
  classe <- if (is.na(ecart) || is.null(juge) || !sd_ok || nul || s$classe[i] == "neutre")
    "cf-kpi--neutre" else paste0("cf-kpi--", juge[[s$classe[i]]])
  qualif <- if (is.na(ecart) || is.null(juge) || !sd_ok) ""
            else if (nul || s$classe[i] == "neutre") " · proche de la normale"
            else if (s$classe[i] == "haut") " · au-dessus" else " · en dessous"

  shiny::div(
    class = paste("cf-kpi", classe),
    shiny::div(class = "cf-kpi__libelle", cf_terme(s$indicateur[i])),
    shiny::div(class = "cf-kpi__valeur",
               paste(val, if (s$unite[i] == "-") "" else s$unite[i])),
    shiny::div(class = "cf-kpi__ecart",
               if (is.na(ecart)) "—" else
                 sprintf("%s vs normale%s", fmt_fr(ecart, dec, signe = TRUE), qualif))
  )
}

#' Ce qui nuance la lecture des cartes, en phrases : jours sans mesure (non
#' compenses), altitude du point (maille SAFRAN), gel tardif et altitude.
#'
#' @param s synthese ; @param don donnees (attribut « altitude_point »).
#' @param en_station source = station (la maille SAFRAN ne s'applique pas).
#' @param familles familles d'indicateurs disponibles ; @param seuils seuils en vigueur.
#' @return vecteur de phrases (eventuellement vide).
#' @keywords internal
synthese_notes <- function(s, don, en_station, familles, seuils) {
  manq <- attr(s, "manquants")
  note_manq <- if (!is.null(manq) && any(manq > 0)) {
    sprintf(paste("Jours sans mesure dans l'année comparée : %s. Ils ne sont pas",
                  "compensés : les cumuls et les comptages sont sous-estimés d'autant."),
            paste(c(if (manq[["temperature"]] > 0) sprintf("%d en température", manq[["temperature"]]),
                    if (manq[["pluie"]] > 0) sprintf("%d en précipitations", manq[["pluie"]])),
                  collapse = ", "))
  }
  alt_pt <- attr(don, "altitude_point")
  note_alt <- if (!en_station && !is.null(alt_pt) && is.finite(alt_pt)) {
    sprintf(paste("Le point est à %.0f m. La maille SAFRAN de 8 km en lisse le relief et",
                  "sa propre altitude n'est pas fournie par la source : en terrain accidenté,",
                  "les températures et les précipitations peuvent s'écarter de la maille."),
            alt_pt)
  }
  # Gel tardif et altitude (audit M7) : seulement si l'indicateur est present.
  note_gel <- if (!is.null(alt_pt) && "gel" %in% familles) {
    gel_tardif_conseil(alt_pt, seuils$gel_tardif_fin)
  }
  as.character(unlist(list(note_manq, note_alt, note_gel), use.names = FALSE))
}

#' Bloc « A retenir » : les ecarts les plus marques (audit, S1).
#'
#' @param retenir sortie de synthese_a_retenir() ; @param dispo familles
#'   disponibles (un lien vers les graphiques n'est offert que si la famille l'est).
#' @param ns fonction d'espace de noms du module.
#' @keywords internal
synthese_retenir_ui <- function(retenir, dispo, ns) {
  shiny::div(
    class = "cf-retenir",
    shiny::h3(class = "cf-retenir__titre", "À retenir"),
    if (nrow(retenir)) {
      shiny::tags$ul(
        class = "cf-retenir__liste",
        lapply(seq_len(nrow(retenir)), function(i) {
          f <- retenir$famille[i]
          nom <- retenir$indicateur[i]
          shiny::tags$li(
            shiny::strong(cf_terme(nom)),
            substring(retenir$texte[i], nchar(nom) + 1L),
            if (!is.na(f) && f %in% dispo)
              shiny::tags$a(
                href = paste0("#fam-", f), class = "cf-retenir__lien",
                `data-ouvrir-famille` = f, `data-entree` = ns("ouvrir_famille"),
                sprintf("Voir les graphiques (%s)", FAMILLES[[f]])))
        }))
    } else {
      shiny::p(sprintf(paste("Aucun écart ne dépasse %s écart-type de la référence :",
                             "l'année comparée reste dans la variabilité habituelle."),
                       format(CONFIG$tendance$bande_neutre, decimal.mark = ",")))
    },
    shiny::div(class = "cf-aide", paste(
      "Les écarts les plus marqués, rapportés à l'écart-type de la référence",
      "(une pluie en mm et des jours de gel ne se comparent pas autrement).",
      "Les autres indicateurs sont détaillés ci-dessous."))
  )
}

#' Synthese de l'annee comparee : « A retenir », cartes et notes de lecture.
#'
#' @param s synthese (calc_synthese) ; @param don donnees ; @param en_station,
#'   @param familles, @param seuils, @param ns voir synthese_notes et synthese_retenir_ui.
#' @keywords internal
synthese_ui <- function(s, don, en_station, familles, seuils, ns) {
  doy <- attr(s, "doy_max")
  notes <- synthese_notes(s, don, en_station, familles, seuils)
  shiny::div(
    shiny::h2(class = "cf-titre-section", "Synthèse de l'année comparée"),
    shiny::div(class = "cf-aide",
               sprintf(paste("Comparaison à période égale : l'année en cours est arrêtée",
                             "au %d%s jour, la normale est tronquée d'autant."),
                       doy, if (doy == 1) "er" else "e")),
    synthese_retenir_ui(synthese_a_retenir(s, n = 3L), familles, ns),
    shiny::div(class = "cf-kpis", lapply(seq_len(nrow(s)), synthese_carte, s = s)),
    shiny::tags$ul(
      class = "cf-notes",
      shiny::tags$li(sprintf(paste("Une carte n'est colorée que si l'écart dépasse %s écart-type",
                                   "des années de la référence ; en deçà, elle reste grise."),
                             format(CONFIG$tendance$bande_neutre, decimal.mark = ","))),
      lapply(notes, shiny::tags$li),
      shiny::tags$li(
        "Les termes soulignés en pointillé portent leur définition en infobulle ; ",
        "tous figurent dans le ",
        shiny::tags$a(href = "#glossaire", `data-onglet` = "sources", "glossaire"),
        "."))
  )
}

#' Indicateurs que la source ne permet pas de calculer, avec la raison.
#'
#' Seulement pour une station : SAFRAN renseigne tout, et un resultat vide y reste
#' un resultat vide (message du graphique), pas une absence de source.
#'
#' @param don donnees ; @param res sortie de calculer_indicateurs() ;
#'   @param en_station TRUE si la source est une station.
#' @return data.frame(id, titre, manque, famille), eventuellement vide.
#' @keywords internal
indicateurs_non_calculables <- function(don, res, en_station) {
  vide <- data.frame(id = character(0), titre = character(0), manque = character(0),
                     famille = character(0), stringsAsFactors = FALSE)
  if (!en_station) return(vide)
  m <- indicateurs_indisponibles(don)
  # Colonnes presentes mais aucune annee assez complete
  reste <- setdiff(names(INDICATEURS), m$id)
  # Un echec de calcul n'est pas un manque de donnees : il garde sa carte, ou
  # s'affiche son detail technique (audit, S5).
  sans_annee <- Filter(function(id) !NROW(res[[id]]) &&
                         !identical(attr(res[[id]], "raison"), "erreur"), reste)
  if (length(sans_annee)) {
    m <- rbind(m, data.frame(
      id = sans_annee,
      titre = vapply(INDICATEURS[sans_annee], function(i) i$titre, character(1)),
      manque = "aucune année assez complète (moins de 95 % des jours renseignés)",
      stringsAsFactors = FALSE))
  }
  if (!nrow(m)) return(vide)
  m$famille <- vapply(INDICATEURS[m$id], function(i) i$famille, character(1))
  rownames(m) <- NULL
  m
}

#' Carte « Qualite de la serie » d'une station.
#' @param q sortie de mf_qualite_serie() ; @param st station (liste) ;
#'   @param ref_label libelle de la reference.
#' @keywords internal
qualite_serie_ui <- function(q, st, ref_label) {
  detail <- sprintf("%s · %s du point · %s",
                    st$nom, mf_km(st$distance_km %||% NA_real_),
                    if (is.finite(st$alt)) sprintf("%.0f m", st$alt) else "altitude inconnue")
  alertes <- q$alertes
  shiny::div(
    class = paste0("cf-qualite cf-qualite--n", q$niveau),
    shiny::div(
      class = "cf-qualite__tete",
      mf_pastille(q$niveau, paste("Série de la station :", tolower(q$libelle))),
      shiny::span(class = "cf-qualite__station", detail)),
    if (length(alertes) <= 2L) {
      if (length(alertes))
        shiny::tags$ul(class = "cf-qualite__liste", lapply(alertes, shiny::tags$li))
      else shiny::div(class = "cf-aide", sprintf(
        "La station couvre la référence %s.", ref_label))
    } else {
      htmltools::tagList(
        shiny::tags$ul(class = "cf-qualite__liste",
                       lapply(utils::head(alertes, 2L), shiny::tags$li)),
        cf_repli(if (length(alertes) == 3L) "Voir l'autre remarque"
                 else sprintf("Voir les %d autres remarques", length(alertes) - 2L),
                 shiny::tags$ul(class = "cf-qualite__liste",
                                lapply(utils::tail(alertes, -2L), shiny::tags$li))))
    }
  )
}

#' Export CSV du bilan : le bouton et les deux fichiers.
#'
#' Sorti du serveur du Bilan, qui ne doit pas grossir (tests/test_v16.R). Les
#' sorties sont posees sur `output` du module appelant.
#' @param output,ns sorties et espace de noms du module ; @param donnees,resultats,ctx,lieu reactives.
#' @keywords internal
mod_bilan_export_server <- function(output, ns, donnees, resultats, ctx, lieu) {
  # -- Export CSV du bilan ------------------------------------------------------
  # Deux fichiers : la synthese chiffree (ce que montrent les cartes du haut) et
  # toutes les series annuelles en un seul tableau long. Les profils mensuels
  # gardent le CSV de leur graphique. Chaque fichier porte ses lignes de
  # provenance (source, reference, seuils, date, licence).
  output$export <- shiny::renderUI({
    shiny::req(donnees(), nrow(donnees()) > 0)
    shiny::div(
      class = "cf-export",
      shiny::span(class = "cf-export__legende", "Exporter ce bilan"),
      shiny::downloadButton(ns("csv_synthese"), "Synthèse chiffrée (CSV)",
                            class = "cf-btn cf-btn--sec"),
      shiny::downloadButton(ns("csv_series"), "Toutes les séries annuelles (CSV)",
                            class = "cf-btn cf-btn--sec"))
  })

  # Le pseudo-indicateur ne sert qu'a titrer les lignes de provenance.
  entete_export <- function(titre) {
    lignes_metadonnees_csv(list(titre = titre, fenetre = FALSE), ctx(), don = donnees(),
                           lieu = lieu(), dat = NULL)
  }

  output$csv_synthese <- shiny::downloadHandler(
    filename = function() nom_fichier_export("synthese", lieu()),
    content = function(file) {
      s <- synthese_securisee(donnees(), ctx())
      shiny::req(nrow(s) > 0)
      ecrire_csv_avec_metadonnees(
        bilan_synthese_export(s), file,
        c(entete_export("Synthèse chiffrée de l'année comparée"),
          sprintf("Comparaison à période égale : jusqu'au jour %d de l'année %d",
                  attr(s, "doy_max"), ctx()$courante)))
    }
  )

  output$csv_series <- shiny::downloadHandler(
    filename = function() nom_fichier_export("series_annuelles", lieu()),
    content = function(file) {
      d <- bilan_series_longues(resultats())
      shiny::req(nrow(d) > 0)
      ecrire_csv_avec_metadonnees(
        d, file,
        c(entete_export("Séries annuelles de tous les indicateurs"),
          "Format long : une ligne par indicateur, grandeur et année. Profils mensuels et journaliers : voir le CSV de chaque graphique"))
    }
  )
  invisible(NULL)
}

#' @param donnees reactive() -> tableau journalier SAFRAN.
#' @param ctx reactive() -> contexte de calcul.
#' @param lieu reactive() -> point courant.
#' @param mobile reactive() -> TRUE si affichage etroit.
#' @param tactile reactive() -> TRUE si l'appareil est tactile (pointeur grossier).
#' @return reactive() -> liste nommee des tableaux d'indicateurs calcules
#'   (reutilisee par le module rapport, pour ne pas recalculer deux fois).
mod_bilan_server <- function(id, donnees, ctx, lieu, mobile = shiny::reactive(FALSE),
                             tactile = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Calcul de tous les indicateurs, en une passe -------------------------
    resultats <- shiny::reactive({
      don <- donnees()
      shiny::req(don, nrow(don) > 0)
      shiny::withProgress(message = "Calcul des indicateurs", value = 0.5, {
        calculer_indicateurs(don, ctx())
      })
    })

    # -- Indicateurs que la source ne permet pas de calculer -------------------
    # Seulement pour une station : SAFRAN renseigne tout, et un resultat vide y
    # reste un resultat vide (message du graphique), pas une absence de source.
    est_station <- shiny::reactive(identical(attr(donnees(), "source"), "station"))

    indispos <- shiny::reactive({
      don <- donnees()
      shiny::req(don, nrow(don) > 0)
      indicateurs_non_calculables(don, resultats(), est_station())
    })

    familles_dispo <- shiny::reactive({
      ind <- indicateurs_ordonnes()
      ok <- Filter(function(i) !i$id %in% indispos()$id, ind)
      fam <- unique(vapply(ok, function(i) i$famille, character(1)))
      names(FAMILLES)[names(FAMILLES) %in% fam]
    })

    # -- Qualite de la serie (station) ---------------------------------------
    output$qualite <- shiny::renderUI({
      don <- donnees(); shiny::req(don, nrow(don) > 0)
      if (!est_station()) return(NULL)
      q <- mf_qualite_serie(don, ctx())
      shiny::req(q)
      qualite_serie_ui(q, attr(don, "station"), ctx()$ref_label)
    })

    # -- Bandeau (impression) -------------------------------------------------
    output$bandeau <- shiny::renderUI({
      p <- lieu(); c <- ctx(); don <- donnees()
      shiny::req(p, don, nrow(don) > 0)
      derniere <- max(don$date, na.rm = TRUE)
      shiny::div(
        class = "cf-bandeau",
        shiny::h2(class = "cf-bandeau__titre", p$nom),
        shiny::p(
          class = "cf-bandeau__meta",
          sprintf("%s N, %s E — référence %s — année comparée %d — série %d à %d — données jusqu'au %s",
                  fmt_fr(p$lat, 4), fmt_fr(p$lon, 4), c$ref_label, c$courante,
                  min(don$annee), max(don$annee),
                  format(derniere, "%d/%m/%Y"))
        )
      )
    })

    # -- Synthese chiffree ----------------------------------------------------
    output$synthese <- shiny::renderUI({
      don <- donnees(); shiny::req(don, nrow(don) > 0)
      s <- synthese_securisee(don, ctx())
      # Un calcul qui ECHOUE n'est pas une synthese vide : sans ce message, le
      # bloc entier (et ses points d'attention) disparaissait sans un mot (C3).
      if (!is.null(attr(s, "erreur"))) {
        return(shiny::div(
          class = "cf-alerte cf-alerte--attention", role = "status",
          shiny::span(class = "cf-alerte__titre", "Synthèse chiffrée indisponible"),
          "Le calcul de la synthèse a échoué (erreur consignée dans le journal de",
          "l'application). Les graphiques ci-dessous ne sont pas affectés.",
          cf_erreur_detail("synthese", attr(s, "erreur"))))
      }
      if (!nrow(s)) return(NULL)

      synthese_ui(s, don, en_station = est_station(), familles = familles_dispo(),
                  seuils = ctx()$seuils, ns = ns)
    })

    # -- Export CSV du bilan (voir mod_bilan_export_server) ------------------------
    mod_bilan_export_server(output, ns, donnees, resultats, ctx, lieu)

    # -- Indicateurs dont le calcul a echoue ------------------------------------
    # Avant : une carte generique par indicateur, et le detail dans le seul
    # journal du serveur (audit, S5). Ici, une alerte en tete de page les nomme ;
    # le detail technique est sur la carte.
    output$erreurs_calcul <- shiny::renderUI({
      res <- resultats()
      en_echec <- Filter(function(id) identical(attr(res[[id]], "raison"), "erreur"),
                         names(res))
      if (!length(en_echec)) return(NULL)
      n <- length(en_echec)
      titres <- vapply(INDICATEURS[en_echec], function(i) i$titre, character(1))
      shiny::div(
        class = "cf-alerte cf-alerte--attention", role = "status",
        shiny::span(class = "cf-alerte__titre",
                    sprintf("%d indicateur%s n'%s pas pu être calculé%s",
                            n, if (n > 1) "s" else "", if (n > 1) "ont" else "a",
                            if (n > 1) "s" else "")),
        shiny::tags$ul(lapply(unname(titres), shiny::tags$li)),
        shiny::div(class = "cf-aide", paste(
          "Les autres indicateurs ne sont pas affectés. Le détail technique figure",
          "sur la carte concernée : joignez-le à votre signalement."))
      )
    })

    # -- Filtre thematique ----------------------------------------------------
    # Rendu cote serveur pour n'apparaitre qu'une fois des resultats presents :
    # une barre de filtres au-dessus d'une page vide n'a rien a filtrer.
    # Grand ecran : cases a cocher (plusieurs familles). Ecran etroit : pastilles
    # a choix unique (une famille a la fois).
    # Les cases a cocher n'existent qu'une fois le filtre rendu. Tant que Shiny ne
    # leur a pas donne de valeur, input$familles vaut NULL — exactement comme
    # lorsque l'utilisateur les decoche toutes. Ce drapeau distingue les deux :
    # sans lui, « Tout masquer » n'avait aucun effet (NULL etait lu comme
    # « pas encore de filtre, tout afficher »).
    filtre_pret <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$familles, {
      if (!is.null(input$familles)) filtre_pret(TRUE)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$filtres <- shiny::renderUI({
      shiny::req(donnees(), nrow(donnees()) > 0)
      shiny::isolate(filtre_pret(FALSE))
      dispo <- familles_dispo()
      choix <- stats::setNames(dispo, unname(FAMILLES[dispo]))

      if (isTRUE(mobile())) {
        return(shiny::div(
          class = "cf-pastilles",
          shiny::radioButtons(
            ns("famille_unique"), label = "Thématique", choices = choix,
            selected = shiny::isolate(input$famille_unique) %||% dispo[1],
            inline = TRUE)))
      }

      shiny::div(
        class = "cf-filtres",
        shiny::div(
          class = "cf-filtres__entete",
          shiny::span(class = "cf-filtres__legende", "Thématiques affichées"),
          shiny::div(
            class = "cf-filtres__raccourcis",
            shiny::actionLink(ns("tout_cocher"), "Tout afficher"),
            shiny::actionLink(ns("tout_decocher"), "Tout masquer")
          )
        ),
        shiny::div(
          class = "cf-chips",
          shiny::checkboxGroupInput(
            ns("familles"), label = "Thématiques affichées",
            choices = choix, selected = dispo, inline = TRUE
          )
        )
      )
    })

    shiny::observeEvent(input$tout_cocher, {
      shiny::updateCheckboxGroupInput(session, "familles", selected = familles_dispo())
    })
    shiny::observeEvent(input$tout_decocher, {
      shiny::updateCheckboxGroupInput(session, "familles", selected = character(0))
    })

    # Familles effectivement affichees
    familles_actives <- shiny::reactive({
      dispo <- familles_dispo()
      if (isTRUE(mobile())) {
        f <- input$famille_unique
        if (is.null(f) || !f %in% dispo) dispo[1] else f
      } else if (filtre_pret()) {
        intersect(input$familles %||% character(0), dispo)
      } else {
        dispo
      }
    })

    # -- Lien « Voir les graphiques » du bloc A retenir ---------------------------
    # Le defilement jusqu'a la section est fait par app.js ; ici, on s'assure que
    # la thematique est AFFICHEE (filtre decoche, ou autre pastille en affichage
    # etroit), sans quoi l'ancre n'existerait pas.
    shiny::observeEvent(input$ouvrir_famille, {
      f <- input$ouvrir_famille$f
      shiny::req(is.character(f), length(f) == 1L, f %in% familles_dispo())
      if (isTRUE(mobile())) {
        shiny::updateRadioButtons(session, "famille_unique", selected = f)
      } else if (!f %in% familles_actives()) {
        shiny::updateCheckboxGroupInput(session, "familles",
                                        selected = union(familles_actives(), f))
      }
    })

    # -- Instanciation generique des modules graphiques -----------------------
    # Les serveurs sont crees une seule fois, pour tous les indicateurs du
    # registre. L'UI, elle, est filtree dynamiquement : on evite ainsi de
    # recreer des observateurs a chaque changement de case a cocher.
    lapply(names(INDICATEURS), function(cle) {
      ind <- INDICATEURS[[cle]]
      mod_graphique_server(
        id      = ind$id,
        ind     = ind,
        # resultat_indicateur() et non « %||% data.frame() » : voir sa documentation
        # (une table vide perdait ses attributs « raison » et « message »).
        donnees = shiny::reactive({ resultat_indicateur(resultats(), cle) }),
        ctx     = ctx,
        lieu    = lieu,
        mobile  = mobile,
        tactile = tactile,
        # Les donnees journalieres brutes ne servent qu'aux cartes munies d'un
        # selecteur d'annee, qui recalculent leur indicateur elles-memes. Les
        # autres continuent de consommer le calcul unique ci-dessus.
        brut    = donnees
      )
    })

    output$graphiques <- shiny::renderUI({
      shiny::req(donnees(), nrow(donnees()) > 0)

      familles <- familles_actives()
      # Un indicateur que la source ne permet pas de calculer n'a pas de carte
      liste <- Filter(function(i) !i$id %in% indispos()$id, indicateurs_ordonnes(familles))

      if (!length(liste)) {
        return(shiny::div(class = "cf-alerte cf-alerte--info",
                          if (length(familles_dispo()))
                            "Sélectionnez au moins une thématique."
                          else "Aucun indicateur ne peut être calculé avec cette source."))
      }

      # L'ordre des sections est celui de FAMILLES ; a l'interieur d'une
      # section, celui d'ORDRE_INDICATEURS. Les deux viennent du registre :
      # ce module ne connait toujours aucun indicateur par son nom.
      sections <- lapply(names(FAMILLES), function(f) {
        if (!f %in% familles) return(NULL)
        inds <- Filter(function(i) identical(i$famille, f), liste)
        n <- length(inds)
        if (!n) return(NULL)

        shiny::tags$section(
          class = "cf-famille", id = paste0("fam-", f),
          shiny::h2(
            class = "cf-famille__titre",
            FAMILLES[[f]],
            shiny::span(class = "cf-famille__compte",
                        sprintf("%d indicateur%s", n, if (n > 1) "s" else ""))
          ),
          shiny::div(
            class = "cf-grille-graphes",
            # ns() est indispensable : l'UI est generee cote serveur, elle doit
            # etre placee dans le namespace du module parent pour rejoindre les
            # serveurs instancies ci-dessus.
            lapply(inds, function(ind) mod_graphique_ui(ns(ind$id), ind))
          )
        )
      })

      # Sommaire : la page fait une quinzaine d'ecrans en grand format. Un menu
      # d'ancres, colle sous la barre d'onglets, evite de tout faire defiler
      # pour atteindre la derniere thematique. Inutile sur ecran etroit, ou une
      # seule thematique est construite a la fois (pastilles).
      fam_ok <- Filter(function(f) f %in% familles &&
                         any(vapply(liste, function(i) identical(i$famille, f), logical(1))),
                       names(FAMILLES))
      sommaire <- if (!isTRUE(mobile()) && length(fam_ok) > 1L) {
        shiny::tags$nav(
          class = "cf-sommaire", `aria-label` = "Sommaire du bilan",
          shiny::span(class = "cf-sommaire__lib", "Aller à"),
          shiny::tags$ul(
            lapply(fam_ok, function(f) {
              n <- sum(vapply(liste, function(i) identical(i$famille, f), logical(1)))
              shiny::tags$li(shiny::tags$a(
                href = paste0("#fam-", f), `data-famille` = f,
                FAMILLES[[f]],
                shiny::span(class = "cf-sommaire__n", `aria-hidden` = "true", n),
                shiny::span(class = "cf-sr-only",
                            sprintf(", %d indicateur%s", n, if (n > 1) "s" else ""))))
            })))
      }

      htmltools::tagList(sommaire, Filter(Negate(is.null), sections))
    })

    # -- Indicateurs absents, en fin de page ----------------------------------
    output$indisponibles <- shiny::renderUI({
      shiny::req(donnees(), nrow(donnees()) > 0)
      m <- indispos()
      if (!nrow(m)) return(NULL)
      cf_repli(
        sprintf("%d indicateur%s non calculé%s avec cette station",
                nrow(m), if (nrow(m) > 1) "s" else "", if (nrow(m) > 1) "s" else ""),
        shiny::tags$ul(
          class = "cf-liste-simple",
          lapply(seq_len(nrow(m)), function(i)
            shiny::tags$li(shiny::strong(m$titre[i]), " — manque : ", m$manque[i]))),
        shiny::div(class = "cf-aide", paste(
          "Ces indicateurs ne sont pas affichés plutôt que calculés sur du vide :",
          "une variable non mesurée donnerait « 0 » alors que la valeur est inconnue.",
          "La grille SAFRAN les fournit tous.")),
        ouvert = FALSE)
    })

    resultats
  })
}
