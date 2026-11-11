# ==============================================================================
# ui_page.R — PAGE DE L'APPLICATION : ENTREE, EN-TETE, ONGLETS PAR PROFIL (v2.0)
# ------------------------------------------------------------------------------
# Jusqu'a la v1.9, app.R construisait une seule interface. Depuis la v2.0 la page
# depend du PROFIL (utils_profils.R) :
#
#   - sans profil reconnu : la page d'entree (identifiant, et profils de demonstration
#     si CONFIG$acces$demo) ;
#   - avec un profil : l'en-tete, les onglets de ce profil, le pied de page.
#
# Les onglets sont construits UN PAR UN, selon la liste du profil : un onglet absent du
# profil n'est pas simplement cache, il n'existe pas dans la page (ses champs ne sont
# pas envoyes, son code ne tourne pas).
#
# POURQUOI bootstrapPage() ET NON UN SIMPLE tagList()
# ---------------------------------------------------
# A NE PAS SUPPRIMER. Les fonctions de page de Shiny — fluidPage(), navbarPage(),
# bootstrapPage() — declarent la DEPENDANCE BOOTSTRAP (CSS et JavaScript). Sans elle,
# tabsetPanel() n'emet qu'une liste a puces : ni mise en forme, ni greffon qui bascule
# d'un panneau a l'autre. bootstrapPage() est la plus discrete des trois : elle n'ajoute
# ni conteneur ni marge.
# ==============================================================================

#' Elements communs du <head> : adaptation a l'ecran, icone, feuille de style, scripts.
#' @keywords internal
ui_page_tete <- function(avec_carte = TRUE) {
  shiny::tags$head(
    shiny::tags$meta(name = "viewport",
                     content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    shiny::tags$meta(name = "description",
                     content = "Bilan météorologique et climatique pour la gestion forestière"),
    # Favicon : sans lien declare, le navigateur demande /favicon.ico et recoit une
    # erreur 404 a chaque chargement. Pictogramme du DSF (l'oeil), en PNG transparent.
    shiny::tags$link(rel = "icon", type = "image/png", sizes = "64x64", href = "favicon.png"),
    shiny::tags$link(rel = "stylesheet", href = "climafor.css"),
    shiny::tags$script(src = "app.js"),
    # Carte (v1.8) : ce petit chargeur n'appelle MapLibre qu'a la premiere ouverture de
    # l'onglet.
    if (avec_carte) shiny::tags$script(src = "carte/carte.js")
  )
}

#' Barre d'application : logo du DSF, nom du service, et le profil en cours avec le moyen
#' d'en changer.
#' @param res profil_resoudre() ; NULL sur la page d'entree.
#' @keywords internal
ui_page_barre <- function(res = NULL) {
  profil <- if (is.null(res)) NULL else res$profil
  shiny::tags$header(
    class = "cf-barre",
    shiny::div(
      class = "cf-barre__interieur",
      shiny::div(
        class = "cf-entete-marques",
        # L'image porte un texte de remplacement : c'est un logo porteur de sens.
        shiny::img(class = "cf-logo cf-logo--operateur", src = "logo_dsf.png",
                   alt = "Correspondant observateur — Département de la santé des forêts")),
      shiny::div(
        class = "cf-marque",
        shiny::h1(class = "cf-marque__titre", CONFIG$app$nom),
        shiny::div(class = "cf-marque__soustitre", CONFIG$app$sous_titre)),
      if (!is.null(profil)) shiny::div(
        class = "cf-profil",
        shiny::span(class = "cf-profil__nom",
                    profil$libelle, if (nzchar(res$nom %||% "")) paste0(" · ", res$nom)),
        # Retour a la page d'entree : l'adresse sans requete.
        shiny::tags$a(class = "cf-profil__changer", href = "./", "Changer d'identifiant"))))
}

#' Pied de page.
#' @keywords internal
ui_page_pied <- function() {
  shiny::tags$footer(
    class = "cf-pied",
    shiny::div(
      class = "cf-pied__interieur",
      shiny::p(shiny::strong(sprintf("%s version %s", CONFIG$app$nom, CONFIG$app$version)),
               " — ", CONFIG$app$organisme),
      shiny::p(paste("Données : stations Météo-France (données climatologiques de",
                     "base) ou réanalyse SAFRAN-ISBA (Météo-France, via GeoSAS INRAE).",
                     "Licence Etalab 2.0.")),
      shiny::p(paste("Ce bilan décrit le climat d'une station ou d'une maille de",
                     "8 km. Il ne se substitue pas à un diagnostic stationnel."))))
}

#' Tableau « Synthèse, Bilan climatique, Rapport : quelle différence ? », replie sur l'accueil.
#' Les trois onglets partent du meme calcul de normale et d'ecart, mais ne servent pas a la
#' meme chose : le dire a l'entree evite de lancer un rapport pour une phrase.
#' @keywords internal
accueil_differences_ui <- function() {
  lignes <- list(
    c("Pour quoi faire", "Lire le climat d'un lieu en quelques phrases et un tableau, et en parler.",
      "Analyser un point à fond : tous les indicateurs, avec leurs graphiques.",
      "Emporter l'analyse du Bilan dans un document rédigé."),
    c("Quels lieux", "Un point, un département, une région, ou des fiches importées.",
      "Un seul point, celui de l'accueil.", "Le point analysé dans le Bilan."),
    c("Quelles données", "Mailles SAFRAN de 8 km précalculées ; pour un point, les stations Météo-France publiées à côté.",
      "La station Météo-France la plus proche (mesure) ou la grille SAFRAN (modèle), téléchargées pour le point.",
      "Celles du Bilan climatique."),
    c("Réglages", "Année, référence et grandeurs ; seuils par défaut.",
      "Tous ceux de l'onglet Paramètres : seuils, période, source.", "Niveau de détail et format."),
    c("Temps", "Quelques secondes.", "Une à plusieurs minutes la première fois : les séries sont téléchargées.",
      "Un peu plus encore : le document est composé."),
    c("Ce qu'on emporte", "Une page HTML, un fichier Word, des CSV.", "Les graphiques et les CSV.", "Un fichier Word ou HTML."))
  cf_repli(
    "Synthèse, Bilan climatique, Rapport : quelle différence ?",
    shiny::div(
      class = "cf-synt__defile", role = "region", tabindex = "0", `aria-label` = "Différences entre Synthèse, Bilan climatique et Rapport (défile horizontalement)",
      shiny::tags$table(
        class = "cf-tableau cf-synt__table",
        shiny::tags$caption(class = "cf-sr-only", "Différences entre Synthèse, Bilan climatique et Rapport"),
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(scope = "col", ""), shiny::tags$th(scope = "col", "Synthèse"),
          shiny::tags$th(scope = "col", "Bilan climatique"), shiny::tags$th(scope = "col", "Rapport"))),
        shiny::tags$tbody(lapply(lignes, function(l) shiny::tags$tr(
          shiny::tags$th(scope = "row", l[1]), lapply(l[-1], shiny::tags$td)))))),
    shiny::p(class = "cf-mention", paste(
      "En bref : la Synthèse pour lire et comparer vite ; le Bilan climatique pour analyser un point en détail ;",
      "le Rapport pour en faire un document.")))
}

#' Un onglet de l'application, par sa valeur. Les branches suivent l'ordre de la
#' navigation du profil permanent ; c'est profil$onglets qui decide lesquelles servent.
#' @param nom valeur de l'onglet ; @param profil element de PROFILS ; @param etat lien partage.
#' @keywords internal
ui_page_onglet <- function(nom, profil, etat = NULL) {
  conteneur <- function(...) shiny::div(class = "cf-conteneur", ...)
  switch(nom,

    # ---- Accueil : lieu et periode ------------------------------------------------
    accueil = shiny::tabPanel(
      title = cf_onglet_titre("accueil", "Accueil"), value = "accueil",
      conteneur(
        shiny::uiOutput("accueil"),
        shiny::div(
          class = "cf-accueil-grille",
          # Colonne 1 : le point
          shiny::div(
            class = "cf-carte",
            cf_etape(1, "Où ?", "le point à analyser"),
            mod_localisation_ui("loc", source_station = TRUE),
            # Apparait des qu'un point est designe : station proposee et qualite
            mod_station_ui("station")),
          # Colonne 2 : la periode
          shiny::div(
            class = "cf-carte",
            cf_etape(2, "Quand ?", "référence et année comparée"),
            mod_periode_ui("periode", etat))),
        # Barre d'action collante : le bouton reste visible pendant qu'on fait defiler les
        # reglages, y compris sur un ecran de telephone.
        shiny::div(
          class = "cf-barre-action",
          shiny::div(class = "cf-barre-action__etat", shiny::uiOutput("etat_chargement")),
          shiny::actionButton(
            "lancer", label = shiny::tagList(cf_icone("lancer"), "Lancer l'analyse"),
            class = "cf-btn cf-btn--principal cf-btn--large")),
        # Messages du service de donnees : ils concernent le chargement, donc ils
        # s'affichent la ou l'on a lance le chargement.
        shiny::uiOutput("messages_api"),
        accueil_differences_ui())),

    # ---- Terrain : « autour de moi », un ecran de telephone -----------------------
    terrain = shiny::tabPanel(
      title = cf_onglet_titre("position", "Autour de moi"), value = "terrain",
      conteneur(
        class = "cf-terrain",
        shiny::h2(class = "cf-titre-section", "Autour de moi"),
        shiny::p(class = "cf-chapeau", paste(
          "Le climat du lieu où vous êtes : la valeur de l'année la plus récente et son écart",
          "à la normale, d'après les mailles de 8 km de la carte et, si le jeu en contient, les stations",
          "Météo-France proches. Une synthèse courte s'envoie par courriel.")),
        shiny::div(class = "cf-carte", mod_localisation_ui("loc")),
        mod_terrain_ui("terrain"))),

    # ---- Carte : un indicateur par maille SAFRAN, pour toute la France ------------
    carte = shiny::tabPanel(
      title = cf_onglet_titre("carte", "Carte"), value = "carte",
      conteneur(mod_carte_ui("carte"))),

    # ---- Comparer : points et zones importes, rattaches aux mailles ----------------
    comparer = shiny::tabPanel(
      title = cf_onglet_titre("comparer", "Comparer"), value = "comparer",
      conteneur(mod_comparer_ui("comparer"))),

    # ---- Synthese : le bilan court d'un territoire, d'un point ou de fiches --------
    synthese = shiny::tabPanel(
      title = cf_onglet_titre("synthese", "Synthèse"), value = "synthese",
      conteneur(
        # Sans onglet Accueil, le point se choisit ici (commune ou position).
        if (!"accueil" %in% profil$onglets) shiny::tags$details(
          class = "cf-details", open = NA,
          shiny::tags$summary("Où ? Choisir un point"),
          shiny::div(class = "cf-details__corps", mod_localisation_ui("loc"))),
        mod_synthese_ui("synthese", profil))),

    # ---- Bilan climatique d'un point ----------------------------------------------
    bilan = shiny::tabPanel(
      title = cf_onglet_titre("bilan", "Bilan climatique"), value = "bilan",
      conteneur(
        shiny::uiOutput("recap"),
        shiny::uiOutput("bilan_vide"),
        mod_bilan_ui("bilan"))),

    # ---- Rapport ------------------------------------------------------------------
    rapport = shiny::tabPanel(
      title = cf_onglet_titre("rapport", "Rapport"), value = "rapport",
      conteneur(mod_rapport_ui("rapport"))),

    # ---- Parametres ---------------------------------------------------------------
    parametres = shiny::tabPanel(
      title = cf_onglet_titre("parametres", "Paramètres"), value = "parametres",
      conteneur(mod_parametres_ui("parametres", etat))),

    # ---- Sources ------------------------------------------------------------------
    # Libelle court : plusieurs onglets defilent sur la largeur d'un telephone ; un
    # libelle court limite le defilement. Le titre complet figure en tete de l'onglet.
    sources = shiny::tabPanel(
      title = cf_onglet_titre("sources", "Sources"), value = "sources",
      conteneur(mod_sources_ui("sources"))),

    stop(sprintf("onglet inconnu : %s", nom))
  )
}

#' La page d'un profil : en-tete, onglets du profil, pied de page.
#' @param etat etat decode d'un lien partage (partage_decoder), ou NULL.
#' @param res profil_resoudre() reussi.
#' @export
ui_page_construire <- function(etat, res) {
  profil <- res$profil
  onglets <- lapply(profil$onglets, ui_page_onglet, profil = profil, etat = etat)
  shiny::bootstrapPage(
    title = sprintf("%s — %s", CONFIG$app$nom, CONFIG$app$sous_titre),
    # <html lang="fr"> : sans lui, un lecteur d'ecran lit le francais avec la
    # prononciation de sa langue par defaut (WCAG 3.1.1, niveau A).
    lang = "fr",
    ui_page_tete("carte" %in% profil$onglets),
    # Lien d'evitement : premier element focusable de la page (exigence RGAA).
    shiny::tags$a(class = "cf-evitement", href = "#contenu", "Aller au contenu"),
    ui_page_barre(res),
    # <main> : repere de contenu principal. Le lien d'evitement pointe dessus ;
    # tabindex = -1 lui permet de recevoir le focus quand on l'active.
    shiny::tags$main(
      class = paste("cf-onglets", paste0("cf-profil-", profil$id)), id = "contenu", tabindex = "-1",
      do.call(shiny::tabsetPanel, c(list(id = "onglets", type = "tabs"), onglets))),
    ui_page_pied())
}

#' La page d'entree : un champ d'identifiant, et les profils de demonstration.
#'
#' Le formulaire est un formulaire HTML ordinaire (methode GET) : il recharge la page avec
#' ?id=..., sans serveur Shiny ni JavaScript. L'identifiant figure donc dans l'adresse.
#' @param res profil_resoudre() sans profil ; `res$inconnu` : un identifiant a ete refuse.
#' @export
ui_page_entree <- function(res = NULL) {
  demo <- isTRUE(CONFIG$acces$demo)
  shiny::bootstrapPage(
    title = sprintf("%s — %s", CONFIG$app$nom, CONFIG$app$sous_titre),
    lang = "fr",
    ui_page_tete(avec_carte = FALSE),
    shiny::tags$a(class = "cf-evitement", href = "#contenu", "Aller au contenu"),
    ui_page_barre(NULL),
    shiny::tags$main(
      class = "cf-onglets", id = "contenu", tabindex = "-1",
      shiny::div(
        class = "cf-conteneur cf-entree",
        shiny::h2(class = "cf-titre-section", "Accéder à ClimaFor"),
        shiny::p(class = "cf-chapeau", paste(
          "Le climat de votre forêt, d'un département ou de vos fiches, sur les mailles de 8 km de la",
          "réanalyse SAFRAN. Saisissez l'identifiant qui vous a été communiqué.")),
        if (isTRUE(res$inconnu)) shiny::div(
          class = "cf-alerte cf-alerte--erreur", role = "alert",
          shiny::span(class = "cf-alerte__titre", "Identifiant inconnu"),
          "Vérifiez-le, ou demandez-le à votre contact au DSF."),
        shiny::tags$form(
          class = "cf-carte cf-entree__formulaire", method = "get", action = "./",
          shiny::tags$label(`for` = "cf-id", "Votre identifiant"),
          shiny::div(
            class = "cf-entree__champ",
            shiny::tags$input(id = "cf-id", name = "id", type = "text", class = "form-control cf-champ",
                              maxlength = CONFIG$acces$id_long_max, required = NA, autocomplete = "off",
                              autocapitalize = "none", spellcheck = "false"),
            # Pas de type="submit" : Shiny detourne tout bouton de ce type (il y voit un
            # submitButton et envoie le formulaire par sa websocket). Le champ etant seul
            # dans le formulaire, la touche Entree l'envoie quand meme, par le navigateur.
            shiny::tags$button(
              type = "button", class = "cf-btn cf-btn--principal",
              onclick = "if (this.form.reportValidity()) this.form.submit();", "Entrer"))),
        if (demo) shiny::div(
          class = "cf-entree__demo",
          shiny::h3(class = "cf-carte__titre", "Ou essayer une démonstration"),
          shiny::p(class = "cf-mention", "Chaque profil est une interface différente, sur les mêmes données."),
          shiny::tags$ul(
            class = "cf-entree__profils",
            lapply(PROFILS, function(p) shiny::tags$li(
              shiny::tags$a(
                class = "cf-entree__profil", href = sprintf("./?profil=%s", p$id),
                shiny::span(class = "cf-entree__libelle", p$libelle),
                shiny::span(class = "cf-entree__resume", p$resume)))))))),
    ui_page_pied())
}
