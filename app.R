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
#
# 7. (v2.0) TROIS PROFILS, UNE APPLICATION. L'adresse (?id=... ou ?profil=...) designe
#    le profil : permanent DSF, correspondant-observateur ou terrain
#    (src/utils_profils.R). La page (src/ui_page.R) et le serveur n'en construisent que
#    les onglets : l'analyse d'un point (stations, telechargements SAFRAN, rapport) vit
#    dans src/serveur_dsf.R et ne tourne que pour le profil permanent. Sans profil, la
#    page d'entree demande l'identifiant. Ce n'est PAS une authentification.
# Les identifiants d'input et les modules sont inchanges.
# ==============================================================================

# global.R peut s'appeler global.r (partage reseau) : on le retrouve quelle que soit la casse.
if (!exists("CONFIG")) source(list.files(".", pattern = "^global\\.r$", ignore.case = TRUE)[1])

# ==============================================================================
# INTERFACE
# ==============================================================================

# `etat` : etat decode d'un lien partage (partage_decoder), ou NULL.
# `res`  : profil resolu (profil_pour_requete) ; le profil permanent par defaut.
ui_construire <- function(etat = NULL, res = NULL) {
  if (is.null(res)) res <- list(ok = TRUE, profil = PROFILS$dsf, id = NA_character_, nom = "")
  ui_page_construire(etat, res)
}

# L'interface est calculee a chaque ouverture de page : l'adresse demandee fixe le profil
# (?id=...) et les valeurs de depart (?x=...&y=..., lien partage). Sans profil reconnu, la
# page d'entree ; une adresse de partage illisible donne l'interface habituelle : jamais une
# erreur.
ui <- function(request) {
  q <- request$QUERY_STRING
  res <- tryCatch(profil_pour_requete(q), error = function(e) list(ok = FALSE))
  if (!isTRUE(res$ok)) return(ui_page_entree(res))
  etat <- tryCatch(partage_decoder(q), error = function(e) NULL)
  ui_construire(etat, res)
}


# ==============================================================================
# SERVEUR
# ==============================================================================

# Une erreur non rattrapee dans server() arrete la session ; sous RStudio la fenetre se
# ferme alors sans un mot. On la rattrape donc : elle est ecrite dans la console R
# ([ClimaFor] ...) ET montree a l'ecran, avec l'identifiant du profil en cours.
server <- function(input, output, session) {
  tryCatch(
    serveur_demarrer(input, output, session),
    error = function(e) {
      journaliser_erreur("le demarrage de l'application", e)
      shiny::showNotification(
        shiny::tagList(
          shiny::strong("ClimaFor n'a pas pu démarrer."),
          shiny::tags$br(),
          erreur_lisible(e, 240L),
          shiny::tags$br(),
          "Le détail est dans la console R ; à transmettre au mainteneur."
        ),
        type = "error", duration = NULL
      )
      invisible(NULL)
    }
  )
}

serveur_demarrer <- function(input, output, session) {

  # -- Profil ----------------------------------------------------------------
  # Sur la page d'entree il n'y a rien a calculer. Sinon le profil decide des modules.
  pr <- profil_de_session(session)
  if (!isTRUE(pr$ok)) return(invisible(NULL))
  profil <- pr$profil
  a_onglet <- function(o) o %in% profil$onglets

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

  aller_a <- function(onglet) {
    shiny::updateTabsetPanel(session, "onglets", selected = onglet)
  }

  # -- Le point : choisi dans l'Accueil, l'ecran Terrain ou la Synthese ---------
  # `choix_initial` : source de donnees demandee avec le point (une station cliquee sur une carte,
  # ou un lien partage). Posee ici parce que deux modules y ecrivent : la carte de l'Accueil
  # (mod_localisation_server) et le serveur d'analyse (serveur_dsf), qui la lit.
  choix_initial <- shiny::reactiveVal(NULL)
  lieu <- mod_localisation_server("loc", choix_station = if (a_onglet("accueil")) choix_initial)
  mod_sources_server("sources")

  # -- Onglet Carte : un clic sur une maille ouvre l'analyse de son point ---------
  # Profil permanent : l'analyse complete du point (serveur_dsf). Autres profils : le bilan
  # du point, lu dans les mailles de la carte, sans appel a un service distant.
  declencheur <- shiny::reactiveVal(NULL)
  ouvrir_maille <- if (a_onglet("accueil")) {
    serveur_dsf(input, output, session, lieu, aller_a, mobile, tactile,
                prefixe_adresse = profil_prefixe_adresse(pr), choix_initial = choix_initial)
  } else {
    function(p) {
      lieu(p)
      declencheur(list(point = p, instant = Sys.time()))
      aller_a(if (a_onglet("synthese")) "synthese" else "terrain")
    }
  }
  # Une station n'est analysable que par le profil permanent (source de donnees = la station) ;
  # les autres profils lisent des mailles : la fiche de la station s'y affiche sans bouton.
  ouvrir_station <- if (a_onglet("accueil")) function(p, id) ouvrir_maille(p, id) else NULL
  mod_carte_server("carte", onglet = shiny::reactive(input$onglets), ouvrir_maille = ouvrir_maille,
                   ouvrir_station = ouvrir_station)

  # -- Synthese et Terrain (v2.0) ---------------------------------------------------
  if (a_onglet("synthese")) {
    mod_synthese_server("synthese", profil, lieu, naviguer = aller_a, declencheur = declencheur,
                        mobile = mobile)
  }
  if (a_onglet("terrain")) mod_terrain_server("terrain", profil, lieu, mobile = mobile)
}

shiny::shinyApp(ui, server)
