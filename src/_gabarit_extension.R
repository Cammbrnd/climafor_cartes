# ==============================================================================
# _gabarit_extension.R — MODELE POUR LES EVOLUTIONS A VENIR
# ------------------------------------------------------------------------------
# CE FICHIER N'EST PAS CHARGE PAR global.R (son nom commence par un souligne).
# Il documente, par l'exemple, les trois façons d'etendre l'application, dans
# l'ordre croissant d'effort.
#
#   Cas 1 — un graphique de plus              : ~30 lignes, aucun fichier a modifier
#   Cas 2 — un onglet thematique de plus      : un module + 2 lignes dans app.R
#   Cas 3 — une nouvelle source de donnees    : une fonction dans utils_api.R
#
# Les trois evolutions annoncees se rangent ainsi :
#   - module ravageurs (typographe, chalcographe, stenographe, monochamus,
#     processionnaire du pin) -> cas 1 puis cas 2 ;
#   - module aleas (neige lourde, grele, secheresse)                -> cas 1 ;
#   - module territoire (bilans departementaux et regionaux, cartes
#     en aplats de couleur)                                         -> cas 3.
# ==============================================================================


# ==============================================================================
# CAS 1 — AJOUTER UN INDICATEUR
# ------------------------------------------------------------------------------
# Exemple reel et directement reutilisable : le nombre de generations annuelles
# potentielles d'Ips typographus, estime par cumul de degres-jours.
#
# Trois etapes : une fonction de calcul, une fonction de trace, une entree dans
# le registre. Rien d'autre. L'indicateur apparait alors dans le bilan, dans le
# filtre par famille, dans les exports CSV et PNG, dans l'apercu du rapport et
# dans les fiches methodologiques de l'onglet Sources.
# ==============================================================================

# --- Etape 1 : le calcul, dans src/utils_calc.R --------------------------------

#' Generations potentielles du typographe (Ips typographus).
#'
#' Le developpement d'une generation complete demande environ 550 degres-jours
#' au-dessus de 8,3 degC (seuil de developpement de l'espece). Le vol de
#' printemps est declenche par le franchissement de 16-18 degC. On cumule donc
#' les degres-jours du 1er avril au 30 septembre et on divise par l'exigence
#' thermique d'une generation.
#'
#' Reference : Wermelinger B., Seifert M. (1998). Analysis of the temperature
#' dependent development of the spruce bark beetle. Journal of Applied
#' Entomology, 122.
#'
#' A VERIFIER AVANT MISE EN SERVICE : les parametres ci-dessous doivent etre
#' cales sur les references du Departement de la sante des forets pour la
#' region concernee ; ils varient selon les auteurs.
calc_generations_typographe <- function(don, ctx) {
  base    <- 8.3    # degC — seuil de developpement (a passer dans CONFIG$seuils)
  exigence <- 557   # degres-jours pour une generation complete
  fenetre <- 4:9    # avril a septembre

  d <- don[don$mois %in% fenetre, , drop = FALSE]
  d$dj <- pmax(0, d$t_moy - base)

  res <- stats::aggregate(list(dj = d$dj), by = list(annee = d$annee),
                          FUN = function(x) sum(x, na.rm = TRUE))
  res$annee       <- as.integer(res$annee)
  res$generations <- res$dj / exigence
  # Les fonctions transverses de utils_calc.R sont directement reutilisables
  enrichir_annuel(res, don, ctx, "generations")
}

# --- Etape 2 : le trace, dans src/utils_plot.R ---------------------------------

plot_generations_typographe <- function(dat, ctx, mobile = FALSE) {
  # graphe_annuel() apporte gratuitement : coloration des anomalies, ligne de
  # normale, moyenne mobile, tendance, graduations adaptees a la largeur.
  graphe_annuel(dat, "generations", "Générations complètes potentielles", ctx,
                type = "barres", sens = "chaud", montrer_tendance = TRUE,
                mobile = mobile) +
    ggplot2::geom_hline(yintercept = c(1, 2, 3), colour = CL$texte_mention,
                        linetype = "dotted", linewidth = 0.4)
}

# --- Etape 3 : la declaration, dans src/registre_indicateurs.R ------------------

# A inserer dans la liste INDICATEURS :
#
#   typo = list(
#     id = "typo", titre = "Générations potentielles du typographe",
#     famille = "ravageurs",
#     calc = calc_generations_typographe, plot = plot_generations_typographe,
#     colonnes = c("annee", "dj", "generations", "normale", "anomalie"),
#     comparaison = TRUE, fenetre = FALSE,
#     methode = "...",
#     lecture = "...",
#     resume = function(dat, ctx) resume_standard(
#       dat, ctx, "generations", "Générations potentielles", "", 2,
#       "au-dessus de", "en dessous de")
#   )
#
# Puis ajouter "typo" a ORDRE_INDICATEURS et la famille a FAMILLES :
#   FAMILLES <- c(..., ravageurs = "Ravageurs et pathogènes")


# ==============================================================================
# CAS 2 — AJOUTER UN ONGLET
# ------------------------------------------------------------------------------
# Squelette d'un module thematique. Il suffit ensuite d'ajouter dans app.R :
#   UI      : shiny::tabPanel("Aléas", shiny::div(class = "cf-conteneur",
#                                                 mod_aleas_ui("aleas")))
#   Serveur : mod_aleas_server("aleas", donnees, ctx, lieu)
# ==============================================================================

mod_gabarit_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::h2(class = "cf-titre-section", "Titre de l'onglet"),
    shiny::p(class = "cf-chapeau", "Une phrase disant ce que l'onglet permet de faire."),
    shiny::div(class = "cf-carte", shiny::plotOutput(ns("graphe")))
  )
}

mod_gabarit_server <- function(id, donnees, ctx, lieu) {
  shiny::moduleServer(id, function(input, output, session) {
    output$graphe <- shiny::renderPlot({
      don <- donnees()
      shiny::req(don, nrow(don) > 0)
      # ... calcul et trace
      graphe_message("À implémenter")
    })
  })
}


# ==============================================================================
# CAS 3 — MODULE TERRITOIRE : ce qu'il faut anticiper
# ------------------------------------------------------------------------------
# Passer du point au departement ou a la region change la nature du probleme :
# on ne peut plus interroger l'API point par point.
#
# Trois options, par ordre de faisabilite :
#
# a) Grille d'echantillonnage. Tirer 30 a 80 points repartis sur le territoire
#    (centroides de communes ponderes par la surface boisee, ou grille reguliere
#    de 16 km), les charger en parallele, et agreger. Fonctionne avec
#    l'infrastructure actuelle : safran_charger() se boucle simplement, et les
#    calc_*() s'appliquent tels quels au tableau agrege.
#    -> Interpolation ensuite par IDW ou krigeage pour l'aplat de couleur.
#
# b) Requete EDR "cube" ou "area" sur une emprise. L'API OGC EDR expose,
#    outre /position, les points d'entree /cube et /area. Beaucoup plus
#    efficace, mais renvoie un volume important : prevoir une agregation
#    cote serveur et un dimensionnement memoire superieur a l'offre gratuite.
#
# c) Precalcul. Constituer hors ligne un jeu de normales par maille, stocke en
#    Parquet ou GeoPackage et livre avec l'application. L'app ne fait plus que
#    lire. C'est l'option la plus econome a l'usage, et la seule tenable sur
#    l'instance gratuite pour un rendu cartographique regional.
#
# Dependances a prevoir dans les trois cas : sf pour les geometries
# administratives (contours ADMIN-EXPRESS de l'IGN, telecharges une fois et
# simplifies avec rmapshaper), et une echelle de couleur continue construite
# avec ggplot2::scale_fill_viridis_c() ou une palette DSFR sequentielle.
#
# Charte : le DSFR fournit des palettes sequentielles et divergentes
# accessibles. Pour une anomalie (positive/negative), retenir une palette
# divergente ancree sur zero, et non un gradient continu, sous peine de rendre
# le signe illisible.
# ==============================================================================
