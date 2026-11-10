# ==============================================================================
# references.R — REFERENCES DATEES ET STATUT DES AFFIRMATIONS
# ------------------------------------------------------------------------------
# Audit v1.1, M6 : les seuils et les affirmations ecologiques de l'application
# (photosynthese au-dela de 35 degC, SWI de 0,3 et scolytes, stomates fermes a
# 1,5 kPa, deperissement l'annee suivante) ne renvoyaient a aucune reference.
# Plausibles, mais inverifiables par l'utilisateur.
#
# UNE SEULE TABLE, TROIS USAGES
# -----------------------------
#  - la carte « Références » de l'onglet Sources (liste complete) ;
#  - la mention « Réf. : Auteur et al. année » sous chaque seuil (Paramètres) et
#    dans les fiches de méthode ;
#  - les tests, qui verifient qu'aucun seuil ne cite une reference absente.
#
# REGLE DE LA TABLE
# -----------------
# Les references ajoutees en v1.5 ont ete verifiees (titre, auteurs, revue, annee)
# sur des sources en ligne ; volumes et pages ne l'ont pas tous ete. Celles de la
# bibliographie d'origine sont reprises telles quelles. Quand une valeur est une
# convention d'usage sans etude derriere, on le dit (statut « convention ») :
# mieux vaut un repere assume comme tel qu'une citation qui lui preterait une
# autorite qu'il n'a pas.
#
# Deux statuts, pas davantage :
#   « convention » : repere d'usage courant, non demontre par une etude. Le
#                    changer n'est pas une erreur.
#   « indicatif »  : ordre de grandeur appuye par la litterature citee, sans que
#                    la valeur exacte en decoule ni qu'elle vaille pour toutes
#                    les essences.
# ==============================================================================

REFERENCES <- data.frame(
  id = c(
    "vidal2010", "quintana2008", "soubeyroux2008",
    "allen1998", "hargreaves1985", "bagnouls1953", "demartonne1926",
    "wmo2017", "zhang2011", "frich2002",
    "mann1945", "kendall1975", "sen1968", "hamed1998", "pettitt1979",
    "granier1999", "breda2006", "netherer2015", "grossiord2020", "sage2007",
    "zohner2020", "vitasse2018", "gardiner2016", "piedallu2013",
    "bigler2018", "lenz2013", "vitasse2018gel", "delpierre2016"),
  court = c(
    "Vidal et al. 2010", "Quintana-Seguí et al. 2008", "Soubeyroux et al. 2008",
    "Allen et al. 1998", "Hargreaves et Samani 1985", "Bagnouls et Gaussen 1953",
    "De Martonne 1926",
    "OMM 2017", "Zhang et al. 2011", "Frich et al. 2002",
    "Mann 1945", "Kendall 1975", "Sen 1968", "Hamed et Rao 1998", "Pettitt 1979",
    "Granier et al. 1999", "Bréda et al. 2006", "Netherer et al. 2015",
    "Grossiord et al. 2020", "Sage et Kubien 2007",
    "Zohner et al. 2020", "Vitasse et al. 2018a", "Gardiner et al. 2016",
    "Piedallu et al. 2013",
    "Bigler et Bugmann 2018", "Lenz et al. 2013", "Vitasse et al. 2018b",
    "Delpierre et al. 2016"),
  citation = c(
    "Vidal J.-P. et al. (2010). A 50-year high-resolution atmospheric reanalysis over France with the Safran system. International Journal of Climatology, 30(11).",
    "Quintana-Seguí P., Le Moigne P., Durand Y., Martin E., Habets F., Baillon M., Canellas C., Franchisteguy L., Morel S. (2008). Analysis of near-surface atmospheric variables: validation of the SAFRAN analysis over France. Journal of Applied Meteorology and Climatology, 47(1), 92-107. doi:10.1175/2007JAMC1636.1",
    "Soubeyroux J.-M. et al. (2008). Le système SIM : bilan et perspectives. La Météorologie, 63.",
    "Allen R.G. et al. (1998). Crop evapotranspiration — guidelines for computing crop water requirements. FAO Irrigation and Drainage Paper 56.",
    "Hargreaves G.H., Samani Z.A. (1985). Reference crop evapotranspiration from temperature. Applied Engineering in Agriculture, 1(2).",
    "Bagnouls F., Gaussen H. (1953). Saison sèche et indice xérothermique. Bulletin de la Société d'Histoire Naturelle de Toulouse, 88.",
    "De Martonne E. (1926). Aréisme et indice d'aridité. Comptes Rendus de l'Académie des Sciences, 182.",
    "Organisation météorologique mondiale (2017). WMO Guidelines on the Calculation of Climate Normals. WMO-No. 1203.",
    "Zhang X., Alexander L., Hegerl G.C., Jones P., Klein Tank A., Peterson T.C., Trewin B., Zwiers F.W. (2011). Indices for monitoring changes in extremes based on daily temperature and precipitation data. WIREs Climate Change, 2(6), 851-870. doi:10.1002/wcc.147",
    "Frich P., Alexander L.V., Della-Marta P., Gleason B., Haylock M., Klein Tank A.M.G., Peterson T. (2002). Observed coherent changes in climatic extremes during the second half of the twentieth century. Climate Research, 19(3), 193-212. doi:10.3354/cr019193",
    "Mann H.B. (1945). Nonparametric tests against trend. Econometrica, 13(3), 245-259.",
    "Kendall M.G. (1975). Rank Correlation Methods (4e éd.). Londres : Griffin.",
    "Sen P.K. (1968). Estimates of the regression coefficient based on Kendall's tau. Journal of the American Statistical Association, 63(324), 1379-1389.",
    "Hamed K.H., Rao A.R. (1998). A modified Mann-Kendall trend test for autocorrelated data. Journal of Hydrology, 204(1-4), 182-196.",
    "Pettitt A.N. (1979). A non-parametric approach to the change-point problem. Journal of the Royal Statistical Society, Series C (Applied Statistics), 28(2), 126-135.",
    "Granier A., Bréda N., Biron P., Villette S. (1999). A lumped water balance model to evaluate duration and intensity of drought constraints in forest stands. Ecological Modelling, 116, 269-283.",
    "Bréda N., Huc R., Granier A., Dreyer E. (2006). Temperate forest trees and stands under severe drought: a review of ecophysiological responses, adaptation processes and long-term consequences. Annals of Forest Science, 63(6), 625-644. doi:10.1051/forest:2006042",
    "Netherer S. et al. (2015). Do water-limiting conditions predispose Norway spruce to bark beetle attack? New Phytologist, 205, 1128-1141.",
    "Grossiord C., Buckley T.N., Cernusak L.A., Novick K.A., Poulter B., Siegwolf R.T.W., Sperry J.S., McDowell N.G. (2020). Plant responses to rising vapor pressure deficit. New Phytologist, 226(6), 1550-1566. doi:10.1111/nph.16485",
    "Sage R.F., Kubien D.S. (2007). The temperature response of C3 and C4 photosynthesis. Plant, Cell & Environment, 30(9), 1086-1106.",
    "Zohner C.M. et al. (2020). Late-spring frost risk between 1959 and 2017 decreased in North America but increased in Europe and Asia. Proceedings of the National Academy of Sciences, 117(22), 12192-12200.",
    "Vitasse Y., Signarbieux C., Fu Y.H. (2018). Global warming leads to more uniform spring phenology across elevations. Proceedings of the National Academy of Sciences, 115(5), 1004-1008. doi:10.1073/pnas.1717342115",
    "Gardiner B., Berry P., Moulia B. (2016). Review: Wind impacts on plant growth, mechanics and damage. Plant Science, 245, 94-118.",
    "Piedallu C. et al. (2013). Soil water balance performs better than climatic water variables in tree species distribution modelling. Global Ecology and Biogeography, 22(4).",
    "Bigler C., Bugmann H. (2018). Climate-induced shifts in leaf unfolding and frost risk of European trees and shrubs. Scientific Reports, 8, 9865. doi:10.1038/s41598-018-27893-1",
    "Lenz A., Hoch G., Vitasse Y., Körner C. (2013). European deciduous trees exhibit similar safety margins against damage by spring freeze events along elevational gradients. New Phytologist, 200(4), 1166-1175. doi:10.1111/nph.12452",
    "Vitasse Y., Schneider L., Rixen C., Christen D., Rebetez M. (2018). Increase in the risk of exposure of forest and fruit trees to spring frosts at higher elevation in Switzerland over the last four decades. Agricultural and Forest Meteorology, 248, 60-69. doi:10.1016/j.agrformet.2017.09.005",
    "Delpierre N., Vitasse Y., Chuine I., Guillemot J., Bazot S., Rutishauser T., Rathgeber C.B.K. (2016). Temperate and boreal forest tree phenology: from organ-scale processes to terrestrial ecosystem models. Annals of Forest Science, 73, 5-25. doi:10.1007/s13595-015-0477-6"),
  stringsAsFactors = FALSE
)

# References sans auteur-annee, qui ne se citent pas dans un seuil.
REFERENCES_DIVERSES <- c(
  "Département de la santé des forêts. Protocoles de notation du dépérissement (DEPERIS).",
  "Météo-France, DRIAS et Climadiag Forêt — projections climatiques régionalisées.")

#' Forme courte « Auteur et al. année » d'une liste d'identifiants.
#' Un identifiant inconnu est une erreur de programmation, pas de donnees :
#' on le signale (stop) plutot que de l'afficher tel quel.
#' @param ids vecteur d'identifiants, ou une chaine « id1;id2 ».
#' @return une chaine « A ; B », ou "" si `ids` est vide.
#' @export
ref_courte <- function(ids) {
  ids <- unlist(strsplit(as.character(ids %||% ""), ";", fixed = TRUE))
  ids <- trimws(ids[nzchar(trimws(ids))])
  if (!length(ids)) return("")
  inconnus <- setdiff(ids, REFERENCES$id)
  if (length(inconnus)) {
    stop(sprintf("Référence inconnue : %s", paste(inconnus, collapse = ", ")))
  }
  paste(REFERENCES$court[match(ids, REFERENCES$id)], collapse = " ; ")
}

#' Libelles des deux statuts d'un seuil.
#' @keywords internal
STATUTS_SEUIL <- c(
  convention = "Convention d'usage",
  indicatif  = "Repère indicatif")

#' Explication des deux statuts, affichee dans Paramètres et dans Sources.
#' @keywords internal
STATUTS_SEUIL_AIDE <- c(
  convention = paste("Repère couramment utilisé, qu'aucune étude ne démontre :",
                     "le modifier n'est pas une erreur."),
  indicatif  = paste("Ordre de grandeur appuyé par la littérature citée, sans que",
                     "la valeur exacte en découle, ni qu'elle vaille pour toutes",
                     "les essences."))

#' Corrections de methode : ce qui a change de facon a modifier des chiffres
#' deja publies. Affichee dans Sources, pour que l'on sache pourquoi un rapport
#' de la v1.1 ne donne plus les memes valeurs.
#' @keywords internal
CORRECTIONS_METHODE <- data.frame(
  version = c("2.4", "1.5", "1.2", "1.2"),
  sujet = c(
    "Gel tardif",
    "VPD",
    "Année en cours",
    "Tendance"),
  texte = c(
    paste("Le gel tardif n'est plus compté sur une fenêtre de dates fixe (du 1er avril au 31 mai) :",
          "c'est un jour de gel postérieur au débourrement estimé de l'année, au plus tard le 30 juin.",
          "Il devient un indicateur à part, et le tableau des jours de gel ne le porte plus. Les valeurs",
          "d'un rapport ou d'un export antérieur ne sont pas comparables."),
    paste("Formule du déficit de pression de vapeur corrigée (FAO-56) : la pression",
          "de vapeur réelle est calculée sur la moyenne des saturations aux",
          "températures extrêmes, comme la saturation. Les versions antérieures",
          "surestimaient le VPD de 5 à 20 % (jusqu'à 20 % en hiver, 5 % en canicule).",
          "Sur des étés simulés, elles comptaient près de deux fois trop de jours à",
          "1,5 kPa ou plus."),
    paste("Une année à laquelle il manque plus de deux jours est dite partielle :",
          "grisée, exclue de la normale, de la moyenne mobile et de la tendance."),
    paste("Pente de Sen et test de Mann-Kendall corrigé de l'autocorrélation, sur",
          "vingt années complètes au moins. L'ancienne régression linéaire déclarait",
          "une tendance « significative » dans 13 à 42 % des séries sans tendance dès",
          "que leur autocorrélation atteignait 0,3 à 0,7, au lieu de 5 %.")),
  stringsAsFactors = FALSE
)

#' Carte « Références » de l'onglet Sources.
#' @keywords internal
references_ui <- function() {
  ordre <- order(iconv(REFERENCES$citation, to = "ASCII//TRANSLIT"))
  shiny::div(
    class = "cf-carte", id = "references",
    shiny::h3(class = "cf-carte__titre", "Références"),
    shiny::div(class = "cf-aide", paste(
      "Les seuils de l'onglet Paramètres renvoient à cette liste ; les valeurs sans",
      "étude derrière sont signalées comme conventions. Titre, auteurs, revue et année",
      "des références ajoutées en version 1.5 ont été vérifiés sur des sources en",
      "ligne (volumes et pages ne l'ont pas tous été) ; celles de la bibliographie",
      "d'origine sont reprises telles quelles.")),
    shiny::tags$ul(
      class = "cf-liste cf-liste--refs",
      c(lapply(ordre, function(i) shiny::tags$li(REFERENCES$citation[i])),
        lapply(REFERENCES_DIVERSES, shiny::tags$li)))
  )
}

#' Carte « Corrections de methode » de l'onglet Sources.
#' @keywords internal
corrections_ui <- function() {
  shiny::div(
    class = "cf-carte", id = "corrections",
    shiny::h3(class = "cf-carte__titre", "Corrections de méthode"),
    shiny::div(class = "cf-aide", paste(
      "Ce qui a changé de façon à modifier des chiffres déjà publiés. Un rapport",
      "ou un export d'une version antérieure peut donc donner des valeurs différentes.")),
    shiny::tags$dl(
      class = "cf-liste-def",
      unlist(lapply(seq_len(nrow(CORRECTIONS_METHODE)), function(i) {
        list(shiny::tags$dt(sprintf("Version %s — %s",
                                    CORRECTIONS_METHODE$version[i],
                                    CORRECTIONS_METHODE$sujet[i])),
             shiny::tags$dd(CORRECTIONS_METHODE$texte[i]))
      }), recursive = FALSE))
  )
}
