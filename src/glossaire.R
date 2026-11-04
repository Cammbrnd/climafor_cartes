# ==============================================================================
# glossaire.R — VOCABULAIRE DE L'APPLICATION
# ------------------------------------------------------------------------------
# P - ETP, SWI, VPD, GDD, De Martonne, normale : ces termes n'etaient expliques
# que dans le depliant « Methode et lecture » de chaque carte (audit, S3). Un
# gestionnaire forestier s'en accommode ; un decideur non specialiste, non.
#
# UNE SEULE TABLE, DEUX USAGES
# ----------------------------
#  - la carte « Glossaire » de l'onglet Sources (toutes les entrees) ;
#  - des infobulles sur les termes qui figurent dans les titres de cartes et les
#    cartes de synthese (cf_terme) : survoler ou toucher le terme en donne la
#    definition, sans quitter le graphique.
#
# Les definitions disent AUSSI ce qu'elles n'affirment pas (un sol « standard »,
# une ETP de couvert herbace) : elles reprennent les memes limites que les
# fiches de methode du registre.
#
# `motif` : expression reguliere des formes reperees dans les titres ; NA pour
# une entree qui ne figure que dans le glossaire.
# ==============================================================================

GLOSSAIRE <- data.frame(
  terme = c(
    "Normale", "Période de référence", "Écart à la normale", "Écart-type",
    "Année partielle", "ETP", "P − ETP", "SWI", "VPD", "GDD (degrés-jours)",
    "Indice de De Martonne", "Diagramme ombrothermique", "Gel tardif",
    "Séquence sèche", "Vent soutenu", "Tendance", "Rupture d'homogénéité",
    "Maille SAFRAN", "Fenêtre de calcul"),
  definition = c(
    paste("Moyenne d'une grandeur sur la période de référence, calculée année par",
          "année. Elle décrit le climat passé, pas une trajectoire."),
    paste("Les trente années sur lesquelles on calcule la normale. 1991-2020 est la",
          "référence en vigueur à l'OMM ; 1961-1990 sert de repère antérieur à",
          "l'essentiel du réchauffement récent. Ce n'est pas la base",
          "pré-industrielle du GIEC (1850-1900), que SAFRAN ne couvre pas."),
    paste("Valeur de l'année comparée moins la normale. Une carte n'est colorée que si",
          "l'écart dépasse une demi-écart-type de la référence ; en deçà, il reste",
          "dans la variabilité habituelle."),
    paste("Mesure de la variabilité d'une grandeur d'une année sur l'autre. Un écart de",
          "plus d'un écart-type est marqué ; de moins d'une demi, banal."),
    paste("Année à laquelle il manque plus de deux jours, en général l'année en cours.",
          "Elle est comparée à période égale, grisée sur les graphiques et exclue de la",
          "normale, de la moyenne mobile et de la tendance."),
    paste("Évapotranspiration potentielle : eau, en mm, qu'un couvert herbacé bien",
          "alimenté évaporerait et transpirerait. Calculée par Penman-Monteith avec",
          "SAFRAN, estimée par Hargreaves avec une station : les deux ne sont pas",
          "comparables en valeur absolue."),
    paste("Bilan hydrique climatique : précipitations moins ETP sur une fenêtre de mois.",
          "Négatif, c'est un déficit. À lire en relatif, entre années et entre sites."),
    paste("Soil Wetness Index : humidité du sol simulée par le modèle ISBA, de 0 (sol",
          "sec) à 1 (sol saturé). Le sol simulé est un sol standard : la réserve utile",
          "réelle d'une station peut différer."),
    paste("Vapour Pressure Deficit : écart, en kPa, entre la pression de vapeur",
          "saturante de l'air et sa pression de vapeur réelle. Plus il est élevé, plus",
          "l'air est avide d'eau et plus les arbres ferment leurs stomates, de façon",
          "progressive et selon l'essence. Ici, un VPD moyen journalier (FAO-56), plus",
          "faible que celui de l'après-midi : 1,5 kPa y est un repère d'air très sec,",
          "pas un seuil de fermeture des stomates."),
    paste("Growing Degree Days : somme, jour après jour, de l'excédent de température",
          "moyenne au-dessus d'une base (5 °C par défaut), plafonné. Mesure la chaleur",
          "disponible pour la croissance et le développement."),
    paste("Indice d'aridité annuel P / (T + 10), avec P en mm et T en °C. Plus il est",
          "bas, plus le climat est aride."),
    paste("Diagramme de Bagnouls-Gaussen : précipitations et températures mensuelles sur",
          "un même graphique. Un mois est dit sec lorsque ses précipitations (mm) sont",
          "inférieures au double de sa température moyenne (°C)."),
    paste("Gel de printemps, après le débourrement, qui peut détruire les jeunes pousses",
          "non aoûtées. Ici : jours de gel sous abri dans une fenêtre de dates fixe (du",
          "1er avril au 31 mai par défaut), sans lien avec le débourrement réel du",
          "peuplement, qui varie avec l'essence, l'année et l'altitude."),
    paste("Suite de jours consécutifs sans pluie significative (moins de 1 mm par",
          "défaut)."),
    paste("Vent moyen journalier supérieur au 95e percentile des vents de la période de",
          "référence, au point étudié. Il mesure une exposition, pas un risque de",
          "chablis."),
    paste("Pente robuste (médiane des pentes entre toutes les paires d'années, méthode de",
          "Theil-Sen) et test de Mann-Kendall corrigé de l'autocorrélation. Affichée à",
          "partir de vingt années complètes ; « non significative » ne veut pas dire",
          "« absente »."),
    paste("Changement brusque dans une série de station (déplacement, changement de",
          "capteur), repéré par le test de Pettitt. Il est signalé à titre indicatif ;",
          "la série n'est pas corrigée."),
    paste("Carré de 8 km de côté de la réanalyse SAFRAN-ISBA de Météo-France. Il lisse",
          "le relief : en terrain accidenté, le point étudié peut s'écarter de la maille.",
          "SAFRAN analyse les variables par zones d'environ 1 000 km² avant de les",
          "ramener à la maille (Quintana-Seguí et al. 2008) : la finesse de 8 km est",
          "celle de la grille, pas celle de l'information."),
    paste("Suite de mois sur laquelle on cumule précipitations et ETP : avril à octobre",
          "(saison de végétation) par défaut.")),
  motif = c(
    "normale", NA, "écart à la normale", "écart-type", NA, "ETP", "P\\s*[-−]\\s*ETP",
    "SWI", "VPD|Déficit de pression de vapeur", "GDD|Degrés-jours",
    "De Martonne", "ombrothermique", "gel tardif", "Séquences? sèches?",
    "vent soutenu", NA, NA, NA, NA),
  stringsAsFactors = FALSE
)

#' Motif unique couvrant toutes les formes reperees (alternance, sans casse).
#' Les bornes (?<![[:alnum:]]) / (?![[:alnum:]]) evitent de saisir « ETP » au
#' milieu d'un mot ; (*UCP) fait reconnaitre les lettres accentuees.
#' @keywords internal
glossaire_motif_global <- function() {
  m <- GLOSSAIRE$motif[!is.na(GLOSSAIRE$motif)]
  # « P − ETP » avant « ETP » : l'alternance retient la premiere forme qui convient
  m <- m[order(-nchar(m))]
  sprintf("(*UCP)(?i)(?<![[:alnum:]])(?:%s)(?![[:alnum:]])", paste(m, collapse = "|"))
}

#' Definition associee a une forme reperee dans un texte.
#' @keywords internal
glossaire_definition <- function(forme) {
  for (i in which(!is.na(GLOSSAIRE$motif))) {
    if (grepl(sprintf("(*UCP)(?i)^(?:%s)$", GLOSSAIRE$motif[i]), forme, perl = TRUE)) {
      return(GLOSSAIRE$definition[i])
    }
  }
  NA_character_
}

#' Texte dont les termes du glossaire portent une infobulle (<abbr>).
#'
#' Rend le texte tel quel s'il ne contient aucun terme. Chaque occurrence est
#' balisee : le texte lui-meme n'est jamais modifie, seulement enveloppe.
#' @param txt chaine unique.
#' @return une chaine, ou un htmltools::tagList.
#' @export
cf_terme <- function(txt) {
  txt <- as.character(txt %||% "")[1]
  if (is.na(txt) || !nzchar(txt)) return(txt)
  m <- gregexpr(glossaire_motif_global(), txt, perl = TRUE)
  trouvees <- regmatches(txt, m)[[1]]
  if (!length(trouvees)) return(txt)
  entre <- regmatches(txt, m, invert = TRUE)[[1]]
  morceaux <- list()
  deja <- character(0)
  for (i in seq_along(trouvees)) {
    morceaux <- c(morceaux, list(entre[i]))
    def <- glossaire_definition(trouvees[i])
    # Une definition n'est portee qu'une fois par texte : « Degres-jours de
    # croissance (GDD) » ne souligne pas deux fois la meme notion.
    morceaux <- c(morceaux, list(
      if (is.na(def) || def %in% deja) trouvees[i]
      else shiny::tags$abbr(class = "cf-terme", title = def, trouvees[i])))
    if (!is.na(def)) deja <- c(deja, def)
  }
  morceaux <- c(morceaux, list(entre[length(entre)]))
  do.call(htmltools::tagList, morceaux)
}

#' Carte « Glossaire » de l'onglet Sources.
#' @keywords internal
glossaire_ui <- function() {
  ordre <- order(iconv(GLOSSAIRE$terme, to = "ASCII//TRANSLIT"))
  shiny::div(
    class = "cf-carte", id = "glossaire",
    shiny::h3(class = "cf-carte__titre", "Glossaire"),
    shiny::div(class = "cf-aide", paste(
      "Les termes soulignés en pointillé dans les titres des graphiques portent",
      "leur définition en infobulle ; les voici tous, par ordre alphabétique.")),
    shiny::tags$dl(
      class = "cf-liste-def",
      unlist(lapply(ordre, function(i) {
        list(shiny::tags$dt(GLOSSAIRE$terme[i]), shiny::tags$dd(GLOSSAIRE$definition[i]))
      }), recursive = FALSE))
  )
}
