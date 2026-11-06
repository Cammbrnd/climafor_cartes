# ==============================================================================
# utils_rapport.R — FONCTIONS COMMUNES AU RAPPORT ET AUX EXPORTS
# ------------------------------------------------------------------------------
# Ce qui sert a la fois au rapport (rapport/rapport_climat.Rmd), aux exports CSV
# des graphiques (mod_graphique.R) et a la generation en arriere-plan
# (mod_rapport.R, rapport/generer_rapport.R) :
#
#   1. ASSAINISSEMENT d'un texte libre avant insertion dans le document ;
#   2. NIVEAUX de detail, valides une fois pour toutes ;
#   3. PARAMETRES de l'analyse : source, periode, seuils modifies, versions —
#      de quoi retrouver un resultat a l'identique ;
#   4. METADONNEES des CSV ;
#   5. GENERATION du rapport, en processus separe ou en direct.
#
# Aucune fonction ici ne connait Shiny (sauf pour l'appel depuis le module).
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Assainissement d'un texte libre
# ------------------------------------------------------------------------------
# Le champ « contexte de la demande » etait insere TEL QUEL dans un document
# Markdown (v11) : une balise <script> ou un lien javascript: saisis dans le
# champ se retrouvaient dans le fichier HTML. Le rapport est fait pour etre
# transmis a d'autres personnes : son contenu ne doit pas dependre de ce qu'une
# seule a tape dans une zone de texte.
#
# Methode : le texte est traite comme du TEXTE, jamais comme du balisage. Chaque
# signe de ponctuation ASCII est precede d'une barre oblique inverse, ce que
# Pandoc lit comme « ce caractere, litteralement ». Ni Markdown ni HTML ni LaTeX
# brut ne peuvent alors etre interpretes, quelle que soit l'extension active.

#' Echappe un texte pour l'inserer litteralement dans du Markdown Pandoc.
#' Vectorisee : un element en entree, un element en sortie. Les retours a la
#' ligne simples sont conserves comme sauts de ligne.
#' @export
echapper_md <- function(txt) {
  # Parentheses et crochets ne sont PAS echappes par une barre oblique : rmarkdown
  # active l'extension tex_math_single_backslash de Pandoc, pour laquelle « \\( »
  # ouvre une formule (rendue par MathJax, charge depuis un CDN). Ils sont emis
  # en references numeriques HTML, que Pandoc lit comme le caractere litteral.
  marques <- c("(" = "\uE000", ")" = "\uE001", "[" = "\uE002", "]" = "\uE003")
  entites <- c("\uE000" = "&#40;", "\uE001" = "&#41;", "\uE002" = "&#91;", "\uE003" = "&#93;")
  un <- function(x) {
    if (is.na(x)) return("")
    # Caracteres de controle (hors retour a la ligne et tabulation) et zone
    # d'usage prive (reservee aux marques ci-dessus) : supprimes.
    x <- gsub("[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F\\x7F\uE000-\uE003]", "", x, perl = TRUE)
    x <- gsub("\r\n?", "\n", x)
    x <- gsub("\n{3,}", "\n\n", x)
    x <- gsub("[ \t]+\n", "\n", x)
    for (k in names(marques)) x <- gsub(k, marques[[k]], x, fixed = TRUE)
    # Autre ponctuation ASCII : precedee d'une barre oblique inverse, que Pandoc
    # lit comme « ce caractere, litteralement ».
    x <- gsub("([!-/:-@\\[-`{-~])", "\\\\\\1", x, perl = TRUE)
    for (k in names(entites)) x <- gsub(k, entites[[k]], x, fixed = TRUE)
    # Un seul retour a la ligne : saut de ligne explicite (sinon Pandoc le fond).
    gsub("(?<!\n)\n(?!\n)", "\\\\\n", x, perl = TRUE)
  }
  txt <- as.character(txt %||% "")
  if (!length(txt)) return(character(0))
  vapply(txt, un, character(1), USE.NAMES = FALSE)
}

#' Texte de contexte borne a la reception (audit v1.7, V8).
#'
#' Le champ de saisie n'impose rien cote serveur : un client peut envoyer un
#' texte de plusieurs Mo, que l'application copierait ensuite dans le fichier
#' d'entree du rapport. On garde au plus `max + 1` caracteres : un de plus que la
#' limite, pour que nettoyer_contexte() constate encore le depassement et le
#' signale dans le rapport.
#' @return une chaine (jamais NA ni vecteur).
#' @keywords internal
contexte_borne <- function(txt, max = CONFIG$rapport$contexte_max) {
  if (is.null(txt) || is.list(txt) || length(txt) < 1L) return("")
  txt <- as.character(txt)[1]
  if (is.na(txt)) return("")
  substr(txt, 1L, as.integer(max) + 1L)
}

#' Texte de contexte saisi par l'utilisateur, pret a inserer.
#' @param max nombre maximal de caracteres conserves.
#' @return list(texte = chaine echappee, tronque = logique).
#' @export
nettoyer_contexte <- function(txt, max = CONFIG$rapport$contexte_max) {
  txt <- trimws(as.character(txt %||% "")[1])
  if (is.na(txt)) txt <- ""
  tronque <- nchar(txt) > max
  if (tronque) txt <- substr(txt, 1L, max)
  list(texte = echapper_md(txt), tronque = tronque)
}


# ------------------------------------------------------------------------------
# 2. Niveaux de detail
# ------------------------------------------------------------------------------

#' Valide un niveau de detail, ou echoue en le disant.
#' Une valeur inconnue (« standard »...) retombait silencieusement sur le niveau
#' ingenieur en v11 : un rapport different de celui demande, sans un mot.
#' @export
valider_niveau <- function(niveau) {
  niveaux <- CONFIG$rapport$niveaux
  if (length(niveau) != 1L || !is.character(niveau) || !niveau %in% niveaux) {
    stop(sprintf("Niveau de rapport inconnu : « %s ». Valeurs admises : %s.",
                 paste(niveau, collapse = ", "), paste(niveaux, collapse = ", ")),
         call. = FALSE)
  }
  niveau
}

#' Le niveau demande atteint-il au moins `minimum` ? decideur < ingenieur < technique.
#' @export
niveau_au_moins <- function(niveau, minimum) {
  niveaux <- CONFIG$rapport$niveaux
  match(valider_niveau(niveau), niveaux) >= match(valider_niveau(minimum), niveaux)
}


# ------------------------------------------------------------------------------
# 3. Parametres de l'analyse — ce qui rend un rapport reproductible
# ------------------------------------------------------------------------------

#' Description de la source des donnees, en une phrase.
#' @export
texte_source <- function(don) {
  st <- attr(don, "station")
  if (identical(attr(don, "source"), "station") && !is.null(st)) {
    return(sprintf("station Météo-France %s (n° %s), à %s du point, altitude %s",
                   st$nom, st$id, mf_km(st$distance_km %||% NA_real_),
                   if (is.finite(st$alt %||% NA_real_)) sprintf("%.0f m", st$alt) else "inconnue"))
  }
  alt <- attr(don, "altitude_point")
  paste0("réanalyse SAFRAN-ISBA (Météo-France), maille de 8 km",
         if (!is.null(alt) && is.finite(alt))
           sprintf(" ; altitude du point : %.0f m (l'altitude moyenne de la maille n'est pas fournie par la source)", alt)
         else "")
}

#' Ligne « Source » au pied d'un export image : source, licence, version, date.
#' @export
ligne_source <- function(don) {
  src <- if (!is.null(don) && is.data.frame(don)) texte_source(don) else "Météo-France"
  fin <- if (!is.null(don) && is.data.frame(don))
    tryCatch(as.Date(attr(don, "derniere_mesure") %||% max(don$date)), error = function(e) NA)
  else NA
  sprintf("Source : %s — Licence ouverte Etalab 2.0 — %s %s — extrait le %s%s",
          src, CONFIG$app$nom, CONFIG$app$version, format(Sys.Date(), "%d/%m/%Y"),
          if (length(fin) && !is.na(fin)) sprintf(" (données jusqu'au %s)", format(fin, "%d/%m/%Y")) else "")
}

#' Libelles des seuils : ceux de l'onglet Parametres, sinon le nom technique.
#' @keywords internal
libelle_seuil <- function(id) {
  i <- match(id, DESCRIPTIF_SEUILS$id)
  if (is.na(i)) id else DESCRIPTIF_SEUILS$libelle[i]
}

#' Tableau des seuils retenus, compares aux valeurs par defaut.
#' @return data.frame(Groupe, Paramètre, Valeur, `Valeur par défaut`, Modifié).
#' @export
tableau_seuils <- function(ctx) {
  ref <- CONFIG$seuils
  ids <- names(ref)
  val <- vapply(ids, function(i) as.numeric((ctx$seuils %||% ref)[[i]] %||% NA_real_), numeric(1))
  def <- vapply(ids, function(i) as.numeric(ref[[i]]), numeric(1))
  grp <- vapply(ids, function(i) {
    j <- match(i, DESCRIPTIF_SEUILS$id)
    if (is.na(j)) "Autres" else DESCRIPTIF_SEUILS$groupe[j]
  }, character(1))
  data.frame(
    Groupe = unname(grp),
    `Paramètre` = vapply(ids, libelle_seuil, character(1), USE.NAMES = FALSE),
    Valeur = unname(val), `Valeur par défaut` = unname(def),
    `Modifié` = !mapply(function(a, b) isTRUE(all.equal(a, b)), val, def),
    check.names = FALSE, stringsAsFactors = FALSE)
}

#' Seuils modifies par rapport aux valeurs par defaut, en une phrase.
#' @export
texte_seuils_modifies <- function(ctx) {
  t <- tableau_seuils(ctx)
  m <- t[t$`Modifié`, , drop = FALSE]
  if (!nrow(m)) return("aucun : tous les seuils ont leur valeur par défaut")
  paste(sprintf("%s = %s (par défaut %s)", m$`Paramètre`,
                format(m$Valeur, decimal.mark = ","),
                format(m$`Valeur par défaut`, decimal.mark = ",")),
        collapse = " ; ")
}

#' Versions du logiciel et des paquets qui determinent le resultat.
#' @export
versions_logiciels <- function() {
  pq <- c("ggplot2", "plotly", "rmarkdown", "knitr", "shiny", "httr2", "jsonlite")
  v <- vapply(pq, function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) "non installé")
  }, character(1))
  c(R = paste(R.version$major, R.version$minor, sep = "."), v)
}

#' Tout ce qu'il faut pour refaire l'analyse a l'identique.
#' @export
parametres_analyse <- function(ctx, don) {
  list(
    application = sprintf("%s %s", CONFIG$app$nom, CONFIG$app$version),
    date        = Sys.Date(),
    source      = texte_source(don),
    reference   = ctx$ref_label,
    courante    = ctx$courante,
    affichage   = sprintf("%d-%d", min(ctx$affichage), max(ctx$affichage)),
    fenetre     = libelle_fenetre(ctx$fenetre),
    seuils      = tableau_seuils(ctx),
    seuils_modifies = texte_seuils_modifies(ctx),
    etp         = attr(don, "etp_source"),
    definition_tmoy = attr(don, "definition_tmoy"),
    derniere_date = tryCatch(as.Date(attr(don, "derniere_mesure") %||% max(don$date)),
                             error = function(e) as.Date(NA)),
    seuil_annee_station = if (identical(attr(don, "source"), "station"))
      100 * CONFIG$stations$seuil_annee else NA_real_,
    tendance    = sprintf(
      "pente de Sen, test de Mann-Kendall corrigé de l'autocorrélation d'ordre 1, intervalle de confiance à %.0f %%, au moins %d années complètes",
      100 * CONFIG$tendance$niveau, CONFIG$tendance$n_min),
    ecart_neutre = sprintf(
      "un écart inférieur à %s écart-type de la référence n'est pas coloré",
      format(CONFIG$tendance$bande_neutre, decimal.mark = ",")),
    versions    = versions_logiciels()
  )
}


# ------------------------------------------------------------------------------
# 4. Metadonnees des exports CSV
# ------------------------------------------------------------------------------

#' Lignes de provenance placees en tete d'un CSV.
#'
#' Un CSV qui circule seul perd son contexte : de quelle source, quelle
#' reference, quels seuils, a quelle date ? Les lignes commencent par « # »
#' (R : read.csv2(f, comment.char = "#", fileEncoding = "UTF-8-BOM") ; Excel les
#' affiche en tete de feuille).
#' Elles ne contiennent aucun point-virgule, separateur de colonnes.
#' @param ind entree du registre ; @param dat resultat de l'indicateur.
#' @export
lignes_metadonnees_csv <- function(ind, ctx, don = NULL, lieu = NULL, dat = NULL) {
  meta <- attr(dat, "meta")
  src <- if (!is.null(don)) texte_source(don) else {
    if (identical(meta$source, "station") && !is.null(meta$station))
      sprintf("station Météo-France %s", meta$station$nom) else "réanalyse SAFRAN-ISBA (Météo-France)"
  }
  fin <- if (!is.null(don)) tryCatch(as.Date(attr(don, "derniere_mesure") %||% max(don$date)),
                                     error = function(e) NA) else meta$derniere_date
  l <- c(
    sprintf("%s %s — %s", CONFIG$app$nom, CONFIG$app$version, ind$titre),
    sprintf("Lieu : %s", (lieu %||% list(nom = "non précisé"))$nom),
    sprintf("Source : %s", src),
    sprintf("Référence : %s ; année comparée : %d ; années affichées : %d-%d",
            ctx$ref_label, ctx$courante, min(ctx$affichage), max(ctx$affichage)),
    sprintf("Seuils modifiés : %s", texte_seuils_modifies(ctx)),
    if (isTRUE(ind$fenetre)) sprintf("Fenêtre mensuelle : %s", libelle_fenetre(ctx$fenetre)),
    if (!is.null(meta$etp)) sprintf("ETP : %s", meta$etp),
    if (!is.null(don) && is.finite(attr(don, "altitude_point") %||% NA_real_))
      sprintf("Altitude du point : %.0f m", attr(don, "altitude_point")),
    if (!is.null(attr(don, "definition_tmoy")))
      sprintf("Température moyenne journalière : %s", attr(don, "definition_tmoy")),
    sprintf("Extraction : %s%s", format(Sys.Date(), "%d/%m/%Y"),
            if (length(fin) && !is.na(fin)) sprintf(" ; dernière donnée : %s", format(fin, "%d/%m/%Y")) else ""),
    "Colonne partielle : TRUE si l'année (ou la saison) est incomplète ; sa valeur n'est pas comparée à la normale",
    "Colonne n_manquants : jours sans mesure, non compensés (cumuls et comptages sous-estimés)",
    "Licence : Licence ouverte Etalab 2.0 — Météo-France"
  )
  gsub(" ?; ?", ", ", Filter(Negate(is.null), l))
}

#' Ecrit un CSV precede de ses lignes de provenance, lisible dans Excel (UTF-8
#' avec marque d'ordre des octets, separateur « ; », virgule decimale).
#' @export
ecrire_csv_avec_metadonnees <- function(d, file, lignes = character(0)) {
  corps <- tempfile(fileext = ".csv")
  on.exit(unlink(corps), add = TRUE)
  utils::write.table(d, corps, sep = ";", dec = ",", row.names = FALSE,
                     na = "", fileEncoding = "UTF-8")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), file)
  if (length(lignes)) {
    con <- file(file, open = "ab")
    writeLines(enc2utf8(paste0("# ", lignes)), con, useBytes = TRUE)
    close(con)
  }
  file.append(file, corps)
  invisible(file)
}

#' Nom d'un fichier d'export : sans accent ni espace, avec le lieu et la date.
#' @param prefixe debut du nom ; @param lieu liste(nom = ...) ou NULL ; @param ext extension.
#' @export
nom_fichier_export <- function(prefixe, lieu = NULL, ext = "csv") {
  nom <- iconv((lieu %||% list(nom = "point"))$nom %||% "point", to = "ASCII//TRANSLIT")
  nom <- gsub("^_+|_+$", "", gsub("[^A-Za-z0-9]+", "_", if (is.na(nom)) "point" else nom))
  sprintf("%s_%s_%s.%s", prefixe, if (nzchar(nom)) nom else "point",
          format(Sys.Date(), "%Y%m%d"), ext)
}

#' Toutes les series ANNUELLES du bilan dans un seul tableau « long ».
#'
#' Une ligne par indicateur, grandeur et annee : le format que lisent sans effort
#' R (`read.csv2`), Excel (tableau croise dynamique) et tout outil de graphiques.
#' Les profils mensuels et journaliers (bilan hydrique, diagramme ombrothermique,
#' normales par mois...) n'ont pas d'annee : ils restent dans le CSV de leur
#' graphique. Les colonnes de texte (classe de De Martonne) et les colonnes de
#' calcul sans interet pour la lecture (nombre de jours, dernier jour) sont
#' laissees de cote ; l'etat de completude, lui, est garde : `partielle`.
#'
#' @param resultats liste de calculer_indicateurs().
#' @param ids indicateurs a exporter, dans cet ordre.
#' @return data.frame(indicateur_id, indicateur, grandeur, annee, valeur, partielle),
#'   zero ligne si aucune serie annuelle n'est calculable.
#' @export
bilan_series_longues <- function(resultats, ids = ORDRE_INDICATEURS) {
  vide <- data.frame(indicateur_id = character(0), indicateur = character(0),
                     grandeur = character(0), annee = integer(0), valeur = numeric(0),
                     partielle = logical(0), stringsAsFactors = FALSE)
  morceaux <- lapply(ids, function(id) {
    d <- resultats[[id]]; ind <- INDICATEURS[[id]]
    if (is.null(d) || is.null(ind) || !nrow(d) || !"annee" %in% names(d)) return(NULL)
    cols <- setdiff(colonnes_valeurs(d, ind), c("annee", "partielle", "n_manquants",
                                                COLONNES_TECHNIQUES))
    cols <- cols[vapply(cols, function(c) is.numeric(d[[c]]), logical(1))]
    if (!length(cols)) return(NULL)
    part <- if ("partielle" %in% names(d)) d$partielle %in% TRUE else rep(FALSE, nrow(d))
    do.call(rbind, lapply(cols, function(c) {
      data.frame(indicateur_id = id, indicateur = ind$titre, grandeur = c,
                 annee = as.integer(d$annee), valeur = round(as.numeric(d[[c]]), 3),
                 partielle = part, stringsAsFactors = FALSE)
    }))
  })
  morceaux <- Filter(Negate(is.null), morceaux)
  if (!length(morceaux)) return(vide)
  res <- do.call(rbind, morceaux)
  rownames(res) <- NULL
  res
}

#' La synthese chiffree, telle que l'ecran la montre, prete a etre exportee.
#' @param s resultat de calc_synthese().
#' @return data.frame(indicateur, unite, valeur, normale, ecart, ecart_type_reference,
#'   annees_reference, lecture) ; `lecture` : « au-dessus », « en dessous » ou
#'   « proche de la normale » (une demi ecart-type de la reference), vide si
#'   l'indicateur n'a pas de ecart-type.
#' @export
bilan_synthese_export <- function(s) {
  if (is.null(s) || !nrow(s)) return(data.frame())
  lecture <- ifelse(!is.finite(s$ecart) | !is.finite(s$sd_ref), "",
             ifelse(s$classe == "haut", "au-dessus",
             ifelse(s$classe == "bas", "en dessous", "proche de la normale")))
  d <- data.frame(indicateur = s$indicateur, unite = ifelse(s$unite == "-", "", s$unite),
                  valeur = s$valeur, normale = s$normale, ecart = s$ecart,
                  ecart_type_reference = s$sd_ref, annees_reference = s$n_ref,
                  lecture = lecture, stringsAsFactors = FALSE)
  num <- vapply(d, is.double, logical(1))
  d[num] <- lapply(d[num], round, 3)
  d
}


# ------------------------------------------------------------------------------
# 4b. Tableau des valeurs d'un indicateur (ecran et rapport)
# ------------------------------------------------------------------------------

# Colonnes de calcul qui n'ont pas leur place dans un tableau de lecture : elles
# restent dans le CSV.
COLONNES_TECHNIQUES <- c("n_jours", "attendu", "couverts", "dernier_jour", "ecart_type_ref")

#' Colonnes montrees dans le tableau des valeurs : celles du registre (ou, a
#' defaut, toutes sauf les colonnes de calcul), plus l'etat de completude.
#' @export
colonnes_valeurs <- function(d, ind) {
  cols <- if (!is.null(ind$colonnes)) intersect(ind$colonnes, names(d))
          else setdiff(names(d), COLONNES_TECHNIQUES)
  cols <- union(cols, intersect(c("partielle", "n_manquants"), names(d)))
  if (length(cols)) cols else names(d)
}

#' Valeurs d'un indicateur en texte : virgule decimale, oui/non, dates en clair.
#' @param limite nombre maximal de lignes ; @param dernieres garde les dernieres
#'   plutot que les premieres.
#' @return data.frame de chaines ; attribut "tronque" si des lignes ont ete omises.
#' @export
valeurs_formatees <- function(dat, ind, ctx, limite = 400L, dernieres = FALSE) {
  if (!is.data.frame(dat) || !nrow(dat)) return(NULL)
  d <- dat[, colonnes_valeurs(dat, ind), drop = FALSE]
  if ("annee" %in% names(d)) d <- d[d$annee %in% ctx$affichage, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  tronque <- nrow(d) > limite
  if (tronque) d <- if (dernieres) utils::tail(d, limite) else utils::head(d, limite)

  cellule <- function(x) {
    if (inherits(x, "Date")) return(ifelse(is.na(x), "", format(x, "%d/%m/%Y")))
    if (is.logical(x)) return(ifelse(is.na(x), "", ifelse(x, "oui", "non")))
    if (is.numeric(x)) {
      ent <- all(is.na(x) | x == round(x))
      return(ifelse(is.na(x), "",
                    format(if (ent) x else round(x, 2), decimal.mark = ",",
                           trim = TRUE, scientific = FALSE, big.mark = "")))
    }
    ifelse(is.na(x), "", as.character(x))
  }
  out <- as.data.frame(lapply(d, cellule), stringsAsFactors = FALSE, optional = TRUE)
  names(out) <- names(d)
  rownames(out) <- NULL
  attr(out, "tronque") <- tronque
  out
}


# ------------------------------------------------------------------------------
# 5. Generation du rapport
# ------------------------------------------------------------------------------
# Un rapport complet prend de 6 a 16 secondes. En v11 la generation tournait
# DANS le processus Shiny : pendant ce temps, aucune autre session servie par ce
# processus ne repondait. Elle se fait desormais dans un processus R SEPARE
# (Rscript), le processus de l'application ne fait qu'attendre un fichier.
#
# Si le processus ne peut pas etre lance (Rscript introuvable, systeme de
# fichiers en lecture seule...), la generation retombe sur le mode direct, comme
# avant : un rapport lent vaut mieux qu'un rapport absent.

#' Taille d'un fichier en clair (« 1,2 Mo »).
#' @keywords internal
format_taille <- function(octets) {
  if (!is.finite(octets)) return("")
  if (octets < 1024^2) sprintf("%.0f ko", octets / 1024)
  else sub(".", ",", sprintf("%.1f Mo", octets / 1024^2), fixed = TRUE)
}

#' Racine du projet (dossier contenant global.R), depuis le repertoire courant.
#' @keywords internal
racine_projet <- function() normalizePath(getwd(), winslash = "/", mustWork = FALSE)

#' Copie le modele, sa feuille de style, le modele Word et les logos dans un
#' dossier de travail : knitr y ecrit ses fichiers intermediaires, et le dossier
#' de l'application peut etre en lecture seule.
#' @keywords internal
preparer_dossier_rapport <- function(dossier) {
  dir.create(dossier, showWarnings = FALSE, recursive = TRUE)
  file.copy(chemin_ci("rapport/rapport_climat.Rmd"),
            file.path(dossier, "rapport_climat.Rmd"), overwrite = TRUE)
  file.copy(chemin_ci("rapport/styles_rapport.css"),
            file.path(dossier, "styles_rapport.css"), overwrite = TRUE)
  file.copy(chemin_ci("rapport/modele_dsfr.docx"),
            file.path(dossier, "modele_dsfr.docx"), overwrite = TRUE)
  dir.create(file.path(dossier, "images"), showWarnings = FALSE)
  for (img in c("republique_francaise.png", "logo_dsf.png")) {
    src <- chemin_ci(file.path("rapport", "images", img))
    if (file.exists(src)) file.copy(src, file.path(dossier, "images", img), overwrite = TRUE)
  }
  invisible(dossier)
}

#' Genere le rapport dans `dossier` et rend le chemin du fichier produit.
#' @param entree list(donnees, resultats, ctx, lieu, indicateurs, niveau,
#'   contexte, interactif).
#' @param format "html" ou "docx".
#' @export
rendre_rapport <- function(dossier, entree, format = c("html", "docx")) {
  format <- match.arg(format)
  valider_niveau(entree$niveau)
  preparer_dossier_rapport(dossier)
  modele <- file.path(dossier, "rapport_climat.Rmd")

  sortie_format <- if (identical(format, "docx")) {
    # Le modele de reference porte les styles DSFR (Marianne, bleu France),
    # l'en-tete a logos et le pied de page (outils/construire_modele_docx.py).
    # Absent : mise en forme par defaut plutot qu'un echec.
    modele_word <- file.path(dossier, "modele_dsfr.docx")
    # --columns=400 : sans lui, pandoc juge « larges » les tableaux de plus de 72
    # caracteres et fixe les largeurs de colonnes d'apres le nombre de tirets de
    # la ligne de separation (« annee » en 5 % de la page, coupe en « anne / e »).
    # Avec une valeur haute, Word ajuste les colonnes a leur contenu.
    rmarkdown::word_document(
      toc = TRUE, pandoc_args = "--columns=400",
      reference_docx = if (file.exists(modele_word)) modele_word else NULL)
  } else {
    # theme = NULL est delibere : Bootstrap entrerait en conflit avec
    # styles_rapport.css (html { font-size: 10px } reduirait le document a
    # 62,5 %). Consequence : pas de toc_float (il depend de Bootstrap) ; le
    # sommaire est statique et rendu collant par la feuille de style.
    rmarkdown::html_document(
      toc = TRUE, toc_depth = 2, toc_float = FALSE, fig_retina = 1,
      # highlight = NULL : le rapport n'affiche aucun code ; la coloration
      # syntaxique embarquait 800 ko de JavaScript pour rien.
      highlight = NULL,
      # mathjax = NULL : aucune formule dans le rapport ; sans cela le fichier
      # appelait un script distant (CDN) a chaque ouverture, ce qui contredit
      # « autonome » et echoue hors ligne.
      mathjax = NULL,
      self_contained = TRUE, theme = NULL, css = "styles_rapport.css")
  }

  rmarkdown::render(
    input = modele,
    output_format = sortie_format,
    output_file = basename(tempfile(fileext = ".out")),
    output_dir = dossier,
    params = list(
      donnees     = entree$donnees,
      resultats   = entree$resultats,
      ctx         = entree$ctx,
      lieu        = entree$lieu,
      indicateurs = intersect(entree$indicateurs %||% ORDRE_INDICATEURS, names(INDICATEURS)),
      niveau      = entree$niveau,
      contexte    = entree$contexte %||% "",
      interactif  = isTRUE(entree$interactif) && !identical(format, "docx")
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
}

#' Lance la generation dans un processus R separe.
#' @return list(dossier, etat, journal, debut, format) ; NULL si le lancement a echoue.
#' @export
lancer_rapport_arriere_plan <- function(entree, format = "html") {
  rscript <- file.path(R.home("bin"),
                       if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  script <- chemin_ci("rapport/generer_rapport.R")
  if (!file.exists(rscript) || !file.exists(script)) return(NULL)

  dossier <- tempfile("rapport_")
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dossier)) return(NULL)
  etat <- file.path(dossier, "etat.txt")
  journal <- file.path(dossier, "journal.txt")
  ok <- tryCatch({
    saveRDS(entree, file.path(dossier, "entree.rds"))
    writeLines("en_cours", etat)
    system2(rscript,
            c(shQuote(script), shQuote(dossier), shQuote(format),
              shQuote(racine_projet())),
            wait = FALSE, stdout = journal, stderr = journal)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok)) return(NULL)
  list(dossier = dossier, etat = etat, journal = journal, format = format,
       debut = Sys.time())
}

#' Etat d'une generation en arriere-plan.
#' @return list(statut = "en_cours" | "ok" | "erreur", fichier, message).
#' @export
etat_rapport <- function(job) {
  if (is.null(job)) return(list(statut = "aucun", fichier = NULL, message = ""))
  lu <- tryCatch(readLines(job$etat, n = 1L, warn = FALSE), error = function(e) character(0))
  lu <- if (length(lu)) lu else "en_cours"
  if (startsWith(lu, "ok:")) {
    f <- substring(lu, 4L)
    if (file.exists(f)) return(list(statut = "ok", fichier = f, message = ""))
    return(list(statut = "erreur", fichier = NULL, message = "Le fichier produit a disparu."))
  }
  if (startsWith(lu, "erreur:")) {
    return(list(statut = "erreur", fichier = NULL, message = substring(lu, 8L)))
  }
  ecoule <- as.numeric(difftime(Sys.time(), job$debut, units = "secs"))
  if (ecoule > CONFIG$rapport$delai_max_s) {
    # Le processus est arrete : un rapport abandonne ne doit pas continuer a
    # occuper le serveur.
    pid <- suppressWarnings(tryCatch(
      as.integer(readLines(file.path(job$dossier, "pid.txt"), n = 1L, warn = FALSE)),
      error = function(e) NA_integer_))
    if (length(pid) == 1L && is.finite(pid)) try(tools::pskill(pid), silent = TRUE)
    return(list(statut = "erreur", fichier = NULL, message = sprintf(
      "La génération dépasse %d secondes : elle est abandonnée.", CONFIG$rapport$delai_max_s)))
  }
  list(statut = "en_cours", fichier = NULL, message = "", ecoule = ecoule)
}
