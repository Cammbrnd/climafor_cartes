# ==============================================================================
# utils_synthese_rendu.R — BILAN SYNTHETIQUE : TABLEAUX HTML, CSV, BILAN A EMPORTER (v2.0)
# ------------------------------------------------------------------------------
# Tout ce qui sort du bilan sans passer par un graphique Shiny :
#   - les tableaux HTML de la page (un seul constructeur pour tous) ;
#   - les CSV, avec leurs lignes de provenance ;
#   - le bilan a emporter : une page HTML AUTONOME (graphiques integres en image,
#     rien a telecharger a l'ouverture) qui s'imprime en PDF depuis le navigateur,
#     et sa conversion en Word.
#
# POURQUOI UNE PAGE HTML ET PAS UN RAPPORT rmarkdown. Un bilan court (une a trois
# pages) n'a pas besoin de la chaine de rendu du rapport complet : une page HTML
# s'ecrit en quelques centaines de millisecondes, sans processus a part, et pandoc
# la convertit en Word quand il est installe. Le meme texte, les memes tableaux, les
# memes images que ceux de la page : aucune seconde version a maintenir.
# ==============================================================================

#' Tableau HTML d'un data.frame de texte : la premiere colonne en en-tete de ligne,
#' defilement horizontal sur ecran etroit, legende pour les lecteurs d'ecran.
#' @param d data.frame (colonnes de texte) ; @param legende texte de la legende.
#' @keywords internal
synthese_table_tag <- function(d, legende = "") {
  if (is.null(d) || !nrow(d)) return(NULL)
  shiny::div(
    class = "cf-synt__defile", role = "region", tabindex = "0",
    `aria-label` = paste0(legende, " (défile horizontalement)"),
    shiny::tags$table(
      class = "cf-tableau cf-synt__table",
      shiny::tags$caption(class = "cf-sr-only", legende),
      shiny::tags$thead(shiny::tags$tr(lapply(names(d), function(n) shiny::tags$th(scope = "col", n)))),
      shiny::tags$tbody(lapply(seq_len(nrow(d)), function(i) {
        shiny::tags$tr(
          shiny::tags$th(scope = "row", as.character(d[i, 1L])),
          lapply(seq_len(ncol(d))[-1L], function(j) shiny::tags$td(as.character(d[i, j]))))
      }))))
}


# ------------------------------------------------------------------------------
# CSV
# ------------------------------------------------------------------------------

#' Libelle court de ce qui est analyse, pour les titres et les noms de fichiers.
#' @keywords internal
synthese_libelle <- function(res) {
  if (identical(res$echelle, "fiches")) {
    return(sprintf("%d fiche%s importée%s", nrow(res$fiches), if (nrow(res$fiches) > 1L) "s" else "",
                   if (nrow(res$fiches) > 1L) "s" else ""))
  }
  # Un point et ses sources : le nom du point, avec ce qu'on a regarde.
  if (identical(res$echelle, "point") && !is.null(res$point_nom) && !isTRUE(res$duo)) {
    ret <- res$tl[res$tl$retenu, , drop = FALSE]
    n_st <- sum(ret$source == "station")
    if (!n_st) return(res$point_nom)
    return(sprintf("%s (%s)", res$point_nom, paste(c(if (any(ret$source == "maille")) "maille SAFRAN",
      sprintf("%d station%s", n_st, if (n_st > 1L) "s" else "")), collapse = " + ")))
  }
  l <- res$tl$lieu[res$tl$retenu]
  if (length(l) <= 3L) paste(l, collapse = ", ") else sprintf("%s et %d autres territoires", paste(l[1:2], collapse = ", "), length(l) - 2L)
}

#' Lignes de provenance placees en tete d'un CSV du bilan.
#' @keywords internal
synthese_lignes_metadonnees <- function(res, titre) {
  l <- c(
    sprintf("%s %s — %s", CONFIG$app$nom, CONFIG$app$version, titre),
    sprintf("Source : mailles SAFRAN-ISBA précalculées (Météo-France), %d mailles de 8 km, jeu produit le %s",
            res$jeu$n_mailles, synthese_date_jeu(res$jeu$genere_le)),
    if (!is.null(res$jeu$stations)) sprintf(
      "Stations : mesures Météo-France (données climatologiques de base quotidiennes), indicateurs annuels précalculés le %s ; ETP estimée (Hargreaves-Samani)",
      synthese_date_jeu(res$jeu$stations$genere_le)),
    sprintf("Échelle : %s ; %s", unname(SYNTHESE_ECHELLES[res$echelle]), synthese_libelle(res)),
    switch(res$echelle,
      point = if (!is.null(res$jeu$stations)) "Valeur d'une source : maille la plus proche pour la maille SAFRAN (modèle), série de la station pour une station (mesure)"
              else "Valeur d'un point : maille la plus proche",
      fiches = "Valeur d'une fiche : maille la plus proche, pour l'année de la fiche",
      "Valeur d'un territoire : moyenne à poids égaux des mailles dont le centre est dans le contour"),
    "Seuils : valeurs par défaut du précalcul (les réglages de l'onglet Paramètres ne s'y appliquent pas)",
    sprintf("Référence : %s ; année comparée : %d", res$ctx$ref_label, res$annee),
    sprintf("Extraction : %s", format(Sys.Date(), "%d/%m/%Y")),
    "Licence : Licence ouverte Etalab 2.0 — Météo-France")
  gsub(" ?; ?", ", ", l)
}

#' Une ligne par fiche et par grandeur : la fiche, l'annee retenue, la valeur, la normale,
#' l'ecart et sa lecture.
#' @export
synthese_export_fiches <- function(res) {
  c1 <- res$contexte
  if (is.null(c1) || !nrow(c1)) return(data.frame())
  f <- res$fiches[match(c1$fiche, res$fiches$id), , drop = FALSE]
  data.frame(
    # Les textes viennent d'un fichier importe : un tableur lirait « =... » comme une formule.
    fiche = cmp_texte_csv_sur(c1$fiche), nom = cmp_texte_csv_sur(f$nom),
    longitude = round(f$lon, 5), latitude = round(f$lat, 5),
    date = ifelse(is.na(f$date), "", format(f$date, "%Y-%m-%d")),
    essence = cmp_texte_csv_sur(ifelse(is.na(f$essence), "", f$essence)),
    probleme = cmp_texte_csv_sur(ifelse(is.na(f$probleme), "", f$probleme)),
    grandeur = vapply(c1$cle, function(k) CARTE_COUCHES[[k]]$titre, ""),
    unite = vapply(c1$cle, function(k) CARTE_COUCHES[[k]]$unite, ""),
    annee_retenue = c1$annee, valeur = round(c1$valeur, 3), normale = round(c1$normale, 3),
    ecart = round(c1$ecart, 3),
    lecture = c(haut = "au-dessus", bas = "en dessous", neutre = "proche de la normale")[c1$classe],
    stringsAsFactors = FALSE, row.names = NULL)
}

#' Le tableau principal du bilan, en CSV : territoires et point (une ligne par lieu et
#' grandeur) ou fiches (une ligne par fiche et grandeur).
#' @export
synthese_ecrire_csv <- function(res, file) {
  d <- if (identical(res$echelle, "fiches")) synthese_export_fiches(res)
       else cmp_export_comparaison(res$bilan[res$bilan$cle %in% res$cles, , drop = FALSE], res$tl, res$annee)
  ecrire_csv_avec_metadonnees(d, file, synthese_lignes_metadonnees(res, "Bilan synthétique"))
}

#' Les series annuelles (territoires et point), en CSV.
#' @export
synthese_ecrire_csv_series <- function(res, file) {
  ecrire_csv_avec_metadonnees(cmp_export_series(res$series), file,
                              synthese_lignes_metadonnees(res, "Séries annuelles du bilan synthétique"))
}


# ------------------------------------------------------------------------------
# Bilan a emporter (HTML autonome, Word)
# ------------------------------------------------------------------------------

#' Image PNG d'un graphique, en adresse `data:` : integree a la page, sans fichier a part.
#' @keywords internal
synthese_png_donnees <- function(p, largeur = 7.5, hauteur = 3.9, dpi = 110) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)
  ggplot2::ggsave(f, plot = p, width = largeur, height = hauteur, dpi = dpi, bg = "white")
  paste0("data:image/png;base64,", gsub("[\r\n]", "", jsonlite::base64_enc(readBin(f, "raw", file.size(f)))))
}

SYNTHESE_STYLE_HTML <- "
body{font:15px/1.5 'Open Sans',Arial,sans-serif;color:#1E2A23;max-width:880px;margin:2rem auto;padding:0 1rem}
h1{font-size:1.5rem;color:#00664B;margin:0 0 .25rem}
h2{font-size:1.15rem;margin:1.8rem 0 .5rem;border-bottom:2px solid #D9DFD6;padding-bottom:.2rem}
h3{font-size:1rem;margin:1.2rem 0 .3rem}
.meta{color:#56635B;font-size:.85rem;margin:0 0 1rem}
.bref{background:#F3F6F1;border-left:4px solid #00664B;padding:.6rem 1rem;margin:.5rem 0}
.bref p{margin:.25rem 0}
table{border-collapse:collapse;width:100%;font-size:.85rem;margin:.5rem 0}
th,td{border:1px solid #D9DFD6;padding:.3rem .5rem;text-align:left;vertical-align:top}
thead th{background:#EAF0E8}
img{max-width:100%;height:auto;display:block;margin:.4rem 0}
ul{margin:.3rem 0 .3rem 1.2rem;padding:0}
.lecture{font-size:.85rem;color:#56635B}
.limites{font-size:.85rem;color:#56635B}
section{break-inside:avoid}
@media print{body{margin:0;max-width:none}}
"

#' Table HTML simple (sans Shiny) d'un data.frame de texte.
#' @keywords internal
synthese_table_simple <- function(d) {
  if (is.null(d) || !nrow(d)) return(NULL)
  htmltools::tags$table(
    htmltools::tags$thead(htmltools::tags$tr(lapply(names(d), function(n) htmltools::tags$th(scope = "col", n)))),
    htmltools::tags$tbody(lapply(seq_len(nrow(d)), function(i)
      htmltools::tags$tr(htmltools::tags$th(scope = "row", as.character(d[i, 1L])),
                         lapply(seq_len(ncol(d))[-1L], function(j) htmltools::tags$td(as.character(d[i, j])))))))
}

#' Le bilan a emporter : une page HTML autonome.
#'
#' @param res synthese_preparer() ; @param file fichier a ecrire.
#' @param blocs_fiches repartitions de fiches a inclure (parmi names(SYNTHESE_BLOCS_FICHES)).
#' @return `file`, invisiblement.
#' @export
synthese_ecrire_html <- function(res, file, blocs_fiches = character(0)) {
  fiches <- identical(res$echelle, "fiches")
  titre <- sprintf("Bilan climatique %d — %s", res$annee, synthese_libelle(res))
  generales <- synthese_phrases_generales(res)

  tableau <- if (fiches) synthese_tableau_resume_fiches(res) else synthese_tableau(res)
  tableau_f <- if (fiches && length(res$cles_t)) synthese_tableau_fiches(res) else NULL

  graphes <- lapply(res$cles_g, function(cle) {
    k <- CARTE_COUCHES[[cle]]
    htmltools::tags$section(
      htmltools::tags$h3(k$titre),
      htmltools::tags$ul(lapply(synthese_phrases_cle(res, cle), htmltools::tags$li)),
      htmltools::tags$img(src = synthese_png_donnees(synthese_graphe(res, cle)),
                          alt = sprintf("Graphique : %s. %s", k$titre, paste(synthese_phrases_cle(res, cle), collapse = " "))),
      htmltools::tags$p(class = "lecture", synthese_lecture_cle(cle)))
  })
  blocs <- lapply(intersect(blocs_fiches, names(SYNTHESE_BLOCS_FICHES)), function(b) {
    col <- sub("^f_", "", b)
    htmltools::tags$section(
      htmltools::tags$h3(SYNTHESE_BLOCS_FICHES[[b]]),
      htmltools::tags$img(src = synthese_png_donnees(synthese_graphe(res, b), hauteur = 3.2),
                          alt = SYNTHESE_BLOCS_FICHES[[b]]),
      synthese_table_simple(synthese_tableau_repartition(res, col)))
  })

  # Le <head> s'ecrit a la main : as.character() d'une balise <html> qui contient tags$head()
  # le JETTE (htmltools le reserve a renderTags()), et la page perdrait son jeu de
  # caracteres, son titre et son style.
  tete <- paste0(
    "<head>\n<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", htmltools::htmlEscape(titre), "</title>\n",
    "<style>", SYNTHESE_STYLE_HTML, "</style>\n</head>\n")
  doc <- htmltools::tagList(
    htmltools::tags$body(
      htmltools::tags$h1(titre),
      htmltools::tags$p(class = "meta", sprintf(
        "%s %s · échelle : %s · référence %s · établi le %s", CONFIG$app$nom, CONFIG$app$version,
        tolower(unname(SYNTHESE_ECHELLES[res$echelle])), res$ctx$ref_label, format(Sys.Date(), "%d/%m/%Y"))),
      htmltools::tags$h2("En bref"),
      htmltools::tags$div(class = "bref", lapply(generales, htmltools::tags$p)),
      if (!is.null(tableau)) htmltools::tagList(
        htmltools::tags$h2(if (fiches) "Contexte climatique des fiches" else "Tableau de synthèse"),
        synthese_table_simple(tableau)),
      if (length(graphes) || length(blocs)) htmltools::tags$h2("Graphiques"),
      graphes, blocs,
      if (!is.null(tableau_f)) htmltools::tagList(htmltools::tags$h2("Fiches"), synthese_table_simple(tableau_f)),
      htmltools::tags$h2("Méthode et limites"),
      htmltools::tags$div(class = "limites", lapply(synthese_methode(res), htmltools::tags$p)),
      htmltools::tags$p(class = "meta", sprintf("%s — Licence ouverte Etalab 2.0 (Météo-France).", CONFIG$app$organisme))))

  writeLines(paste0("<!doctype html>\n<html lang=\"fr\">\n", tete, as.character(doc), "\n</html>"),
             file, useBytes = TRUE)
  invisible(file)
}

#' Le bilan a emporter en Word : la page HTML convertie par pandoc.
#' @return `file`, invisiblement ; erreur si pandoc est absent.
#' @export
synthese_ecrire_docx <- function(res, file, blocs_fiches = character(0)) {
  if (!requireNamespace("rmarkdown", quietly = TRUE) || !rmarkdown::pandoc_available()) {
    stop("pandoc absent : conversion en Word impossible")
  }
  h <- tempfile(fileext = ".html")
  on.exit(unlink(h), add = TRUE)
  synthese_ecrire_html(res, h, blocs_fiches)
  # Pandoc reprend le <title> comme titre du document EN PLUS du <h1> : le titre sortirait
  # deux fois dans le Word. On retire le <title> de la copie temporaire.
  l <- readLines(h, encoding = "UTF-8", warn = FALSE)
  writeLines(sub("<title>.*</title>", "", l), h, useBytes = TRUE)
  rmarkdown::pandoc_convert(h, to = "docx", output = file, from = "html")
  invisible(file)
}
