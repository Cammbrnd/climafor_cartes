# ==============================================================================
# utils_plot.R — MOTEUR GRAPHIQUE
# ------------------------------------------------------------------------------
# Deux niveaux :
#   1. theme_climafor() et les fabriques generiques (graphe_annuel, graphe_mensuel)
#      qui portent 90 % de la mise en forme ;
#   2. les fonctions plot_xxx(dat, ctx), tres courtes, qui se contentent de
#      declarer quelle colonne tracer et avec quel libelle.
#
# Ajouter un graphique se resume donc a ecrire un calc_xxx() et un plot_xxx()
# de quelques lignes, puis a les declarer dans registre_indicateurs.R.
# ==============================================================================

CL <- CONFIG$couleurs   # raccourci de lecture


# ------------------------------------------------------------------------------
# 1. Theme graphique
# ------------------------------------------------------------------------------

#' Theme ggplot2 de ClimaFor : sobriete, grille horizontale discrete, pas de
#' cadre, typographie Open Sans avec repli systeme.
#' @export
theme_climafor <- function(base_size = 13, mobile = FALSE) {
  taille <- if (mobile) base_size - 2 else base_size
  ggplot2::theme_minimal(base_size = taille) +
    ggplot2::theme(
      text             = ggplot2::element_text(colour = CL$texte),
      plot.title       = ggplot2::element_text(face = "bold", size = taille + 1,
                                               colour = CL$texte, margin = ggplot2::margin(b = 4)),
      plot.subtitle    = ggplot2::element_text(size = taille - 2, colour = CL$texte_mention,
                                               margin = ggplot2::margin(b = 8)),
      plot.caption     = ggplot2::element_text(size = taille - 3, colour = CL$texte_mention,
                                               hjust = 0, margin = ggplot2::margin(t = 8)),
      axis.title       = ggplot2::element_text(size = taille - 2, colour = CL$texte_mention),
      axis.text        = ggplot2::element_text(size = taille - 2, colour = CL$texte),
      panel.grid.major.y = ggplot2::element_line(colour = CL$bordure, linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_blank(),
      legend.text      = ggplot2::element_text(size = taille - 2),
      legend.key.height = grid::unit(10, "pt"),
      plot.margin      = ggplot2::margin(8, 12, 8, 8)
    )
}

#' Graphique de repli affiche lorsqu'un indicateur n'a pas pu etre calcule.
#' Un ecran vide n'informe pas : on explique ce qui manque.
#' @export
graphe_message <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message,
                      size = 4, colour = CL$texte_mention, hjust = 0.5) +
    ggplot2::theme_void()
}

#' Phrase expliquant POURQUOI un indicateur est vide.
#'
#' Trois causes que la v11 affichait toutes comme « Données indisponibles » :
#' la source ne mesure pas la variable, aucune annee n'est assez renseignee, ou
#' le calcul a echoue. Elles n'appellent pas la meme reaction de l'utilisateur.
#' La phrase est posee en attribut "message" par calculer_un_indicateur().
#' @export
message_indisponible <- function(dat, defaut = "Aucune donnée pour cet indicateur sur la période choisie") {
  m <- attr(dat, "message")
  if (is.null(m) || !nzchar(m)) defaut else m
}


# ------------------------------------------------------------------------------
# 1b. Notes de bas de graphique
# ------------------------------------------------------------------------------
# Tout ce qui change la lecture d'un graphique sans etre sur le graphique y est
# dit ici : annee incomplete, annees sans valeur, jours sans mesure, normale
# calculee sur une partie de la reference, source de l'ETP, tendance et limites.
# Les notes sont portees par le `caption` du ggplot, une note par ligne. Le
# rendu interactif les perd (ggplotly ne garde pas la legende) : le module les
# reaffiche sous le graphique, comme les sous-titres.

#' Date en francais, « 27 octobre ».
#' @keywords internal
date_fr <- function(d) {
  d <- as.Date(d)
  sprintf("%d %s", as.integer(format(d, "%d")), MOIS_LONG[as.integer(format(d, "%m"))])
}

#' Notes d'un graphique annuel.
#' @param d lignes affichees (colonnes partielle, n_manquants, dernier_jour).
#' @param meta metadonnees de la source (attribut "meta" des resultats).
#' @param ref_n c(utilisees, attendues) : annees de reference effectivement retenues.
#' @param tend resultat de tendance(), ou NULL.
#' @param etp TRUE pour les indicateurs qui reposent sur l'ETP.
#' @param periode "année" ou "saison" (indicateurs saisonniers).
#' @return vecteur de phrases (character(0) s'il n'y a rien a signaler).
#' @keywords internal
notes_annuelles <- function(d, ctx, meta = NULL, ref_n = NULL, tend = NULL,
                            etp = FALSE, periode = "année") {
  notes <- character(0)
  station <- identical(meta$source, "station")

  inc <- d[d$partielle %in% TRUE, , drop = FALSE]
  for (i in seq_len(nrow(inc))) {
    fin <- inc$dernier_jour[i]
    notes <- c(notes, sprintf(
      "%d : %s incomplète%s. Non comparée à la normale (en gris), exclue de la moyenne mobile et de la tendance.",
      inc$annee[i], periode,
      if (length(fin) && !is.na(fin)) sprintf(" (données jusqu'au %s)", date_fr(fin)) else ""))
  }

  ex <- sort(intersect(meta$annees_exclues, ctx$affichage))
  if (length(ex)) {
    notes <- c(notes, sprintf(
      "Sans valeur (moins de %.0f %% des jours renseignés) : %s.",
      100 * CONFIG$stations$seuil_annee, paste(ex, collapse = ", ")))
  }

  seuil_m <- CONFIG$stations$signaler_manquants_j %||% 3L
  mq <- d[d$n_manquants >= seuil_m & !(d$partielle %in% TRUE), , drop = FALSE]
  mq <- mq[is.finite(mq$n_manquants), , drop = FALSE]
  if (nrow(mq)) {
    liste <- sprintf("%d (%d j)", mq$annee, as.integer(mq$n_manquants))
    if (length(liste) > 6) liste <- c(liste[1:6], sprintf("… +%d", length(liste) - 6))
    notes <- c(notes, sprintf(
      "Jours sans mesure, non compensés (cumuls et comptages sous-estimés) : %s.",
      paste(liste, collapse = ", ")))
  }

  if (!is.null(ref_n) && is.finite(ref_n[["attendues"]]) && ref_n[["attendues"]] > 0 &&
      ref_n[["utilisees"]] < ref_n[["attendues"]]) {
    notes <- c(notes, if (ref_n[["utilisees"]] == 0)
      "Normale non calculable : aucune année de la référence n'est complète."
      else sprintf("Normale %s calculée sur %d années complètes sur %d.",
                   ctx$ref_label, ref_n[["utilisees"]], ref_n[["attendues"]]))
  }

  if (isTRUE(etp)) notes <- c(notes, note_etp(meta))

  if (!is.null(tend)) {
    notes <- c(notes, tend$libelle)
    if (station && !is.null(tend$rupture)) {
      notes <- c(notes, sprintf(
        "Rupture de niveau possible vers %d (%s, %s) : vérifier un changement de capteur ou un déplacement de la station avant d'interpréter la tendance.",
        tend$rupture$annee, fmt_fr(tend$rupture$ecart, 2, signe = TRUE),
        fmt_p(tend$rupture$p)))
    }
  }
  notes
}

#' Pose les notes sur un ggplot (une par ligne dans le caption).
#' @keywords internal
avec_notes <- function(p, notes) {
  if (!length(notes)) return(p)
  p + ggplot2::labs(caption = paste(notes, collapse = "\n"))
}

#' Ajoute des notes a celles que le ggplot porte deja.
#' @keywords internal
ajouter_notes <- function(p, notes) {
  notes <- notes[nzchar(notes)]
  if (!inherits(p, "ggplot") || !length(notes)) return(p)
  cap <- p$labels$caption
  p + ggplot2::labs(caption = paste(c(if (!is.null(cap) && nzchar(cap)) cap, notes),
                                    collapse = "\n"))
}

#' Note sur l'origine de l'ETP (la source change la valeur : D7 de l'audit).
#' @keywords internal
note_etp <- function(meta) {
  if (is.null(meta$etp)) return(character(0))
  sprintf("ETP : %s.%s", meta$etp,
          if (identical(meta$source, "station"))
            " Sans vent ni humidité : non comparable à l'ETP de SAFRAN." else "")
}

#' Notes (une par element) d'un ggplot, telles que posees par avec_notes().
#' @export
graphe_notes <- function(p) {
  if (!inherits(p, "ggplot")) return(character(0))
  cap <- p$labels$caption
  if (is.null(cap) || !nzchar(cap)) return(character(0))
  strsplit(cap, "\n", fixed = TRUE)[[1]]
}

#' Replie les notes d'un ggplot sur plusieurs lignes (rendu en image).
#' @param largeur nombre de caracteres par ligne.
#' @export
graphe_notes_repliees <- function(p, largeur = 110) {
  n <- graphe_notes(p)
  if (!length(n)) return(p)
  p + ggplot2::labs(caption = paste(
    vapply(n, function(x) paste(strwrap(x, width = largeur), collapse = "\n"),
           character(1)), collapse = "\n"))
}

#' Ajoute une ligne « Source » au pied d'un ggplot (exports PNG autonomes).
#' @export
graphe_avec_source <- function(p, source) ajouter_notes(p, source %||% "")


# ------------------------------------------------------------------------------
# 2. Fabrique generique — series annuelles
# ------------------------------------------------------------------------------

#' Construit un graphique de serie annuelle standardise.
#'
#' Composition (chaque couche est optionnelle) :
#'   - barres ou points annuels, colores selon l'ecart a la normale ;
#'   - ligne horizontale de la normale de reference ;
#'   - moyenne mobile (sans l'annee incomplete) ;
#'   - tendance de Sen, avec son intervalle de confiance ;
#'   - liseré sur l'annee mise en avant.
#'
#' COLORATION DES ECARTS
#' ---------------------
#' Un ecart n'est colore que s'il depasse CONFIG$tendance$bande_neutre fois
#' l'ecart-type des annees de reference ; en deca, il est gris (« proche de la
#' normale »). La legende dit ce que les couleurs signifient, en mots.
#' L'annee INCOMPLETE est grise et nommee dans la legende : elle n'est comparee a
#' rien.
#'
#' @param dat data.frame contenant au minimum `annee` et la colonne `y`.
#' @param y Nom (chaine) de la colonne a representer.
#' @param titre_y Libelle de l'axe des ordonnees.
#' @param ctx Contexte de calcul (ctx_creer).
#' @param type "barres" ou "lignes".
#' @param sens "sec" | "chaud" | "neutre" : cote defavorable d'un ecart. Pour un
#'   cumul de pluie, un deficit est defavorable ; pour une temperature, c'est
#'   l'exces. "neutre" : aucune lecture favorable/defavorable, une seule couleur.
#' @param couleur Couleur unique lorsque `sens = "neutre"`.
#' @param lissage Nom de la colonne de moyenne mobile, ou NULL.
#' @param montrer_tendance Trace la pente de Sen et son intervalle de confiance.
#' @param unite_tendance,dec_tendance unite et decimales de la pente (decennie).
#' @param etp TRUE si l'indicateur repose sur l'ETP (la note en indique la source).
#' @param periode "année" ou "saison" pour la formulation des notes.
#' @export
graphe_annuel <- function(dat, y, titre_y, ctx,
                          type = c("barres", "lignes"),
                          sens = "neutre",
                          couleur = CL$annee_courante,
                          lissage = NULL,
                          montrer_tendance = FALSE,
                          unite_tendance = "", dec_tendance = 2,
                          etp = FALSE, periode = "année",
                          mobile = FALSE) {

  type <- match.arg(type)
  if (is.null(dat) || !nrow(dat) || !y %in% names(dat)) {
    return(graphe_message(message_indisponible(dat)))
  }
  meta  <- attr(dat, "meta")
  ref_n <- attr(dat, "ref_n")

  d <- dat[dat$annee %in% ctx$affichage, , drop = FALSE]
  if (!nrow(d)) return(graphe_message("Aucune année dans la plage sélectionnée"))
  d$val_y <- d[[y]]
  if (all(is.na(d$val_y))) return(graphe_message("Variable non renseignée sur la période"))
  if (!"partielle" %in% names(d)) d$partielle <- FALSE
  if (!"n_manquants" %in% names(d)) d$n_manquants <- 0L
  if (!"dernier_jour" %in% names(d)) d$dernier_jour <- as.Date(NA)
  d$partielle <- d$partielle %in% TRUE

  normale <- if ("normale" %in% names(d)) d$normale[1] else NA_real_

  # -- Classe de chaque annee ------------------------------------------------
  # haut / bas / neutre selon l'ecart a la normale, "partielle" pour l'annee
  # incomplete. Sans cote defavorable (sens "neutre") : une seule couleur.
  jugement <- sens %in% c("sec", "chaud")
  anomalie <- if ("anomalie" %in% names(d)) d$anomalie else d$val_y - normale
  sd_ref   <- if ("ecart_type_ref" %in% names(d)) d$ecart_type_ref else NA_real_
  classe   <- if (jugement) classe_ecart(anomalie, sd_ref) else rep("complete", nrow(d))
  classe[d$partielle] <- "partielle"

  lib <- c(haut = "Au-dessus de la normale", bas = "En dessous de la normale",
           neutre = "Proche de la normale", complete = "Année complète",
           partielle = "Année incomplète")
  pal <- c(haut   = if (sens == "chaud") CL$deficit else CL$excedent,
           bas    = if (sens == "chaud") CL$excedent else CL$deficit,
           neutre = CL$neutre, complete = couleur, partielle = CL$en_cours)
  presentes <- names(lib)[names(lib) %in% classe]
  d$classe_lib <- factor(lib[classe], levels = lib[presentes])
  valeurs <- stats::setNames(pal[presentes], lib[presentes])
  # Sans jugement ni annee incomplete, la legende n'apprendrait rien.
  guide <- if (!jugement && !any(d$partielle)) "none" else "legend"

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$annee, y = .data$val_y))

  if (type == "barres") {
    p <- p + ggplot2::geom_col(ggplot2::aes(fill = .data$classe_lib), width = 0.75,
                               na.rm = TRUE) +
      ggplot2::scale_fill_manual(values = valeurs, guide = guide)
  } else {
    # La ligne ne traverse pas l'annee incomplete : elle n'est pas du meme ordre.
    p <- p +
      ggplot2::geom_line(data = d[!d$partielle, , drop = FALSE],
                         colour = couleur, linewidth = 0.6, na.rm = TRUE) +
      ggplot2::geom_point(ggplot2::aes(colour = .data$classe_lib), size = 1.8,
                          na.rm = TRUE) +
      ggplot2::scale_colour_manual(values = valeurs, guide = guide)
  }

  # -- Normale de reference
  if (is.finite(normale)) {
    p <- p + ggplot2::geom_hline(yintercept = normale, colour = CL$reference,
                                 linetype = "dashed", linewidth = 0.5)
  }

  # -- Moyenne mobile (le calcul exclut deja l'annee incomplete)
  if (!is.null(lissage) && lissage %in% names(d)) {
    p <- p + ggplot2::geom_line(ggplot2::aes(y = .data[[lissage]]),
                                colour = CL$annee_courante, linewidth = 1, na.rm = TRUE)
  }

  # -- Tendance : pente de Sen + intervalle de confiance
  tend <- NULL
  if (isTRUE(montrer_tendance)) {
    tend <- tendance(d$annee, d$val_y, unite_tendance, exclure = d$partielle,
                     dec = dec_tendance)
    if (isTRUE(tend$ok)) {
      xs <- range(d$annee[!d$partielle & is.finite(d$val_y)])
      tr <- data.frame(annee = seq(xs[1], xs[2]))
      b <- tend$pente_dec / 10
      tr$y    <- tend$ordonnee + b * tr$annee
      tr$ymin <- tend$centre[2] + tend$pentes_ic[1] / 1 * (tr$annee - tend$centre[1])
      tr$ymax <- tend$centre[2] + tend$pentes_ic[2] / 1 * (tr$annee - tend$centre[1])
      p <- p +
        ggplot2::geom_ribbon(data = tr, ggplot2::aes(x = .data$annee, ymin = .data$ymin,
                                                     ymax = .data$ymax),
                             inherit.aes = FALSE, fill = CL$alerte, alpha = 0.13) +
        ggplot2::geom_line(data = tr, ggplot2::aes(x = .data$annee, y = .data$y),
                           inherit.aes = FALSE, colour = CL$alerte,
                           linewidth = 0.7)
    }
  }

  # -- Mise en avant de l'annee comparee
  if (ctx$courante %in% d$annee) {
    p <- p + ggplot2::geom_vline(xintercept = ctx$courante, colour = CL$annee_courante,
                                 linewidth = 0.4, alpha = 0.5)
  }

  p <- p +
    ggplot2::scale_x_continuous(breaks = graphe_ruptures_annees(d$annee, mobile)) +
    ggplot2::labs(x = NULL, y = titre_y, fill = NULL, colour = NULL) +
    theme_climafor(mobile = mobile)

  avec_notes(p, notes_annuelles(d, ctx, meta, ref_n, tend, etp, periode))
}

#' Choisit un pas d'annees lisible selon la largeur disponible.
#' @keywords internal
graphe_ruptures_annees <- function(annees, mobile = FALSE) {
  n_max <- if (mobile) 6 else 12
  pas <- max(1, ceiling(length(unique(annees)) / n_max))
  pas <- c(1, 2, 5, 10, 20)[which.min(abs(c(1, 2, 5, 10, 20) - pas))]
  seq(ceiling(min(annees) / pas) * pas, max(annees), by = pas)
}

#' Prepare les lignes affichees d'un graphique annuel personnalise : plage
#' d'annees, colonnes de completude presentes.
#' @return list(d, repli) : `d` est le data.frame a tracer, ou NULL si rien n'est
#'   affichable, auquel cas `repli` est le graphique d'explication a renvoyer.
#' @keywords internal
lignes_affichees <- function(dat, ctx) {
  if (is.null(dat) || !nrow(dat)) {
    return(list(d = NULL, repli = graphe_message(message_indisponible(dat))))
  }
  d <- dat[dat$annee %in% ctx$affichage, , drop = FALSE]
  if (!nrow(d)) {
    return(list(d = NULL, repli = graphe_message("Aucune année dans la plage sélectionnée")))
  }
  if (!"partielle" %in% names(d)) d$partielle <- FALSE
  if (!"n_manquants" %in% names(d)) d$n_manquants <- 0L
  if (!"dernier_jour" %in% names(d)) d$dernier_jour <- as.Date(NA)
  d$partielle <- d$partielle %in% TRUE
  list(d = d, repli = NULL)
}

#' Barre grise de l'annee incomplete, pour les graphiques personnalises.
#' `y` : colonne de la hauteur a representer. NULL si aucune annee incomplete.
#' @keywords internal
barre_partielle <- function(d, y) {
  dp <- d[d$partielle & is.finite(d[[y]]), , drop = FALSE]
  if (!nrow(dp)) return(NULL)
  ggplot2::geom_col(data = dp, ggplot2::aes(x = .data$annee, y = .data[[y]]),
                    fill = CL$en_cours, width = 0.75, inherit.aes = FALSE)
}


# ------------------------------------------------------------------------------
# 3. Fabrique generique — profils mensuels (reference vs annee comparee)
# ------------------------------------------------------------------------------

#' Profil mensuel en barres : reference en fond, annee comparee en superposition.
#' Le mois incomplet de l'annee comparee est marque d'un astérisque et nomme en
#' note : sa valeur est tronquee, il ne se lit pas comme les autres.
#' @export
graphe_mensuel <- function(dat, col_ref, col_cur, titre_y, ctx,
                           couleur_ref = CL$reference,
                           couleur_cur = CL$annee_courante,
                           mobile = FALSE) {

  if (is.null(dat) || !nrow(dat)) return(graphe_message(message_indisponible(dat)))
  meta <- attr(dat, "meta")

  lib_ref <- paste0("Normale ", ctx$ref_label)
  lib_cur <- as.character(ctx$courante)
  long <- rbind(
    data.frame(mois_lib = dat$mois_lib, serie = lib_ref,
               valeur = dat[[col_ref]], stringsAsFactors = FALSE),
    data.frame(mois_lib = dat$mois_lib, serie = lib_cur,
               valeur = dat[[col_cur]], stringsAsFactors = FALSE)
  )
  long$serie <- factor(long$serie, levels = c(lib_ref, lib_cur))

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$mois_lib, y = .data$valeur,
                                          fill = .data$serie)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                      width = 0.72, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = 0, colour = CL$texte, linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = stats::setNames(
      c(couleur_ref, couleur_cur), c(lib_ref, lib_cur))) +
    ggplot2::labs(x = NULL, y = titre_y, fill = NULL) +
    theme_climafor(mobile = mobile)

  p <- marquer_mois_partiel(p, dat, col_cur, lib_cur)
  avec_notes(p, notes_mensuelles(dat, ctx, meta))
}

#' Astérisque au-dessus du mois incomplet (profils mensuels).
#' @keywords internal
marquer_mois_partiel <- function(p, dat, col_cur, lib_cur) {
  if (!"cur_partiel" %in% names(dat) || !any(dat$cur_partiel %in% TRUE)) return(p)
  i <- which(dat$cur_partiel %in% TRUE & is.finite(dat[[col_cur]]))
  if (!length(i)) return(p)
  a <- data.frame(mois_lib = dat$mois_lib[i], valeur = dat[[col_cur]][i], serie = lib_cur)
  a$serie <- factor(a$serie, levels = levels(p$data$serie))
  p + ggplot2::geom_text(data = a, label = "*", size = 6, vjust = -0.1,
                         position = ggplot2::position_dodge(width = 0.8),
                         show.legend = FALSE, na.rm = TRUE)
}

#' Notes d'un profil mensuel : mois incomplet et reference.
#' @keywords internal
notes_mensuelles <- function(dat, ctx, meta = NULL) {
  notes <- character(0)
  if ("cur_partiel" %in% names(dat) && any(dat$cur_partiel %in% TRUE)) {
    mois <- MOIS_LONG[which(dat$cur_partiel %in% TRUE)]
    fin <- meta$derniere_date
    notes <- c(notes, sprintf(
      "* %s %d : mois incomplet%s ; sa valeur ne porte que sur les jours connus, à lire avec prudence.",
      paste(mois, collapse = ", "), ctx$courante,
      if (!is.null(fin) && !is.na(fin) && as.integer(format(fin, "%Y")) == ctx$courante)
        sprintf(" (données jusqu'au %s)", date_fr(fin)) else ""))
  }
  ex <- sort(intersect(meta$annees_exclues, ctx$ref))
  if (length(ex)) {
    notes <- c(notes, sprintf(
      "Années de la référence sans valeur (moins de %.0f %% des jours renseignés) : %s.",
      100 * CONFIG$stations$seuil_annee, paste(ex, collapse = ", ")))
  }
  notes
}

# ------------------------------------------------------------------------------
# 4. Fonctions de trace, une par indicateur
# ------------------------------------------------------------------------------

#' 01 — Bilan hydrique mensuel P - ETP.
#' @export
plot_bilan_hydrique <- function(dat, ctx, mobile = FALSE) {
  p <- graphe_mensuel(dat, "bh_ref", "bh_cur", "P - ETP (mm)", ctx,
                      couleur_ref = CL$reference, couleur_cur = CL$annee_courante,
                      mobile = mobile) +
    ggplot2::labs(subtitle = "Solde négatif = la demande évaporative excède les apports")
  ajouter_notes(p, note_etp(attr(dat, "meta")))
}

#' 02 — Diagramme ombrothermique de Bagnouls & Gaussen.
#' Axe des precipitations dilate d'un facteur 2 par rapport aux temperatures :
#' toute portion ou la courbe thermique passe au-dessus des barres de pluie
#' correspond a une periode seche.
#' @export
plot_ombrothermique <- function(dat, ctx, mobile = FALSE) {
  if (is.null(dat) || !nrow(dat)) return(graphe_message(message_indisponible(dat)))
  meta <- attr(dat, "meta")

  # L'utilisateur compare la normale et l'annee mise en avant : on trace la
  # normale, et l'annee en surimpression si elle est disponible.
  y_max <- max(c(dat$p_ref, dat$p_cur, 2 * dat$t_ref, 2 * dat$t_cur), na.rm = TRUE)
  partiel <- if ("cur_partiel" %in% names(dat)) dat$cur_partiel %in% TRUE else rep(FALSE, nrow(dat))

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$mois_lib)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$p_ref), fill = CL$pluie,
                      alpha = 0.75, width = 0.7, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = 2 * .data$t_ref, group = 1),
                       colour = CL$temperature, linewidth = 1, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(y = 2 * .data$t_ref),
                        colour = CL$temperature, size = 1.8, na.rm = TRUE)

  if (any(is.finite(dat$p_cur))) {
    p <- p +
      ggplot2::geom_point(data = dat[!partiel, , drop = FALSE],
                          ggplot2::aes(y = .data$p_cur), colour = CL$annee_courante,
                          shape = 18, size = 2.6, na.rm = TRUE) +
      ggplot2::geom_line(ggplot2::aes(y = 2 * .data$t_cur, group = 1),
                         colour = CL$temperature, linetype = "dashed",
                         linewidth = 0.7, na.rm = TRUE)
    if (any(partiel)) {
      # Mois incomplet : losange creux et astérisque, pas de verdict sec/humide.
      p <- p +
        ggplot2::geom_point(data = dat[partiel, , drop = FALSE],
                            ggplot2::aes(y = .data$p_cur), colour = CL$annee_courante,
                            shape = 5, size = 2.6, na.rm = TRUE) +
        ggplot2::geom_text(data = dat[partiel, , drop = FALSE],
                           ggplot2::aes(y = .data$p_cur), label = "*", size = 6,
                           vjust = -0.2, na.rm = TRUE)
    }
  }

  p <- p +
    ggplot2::scale_y_continuous(
      name = "Précipitations (mm)",
      limits = c(0, max(y_max, 10) * 1.05),
      sec.axis = ggplot2::sec_axis(~ . / 2, name = "Température (°C)")
    ) +
    ggplot2::labs(
      x = NULL,
      subtitle = sprintf(
        "Barres : pluie normale %s | Losanges : pluie %d | Trait plein : T normale | Tirets : T %d",
        ctx$ref_label, ctx$courante, ctx$courante)
    ) +
    theme_climafor(mobile = mobile) +
    ggplot2::theme(
      axis.title.y.left  = ggplot2::element_text(colour = CL$pluie),
      axis.title.y.right = ggplot2::element_text(colour = CL$temperature)
    )
  avec_notes(p, notes_mensuelles(dat, ctx, meta))
}

#' 03 — Evolution thermique annuelle.
#' @export
plot_temperature_annuelle <- function(dat, ctx, mobile = FALSE) {
  meta <- attr(dat, "meta")
  p <- graphe_annuel(dat, "t_moy", "Température moyenne annuelle (°C)", ctx,
                     type = "lignes", sens = "chaud", lissage = "lissee",
                     montrer_tendance = TRUE, unite_tendance = "°C", dec_tendance = 2,
                     mobile = mobile) +
    ggplot2::labs(subtitle = sprintf(paste(
      "Trait épais : moyenne mobile %d ans | Tirets : normale %s |",
      "Trait rouge et bande : tendance de Sen et son intervalle de confiance à 95 %%"),
      CONFIG$graphiques$lissage_annees, ctx$ref_label))
  if (identical(meta$source, "station") && !is.null(meta$definition_tmoy)) {
    p <- ajouter_notes(p, sprintf("Température moyenne de la station : %s.", meta$definition_tmoy))
  }
  p
}

#' 04 — Jours de gel et gel tardif.
#' @export
plot_gel <- function(dat, ctx, mobile = FALSE) {
  a <- lignes_affichees(dat, ctx); if (is.null(a$d)) return(a$repli)
  d <- a$d; ok <- d[!d$partielle, , drop = FALSE]

  p <- ggplot2::ggplot(ok, ggplot2::aes(x = .data$annee)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$n_gel, fill = "Jours de gel (T min < 0 °C)"),
                      width = 0.75, na.rm = TRUE) +
    ggplot2::geom_col(ggplot2::aes(y = .data$n_gel_tardif,
                                   fill = "dont gel tardif (avril-mai)"),
                      width = 0.75, na.rm = TRUE) +
    barre_partielle(d, "n_gel") +
    ggplot2::geom_hline(yintercept = d$normale[1], colour = CL$reference,
                        linetype = "dashed", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = c(
      "Jours de gel (T min < 0 °C)"  = CL$neige,
      "dont gel tardif (avril-mai)"    = CL$alerte
    )) +
    ggplot2::scale_x_continuous(breaks = graphe_ruptures_annees(d$annee, mobile)) +
    ggplot2::labs(x = NULL, y = "Nombre de jours", fill = NULL,
                  subtitle = "Le gel tardif frappe après le débourrement : c'est le plus dommageable") +
    theme_climafor(mobile = mobile)
  avec_notes(p, notes_annuelles(d, ctx, attr(dat, "meta"), attr(dat, "ref_n")))
}

#' 05 — Jours de chaleur (trois seuils emboites).
#' @export
plot_chaleur <- function(dat, ctx, mobile = FALSE) {
  a <- lignes_affichees(dat, ctx); if (is.null(a$d)) return(a$repli)
  d <- a$d; ok <- d[!d$partielle, , drop = FALSE]

  s <- ctx$seuils
  lib <- c(sprintf("T max > %g °C", s$chaleur_t1),
           sprintf("T max > %g °C", s$chaleur_t2),
           sprintf("T max > %g °C", s$chaleur_t3))

  p <- ggplot2::ggplot(ok, ggplot2::aes(x = .data$annee)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$n_t1, fill = lib[1]), width = 0.75, na.rm = TRUE) +
    ggplot2::geom_col(ggplot2::aes(y = .data$n_t2, fill = lib[2]), width = 0.75, na.rm = TRUE) +
    ggplot2::geom_col(ggplot2::aes(y = .data$n_t3, fill = lib[3]), width = 0.75, na.rm = TRUE) +
    barre_partielle(d, "n_t1") +
    ggplot2::scale_fill_manual(values = stats::setNames(
      c("#FFCA00", CL$attention, CL$erreur), lib), breaks = lib) +
    ggplot2::scale_x_continuous(breaks = graphe_ruptures_annees(d$annee, mobile)) +
    ggplot2::labs(x = NULL, y = "Nombre de jours", fill = NULL,
                  subtitle = "Seuils emboîtés : chaque barre englobe la suivante") +
    theme_climafor(mobile = mobile)
  avec_notes(p, notes_annuelles(d, ctx, attr(dat, "meta"), attr(dat, "ref_n")))
}

#' 05 bis — Vagues de chaleur : nombre par an, moyenne mobile en surimpression.
#' @export
plot_vagues <- function(dat, ctx, mobile = FALSE) {
  s <- ctx$seuils
  g <- graphe_annuel(dat, "n_vagues", "Nombre de vagues de chaleur", ctx,
                     type = "barres", sens = "chaud", lissage = "lissee",
                     montrer_tendance = TRUE, unite_tendance = "vagues", mobile = mobile)
  # Un graphique d'explication (donnees absentes) n'a ni axe ni sous-titre a regler.
  if (!"val_y" %in% names(g$data)) return(g)
  g +
    # Un nombre de vagues est entier : pas de graduation a 0,5.
    ggplot2::scale_y_continuous(breaks = function(lim) {
      b <- pretty(c(0, lim[2])); b[b == round(b)]
    }) +
    ggplot2::labs(subtitle = sprintf(
      "Au moins %d jours de suite à %g °C ou plus au-dessus de la normale du jour, de mai à septembre",
      s$vague_duree, s$vague_ecart))
}

#' 06 — Precipitations annuelles, phase liquide et solide empilees.
#' @export
plot_precipitations <- function(dat, ctx, mobile = FALSE) {
  a <- lignes_affichees(dat, ctx); if (is.null(a$d)) return(a$repli)
  d <- a$d; ok <- d[!d$partielle, , drop = FALSE]

  # Station : pluie et neige ne sont pas separees (p_sol vide). Une seule serie,
  # le total, plutot qu'une pile dont la moitie serait a zero.
  phases <- !all(is.na(d$p_sol))
  long <- if (phases) {
    rbind(
      data.frame(annee = ok$annee, phase = "Pluie",  valeur = ok$p_liq),
      data.frame(annee = ok$annee, phase = "Neige",  valeur = ok$p_sol)
    )
  } else {
    data.frame(annee = ok$annee, phase = "Total", valeur = ok$p_tot)
  }
  long$phase <- factor(long$phase, levels = c("Neige", "Pluie", "Total"))

  p <- ggplot2::ggplot() +
    ggplot2::geom_col(data = long,
                      ggplot2::aes(x = .data$annee, y = .data$valeur, fill = .data$phase),
                      width = 0.75, na.rm = TRUE) +
    barre_partielle(d, "p_tot") +
    ggplot2::geom_line(data = d, ggplot2::aes(x = .data$annee, y = .data$lissee),
                       colour = CL$annee_courante, linewidth = 1, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = d$normale[1], colour = CL$reference,
                        linetype = "dashed", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = c(Pluie = CL$pluie, Neige = CL$neige,
                                          Total = CL$pluie)) +
    ggplot2::scale_x_continuous(breaks = graphe_ruptures_annees(d$annee, mobile)) +
    ggplot2::labs(x = NULL, y = "Cumul annuel (mm)", fill = NULL,
                  subtitle = if (phases)
                    "Total = phase liquide + phase solide (équivalent en eau)"
                  else "Cumul mesuré par la station : pluie et neige ne sont pas distinguées") +
    theme_climafor(mobile = mobile)
  avec_notes(p, notes_annuelles(d, ctx, attr(dat, "meta"), attr(dat, "ref_n")))
}

#' 07 — Ete meteorologique : temperature et precipitations JJA.
#' Double axe, la pluie ramenee a l'echelle thermique.
#' @export
plot_ete <- function(dat, ctx, mobile = FALSE) {
  a <- lignes_affichees(dat, ctx); if (is.null(a$d)) return(a$repli)
  d <- a$d; ok <- d[!d$partielle, , drop = FALSE]

  # Facteur d'echelle : place les deux series dans le meme champ visuel
  k <- suppressWarnings(max(d$p_jja, na.rm = TRUE) / max(d$t_jja, na.rm = TRUE))
  if (!is.finite(k) || k <= 0) k <- 1

  p <- ggplot2::ggplot(ok, ggplot2::aes(x = .data$annee)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$p_jja, fill = "Précipitations JJA"),
                      width = 0.75, na.rm = TRUE) +
    barre_partielle(d, "p_jja") +
    ggplot2::geom_line(ggplot2::aes(y = .data$t_jja * k, colour = "Température JJA"),
                       linewidth = 1, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = d$p_normale[1], colour = CL$pluie,
                        linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = d$t_normale[1] * k, colour = CL$temperature,
                        linetype = "dashed", linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = c("Précipitations JJA" = CL$pluie)) +
    ggplot2::scale_colour_manual(values = c("Température JJA" = CL$temperature)) +
    ggplot2::scale_y_continuous(
      name = "Précipitations JJA (mm)",
      sec.axis = ggplot2::sec_axis(~ . / k, name = "Température JJA (°C)")
    ) +
    ggplot2::scale_x_continuous(breaks = graphe_ruptures_annees(d$annee, mobile)) +
    ggplot2::labs(x = NULL, fill = NULL, colour = NULL,
                  subtitle = sprintf("Tirets : normales estivales %s", ctx$ref_label)) +
    theme_climafor(mobile = mobile)
  avec_notes(p, notes_annuelles(d, ctx, attr(dat, "meta"), attr(dat, "ref_n"),
                                periode = "saison"))
}

#' 08 — SWI estival et jours de contrainte hydrique.
#' @export
plot_swi <- function(dat, ctx, mobile = FALSE) {
  if (is.null(dat) || !nrow(dat)) {
    return(graphe_message(message_indisponible(
      dat, "SWI non disponible pour ce point (variable SWI_Q absente)")))
  }
  g <- graphe_annuel(dat, "swi_moy", "SWI moyen juin-août (0 = flétrissement, 1 = capacité au champ)",
                     ctx, type = "lignes", sens = "sec", periode = "saison",
                     mobile = mobile)
  g +
    ggplot2::geom_hline(yintercept = ctx$seuils$swi_stress, colour = CL$attention,
                        linetype = "dotted", linewidth = 0.6) +
    ggplot2::geom_hline(yintercept = ctx$seuils$swi_severe, colour = CL$erreur,
                        linetype = "dotted", linewidth = 0.6) +
    ggplot2::labs(subtitle = sprintf(
      "Pointillés : seuil de contrainte (%s) et de stress sévère (%s)",
      fmt_fr(ctx$seuils$swi_stress, 2), fmt_fr(ctx$seuils$swi_severe, 2)))
}

#' 09 — Sequences seches consecutives.
#' @export
plot_secheresse <- function(dat, ctx, mobile = FALSE) {
  g <- graphe_annuel(dat, "duree_max", "Durée maximale de sécheresse (jours)", ctx,
                     type = "barres", sens = "chaud", mobile = mobile)
  g +
    ggplot2::geom_hline(yintercept = ctx$seuils$sec_duree_alerte, colour = CL$attention,
                        linetype = "dotted", linewidth = 0.6) +
    ggplot2::labs(subtitle = sprintf(
      "Suite de jours à moins de %g mm | Pointillés : seuil d'alerte à %d jours",
      ctx$seuils$sec_seuil_mm, ctx$seuils$sec_duree_alerte))
}

#' 10 — Indice de De Martonne.
#' @export
plot_martonne <- function(dat, ctx, mobile = FALSE) {
  a <- lignes_affichees(dat, ctx); if (is.null(a$d)) return(a$repli)
  d <- a$d; ok <- d[!d$partielle, , drop = FALSE]

  # L'indice annuel d'une annee incomplete n'est pas trace (cumul tronque) ;
  # l'indice estival l'est des que l'ete est termine, il est NA sinon.
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$annee)) +
    # Bandes de lecture : elles rendent l'indice interpretable sans table annexe
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 20, ymax = 30,
                      fill = CL$attention, alpha = 0.07) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 10, ymax = 20,
                      fill = CL$erreur, alpha = 0.07) +
    ggplot2::geom_line(data = ok, ggplot2::aes(y = .data$i_annuel, colour = "Indice annuel"),
                       linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$i_estival, colour = "Indice estival (JJA)"),
                       linewidth = 0.9, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = c("Indice annuel" = CL$annee_courante,
                                            "Indice estival (JJA)" = CL$attention)) +
    ggplot2::scale_x_continuous(breaks = graphe_ruptures_annees(d$annee, mobile)) +
    ggplot2::labs(x = NULL, y = "Indice de De Martonne", colour = NULL,
                  subtitle = "Bandes : domaine subhumide (20-30) et semi-aride (10-20)") +
    theme_climafor(mobile = mobile)
  avec_notes(p, notes_annuelles(d, ctx, attr(dat, "meta"), attr(dat, "ref_n")))
}

#' 11 — Degres-jours de croissance.
#' @export
plot_gdd <- function(dat, ctx, mobile = FALSE) {
  graphe_annuel(dat, "gdd", sprintf("Cumul annuel de degrés-jours (base %g °C)",
                                    ctx$seuils$gdd_base),
                ctx, type = "barres", sens = "neutre", couleur = CL$succes,
                lissage = "lissee", montrer_tendance = TRUE,
                unite_tendance = "°C-jours", dec_tendance = 0, mobile = mobile) +
    ggplot2::labs(subtitle = "Énergie thermique disponible pour la croissance ligneuse")
}

#' 12 — Saison de vegetation : duree et bornes.
#' @export
plot_vegetation <- function(dat, ctx, mobile = FALSE) {
  a <- lignes_affichees(dat, ctx); if (is.null(a$d)) return(a$repli)
  d <- a$d; ok <- d[!d$partielle, , drop = FALSE]

  p <- ggplot2::ggplot(ok, ggplot2::aes(x = .data$annee)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = .data$doy_debut, ymax = .data$doy_fin),
                            colour = CL$succes, linewidth = 1.6, alpha = 0.75, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(y = .data$doy_debut), colour = CL$succes,
                        size = 1.3, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(y = .data$doy_fin), colour = CL$attention,
                        size = 1.3, na.rm = TRUE) +
    ggplot2::scale_y_continuous(
      breaks = c(32, 60, 91, 121, 152, 182, 213, 244, 274, 305),
      labels = c("1 fév", "1 mar", "1 avr", "1 mai", "1 juin", "1 juil",
                 "1 août", "1 sep", "1 oct", "1 nov")
    ) +
    ggplot2::scale_x_continuous(breaks = graphe_ruptures_annees(d$annee, mobile)) +
    ggplot2::labs(x = NULL, y = NULL,
                  subtitle = sprintf(
                    "Barres verticales : période ou T moy dépasse %g °C pendant %d jours consécutifs",
                    ctx$seuils$veg_seuil_t, ctx$seuils$veg_n_jours)) +
    theme_climafor(mobile = mobile)
  avec_notes(p, notes_annuelles(d, ctx, attr(dat, "meta"), attr(dat, "ref_n")))
}

#' 13 — P - ETP sur fenetre configurable.
#' @export
plot_petp <- function(dat, ctx, mobile = FALSE) {
  graphe_annuel(dat, "petp", sprintf("P - ETP cumulé, %s (mm)", libelle_fenetre(ctx$fenetre)),
                ctx, type = "barres", sens = "sec", lissage = "lissee",
                montrer_tendance = TRUE, unite_tendance = "mm", dec_tendance = 0,
                etp = TRUE, periode = "saison", mobile = mobile) +
    ggplot2::geom_hline(yintercept = 0, colour = CL$texte, linewidth = 0.4) +
    ggplot2::labs(subtitle = "En dessous de zéro : déficit hydrique climatique de la saison")
}

#' 14 — Precipitations neigeuses.
#' @export
plot_neige <- function(dat, ctx, mobile = FALSE) {
  graphe_annuel(dat, "p_sol", "Précipitations neigeuses (mm équivalent eau)", ctx,
                type = "barres", sens = "neutre", couleur = CL$neige,
                mobile = mobile) +
    ggplot2::labs(subtitle = "Phase solide seule (PRENEI_Q), exprimée en équivalent en eau")
}


# ------------------------------------------------------------------------------
# 5. Profils mensuels et journaliers d'une annee choisie
# ------------------------------------------------------------------------------

#' 15 — Precipitations totales mois par mois.
#' @export
plot_precip_mensuel <- function(dat, ctx, mobile = FALSE) {
  graphe_mensuel(dat, "val_ref", "val_cur", "Cumul mensuel (mm)", ctx,
                 couleur_ref = CL$reference, couleur_cur = CL$pluie,
                 mobile = mobile) +
    ggplot2::labs(subtitle = "Pluie et neige confondues, exprimées en équivalent en eau")
}

#' 16 — Temperature moyenne mois par mois.
#' @export
plot_temperature_mensuelle <- function(dat, ctx, mobile = FALSE) {
  graphe_mensuel(dat, "val_ref", "val_cur", "Température moyenne (°C)", ctx,
                 couleur_ref = CL$reference, couleur_cur = CL$temperature,
                 mobile = mobile) +
    ggplot2::labs(subtitle = "Moyenne des températures journalières de chaque mois")
}

#' 17 — Precipitations neigeuses mois par mois.
#' @export
plot_neige_mensuelle <- function(dat, ctx, mobile = FALSE) {
  graphe_mensuel(dat, "val_ref", "val_cur", "Neige (mm équivalent eau)", ctx,
                 couleur_ref = CL$reference, couleur_cur = CL$neige,
                 mobile = mobile) +
    ggplot2::labs(subtitle = "Phase solide seule (PRENEI_Q)")
}

#' 18 — Temperature journaliere contre la normale, ecarts colores.
#'
#' POURQUOI DEUX BANDEAUX ET NON UNE COURBE COLOREE
#' ------------------------------------------------
#' Colorer la courbe elle-meme selon son signe suppose de la decouper a chaque
#' croisement, ce que ggplot ne fait pas nativement et qui produit des segments
#' interrompus. On remplit plutot l'espace ENTRE la courbe et la normale, en deux
#' couches : l'une bornee par le bas a la normale (donc visible seulement quand
#' l'annee est au-dessus), l'autre bornee par le haut (visible quand elle est en
#' dessous). Chaque couche est ecretee par pmax/pmin, si bien qu'aucune des deux
#' ne deborde du mauvais cote. C'est la construction habituelle des graphiques
#' d'anomalie thermique, et elle reste exacte au jour pres.
#' @export
plot_temperature_journaliere <- function(dat, ctx, mobile = FALSE) {
  if (is.null(dat) || !nrow(dat)) {
    return(graphe_message(message_indisponible(
      dat, "Aucune donnée journalière pour l'année sélectionnée")))
  }
  if (all(is.na(dat$t_norm))) {
    return(graphe_message("Normale journalière indisponible sur cette référence"))
  }

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$date_axe)) +
    # Au-dessus de la normale
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$t_norm,
                   ymax = pmax(.data$t_cur, .data$t_norm)),
      fill = CL$temperature, alpha = 0.55, na.rm = TRUE) +
    # En dessous de la normale
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pmin(.data$t_cur, .data$t_norm),
                   ymax = .data$t_norm),
      fill = CL$pluie, alpha = 0.45, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$t_norm),
                       colour = CL$reference, linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$t_cur),
                       colour = CL$texte, linewidth = 0.3, alpha = 0.8, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = 0, colour = CL$bordure, linewidth = 0.4) +
    ggplot2::scale_x_date(date_breaks = if (mobile) "2 months" else "1 month",
                          date_labels = "%b", expand = c(0.01, 0)) +
    ggplot2::labs(
      x = NULL, y = "Température moyenne journalière (°C)",
      subtitle = sprintf(paste("Trait gris : normale %s lissée sur 15 jours.",
                               "Rouge : %d au-dessus. Bleu : %d en dessous."),
                         ctx$ref_label, ctx$courante, ctx$courante)) +
    theme_climafor(mobile = mobile)
}


# ------------------------------------------------------------------------------
# 6. Vent et deficit de pression de vapeur
# ------------------------------------------------------------------------------

#' 19 — Vent moyen annuel.
#' @export
plot_vent <- function(dat, ctx, mobile = FALSE) {
  graphe_annuel(dat, "ff_moy", "Vent moyen à 10 m (m/s)", ctx,
                type = "lignes", couleur = CL$annee_courante,
                montrer_tendance = TRUE, unite_tendance = "m/s", dec_tendance = 2,
                mobile = mobile) +
    ggplot2::labs(subtitle = paste(
      "Moyenne des vents moyens journaliers. La source ne fournit pas de rafales :",
      "ce graphique décrit un régime de vent, pas un risque de chablis."))
}

#' 20 — Vent moyen mois par mois.
#' @export
plot_vent_mensuel <- function(dat, ctx, mobile = FALSE) {
  graphe_mensuel(dat, "val_ref", "val_cur", "Vent moyen à 10 m (m/s)", ctx,
                 couleur_ref = CL$reference, couleur_cur = CL$annee_courante,
                 mobile = mobile) +
    ggplot2::labs(subtitle = "Saisonnalité du régime de vent")
}

#' 21 — Deficit de pression de vapeur annuel.
#' @export
plot_vpd <- function(dat, ctx, mobile = FALSE) {
  graphe_annuel(dat, "vpd_moy", "VPD moyen annuel (kPa)", ctx,
                type = "lignes", sens = "chaud", couleur = CL$temperature,
                montrer_tendance = TRUE, unite_tendance = "kPa", dec_tendance = 3,
                mobile = mobile) +
    ggplot2::labs(subtitle = paste(
      "Déficit de saturation de l'air : ce que l'atmosphère réclame à la",
      "plante, indépendamment de ce que le sol peut lui fournir."))
}

#' 22 — Deficit de pression de vapeur mois par mois.
#' @export
plot_vpd_mensuel <- function(dat, ctx, mobile = FALSE) {
  graphe_mensuel(dat, "val_ref", "val_cur", "VPD moyen (kPa)", ctx,
                 couleur_ref = CL$reference, couleur_cur = CL$temperature,
                 mobile = mobile) +
    ggplot2::labs(subtitle = "La demande évaporative se concentre sur la saison de végétation")
}


# ------------------------------------------------------------------------------
# 7. Vent soutenu et conjonction vent / sol sature
# ------------------------------------------------------------------------------

#' 23 — Journees de vent soutenu.
#' @export
plot_vent_soutenu <- function(dat, ctx, mobile = FALSE) {
  if (is.null(dat) || !nrow(dat)) {
    return(graphe_message(message_indisponible(dat, "Vent non renseigné sur cette période")))
  }
  graphe_annuel(dat, "n_p95", "Journées de vent soutenu", ctx,
                type = "barres", sens = "chaud", couleur = CL$annee_courante,
                montrer_tendance = TRUE, unite_tendance = "j", dec_tendance = 1,
                mobile = mobile) +
    ggplot2::labs(subtitle = sprintf(paste(
      "Seuil calculé sur ce point : %s m/s, soit le %d%s percentile des vents",
      "moyens journaliers de la référence %s."),
      fmt_fr(dat$seuil_p95[1], 1),
      round(100 * (ctx$seuils$vent_percentile %||% CONFIG$seuils$vent_percentile)),
      "e", ctx$ref_label))
}

#' 24 — Vent soutenu sur sol gorge d'eau.
#' @export
plot_vent_sol_sature <- function(dat, ctx, mobile = FALSE) {
  if (is.null(dat) || !nrow(dat)) {
    return(graphe_message(message_indisponible(
      dat, "Vent ou humidité du sol non renseignés sur cette période")))
  }
  graphe_annuel(dat, "n_combo", "Journées ventées sur sol saturé", ctx,
                type = "barres", sens = "chaud", couleur = CL$deficit,
                montrer_tendance = TRUE, unite_tendance = "j", dec_tendance = 1,
                mobile = mobile) +
    ggplot2::labs(subtitle = sprintf(paste(
      "Vent journalier au-delà de %s m/s ET indice d'humidité des sols",
      "au-delà de %s. Mesure une exposition, non un risque de chablis."),
      fmt_fr(dat$seuil_p95[1], 1), fmt_fr(dat$seuil_swi[1], 2)))
}


# ------------------------------------------------------------------------------
# 8. CONVERSION VERS PLOTLY
# ------------------------------------------------------------------------------
# PRINCIPE : LE GGPLOT RESTE LA SOURCE DE VERITE
# ----------------------------------------------
# Aucune fonction plot_* n'est reecrite en plotly. Chacune continue de produire
# un ggplot, qui est ensuite CONVERTI pour l'affichage a l'ecran. Trois raisons :
#
#   1. l'export PNG, le rapport Word et l'impression ont besoin d'une image
#      matricielle, que seul ggplot sait produire ici ;
#   2. reecrire vingt-quatre traces en plotly doublerait le code a maintenir, et
#      les deux versions divergeraient au premier correctif ;
#   3. un ggplot se teste hors interface — c'est ce que fait la suite de tests.
#
# CE QUE ggplotly() NE SAIT PAS CONVERTIR
# ---------------------------------------
# Les AXES SECONDAIRES. `sec.axis` est purement decoratif dans ggplot — une
# transformation affine appliquee a l'affichage — alors que plotly gere un
# second axe comme un objet a part entiere. La conversion perd donc l'axe de
# droite SANS LEVER D'ERREUR : le graphique s'affiche, avec une courbe de
# temperature dont l'echelle a disparu. C'est le pire des cas, un graphique
# faux et plausible.
#
# Deux traces sont concernes, le diagramme ombrothermique et les temperatures
# estivales. Ils portent `interactif = FALSE` dans le registre et restent
# affiches en image. Ce n'est pas une limitation a contourner : c'est la reponse
# correcte.
#
# Les sous-titres sont eux aussi perdus a la conversion. Ils portent des
# informations de lecture indispensables (seuils retenus, sens des couleurs) :
# le module les recupere et les affiche sous le graphique.

#' Sous-titre d'un ggplot, ou NULL.
#' @export
graphe_sous_titre <- function(p) {
  if (!inherits(p, "ggplot")) return(NULL)
  st <- p$labels$subtitle
  if (is.null(st) || !nzchar(st)) return(NULL)
  st
}

#' Replie le sous-titre d'un ggplot sur plusieurs lignes (ecran etroit).
#'
#' Un sous-titre de 70 caracteres depasse la largeur d'un telephone et est
#' simplement coupe : « Solde negatif = la demande evaporative excede les ap ».
#' Or il porte le sens de lecture du graphique.
#' @param largeur Nombre de caracteres par ligne.
#' @export
graphe_sous_titre_replie <- function(p, largeur = 50) {
  st <- graphe_sous_titre(p)
  if (is.null(st) || nchar(st) <= largeur) return(p)
  p + ggplot2::labs(subtitle = paste(strwrap(st, width = largeur), collapse = "\n"))
}

#' Convertit un ggplot en graphique interactif plotly.
#'
#' @param p Objet ggplot.
#' @param mobile TRUE si affichage etroit : la barre d'outils est reduite.
#' @return Objet plotly, ou un plotly portant un message si la conversion echoue.
#' @export
graphe_interactif <- function(p, mobile = FALSE, tactile = FALSE) {
  if (!inherits(p, "ggplot")) {
    return(plotly::plot_ly() |>
             plotly::layout(annotations = list(
               text = "Graphique indisponible", showarrow = FALSE,
               font = list(family = CONFIG$graphiques$police, size = 14))))
  }

  # Le sous-titre est retire AVANT conversion : plotly le placerait au mieux
  # nulle part, au pire par-dessus le premier point de donnee.
  p$labels$subtitle <- NULL
  p$labels$caption  <- NULL

  res <- tryCatch({
    gp <- plotly::ggplotly(p, tooltip = c("x", "y"))

    gp <- plotly::layout(
      gp,
      font = list(family = CONFIG$graphiques$police, size = 13,
                  color = CL$texte),
      hoverlabel = list(font = list(family = CONFIG$graphiques$police)),
      margin = list(t = 30, r = 20, b = 40, l = 60),
      # Le fond doit rester transparent : la carte qui accueille le graphique
      # porte deja son propre fond, et un blanc opaque y creerait un rectangle.
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      legend = list(orientation = "h", x = 0, y = -0.18),
      # Sur ecran tactile, plotly capte le glissement du doigt pour deplacer le
      # graphique : la page ne defile plus des qu'on la touche sur un trace.
      # Sans deplacement ni zoom, le doigt fait defiler et un appui affiche
      # l'infobulle. Le zoom a la molette reste inutile sur ces appareils.
      dragmode = if (isTRUE(tactile)) FALSE else "zoom"
    )

    plotly::config(
      gp,
      displaylogo = FALSE,
      locale = "fr",
      scrollZoom = FALSE,
      doubleClick = if (isTRUE(tactile)) FALSE else "reset",
      # Sur un ecran etroit, la barre d'outils mange la moitie de la largeur.
      displayModeBar = !isTRUE(mobile) && !isTRUE(tactile),
      modeBarButtonsToRemove = list(
        "select2d", "lasso2d", "hoverClosestCartesian",
        "hoverCompareCartesian", "toggleSpikelines"),
      toImageButtonOptions = list(format = "png", scale = 2)
    )
  },
  error = function(e) {
    # La conversion echoue sur un trace inhabituel : on le dit, on ne masque
    # pas. Le bouton « Image PNG » de la carte reste fonctionnel et donne le
    # graphique complet.
    journaliser_erreur("Conversion en graphique interactif", e)
    plotly::plot_ly() |>
      plotly::layout(annotations = list(
        text = paste("Ce graphique ne peut pas être rendu en interactif.",
                     "\nUtilisez le bouton « Image PNG »."),
        showarrow = FALSE, align = "center",
        font = list(family = CONFIG$graphiques$police, size = 14,
                    color = CL$texte_mention %||% "#5A5A5A")))
  })

  res
}
