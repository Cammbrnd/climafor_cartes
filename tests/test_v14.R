# ==============================================================================
# tests/test_v14.R — SIMPLICITE D'UTILISATION, VERSION 1.4 (audit, partie 5)
# ------------------------------------------------------------------------------
# Cinq constats, cinq groupes :
#   S2  l'analyse se partage par son adresse (aller-retour, entrees hostiles) ;
#   S3  les termes techniques ont un glossaire et des infobulles ;
#   S4  l'attente est annoncee avant de lancer (comptage du cache) ;
#   S1  « A retenir », reglages et indicateurs repliés avec leur ligne d'etat ;
#   S5  un echec de calcul laisse un detail technique, nettoye.
#
# Ces tests lisent le balisage et les fonctions pures, sans navigateur. Ce qu'ils
# ne peuvent pas verifier (rechargement reel de la page, copie dans le
# presse-papiers, defilement, infobulle au survol) l'est dans un navigateur :
# voir README, section « Version 1.4 ».
# ==============================================================================

html_de14 <- function(ui) paste(as.character(htmltools::renderTags(ui)$html), collapse = "\n")
an_max <- as.integer(format(Sys.Date(), "%Y"))
sans_blancs <- function(x) gsub("[[:space:]]+", "", x)


# ==============================================================================
groupe("v1.4 — lien partageable : aller-retour (S2)")
# ==============================================================================

etat0 <- list(lon = 1.4727, lat = 43.35, nom = "Auterive (31190)", source = "safran",
              ref = 1991:2020, courante = 2025L, plage = 1970:an_max,
              fenetre = c(4L, 10L), annees_charge = c(1970L, an_max),
              seuils = list(chaleur_t2 = 28, gel_t = -1))
q0 <- partage_encoder(etat0)
d0 <- partage_decoder(paste0("?", q0))

verifier(!is.null(d0), "une adresse produite par l'application se relit")
verifier(presque_egal(d0$lon, 1.4727) && presque_egal(d0$lat, 43.35),
         "le point est restitue")
verifier(identical(d0$nom, "Auterive (31190)") && identical(d0$source, "safran"),
         "le nom du point et la source sont restitues")
verifier(identical(d0$ref, c(1991L, 2020L)) && identical(d0$courante, 2025L),
         "la reference et l'annee comparee sont restituees")
verifier(identical(d0$plage, c(1970L, an_max)) && identical(d0$fenetre, c(4L, 10L)),
         "la plage affichee et la fenetre de mois sont restituees")
verifier(identical(d0$seuils, list(chaleur_t2 = 28, gel_t = -1)),
         "les seuils modifies sont restitues, et eux seuls")
verifier(length(d0$remarques) == 0L, "une adresse valide ne produit aucune remarque")
verifier(grepl("^v=1&x=1\\.4727&y=43\\.3500&", q0),
         "l'adresse porte une version de format, puis le point")

etat_sans_seuil <- etat0; etat_sans_seuil$seuils <- list()
verifier(!grepl("(^|&)t=", partage_encoder(etat_sans_seuil)),
         "sans seuil modifie, l'adresse n'en porte pas")

tous <- stats::setNames(as.list(DESCRIPTIF_SEUILS$min), DESCRIPTIF_SEUILS$id)
etat_tous <- etat0; etat_tous$seuils <- tous
q_tous <- partage_encoder(etat_tous)
verifier(identical(partage_decoder(q_tous)$seuils, lapply(tous, as.numeric)),
         "les dix-neuf seuils modifies a la fois font l'aller-retour")
verifier(nchar(q_tous) < 900L,
         "meme avec tous les seuils modifies, l'adresse reste courte (moins de 900 caracteres)")

if (isTRUE(l10n_info()[["UTF-8"]])) {
  e_acc <- etat0; e_acc$nom <- "Saint-Étienne (42000)"
  verifier(identical(partage_decoder(partage_encoder(e_acc))$nom, "Saint-Étienne (42000)"),
           "un nom accentue fait l'aller-retour")
}

verifier(identical(partage_preset_reference(c(1991, 2020)), "1991-2020") &&
           identical(partage_preset_reference(c(1985, 2015)), "custom"),
         "une reference connue retrouve son libelle, une autre devient « personnalisee »")
verifier(identical(partage_preset_fenetre(c(4, 10)), "Saison de végétation (avr-oct)") &&
           identical(partage_preset_fenetre(c(6, 8)), "Cœur d'été (juin-août)") &&
           identical(partage_preset_fenetre(c(7, 9)), "custom"),
         "une fenetre connue retrouve son libelle, une autre devient « personnalisee »")

verifier(length(partage_seuils_modifies(CONFIG$seuils)) == 0L,
         "les seuils par defaut ne comptent comme aucune modification")
s_mod <- CONFIG$seuils; s_mod$chaleur_t2 <- 28
verifier(identical(names(partage_seuils_modifies(s_mod)), "chaleur_t2"),
         "seul le seuil change est signale comme modifie")


# ==============================================================================
groupe("v1.4 — lien partage : l'adresse est une entree non fiable (S2)")
# ==============================================================================

attaque <- function(qs) tryCatch(partage_decoder(qs), error = function(e) "ERREUR")

verifier(is.null(attaque("")) && is.null(attaque(NULL)) && is.null(attaque(list())),
         "une adresse vide donne l'ouverture habituelle")
verifier(is.null(attaque("?x=999&y=43")), "longitude hors de France : lien ignore")
verifier(is.null(attaque("?x=1.4&y=60")), "latitude hors de France : lien ignore")
verifier(is.null(attaque("?x=1.4&y=abc")), "coordonnee non numerique : lien ignore")
verifier(is.null(attaque("?y=43.3")), "coordonnee absente : lien ignore")
verifier(is.null(attaque("?x=NaN&y=43")) && is.null(attaque("?x=Inf&y=43")),
         "NaN et Inf ne passent pas pour des coordonnees")
verifier(!identical(attaque("?x=1&x=2&y=43"), "ERREUR"),
         "un parametre repete ne provoque pas d'erreur")

d_xss <- attaque("?x=1.5&y=43.3&n=%3Cscript%3Ealert(1)%3C%2Fscript%3E%22%27")
verifier(is.list(d_xss) && !grepl("[<>\"]", d_xss$nom),
         "un nom contenant du balisage est nettoye (ni < ni > ni guillemet)")
d_long <- attaque(paste0("?x=1.5&y=43.3&n=", strrep("a", 5000)))
verifier(is.list(d_long) && nchar(d_long$nom) <= 80L,
         "un nom de 5 000 caracteres est tronque a 80")
d_vide <- attaque("?x=1.5&y=43.3&n=%3C%3E")
verifier(is.list(d_vide) && nzchar(d_vide$nom), "un nom entierement filtre prend un nom par defaut")

d_src <- attaque("?x=1.5&y=43.3&s=%27%3B%20DROP%20TABLE")
verifier(is.list(d_src) && identical(d_src$source, "safran") && length(d_src$remarques) == 1L,
         "une source illisible retombe sur SAFRAN, avec une remarque")

d_inv <- attaque("?x=1.5&y=43.3&r=2020-1991&a=3000&p=1800-2026&f=13-2&c=abcd")
verifier(is.list(d_inv) && length(d_inv$remarques) == 5L,
         "cinq champs invalides font cinq remarques (reference, annee, plage, fenetre, telechargement)")
verifier(identical(d_inv$ref, as.integer(CONFIG$references[[CONFIG$reference_defaut]])) &&
           identical(d_inv$courante, an_max) &&
           identical(d_inv$fenetre, as.integer(CONFIG$fenetre_defaut)),
         "les champs invalides prennent leur valeur par defaut")

d_t <- attaque("?x=1.5&y=43.3&t=chaleur_t2:99,inconnu:3,gel_t:-1,ab,gel_t:x")
verifier(is.list(d_t) && identical(d_t$seuils, list(gel_t = -1)),
         "un seuil inconnu, hors bornes ou mal forme est ecarte ; un seuil valide est garde")
verifier(is.list(d_t) && length(d_t$remarques) == 1L,
         "les seuils ecartes donnent UNE remarque")

d_c <- attaque("?x=1.5&y=43.3&a=2024&r=1991-2020&c=2000-2010")
verifier(is.list(d_c) && identical(d_c$annees_charge, c(1991L, 2024L)),
         "la plage telechargee est etendue pour couvrir la reference et l'annee comparee")

d_f <- attaque("?x=1.5&y=43.3&a=1900")
verifier(is.list(d_f) && identical(d_f$courante, an_max),
         "une annee comparee anterieure a SAFRAN est refusee")

verifier(!grepl("[<>\"' ]", partage_encoder(modifyList(etat0, list(nom = "A <b>&</b> \"x\" = y")))),
         "l'adresse produite ne contient ni balisage, ni guillemet, ni espace")


# ==============================================================================
groupe("v1.4 — lien partage : l'interface naît avec les valeurs du lien (S2)")
# ==============================================================================

app14 <- new.env()
sys.source("app.R", envir = app14)
# Les identifiants d'onglets (tab-1234-1, data-tabsetid) sont tires au hasard a
# chaque appel : on les neutralise pour comparer deux rendus.
ui14 <- function(qs) gsub("data-tabsetid=\"[0-9]+\"", "data-tabsetid=\"N\"",
  gsub("tab-[0-9]+-[0-9]+", "tab-N",
       html_de14(app14$ui(if (is.null(qs)) list() else list(QUERY_STRING = qs)))))

h_defaut <- ui14("")
qs_lien <- paste0("?x=1.4727&y=43.35&n=Auterive&s=safran&r=1981-2010&a=2019",
                  "&p=1980-2025&f=7-9&c=1975-2026&t=chaleur_t2%3A28")
h_lien <- ui14(qs_lien)

verifier(is.function(app14$ui) && is.function(app14$ui_construire),
         "ui est une fonction de la requete (ui_construire gere l'etat)")
verifier(identical(ui14("?x=oops&y=1"), h_defaut) && identical(ui14("?v=1&junk=%00"), h_defaut) &&
           identical(ui14(NULL), h_defaut),
         "une adresse sans point exploitable, ou sans requete, donne l'interface habituelle")
verifier(grepl("value=\"1991-2020\" selected", h_defaut) &&
           grepl("value=\"1981-2010\" selected", h_lien),
         "la reference du lien est preselectionnee")
verifier(grepl("id=\"periode-courante\"[^>]*value=\"2019\"", h_lien) &&
           grepl(sprintf("id=\"periode-courante\"[^>]*value=\"%d\"", an_max), h_defaut),
         "l'annee comparee du lien est dans le champ")
verifier(grepl("id=\"periode-plage\"[^>]*data-from=\"1980\"[^>]*data-to=\"2025\"", h_lien),
         "la plage affichee du lien est dans le curseur")
verifier(grepl("value=\"custom\" selected", h_lien) && !grepl("value=\"custom\" selected", h_defaut),
         "une fenetre hors liste passe en « personnalisee »")
verifier(grepl("<option value=\"7\" selected", h_lien) && grepl("<option value=\"9\" selected", h_lien),
         "les mois de la fenetre du lien sont preselectionnes")
verifier(grepl("id=\"parametres-seuil_chaleur_t2\"[^>]*value=\"28\"", h_lien) &&
           grepl("id=\"parametres-seuil_chaleur_t2\"[^>]*value=\"30\"", h_defaut),
         "le seuil modifie du lien est dans son champ ; les autres gardent leur defaut")
verifier(grepl("id=\"parametres-annees_charge\"[^>]*data-from=\"1975\"[^>]*data-to=\"2026\"", h_lien),
         "la plage telechargee du lien est dans le curseur des Parametres")
verifier(!grepl("<details class=\"cf-repli\" open", h_defaut) &&
           lengths(regmatches(h_lien, gregexpr("<details class=\"cf-repli\" open", h_lien))) >= 2L,
         "les blocs repliés qui contiennent un reglage du lien s'ouvrent (avance, seuils)")

src_app <- paste(readLines(chemin_ci("app.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
# Depuis la v1.6 (C7), la barre de contexte est construite dans src/utils_ui.R.
src_app <- paste(src_app, paste(readLines(chemin_ci("src/utils_ui.R"), warn = FALSE,
                                          encoding = "UTF-8"), collapse = "\n"), sep = "\n")
verifier(grepl("updateQueryString", src_app) && grepl("mode = \"replace\"", src_app),
         "l'adresse est mise a jour apres chaque analyse, sans entree d'historique")
verifier(grepl("data-copier-lien", src_app) && grepl("a_restaurer", src_app),
         "le bouton « Copier le lien » et la restauration au demarrage sont presents")


# ==============================================================================
groupe("v1.4 — glossaire et infobulles (S3)")
# ==============================================================================

verifier(!anyDuplicated(GLOSSAIRE$terme) && all(nzchar(GLOSSAIRE$definition)),
         "le glossaire : termes uniques, definitions non vides")
verifier(all(vapply(GLOSSAIRE$motif[!is.na(GLOSSAIRE$motif)], function(m)
  !inherits(try(grepl(m, "x", perl = TRUE), silent = TRUE), "try-error"), logical(1))),
  "chaque motif de reperage est une expression reguliere valide")

texte_de <- function(x) if (is.character(x)) x else gsub("&amp;", "&", gsub("<[^>]+>", "", as.character(x)))
titres <- vapply(INDICATEURS, function(i) i$titre, character(1))
verifier(all(vapply(titres, function(t) identical(sans_blancs(texte_de(cf_terme(t))), sans_blancs(t)), logical(1))),
         "les infobulles n'alterent aucun titre d'indicateur (le texte est seulement enveloppe)")

avec <- function(id) grepl("class=\"cf-terme\"", html_de14(cf_terme(INDICATEURS[[id]]$titre)))
verifier(all(vapply(c("petp", "swi", "gdd", "mart", "vpd", "ombro", "gel", "dry"), avec, logical(1))),
         "P - ETP, SWI, GDD, De Martonne, VPD, ombrothermique, gel tardif, sequences seches : infobulle")
verifier(is.character(cf_terme(INDICATEURS[["bh"]]$titre)),
         "un titre sans terme technique reste un simple texte")
h_gdd <- html_de14(cf_terme("Degrés-jours de croissance (GDD)"))
verifier(lengths(regmatches(h_gdd, gregexpr("<abbr", h_gdd))) == 1L,
         "une meme notion n'est soulignee qu'une fois par titre")
verifier(is.character(cf_terme("RETPA et SWIFT")),
         "un terme au milieu d'un mot n'est pas repere (ETP dans RETPA, SWI dans SWIFT)")
h_swi <- html_de14(cf_terme("SWI moyen"))
verifier(grepl("title=\"Soil Wetness Index", h_swi), "l'infobulle de SWI porte sa definition")
verifier(identical(cf_terme(NULL), "") && identical(cf_terme(NA_character_), NA_character_) &&
           identical(cf_terme(""), ""),
         "NULL, NA et chaine vide ne provoquent pas d'erreur")
h_inj <- html_de14(cf_terme("<img src=x onerror=alert(1)> SWI"))
verifier(grepl("&lt;img", h_inj) && !grepl("<img", h_inj),
         "un texte contenant du balisage reste du texte (echappe)")

h_src <- html_de14(mod_sources_ui("sources"))
verifier(grepl("id=\"glossaire\"", h_src), "l'onglet Sources contient la carte « Glossaire »")
verifier(lengths(regmatches(h_src, gregexpr("<dt>", h_src)))[1] >= nrow(GLOSSAIRE),
         "tous les termes du glossaire y figurent")
verifier(all(vapply(GLOSSAIRE$terme, function(t) grepl(t, h_src, fixed = TRUE), logical(1))),
         "chaque terme du glossaire apparait dans la page Sources")


# ==============================================================================
groupe("v1.4 — annonce de l'attente avant le lancement (S4)")
# ==============================================================================

cache_avant <- CONFIG$cache
tmp_cache <- file.path(tempdir(), "cache_v14")
dir.create(tmp_cache, showWarnings = FALSE)
CONFIG$cache$repertoire <- tmp_cache
CONFIG$cache$actif <- TRUE

for (a in 2000:2002) cache_ecrire(1.4727, 43.35, a, data.frame(x = 1))
e1 <- estimer_chargement(1.4727, 43.35, 2000:2009)
verifier(e1$n_total == 10L && e1$n_cache == 3L && e1$n_telecharger == 7L,
         "trois annees en cache sur dix : sept restent a telecharger")
verifier(e1$duree_s == ceiling(7 / CONFIG$api$n_paralleles) * CONFIG$api$duree_lot_s,
         "la duree compte un lot par groupe de requetes simultanees")
verifier(estimer_chargement(5.0, 45.0, 2000:2009)$n_cache == 0L,
         "un autre point n'a rien en cache")
verifier(estimer_chargement(1.4727, 43.35, c(2001, 2001, 2002))$n_total == 2L,
         "les annees en double ne sont comptees qu'une fois")

f2000 <- cache_chemin(1.4727, 43.35, 2000)
Sys.setFileTime(f2000, Sys.time() - (CONFIG$cache$duree_j + 1) * 86400)
verifier(estimer_chargement(1.4727, 43.35, 2000:2009)$n_cache == 2L,
         "une entree plus ancienne que la duree de validite ne compte pas")

CONFIG$cache$actif <- FALSE
verifier(estimer_chargement(1.4727, 43.35, 2000:2009)$n_cache == 0L,
         "cache desactive : tout est a telecharger")

CONFIG$cache <- cache_avant
unlink(tmp_cache, recursive = TRUE)

verifier(phrase_duree(0) == "quelques secondes" && phrase_duree(NA_real_) == "quelques secondes" &&
           phrase_duree(12) == "quelques secondes",
         "moins de quinze secondes : « quelques secondes »")
verifier(phrase_duree(60) == "environ une minute" && phrase_duree(72) == "environ une minute",
         "une soixantaine de secondes : « environ une minute » (68 annees sans cache = 72 s)")
verifier(phrase_duree(200) == "environ 3 minutes", "au-dela : un nombre de minutes arrondi")
verifier(is.numeric(CONFIG$api$duree_lot_s) && CONFIG$api$duree_lot_s > 0,
         "la duree d'un lot est un reglage de CONFIG (estimation, non mesuree)")


# ==============================================================================
groupe("v1.4 — « A retenir » (S1)")
# ==============================================================================

set.seed(11)
bruit_t <- function(doy, a) 10 + 8 * sin((doy - 110) / 365 * 2 * pi) + ifelse(a == 2012, 3, 0) +
  stats::rnorm(length(doy), 0, 2)
don_ecart <- serie_synthetique(2000:2012, t_moy = bruit_t, p_jour = 2)
ctx_ecart <- ctx_test(ref = 2000:2011, courante = 2012, affichage = 2000:2012)
s_ecart <- calc_synthese(don_ecart, ctx_ecart)

verifier("famille" %in% names(s_ecart) && all(s_ecart$famille %in% names(FAMILLES)),
         "chaque ligne de synthese porte une famille du registre")
r_ecart <- synthese_a_retenir(s_ecart)
verifier(nrow(r_ecart) >= 1L && nrow(r_ecart) <= 3L, "au plus trois constats sont retenus")
verifier(all(diff(abs(r_ecart$ecart_reduit)) <= 0),
         "ils sont classes du plus marque au moins marque (ecart rapporte a l'ecart-type)")
verifier(identical(r_ecart$indicateur[1], "Température moyenne"),
         "une annee plus chaude de 3 °C place la temperature moyenne en tete")
verifier(all(s_ecart$classe[match(r_ecart$indicateur, s_ecart$indicateur)] != "neutre"),
         "aucune ligne neutre (ecart sous la bande) n'est retenue")
verifier(all(grepl("normale", r_ecart$texte)) &&
           all(mapply(function(i, t) startsWith(t, i), r_ecart$indicateur, r_ecart$texte)),
         "chaque phrase commence par l'indicateur et cite la normale")

# Annees ordinaires seulement : une annee bissextile cumulerait 2 mm de plus et
# serait, a bon droit, signalee.
don_plat <- serie_synthetique(2001:2005, t_moy = 10, p_jour = 2)
ctx_plat <- ctx_test(ref = 2001:2003, courante = 2005, affichage = 2001:2005)
verifier(nrow(synthese_a_retenir(calc_synthese(don_plat, ctx_plat))) == 0L,
         "une annee identique a la normale ne produit aucun constat")
ctx_court <- ctx_test(ref = 2000:2001, courante = 2012, affichage = 2000:2012)
verifier(nrow(synthese_a_retenir(calc_synthese(don_ecart, ctx_court))) == 0L,
         "une reference de moins de trois annees ne permet pas de juger : rien n'est retenu")
verifier(nrow(synthese_a_retenir(data.frame())) == 0L && nrow(synthese_a_retenir(NULL)) == 0L,
         "une synthese vide ou absente ne produit aucun constat")

src_bilan <- paste(readLines(chemin_ci("src/mod_bilan.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
verifier(grepl("cf-retenir", src_bilan) && grepl("data-ouvrir-famille", src_bilan) &&
           grepl("ouvrir_famille", src_bilan),
         "le bilan affiche le bloc et gere l'ouverture de la thematique visee")

h_par_def <- html_de14(mod_parametres_ui("parametres"))
p_annees <- regexpr("id=\"parametres-annees_charge\"", h_par_def)
p_repli  <- regexpr("<details class=\"cf-repli\"", h_par_def)
p_seuil  <- regexpr("id=\"parametres-seuil_gel_t\"", h_par_def)
verifier(p_annees > 0 && p_repli > p_annees && p_seuil > p_repli,
         "Parametres : l'etendue de la serie reste visible, les seuils sont dans un bloc replie")
verifier(!grepl("<details class=\"cf-repli\" open", h_par_def),
         "Parametres : le bloc des seuils est replie par defaut")
verifier(grepl("id=\"parametres-etat_seuils\"", h_par_def),
         "Parametres : une ligne d'etat dit ce que contient le bloc replie")
verifier(all(vapply(DESCRIPTIF_SEUILS$id, function(i)
  grepl(sprintf("id=\"parametres-seuil_%s\"", i), h_par_def), logical(1))),
  "aucun des dix-neuf seuils n'est retire de la page (replie, pas supprime)")

src_rap <- paste(readLines(chemin_ci("src/mod_rapport.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
verifier(!grepl("input\\$indicateurs %\\|\\|% ORDRE_INDICATEURS", src_rap),
         "Rapport : aucune case cochee (NULL) n'est plus lue comme « tous les indicateurs »")
verifier(grepl("au moins un indicateur", src_rap),
         "Rapport : la preparation sans indicateur est refusee avec un message")

h_rap14 <- html_de14(mod_rapport_ui("rapport"))
p_tous <- regexpr("id=\"rapport-tous_indicateurs\"", h_rap14)
p_rep2 <- regexpr("<details class=\"cf-repli\"", h_rap14)
p_ind  <- regexpr("id=\"rapport-indicateurs\"", h_rap14)
verifier(p_tous > 0 && p_rep2 > p_tous && p_ind > p_rep2,
         "Rapport : « Tout inclure / Tout retirer » restent visibles, les cases sont repliees")
verifier(grepl("id=\"rapport-etat_indicateurs\"", h_rap14),
         "Rapport : une ligne d'etat dit combien d'indicateurs sont inclus")


# ==============================================================================
groupe("v1.4 — echec d'un indicateur : detail technique (S5)")
# ==============================================================================

ind_ko <- INDICATEURS[["pre"]]
ind_ko$calc <- function(d, ctx) stop("boom dans /srv/climafor/src/x.R ligne 12\navec retour")
res_ko <- suppressWarnings(calculer_un_indicateur(
  serie_synthetique(2000:2002, t_moy = 10, p_jour = 2), ind_ko, ctx_test()))
verifier(identical(attr(res_ko, "raison"), "erreur") && nzchar(attr(res_ko, "message")),
         "un calcul en echec rend un resultat vide, raison « erreur », phrase prete a afficher")
verifier(grepl("boom", attr(res_ko, "detail")),
         "le message d'origine est conserve dans l'attribut « detail »")
verifier(is.null(attr(calculer_un_indicateur(
  serie_synthetique(2000:2002, t_moy = 10, p_jour = 2), INDICATEURS[["pre"]], ctx_test()), "detail")),
  "un calcul reussi ne porte aucun detail")

# Racine du defaut : %||% remplacait une table vide par une table neuve.
vide_attr <- data.frame(); attr(vide_attr, "raison") <- "erreur"; attr(vide_attr, "message") <- "cause reelle"
verifier(is.null(attr(vide_attr %||% data.frame(), "raison")),
         "(piege documente) %||% fait perdre ses attributs a une table vide")
verifier(identical(attr(resultat_indicateur(list(a = vide_attr), "a"), "raison"), "erreur"),
         "resultat_indicateur() conserve les attributs d'un resultat vide")
verifier(nrow(resultat_indicateur(list(), "absent")) == 0L &&
           is.null(attr(resultat_indicateur(list(), "absent"), "raison")),
         "un indicateur absent de la liste donne une table vide ordinaire")
verifier(identical(message_indisponible(resultat_indicateur(list(pre = res_ko), "pre")),
                   attr(res_ko, "message")),
         "la carte recoit la cause reelle de l'echec, pas le message generique")
src_rmd <- paste(readLines(chemin_ci("rapport/rapport_climat.Rmd"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
src_rap0 <- paste(readLines(chemin_ci("src/mod_rapport.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
verifier(!any(grepl("\\[\\[cle\\]\\] %\\|\\|% data\\.frame\\(\\)", c(src_rmd, src_bilan, src_rap0))),
         "ni le bilan ni le rapport ne remplacent plus un resultat vide par une table neuve")

l <- erreur_lisible("boom dans /srv/climafor/src/x.R ligne 12\navec retour")
verifier(!grepl("/srv", l, fixed = TRUE) && !grepl("\n", l) && grepl("boom", l),
         "le detail affiche tient sur une ligne et ne montre aucun chemin du serveur")
verifier(!grepl("Users", erreur_lisible("echec dans C:\\Users\\camille\\app\\x.R"), fixed = TRUE),
         "un chemin Windows est masque aussi")
verifier(nchar(erreur_lisible(strrep("mot ", 500))) <= 300L,
         "un message tres long est tronque a 300 caracteres")
verifier(identical(erreur_lisible(NA), "") && erreur_lisible(simpleError("e1")) == "e1",
         "NA donne une chaine vide ; une condition est lue par son message")
verifier(grepl("1 / 2", erreur_lisible("fraction 1 / 2 invalide"), fixed = TRUE),
         "une division ou une fraction dans un message n'est pas prise pour un chemin")

h_det <- html_de14(cf_erreur_detail("pre", "<b>x</b> \"y\""))
verifier(grepl("Détail technique", h_det) && grepl("<code>pre</code>", h_det) &&
           grepl(CONFIG$app$version, h_det, fixed = TRUE),
         "le bloc donne l'identifiant de l'indicateur et la version de l'application")
verifier(grepl("&lt;b&gt;", h_det) && !grepl("<b>x", h_det),
         "le message est echappe : il ne peut pas injecter de balisage")
verifier(grepl("<details", h_det) && !grepl("<details[^>]* open", h_det),
         "le bloc est replie")
detail_avant <- CONFIG$app$detail_erreurs
CONFIG$app$detail_erreurs <- FALSE
verifier(is.null(cf_erreur_detail("pre", "boom")),
         "CONFIG$app$detail_erreurs = FALSE masque le detail (le journal le garde)")
CONFIG$app$detail_erreurs <- detail_avant

src_graph14 <- paste(readLines(chemin_ci("src/mod_graphique.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
verifier(grepl("cf_erreur_detail", src_graph14) && grepl("erreur_detail", src_graph14),
         "la carte affiche le detail d'un echec de calcul ou de trace")
verifier(grepl("cf_terme\\(ind\\$titre\\)", src_graph14),
         "le titre de la carte porte les infobulles du glossaire")
verifier(grepl("erreurs_calcul", src_bilan) && grepl("identical\\(attr\\(res\\[\\[id\\]\\], \"raison\"\\), \"erreur\"\\)", src_bilan),
         "le bilan signale en tete de page les indicateurs dont le calcul a echoue")


# ==============================================================================
groupe("v1.4 — version, feuilles de style et script")
# ==============================================================================

verifier(numeric_version(CONFIG$app$version) >= "1.4.0", "la version est 1.4.0 ou plus recente")
css14 <- paste(readLines(chemin_ci("www/dsfr-lite.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
js14  <- paste(readLines(chemin_ci("www/app.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
verifier(grepl("\\.cf-retenir \\{", css14) && grepl("abbr\\.cf-terme", css14) &&
           grepl("\\.cf-erreur-detail", css14) && grepl("\\.cf-etat-reglage", css14),
         "la feuille de style couvre les quatre nouveaux composants")
verifier(grepl("copierTexte", js14) && grepl("data-copier-lien", js14) &&
           grepl("data-ouvrir-famille", js14) && grepl("a\\[data-onglet\\]", js14),
         "app.js gere la copie du lien et les deux liens internes")
verifier(grepl("aria-live", src_app) && grepl("data-lien-etat", js14),
         "la copie du lien est annoncee aux lecteurs d'ecran")
verifier(!is.null(CF_ICONES$lien), "l'icone « lien » existe")
