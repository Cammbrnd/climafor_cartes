# ==============================================================================
# tests/test_v16.R — SOLIDITE DU CODE, VERSION 1.6 (audit, partie 7)
# ------------------------------------------------------------------------------
#   C2  le cache est une option de SESSION, pas de processus ;
#   C3  une erreur n'est plus avalee : journal, message a l'ecran, rapport ;
#   C4  les constantes de methode vivent dans CONFIG ;
#   C6  les dependances sont declarees (DESCRIPTION) et verrouillables ;
#   C7  les gros blocs des serveurs sont des fonctions que l'on teste seules ;
#   C9  fautes dans les textes affiches.
# (C1 et C5 etaient deja traites : voir tests/lancer_tests.R, « jeu vide », et
# CONFIG$cache$schema. C8 : mesure de couverture, outils/couverture.R.)
#
# Ce que ces tests ne voient pas : le comportement de plusieurs VRAIES sessions
# simultanees dans un navigateur (le simulateur de Shiny en fait vivre deux, mais
# sans reseau ni navigateur) et les versions de paquets d'une autre machine.
# ==============================================================================

html_de16 <- function(ui) paste(as.character(htmltools::renderTags(ui)$html), collapse = "\n")
lire16 <- function(f) paste(readLines(chemin_ci(f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Execute `expr` en gardant les messages (le journal du serveur) au lieu de les afficher.
avec_journal <- function(expr) {
  journal <- character(0)
  valeur <- withCallingHandlers(expr, message = function(m) {
    journal <<- c(journal, conditionMessage(m)); invokeRestart("muffleMessage")
  })
  list(valeur = valeur, journal = journal)
}


# ==============================================================================
groupe("v1.6 — le cache est une option de session (C2)")
# ==============================================================================

fichiers_src16 <- c(chemin_ci("app.R"), list.files("src", pattern = "\\.[Rr]$", full.names = TRUE))
affect_config <- unlist(lapply(fichiers_src16, function(f) {
  l <- readLines(f, warn = FALSE, encoding = "UTF-8")
  l <- l[!grepl("^\\s*#", l)]
  grep("CONFIG(\\$[A-Za-z_]+)+\\s*<<-", l, value = TRUE)
}))
verifier(length(affect_config) == 0L,
         "aucune affectation globale (<<-) ne modifie CONFIG : la configuration n'est plus changee en cours de route")

s_off <- shiny::MockShinySession$new(); s_off$userData$cache_actif <- FALSE
s_on  <- shiny::MockShinySession$new(); s_on$userData$cache_actif  <- TRUE

# La suite de tests coupe le cache par defaut : on le rallume le temps de ce groupe.
actif_avant <- CONFIG$cache$actif
CONFIG$cache$actif <- TRUE
verifier(cache_est_actif(), "hors de toute session, c'est CONFIG$cache$actif qui decide (actif)")
CONFIG$cache$actif <- FALSE
verifier(!cache_est_actif(), "...et inactif quand CONFIG le dit")
CONFIG$cache$actif <- TRUE
verifier(!shiny::withReactiveDomain(s_off, cache_est_actif()),
         "une session qui decoche le cache le voit desactive...")
verifier(shiny::withReactiveDomain(s_on, cache_est_actif()),
         "...une autre session le voit toujours actif")
verifier(isTRUE(CONFIG$cache$actif),
         "et la configuration partagee n'a pas bouge")

# Cache disque reel, dans un repertoire temporaire
rep_avant <- CONFIG$cache$repertoire
CONFIG$cache$repertoire <- file.path(tempdir(), "cache_v16")
dir.create(CONFIG$cache$repertoire, showWarnings = FALSE)
an_c <- serie_synthetique(2001, t_moy = 10)
cache_ecrire(1.40, 43.30, 2001L, an_c)
verifier(!is.null(cache_lire(1.40, 43.30, 2001L)), "hors session : une annee ecrite se relit")
verifier(is.null(shiny::withReactiveDomain(s_off, cache_lire(1.40, 43.30, 2001L))),
         "session au cache decoche : l'entree existante n'est pas lue")
verifier(!is.null(shiny::withReactiveDomain(s_on, cache_lire(1.40, 43.30, 2001L))),
         "session au cache actif : elle est lue")
e_on  <- shiny::withReactiveDomain(s_on,  estimer_chargement(1.40, 43.30, 2001:2002))
e_off <- shiny::withReactiveDomain(s_off, estimer_chargement(1.40, 43.30, 2001:2002))
verifier(e_on$n_cache == 1L && e_off$n_cache == 0L,
         "l'annonce « X annees deja en cache » suit le choix de la session")
ecrit <- shiny::withReactiveDomain(s_off, cache_ecrire(1.40, 43.30, 2005L, an_c))
verifier(!isTRUE(ecrit) && is.null(cache_lire(1.40, 43.30, 2005L)),
         "une session au cache decoche n'ecrit rien")
unlink(CONFIG$cache$repertoire, recursive = TRUE)
CONFIG$cache$repertoire <- rep_avant

# Le module Parametres range le choix dans la session
choix_cache <- list()
shiny::testServer(mod_parametres_server, {
  session$setInputs(cache = FALSE)
  choix_cache$session <<- session$userData$cache_actif
  choix_cache$reactif <<- session$getReturned()$cache()
  session$setInputs(cache = TRUE)
  choix_cache$session2 <<- session$userData$cache_actif
})
verifier(identical(choix_cache$session, FALSE) && identical(choix_cache$reactif, FALSE),
         "decocher « Utiliser le cache local » ecrit FALSE dans la session et dans le reactif rendu")
verifier(identical(choix_cache$session2, TRUE), "le recocher le rend actif")
verifier(isTRUE(CONFIG$cache$actif),
         "...sans que CONFIG$cache$actif ait ete touche")
CONFIG$cache$actif <- actif_avant


# ==============================================================================
groupe("v1.6 — une erreur n'est plus avalee (C3)")
# ==============================================================================

j <- avec_journal(journaliser_erreur("Synthese chiffree", simpleError("boom 42")))
verifier(identical(j$valeur, "boom 42"), "journaliser_erreur rend le message d'origine")
verifier(length(j$journal) == 1L && grepl("^\\[ClimaFor\\] Synthese chiffree en échec : boom 42", j$journal),
         "...et l'ecrit au journal avec son contexte")

ind_ok <- list(id = "x", resume = function(d, ctx) "Phrase du jour.")
ind_ko <- list(id = "x", resume = function(d, ctx) stop("resume casse"))
ind_rq <- list(id = "x", resume = function(d, ctx) shiny::req(FALSE))
verifier(identical(commentaire_indicateur(ind_ok, NULL, NULL), "Phrase du jour."),
         "commentaire_indicateur : cas normal")
verifier(identical(commentaire_indicateur(list(id = "y"), NULL, NULL), "") &&
           identical(commentaire_indicateur(NULL, NULL, NULL), ""),
         "...pas de fonction resume ou pas d'indicateur : texte vide")
c_ko <- avec_journal(commentaire_indicateur(ind_ko, NULL, NULL))
verifier(identical(as.character(c_ko$valeur), "") &&
           identical(attr(c_ko$valeur, "erreur"), "resume casse"),
         "...un echec rend un texte vide MAIS porte la cause (attribut « erreur »)")
verifier(length(c_ko$journal) == 1L && grepl("Commentaire de 'x'.*resume casse", c_ko$journal),
         "...et il est consigne au journal avec l'identifiant de l'indicateur")
c_rq <- avec_journal(commentaire_indicateur(ind_rq, NULL, NULL))
verifier(identical(as.character(c_rq$valeur), "") && is.null(attr(c_rq$valeur, "erreur")) &&
           length(c_rq$journal) == 0L,
         "...un signal reactif (req) n'est pas une erreur : ni attribut ni journal")

# Synthese : un calcul qui echoue est signale, pas efface
calc_synthese_orig <- calc_synthese
calc_synthese <- function(don, ctx) stop("synthese cassee")
s_ko <- avec_journal(synthese_securisee(serie_synthetique(2001), ctx_test()))
calc_synthese <- calc_synthese_orig
verifier(is.data.frame(s_ko$valeur) && !nrow(s_ko$valeur) &&
           identical(attr(s_ko$valeur, "erreur"), "synthese cassee"),
         "synthese_securisee : tableau vide, cause portee par l'attribut « erreur »")
verifier(any(grepl("Synthese chiffree en échec", s_ko$journal)), "...et consignee au journal")
verifier(nrow(synthese_securisee(serie_synthetique(2000:2002), ctx_test())) > 0L,
         "...et inchangee quand tout va bien")

# Le Bilan complet, avec le simulateur de serveur de Shiny
don16 <- serie_synthetique(2000:2012, t_moy = 11, p_jour = 2)
attr(don16, "altitude_point") <- 850
ctx16 <- ctx_test(ref = 2000:2011, courante = 2012, affichage = 2000:2012)
args_bilan <- list(donnees = shiny::reactive(don16), ctx = shiny::reactive(ctx16),
                   lieu = shiny::reactive(list(nom = "Test", lon = 1.4, lat = 43.3)))
res_bilan <- list()
suppressWarnings(shiny::testServer(mod_bilan_server, args = args_bilan, {
  session$setInputs(familles = c("thermique", "hydrique"))
  res_bilan$synthese <<- output$synthese$html
  res_bilan$erreurs  <<- output$erreurs_calcul
}))
verifier(grepl("Synthèse de l'année comparée", res_bilan$synthese) &&
           grepl("cf-kpi", res_bilan$synthese),
         "Bilan : la synthese s'affiche (executee par le simulateur de serveur)")
verifier(grepl("Gel tardif : le point est à 850 m", res_bilan$synthese),
         "Bilan : la note de gel tardif apparait a 850 m")
verifier(is.null(res_bilan$erreurs), "Bilan : aucune alerte d'echec quand tout va bien")

calc_synthese <- function(don, ctx) stop("synthese cassee")
res_ko <- list()
j_bilan <- avec_journal(suppressWarnings(shiny::testServer(mod_bilan_server, args = args_bilan, {
  session$setInputs(familles = "thermique")
  res_ko$synthese <<- output$synthese$html
})))
calc_synthese <- calc_synthese_orig
verifier(grepl("Synthèse chiffrée indisponible", res_ko$synthese),
         "Bilan : une synthese en echec affiche une alerte au lieu de disparaitre")
verifier(grepl("Détail technique", res_ko$synthese) && grepl("synthese cassee", res_ko$synthese),
         "...avec le detail technique replie")
verifier(!grepl("cf-kpi", res_ko$synthese), "...et sans carte trompeuse")
verifier(any(grepl("Synthese chiffree en échec", j_bilan$journal)), "...et la cause est au journal")

# Module graphique : un commentaire qui echoue est signale sur la carte
ind_g <- INDICATEURS[["tmp"]]
ind_g$resume <- function(d, ctx) stop("commentaire casse")
res_g <- list()
j_g <- avec_journal(suppressWarnings(shiny::testServer(
  mod_graphique_server,
  args = list(ind = ind_g,
              donnees = shiny::reactive(calculer_un_indicateur(don16, INDICATEURS[["tmp"]], ctx16)),
              ctx = shiny::reactive(ctx16),
              lieu = shiny::reactive(list(nom = "Test", lon = 1.4, lat = 43.3))), {
    res_g$commentaire <<- output$commentaire$html
  })))
verifier(grepl("commentaire automatique n'a pas pu être produit", res_g$commentaire),
         "Carte : un commentaire qui echoue est annonce, pas remplace par du vide")
verifier(grepl("commentaire casse", res_g$commentaire) && grepl("Détail technique", res_g$commentaire),
         "...avec son detail technique")
verifier(any(grepl("Commentaire de 'tmp'", j_g$journal)), "...et il est au journal")

# Apercu du rapport (module Rapport sous le simulateur de serveur)
res16_calc <- calculer_indicateurs(don16, ctx16)
args_rap <- list(donnees = shiny::reactive(don16), resultats = shiny::reactive(res16_calc),
                 ctx = shiny::reactive(ctx16),
                 lieu = shiny::reactive(list(nom = "Test", lon = 1.4, lat = 43.3)))
res_rap <- list()
suppressWarnings(shiny::testServer(mod_rapport_server, args = args_rap, {
  session$setInputs(indicateurs = c("tmp", "pre"))
  res_rap$normal <<- output$apercu$html
}))
verifier(grepl("Aperçu des messages clés", res_rap$normal) && !grepl("Commentaire non produit", res_rap$normal),
         "Rapport : l'apercu montre les phrases cles quand tout va bien")
resume_sauve <- INDICATEURS[["tmp"]]$resume
INDICATEURS[["tmp"]]$resume <- function(d, ctx) stop("apercu casse")
j_rap <- avec_journal(suppressWarnings(shiny::testServer(mod_rapport_server, args = args_rap, {
  session$setInputs(indicateurs = c("tmp", "pre"))
  res_rap$casse <<- output$apercu$html
})))
INDICATEURS[["tmp"]]$resume <- resume_sauve
verifier(grepl("Commentaire non produit pour", res_rap$casse) &&
           grepl(INDICATEURS[["tmp"]]$titre, res_rap$casse, fixed = TRUE),
         "Rapport : un commentaire en echec est nomme dans l'apercu, pas passe sous silence")
verifier(grepl("Aperçu des messages clés", res_rap$casse),
         "...et les autres phrases cles restent affichees")
verifier(any(grepl("Commentaire de 'tmp'.*apercu casse", j_rap$journal)),
         "...et la cause est au journal")

# Rapport complet (pandoc requis) : un commentaire en echec est signale dans le document
if (rmarkdown::pandoc_available()) {
  entree16 <- list(donnees = don16, resultats = res16_calc, ctx = ctx16,
                   lieu = list(nom = "Test (00)", lat = 43.3, lon = 1.4),
                   indicateurs = c("tmp", "pre"), niveau = "decideur",
                   contexte = "", interactif = FALSE)
  rendre_texte <- function(niveau) {
    f <- tryCatch(rendre_rapport(tempfile("rap16_"), modifyList(entree16, list(niveau = niveau)), "html"),
                  error = function(e) conditionMessage(e))
    if (!is.character(f) || !file.exists(f)) return(paste("ECHEC:", f))
    h <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    gsub("data:image/[a-z]+;base64,[A-Za-z0-9+/=]+", "", h)
  }
  resume_sauve <- INDICATEURS[["tmp"]]$resume
  INDICATEURS[["tmp"]]$resume <- function(d, ctx) stop("rapport casse")
  h_ok_pre <- rendre_texte("decideur")
  INDICATEURS[["tmp"]]$resume <- resume_sauve
  h_ok <- rendre_texte("decideur")
  verifier(!grepl("^ECHEC", h_ok_pre) && grepl("Commentaire non produit", h_ok_pre),
           "Rapport genere : un commentaire en echec est signale dans « Ce qu'il faut retenir »")
  verifier(!grepl("^ECHEC", h_ok) && !grepl("Commentaire non produit", h_ok),
           "...et le meme rapport, sans echec, n'a pas cette mention")
}

# Rapport : meme regle dans le modele et dans l'apercu
src_rmd <- lire16("rapport/rapport_climat.Rmd")
src_rap <- lire16("src/mod_rapport.R")
verifier(grepl("commentaire_indicateur", src_rmd) && grepl("synthese_securisee", src_rmd) &&
           grepl("Commentaire non produit", src_rmd),
         "le modele de rapport signale un commentaire ou une synthese en echec")
verifier(grepl("commentaire_indicateur", src_rap) && grepl("Commentaire non produit", src_rap),
         "...l'apercu du rapport aussi")
verifier(!grepl("error = function\\((e|err)\\) \"\"", src_rmd) &&
           !grepl("error = function\\(e\\) \"\"", src_rap) &&
           !grepl("tryCatch\\(calc_synthese", lire16("src/mod_bilan.R")),
         "...et plus aucun repli silencieux sur « \"\" » dans ces trois fichiers")


# ==============================================================================
groupe("v1.6 — constantes de methode dans CONFIG (C4)")
# ==============================================================================

verifier(identical(CONFIG$methode$gel_printemps_doy_max, 181L),
         "CONFIG$methode$gel_printemps_doy_max existe (30 juin)")
verifier(!grepl("doy <= 181", lire16("src/utils_calc.R"), fixed = TRUE),
         "utils_calc.R ne porte plus la valeur 181 en dur")
# Un gel au jour 160 : compte comme « dernier gel de printemps » avec la limite par defaut,
# plus avec une limite ramenee au jour 150.
don_g16 <- serie_synthetique(2001, t_moy = 12, amplitude = 1)
don_g16$t_min[don_g16$doy == 160] <- -2
g_def <- calc_gel(don_g16, ctx_test(ref = 2001, courante = 2001, affichage = 2001))
lim_avant <- CONFIG$methode$gel_printemps_doy_max
CONFIG$methode$gel_printemps_doy_max <- 150L
g_150 <- calc_gel(don_g16, ctx_test(ref = 2001, courante = 2001, affichage = 2001))
CONFIG$methode$gel_printemps_doy_max <- lim_avant
verifier(identical(as.numeric(g_def$doy_dernier_gel[1]), 160),
         "limite par defaut : un gel au jour 160 est le dernier gel de printemps")
verifier(is.na(g_150$doy_dernier_gel[1]),
         "limite ramenee au jour 150 : ce gel n'en est plus un (la valeur vient bien de CONFIG)")
verifier(!"gel_printemps_doy_max" %in% names(CONFIG$seuils),
         "ce n'est pas un seuil reglable : il n'entre ni dans Parametres ni dans l'annexe des rapports")
verifier(grepl("chaleur_t2", lire16("src/registre_indicateurs.R")) &&
           !grepl("CONFIG\\$seuils\\$chaleur_t2", lire16("src/registre_indicateurs.R")),
         "le commentaire de chaleur lit les seuils du contexte, pas ceux de CONFIG")


# ==============================================================================
groupe("v1.6 — les gros blocs des serveurs sont des fonctions testables (C7)")
# ==============================================================================

set.seed(11)
bruit16 <- function(doy, a) 10 + 8 * sin((doy - 110) / 365 * 2 * pi) + ifelse(a == 2012, 3, 0) +
  stats::rnorm(length(doy), 0, 2)
don_s16 <- serie_synthetique(2000:2012, t_moy = bruit16, p_jour = 2)
s16 <- calc_synthese(don_s16, ctx16)

# -- synthese_carte -----------------------------------------------------------
i_t <- which(s16$indicateur == "Température moyenne")
h_t <- html_de16(synthese_carte(s16, i_t))
verifier(grepl("cf-kpi--alerte", h_t) && grepl("au-dessus", h_t),
         "carte : un ecart chaud marque est en alerte et dit « au-dessus »")
h_p <- html_de16(synthese_carte(s16, which(grepl("^Précipitations totales", s16$indicateur))))
verifier(grepl("cf-kpi--(ok|alerte|neutre)", h_p), "carte : toujours une classe de couleur")
s_n <- s16; s_n$classe[i_t] <- "neutre"
verifier(grepl("cf-kpi--neutre", html_de16(synthese_carte(s_n, i_t))) &&
           grepl("proche de la normale", html_de16(synthese_carte(s_n, i_t))),
         "carte : sous la bande neutre, grise et « proche de la normale »")
s_sd <- s16; s_sd$sd_ref[i_t] <- NA_real_
verifier(grepl("cf-kpi--neutre", html_de16(synthese_carte(s_sd, i_t))) &&
           !grepl("au-dessus|en dessous|proche", html_de16(synthese_carte(s_sd, i_t))),
         "carte : sans ecart-type de reference, pas de couleur ni de qualificatif")
s_na <- s16; s_na$ecart[i_t] <- NA_real_
verifier(grepl("—", html_de16(synthese_carte(s_na, i_t))), "carte : ecart inconnu -> tiret")
s_gel <- s16[grepl("^Jours de gel", s16$indicateur), ]
verifier(grepl("cf-kpi--neutre", html_de16(synthese_carte(s_gel, 1L))),
         "carte : le gel n'a pas de sens favorable, elle reste grise")

# -- synthese_notes -----------------------------------------------------------
n_saf <- synthese_notes(s16, don16, en_station = FALSE, familles = c("thermique", "gel"), seuils = CONFIG$seuils)
verifier(any(grepl("Le point est à 850 m", n_saf)) && any(grepl("Gel tardif", n_saf)),
         "notes : SAFRAN a 850 m -> altitude de la maille et gel tardif")
n_sta <- synthese_notes(s16, don16, en_station = TRUE, familles = c("thermique", "gel"), seuils = CONFIG$seuils)
verifier(!any(grepl("maille SAFRAN", n_sta)) && any(grepl("Gel tardif", n_sta)),
         "notes : une station n'a pas de maille SAFRAN, mais le conseil de gel reste")
n_sans_gel <- synthese_notes(s16, don16, FALSE, familles = "thermique", seuils = CONFIG$seuils)
verifier(!any(grepl("Gel tardif", n_sans_gel)), "notes : pas de conseil de gel si l'indicateur n'est pas disponible")
don_plaine <- don16; attr(don_plaine, "altitude_point") <- 120
verifier(!any(grepl("Gel tardif", synthese_notes(s16, don_plaine, FALSE, c("gel"), CONFIG$seuils))),
         "notes : en plaine, pas de conseil de gel")
s_manq <- s16; attr(s_manq, "manquants") <- c(temperature = 3L, pluie = 0L)
verifier(any(grepl("3 en température", synthese_notes(s_manq, don16, FALSE, character(0), CONFIG$seuils))),
         "notes : les jours sans mesure sont signales")
verifier(is.character(synthese_notes(s16, serie_synthetique(2001), FALSE, character(0), CONFIG$seuils)),
         "notes : toujours un vecteur de phrases, meme vide")

# -- synthese_retenir_ui ------------------------------------------------------
ret16 <- synthese_a_retenir(s16)
h_ret <- html_de16(synthese_retenir_ui(ret16, dispo = names(FAMILLES), ns = function(x) paste0("bilan-", x)))
verifier(grepl("À retenir", h_ret) && grepl("data-ouvrir-famille", h_ret) &&
           grepl("bilan-ouvrir_famille", h_ret, fixed = TRUE),
         "À retenir : un lien vers les graphiques, avec l'identifiant du module")
h_ret0 <- html_de16(synthese_retenir_ui(ret16, dispo = character(0), ns = identity))
verifier(!grepl("data-ouvrir-famille", h_ret0), "À retenir : pas de lien vers une famille indisponible")
h_ret_vide <- html_de16(synthese_retenir_ui(ret16[0, ], dispo = names(FAMILLES), ns = identity))
verifier(grepl("variabilité habituelle", h_ret_vide), "À retenir : aucun ecart -> phrase de calme")

# -- synthese_ui, assemblage ---------------------------------------------------
h_syn <- html_de16(synthese_ui(s16, don16, FALSE, names(FAMILLES), CONFIG$seuils, identity))
verifier(grepl("cf-kpis", h_syn) && grepl("cf-retenir", h_syn) && grepl("glossaire", h_syn) &&
           grepl("Gel tardif", h_syn),
         "synthese_ui assemble « A retenir », cartes, notes et renvoi au glossaire")

# -- indicateurs_non_calculables ------------------------------------------------
res16 <- calculer_indicateurs(don16, ctx16)
nc_saf <- indicateurs_non_calculables(don16, res16, en_station = FALSE)
verifier(nrow(nc_saf) == 0L && identical(names(nc_saf), c("id", "titre", "manque", "famille")),
         "indicateurs_non_calculables : SAFRAN renseigne tout, tableau vide de la bonne forme")
don_sans_hu <- don16; don_sans_hu$hu <- NA_real_; don_sans_hu$vpd <- NA_real_  # comme une station
res_sans_hu <- calculer_indicateurs(don_sans_hu, ctx16)
nc_sta <- indicateurs_non_calculables(don_sans_hu, res_sans_hu, en_station = TRUE)
verifier("vpd" %in% nc_sta$id && all(nc_sta$famille %in% names(FAMILLES)) &&
           all(nzchar(nc_sta$manque)),
         "...station sans humidite : le VPD est signale, avec sa famille et la raison")

# -- qualite_serie_ui -----------------------------------------------------------
st16 <- list(nom = "STATION <b>X</b>", distance_km = 4.2, alt = 310)
q0 <- list(niveau = 1L, libelle = "Bonne", alertes = character(0))
h_q0 <- html_de16(qualite_serie_ui(q0, st16, "1991-2020"))
verifier(grepl("La station couvre la référence 1991-2020", h_q0) && grepl("310 m", h_q0),
         "qualite : aucune alerte -> la station couvre la reference, altitude affichee")
verifier(!grepl("<b>X</b>", h_q0, fixed = TRUE) && grepl("&lt;b&gt;X", h_q0, fixed = TRUE),
         "qualite : le nom de la station est echappe")
q4 <- list(niveau = 3L, libelle = "Limitée", alertes = c("a", "b", "c", "d"))
h_q4 <- html_de16(qualite_serie_ui(q4, st16, "1991-2020"))
verifier(grepl("Voir les 2 autres remarques", h_q4) && grepl("<details", h_q4),
         "qualite : plus de deux alertes -> les premieres, puis un repli pour les autres")
st_inc <- st16; st_inc$alt <- NA_real_
verifier(grepl("altitude inconnue", html_de16(qualite_serie_ui(q0, st_inc, "x"))),
         "qualite : altitude inconnue dite telle")

# -- app.R : contexte, messages, attente, etat ----------------------------------
p16 <- list(nom = "Auterive", lon = 1.47, lat = 43.35)
h_c <- html_de16(contexte_ui(don16, p16, ctx16))
verifier(grepl("Auterive", h_c) && grepl("point à 850 m", h_c) && grepl("data-copier-lien", h_c) &&
           grepl("rouvrir_reglages", h_c),
         "contexte_ui : lieu, altitude du point, bouton de copie et bouton Modifier")
don_st <- don16; attr(don_st, "station") <- list(nom = "<img src=x onerror=alert(1)>", distance_km = 3)
verifier(!grepl("<img", html_de16(contexte_ui(don_st, p16, ctx16)), fixed = TRUE),
         "contexte_ui : le nom d'une station n'est jamais insere comme balise")

h_m <- html_de16(messages_service_ui(paste("message", 1:15), bloquant = FALSE))
verifier(sum(gregexpr("<li>", h_m, fixed = TRUE)[[1]] > 0) == 12L && grepl("et 3 autre\\(s\\) message", h_m) &&
           grepl("cf-alerte--attention", h_m) && !grepl("diagnostic.R", h_m),
         "messages : douze au plus, le reste est compte ; sans diagnostic quand ce n'est pas bloquant")
h_mb <- html_de16(messages_service_ui("rien", bloquant = TRUE))
verifier(grepl("cf-alerte--erreur", h_mb) && grepl("diagnostic.R", h_mb) &&
           grepl("n'a rien renvoyé", h_mb),
         "messages : bloquant -> alerte rouge et renvoi vers diagnostic.R")

verifier(grepl("quelques secondes à une minute", phrase_attente(TRUE, NULL)),
         "attente : station -> de quelques secondes a une minute")
verifier(grepl("environ une minute", phrase_attente(FALSE, NULL)),
         "attente : estimation impossible -> environ une minute")
verifier(identical(phrase_attente(FALSE, list(n_total = 5L, n_cache = 5L, n_telecharger = 0L, duree_s = 0)),
                   "5 années déjà en cache : chargement immédiat."),
         "attente : tout en cache -> chargement immediat")
ph <- phrase_attente(FALSE, list(n_total = 10L, n_cache = 4L, n_telecharger = 6L, duree_s = 12))
verifier(grepl("^6 années à télécharger", ph) && grepl("4 déjà en cache", ph),
         "attente : annees a telecharger et deja en cache")
verifier(grepl("^1 année à télécharger", phrase_attente(FALSE, list(n_total = 1L, n_cache = 0L, n_telecharger = 1L, duree_s = 6))),
         "attente : singulier")

verifier(grepl("Aucun point", html_de16(etat_chargement_ui(NULL, NULL, NULL, FALSE, NULL))),
         "etat : sans point -> « Aucun point sélectionné »")
h_e <- html_de16(etat_chargement_ui(NULL, p16, list(type = "safran"), FALSE, NULL))
verifier(grepl("Prêt : Auterive · grille SAFRAN", h_e), "etat : point choisi, SAFRAN -> « Prêt »")
h_es <- html_de16(etat_chargement_ui(NULL, p16, list(type = "station", station = list(nom = "TOULOUSE")), FALSE, NULL))
verifier(grepl("station TOULOUSE", h_es) && grepl("quelques secondes", h_es),
         "etat : station choisie -> son nom et l'attente de station")
verifier(grepl("cf-etat--ok", html_de16(etat_chargement_ui(don16, p16, NULL, FALSE, NULL))) &&
           grepl("jours chargés", html_de16(etat_chargement_ui(don16, p16, NULL, FALSE, NULL))),
         "etat : donnees chargees et a jour -> confirmation")
verifier(grepl("Prêt", html_de16(etat_chargement_ui(don16, p16, list(type = "safran"), TRUE, NULL))),
         "etat : selection changee depuis l'analyse -> de nouveau « Prêt »")

# -- taille des serveurs -----------------------------------------------------------
# Reference de la v1.5 : serveur du Bilan 450 lignes, serveur d'app.R 346.
long_fonction <- function(fichier, motif) {
  e <- parse(chemin_ci(fichier), keep.source = TRUE, encoding = "UTF-8")
  sr <- attr(e, "srcref")
  for (k in seq_along(e)) {
    if (grepl(motif, paste(deparse(e[[k]])[1], collapse = ""))) {
      return(sr[[k]][3] - sr[[k]][1] + 1L)
    }
  }
  NA_integer_
}
n_bilan <- long_fonction("src/mod_bilan.R", "^mod_bilan_server <-")
n_app   <- long_fonction("app.R", "^server <-")
verifier(!is.na(n_bilan) && n_bilan <= 330L,
         sprintf("le serveur du Bilan ne grossit pas (%d lignes, 450 en v1.5)", n_bilan))
verifier(!is.na(n_app) && n_app <= 290L,
         sprintf("le serveur d'app.R ne grossit pas (%d lignes, 346 en v1.5)", n_app))


# ==============================================================================
groupe("v1.6 — paquets declares et verrouillables (C6)")
# ==============================================================================

desc <- read.dcf(chemin_ci("DESCRIPTION"))
imports <- trimws(sub("\\s*\\(.*\\)", "", strsplit(desc[1, "Imports"], ",")[[1]]))
src_global <- lire16("global.R")
libs <- unique(sub("^library\\((\\w+)\\).*$", "\\1",
                   grep("^library\\(\\w+\\)", strsplit(src_global, "\n")[[1]], value = TRUE)))
# v1.7.1 : curl s'y ajoute (appele par curl::curl_options()) ; test_v171.R verifie
# que chaque paquet de plus est bien appele par le code.
verifier(all(libs %in% imports),
         "DESCRIPTION declare tous les paquets que global.R charge")
verifier(identical(unname(desc[1, "Version"]), CONFIG$app$version),
         "DESCRIPTION et CONFIG$app$version portent la meme version")
verifier(grepl("R \\(>= 4\\.1", desc[1, "Depends"]),
         "R >= 4.1 est exige (le code utilise le tube natif |>)")
verifier(grepl("httr2 \\(>= 1\\.0", desc[1, "Imports"]),
         "httr2 >= 1.0 est exige (telechargements en parallele)")
rsc <- strsplit(lire16(".rscignore"), "\n")[[1]]
verifier(all(c("DESCRIPTION", "description") %in% rsc),
         "DESCRIPTION n'est pas envoye par le bouton Publish (le deploiement ne change pas)")
verifier(!inherits(try(parse(chemin_ci("outils/figer_versions.R")), silent = TRUE), "try-error") &&
           grepl("type = \"explicit\"", lire16("outils/figer_versions.R"), fixed = TRUE),
         "outils/figer_versions.R est lisible et ecrit un verrou a partir de DESCRIPTION")
verifier(file.exists(chemin_ci("outils/couverture.R")),
         "outils/couverture.R (mesure de couverture) est fourni")


# ==============================================================================
groupe("v1.6 — textes affiches (C9) et version")
# ==============================================================================

texte_registre <- lire16("src/registre_indicateurs.R")
verifier(!grepl("Paramètrès", texte_registre, fixed = TRUE),
         "« Paramètrès » est corrige")
verifier(!grepl("est comptée des que", texte_registre, fixed = TRUE),
         "« dès que » porte son accent dans la methode des jours de chaleur")
verifier(numeric_version(CONFIG$app$version) >= "1.6.0", "la version est au moins 1.6.0")
