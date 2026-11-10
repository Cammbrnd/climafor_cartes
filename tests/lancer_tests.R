# ==============================================================================
# tests/lancer_tests.R — SUITE DE TESTS
# ------------------------------------------------------------------------------
# Lancement, depuis la RACINE du projet :
#
#     Rscript tests/lancer_tests.R
#
# Ces tests ne demandent aucun acces reseau : tout repose sur des series
# synthetiques dont le resultat attendu se calcule a la main.
#
# CE QUE CES TESTS CHERCHENT A ATTRAPER
# -------------------------------------
# Pas les plantages — ceux-la se voient a l'usage. Les ERREURS DE METHODE, qui
# produisent un nombre plausible et faux : une normale mensuelle calculee dans
# le mauvais ordre, un lissage non circulaire, une pression de saturation prise
# sur la temperature moyenne au lieu des extremes. Un graphique faux mais joli
# est plus dangereux qu'un graphique absent.
# ==============================================================================

if (!file.exists("global.R")) {
  stop("Lancez ce script depuis la racine du projet : Rscript tests/lancer_tests.R")
}

suppressMessages(source("global.R"))
source("tests/harnais.R")
source("tests/donnees_synthetiques.R")

cat(sprintf("\nClimaFor %s — suite de tests\n", CONFIG$app$version))


# ==============================================================================
groupe("Agregations annuelles")
# ==============================================================================

don <- serie_synthetique(2000:2002, t_moy = 10, p_jour = 2)
ctx <- ctx_test()

pre <- calc_precipitations(don, ctx)
verifier(nrow(pre) == 3, "une ligne par annee")
# 2 mm/j sur 366 jours en 2000 (bissextile), 365 les deux annees suivantes
verifier(presque_egal(pre$p_tot[pre$annee == 2000], 732),
         "cumul annuel exact sur une annee bissextile (2 mm x 366)")
verifier(presque_egal(pre$p_tot[pre$annee == 2002], 730),
         "cumul annuel exact sur une annee ordinaire (2 mm x 365)")
# Normale = moyenne de 732 et 730
verifier(presque_egal(pre$normale[1], 731),
         "la normale est la moyenne des cumuls annuels de la reference")
verifier(presque_egal(pre$anomalie[pre$annee == 2002], -1),
         "l'anomalie est la difference a cette normale")

tmp <- calc_temperature_annuelle(don, ctx)
verifier(presque_egal(tmp$t_moy[1], 10),
         "moyenne annuelle exacte sur une serie constante")


# ==============================================================================
groupe("Profils mensuels — ordre des operations")
# ==============================================================================

pm <- calc_precip_mensuel(don, ctx)
verifier(nrow(pm) == 12, "douze lignes, une par mois")
verifier(presque_egal(pm$val_cur[1], 62),
         "janvier 2002 : 2 mm x 31 jours")
verifier(presque_egal(pm$val_cur[2], 56),
         "fevrier 2002, non bissextile : 2 mm x 28 jours")
# Reference 2000 (bissextile, fevrier a 58) et 2001 (56) -> moyenne 57.
# C'est le point de methode : la moyenne porte sur les CUMULS MENSUELS, pas sur
# les valeurs journalieres. Une somme globale divisee par le nombre d'annees
# donnerait la meme chose ici, mais divergerait sur une annee incomplete —
# c'est ce que verifie le test suivant.
verifier(presque_egal(pm$val_ref[2], 57),
         "normale de fevrier : moyenne des cumuls mensuels 2000 et 2001")

# Mois incomplet : on retire la seconde moitie de fevrier 2001 (14 jours sur 28).
# Un cumul de 14 jours n'est pas un cumul mensuel : le mois est ECARTE de la
# reference (moins de 90 % de jours renseignes) au lieu d'abaisser la normale a 43.
don_troue <- don[!(don$annee == 2001 & don$mois == 2 & don$jour > 14), ]
pm2 <- calc_precip_mensuel(don_troue, ctx)
verifier(presque_egal(pm2$val_ref[2], 58),
         "un mois a moitie renseigne est ecarte de la normale : fevrier = 58 mm (2000 seul), pas 43")
verifier(presque_egal(pm2$val_ref[1], 62),
         "les autres mois restent intacts")

# Deux jours manquants sur 28 (26 jours, 93 %) : le mois reste retenu, la moyenne
# porte sur les CUMULS mensuels (58 et 52 -> 55), pas sur les valeurs journalieres.
don_2j <- don[!(don$annee == 2001 & don$mois == 2 & don$jour > 26), ]
verifier(presque_egal(calc_precip_mensuel(don_2j, ctx)$val_ref[2], 55),
         "un mois renseigne a 93 % est conserve ; la normale moyenne des cumuls mensuels")

# Annee comparee : son mois incomplet est conserve mais signale.
don_cur <- don[!(don$annee == 2002 & don$mois == 12 & don$jour > 10), ]
pc <- calc_precip_mensuel(don_cur, ctx)
verifier(isTRUE(pc$cur_partiel[12]) && !any(pc$cur_partiel[1:11]),
         "le mois incomplet de l'annee comparee est signale (cur_partiel), les autres non")
verifier(presque_egal(pc$val_cur[12], 20),
         "sa valeur reste celle des jours connus (10 jours x 2 mm)")

tm <- calc_temperature_mensuelle(don, ctx)
verifier(presque_egal(tm$val_cur[7], 10),
         "moyenne mensuelle exacte pour une temperature constante")


# ==============================================================================
groupe("Lissage circulaire")
# ==============================================================================

verifier(presque_egal(lisser_circulaire(rep(5, 366), 15), rep(5, 366)),
         "une serie constante reste constante apres lissage")

pic <- rep(0, 366); pic[1] <- 366
liss <- lisser_circulaire(pic, 15)
verifier(presque_egal(sum(liss), 366, tolerance = 1e-6),
         "le lissage conserve la somme (aucune masse creee ni perdue)")
# Le point cle : un pic au 1er janvier doit influencer la FIN de decembre.
verifier(liss[366] > 0 && liss[360] > 0,
         "un pic au 1er janvier se propage sur la fin de decembre (circularite)")
verifier(presque_egal(liss[180], 0),
         "et n'atteint pas le milieu de l'annee")


# ==============================================================================
groupe("Temperatures journalieres")
# ==============================================================================

# Reference a 10 degres, annee comparee a 12 : ecart de +2 partout.
don_chaud <- rbind(
  serie_synthetique(2000:2001, t_moy = 10),
  serie_synthetique(2002,      t_moy = 12))
tj <- calc_temperature_journaliere(don_chaud, ctx_test())
verifier(nrow(tj) > 360, "une ligne par jour renseigne de l'annee comparee")
verifier(presque_egal(mean(tj$ecart, na.rm = TRUE), 2, tolerance = 1e-6),
         "ecart moyen exact de +2 degres")
verifier(all(tj$ecart > 0, na.rm = TRUE),
         "tous les jours au-dessus de la normale, aucun en dessous")
verifier(all(!is.na(tj$t_cur)),
         "aucune ligne sans donnee : l'annee ne se prolonge pas au-dela du dernier jour connu")


# ==============================================================================
groupe("Deficit de pression de vapeur")
# ==============================================================================

es <- function(t) 0.6108 * exp(17.27 * t / (t + 237.3))

# Air sature : le deficit est nul par construction.
don_sature <- serie_synthetique(2000:2002, t_moy = 20, amplitude = 0, hu = 100)
verifier(presque_egal(unique(don_sature$vpd), 0, tolerance = 1e-9),
         "a 100 % d'humidite et sans amplitude, le VPD est nul")

# Valeur calculee a la main : T = 20 degC, amplitude nulle, HU = 50 %
# es(20) = 2,338 kPa ; ea = 1,169 kPa ; VPD = 1,169 kPa
don_sec <- serie_synthetique(2000:2002, t_moy = 20, amplitude = 0, hu = 50)
verifier(presque_egal(unique(don_sec$vpd), es(20) * 0.5, tolerance = 1e-9),
         "valeur exacte a 20 degres et 50 % d'humidite")

# LE point de methode : la pression de saturation est convexe. Prise sur les
# extremes, elle est superieure a celle prise sur la moyenne — et l'ecart
# grandit avec l'amplitude. Si un jour quelqu'un « simplifie » en utilisant
# es(t_moy), ce test tombe.
don_ampli <- serie_synthetique(2000, t_moy = 20, amplitude = 10, hu = 50)
verifier(unique(don_ampli$vpd) > unique(don_sec$vpd),
         "l'amplitude thermique augmente le VPD (convexite de la pression de saturation)")

vpd <- calc_vpd(don_sec, ctx_test())
verifier(nrow(vpd) == 3 && presque_egal(vpd$anomalie[vpd$annee == 2002], 0),
         "agregation annuelle du VPD, ecart nul sur une serie constante")

# Variable absente : degradation propre, pas de plantage.
don_sans_hu <- don; don_sans_hu$vpd <- NA_real_
verifier(nrow(calc_vpd(don_sans_hu, ctx)) == 0,
         "sans humidite, calc_vpd renvoie un tableau vide plutot qu'une erreur")


# ==============================================================================
groupe("Vent")
# ==============================================================================

don_vent <- serie_synthetique(2000:2002, ff = 3)
v <- calc_vent(don_vent, ctx_test())
verifier(presque_egal(v$ff_moy[1], 3), "moyenne annuelle du vent exacte")
verifier(all(v$n_ventes == 0),
         "aucune journee venteuse a 3 m/s pour un seuil a 8 m/s")

don_vente <- serie_synthetique(2002, ff = 12)
v2 <- calc_vent(don_vente, ctx_test(ref = 2002, courante = 2002, affichage = 2002))
verifier(v2$n_ventes[1] == 365,
         "toutes les journees comptees comme venteuses a 12 m/s")

don_sans_ff <- don; don_sans_ff$ff <- NA_real_
verifier(nrow(calc_vent(don_sans_ff, ctx)) == 0,
         "sans vent, calc_vent renvoie un tableau vide plutot qu'une erreur")


# ==============================================================================
groupe("Vent soutenu — seuil relatif au point")
# ==============================================================================

# Serie a 2 m/s, avec dix journees a 20 m/s par annee de reference et trente en
# 2002. Le 95e percentile de la reference (731 jours dont 20 ventes, soit 2,7 %)
# vaut donc 2, et le comptage STRICT ne retient que les journees a 20.
souffle <- function(swi = 0.6) {
  d <- serie_synthetique(2000:2002, ff = 2, swi = swi)
  d$ff[d$doy <= 10] <- 20
  d$ff[d$annee == 2002 & d$doy <= 30] <- 20
  d
}

vs <- calc_vent_soutenu(souffle(), ctx_test())
verifier(presque_egal(vs$seuil_p95[1], 2),
         "le seuil est le 95e percentile des vents de la reference")
verifier(vs$n_p95[vs$annee == 2000] == 10 && vs$n_p95[vs$annee == 2001] == 10,
         "comptage exact sur les annees de reference")
verifier(vs$n_p95[vs$annee == 2002] == 30,
         "comptage exact sur l'annee comparee")
verifier(presque_egal(vs$normale[1], 10),
         "la normale est la moyenne des comptages de reference")

# Point clef : comparaison STRICTE. Sur un vent constant, le percentile vaut la
# valeur constante ; un test large compterait les 365 jours, ce qui n'aurait
# aucun sens. Si quelqu'un remplace un jour `>` par `>=`, ce test tombe.
vs_plat <- calc_vent_soutenu(serie_synthetique(2000:2002, ff = 3), ctx_test())
verifier(all(vs_plat$n_p95 == 0),
         "sur un vent constant, aucune journee ne depasse strictement le percentile")


# ==============================================================================
groupe("Vent soutenu sur sol gorge d'eau")
# ==============================================================================

# Sol sature en permanence : la conjonction se reduit au comptage du vent.
vsol <- calc_vent_sol_sature(souffle(swi = 0.95), ctx_test())
verifier(vsol$n_combo[vsol$annee == 2002] == 30,
         "sol sature : toutes les journees ventees sont retenues")
verifier(vsol$n_sature[vsol$annee == 2002] == 365,
         "comptage des journees de sol sature")

# Sol ressuye : la meme annee ventee ne produit aucune conjonction. C'est tout
# l'interet de l'indicateur — le vent seul ne suffit pas.
vsec <- calc_vent_sol_sature(souffle(swi = 0.6), ctx_test())
verifier(all(vsec$n_combo == 0),
         "sol ressuye : aucune conjonction malgre un vent identique")
verifier(all(vsec$n_sature == 0),
         "aucune journee de sol sature a SWI 0,6")

don_sans_swi <- souffle(); don_sans_swi$swi <- NA_real_
verifier(nrow(calc_vent_sol_sature(don_sans_swi, ctx_test())) == 0,
         "sans humidite du sol, l'indicateur renvoie un tableau vide")


# ==============================================================================
groupe("Bilan hydrique et P - ETP")
# ==============================================================================

# 2 mm de pluie et 1 mm d'ETP par jour : le solde est de +1 mm/jour.
bh <- calc_bilan_hydrique(don, ctx)
verifier(nrow(bh) == 12, "un profil sur douze mois")

petp <- calc_petp(don, ctx)
# Fenetre avril-octobre : 30+31+30+31+31+30+31 = 214 jours, donc +214 mm.
verifier(presque_egal(petp$petp[petp$annee == 2002], 214),
         "P - ETP exact sur la fenetre avril-octobre")


# ==============================================================================
groupe("Gel et chaleur")
# ==============================================================================

# t_moy = 3, amplitude 5 -> t_min = -2 : tous les jours sont des jours de gel.
don_gel <- serie_synthetique(2000:2002, t_moy = 3, amplitude = 5)
g <- calc_gel(don_gel, ctx_test())
verifier(g$n_gel[g$annee == 2002] == 365,
         "tous les jours comptes comme jours de gel quand t_min est sous le seuil")

don_doux <- serie_synthetique(2000:2002, t_moy = 20, amplitude = 5)
g2 <- calc_gel(don_doux, ctx_test())
verifier(all(g2$n_gel == 0), "aucun jour de gel sur une serie douce")


# ==============================================================================
groupe("Integrite du registre")
# ==============================================================================
# Ces controles ne verifient aucun calcul : ils garantissent qu'une entree
# ajoutee au registre est complete et coherente. C'est le filet de securite de
# l'architecture generique — une entree malformee ne casserait qu'a l'affichage,
# donc tard, et seulement si l'on pense a regarder la bonne carte.

verifier(length(INDICATEURS) > 0, "le registre n'est pas vide")

verifier(all(vapply(names(INDICATEURS),
                    function(k) identical(INDICATEURS[[k]]$id, k), logical(1))),
         "l'identifiant de chaque entree correspond a sa cle")

verifier(all(vapply(INDICATEURS,
                    function(i) is.function(i$calc) && is.function(i$plot), logical(1))),
         "chaque entree porte une fonction de calcul et une fonction de trace")

verifier(all(vapply(INDICATEURS,
                    function(i) i$famille %in% names(FAMILLES), logical(1))),
         "chaque famille declaree existe dans FAMILLES")

verifier(setequal(names(INDICATEURS), ORDRE_INDICATEURS),
         "ORDRE_INDICATEURS couvre exactement le registre, sans oubli ni orphelin")

verifier(all(vapply(INDICATEURS,
                    function(i) nzchar(i$titre) && nzchar(i$methode) && nzchar(i$lecture),
                    logical(1))),
         "titre, methode et lecture sont renseignes partout")

verifier(all(vapply(INDICATEURS,
                    function(i) is.null(i$resume) || is.function(i$resume), logical(1))),
         "le commentaire automatique, quand il existe, est bien une fonction")


# ==============================================================================
groupe("Rendu interactif")
# ==============================================================================

verifier(is.function(graphe_est_interactif),
         "l'aiguillage entre rendu interactif et image existe")

# Les traces a double axe DOIVENT rester en image : ggplotly perd l'axe de
# droite sans lever d'erreur, ce qui produirait un graphique faux et plausible.
# Si quelqu'un retire un jour `interactif = FALSE` de ces entrees, ce test
# tombe — c'est precisement son role.
verifier(identical(INDICATEURS$ombro$interactif, FALSE),
         "le diagramme ombrothermique reste en image (double axe)")
verifier(identical(INDICATEURS$jja$interactif, FALSE),
         "les temperatures estivales restent en image (double axe)")

axes_secondaires <- c("ombro", "jja")
verifier(all(vapply(axes_secondaires,
                    function(k) !graphe_est_interactif(INDICATEURS[[k]]),
                    logical(1))),
         "l'aiguillage refuse bien l'interactif sur ces deux entrees")

# Tous les autres doivent, eux, etre convertibles.
don_i <- serie_synthetique(2000:2002, t_moy = 12)
ctx_i <- ctx_test()
echecs_conv <- character(0)
for (cle in setdiff(names(INDICATEURS), axes_secondaires)) {
  ind <- INDICATEURS[[cle]]
  tryCatch({
    g <- ind$plot(ind$calc(don_i, ctx_i), ctx_i, mobile = FALSE)
    w <- graphe_interactif(g, mobile = FALSE)
    if (!inherits(w, "plotly")) echecs_conv[[cle]] <- "n'est pas un objet plotly"
  }, error = function(e) echecs_conv[[cle]] <<- conditionMessage(e))
}
if (length(echecs_conv)) cat(sprintf("   conversion en echec : %s\n",
                                     paste(names(echecs_conv), collapse = ", ")))
verifier(length(echecs_conv) == 0,
         "tous les autres traces se convertissent en plotly")

# Les sous-titres sont perdus par ggplotly : le module les recupere. Encore
# faut-il pouvoir les lire sur l'objet ggplot.
g_st <- plot_temperature_journaliere(
  calc_temperature_journaliere(don_i, ctx_i), ctx_i)
verifier(!is.null(graphe_sous_titre(g_st)),
         "le sous-titre reste lisible sur l'objet ggplot avant conversion")


# ==============================================================================
groupe("Execution de bout en bout de tous les indicateurs")
# ==============================================================================
# Le test le plus rentable du lot : il fait tourner TOUS les indicateurs sur des
# donnees completes, et signale nommement celui qui echoue. Ajouter une entree
# au registre la soumet automatiquement a ce controle.

don_complet <- serie_synthetique(2000:2002, t_moy = 12, p_jour = 2)
ctx_complet <- ctx_test()

echecs_calc <- character(0)
echecs_plot <- character(0)
echecs_resume <- character(0)

for (cle in names(INDICATEURS)) {
  ind <- INDICATEURS[[cle]]

  res <- tryCatch(ind$calc(don_complet, ctx_complet),
                  error = function(e) { echecs_calc[[cle]] <<- conditionMessage(e); NULL })
  if (is.null(res)) next
  if (!is.data.frame(res)) echecs_calc[[cle]] <- "ne renvoie pas un data.frame"

  gr <- tryCatch(ind$plot(res, ctx_complet, mobile = FALSE),
                 error = function(e) { echecs_plot[[cle]] <<- conditionMessage(e); NULL })
  if (!is.null(gr) && !inherits(gr, "ggplot")) {
    echecs_plot[[cle]] <- "ne renvoie pas un objet ggplot"
  }

  if (is.function(ind$resume)) {
    tryCatch(ind$resume(res, ctx_complet),
             error = function(e) echecs_resume[[cle]] <<- conditionMessage(e))
  }
}

if (length(echecs_calc))   cat(sprintf("   calc en echec : %s\n", paste(names(echecs_calc), collapse = ", ")))
if (length(echecs_plot))   cat(sprintf("   plot en echec : %s\n", paste(names(echecs_plot), collapse = ", ")))
if (length(echecs_resume)) cat(sprintf("   resume en echec : %s\n", paste(names(echecs_resume), collapse = ", ")))

verifier(length(echecs_calc) == 0,   "tous les calculs s'executent et renvoient un data.frame")
verifier(length(echecs_plot) == 0,   "tous les traces s'executent et renvoient un ggplot")
verifier(length(echecs_resume) == 0, "tous les commentaires automatiques s'executent")

# Les memes, sur des donnees VIDES. C'est le scenario reel d'un point ou l'API
# n'a rien renvoye : aucun indicateur ne doit alors faire tomber la page.
vide <- don_complet[0, , drop = FALSE]
echecs_vide <- character(0)
for (cle in names(INDICATEURS)) {
  tryCatch({
    r <- INDICATEURS[[cle]]$calc(vide, ctx_complet)
    INDICATEURS[[cle]]$plot(r, ctx_complet, mobile = FALSE)
  }, error = function(e) echecs_vide[[cle]] <<- conditionMessage(e))
}
if (length(echecs_vide)) cat(sprintf("   en echec sur donnees vides : %s\n",
                                     paste(names(echecs_vide), collapse = ", ")))
verifier(length(echecs_vide) == 0,
         "aucun indicateur ne leve d'erreur sur un jeu de donnees vide")


# ==============================================================================
source("tests/test_stations.R")
source("tests/test_v12.R")
source("tests/test_v13.R")
source("tests/test_v14.R")
source("tests/test_v15.R")
source("tests/test_v16.R")
source("tests/test_v17.R")
source("tests/test_v171.R")
source("tests/test_v18.R")
source("tests/test_v19.R")
source("tests/test_v19_comparer.R")
source("tests/test_v20.R")
source("tests/test_v21.R")
source("tests/test_v22.R")
source("tests/test_v22_stations.R")
source("tests/test_v24.R")


# ==============================================================================
n_echecs <- bilan()
quit(status = if (n_echecs > 0) 1L else 0L)
