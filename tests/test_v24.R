# ==============================================================================
# tests/test_v24.R — SEUILS DE CHALEUR ET DE GEL, PREMIERS JOURS CHAUDS, GEL TARDIF (v2.4)
# ------------------------------------------------------------------------------
# Ce que la v2.4 change, et ce que ces tests gardent :
#   1. l'onglet Bilan climatique vient juste apres l'Accueil ;
#   2. quatre seuils de T max (25, 30, 35, 40 degC) et trois seuils de T min (0, -5, -10 degC),
#      comptes par INEGALITE STRICTE (un jour a 25,0 degC n'est pas « a plus de 25 degC ») ;
#   3. la date du premier jour a plus de 25 degC et du premier jour a plus de 30 degC ;
#   4. le gel tardif : un jour de gel POSTERIEUR au debourrement estime, jusqu'au 30 juin.
#      Les attendus se calculent a la main sur des series synthetiques.
# Aucun test n'appelle le reseau.
# ==============================================================================

ctx24 <- function(...) {
  seuils <- utils::modifyList(CONFIG$seuils, list(...))
  ctx_creer(ref = 2001:2002, affichage = 2001:2002, courante = 2002, fenetre = c(4, 10), seuils = seuils)
}


# ==============================================================================
groupe("v2.4 — l'onglet Bilan climatique vient juste apres l'Accueil")
# ==============================================================================

verifier(identical(PROFILS$dsf$onglets[1:2], c("accueil", "bilan")),
         "le profil permanent : Accueil, puis Bilan climatique")
verifier(setequal(PROFILS$dsf$onglets,
                  c("accueil", "bilan", "carte", "comparer", "synthese", "rapport", "parametres", "sources")),
         "aucun onglet perdu ni ajoute")
ordre24 <- onglets20(h_dsf)
ordre24 <- ordre24[ordre24 %in% PROFILS$dsf$onglets]
verifier(identical(ordre24[1:2], c("accueil", "bilan")),
         "dans la page, l'onglet Bilan climatique suit l'Accueil")


# ==============================================================================
groupe("v2.4 — seuils par defaut : T max 25, 30, 35, 40 ; T min 0, -5, -10")
# ==============================================================================

verifier(identical(unname(unlist(CONFIG$seuils[paste0("chaleur_t", 1:4)])), c(25, 30, 35, 40)),
         "quatre seuils de chaleur : 25, 30, 35 et 40 °C")
verifier(identical(unname(unlist(CONFIG$seuils[c("gel_t", "gel_severe_t", "gel_extreme_t")])), c(0, -5, -10)),
         "trois seuils de gel : 0, -5 et -10 °C")
verifier(!"gel_tardif_debut" %in% names(CONFIG$seuils) && !"gel_tardif_debut" %in% DESCRIPTIF_SEUILS$id,
         "la fenetre de dates fixe du gel tardif n'existe plus")
verifier(all(DESCRIPTIF_SEUILS$id %in% names(CONFIG$seuils)) &&
           all(c("gel_extreme_t", "gel_tardif_t", "gel_tardif_gdd", "gel_tardif_fin", "chaleur_t4") %in% DESCRIPTIF_SEUILS$id),
         "les nouveaux seuils sont reglables dans Parametres")
verifier(all(vapply(DESCRIPTIF_SEUILS$id, function(i) {
  j <- match(i, DESCRIPTIF_SEUILS$id)
  v <- CONFIG$seuils[[i]]
  is.finite(v) && v >= DESCRIPTIF_SEUILS$min[j] && v <= DESCRIPTIF_SEUILS$max[j]
}, TRUE)), "la valeur par defaut de chaque seuil est dans ses bornes")
p24 <- partage_lire_seuils("chaleur_t4:38,gel_extreme_t:-12,gel_tardif_gdd:250,gel_tardif_t:-2")
verifier(identical(p24$valeurs, list(chaleur_t4 = 38, gel_extreme_t = -12, gel_tardif_gdd = 250, gel_tardif_t = -2)) &&
           !length(p24$ecartes),
         "un lien partage peut regler les nouveaux seuils")
verifier(length(partage_lire_seuils("gel_tardif_debut:100")$ecartes) == 1L,
         "l'ancien seuil de debut de fenetre est ecarte d'un lien ancien, sans erreur")


# ==============================================================================
groupe("v2.4 — jours de chaleur : quatre seuils, inegalite stricte")
# ==============================================================================

d24 <- serie_synthetique(2001, t_moy = 10, amplitude = 5)      # t_max = 15 partout
d24$t_max[d24$doy == 100] <- 25                                # exactement 25 : non compte
d24$t_max[d24$doy == 101] <- 25.1
d24$t_max[d24$doy == 150] <- 30.5
d24$t_max[d24$doy %in% c(200, 201)] <- c(35.5, 36)
d24$t_max[d24$doy == 220] <- 40.2
ch24 <- calc_chaleur(d24, ctx24())
verifier(isTRUE(all.equal(c(ch24$n_t1, ch24$n_t2, ch24$n_t3, ch24$n_t4), c(5, 4, 3, 1))),
         "25,0 °C n'est pas « plus de 25 °C » : 5, 4, 3 et 1 jours aux seuils 25, 30, 35 et 40")
ch24b <- calc_chaleur(d24, ctx24(chaleur_t4 = 36))
verifier(isTRUE(all.equal(ch24b$n_t4, 1)),
         "le quatrieme seuil se regle : plus de 36 °C -> 1 jour (36,0 est exclu)")
verifier(all(c("n_t1", "n_t2", "n_t3", "n_t4") %in% INDICATEURS$can$colonnes),
         "la fiche des jours de chaleur porte les quatre colonnes")
verifier(grepl("40 °C", INDICATEURS$can$methode) && grepl("Zhang et al. 2011", INDICATEURS$can$methode),
         "la fiche cite les quatre seuils et l'indice SU")


# ==============================================================================
groupe("v2.4 — premier jour a plus de 25 °C et a plus de 30 °C")
# ==============================================================================

d24a <- d24
d24a$t_max[d24a$doy == 99] <- NA                               # un jour sans mesure n'est pas le premier jour
d24b <- serie_synthetique(2002, t_moy = 10, amplitude = 5)     # 2002 : jamais plus de 25 °C
pj <- calc_premiers_jours_chauds(rbind(d24a, d24b), ctx24())
verifier(isTRUE(all.equal(pj$doy_t1[pj$annee == 2001], 101)) && isTRUE(all.equal(pj$doy_t2[pj$annee == 2001], 150)),
         "2001 : premier jour > 25 °C le jour 101 (le jour 100, a 25,0 °C, est exclu), > 30 °C le jour 150")
verifier(is.na(pj$doy_t1[pj$annee == 2002]) && is.na(pj$doy_t2[pj$annee == 2002]),
         "2002 : seuil jamais atteint -> date vide (et non zero)")
verifier(isTRUE(all.equal(pj$normale[1], 101)),
         "la normale est la moyenne des dates des seules annees ou le seuil est atteint")
verifier(all(c("annee", "doy_t1", "doy_t2", "normale", "anomalie", "n_manquants", "partielle") %in% names(pj)),
         "colonnes de completude presentes")
pj2 <- calc_premiers_jours_chauds(rbind(d24a, d24b), ctx24(chaleur_t1 = 15.5, chaleur_t2 = 25))
verifier(isTRUE(all.equal(pj2$doy_t1[pj2$annee == 2001], 100)) && isTRUE(all.equal(pj2$doy_t2[pj2$annee == 2001], 101)),
         "les seuils du premier jour sont ceux des jours de chaleur (reglables) : > 15,5 °C le jour 100, > 25 °C le jour 101")
verifier(identical(doy_date_fr(c(1, 60, 100, 181, 365)), c("1 janvier", "1 mars", "10 avril", "30 juin", "31 décembre")) &&
           is.na(doy_date_fr(NA_real_)),
         "le jour de l'annee s'ecrit en toutes lettres (jour 60 = 1er mars)")
ctx_2001 <- ctx24(); ctx_2001$courante <- 2001L
r_pj <- INDICATEURS$premchaud$resume(pj, ctx_2001)
verifier(grepl("le 11 avril", r_pj) && grepl("le 30 mai", r_pj) && grepl("Date normale 2001-2002", r_pj),
         "le resume dit les deux dates et la date normale du premier jour a plus de 25 °C")
verifier(grepl("jamais atteint", INDICATEURS$premchaud$resume(pj, ctx24())),
         "une annee complete sans date : « jamais atteint »")
verifier(grepl("pas encore atteint",
               INDICATEURS$premchaud$resume(transform(pj, partielle = TRUE), ctx24())),
         "une annee en cours sans date : « pas encore atteint »")


# ==============================================================================
groupe("v2.4 — jours de gel : trois seuils, inegalite stricte")
# ==============================================================================

g24 <- serie_synthetique(2001, t_moy = 10, amplitude = 5)      # t_min = 5 partout
g24$t_min[g24$doy == 10] <- 0                                  # = 0 : non compte
g24$t_min[g24$doy == 11] <- -0.1
g24$t_min[g24$doy == 12] <- -5                                 # = -5 : non compte au seuil -5
g24$t_min[g24$doy == 13] <- -5.1
g24$t_min[g24$doy == 14] <- -10                                # = -10 : non compte au seuil -10
g24$t_min[g24$doy == 15] <- -12
gg24 <- calc_gel(g24, ctx24())
verifier(isTRUE(all.equal(c(gg24$n_gel, gg24$n_gel_severe, gg24$n_gel_extreme), c(5, 3, 1))),
         "gel : 5 jours sous 0 °C, 3 sous -5 °C, 1 sous -10 °C (les valeurs egales au seuil sont exclues)")
verifier(!"n_gel_tardif" %in% names(gg24) && "doy_dernier_gel" %in% names(gg24),
         "le tableau du gel ne porte plus le gel tardif (indicateur a part), mais garde le dernier gel")
verifier(all(c("n_gel", "n_gel_severe", "n_gel_extreme") %in% INDICATEURS$gel$colonnes) &&
           grepl("-10 °C", INDICATEURS$gel$methode),
         "la fiche du gel porte les trois colonnes et cite le seuil de -10 °C")


# ==============================================================================
groupe("v2.4 — debourrement estime : somme de degres-jours depuis le 1er janvier")
# ==============================================================================

deb <- function(t_moy, seuils = CONFIG$seuils, annees = 2001, na_premiers = 0L) {
  d <- serie_synthetique(annees, t_moy = t_moy)
  if (na_premiers > 0L) d$t_moy[d$doy <= na_premiers] <- NA
  debourrement_estime(d, seuils)
}
verifier(deb(15)$doy_debourrement == 20,
         "15 °C : 10 degres-jours par jour, 200 atteints le jour 20 (la somme atteinte, egalite incluse)")
verifier(deb(12)$doy_debourrement == 29,
         "12 °C : 7 par jour ; 28 jours font 196, 29 jours font 203 -> jour 29")
verifier(deb(40)$doy_debourrement == 8,
         "plafond de 30 °C : 25 degres-jours par jour, jour 8 (40 °C ne compterait pas 35 par jour)")
verifier(is.na(deb(3)$doy_debourrement),
         "sous la base (5 °C) : aucun degre-jour, debourrement jamais atteint -> NA")
verifier(deb(12, na_premiers = 5L)$doy_debourrement == 34,
         "un jour sans temperature moyenne n'ajoute rien a la somme : debourrement estime plus tard")
verifier(deb(12, utils::modifyList(CONFIG$seuils, list(gel_tardif_gdd = 400)))$doy_debourrement == 58,
         "la somme requise se regle : 400 degres-jours -> jour 58")
verifier(deb(12, utils::modifyList(CONFIG$seuils, list(gdd_base = 10)))$doy_debourrement == 100,
         "la base des degres-jours est celle de l'indicateur GDD : base 10 °C, 2 par jour -> jour 100")
dd <- debourrement_estime(rbind(serie_synthetique(2001, t_moy = 12), serie_synthetique(2002, t_moy = 8)), CONFIG$seuils)
verifier(identical(dd$annee, c(2001L, 2002L)) && dd$doy_debourrement[1] == 29 && dd$doy_debourrement[2] == 67,
         "chaque annee a sa date : une annee plus froide debourre plus tard (8 °C : 3 par jour -> jour 67)")
verifier(nrow(debourrement_estime(serie_synthetique(2001)[0, ], CONFIG$seuils)) == 0L,
         "aucune ligne : aucun debourrement, sans erreur")


# ==============================================================================
groupe("v2.4 — gel tardif : un gel POSTERIEUR au debourrement estime, au plus le jour 181")
# ==============================================================================

t24 <- serie_synthetique(2001, t_moy = 12, amplitude = 4)      # t_min = 8 ; debourrement estime : jour 29
t24$t_min[t24$doy %in% c(20, 29, 30, 120, 181, 182, 250)] <- -1
t24$t_min[t24$doy == 31] <- 0                                  # = 0 : pas un gel
gt <- calc_gel_tardif(t24, ctx24())
verifier(gt$doy_debourrement[1] == 29, "debourrement estime au jour 29")
verifier(gt$n_gel_tardif[1] == 3,
         "gel tardif : jours 30, 120 et 181 ; pas le jour 20 (avant), ni le jour 29 (le jour meme), ni 182 et 250 (apres la saison), ni 31 (0,0 °C)")
verifier(calc_gel(t24, ctx24())$n_gel[1] == 7,
         "les sept jours de gel sont tous comptes par l'indicateur du gel : le gel tardif en est une partie")
verifier(calc_gel_tardif(t24, ctx24(gel_tardif_t = -2))$n_gel_tardif[1] == 0,
         "seuil de gel tardif a -2 °C : un gel a -1 °C n'est plus compte (seuil reglable)")
verifier(calc_gel_tardif(t24, ctx24(gel_tardif_gdd = 400))$n_gel_tardif[1] == 2,
         "debourrement plus tardif (400 degres-jours, jour 58) : 120 et 181 seulement")
verifier(calc_gel_tardif(t24, ctx24(gel_tardif_gdd = 100))$n_gel_tardif[1] == 5,
         "debourrement plus precoce (100 degres-jours, jour 15) : les jours 20 et 29 comptent aussi")
verifier(calc_gel_tardif(t24, ctx24(gel_tardif_fin = 150))$n_gel_tardif[1] == 2,
         "fin de saison avancee au jour 150 : jours 30 et 120")

froid <- serie_synthetique(2001, t_moy = 3, amplitude = 5)     # t_min = -2 tout l'an, jamais de debourrement
gf <- calc_gel_tardif(froid, ctx24())
verifier(gf$n_gel_tardif[1] == 0 && is.na(gf$doy_debourrement[1]) && calc_gel(froid, ctx24())$n_gel[1] == 365,
         "debourrement jamais atteint : zero jour de gel tardif et date vide, alors que tous les jours gelent")

deux <- rbind(serie_synthetique(2001, t_moy = 12, amplitude = 4), serie_synthetique(2002, t_moy = 8, amplitude = 4))
deux$t_min[deux$doy == 40] <- -1                               # le meme gel, le jour 40, deux annees de suite
g2 <- calc_gel_tardif(deux, ctx24())
verifier(g2$n_gel_tardif[g2$annee == 2001] == 1 && g2$n_gel_tardif[g2$annee == 2002] == 0,
         "le meme gel est tardif apres un debourrement precoce (jour 29), pas apres un debourrement tardif (jour 67)")

verifier(all(INDICATEURS$geltd$colonnes %in% names(gt)) && all(INDICATEURS$premchaud$colonnes %in% names(pj)) &&
           all(INDICATEURS$gel$colonnes %in% names(gg24)) && all(INDICATEURS$can$colonnes %in% names(ch24)),
         "les colonnes annoncees par le registre existent dans les tableaux calcules")
verifier(all(c("geltd", "premchaud") %in% ORDRE_INDICATEURS) && all(c("geltd", "premchaud") %in% names(REQUIS)),
         "les nouveaux indicateurs sont ordonnes et declarent leurs variables requises")


# ==============================================================================
groupe("v2.4 — la definition du gel tardif est ecrite, sourcee, et ses limites dites")
# ==============================================================================

m_gt <- INDICATEURS$geltd$methode
verifier(grepl("DÉFINITION", m_gt) && grepl("postérieur au débourrement estimé", m_gt) && grepl("30 juin", m_gt),
         "la fiche pose la definition : seuil, apres le debourrement estime, jusqu'au 30 juin")
verifier(all(vapply(c("Bigler et Bugmann 2018", "Lenz et al. 2013", "Vitasse et al. 2018b", "Delpierre et al. 2016"),
                    function(r) grepl(r, m_gt, fixed = TRUE), TRUE)),
         "la fiche cite Bigler et Bugmann 2018, Lenz et al. 2013, Vitasse et al. 2018b et Delpierre et al. 2016")
verifier(grepl("LIMITES", m_gt) && grepl("INDICATIVE", m_gt) && grepl("exposition, non", m_gt),
         "la fiche dit ses limites : somme de degres-jours indicative, seuil qui compte l'exposition et non le degat")
verifier(all(c("bigler2018", "lenz2013", "vitasse2018gel", "delpierre2016", "vitasse2018") %in% REFERENCES$id) &&
           !anyDuplicated(REFERENCES$court),
         "les references sont dans la table, avec des formes courtes distinctes (Vitasse 2018a et 2018b)")
verifier(grepl("doi:10.1038/s41598-018-27893-1", REFERENCES$citation[REFERENCES$id == "bigler2018"], fixed = TRUE) &&
           grepl("doi:10.1111/nph.12452", REFERENCES$citation[REFERENCES$id == "lenz2013"], fixed = TRUE) &&
           grepl("doi:10.1016/j.agrformet.2017.09.005", REFERENCES$citation[REFERENCES$id == "vitasse2018gel"], fixed = TRUE) &&
           grepl("doi:10.1007/s13595-015-0477-6", REFERENCES$citation[REFERENCES$id == "delpierre2016"], fixed = TRUE),
         "chaque reference nouvelle porte son DOI")
i_gt <- match(c("gel_tardif_t", "gel_tardif_gdd", "gel_tardif_fin"), DESCRIPTIF_SEUILS$id)
verifier(all(!is.na(i_gt)) && DESCRIPTIF_SEUILS$statut[i_gt[2]] == "convention" &&
           grepl("indicative", DESCRIPTIF_SEUILS$aide[i_gt[2]]),
         "les 200 degres-jours sont presentes comme une convention indicative, non comme un resultat")
verifier(grepl("Bigler", DESCRIPTIF_SEUILS$aide[i_gt[1]]) && grepl("-7,5", DESCRIPTIF_SEUILS$aide[i_gt[1]]),
         "l'aide du seuil de gel tardif cite la plage de seuils de dommage de Bigler et Bugmann")
verifier(grepl("débourrement estimé", GLOSSAIRE$definition[GLOSSAIRE$terme == "Gel tardif"]),
         "le glossaire definit le gel tardif par le debourrement estime")


# ==============================================================================
groupe("v2.4 — la synthese a periode egale compte comme les indicateurs annuels")
# ==============================================================================

set.seed(24)
saison24 <- function(doy, a) 11 + 9 * sin((doy - 110) / 365 * 2 * pi) + stats::rnorm(length(doy), 0, 3)
don_s24 <- serie_synthetique(2001:2006, t_moy = saison24, p_jour = 1, amplitude = 5)
ctx_s24 <- ctx_creer(ref = 2001:2005, affichage = 2001:2006, courante = 2006, fenetre = c(4, 10))
s24 <- calc_synthese(don_s24, ctx_s24)
val24 <- function(motif) s24$valeur[grepl(motif, s24$indicateur)]
verifier(all(vapply(c("T max > 25", "T max > 30", "T max > 35", "T max > 40",
                      "T min < 0 ", "T min < -5", "T min < -10", "Jours de gel tardif"),
                    function(m) sum(grepl(m, s24$indicateur, fixed = TRUE)) == 1L, TRUE)),
         "la synthese porte les quatre seuils de chaleur, les trois seuils de gel et le gel tardif")
tout24 <- list(can = calc_chaleur(don_s24, ctx_s24), gel = calc_gel(don_s24, ctx_s24),
               geltd = calc_gel_tardif(don_s24, ctx_s24))
cur24 <- function(t, col) t[[col]][t$annee == 2006]
verifier(isTRUE(all.equal(c(val24("T max > 25"), val24("T max > 30"), val24("T max > 35"), val24("T max > 40")),
                          c(cur24(tout24$can, "n_t1"), cur24(tout24$can, "n_t2"), cur24(tout24$can, "n_t3"),
                            cur24(tout24$can, "n_t4")))),
         "annee entiere : les jours de chaleur de la synthese sont ceux de calc_chaleur")
verifier(isTRUE(all.equal(c(val24("T min < 0 "), val24("T min < -5"), val24("T min < -10")),
                          c(cur24(tout24$gel, "n_gel"), cur24(tout24$gel, "n_gel_severe"), cur24(tout24$gel, "n_gel_extreme")))),
         "annee entiere : les jours de gel de la synthese sont ceux de calc_gel")
verifier(isTRUE(all.equal(val24("Jours de gel tardif"), cur24(tout24$geltd, "n_gel_tardif"))),
         "annee entiere : le gel tardif de la synthese est celui de calc_gel_tardif (meme definition)")
verifier(isTRUE(all.equal(s24$normale[grepl("Jours de gel tardif", s24$indicateur)],
                          mean(tout24$geltd$n_gel_tardif[tout24$geltd$annee %in% 2001:2005]))),
         "la normale du gel tardif est la moyenne des annees de la reference")
verifier(identical(unique(s24$famille[grepl("T min <|gel tardif", s24$indicateur)]), "gel") &&
           all(s24$sens[grepl("T min <|gel tardif", s24$indicateur)] == "neutre"),
         "les lignes de gel sont de la famille gel et n'ont pas de sens favorable")


# ==============================================================================
groupe("v2.4 — graphiques : quatre et trois seuils, gel tardif, premiers jours")
# ==============================================================================

ctx_g24 <- ctx_creer(ref = 2001:2005, affichage = 2001:2006, courante = 2006, fenetre = c(4, 10))
res_g24 <- list(gel = tout24$gel, geltd = tout24$geltd, can = tout24$can,
                premchaud = calc_premiers_jours_chauds(don_s24, ctx_g24))
fait24 <- vapply(names(res_g24), function(id) {
  g <- INDICATEURS[[id]]$plot(res_g24[[id]], ctx_g24, mobile = FALSE)
  inherits(g, "ggplot") && inherits(tryCatch(ggplot2::ggplot_build(g), error = function(e) e), "ggplot_built")
}, TRUE)
verifier(all(fait24), "les quatre graphiques se construisent sur une serie realiste")
leg24 <- ggplot2::ggplot_build(INDICATEURS$gel$plot(res_g24$gel, ctx_g24))$plot$scales$get_scales("fill")$get_labels()
verifier(identical(as.character(leg24), c("T min < 0 °C", "T min < -5 °C", "T min < -10 °C")),
         "la legende du gel : 0, -5 et -10 °C")
leg24c <- ggplot2::ggplot_build(INDICATEURS$can$plot(res_g24$can, ctx_g24))$plot$scales$get_scales("fill")$get_labels()
verifier(identical(as.character(leg24c), c("T max > 25 °C", "T max > 30 °C", "T max > 35 °C", "T max > 40 °C")),
         "la legende de la chaleur : 25, 30, 35 et 40 °C")
vide24 <- res_g24$premchaud
vide24$doy_t1 <- NA_real_; vide24$doy_t2 <- NA_real_
verifier(inherits(INDICATEURS$premchaud$plot(vide24, ctx_g24), "ggplot"),
         "premiers jours : un graphique d'explication, pas une erreur, quand aucun seuil n'est atteint")
verifier(identical(unname(vapply(c("geltd", "premchaud"), function(i) nrow(INDICATEURS[[i]]$calc(serie_synthetique(2001)[0, ], ctx24())), 1L)),
                   c(0L, 0L)),
         "les deux nouveaux calculs rendent un tableau vide sur un jeu vide, sans erreur")


# ==============================================================================
groupe("v2.4 — cartes et base precalculee : colonnes et couches")
# ==============================================================================

verifier(all(c("can1", "can", "can3", "can4", "chaud1", "chaud2", "gel", "gel5", "gel10", "geltd") %in% names(CARTE_COUCHES)),
         "la carte porte les quatre seuils de chaleur, les trois de gel, le gel tardif et les premiers jours")
verifier(identical(CARTE_COUCHES$geltd$source, "geltd") && identical(CARTE_COUCHES$chaud1$source, "premchaud"),
         "les couches nouvelles s'appuient sur les nouveaux indicateurs")
verifier(identical(BASE_INDICATEURS$can$col, c("n_t1", "n_t2", "n_t3", "n_t4")) &&
           identical(BASE_INDICATEURS$gel$col, c("n_gel", "n_gel_severe", "n_gel_extreme", "doy_dernier_gel")) &&
           identical(BASE_INDICATEURS$geltd$col, c("n_gel_tardif", "doy_debourrement")) &&
           identical(BASE_INDICATEURS$premchaud$col, c("doy_t1", "doy_t2")),
         "la base lit les colonnes des quatre indicateurs")
verifier(!is.na(base_cle_colonne("can", "n_t4")) && !is.na(base_cle_colonne("gel", "n_gel_extreme")) &&
           !is.na(base_cle_colonne("geltd", "doy_debourrement")) && !is.na(base_cle_colonne("premchaud", "doy_t2")),
         "chaque colonne nouvelle est publiee par une couche ou une colonne de la base")
annuelles24 <- Filter(function(k) identical(k$type %||% "annuel", "annuel"), c(CARTE_COUCHES, BASE_COUCHES))
verifier(!anyDuplicated(vapply(annuelles24, function(k) paste(k$source, k$nom %||% k$colonne), "")),
         "aucune colonne annuelle n'est publiee deux fois")
