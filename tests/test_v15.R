# ==============================================================================
# tests/test_v15.R — RIGUEUR SCIENTIFIQUE, VERSION 1.5 (audit, partie 6)
# ------------------------------------------------------------------------------
# Neuf constats, sept groupes :
#   M1, M8  le VPD suit la FAO-56 ; les valeurs attendues sont ECRITES EN DUR, y
#           compris l'exemple de la FAO, et ne sortent d'aucune copie de la formule ;
#   M8      Hargreaves-Samani, pente de Sen, synthese chiffree : verifies sur des
#           cas calculables a la main (l'audit n'en trouvait aucun) ;
#   M5      le seuil de VPD ne pretend plus decrire la fermeture des stomates ;
#   M6      chaque seuil a un statut et des references qui existent ;
#   M7      le gel tardif dit ce que sa fenetre ne sait pas ;
#   M9      1961-1990 n'est plus dite « pre-industrielle du GIEC ».
# M2, M3 et la part « intervalle » de M4 (tendance corrigee, 20 ans, IC de Sen)
# sont des acquis de la v1.2, testes dans test_v12.R.
#
# POURQUOI DES VALEURS EN DUR
# ---------------------------
# Un test qui recalcule la formule du code avec la formule du code ne peut pas
# echouer le jour ou la formule est fausse : c'etait M8. Ici, chaque nombre
# attendu vient d'une source exterieure (exemple FAO-56, tableau de l'audit,
# calcul a la main reproduit en commentaire).
# ==============================================================================

html_de15 <- function(ui) paste(as.character(htmltools::renderTags(ui)$html), collapse = "\n")
lire_src <- function(f) paste(readLines(chemin_ci(f), encoding = "UTF-8", warn = FALSE), collapse = "\n")


# ==============================================================================
groupe("v1.5 — VPD : FAO-56, valeurs attendues en dur (M1, M8)")
# ==============================================================================

# Pression de vapeur saturante : table de la FAO-56 (annexe 2, tableau 2.3).
verifier(presque_egal(pression_saturante(25), 3.168, tolerance = 1e-3) &&
           presque_egal(pression_saturante(18), 2.064, tolerance = 1e-3),
         "pression de vapeur saturante : e°(25) = 3,168 et e°(18) = 2,064 kPa (FAO-56)")

# Exemple FAO-56 : Tn 18, Tx 25 degC, HU moyenne 68 %
#   es = (3,168 + 2,064) / 2 = 2,616 ; ea = 0,68 * 2,616 = 1,779 ; VPD = 0,837
vpd_fao <- vpd_fao56(18, 25, 68)
verifier(presque_egal(vpd_fao, 0.8371, tolerance = 5e-4),
         "exemple FAO-56 (Tn 18, Tx 25, HU 68 %) : VPD = 0,837 kPa")
verifier(presque_egal(2.6159 - vpd_fao, 1.7788, tolerance = 5e-4),
         "... soit une pression de vapeur reelle ea = 1,7788 kPa, la valeur de l'audit")

# Les cinq situations du tableau de l'audit. Valeurs FAO-56 attendues (kPa) ; on
# verifie aussi que l'ancienne formule (ea prise sur es(Tmoy)) en est bien
# eloignee de plus de 5 % : sans cela, ce test ne verrait pas M1.
cas <- data.frame(tn = c(-2, 5, 12, 16, 22), tx = c(6, 17, 28, 34, 38),
                  hu = c(85, 65, 55, 45, 38),
                  attendu = c(0.110, 0.492, 1.166, 1.963, 2.873))
calcule <- vpd_fao56(cas$tn, cas$tx, cas$hu)
verifier(all(abs(calcule - cas$attendu) < 6e-4),
         "VPD de l'hiver a la canicule : 0,110 / 0,492 / 1,166 / 1,963 / 2,873 kPa")
ancien <- pmax((pression_saturante(cas$tx) + pression_saturante(cas$tn)) / 2 -
                 pression_saturante((cas$tn + cas$tx) / 2) * cas$hu / 100, 0)
verifier(all(ancien / calcule - 1 > 0.05) && max(ancien / calcule - 1) < 0.21,
         "l'ancienne formule surestimait de 5 a 20 % : ce test l'aurait vue (M1)")

verifier(presque_egal(vpd_fao56(10, 20, 100), 0) && presque_egal(vpd_fao56(10, 20, 120), 0),
         "air sature, ou humidite superieure a 100 % : VPD nul, jamais negatif")
verifier(is.na(vpd_fao56(10, 20, NA)) && is.na(vpd_fao56(NA, 20, 50)),
         "une entree manquante donne NA, pas zero")
verifier(all(diff(vpd_fao56(10, 25, c(20, 40, 60, 80))) < 0),
         "a temperatures egales, le VPD decroit quand l'humidite croit")

# Le VPD ne depend QUE de Tmin, Tmax et HU : changer la temperature moyenne
# (colonne distincte dans SAFRAN) ne le modifie pas.
d_a <- data.frame(p_liq = 1, p_sol = 0, t_moy = 10, t_min = 5, t_max = 15, hu = 60)
d_b <- d_a; d_b$t_moy <- 14
verifier(presque_egal(safran_deriver(d_a)$vpd, safran_deriver(d_b)$vpd),
         "le VPD journalier ne depend pas de t_moy")
d_c <- d_a; d_c$vpd <- 999
verifier(safran_deriver(d_c)$vpd < 5,
         "une colonne vpd heritee d'un ancien cache est recalculee, pas conservee")

# Le jeu de test utilise la vraie fonction, et pas une copie de la formule.
syn <- serie_synthetique(2001, t_moy = 20, amplitude = 7, hu = 60)
# e°(13) = 1,498 ; e°(27) = 3,567 ; es = 2,532 ; VPD = 0,4 * 2,532 = 1,013 kPa
verifier(presque_egal(unique(syn$vpd), vpd_fao56(13, 27, 60)) &&
           presque_egal(unique(syn$vpd), 1.013, tolerance = 1e-3),
         "le jeu synthetique porte le VPD de l'application (Tmin 13, Tx 27, HU 60 % : 1,013 kPa)")

# Effet sur le decompte de jours au-dessus du seuil : sur des etes simules a
# amplitude courante (14 degC jour-nuit), l'ancienne formule comptait pres de
# deux fois plus de jours >= 1,5 kPa (tests/simulation_vpd.R).
simuler <- function(graine) {
  set.seed(graine); n <- 30 * 92
  tm <- stats::rnorm(n, 20.5, 3.2); amp <- pmax(stats::rnorm(n, 7, 2), 0.5)
  hu <- pmin(pmax(stats::rnorm(n, 64, 11), 5), 100)
  tn <- tm - amp; tx <- tm + amp
  va <- pmax((pression_saturante(tx) + pression_saturante(tn)) / 2 -
               pression_saturante(tm) * hu / 100, 0)
  vn <- vpd_fao56(tn, tx, hu)
  c(m_a = mean(va), m_n = mean(vn), j_a = sum(va >= 1.5), j_n = sum(vn >= 1.5))
}
sim <- colMeans(t(vapply(1:60, simuler, numeric(4))))
verifier(abs(sim[["m_a"]] - 1.10) < 0.03 && abs(sim[["m_n"]] - 0.96) < 0.03,
         "etes simules : VPD moyen 1,10 kPa (v1.4) -> 0,96 kPa (v1.5), comme mesure par l'audit")
verifier(sim[["j_a"]] / sim[["j_n"]] > 1.6,
         "... et pres de deux fois moins de jours au-dessus de 1,5 kPa")


# ==============================================================================
groupe("v1.5 — Hargreaves-Samani : exemple FAO-56 et proprietes (M8)")
# ==============================================================================

# FAO-56, exemple 8 : Ra = 32,2 MJ m-2 j-1 a 20 °S le 3 septembre (jour 246).
# ET0 = 0,0023 (Tmoy + 17,8) (Tmax - Tmin)^0,5 Ra * 0,408   (eq. 52)
#     = 0,0023 * 37,8 * sqrt(10) * 32,2 * 0,408 = 3,612 mm/j
et0 <- etp_hargreaves(t_moy = 20, t_min = 15, t_max = 25, doy = 246, lat = -20)
verifier(presque_egal(et0, 0.0023 * 37.8 * sqrt(10) * 32.2 * 0.408, tolerance = 0.01),
         "Ra de l'exemple 8 de la FAO-56 (32,2 MJ/m2/j) : ETP = 3,61 mm/j a 0,3 % pres")
verifier(etp_hargreaves(20, 20, 20, 180, 45) == 0,
         "sans amplitude thermique, l'ETP est nulle")
verifier(etp_hargreaves(20, 14, 26, 196, 45) > etp_hargreaves(20, 14, 26, 15, 45),
         "a 45 °N, l'ETP de juillet depasse celle de janvier a temperatures egales")
verifier(etp_hargreaves(25, 19, 31, 196, 45) > etp_hargreaves(15, 9, 21, 196, 45),
         "l'ETP croit avec la temperature moyenne")
et_ete <- etp_hargreaves(22, 15, 29, 196, 45)
verifier(et_ete > 4.5 && et_ete < 7.5,
         "ordre de grandeur d'un jour de juillet a 45 °N : 4,5 a 7,5 mm/j")
verifier(etp_hargreaves(-5, -8, -2, 355, 80) == 0 && all(etp_hargreaves(0, -3, 3, c(1, 100, 200, 300), 48) >= 0),
         "nuit polaire : ETP nulle ; elle n'est jamais negative")


# ==============================================================================
groupe("v1.5 — tendance : calcul a la main (M8)")
# ==============================================================================

# y = (1, 3, 2, 5, 4) pour les annees 1 a 5. Les dix pentes entre paires :
#   2 ; 0,5 ; 1,333 ; 0,75 ; -1 ; 1 ; 0,333 ; 3 ; 1 ; -1
# triees : -1 -1 0,333 0,5 0,75 1 1 1,333 2 3 ; mediane = (0,75 + 1) / 2 = 0,875
# par an, soit 8,75 par decennie. S de Mann-Kendall = 6.
tr_main <- tendance(1:5, c(1, 3, 2, 5, 4), "u", n_min = 5L)
verifier(isTRUE(tr_main$ok) && presque_egal(tr_main$pente_dec, 8.75, tolerance = 1e-9),
         "pente de Sen calculee a la main : 8,75 par decennie")

# Droite parfaite : pente exacte, intervalle de confiance qui la contient, p minuscule.
a_lin <- 1980:2019
tr_lin <- tendance(a_lin, 2 + 0.05 * (a_lin - 1980), "u")
verifier(presque_egal(tr_lin$pente_dec, 0.5, tolerance = 1e-9) &&
           tr_lin$ic_bas_dec <= 0.5 + 1e-9 && tr_lin$ic_haut_dec >= 0.5 - 1e-9 && tr_lin$p < 1e-6,
         "serie lineaire parfaite : 0,5 par decennie, IC contenant la pente, p < 1e-6")
verifier(isTRUE(tr_lin$significative), "... et la tendance est declaree significative")

# Duree minimale (M3) : 19 annees refusees, 20 acceptees.
verifier(!isTRUE(tendance(1991:2009, seq_len(19) * 0.1, "u")$ok) &&
           isTRUE(tendance(1990:2009, seq_len(20) * 0.1, "u")$ok),
         "pas de tendance sous 20 annees completes, une tendance des 20")
verifier(grepl("20 au minimum", tendance(1991:2009, seq_len(19), "u")$libelle),
         "le refus dit pourquoi : « 20 au minimum »")

# Serie constante : pente nulle, jamais significative (pas de division par zero).
tr_cst <- tendance(1990:2019, rep(7, 30), "u")
verifier(isTRUE(tr_cst$ok) && presque_egal(tr_cst$pente_dec, 0) && !isTRUE(tr_cst$significative),
         "serie constante : pente nulle, non significative")


# ==============================================================================
groupe("v1.5 — synthese chiffree : valeurs exactes (M8)")
# ==============================================================================

# Annees ordinaires seulement (une annee bissextile cumulerait un jour de plus).
# Reference 2001-2003 : 10 degC et 2 mm/j ; annee comparee 2005 : 12 degC, 3 mm/j.
don_s <- rbind(serie_synthetique(2001:2003, t_moy = 10, p_jour = 2),
               serie_synthetique(2005,      t_moy = 12, p_jour = 3))
ctx_s <- ctx_test(ref = 2001:2003, courante = 2005, affichage = 2001:2005)
syn_s <- calc_synthese(don_s, ctx_s)
lg_t <- syn_s[syn_s$indicateur == "Température moyenne", ]
verifier(nrow(lg_t) == 1L && presque_egal(lg_t$valeur, 12) && presque_egal(lg_t$normale, 10) &&
           presque_egal(lg_t$ecart, 2),
         "temperature moyenne : 12 °C contre une normale de 10 °C, ecart de +2")
lg_p <- syn_s[grepl("^Précipitations totales", syn_s$indicateur), ][1, ]
verifier(presque_egal(lg_p$valeur, 3 * 365) && presque_egal(lg_p$normale, 2 * 365) &&
           presque_egal(lg_p$ecart, 365),
         "precipitations : 1 095 mm contre une normale de 730 mm, ecart de +365 mm")
verifier(!is.finite(lg_t$sd_ref) || presque_egal(lg_t$sd_ref, 0),
         "reference constante : ecart-type nul (ou non defini)")


# ==============================================================================
groupe("v1.5 — le seuil de VPD ne decrit plus la fermeture des stomates (M5)")
# ==============================================================================

lg_vpd <- DESCRIPTIF_SEUILS[DESCRIPTIF_SEUILS$id == "vpd_stress_kpa", ]
verifier(!grepl("fermeture", lg_vpd$libelle) && grepl("moyenne journali", lg_vpd$libelle),
         "le libelle du seuil dit qu'il s'applique a une moyenne journaliere")
verifier(grepl("MOYEN", lg_vpd$aide) && grepl("progressive", lg_vpd$aide),
         "l'aide dit que le VPD est une moyenne et que la fermeture est progressive")
verifier(!grepl("ferment leurs stomates et cessent", INDICATEURS$vpd$lecture) &&
           grepl("conventionnel", INDICATEURS$vpd$lecture),
         "la fiche du VPD presente 1,5 kPa comme un repere conventionnel")
def_vpd <- GLOSSAIRE$definition[GLOSSAIRE$terme == "VPD"]
verifier(!grepl("au-delà d'environ 1,5 kPa la", def_vpd) && grepl("pas un seuil", def_vpd),
         "le glossaire ne fait plus de 1,5 kPa un seuil de fermeture")
verifier(grepl("es = \\[es\\(Tmax\\) \\+ es\\(Tmin\\)\\] / 2, ea = es", INDICATEURS$vpd$methode),
         "la methode ecrit la formule complete (es, ea, VPD)")


# ==============================================================================
groupe("v1.5 — references et statut des seuils (M6)")
# ==============================================================================

verifier(!anyDuplicated(REFERENCES$id) && all(nzchar(REFERENCES$court)),
         "identifiants de references uniques, formes courtes renseignees")
annee_de <- function(x) sub(".*?((?:19|20)[0-9]{2})$", "\\1", x, perl = TRUE)
verifier(all(mapply(function(court, cit) grepl(sprintf("(%s)", annee_de(court)), cit, fixed = TRUE),
                    REFERENCES$court, REFERENCES$citation)),
         "l'annee de la forme courte est celle de la citation complete")
verifier(all(DESCRIPTIF_SEUILS$statut %in% names(STATUTS_SEUIL)) &&
           length(DESCRIPTIF_SEUILS$statut) == nrow(DESCRIPTIF_SEUILS),
         "chaque seuil a un statut : convention ou repere indicatif")
ids_cites <- unique(unlist(strsplit(DESCRIPTIF_SEUILS$refs, ";", fixed = TRUE)))
ids_cites <- ids_cites[nzchar(ids_cites)]
verifier(all(ids_cites %in% REFERENCES$id),
         "toute reference citee par un seuil figure dans la table")
verifier(inherits(tryCatch(ref_courte("inexistante2099"), error = function(e) e), "error"),
         "une reference inconnue fait echouer, au lieu de s'afficher telle quelle")
verifier(identical(ref_courte("grossiord2020;breda2006"), "Grossiord et al. 2020 ; Bréda et al. 2006"),
         "forme courte de plusieurs references")
verifier(!any(DESCRIPTIF_SEUILS$statut == "indicatif" & !nzchar(DESCRIPTIF_SEUILS$refs)),
         "un repere « indicatif » cite toujours sa source")

# Chaque « Auteur annee » ecrit dans un texte de l'application est dans la table.
textes <- c(vapply(INDICATEURS, function(i) paste(i$methode, i$lecture), character(1)),
            GLOSSAIRE$definition, DESCRIPTIF_SEUILS$aide,
            lire_src("src/mod_sources.R"), lire_src("src/utils_calc.R"),
            CORRECTIONS_METHODE$texte)
motif_cite <- "(*UCP)(?<![[:alpha:]-])[[:upper:]][[:alpha:]-]+(?: et al\\.| et [[:upper:]][[:alpha:]]+)? (?:19|20)[0-9]{2}(?![0-9-])"
cites <- unique(unlist(regmatches(textes, gregexpr(motif_cite, textes, perl = TRUE))))
cites <- sub("^[[:space:]]+", "", cites)
# « Vitasse et al. 2018 » du texte = « Vitasse et al. 2018 » de la table ; on ecarte
# les formes qui ne sont pas des citations (mois, 1re ligne d'un titre...).
cites <- cites[!grepl("^(Version|Vers|Mars|Mai|Juin|Juillet|Août|Avril) ", cites)]
manquantes <- setdiff(cites, REFERENCES$court)
verifier(length(manquantes) == 0L,
         if (length(manquantes)) paste("citations sans reference :", paste(manquantes, collapse = " | "))
         else "chaque « Auteur annee » ecrit dans l'application est dans la table des references")

# Affirmations absolues retirees (M6).
texte_registre <- paste(vapply(INDICATEURS, function(i) paste(i$methode, i$lecture), character(1)),
                        collapse = " ")
verifier(!grepl("s'effondre|se déclenchent fréquemment|ferment leurs stomates et cessent|l'erreur dépasse 30",
                paste(texte_registre, GLOSSAIRE$definition, DESCRIPTIF_SEUILS$aide,
                      lire_src("src/mod_sources.R"), collapse = " ")),
         "plus d'affirmation ecologique absolue sans reference (« s'effondre », « scolytes se declenchent »...)")

h_src <- html_de15(mod_sources_ui("sources"))
verifier(grepl("Corrections de méthode", h_src) && grepl("Version 1.5 — VPD", h_src, fixed = TRUE),
         "Sources liste les corrections de methode, dont celle du VPD")
verifier(all(vapply(REFERENCES$citation, function(c) grepl(substr(c, 1, 30), h_src, fixed = TRUE), logical(1))),
         "Sources liste toutes les references de la table")
verifier(grepl("-0,3 m/s", h_src, fixed = TRUE) && grepl("2,4 mm par jour", h_src, fixed = TRUE),
         "Sources donne les erreurs connues de SAFRAN (Quintana-Segui et al. 2008) et dit qu'aucune n'est affichee")

h_param <- html_de15(mod_parametres_ui("parametres"))
verifier(grepl("cf-statut--indicatif", h_param) && grepl("cf-statut--convention", h_param) &&
           grepl("Réf. : Grossiord", h_param),
         "Parametres affiche le statut et les references de chaque seuil")
verifier(grepl("Aucun seuil n'est un résultat établi", h_param),
         "Parametres dit qu'aucun seuil n'est un resultat etabli")
verifier(sum(gregexpr("cf-statut ", h_param, fixed = TRUE)[[1]] > 0) == nrow(DESCRIPTIF_SEUILS),
         "un statut par seuil, ni plus ni moins")


# ==============================================================================
groupe("v1.5 — gel tardif : fenetre fixe, temperature sous abri (M7)")
# ==============================================================================

verifier(is.null(gel_tardif_conseil(150)) && is.null(gel_tardif_conseil(NA_real_)) &&
           is.null(gel_tardif_conseil(NULL)),
         "pas de conseil en plaine, ni sans altitude")
# 800 m : 8 x 2 = 16 jours et 8 x 3,5 = 28 jours
c800 <- gel_tardif_conseil(800)
verifier(grepl("800 m", c800) && grepl("16 à 28 jours", c800) && grepl("Vitasse et al. 2018", c800),
         "a 800 m : debourrement retarde de 16 a 28 jours, source citee")
verifier(grepl("jour 151", c800) && grepl("jour 170", gel_tardif_conseil(800, 170)),
         "le conseil cite la fin de fenetre en vigueur")
verifier(grepl("le calcul n'est pas modifié", c800),
         "le conseil ne modifie pas le calcul")
verifier(!is.null(gel_tardif_conseil(GEL_ALTITUDE_ALERTE_M)) && is.null(gel_tardif_conseil(GEL_ALTITUDE_ALERTE_M - 1)),
         "le conseil commence a l'altitude d'alerte")
verifier(grepl("sous abri", INDICATEURS$gel$methode) && grepl("ne suit pas le", INDICATEURS$gel$methode),
         "la fiche du gel dit : temperature sous abri, fenetre qui ne suit pas le debourrement")

# La fenetre reste le seul critere du decompte (comportement inchange).
don_g <- serie_synthetique(2001, t_moy = 12, amplitude = 1)
don_g$t_min[don_g$doy == 100] <- -2     # dans la fenetre (91 a 151)
don_g$t_min[don_g$doy == 160] <- -2     # apres la fenetre
g <- calc_gel(don_g, ctx_test(ref = 2001, courante = 2001, affichage = 2001))
verifier(g$n_gel[1] == 2 && g$n_gel_tardif[1] == 1,
         "deux jours de gel, dont un seul dans la fenetre de gel tardif")


# ==============================================================================
groupe("v1.5 — 1961-1990 n'est pas la base pre-industrielle du GIEC (M9)")
# ==============================================================================

cfg <- lire_src("src/config.R")
verifier(!grepl("pre-industrielle recente", cfg, fixed = TRUE) && grepl("1850-1900", cfg, fixed = TRUE),
         "le commentaire de config.R ne dit plus « pre-industrielle recente du GIEC »")
def_ref <- GLOSSAIRE$definition[GLOSSAIRE$terme == "Période de référence"]
verifier(grepl("1850-1900", def_ref) && grepl("Ce n'est pas la base", def_ref),
         "le glossaire distingue 1961-1990 de la base pre-industrielle (1850-1900)")
h_per <- html_de15(mod_periode_ui("periode"))
verifier(!grepl("pré-réchauffement", h_per) && grepl("pas la base pré-industrielle", h_per),
         "l'aide de l'onglet periode ne dit plus « pre-rechauffement »")


# ==============================================================================
groupe("v1.5 — version, feuille de style, simulation rejouable")
# ==============================================================================

verifier(numeric_version(CONFIG$app$version) >= "1.5.0", "la version est au moins 1.5.0")
css15 <- paste(readLines(chemin_ci("www/dsfr-lite.css"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
verifier(grepl("\\.cf-statut--indicatif", css15) && grepl("\\.cf-refs", css15),
         "la feuille de style definit le statut des seuils et les references")
verifier(file.exists(chemin_ci("tests/simulation_vpd.R")),
         "le script de mesure du VPD est fourni (Rscript tests/simulation_vpd.R)")
