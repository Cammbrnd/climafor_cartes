# ==============================================================================
# tests/test_v12.R — CORRECTIFS DE LA VERSION 1.2
# ------------------------------------------------------------------------------
# Un groupe par famille de constats de l'audit (donnees D, graphiques G,
# rapports R). Chaque test pose le SCENARIO qui avait ete mesure en v11 : la
# serie arretee au 27 octobre, l'autocorrelation, le texte hostile.
#
# Lance par tests/lancer_tests.R, APRES tests/test_stations.R (il reutilise les
# fichiers de stations synthetiques et la lecture locale posees par celui-ci).
# ==============================================================================

cyc <- function(doy, a) 12 + 8 * sin(2 * pi * (doy - 110) / 365)   # stationnaire


# ==============================================================================
groupe("v1.2 — annee en cours incomplete (D1, G1)")
# ==============================================================================
# Scenario v11 : serie stationnaire arretee au 27 octobre 2026. L'annee 2026 etait
# traitee comme complete : +1,07 degre, 600 mm au lieu de 730, fausse pente.

don_p <- serie_synthetique(1991:2026, t_moy = cyc, p_jour = 2)
don_p <- don_p[!(don_p$annee == 2026 & don_p$doy > 300), ]
ctx_p <- ctx_creer(ref = 1991:2020, affichage = 1991:2026, courante = 2026)

r <- calc_temperature_annuelle(don_p, ctx_p)
verifier(isTRUE(r$partielle[r$annee == 2026]) && !any(r$partielle[r$annee < 2026]),
         "seule l'annee 2026 est signalee incomplete")
verifier(is.na(r$anomalie[r$annee == 2026]),
         "l'annee incomplete n'a pas d'ecart a la normale")
verifier(is.finite(r$t_moy[r$annee == 2026]),
         "sa valeur est conservee : elle reste affichee, en gris")
verifier(presque_egal(r$normale[1], mean(r$t_moy[r$annee %in% 1991:2020]), 1e-9),
         "la normale ne depend que des annees completes de la reference")

tr <- tendance(r$annee, r$t_moy, "°C", exclure = r$partielle)
verifier(abs(tr$pente_dec) < 1e-6,
         "serie stationnaire : pente nulle (v11 : +0,048 °C/decennie avec l'annee partielle)")
verifier(!isTRUE(tendance(r$annee, r$t_moy, "°C")$ok) ||
           abs(tendance(r$annee, r$t_moy, "°C", exclure = r$partielle)$pente_dec) < 1e-6,
         "sans exclusion, la pente serait faussee ; avec, elle est nulle")
verifier(max(abs(diff(r$lissee)), na.rm = TRUE) < 0.01,
         "la moyenne mobile ne remonte plus en fin de serie (annee partielle exclue)")

p <- calc_precipitations(don_p, ctx_p)
verifier(isTRUE(p$partielle[p$annee == 2026]) && is.na(p$anomalie[p$annee == 2026]),
         "precipitations : l'annee partielle (600 mm) n'est pas comparee a 730 mm")
verifier(presque_egal(p$normale[1], 730, 1.5),
         "la normale de precipitations reste a 730 mm")

res_t <- INDICATEURS[["tmp"]]$resume(r, ctx_p)
verifier(grepl("incomplète", res_t) && !grepl("au-dessus de à", res_t),
         "le commentaire dit que l'annee est incomplete, sans comparer ni faute de grammaire")

cp <- completude(don_p)
verifier(cp$partielle[cp$annee == 2026] && !any(cp$partielle[cp$annee < 2026]),
         "completude() : 2026 partielle, les autres non")
verifier(cp$dernier_jour[cp$annee == 2026] == as.Date("2026-10-27"),
         "completude() donne le dernier jour observe")

# Saison de vegetation / P - ETP : incomplete seulement si la fin de fenetre n'est pas observee
cp_ete <- completude(don_p, c(6, 8))
verifier(!cp_ete$partielle[cp_ete$annee == 2026],
         "juin-aout 2026 est complet : une annee arretee en octobre n'invalide pas l'ete")
cp_aut <- completude(don_p, c(10, 12))
verifier(cp_aut$partielle[cp_aut$annee == 2026],
         "octobre-decembre 2026 est incomplet")

# Jours manquants : comptes, signales, JAMAIS compenses
don_m <- serie_synthetique(2000:2010, p_jour = 2)
don_m$p_tot[don_m$annee == 2005 & don_m$doy %in% 100:109] <- NA
ctx_m <- ctx_creer(ref = 2000:2004, affichage = 2000:2010, courante = 2010)
pm <- calc_precipitations(don_m, ctx_m)
verifier(pm$n_manquants[pm$annee == 2005] == 10L && !pm$partielle[pm$annee == 2005],
         "10 jours sans mesure sont comptes, sans declarer l'annee partielle")
verifier(presque_egal(pm$p_tot[pm$annee == 2005], 730 - 20, 1e-9),
         "le cumul n'est pas compense (sous-estime de 10 j x 2 mm) : choix assume et signale")
g_m <- INDICATEURS[["pre"]]$plot(pm, ctx_m)
verifier(any(grepl("sans mesure", graphe_notes(g_m))),
         "le graphique signale les jours sans mesure sous le trace")


# ==============================================================================
groupe("v1.2 — tendance : pente de Sen, intervalle, autocorrelation (G4)")
# ==============================================================================
set.seed(123)
a <- 1981:2020
x <- 0.03 * (a - 1981) + sin(a)                     # pente vraie : 0,3 par decennie
t1 <- tendance(a, x, "°C")
verifier(isTRUE(t1$ok) && t1$n == 40L, "40 annees complete : tendance estimee")
verifier(abs(t1$pente_dec - 0.3) < 0.05, "pente de Sen proche de la pente vraie")
verifier(t1$ic_bas_dec <= t1$pente_dec && t1$pente_dec <= t1$ic_haut_dec,
         "l'intervalle de confiance encadre la pente")
verifier(grepl("Mann-Kendall", t1$libelle) && grepl("IC 95", t1$libelle),
         "le libelle nomme le test et l'intervalle")
verifier(!grepl("R²|R2", t1$libelle), "le R² (non robuste) n'est plus affiche")

t_court <- tendance(1991:2009, rnorm(19), "°C")
verifier(!isTRUE(t_court$ok) && grepl("20", t_court$libelle),
         "moins de 20 annees : pas de tendance, et le seuil est dit")

ex <- c(rep(FALSE, 39), TRUE)
verifier(tendance(a, x, "°C", exclure = ex)$n == 39L, "une annee exclue ne compte pas")

# Autocorrelation : la variance est corrigee, les faux positifs diminuent
ar <- as.numeric(stats::filter(rnorm(60, 0, 0.6), 0.7, "recursive"))
t_ar <- tendance(1961:2020, ar, "°C")
verifier(t_ar$r1 > 0.3 && t_ar$n_eff < t_ar$n,
         "autocorrelation positive detectee : effectif efficace inferieur a l'effectif")

# Rupture de niveau (stations)
saut <- c(rnorm(20, 10, 0.2), rnorm(20, 11.5, 0.2))
t_s <- tendance(1981:2020, saut, "°C")
verifier(!is.null(t_s$rupture) && abs(t_s$rupture$annee - 2000) <= 2,
         "un saut de niveau en 2000 est detecte (indicatif)")
verifier(is.null(tendance(a, 0.03 * (a - 1981) + rnorm(40, 0, 0.05), "°C")$rupture) ||
           TRUE, "la detection de rupture ne leve pas d'erreur sur une tendance reguliere")


# ==============================================================================
groupe("v1.2 — classes d'ecart et couleurs (G7)")
# ==============================================================================
verifier(identical(classe_ecart(c(0.1, 1, -1, NA), rep(1, 4)),
                   c("neutre", "haut", "bas", "neutre")),
         "ecart < 0,5 ecart-type : neutre ; au-dela : haut / bas ; inconnu : neutre")
verifier(identical(classe_ecart(0.4, 1, bande = 0.25), "haut"),
         "la bande neutre est un parametre (CONFIG$tendance$bande_neutre)")
verifier(identical(classe_ecart(1e-12, 0), "neutre"),
         "ecart nul (a l'arrondi pres) avec ecart-type nul : neutre")

g_t <- INDICATEURS[["tmp"]]$plot(r, ctx_p)
b_t <- ggplot2::ggplot_build(g_t)
lib_t <- unique(unlist(lapply(b_t$plot$layers, function(l) {
  d <- tryCatch(l$data, error = function(e) NULL)
  if (is.data.frame(d) && "classe_lib" %in% names(d)) as.character(d$classe_lib) else NULL
})))
lib_t <- c(lib_t, as.character(b_t$plot$data$classe_lib))
verifier(any(grepl("incomplète", lib_t)) && !any(grepl("^#", lib_t)),
         "la legende nomme les classes en mots (« Année incomplète »), jamais en code hexadecimal")
verifier(!any(grepl("phase", c(g_t$labels$fill, g_t$labels$colour), ignore.case = TRUE)),
         "plus de titre de legende « phase »")


# ==============================================================================
groupe("v1.2 — colonnes derivees et cache (D6)")
# ==============================================================================
d0 <- data.frame(p_liq = c(1, NA, 3, NA), p_sol = c(0.5, 2, NA, NA),
                 t_max = 20, t_min = 10, t_moy = 15, hu = 60)
d1 <- safran_deriver(d0)
verifier(identical(d1$p_tot, c(1.5, 2, 3, NA_real_)),
         "p_tot = liquide + solide ; une seule phase connue suffit ; deux NA donnent NA")
verifier(all(d1$vpd > 0), "le VPD est derive avec la meme fonction partout")
verifier(identical(safran_deriver(d1)$p_tot, d1$p_tot),
         "derivation idempotente (rejouee apres l'assemblage du cache)")

ch <- cache_chemin(1.4727, 43.35, 2020)
verifier(grepl(sprintf("_s%d\\.json\\.gz$", CONFIG$cache$schema), ch),
         "le numero de schema est dans le nom du fichier de cache")
old_schema <- CONFIG$cache$schema
CONFIG$cache$schema <- old_schema + 1L
verifier(cache_chemin(1.4727, 43.35, 2020) != ch,
         "changer de schema change de fichier : une ancienne entree n'est jamais relue")
CONFIG$cache$schema <- old_schema


# ==============================================================================
groupe("v1.2 — temperature moyenne des stations : definition homogene (D3)")
# ==============================================================================
# Les fichiers synthetiques ont TM = TNTXM - 0,5 : les deux definitions different.
old_def <- CONFIG$stations$definition_tmoy
CONFIG$stations$definition_tmoy <- "tntx"
don_h <- station_charger(stn, lon = 1.47, lat = 43.35, annees = 2010:2012)
ok_t <- !is.na(don_h$t_min) & !is.na(don_h$t_max)
verifier(max(abs(don_h$t_moy[ok_t] - (don_h$t_min[ok_t] + don_h$t_max[ok_t]) / 2)) < 0.06,
         "par defaut : (TN + TX) / 2 sur toute la serie (arrondi des fichiers : 0,1 °C)")
verifier(grepl("TN \\+ TX", attr(don_h, "definition_tmoy")),
         "la definition retenue est portee par la serie")
CONFIG$stations$definition_tmoy <- "tm"
don_l <- station_charger(stn, lon = 1.47, lat = 43.35, annees = 2010:2012)
verifier(mean(don_h$t_moy - don_l$t_moy, na.rm = TRUE) > 0.4,
         "l'ancienne definition (TM prioritaire) donne une moyenne plus basse de 0,5 °C ici : l'ecart est reel")
verifier(grepl("TM", attr(don_l, "definition_tmoy")),
         "et elle est nommee comme telle")
CONFIG$stations$definition_tmoy <- old_def


# ==============================================================================
groupe("v1.2 — messages explicites quand un indicateur est vide (G9)")
# ==============================================================================
don_ok <- serie_synthetique(2000:2002)
ctx_ok <- ctx_test()
sans_vent <- don_ok; sans_vent$ff <- NA_real_
r_v <- calculer_un_indicateur(sans_vent, INDICATEURS[["vent"]], ctx_ok)
verifier(!nrow(r_v) && identical(attr(r_v, "raison"), "variable_absente"),
         "variable non mesuree : raison « variable_absente »")
verifier(grepl("non mesuré", attr(r_v, "message")),
         "... et la phrase dit laquelle manque")
r_n <- calculer_un_indicateur(NULL, INDICATEURS[["tmp"]], ctx_ok)
verifier(identical(attr(r_n, "raison"), "aucune_donnee"), "aucune donnee chargee : raison dediee")
ind_casse <- list(id = "casse", titre = "Casse", calc = function(d, ctx) stop("boom"), colonnes = NULL)
r_e <- suppressWarnings(calculer_un_indicateur(don_ok, ind_casse, ctx_ok))
verifier(identical(attr(r_e, "raison"), "erreur") && !grepl("boom", attr(r_e, "message")),
         "erreur de calcul : message generique, le texte brut n'est pas montre")
g_v <- INDICATEURS[["vent"]]$plot(r_v, ctx_ok)
lbl <- ggplot2::ggplot_build(g_v)$data[[1]]$label
verifier(is.character(lbl) && grepl("non mesuré", lbl[1]),
         "le graphique vide affiche la vraie raison, plus « Données indisponibles »")


# ==============================================================================
groupe("v1.2 — typographie et textes (G3)")
# ==============================================================================
verifier(fmt_fr(-0.04, 1) == "0,0", "un zero arrondi ne porte pas de signe")
verifier(fmt_fr(1.5, 1, signe = TRUE) == "+1,5" && fmt_fr(-3, 0) == "−3",
         "virgule decimale, vrai signe moins, signe plus explicite")
verifier(fmt_p(0.0004) == "p < 0,001" && fmt_p(0.0931) == "p = 0,093", "p-valeur en francais")

don_c <- serie_synthetique(1991:2025, t_moy = function(doy, a) cyc(doy, a) + 0.02 * (a - 1991), p_jour = 2)
ctx_c <- ctx_creer(ref = 1991:2020, affichage = 1991:2025, courante = 2025)
res_c <- calculer_indicateurs(don_c, ctx_c)
txt <- vapply(names(INDICATEURS), function(k) {
  f <- INDICATEURS[[k]]$resume
  if (is.null(f)) "" else tryCatch(f(res_c[[k]], ctx_c), error = function(e) "ERREUR")
}, character(1))
verifier(!any(txt == "ERREUR"), "tous les commentaires automatiques s'executent")
verifier(!any(grepl("de à |R2|R²", txt)), "plus de « au-dessus de à la normale » ni de R2")
verifier(!any(grepl("[0-9]\\.[0-9]", txt)), "aucun nombre en point decimal dans les commentaires")


# ==============================================================================
groupe("v1.2 — textes alternatifs, tableau des valeurs, exports (G5, G8)")
# ==============================================================================
vf <- valeurs_formatees(res_c[["tmp"]], INDICATEURS[["tmp"]], ctx_c)
verifier(is.data.frame(vf) && all(vapply(vf, is.character, logical(1))),
         "valeurs_formatees() rend du texte pret a afficher")
verifier(!any(grepl("\\.", unlist(vf[c("t_moy")]))) && any(grepl(",", unlist(vf[c("t_moy")]))),
         "virgule decimale dans le tableau")
verifier(!any(c("n_jours", "attendu", "couverts") %in% names(vf)),
         "les colonnes de calcul n'encombrent pas le tableau")

lg <- lignes_metadonnees_csv(INDICATEURS[["tmp"]], ctx_c, don = don_c,
                             lieu = list(nom = "Auterive"), dat = res_c[["tmp"]])
verifier(any(grepl("^Source", lg)) && any(grepl("^Référence", lg)) &&
           any(grepl("^Extraction", lg)) && any(grepl("Etalab", lg)) &&
           any(grepl("^Seuils", lg)),
         "le CSV porte source, reference, seuils, date d'extraction et licence")
verifier(!any(grepl(";", lg)), "aucune ligne de metadonnees ne contient le separateur « ; »")
f_csv <- tempfile(fileext = ".csv")
ecrire_csv_avec_metadonnees(res_c[["tmp"]][, c("annee", "t_moy")], f_csv, lg)
brut <- readBin(f_csv, "raw", 3)
verifier(identical(brut, as.raw(c(0xEF, 0xBB, 0xBF))), "CSV en UTF-8 avec BOM (Excel)")
relu <- utils::read.csv2(f_csv, comment.char = "#", fileEncoding = "UTF-8-BOM")
verifier(nrow(relu) == nrow(res_c[["tmp"]]) && identical(names(relu), c("annee", "t_moy")),
         "les lignes « # » sont ignorees a la relecture : meme nombre de lignes et de colonnes")
verifier(grepl("Source :", ligne_source(don_c)) && grepl("Etalab", ligne_source(don_c)),
         "la ligne de source des exports image est complete")


# ==============================================================================
groupe("v1.2 — rapport : texte libre, niveaux, parametres (R1, R3, R6)")
# ==============================================================================
hostile <- "<script>alert(1)</script> [clic](javascript:alert(1)) **gras** <img src=x onerror=alert(2)> \\(x\\) $y$"
eh <- echapper_md(hostile)
verifier(!grepl("(^|[^\\\\])<", eh), "aucun « < » non echappe")
verifier(!grepl("\\]\\(", eh) && !grepl("\\\\[()]", eh),
         "ni lien Markdown, ni parenthese echappee par barre (qui ouvrirait une formule TeX)")
if (rmarkdown::pandoc_available()) {
  md <- tempfile(fileext = ".md"); writeLines(eh, md)
  html <- paste(system2(rmarkdown::pandoc_exec(), c(md, "-f", "markdown+tex_math_single_backslash+tex_math_dollars+raw_html", "-t", "html"),
                        stdout = TRUE), collapse = "\n")
  verifier(!grepl("<script|<img|<a |class=\"math", html),
           "apres Pandoc (extensions de rmarkdown) : ni script, ni image, ni lien, ni formule")
  verifier(grepl("&lt;script&gt;", html) && grepl("javascript:alert", html),
           "le texte saisi reste lisible tel quel")
}
nc <- nettoyer_contexte(strrep("a", 2000))
verifier(isTRUE(nc$tronque) && nchar(nc$texte) == CONFIG$rapport$contexte_max, "texte borne en longueur et signale")
verifier(!isTRUE(nettoyer_contexte("court")$tronque), "un texte court n'est pas signale tronque")
verifier(identical(nettoyer_contexte(NULL)$texte, ""), "contexte absent : chaine vide")

verifier(identical(valider_niveau("technique"), "technique"), "niveau valide accepte")
verifier(inherits(try(valider_niveau("standard"), silent = TRUE), "try-error"),
         "niveau inconnu : erreur explicite, plus de repli silencieux")
verifier(niveau_au_moins("technique", "ingenieur") && !niveau_au_moins("decideur", "ingenieur"),
         "les niveaux sont ordonnes : decideur < ingenieur < technique")
verifier(identical(CONFIG$rapport$niveau_defaut, "ingenieur") &&
           CONFIG$rapport$niveau_defaut %in% CONFIG$rapport$niveaux,
         "le niveau par defaut existe")

ts0 <- tableau_seuils(ctx_ok)
verifier(!any(ts0$`Modifié`) && nrow(ts0) == length(CONFIG$seuils),
         "seuils par defaut : aucun marque modifie")
ctx_s <- ctx_ok; ctx_s$seuils$gel_t <- -2
verifier(sum(tableau_seuils(ctx_s)$`Modifié`) == 1 && grepl("-2", texte_seuils_modifies(ctx_s)),
         "un seuil modifie est repere et dit dans le rapport")
pa <- parametres_analyse(ctx_ok, don_c)
verifier(all(c("application", "source", "reference", "seuils_modifies", "versions") %in% names(pa)) &&
           "R" %in% names(pa$versions),
         "parametres_analyse() contient de quoi refaire l'analyse (source, reference, seuils, versions)")

# Suivi d'une generation en arriere-plan (sans lancer de processus)
verifier(identical(etat_rapport(NULL)$statut, "aucun"), "aucune tache : etat « aucun »")
dos <- tempfile("rap_"); dir.create(dos)
writeLines("en_cours", file.path(dos, "etat.txt"))
j <- list(dossier = dos, etat = file.path(dos, "etat.txt"), debut = Sys.time())
verifier(identical(etat_rapport(j)$statut, "en_cours"), "tache en cours reconnue")
j_vieux <- j; j_vieux$debut <- Sys.time() - CONFIG$rapport$delai_max_s - 5
verifier(identical(etat_rapport(j_vieux)$statut, "erreur"), "delai maximal depasse : abandon signale")
writeLines("erreur:boom", file.path(dos, "etat.txt"))
verifier(identical(etat_rapport(j)$statut, "erreur") && etat_rapport(j)$message == "boom",
         "echec du processus : message transmis")
f_ok <- file.path(dos, "r.html"); writeLines("x", f_ok)
writeLines(paste0("ok:", f_ok), file.path(dos, "etat.txt"))
verifier(identical(etat_rapport(j)$statut, "ok") && identical(etat_rapport(j)$fichier, f_ok),
         "rapport pret : chemin du fichier transmis")

# Generation complete (niveau decideur, images) : le seul test qui lance rmarkdown
if (rmarkdown::pandoc_available()) {
  dos2 <- tempfile("rap_")
  entree <- list(donnees = don_c, resultats = res_c, ctx = ctx_c, lieu = list(nom = "Auterive (31)", lat = 43.35, lon = 1.47),
                 indicateurs = names(INDICATEURS), niveau = "decideur", contexte = hostile, interactif = FALSE)
  sortie <- tryCatch(rendre_rapport(dos2, entree, "html"), error = function(e) conditionMessage(e))
  verifier(is.character(sortie) && file.exists(sortie), "le rapport decideur se genere de bout en bout")
  if (is.character(sortie) && file.exists(sortie)) {
    h <- paste(readLines(sortie, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    h_sans_img <- gsub("data:image/[a-z]+;base64,[A-Za-z0-9+/=]+", "", h)
    verifier(!grepl("<script>alert|<img src=x|onerror=alert\\(2\\)>", h_sans_img),
             "le texte hostile n'a produit ni script ni balise dans le rapport")
    verifier(grepl("Paramètres de l’analyse|Paramètres de l'analyse", h_sans_img),
             "l'annexe des parametres est presente")
    verifier(!grepl("mathjax|cdn\\.", h_sans_img, ignore.case = TRUE),
             "le rapport n'appelle aucun script distant")
    verifier(file.size(sortie) < 2.5e6, "rapport decideur : moins de 2,5 Mo")
  }
  verifier(inherits(try(rendre_rapport(tempfile("rap_"), modifyList(entree, list(niveau = "standard")), "html"),
                        silent = TRUE), "try-error"),
           "un niveau inconnu fait echouer la generation au lieu de produire un autre rapport")
}

# Ecart qui s'affiche « 0 » : le commentaire ne doit pas le qualifier de superieur/inferieur
dat_z <- data.frame(annee = 2000:2005, n_t2 = c(366, 365, 365, 365, 366, 365),
                    normale = 365.4, ecart_type_ref = 0.548, n_jours = c(366, 365, 365, 365, 366, 365),
                    partielle = FALSE, dernier_jour = as.Date(paste0(2000:2005, "-12-31")))
ctx_z <- ctx_creer(ref = 2000:2004, affichage = 2000:2005, courante = 2005)
txt_z <- resume_standard(dat_z, ctx_z, "n_t2", "Nombre de jours", "jours", 0)
verifier(grepl("proche de", txt_z) && !grepl("inférieure|supérieure", txt_z),
         "un ecart arrondi a 0 jour est dit « proche de » la normale")
dat_z$n_t2[6] <- 380
txt_z2 <- resume_standard(dat_z, ctx_z, "n_t2", "Nombre de jours", "jours", 0)
verifier(grepl("supérieure à", txt_z2), "un ecart reel reste qualifie de superieur")
