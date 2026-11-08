# ==============================================================================
# tests/test_v22_stations.R — BASE PRECALCULEE DES STATIONS ET MODE RAPIDE (VERSION 2.2)
# ------------------------------------------------------------------------------
# Une station a des trous que les mailles n'ont pas : des annees ecartees (moins de 95 % des
# jours), des jours manquants dans des annees valides, des variables qu'elle ne mesure pas. Le
# mode rapide ne vaut que s'il rend EXACTEMENT ce que rend le chargement complet malgre cela.
# Ce que ces tests gardent :
#   1. la definition de la base d'une station (cles, fichiers, references par indicateur) ;
#   2. la publication : ecriture, relecture, validation de stations.json, entrees vides ;
#   3. la PARITE, indicateur par indicateur, sur des stations avec trous : tableau, jours
#      manquants, annees ecartees, qualite de la serie ;
#   4. les garde-fous : station absente du jeu, jeu sans base, reglages differents.
# Aucun test n'appelle Meteo-France : les series viennent de outils/stations_demo.R (FICTIVES).
# ==============================================================================

for (f in c("outils/carte_geo.R", "outils/carte_format.R", "outils/carte_precalcul.R",
            "outils/carte_demo.R", "outils/stations_precalcul.R", "outils/stations_demo.R")) source(chemin_ci(f))


# ==============================================================================
groupe("v2.2 stations — la base d'une station : definition")
# ==============================================================================

cles_s <- names(STATIONS_BASE_COUCHES)
verifier(length(cles_s) >= 40 && !anyDuplicated(cles_s), "la base d'une station a des cles distinctes")
verifier(all(vapply(names(STATIONS_BASE_COUCHES), function(n) identical(STATIONS_BASE_COUCHES[[n]]$cle, n), TRUE)),
         "chaque entree est rangee sous sa propre cle")
verifier(all(grepl("^[a-z0-9_]{1,40}$", cles_s)) &&
           all(vapply(STATIONS_BASE_COUCHES, function(k) carte_nom_fichier_sur(sprintf("s_%s.bin.gz", k$cle)), TRUE)),
         "les cles et les noms de fichiers sont publiables")
verifier(!length(intersect(cles_s, names(CARTE_COUCHES))),
         "aucune entree ne porte la cle d'une grandeur de la carte")
verifier(all(vapply(STATIONS_BASE_COUCHES, function(k) isTRUE(k$base) && k$type %in% c("annuel", "ref"), TRUE)),
         "chaque entree se declare base, annuelle ou reference")

annuelles_bm <- Filter(function(k) identical(k$type, "annuel"), BASE_COUCHES)
verifier(all(names(annuelles_bm) %in% cles_s),
         "toutes les colonnes annuelles de la base des mailles sont aussi celles des stations")
verifier(!any(names(Filter(function(k) identical(k$type, "ref"), BASE_COUCHES)) %in% cles_s),
         "les references des mailles (par variable) ne sont pas celles des stations (par indicateur)")

refs_s <- Filter(function(k) identical(k$type, "ref"), STATIONS_BASE_COUCHES)
verifier(all(vapply(refs_s, function(k) k$ind %in% names(INDICATEURS) && is.function(k$calc) &&
                      k$lignes %in% c(1L, 12L, 366L), TRUE)),
         "chaque reference d'une station nomme un indicateur du registre et un nombre de lignes connu")
verifier(all(vapply(refs_s, function(k) !is.null(.BASE_STATIONS_NOM_CTX[[k$nom]]), TRUE)),
         "chaque reference sait quel nom de `ctx$base` elle alimente")

# Tout indicateur qui consulte une reference a, pour une station, la sienne - ou ne s'applique pas.
sans_station <- c("neige_mois", "vpd_mois", "vent_sol")   # SWI, neige, humidite : jamais mesures
consultent <- c(BASE_INDICATEURS_DIRECTS, BASE_REFS_STATION_INDICATEURS)
manque_ref <- Filter(function(id) !length(base_refs_station(id)) && !id %in% sans_station, consultent)
verifier(!length(manque_ref),
         if (length(manque_ref)) paste("reference de station manquante pour :", paste(manque_ref, collapse = ", "))
         else "les indicateurs a reference ont chacun la leur (ou ne concernent pas une station)")

# Les jours manquants : une colonne par couple (variables, fenetre) de BASE_INDICATEURS.
cle_mq <- vapply(BASE_INDICATEURS, function(d) base_cle_manquants(d$vars, .base_fenetre_resolue(d$fenetre)), "")
verifier(all(cle_mq %in% cles_s), "chaque indicateur annuel a sa colonne de jours manquants")
verifier(identical(base_cle_manquants(c("t_moy", "p_tot"), c(6L, 8L)), base_cle_manquants(c("p_tot", "t_moy"), c(6L, 8L))),
         "la cle des jours manquants ne depend pas de l'ordre des variables")


# ==============================================================================
groupe("v2.2 stations — jeu synthetique avec trous : precalcul et publication")
# ==============================================================================

an_s <- 1985:2025
fin_s <- as.Date("2026-10-15")              # l'annee en cours est partielle, comme en octobre
maintenant_s <- as.Date("2026-10-20")

# Trois stations, dont deux avec des trous :
#   - ANCIENNE : 2003 entierement absente (annee ecartee), 20 jours manquants en juillet 2015
#     (5,5 % : ecartee), 10 jours en juin 2012 (2,7 % : valide, 10 jours manquants), 5 jours en 1997 ;
#   - RECENTE  : complete depuis 1985, un trou d'un mois en 2018 (ecartee) ;
#   - PLUVIOMETRE : pas de temperature.
trous_a <- list(c("2003-01-01", "2003-12-31"), c("2015-07-01", "2015-07-20"),
                c("2012-06-10", "2012-06-19"), c("1997-02-01", "1997-02-05"))
brut_s <- rbind(
  station_demo("31000001", "ANCIENNE 31", 1.20, 43.70, 180, "1985-01-01", fin_s, trous = trous_a, graine = 5L),
  station_demo("31000002", "RECENTE 31", 1.30, 43.80, 320, "1985-01-01", fin_s,
               trous = list(c("2018-03-01", "2018-03-31")), graine = 6L),
  station_demo("31000003", "PLUVIOMETRE 31", 1.10, 43.75, 240, "1990-01-01", fin_s, sans_t = TRUE, graine = 7L))
src_s <- function(dept) if (identical(dept, "31")) brut_s else NULL

t0_s <- Sys.time()
part_s <- stations_construire(src_s, "31", an_s, base = STATIONS_BASE_COUCHES, maintenant = maintenant_s,
                              silencieux = TRUE)
cat(sprintf("    (precalcul de 3 stations avec base : %.1f s)\n", as.numeric(difftime(Sys.time(), t0_s, units = "secs"))))
verifier(nrow(part_s$stations) == 3L, "precalcul : les trois stations sont retenues")
verifier(all(names(STATIONS_BASE_COUCHES) %in% names(part_s$valeurs)),
         "precalcul : la part porte toutes les entrees de la base")

# Mailles d'essai (le jeu de stations est publie a cote d'un jeu de mailles, base comprise).
mm_s <- carte_demo_mailles(300)
n_m_s <- nrow(mm_s)
part_m_s <- carte_construire(carte_demo_source(an_s), mm_s$lon, mm_s$lat, an_s, seq_len(n_m_s),
                             base = BASE_COUCHES)
dos_s <- tempfile("stations22")
carte_assembler(list(part_m_s), mm_s$lon, mm_s$lat, an_s, "jeu synthetique", dos_s, base = BASE_COUCHES)
r_s <- stations_assembler(list(part_s), an_s, STATIONS_DEMO_SOURCE, dos_s, n_min = 1L, base = STATIONS_BASE_COUCHES)

meta_s <- stations_lire_meta_locale(dos_s)
verifier(!is.null(meta_s$meta) && length(meta_s$meta[["base"]]) > 20L,
         "publication : stations.json decrit la base, qui passe la validation")
vides_s <- as.character(unlist(meta_s$meta[["base_parametres"]][["vides"]]))
verifier(all(c("b_swi_swi_min", "b_neige_neige_3j_max", "b_vpd_n_stress") %in% vides_s),
         "publication : une entree qu'aucune station ne renseigne (SWI, neige, humidite) est declaree vide")
verifier(!any(file.exists(file.path(dos_s, sprintf("s_%s.bin.gz", vides_s)))),
         "publication : une entree vide n'est pas ecrite")
verifier(identical(meta_s$meta[["base_parametres"]][["empreinte"]], base_empreinte(carte_ctx(an_s))),
         "publication : l'empreinte des reglages est celle de la base des mailles")
fich_s <- vapply(meta_s$meta[["base"]], function(k) k$fichier, "")
verifier(all(file.exists(file.path(dos_s, fich_s))), "publication : chaque fichier de la base est ecrit")

# Sans base : un jeu de stations de la v2.1 reste valide, il n'a simplement pas de mode rapide.
dos_sb <- tempfile("stations22sb")
carte_assembler(list(part_m_s), mm_s$lon, mm_s$lat, an_s, "jeu synthetique", dos_sb, base = BASE_COUCHES)
part_sb <- stations_construire(src_s, "31", an_s, maintenant = maintenant_s, silencieux = TRUE)
stations_assembler(list(part_sb), an_s, STATIONS_DEMO_SOURCE, dos_sb, n_min = 1L)
verifier(!is.null(stations_lire_meta_locale(dos_sb)$meta) &&
           is.null(stations_lire_meta_locale(dos_sb)$meta[["base"]]),
         "un jeu de stations sans base (v2.1) reste valide")

rm(list = ls(.ETAT_COMPARER), envir = .ETAT_COMPARER)
rm(list = ls(.ETAT_BASE), envir = .ETAT_BASE)
tr_s <- carte_transport(url = "", dossier = dos_s)
jeu_s <- cmp_jeu(tr_s)
sj_s <- cmp_stations(jeu_s)
verifier(isTRUE(sj_s$ok) && nrow(sj_s$table) == 3L && length(sj_s$meta[["base"]]) > 20L,
         "lecture : le jeu de stations et sa base se retrouvent")

ouv_s <- base_ouvrir_station(sj_s$table$id[1], tr_s)
verifier(isTRUE(ouv_s$ok) && ouv_s$i == 1L && base_est_station(ouv_s$jeu) && ouv_s$jeu$n == 3L &&
           grepl("station", ouv_s$note),
         "ouverture : la station est retrouvee par son identifiant")
verifier(identical(base_ouvrir_station("00000000", tr_s)$raison, "station_non_publiee"),
         "ouverture : une station absente du jeu est refusee, avec sa raison")
tr_sb <- carte_transport(url = "", dossier = dos_sb)
rm(list = ls(.ETAT_COMPARER), envir = .ETAT_COMPARER)
verifier(identical(base_ouvrir_station(sj_s$table$id[1], tr_sb)$raison, "sans_base"),
         "ouverture : un jeu de stations sans base est refuse, avec sa raison")
rm(list = ls(.ETAT_COMPARER), envir = .ETAT_COMPARER)
jeu_s <- cmp_jeu(tr_s)

# Relecture d'une entree : la valeur du fichier est celle du precalcul (a la precision pres).
i_anc <- 1L
v_lu <- base_lire(ouv_s$jeu, "b_q_ok_t", i_anc)
v_att <- part_s$valeurs$b_q_ok_t[, which(part_s$stations$id == sj_s$table$id[1])]
verifier(length(v_lu) == length(an_s) && isTRUE(all.equal(v_lu, v_att)),
         "relecture : la validite des annees d'une station se relit a l'identique")
verifier(all(is.na(base_lire(ouv_s$jeu, "b_swi_swi_min", i_anc))) && length(base_lire(ouv_s$jeu, "b_swi_swi_min", i_anc)) == length(an_s),
         "relecture : une entree declaree vide rend des NA, une par annee")
verifier(length(base_lire(ouv_s$jeu, "rs_vag_tmax", i_anc)) == 366L && length(base_lire(ouv_s$jeu, "rs_bh_p", i_anc)) == 12L,
         "relecture : les references de station ont 366, 12 ou 1 valeur")
verifier(inherits(tryCatch(base_lire(ouv_s$jeu, "rs_inconnue", i_anc), error = function(e) e), "error"),
         "relecture : une entree absente et non declaree vide est une erreur")
# Une maille et une station de meme rang ne partagent pas la memoire.
m_tmp <- base_lire(jeu_s, "tmp", 1L)
s_tmp <- base_lire(ouv_s$jeu, "tmp", 1L)
verifier(!isTRUE(all.equal(m_tmp, s_tmp)), "relecture : la maille n et la station n ne se confondent pas en memoire")


# ==============================================================================
groupe("v2.2 stations — parite : base + jours charges = serie entiere, avec trous")
# ==============================================================================

ctx_st <- function(courante = 2026L, ...) ctx_creer(ref = 1991:2020, courante = courante, affichage = 1991:2026, ...)

# Difference entre deux resultats d'indicateur : liste des problemes (vide = identiques).
ecarts_st <- function(a, b, tol) {
  if (!is.data.frame(a) || !is.data.frame(b)) return("pas un tableau")
  if (!nrow(a) && !nrow(b)) {
    return(if (identical(attr(a, "raison"), attr(b, "raison"))) character(0)
           else sprintf("raison %s / %s", attr(a, "raison") %||% "-", attr(b, "raison") %||% "-"))
  }
  if (!setequal(names(a), names(b))) {
    return(sprintf("colonnes : %s / %s", paste(setdiff(names(a), names(b)), collapse = ","),
                   paste(setdiff(names(b), names(a)), collapse = ",")))
  }
  if (nrow(a) != nrow(b)) return(sprintf("%d lignes contre %d", nrow(a), nrow(b)))
  pb <- character(0)
  for (nm in names(a)) {
    x <- a[[nm]]; y <- b[[nm]]
    if (is.numeric(x)) {
      dd <- abs(as.numeric(x) - as.numeric(y)); dd[is.na(x) & is.na(y)] <- 0
      limite <- if (grepl("^(n_|doy_|duree)", nm)) 1e-9 else tol
      if (anyNA(dd) || max(dd) > limite) pb <- c(pb, sprintf("%s (%.4g > %.4g)", nm, suppressWarnings(max(dd, na.rm = TRUE)), limite))
    } else if (!identical(as.character(x), as.character(y))) pb <- c(pb, nm)
  }
  ex_a <- attr(a, "meta")$annees_exclues; ex_b <- attr(b, "meta")$annees_exclues
  if (!identical(as.integer(ex_a), as.integer(ex_b))) {
    pb <- c(pb, sprintf("annees_exclues %s / %s", paste(ex_a, collapse = ","), paste(ex_b, collapse = ",")))
  }
  pb
}

tol_st <- function(id) {
  d <- BASE_INDICATEURS[[id]]
  if (is.null(d)) return(c(bh = 0.15, ombro = 0.15, pre_mois = 0.15, tmp_mois = 0.015,
                           neige_mois = 0.15, vpd_mois = 0.0015, vent_mois = 0.015, tmp_jour = 0.015)[[id]])
  st <- vapply(d$col, function(c0) {
    k <- STATIONS_BASE_COUCHES[[base_cle_colonne(id, c0)]] %||% CARTE_COUCHES[[base_cle_colonne(id, c0)]]
    k$stock }, 0)
  1.01 * 10^(-min(st))
}

# Ce que fait station_charger() : la serie complete d'une part, et pour le mode rapide les seules
# annees demandees (le fichier « latest » de Meteo-France commence au 1er janvier de l'an dernier).
serie_station <- function(brut_station, annees, depuis = NULL) {
  b <- brut_station
  if (!is.null(depuis)) b <- b[b$date >= as.Date(depuis), , drop = FALSE]
  s <- list(id = b$id[1], nom = b$nom[1], dept = "31", lat = b$lat[1], lon = b$lon[1], alt = b$alt[1])
  station_depuis_brut(b, s, s$lon, s$lat, annees)
}

pb_par_station <- function(j, courante, annees_vives, depuis, verbose = FALSE) {
  b <- brut_s[brut_s$id == sj_s$table$id[j], ]
  ouv <- base_ouvrir_station(sj_s$table$id[j], tr_s)
  complet <- serie_station(b, 1985:2026)
  vives <- serie_station(b, annees_vives, depuis = depuis)
  vives <- base_station_attributs(vives, ouv)
  attr(vives, "base") <- ouv[c("jeu", "i", "note")]
  ctx0 <- ctx_st(courante)
  pb <- character(0)
  for (id in names(INDICATEURS)) {
    ind <- INDICATEURS[[id]]
    a <- calculer_un_indicateur(complet, ind, ctx0)
    r <- calculer_un_indicateur(vives, ind, ctx0)
    p <- ecarts_st(r, a, tol_st(id))
    if (length(p)) pb <- c(pb, sprintf("%s : %s", id, paste(p, collapse = " ; ")))
  }
  q_a <- mf_qualite_serie(complet, ctx0); q_r <- mf_qualite_serie(vives, ctx0)
  if (!identical(q_a$alertes, q_r$alertes) || !identical(q_a$niveau, q_r$niveau)) {
    pb <- c(pb, sprintf("qualite de la serie : %s / %s", paste(q_a$alertes, collapse = " | "),
                        paste(q_r$alertes, collapse = " | ")))
  }
  pb
}

noms_s <- sj_s$table$nom
for (j in seq_len(nrow(sj_s$table))) {
  pb <- pb_par_station(j, 2026L, 2026L, "2025-01-01")
  verifier(!length(pb),
           if (length(pb)) paste0(noms_s[j], " — ecarts : ", paste(pb, collapse = " || "))
           else sprintf("%s : les 25 indicateurs et la qualite de la serie, rapides = complets (annee en cours 2026 partielle)", noms_s[j]))
}
pb22 <- pb_par_station(1L, 2022L, c(2022L, 2026L), "2022-01-01")
verifier(!length(pb22),
         if (length(pb22)) paste("annee comparee 2022 :", paste(pb22, collapse = " || "))
         else "annee comparee anterieure (2022) : profils du mois et du jour = serie entiere")

# Les trous de la station ancienne sont bien ceux que le test croit eprouver.
b1 <- brut_s[brut_s$id == sj_s$table$id[1], ]
c1 <- serie_station(b1, 1985:2026)
pre1 <- calculer_un_indicateur(c1, INDICATEURS$pre, ctx_st())
verifier(!(2003L %in% pre1$annee) && !(2015L %in% pre1$annee) && 2012L %in% pre1$annee,
         "(controle) 2003 et 2015 sont ecartees, 2012 reste valide malgre ses 10 jours manquants")
verifier(pre1$n_manquants[pre1$annee == 2012L] == 10L,
         "(controle) 2012 compte 10 jours manquants dans le chargement complet")
b1_r <- serie_station(b1, 2026L, depuis = "2025-01-01")
b1_r <- base_station_attributs(b1_r, ouv_s); attr(b1_r, "base") <- ouv_s[c("jeu", "i", "note")]
pre1_r <- calculer_un_indicateur(b1_r, INDICATEURS$pre, ctx_st())
verifier(pre1_r$n_manquants[pre1_r$annee == 2012L] == 10L,
         "les 10 jours manquants de 2012 viennent de la base, pas de zero")
verifier(identical(attr(pre1_r, "meta")$annees_exclues, attr(pre1, "meta")$annees_exclues) &&
           all(c(2003L, 2015L) %in% attr(pre1_r, "meta")$annees_exclues),
         "les annees ecartees (2003, 2015) sont nommees en mode rapide comme en mode complet")

# Un pluviometre : la temperature n'est pas « non mesuree » parce que l'annee chargee est vide, elle
# l'est parce que la base n'a rien non plus.
b3 <- brut_s[brut_s$id == sj_s$table$id[3], ]
ouv3 <- base_ouvrir_station(sj_s$table$id[3], tr_s)
v3 <- serie_station(b3, 2026L, depuis = "2025-01-01"); v3 <- base_station_attributs(v3, ouv3)
attr(v3, "base") <- ouv3[c("jeu", "i", "note")]
tmp3 <- calculer_un_indicateur(v3, INDICATEURS$tmp, ctx_st())
verifier(!nrow(tmp3) && identical(attr(tmp3, "raison"), "variable_absente"),
         "un pluviometre : la temperature est refusee avec la raison « variable non mesuree »")


# ==============================================================================
groupe("v2.2 stations — une variable qui s'arrete : la base tranche, pas l'annee chargee")
# ==============================================================================

# Station dont le vent est mesure jusqu'a fin 2025 puis plus du tout : l'annee 2026 chargee n'a
# aucune valeur de vent, mais l'historique existe. Le chargement complet montre le vent des annees
# valides ; le mode rapide doit faire de meme, et non « vent non mesure ».
b_vent <- brut_s[brut_s$id == sj_s$table$id[2], ]
b_vent$ffm[b_vent$date >= as.Date("2026-01-01")] <- NA
complet_v <- serie_station(b_vent, 1985:2026)
vives_v <- serie_station(b_vent, 2026L, depuis = "2025-01-01")
ouv2 <- base_ouvrir_station(sj_s$table$id[2], tr_s)
vives_v <- base_station_attributs(vives_v, ouv2); attr(vives_v, "base") <- ouv2[c("jeu", "i", "note")]
a_v <- calculer_un_indicateur(complet_v, INDICATEURS$vent, ctx_st())
r_v <- calculer_un_indicateur(vives_v, INDICATEURS$vent, ctx_st())
verifier(is.data.frame(r_v) && nrow(r_v) > 30L && nrow(r_v) == nrow(a_v) - sum(a_v$annee == 2026L),
         "vent arrete en 2026 : le mode rapide montre les annees valides lues dans la base")


# ==============================================================================
groupe("v2.2 stations — garde-fous et jeu piege")
# ==============================================================================

ouv_g <- base_ouvrir_station(sj_s$table$id[1], tr_s)
verifier(isTRUE(base_applicable(ctx_st(), ouv_g$jeu)$ok), "avec les reglages du precalcul, la base d'une station sert")
verifier(identical(base_applicable(ctx_creer(ref = 1981:2010, courante = 2026L), ouv_g$jeu)$raison, "reference"),
         "une autre periode de reference : la base d'une station ne sert pas")
verifier(identical(base_applicable(ctx_st(fenetre = c(5L, 9L)), ouv_g$jeu)$raison, "reglages"),
         "une autre fenetre de saison : la base d'une station ne sert pas")
jeu_inc <- ouv_g$jeu; jeu_inc$meta[["base"]] <- jeu_inc$meta[["base"]][-1]
verifier(identical(base_applicable(ctx_st(), jeu_inc)$raison, "base_incomplete"),
         "une base de station incomplete est refusee en bloc")

# stations.json piege.
mt <- jsonlite::fromJSON(file.path(dos_s, "stations.json"), simplifyVector = FALSE)
verifier(stations_meta_valider(mt)$ok, "(controle) le stations.json ecrit est valide")
casse_s <- function(f) { m <- mt; m <- f(m); stations_meta_valider(m) }
verifier(!casse_s(function(m) { m[["base"]][[1]]$fichier <- "../x.bin.gz"; m })$ok, "fichier de la base avec chemin : refuse")
verifier(!casse_s(function(m) { m[["base"]][[1]]$stock <- 9; m })$ok, "precision de stockage hors 0..4 : refusee")
verifier(!casse_s(function(m) { m[["base"]][[1]]$type <- "autre"; m })$ok, "type different de celui du code : refuse")
verifier(!casse_s(function(m) { k <- which(vapply(m[["base"]], function(b) identical(b$type, "ref"), TRUE))[1]; m[["base"]][[k]]$lignes <- 7L; m })$ok,
         "nombre de lignes d'une reference different de celui du code : refuse")
verifier(!casse_s(function(m) { m[["base"]][[2]]$cle <- m[["base"]][[1]]$cle; m })$ok, "cle de base en double : refusee")
verifier(!casse_s(function(m) { m[["base"]] <- "oups"; m })$ok, "base qui n'est pas une liste : refusee")
verifier(!casse_s(function(m) { m[["base_parametres"]][["empreinte"]] <- strrep("x", 4000); m })$ok, "empreinte demesuree : refusee")
verifier(!casse_s(function(m) { m[["base_parametres"]] <- NULL; m })$ok, "base sans empreinte : refusee")
verifier(stations_meta_valider({ m <- mt; m[["base"]][[length(m[["base"]]) + 1L]] <- list(cle = "b_future_x", fichier = "s_b_future_x.bin.gz"); m })$ok,
         "une entree de base inconnue (jeu plus recent) est ignoree, pas refusee")
sans_b <- mt; sans_b[["base"]] <- NULL; sans_b[["base_parametres"]] <- NULL
verifier(stations_meta_valider(sans_b)$ok, "un stations.json sans base (v2.1) reste valide")
