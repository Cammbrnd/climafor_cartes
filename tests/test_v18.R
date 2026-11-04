# ==============================================================================
# tests/test_v18.R — ONGLET CARTE (VERSION 1.8)
# ------------------------------------------------------------------------------
# Ce que ces tests gardent :
#   1. les couches : chacune s'appuie sur un indicateur et une colonne qui existent ;
#   2. le format des fichiers : ce que R ecrit, R ET le code du navigateur le relisent
#      a l'identique (le navigateur est execute par Node, quand il est present) ;
#   3. la PARITE avec l'analyse d'un point : une valeur de la carte est celle que
#      l'onglet d'analyse afficherait (meme fonction de calcul, meme tendance) ;
#   4. l'assemblage : fragments, bornes de couleur, refus de publier un jeu troue ;
#   5. la securite : noms de fichiers, adresse, clic renvoye par le navigateur,
#      aucun HTML construit a partir des donnees ;
#   6. le raccord entre le squelette R et carte.js (memes data-role, memes palettes).
# ==============================================================================

lire18 <- function(f) readLines(chemin_ci(f), warn = FALSE, encoding = "UTF-8")
# Vrai si l'expression leve une erreur DE LA FONCTION TESTEE, dont le message contient
# `motif` : une fonction introuvable ou une faute de frappe ne passe pas pour un refus.
refus18 <- function(expr, motif) {
  m <- tryCatch({ expr; NA_character_ }, error = function(e) conditionMessage(e))
  !is.na(m) && !grepl("could not find function|object '.*' not found", m) && grepl(motif, m)
}
for (f in c("outils/carte_geo.R", "outils/carte_format.R", "outils/carte_precalcul.R",
            "outils/carte_demo.R", "outils/carte_sim.R", "outils/carte_sources.R")) source(chemin_ci(f))


# ==============================================================================
groupe("v1.8 — couches de la carte")
# ==============================================================================

cles18 <- names(CARTE_COUCHES)
verifier(length(cles18) >= 15 && !anyDuplicated(cles18), "les couches ont des cles distinctes")
verifier(all(vapply(names(CARTE_COUCHES), function(n) identical(CARTE_COUCHES[[n]]$cle, n), logical(1))),
         "chaque couche est rangee sous sa propre cle")
verifier(all(grepl("^[a-z0-9_]{1,40}$", cles18)), "les cles ne contiennent que des minuscules, chiffres et _")
verifier(all(vapply(CARTE_COUCHES, function(k) carte_nom_fichier_sur(sprintf("c_%s.bin.gz", k$cle)), logical(1))),
         "le fichier de chaque couche porte un nom publiable")
verifier(all(vapply(CARTE_COUCHES, function(k) k$source %in% names(INDICATEURS), logical(1))),
         "chaque couche s'appuie sur un indicateur du registre")
verifier(all(vapply(CARTE_COUCHES, function(k) k$palette %in% CARTE_PALETTES && k$ecart %in% CARTE_ECARTS, logical(1))),
         "palettes et couleurs d'ecart connues")
verifier(all(vapply(CARTE_COUCHES, function(k) k$stock >= k$decimales && k$stock <= 4, logical(1))),
         "on ne conserve jamais moins de decimales qu'on n'en affiche")
verifier(all(vapply(CARTE_COUCHES, function(k) carte_famille(k) %in% names(FAMILLES), logical(1))),
         "chaque couche se range dans une famille connue du selecteur")
verifier(carte_famille(CARTE_COUCHES$jja_p) == "hydrique" && carte_famille(CARTE_COUCHES$jja_t) == "thermique",
         "les precipitations estivales se rangent avec l'eau, pas avec les temperatures")
verifier(!any(c("ombro", "bh", "pre_mois", "tmp_mois", "tmp_jour", "vent_mois") %in%
                vapply(CARTE_COUCHES, function(k) k$source, "")),
         "les profils mensuels et journaliers ne sont pas cartographies")

# Les colonnes declarees existent bien dans la sortie du calcul.
set.seed(18)
annees18 <- 1985:2022
src18 <- carte_demo_source(annees18)
don18 <- src18(1L, 2.0, 47.0)
ctx18 <- carte_ctx(annees18)
res18 <- lapply(unique(vapply(CARTE_COUCHES, function(k) k$source, "")),
                function(s) INDICATEURS[[s]]$calc(don18, ctx18))
names(res18) <- unique(vapply(CARTE_COUCHES, function(k) k$source, ""))
ok_col <- vapply(CARTE_COUCHES, function(k)
  k$colonne %in% names(res18[[k$source]]) &&
    (is.null(k$normale) || k$normale %in% names(res18[[k$source]])), logical(1))
verifier(all(ok_col), if (!all(ok_col)) paste("colonne introuvable :", paste(names(ok_col)[!ok_col], collapse = ", "))
         else "chaque colonne et chaque normale declarees existent dans le tableau annuel")


# ==============================================================================
groupe("v1.8 — geometrie : projection Lambert-93")
# ==============================================================================

# Valeurs de reference calculees avec PROJ (pyproj, EPSG:4326 -> 2154).
ref18 <- rbind(c(2.3522, 48.8566, 652469.023, 6862035.259),
               c(1.4442, 43.6047, 574357.820, 6279642.972),
               c(-4.4860, 48.3904, 146640.348, 6836261.626),
               c(9.4500, 42.1500, 1233538.675, 6138435.654))
p18 <- carte_vers_l93(ref18[, 1], ref18[, 2])
verifier(max(abs(p18$x - ref18[, 3]), abs(p18$y - ref18[, 4])) < 0.01,
         "la projection Lambert-93 retrouve les coordonnees de PROJ au centimetre")
b18 <- carte_depuis_l93(p18$x, p18$y)
verifier(max(abs(b18$lon - ref18[, 1]), abs(b18$lat - ref18[, 2])) < 1e-8, "la projection inverse est exacte")
c18 <- carte_carre(1.4442, 43.6047)
verifier(nrow(c18) == 5 && all(c18[1, ] == c18[5, ]), "une maille est un anneau ferme de 5 points")
lc18 <- carte_vers_l93(c18[1:4, 1], c18[1:4, 2])
cotes18 <- sqrt(diff(c(lc18$x, lc18$x[1]))^2 + diff(c(lc18$y, lc18$y[1]))^2)
verifier(all(abs(cotes18 - 8000) < 25), "les quatre cotes d'une maille mesurent 8 km (a l'arrondi pres)")

# Grille NATIVE (Lambert II etendu, EPSG:27572) : les fichiers SIM donnent LAMBX/LAMBY en hm.
# Reference : centres de mailles renvoyes par le service (x, y de « position », WGS84) et
# leurs coordonnees Lambert II etendu calculees avec PROJ (pyproj) ; ils tombent sur la
# grille x = 4000 + 8000 i, y = 1000 + 8000 j.
ref_l2e <- rbind(c(1.19942, 43.56497, 508000.1, 1840999.7), c(1.29829, 43.56597, 516000.1, 1841000.2),
                 c(1.39716, 43.56687, 523999.7, 1840999.6), c(1.49604, 43.56769, 532000.0, 1841000.2),
                 c(1.20227, 43.42119, 507999.7, 1824999.5), c(1.59678, 43.42462, 539999.7, 1824999.5),
                 c(1.19511, 43.78067, 508000.2, 1864999.9), c(1.59211, 43.78412, 540000.2, 1864999.6),
                 c(1.39598, 43.63877, 524000.1, 1848999.8), c(1.39479, 43.71067, 523999.8, 1856999.6))
l2e18 <- carte_vers_l2e(ref_l2e[, 1], ref_l2e[, 2])
verifier(max(abs(l2e18$x - ref_l2e[, 3]), abs(l2e18$y - ref_l2e[, 4])) < 0.5,
         "Lambert II etendu : retrouve PROJ a 50 cm pres (10 centres de mailles reels)")
ecart_grille18 <- function(v, origine) ((v - origine + 4000) %% 8000) - 4000   # distance a la grille, en m
verifier(max(abs(ecart_grille18(l2e18$x, 4000)), abs(ecart_grille18(l2e18$y, 1000))) <= 1,
         "les centres renvoyes par le service tombent sur la grille de 8 km (a 1 m pres)")
bl18 <- carte_depuis_l2e(l2e18$x, l2e18$y)
verifier(max(abs(bl18$lon - ref_l2e[, 1]), abs(bl18$lat - ref_l2e[, 2])) < 1e-7, "Lambert II etendu : aller-retour exact")
cl18 <- carte_carre_l2e(516000, 1841000)
vl18 <- carte_vers_l2e(cl18[1:4, 1], cl18[1:4, 2])
verifier(all(abs(sqrt(diff(c(vl18$x, vl18$x[1]))^2 + diff(c(vl18$y, vl18$y[1]))^2) - 8000) < 25) &&
           all(abs(c(vl18$x, vl18$y) - c(516000 + c(-4000, 4000, 4000, -4000), 1841000 + c(-4000, -4000, 4000, 4000))) < 20),
         "une maille native est un carre de 8 km, exactement a sa place (bords a 20 m, arrondi a 4 decimales)")


# ==============================================================================
groupe("v1.8 — format binaire")
# ==============================================================================

k18 <- carte_couche("test", "tmp", "t_moy", "Test", "°C", 1, 2, "chaud", "positif_rouge")
x18 <- c(12.3456, -3.2149, NA, 0, 319.99, -319.99)
e18 <- carte_encoder_valeurs(x18, 2)
verifier(identical(e18[3], CARTE_MANQUANT) && e18[1] == 1235L && e18[2] == -321L,
         "valeurs arrondies a la precision stockee, absence codee -32768")
verifier(presque_egal(carte_decoder_valeurs(e18, 2)[-3], round(x18[-3], 2), 1e-12), "decodage inverse")
verifier(refus18(carte_encoder_valeurs(400, 2, "x"), "plage d'un entier 16 bits"),
         "une valeur qui deborderait d'un entier 16 bits est refusee, pas tronquee")

for (n18 in c(5L, 8L, 9L)) {            # 3 remplissages differents (n %% 4 = 1, 0, 1)
  d18 <- tempfile(); dir.create(d18)
  na18 <- 4L
  v18 <- matrix(round(rnorm(na18 * n18, 12, 3), 2), na18, n18); v18[2, 2] <- NA
  nor18 <- runif(n18, 5, 15); pen18 <- rnorm(n18, 0, .2); sig18 <- rep(c(TRUE, FALSE), length.out = n18)
  f18 <- carte_ecrire_couche(d18, k18, nor18, pen18, sig18, v18)
  lu18 <- carte_lire_couche(f18, n18, na18, 2)
  verifier(lu18$taille_exacte && presque_egal(lu18$valeurs, v18, 1e-9) &&
             presque_egal(lu18$normale, nor18, 1e-5) && presque_egal(lu18$pente, pen18, 1e-6) &&
             identical(lu18$signif == 1L, sig18),
           sprintf("une couche de %d mailles se relit a l'identique, sans octet en trop", n18))
  octets18 <- length(memDecompress(readBin(f18, "raw", file.size(f18)), "gzip", asChar = FALSE))
  verifier(octets18 == 8 * n18 + n18 + ((4 - n18 %% 4) %% 4) + 2 * na18 * n18,
           sprintf("taille decompressee attendue par le navigateur (n = %d)", n18))
}


# ==============================================================================
groupe("v1.8 — parite avec l'analyse d'un point")
# ==============================================================================

# La valeur d'une couche pour une annee est la valeur du tableau annuel que
# l'onglet d'analyse afficherait : on la recalcule ICI par l'appel direct.
out18 <- carte_calculer_maille(don18, ctx18, annees18)
diff_max <- 0; ecarts_norm <- 0; ecarts_pente <- 0; n_cmp <- 0
memes_annees18 <- logical(0); memes_signif18 <- logical(0)
for (k in CARTE_COUCHES) {
  r <- res18[[k$source]]
  attendu <- r[[k$colonne]][match(annees18, r$annee)]
  attendu[r$partielle[match(annees18, r$annee)] %in% TRUE] <- NA
  o <- out18[[k$cle]]
  fin <- is.finite(attendu)
  memes_annees18 <- c(memes_annees18, identical(is.finite(o$valeurs), fin))
  diff_max <- max(diff_max, abs(o$valeurs[fin] - attendu[fin]), 0)
  nor <- if (!is.null(k$normale)) r[[k$normale]][1] else stats_ref(r, k$colonne, ctx18)$normale
  ecarts_norm <- max(ecarts_norm, abs(o$normale - nor), 0, na.rm = TRUE)
  t <- tendance(annees18, attendu, unite = k$unite, dec = k$decimales)
  if (isTRUE(t$ok)) ecarts_pente <- max(ecarts_pente, abs(o$pente - t$pente_dec))
  memes_signif18 <- c(memes_signif18, identical(o$signif, isTRUE(t$ok) && isTRUE(t$significative)))
  n_cmp <- n_cmp + sum(fin)
}
verifier(all(memes_annees18), "chaque couche est renseignee pour les memes annees que le tableau d'analyse")
verifier(all(memes_signif18), "chaque couche porte le meme verdict de significativite que tendance()")
verifier(n_cmp > 500 && diff_max == 0, "les valeurs de la carte sont celles du calcul d'analyse, au bit pres")
verifier(ecarts_norm < 1e-12, "la normale de la carte est celle de l'analyse (colonne ou stats_ref)")
verifier(ecarts_pente < 1e-12, "la tendance de la carte est celle de tendance() utilisee par l'analyse")

# Une annee dont la serie est incomplete n'est cartographiee pour aucune couche.
don_part18 <- don18[!(don18$annee == 2022 & don18$mois > 6), ]
out_part18 <- carte_calculer_maille(don_part18, ctx18, annees18)
derniere18 <- vapply(out_part18, function(o) o$valeurs[length(annees18)], 0)
verifier(all(is.na(derniere18)), "une annee tronquee en juin est absente de TOUTES les couches")
avant18 <- vapply(out_part18, function(o) sum(is.finite(o$valeurs[-length(annees18)])), 0)
verifier(all(avant18 > 0 | names(avant18) %in% c("neige", "can3", "geltd")),
         "les annees completes restent renseignees")

# Une maille sans donnees : aucune valeur, pas d'erreur.
vide18 <- carte_calculer_maille(don18[0, ], ctx18, annees18)
verifier(all(vapply(vide18, function(o) !any(is.finite(o$valeurs)), logical(1))),
         "une serie vide donne des couches vides sans lever d'erreur")


# ==============================================================================
groupe("v1.8 — bornes de couleur")
# ==============================================================================

set.seed(3)
va18 <- matrix(rnorm(40 * 30, 10, 2), 40, 30)
dm18 <- carte_domaines(va18, rep(10, 30), rnorm(30, 0.1, 0.05))
verifier(dm18$domaine[1] < dm18$domaine[2] && dm18$domaine[1] <= quantile(va18, .02) &&
           dm18$domaine[2] >= quantile(va18, .98), "les bornes englobent les percentiles 2 et 98")
verifier(dm18$domaine_ecart[1] == -dm18$domaine_ecart[2] && dm18$domaine_pente[1] == -dm18$domaine_pente[2],
         "ecarts et tendances sont symetriques autour de zero (le blanc = la normale)")
vide_dm18 <- carte_domaines(matrix(NA_real_, 3, 4), rep(NA_real_, 4), rep(NA_real_, 4))
verifier(all(vapply(vide_dm18, function(d) d[1] < d[2], logical(1))), "des bornes valides meme sans aucune valeur")
const_dm18 <- carte_domaines(matrix(5, 3, 4), rep(5, 4), rep(0, 4))
verifier(all(vapply(const_dm18, function(d) d[1] < d[2], logical(1))), "des bornes valides pour des valeurs toutes egales")


# ==============================================================================
groupe("v1.8 — assemblage et lecture par le navigateur")
# ==============================================================================

mailles18 <- data.frame(lon = c(2.0, -1.5, 5.4, 3.1, 1.2, 6.2, -0.2), lat = c(47.0, 44.5, 43.5, 49.9, 45.3, 47.8, 48.5))
n18m <- nrow(mailles18)
cache18 <- list()
lire18m <- function(i, lon, lat) {
  if (is.null(cache18[[as.character(i)]])) cache18[[as.character(i)]] <<- src18(i, lon, lat)
  cache18[[as.character(i)]]
}
fr18a <- carte_construire(lire18m, mailles18$lon, mailles18$lat, annees18, c(1L, 3L, 5L, 7L), progres_tous = 1e6)
fr18b <- carte_construire(lire18m, mailles18$lon, mailles18$lat, annees18, c(2L, 4L, 6L), progres_tous = 1e6)
dos18 <- tempfile("carte"); 
r18 <- carte_assembler(list(fr18b, fr18a), mailles18$lon, mailles18$lat, annees18, CARTE_DEMO_SOURCE, dos18)
verifier(r18$n == n18m && !length(r18$echecs), "deux fragments, melanges dans le desordre, donnent un jeu complet")
lu18 <- carte_lire_meta_locale(dos18)
verifier(!is.null(lu18$meta) && identical(unlist(lu18$meta$annees), annees18),
         "le jeu produit passe la validation de l'application")
verifier(lu18$meta$reference == "1991-2020" && grepl("FICTIVES", lu18$meta$source),
         "la reference est celle de l'application et le jeu fictif porte sa mention")

# Les valeurs relues sont celles du calcul direct, a la precision stockee pres.
ecart_rel18 <- 0; memes18 <- TRUE
for (i in seq_len(n18m)) {
  direct <- carte_calculer_maille(lire18m(i, 0, 0), ctx18, annees18)
  for (k in CARTE_COUCHES) {
    lc <- carte_lire_couche(file.path(dos18, sprintf("c_%s.bin.gz", k$cle)), n18m, length(annees18), k$stock)
    a <- lc$valeurs[, i]; b <- direct[[k$cle]]$valeurs
    ecart_rel18 <- max(ecart_rel18, abs(a - b)[is.finite(b)] / (0.5 * 10^-k$stock), 0)
    memes18 <- memes18 && identical(is.finite(a), is.finite(b))
  }
}
verifier(memes18, "apres relecture, chaque couche de chaque maille garde les memes annees renseignees")
verifier(ecart_rel18 <= 1 + 1e-9, "apres relecture, aucune valeur ne s'ecarte de plus de la moitie du dernier chiffre conserve")

g18 <- jsonlite::fromJSON(file.path(dos18, "grille.json"), simplifyVector = FALSE)
verifier(length(g18$features) == n18m &&
           identical(vapply(g18$features, function(f) f$properties$id, 0L), 0:(n18m - 1L)),
         "la grille compte une maille par rang 0..n-1")
verifier(all(vapply(g18$features, function(f) {
  r <- f$geometry$coordinates[[1]]; length(r) == 5 && identical(r[[1]], r[[5]])
}, logical(1))), "chaque maille est un anneau ferme")

# Refus de publier un jeu troue ou incomplet.
src_mauvaise18 <- function(i, lon, lat) if (i %in% c(2L, 3L, 4L)) NULL else lire18m(i, lon, lat)
fr_trou18 <- carte_construire(src_mauvaise18, mailles18$lon, mailles18$lat, annees18, seq_len(n18m), progres_tous = 1e6)
verifier(identical(fr_trou18$echecs, c(2L, 3L, 4L)), "les mailles sans donnees sont comptees")
verifier(refus18(carte_assembler(list(fr_trou18), mailles18$lon, mailles18$lat, annees18, "x", tempfile()), "publication refusee"),
         "plus de 1 % de mailles sans donnees : publication refusee")
verifier(refus18(carte_assembler(list(fr18a), mailles18$lon, mailles18$lat, annees18, "x", tempfile()), "aucune part"),
         "une maille traitee par aucun fragment : publication refusee")

# Le navigateur (carte.js, execute par Node) relit les memes fichiers a l'identique.
if (nzchar(Sys.which("node"))) {
  js18 <- tryCatch(jsonlite::fromJSON(system2("node", c(shQuote(chemin_ci("tests/carte_verif_js.js")), shQuote(dos18)),
                                               stdout = TRUE, stderr = FALSE), simplifyVector = FALSE),
                   error = function(e) NULL)
  verifier(!is.null(js18), "carte.js lit meta.json, la grille et les couches ecrites par R")
  if (!is.null(js18)) {
    verifier(js18$n == n18m && identical(unlist(js18$annees), annees18), "memes mailles et memes annees des deux cotes")
    egal <- vapply(CARTE_COUCHES, function(k) {
      j <- js18$couches[[k$cle]]
      lc <- carte_lire_couche(file.path(dos18, sprintf("c_%s.bin.gz", k$cle)), n18m, length(annees18), k$stock)
      val_js <- matrix(vapply(unlist(j$valeurs, recursive = FALSE), function(x) if (is.null(x)) NA_real_ else x, 0),
                       nrow = length(annees18), byrow = TRUE)
      num <- function(l) vapply(l, function(x) if (is.null(x)) NA_real_ else x, 0)
      identical(is.na(val_js), is.na(lc$valeurs)) && presque_egal(val_js[!is.na(val_js)], lc$valeurs[!is.na(lc$valeurs)], 1e-9) &&
        presque_egal(num(j$normale), lc$normale, 1e-9) && presque_egal(num(j$pente), lc$pente, 1e-9) &&
        identical(as.integer(unlist(j$signif)), as.integer(lc$signif))
    }, logical(1))
    verifier(all(egal), if (!all(egal)) paste("difference R / navigateur :", paste(names(egal)[!egal], collapse = ", "))
             else "valeurs, normales, tendances et significativite : R et le navigateur lisent la meme chose")
    centres_js <- do.call(rbind, lapply(js18$centres, unlist))
    verifier(max(abs(centres_js[, 1] - mailles18$lon), abs(centres_js[, 2] - mailles18$lat)) < 0.05,
             "le centre de chaque maille, relu par le navigateur, retombe sur le point d'origine")
    verifier(all(unlist(js18$rejets)), if (!all(unlist(js18$rejets))) paste("accepte a tort :", paste(names(js18$rejets)[!unlist(js18$rejets)], collapse = ", "))
             else "le navigateur refuse chemins, URL, palettes inconnues, domaines inverses, fichiers tronques et grilles alterees")
  }
} else {
  cat("   (Node absent : controle croise R / navigateur ignore)\n")
}


# ==============================================================================
groupe("v1.8 — securite des entrees")
# ==============================================================================

meta_ok18 <- lu18$meta
alterer18 <- function(f) { m <- meta_ok18; f(m) }
verifier(carte_meta_valider(meta_ok18)$ok, "un meta.json produit par le precalcul est valide")
verifier(!carte_meta_valider(alterer18(function(m) { m$schema <- 2L; m }))$ok, "autre version du format : refusee")
verifier(!carte_meta_valider(alterer18(function(m) { m$couches[[1]]$fichier <- "../x.json"; m }))$ok, "remontee de dossier : refusee")
verifier(!carte_meta_valider(alterer18(function(m) { m$couches[[1]]$fichier <- "dossier/c_x.bin.gz"; m }))$ok, "sous-dossier : refuse")
verifier(!carte_meta_valider(alterer18(function(m) { m$couches[[1]]$fichier <- "C_TMP.bin.gz"; m }))$ok, "majuscules : refusees")
verifier(!carte_meta_valider(alterer18(function(m) { m$couches[[1]]$palette <- "x"; m }))$ok, "palette inconnue : refusee")
verifier(!carte_meta_valider(alterer18(function(m) { m$couches[[1]]$domaine <- list(5, 1); m }))$ok, "domaine inverse : refuse")
verifier(!carte_meta_valider(alterer18(function(m) { m$n_mailles <- 10^6; m }))$ok, "trop de mailles : refuse")
verifier(!carte_meta_valider(alterer18(function(m) { m$couches[[2]]$cle <- m$couches[[1]]$cle; m }))$ok, "cles en double : refusees")
verifier(!carte_meta_valider(alterer18(function(m) { m$couches[[1]]$cle <- "inconnue"; m }))$ok, "couche absente du registre : refusee")
verifier(!carte_meta_valider(alterer18(function(m) { m$annees <- list(2000, 1999); m }))$ok, "annees desordonnees : refusees")
verifier(!carte_meta_valider("n'importe quoi")$ok && !carte_meta_valider(NULL)$ok, "un contenu qui n'est pas une liste : refuse")
verifier(!carte_meta_valider(alterer18(function(m) { m$couches <- list(5); m }))$ok, "une couche qui n'est pas une liste : refusee")

verifier(carte_url_sure("https://cammbrnd.github.io/climafor_carte/") && carte_url_sure("https://exemple.fr:8443/a/b"),
         "une adresse https simple est acceptee")
verifier(!carte_url_sure("http://exemple.fr/") && !carte_url_sure("https://u:p@exemple.fr/") &&
           !carte_url_sure("https://exemple.fr/../x") && !carte_url_sure("javascript:alert(1)") &&
           !carte_url_sure("https://exemple.fr/ a") && !carte_url_sure(NA) && !carte_url_sure(c("https://a.fr", "https://b.fr")),
         "http, identifiants, remontee de dossier, javascript:, espaces, NA, vecteurs : refuses")

verifier(!is.null(carte_point_depuis_clic(list(id = 12, lon = 2.123456, lat = 47.987654))) &&
           carte_point_depuis_clic(list(lon = 2.123456, lat = 47.987654))$lon == 2.1235,
         "un clic valide est arrondi a 4 decimales")
verifier(identical(carte_point_depuis_clic(list(lon = 2, lat = 47))$source, "carte"), "la source du point est « carte »")
verifier(is.null(carte_point_depuis_clic(NULL)) && is.null(carte_point_depuis_clic("x")) &&
           is.null(carte_point_depuis_clic(list(lon = NA, lat = 47))) && is.null(carte_point_depuis_clic(list(lon = Inf, lat = 47))) &&
           is.null(carte_point_depuis_clic(list(lon = "a", lat = 47))) && is.null(carte_point_depuis_clic(list(lon = 2))) &&
           is.null(carte_point_depuis_clic(list(lon = 2, lat = 60))) && is.null(carte_point_depuis_clic(list(lon = 30, lat = 47))),
         "NULL, texte, NA, infini, champ manquant, hors du territoire : tous refuses")
verifier(is.character(carte_point_depuis_clic(list(lon = 2, lat = 47, nom = "<img onerror=x>"))$nom) &&
           !grepl("<", carte_point_depuis_clic(list(lon = 2, lat = 47, nom = "<img onerror=x>"))$nom),
         "le nom du point est construit cote serveur : un nom envoye par le navigateur est ignore")

# Transport : distant (https), local (dossier valide), absent.
verifier(identical(carte_transport(url = "https://x.github.io/c", dossier = "inexistant")$base, "https://x.github.io/c/"),
         "adresse distante : base terminee par /")
verifier(identical(carte_transport(url = "http://x.fr", dossier = dos18)$mode, "absent"),
         "adresse http : jeu declare indisponible plutot que charge en clair")
tr18 <- carte_transport(url = "", dossier = dos18)
verifier(identical(tr18$mode, "local") && identical(tr18$base, paste0(CONFIG$carte$ressource, "/")),
         "dossier local valide : servi par l'application elle-meme")
vide_dos18 <- tempfile(); dir.create(vide_dos18)
verifier(identical(carte_transport(url = "", dossier = vide_dos18)$mode, "absent") &&
           length(carte_transport(url = "", dossier = vide_dos18)$erreurs) >= 1L,
         "aucun jeu : l'onglet l'explique")
casse18 <- tempfile(); dir.create(casse18); file.copy(list.files(dos18, full.names = TRUE), casse18)
file.remove(file.path(casse18, "c_tmp.bin.gz"))
verifier(identical(carte_transport(url = "", dossier = casse18)$mode, "absent"),
         "un fichier de couche manquant : jeu refuse en entier")


# ==============================================================================
groupe("v1.8 — raccord squelette R / carte.js")
# ==============================================================================

js_txt18 <- paste(lire18("www/carte/carte.js"), collapse = "\n")
html18 <- as.character(mod_carte_ui("carte"))
roles_js18 <- unique(c(
  sub('^q\\("(.*)"\\)$', "\\1", regmatches(js_txt18, gregexpr('q\\("[a-z-]+"\\)', js_txt18))[[1]]),
  sub('^.*data-role="(.*)"\\]$', "\\1", regmatches(js_txt18, gregexpr('data-role="[a-z-]+"\\]', js_txt18))[[1]])))
roles_html18 <- unique(sub('^data-role="(.*)"$', "\\1", regmatches(html18, gregexpr('data-role="[a-z-]+"', html18))[[1]]))
verifier(length(roles_js18) >= 10 && !length(setdiff(roles_js18, roles_html18)),
         if (length(setdiff(roles_js18, roles_html18))) paste("carte.js cherche un element absent du squelette :", paste(setdiff(roles_js18, roles_html18), collapse = ", "))
         else "chaque data-role que lit carte.js existe dans le squelette R")
verifier(grepl('id="carte-racine"', html18, fixed = TRUE) && grepl('id="carte-couche"', html18, fixed = TRUE) &&
           grepl('for="carte-couche"', html18, fixed = TRUE),
         "le squelette est dans l'espace de noms du module, et l'etiquette designe son champ")
verifier(grepl("hidden", html18, fixed = TRUE), "la carte reste masquee tant que les donnees ne sont pas chargees")

pal_js18 <- regmatches(js_txt18, regexec("(?s)var SEQ = \\{(.*?)\\n  \\};", js_txt18, perl = TRUE))[[1]][2]
cles_pal18 <- unique(sub("^\\s*([a-z]+):.*$", "\\1", grep("^\\s+[a-z]+:\\s+\\[", strsplit(pal_js18, "\n")[[1]], value = TRUE)))
verifier(setequal(cles_pal18, CARTE_PALETTES), "carte.js connait exactement les palettes de valeurs de CARTE_PALETTES")
div_js18 <- regmatches(js_txt18, regexec("(?s)var DIV = \\{(.*?)\\n  \\};", js_txt18, perl = TRUE))[[1]][2]
cles_div18 <- unique(unlist(regmatches(div_js18, gregexpr("positif_[a-z]+", div_js18))))
verifier(setequal(cles_div18, CARTE_ECARTS), "carte.js connait exactement les couleurs d'ecart de CARTE_ECARTS")

nom_sur_js18 <- regmatches(js_txt18, regexec("var NOM_SUR = /(.*)/;", js_txt18))[[1]][2]
verifier(identical(nom_sur_js18, "^[a-z0-9_]{1,60}\\.(json|bin\\.gz)$"),
         "carte.js applique le meme motif de nom de fichier que carte_nom_fichier_sur()")
verifier(grepl(sprintf("n <= %d", CONFIG$carte$n_mailles_max), js_txt18, fixed = TRUE),
         "carte.js applique le meme plafond de mailles que CONFIG$carte$n_mailles_max")
fam_js18 <- unlist(regmatches(js_txt18, gregexpr("\\b(hydrique|thermique|gel|aridite|phenologie|neige|vent):", js_txt18)))
verifier(all(vapply(CARTE_COUCHES, function(k) any(grepl(sprintf("\\b%s:", carte_famille(k)), fam_js18)), logical(1))),
         "chaque famille de couche a un intitule dans le selecteur de carte.js")

# Securite du composant : aucun HTML construit, aucune evaluation de chaine.
code_js18 <- gsub("(?s)/\\*.*?\\*/", "", js_txt18, perl = TRUE)          # commentaires /* ... */
code_js18 <- gsub("(?m)^\\s*//.*$", "", code_js18, perl = TRUE)            # commentaires de ligne
verifier(!grepl("innerHTML|outerHTML|insertAdjacentHTML|document\\.write|\\beval\\(|new Function|setTimeout\\(\\s*[\"']|\\.html\\(", code_js18),
         "carte.js n'insere jamais de HTML et n'evalue aucune chaine : tout passe par textContent")
verifier(grepl("credentials: \"omit\"", js_txt18, fixed = TRUE), "les fichiers sont demandes sans cookies ni identifiants")
verifier(!grepl("fetch\\(\\s*[\"']https?://|src:\\s*[\"']https?://|href:\\s*[\"']https?://", code_js18),
         "carte.js ne contacte aucune adresse ecrite en dur (seul l'espace de noms SVG est une URL)")
verifier(grepl('"cf-carte-init"', js_txt18, fixed = TRUE) &&
           grepl('sendCustomMessage("cf-carte-init"', paste(lire18("src/mod_carte.R"), collapse = "\n"), fixed = TRUE) &&
           grepl("DOMContentLoaded", js_txt18, fixed = TRUE),
         "le message d'initialisation a le meme nom des deux cotes, et carte.js s'enregistre meme si Shiny charge apres lui")
verifier(file.exists(chemin_ci("www/carte/lib/maplibre-gl.js")) && file.exists(chemin_ci("www/carte/lib/LICENSE-maplibre-gl.txt")),
         "MapLibre est embarque avec sa licence (aucun CDN)")


# ==============================================================================
groupe("v1.8 — integration dans l'application")
# ==============================================================================

app18 <- paste(lire18("app.R"), collapse = "\n")
verifier(grepl('value = "carte"', app18, fixed = TRUE) && grepl('mod_carte_ui("carte")', app18, fixed = TRUE) &&
           grepl('mod_carte_server\\(\\s*"carte"', app18), "l'onglet Carte est branche dans l'interface et le serveur")
verifier(grepl('src = "carte/carte.js"', app18, fixed = TRUE), "app.R charge carte.js")
mod_st18 <- paste(lire18("src/mod_station.R"), collapse = "\n")
verifier(sum(gregexpr("point = r\\$point", mod_st18)[[1]] > 0) >= 3L,
         "les trois sorties du module Station exposent le point analyse (utilise pour lancer l'analyse d'une maille)")
verifier(grepl("mod_carte_ouverture_server(", app18, fixed = TRUE), "app.R confie l'ouverture d'une maille au module Carte")
mod_carte18 <- paste(lire18("src/mod_carte.R"), collapse = "\n")
verifier(grepl("s$point", mod_carte18, fixed = TRUE),
         "l'analyse d'une maille attend que la source en vigueur se rapporte au nouveau point")

# Comportement du raccord, execute par le simulateur de serveur de Shiny : le clic
# pose le point, MAIS l'analyse ne part qu'une fois la source passee au nouveau
# point (sinon elle partirait avec la station de l'ancien).
ancien_actif18 <- CONFIG$stations$actif
CONFIG$stations$actif <- TRUE
p18 <- list(lon = 3.5, lat = 45.5, nom = "Maille test", source = "carte")
shiny::testServer(function(input, output, session) {
  t <- list(lieu = shiny::reactiveVal(NULL), choix = shiny::reactiveVal(NULL),
            src = shiny::reactiveVal(NULL), lances = shiny::reactiveVal(0L))
  t$ouvrir <- mod_carte_ouverture_server(t$lieu, t$choix, t$src,
                                         function() t$lances(shiny::isolate(t$lances()) + 1L))
  session$userData$t <- t
}, {
  t <- session$userData$t
  suppressMessages(t$ouvrir(p18)); session$flushReact()
  verifier(identical(t$choix(), "safran") && identical(t$lieu()$lon, 3.5),
           "un clic demande la source SAFRAN et pose le point")
  verifier(t$lances() == 0L, "l'analyse n'est pas lancee tant que la source decrit un autre point")
  t$src(list(point = list(lon = 1.1, lat = 44.4))); session$flushReact()
  verifier(t$lances() == 0L, "...meme quand une source existe, si elle se rapporte a l'ancien point")
  t$src(list(point = list(lon = 3.5, lat = 45.5))); session$flushReact()
  verifier(t$lances() == 1L, "l'analyse part une fois, des que la source se rapporte au nouveau point")
  t$src(list(point = list(lon = 3.5, lat = 45.5001))); session$flushReact()
  verifier(t$lances() == 1L, "elle ne repart pas d'elle-meme ensuite")
})
CONFIG$stations$actif <- ancien_actif18
verifier(numeric_version(CONFIG$app$version) >= "1.8.0", "la version est au moins 1.8.0")
desc18 <- read.dcf(chemin_ci("DESCRIPTION"))
verifier(identical(unname(desc18[1, "Version"]), CONFIG$app$version), "DESCRIPTION et CONFIG portent la meme version")


# ==============================================================================
groupe("v1.8 — derniere annee pas encore complete")
# ==============================================================================

# SAFRAN publie avec retard : si la derniere annee demandee est (presque) vide, elle
# est retiree du jeu publie et le dit, plutot que de produire une carte blanche.
vide_fin18 <- fr18a
for (cle in names(CARTE_COUCHES)) vide_fin18$valeurs[[cle]][length(annees18), ] <- NA_real_
vide_fin18b <- fr18b
for (cle in names(CARTE_COUCHES)) vide_fin18b$valeurs[[cle]][length(annees18), ] <- NA_real_
dos_fin18 <- tempfile("cartefin")
r_fin18 <- carte_assembler(list(vide_fin18, vide_fin18b), mailles18$lon, mailles18$lat, annees18, "x", dos_fin18)
verifier(identical(r_fin18$annees_retirees, max(annees18)) && max(r_fin18$annees) == max(annees18) - 1L,
         "une derniere annee vide est retiree du jeu et signalee")
lu_fin18 <- carte_lire_meta_locale(dos_fin18)
verifier(!is.null(lu_fin18$meta) && max(unlist(lu_fin18$meta$annees)) == max(annees18) - 1L,
         "le jeu publie s'arrete a la derniere annee complete et reste valide")
verifier(length(r18$annees_retirees) == 0L, "un jeu complet ne perd aucune annee")


# ==============================================================================
groupe("v1.8 — ligne de commande du precalcul")
# ==============================================================================

cli18 <- function(...) {
  system2(file.path(R.home("bin"), "Rscript"), c(shQuote(chemin_ci("outils/carte_cli.R")), ...),
          stdout = FALSE, stderr = FALSE)
}
base_cli18 <- tempfile("cli"); dir.create(base_cli18)
comm18 <- c("mailles=demo:160", "annee_debut=1990", "annee_fin=2022")
s1 <- cli18("fragment", "source=demo", comm18, "fragment=1", "fragments=2", paste0("sortie=", base_cli18, "/parts"))
s2 <- cli18("fragment", "source=demo", comm18, "fragment=2", "fragments=2", paste0("sortie=", base_cli18, "/parts"))
verifier(s1 == 0L && s2 == 0L && length(list.files(file.path(base_cli18, "parts"), pattern = "^part_00[12]\\.rds$")) == 2L,
         "deux fragments se calculent et laissent leurs parts")
s3 <- cli18("assembler", shQuote("source_texte=DONNEES FICTIVES (essai)"), comm18,
            paste0("parts=", base_cli18, "/parts"), paste0("sortie=", base_cli18, "/public"))
verifier(s3 == 0L && file.exists(file.path(base_cli18, "public", "meta.json")), "l'assemblage ecrit le jeu")
verifier(cli18("verifier", paste0("dossier=", base_cli18, "/public")) == 0L, "la verification accepte le jeu produit")
file.remove(file.path(base_cli18, "public", "c_pre.bin.gz"))
verifier(cli18("verifier", paste0("dossier=", base_cli18, "/public")) != 0L,
         "la verification refuse (code de sortie non nul) un jeu dont un fichier manque")
verifier(cli18("fragment", "source=demo", comm18, "fragment=3", "fragments=2", paste0("sortie=", base_cli18, "/x")) != 0L,
         "un numero de fragment hors limites est refuse")
verifier(cli18("fragment", "source=inconnue", comm18, "fragment=1", "fragments=2", paste0("sortie=", base_cli18, "/x")) != 0L,
         "une source inconnue est refusee")
verifier(cli18("fragment", "source=demo", "mailles=inexistant.csv", "annee_debut=1990", "annee_fin=2022", "fragment=1", "fragments=2", paste0("sortie=", base_cli18, "/x")) != 0L,
         "un fichier de mailles introuvable est refuse")

mal18 <- tempfile(fileext = ".csv")
writeLines(c("lon,lat", "2.0,47.0", "2.0,47.0"), mal18)
verifier(refus18(carte_lire_mailles(mal18), "doublons"),
         "des mailles en double sont refusees")
writeLines(c("lon,lat", "2.0,47.0", "30,47"), mal18)
verifier(refus18(carte_lire_mailles(mal18), "hors du territoire"),
         "une maille hors du territoire est refusee")
writeLines(c("x,y", "2.0,47.0"), mal18)
verifier(refus18(carte_lire_mailles(mal18), "colonnes lon et lat"),
         "un fichier sans colonnes lon/lat est refuse")
writeLines(c("lon,lat", "2.0,47.0", "2.1,47.1"), mal18)
verifier(nrow(carte_lire_mailles(mal18)) == 2L, "un fichier de mailles correct est lu")


# ==============================================================================
groupe("v1.8 — workflows GitHub Actions")
# ==============================================================================

wf18 <- lapply(c("construire-carte", "diagnostic-geosas", "veille"),
               function(n) readLines(chemin_ci(sprintf(".github/workflows/%s.yml", n)), warn = FALSE, encoding = "UTF-8"))
names(wf18) <- c("construire", "diagnostic", "veille")
tout18 <- unlist(wf18)
verifier(!any(grepl("\t", tout18)), "aucune tabulation dans les workflows (YAML)")
verifier(!any(grepl("pull_request_target", tout18, fixed = TRUE)),
         "aucun workflow ne s'execute sur pull_request_target (code externe avec droits)")
# Une entree (inputs.*) ne doit jamais etre collee dans une commande : elle passe par une
# variable d'environnement, sinon un texte saisi pourrait etre execute comme du shell.
commandes18 <- grep("^\\s+(Rscript|echo|gh |curl|if \\[|URL=|TEXTE=)", tout18, value = TRUE)
verifier(!any(grepl("\\$\\{\\{\\s*inputs\\.", commandes18)),
         "les entrees des workflows passent par des variables d'environnement, jamais dans une commande")
verifier(any(grepl("if: \\$\\{\\{ inputs.source != 'demo' \\}\\}", wf18$construire)) &&
           sum(grepl("inputs.source != 'demo'", wf18$construire, fixed = TRUE)) >= 2L,
         "la source « demo » (donnees fictives) ne se publie jamais sur Pages")
verifier(any(grepl("concurrency:", wf18$construire, fixed = TRUE)) && any(grepl("max-parallel:", wf18$construire, fixed = TRUE)),
         "un seul calcul a la fois et un nombre borne de fragments simultanes (le service est menage)")
verifier(all(grepl("timeout-minutes", sapply(wf18, paste, collapse = "\n"))), "chaque workflow borne la duree de ses jobs")
verifier(!any(grepl("runs-on:\\s*ubuntu-latest", tout18)) && any(grepl("runs-on:\\s*ubuntu-24\\.04", tout18)),
         "les workflows fixent l'image du runner (ubuntu-latest est passe a 26.04 : l'installation de R s'y fige)")
verifier(!any(grepl("cron:.*\\*/[0-9]", tout18)) && any(grepl("cron:", wf18$veille, fixed = TRUE)) &&
           !any(grepl("cron:", wf18$construire, fixed = TRUE)),
         "seule la veille est planifiee (mensuelle) ; le calcul complet ne part jamais seul")
verifier(any(grepl("donnees_grille/mailles_safran.csv", wf18$construire, fixed = TRUE)) &&
           file.exists(chemin_ci("donnees_grille/LISEZMOI.md")),
         "le fichier de mailles attendu par le workflow est documente")


# ==============================================================================
groupe("v1.8 — source « sim » (fichiers en masse de Meteo-France)")
# ==============================================================================

# Un jeu SIM FICTIF au format observe (14 colonnes utiles + Q en intrus), 110 mailles de la
# grille native (pas de 8 km), 'annees' annees completes. Aucun reseau.
sim_fabriquer18 <- function(dossier, annees) {
  dir.create(dossier, showWarnings = FALSE, recursive = TRUE)
  g <- expand.grid(i = 0:9, j = 0:10)
  lx <- 5000L + 80L * g$i; ly <- 18000L + 80L * g$j            # hectometres
  writeLines(c("LAMBX (hm);LAMBY (hm);LAT_DG;LON_DG",
               sprintf("%d;%d;%s;%s", lx, ly, "43,5", "1,4")), file.path(dossier, "coordonnees.csv"))
  for (a in annees) {
    dd <- seq(as.Date(sprintf("%d-01-01", a)), as.Date(sprintf("%d-12-31", a)), by = "day")
    nd <- length(dd); d <- seq_len(nd)
    sorties <- lapply(seq_along(lx), function(c) {
      t <- round(11 + 9 * sin(2 * pi * (d - 110) / 365) + 0.05 * (c %% 7) + 0.1 * ((a * 7 + d * 3 + c) %% 11), 1)
      data.frame(LAMBX = lx[c], LAMBY = ly[c], DATE = as.integer(format(dd, "%Y%m%d")),
        PRENEI = round(ifelse(t < 1 & d %% 3 == 0, 1.2, 0), 1),
        PRELIQ = round(ifelse((d * c + a) %% 4 == 0, ((d + c) %% 13) * 0.7, 0), 1),
        T = t, FF = round(2 + (d %% 6) * 0.4, 1), Q = 9.4,
        HU = round(65 + 15 * sin(2 * pi * d / 365) + (c %% 5), 1),
        ETP = round(pmax(0.2, 2.2 + 2 * sin(2 * pi * (d - 80) / 365)), 1),
        SWI = round(0.45 + 0.3 * sin(2 * pi * (d - 30) / 365) + 0.001 * (c %% 20), 3),
        RESR_NEIGE = round(ifelse(t < 0, 12.3, 0), 1),
        HTEURNEIGE = round(ifelse(t < 0, 0.05 * (d %% 7), 0), 3),
        TINF_H = round(t - 4 - 0.1 * (d %% 5), 1), TSUP_H = round(t + 5 + 0.1 * (c %% 4), 1))
    })
    f <- file.path(dossier, sprintf("QUOT_SIM2_%d.csv", a))
    utils::write.table(do.call(rbind, sorties), f, sep = ";", quote = FALSE, row.names = FALSE)
    system2("gzip", c("-f", shQuote(f)))
  }
  invisible(dossier)
}
dos_sim18 <- tempfile("sim"); sim_annees18 <- 2003:2022
sim_fabriquer18(dos_sim18, sim_annees18)
tous_api18 <- vapply(CONFIG$variables, function(v) v$api, "")

m_sim18 <- carte_sim_mailles(file.path(dos_sim18, "coordonnees.csv"))
verifier(nrow(m_sim18) == 110L && all(c("lon", "lat", "lambx", "lamby") %in% names(m_sim18)),
         "la grille native se lit : 110 mailles, avec leurs coordonnees d'origine")
v_l2e18 <- carte_vers_l2e(m_sim18$lon, m_sim18$lat)
verifier(max(abs(v_l2e18$x - m_sim18$lambx * 100), abs(v_l2e18$y - m_sim18$lamby * 100)) < 0.5,
         "le centre en lon/lat de chaque maille retombe sur ses coordonnees d'origine (50 cm)")
verifier(refus18(carte_sim_mailles(file.path(dos_sim18, "absent.csv")), "introuvable"),
         "un fichier de coordonnees absent est refuse")

# Quelques mailles du domaine SAFRAN depassent la borne de l'application (4 mailles a lon 10,00-10,04,
# constat reel) : elles sont ecartees, avec un message. Si elles sont nombreuses, le fichier est refuse.
coord_hors18 <- function(n_hors) {
  f <- tempfile(fileext = ".csv")
  lx <- c(5000L + 80L * rep(0:9, 11), if (n_hors == 1L) 11800L else 6000L + 80L * seq_len(n_hors))
  ly <- c(18000L + 80L * rep(0:10, each = 10), rep(if (n_hors == 1L) 22970L else 29000L, n_hors))
  writeLines(c("LAMBX (hm);LAMBY (hm);LAT_DG;LON_DG", sprintf("%d;%d;47,4;10,03", lx, ly)), f)
  f
}
f_hors18 <- coord_hors18(1L)
msg_hors18 <- character(0)
m_hors18 <- withCallingHandlers(carte_sim_mailles(f_hors18),
  message = function(e) { msg_hors18 <<- c(msg_hors18, conditionMessage(e)); invokeRestart("muffleMessage") })
verifier(nrow(m_hors18) == 110L && !anyDuplicated(m_hors18$lambx * 100000L + m_hors18$lamby) &&
           all(m_hors18$lon <= PARTAGE_BORNES$lon[2]) && any(grepl("ecartee", msg_hors18)),
         "une maille du domaine SAFRAN hors des bornes de l'application est ecartee, avec un message")
verifier(refus18(carte_sim_mailles(coord_hors18(30L)), "hors du territoire"),
         "si les mailles hors bornes sont nombreuses (> 1 %), le fichier de coordonnees est refuse")

lire18sim <- carte_source_sim(2018:2022, m_sim18, c(1L, 57L, 110L), carte_sim_fournisseur_local(dos_sim18),
                              supprimer = FALSE, parametres_api = tous_api18)
d57 <- lire18sim(57L, m_sim18$lon[57], m_sim18$lat[57])
verifier(is.data.frame(d57) && nrow(d57) == sum(ifelse((2018:2022 %% 4 == 0 & 2018:2022 %% 100 != 0) | 2018:2022 %% 400 == 0, 366, 365)),
         "le tableau d'une maille couvre tous les jours des annees chargees")
verifier(all(c("date", "annee", "mois", "jour", "doy", "p_tot", "vpd", names(CONFIG$variables)) %in% names(d57)),
         "le tableau a les colonnes que safran_charger() donne a l'application")
verifier(is.null(lire18sim(2L, 2, 47)), "une maille hors du fragment n'est pas servie")

# Les valeurs sont EXACTEMENT celles du fichier (a l'echelle pres, sans perte).
brut18 <- utils::read.csv(gzfile(file.path(dos_sim18, "QUOT_SIM2_2020.csv.gz")), sep = ";")
b57 <- brut18[brut18$LAMBX == m_sim18$lambx[57] & brut18$LAMBY == m_sim18$lamby[57], ]
a57 <- d57[d57$annee == 2020, ]
verifier(identical(a57$t_moy, b57$T) && identical(a57$swi, b57$SWI) && identical(a57$ff, b57$FF) &&
           identical(a57$t_min, b57$TINF_H) && identical(a57$p_liq, b57$PRELIQ) && identical(a57$neig_h, as.numeric(b57$HTEURNEIGE)),
         "les valeurs relues sont identiques au fichier, decimale pour decimale")

# Parite avec l'application : une variable que GeoSAS n'expose pas reste vide, comme dans l'analyse.
sans_neige18 <- carte_source_sim(2020:2021, m_sim18, 57L, carte_sim_fournisseur_local(dos_sim18),
                                 supprimer = FALSE, parametres_api = setdiff(tous_api18, c("HTEURNEIGE_Q", "RESR_NEIGE_Q")))
dn18 <- sans_neige18(57L, m_sim18$lon[57], m_sim18$lat[57])
verifier(all(is.na(dn18$neig_h)) && all(is.na(dn18$neig_e)) && !anyNA(dn18$t_moy),
         "une variable non exposee par GeoSAS reste vide, comme dans l'analyse d'une maille")

# Les indicateurs de l'application s'appliquent tels quels a ce tableau.
ctx_sim18 <- carte_ctx(2018:2022)
r_sim18 <- carte_calculer_maille(d57, ctx_sim18, 2018:2022)
verifier(all(is.finite(r_sim18$tmp$valeurs)) && length(r_sim18) == length(CARTE_COUCHES),
         "le calcul de la carte s'applique au tableau issu des fichiers SIM")

# Gardes de lecture : refus plutot que valeur fausse.
f_mauvais18 <- function(modif) {
  d <- tempfile("simbad"); dir.create(d)
  l <- readLines(gzfile(file.path(dos_sim18, "QUOT_SIM2_2022.csv.gz")))
  l <- modif(l); f <- file.path(d, "QUOT_SIM2_2022.csv"); writeLines(l, f); system2("gzip", c("-f", shQuote(f))); d
}
verifier(refus18(carte_source_sim(2022L, m_sim18, 1L, carte_sim_fournisseur_local(
  f_mauvais18(function(l) { l[3] <- sub("^([0-9]+;[0-9]+;[0-9]+;[^;]*;[^;]*);[^;]*", "\\1;-9999", l[3]); l })),
  supprimer = FALSE, parametres_api = tous_api18), "hors de"),
  "un code d'absence (-9999) est refuse, pas lu comme une temperature")
verifier(refus18(carte_source_sim(2022L, m_sim18, 1L, carte_sim_fournisseur_local(
  f_mauvais18(function(l) { l[3] <- sub("^([0-9]+;[0-9]+;[0-9]+;[^;]*;[^;]*);[^;]*", "\\1;12.3456", l[3]); l })),
  supprimer = FALSE, parametres_api = tous_api18), "decimales"),
  "une valeur avec plus de decimales que l'echelle est refusee, pas arrondie")
d_an18 <- tempfile("simannee"); dir.create(d_an18)
file.copy(file.path(dos_sim18, "QUOT_SIM2_2022.csv.gz"), file.path(d_an18, "QUOT_SIM2_2021.csv.gz"))
verifier(refus18(carte_source_sim(2021L, m_sim18, 1L, carte_sim_fournisseur_local(d_an18), supprimer = FALSE,
                                  parametres_api = tous_api18), "contient l'annee"),
         "un fichier dont le contenu n'est pas l'annee annoncee est refuse")
d_inc18 <- f_mauvais18(function(l) l[-(2:20)])       # la premiere maille perd 19 jours
inc18 <- carte_source_sim(2022L, m_sim18, c(1L, 2L), carte_sim_fournisseur_local(d_inc18), supprimer = FALSE,
                          parametres_api = tous_api18)
verifier(is.null(inc18(1L, 0, 0)) && !is.null(inc18(2L, 0, 0)),
         "une maille aux jours manquants est vide pour l'annee, les autres ne changent pas")
verifier(refus18(carte_sim_fournisseur_local(dos_sim18)(1900), "introuvable"), "un fichier annuel absent est refuse")
verifier(refus18(carte_source_sim(2020L, m_sim18[, c("lon", "lat")], 1L, carte_sim_fournisseur_local(dos_sim18)),
                 "lambx"), "la source sim exige la grille native")
verifier(refus18(carte_sim_fournisseur_http()(1800), "invalide"), "une annee absurde n'est jamais demandee au reseau")
verifier(identical(sub("/[^/]*$", "", SIM_URL_COORD), SIM_URL_BASE) && grepl("^https://", SIM_URL_BASE),
         "les fichiers SIM ne se lisent qu'en https, sur l'hote de Meteo-France")

# Chaine complete par la ligne de commande : fragments -> assemblage -> verification.
cli18e <- function(env, ...) {
  system2(file.path(R.home("bin"), "Rscript"), c(shQuote(chemin_ci("outils/carte_cli.R")), ...),
          stdout = FALSE, stderr = FALSE, env = env)
}
env_sim18 <- c(paste0("CARTE_SIM_LOCAL=", dos_sim18), paste0("CARTE_SIM_PARAMETRES=", paste(tous_api18, collapse = ",")))
base_sim18 <- tempfile("clisim"); dir.create(base_sim18)
m_arg18 <- paste0("mailles=sim:", file.path(dos_sim18, "coordonnees.csv"))
y_arg18 <- c("annee_debut=2003", "annee_fin=2022")
c1 <- cli18e(env_sim18, "fragment", "source=sim", m_arg18, y_arg18, "fragment=1", "fragments=2", paste0("sortie=", base_sim18, "/parts"))
c2 <- cli18e(env_sim18, "fragment", "source=sim", m_arg18, y_arg18, "fragment=2", "fragments=2", paste0("sortie=", base_sim18, "/parts"))
verifier(c1 == 0L && c2 == 0L && length(list.files(file.path(base_sim18, "parts"), pattern = "^part_00[12]\\.rds$")) == 2L,
         "deux fragments se calculent depuis des fichiers SIM")
c3 <- cli18e(env_sim18, "assembler", shQuote("source_texte=SIM (essai)"), m_arg18, y_arg18,
             paste0("parts=", base_sim18, "/parts"), paste0("sortie=", base_sim18, "/public"))
verifier(c3 == 0L && cli18e(character(0), "verifier", paste0("dossier=", base_sim18, "/public")) == 0L,
         "le jeu assemble depuis des fichiers SIM est valide")
grille_sim18 <- paste(readLines(file.path(base_sim18, "public", "grille.json"), warn = FALSE), collapse = "")
carre18 <- carte_carre_l2e(m_sim18$lambx[1] * 100, m_sim18$lamby[1] * 100)
attendu18 <- sprintf("[%s,%s]", format(carre18[1, 1], nsmall = 4, trim = TRUE), format(carre18[1, 2], nsmall = 4, trim = TRUE))
verifier(grepl(attendu18, grille_sim18, fixed = TRUE),
         "la grille publiee dessine les mailles dans leur repere natif (carres exacts)")
meta_sim18 <- jsonlite::fromJSON(file.path(base_sim18, "public", "meta.json"), simplifyVector = FALSE)
note_petp18 <- Filter(function(k) identical(k$cle, "petp"), meta_sim18$couches)[[1]]$note
verifier(grepl("fenêtre d'avril à octobre", note_petp18, fixed = TRUE) && grepl("ETP", note_petp18, fixed = TRUE),
         "la note de la couche P - ETP dit la fenetre (« d'avril ») et l'ecart d'ETP de la source sim")
# La valeur publiee pour une maille est celle que calcule directement l'application sur son tableau.
pt18 <- readRDS(file.path(base_sim18, "parts", "part_001.rds"))
src_ref18 <- carte_source_sim(2003:2022, m_sim18, pt18$indices[1], carte_sim_fournisseur_local(dos_sim18),
                              supprimer = FALSE, parametres_api = tous_api18)
dir18 <- carte_calculer_maille(src_ref18(pt18$indices[1], 0, 0), carte_ctx(2003:2022), 2003:2022)
verifier(identical(unname(dir18$tmp$valeurs), unname(pt18$valeurs$tmp[, 1])),
         "valeur de la carte == valeur du calcul direct sur le tableau SIM de la maille (a l'identique)")
verifier(any(grepl("options: \\[sim, position, demo\\]", wf18$construire)) && any(grepl("extra-packages: any::data.table", wf18$construire, fixed = TRUE)) &&
           any(grepl("SIM quotidienne", wf18$construire, fixed = TRUE)),
         "le workflow propose la source sim (par defaut), installe data.table et cite l'origine des donnees")
verifier(any(grepl("note_etp", lire18("outils/carte_cli.R"), fixed = TRUE)) &&
           any(grepl("s'écarte de quelques %", lire18("outils/carte_cli.R"), fixed = TRUE)),
         "la limite connue de la source sim (ETP differente de celle de l'analyse) est dite a l'utilisateur")
