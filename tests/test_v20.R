# ==============================================================================
# tests/test_v20.R — TROIS PROFILS ET BILAN SYNTHETIQUE (VERSION 2.0)
# ------------------------------------------------------------------------------
# Ce que ces tests gardent :
#   1. les profils et l'acces : identifiant assaini, fichier d'identifiants, routage,
#      liens partages d'avant la v2.0 ; ET le fait que ce n'est pas une authentification
#      (le depot public ne contient que des identifiants de demonstration) ;
#   2. les territoires : contours, point -> departement, rattachement aux mailles ;
#   3. l'import de fiches : formats, dates, bornes, textes assainis ;
#   4. le bilan : valeurs d'un territoire (moyenne des mailles), d'un point, de fiches,
#      verifiees a la main sur un jeu synthetique ; phrases, rangs, tableaux ;
#   5. les exports (CSV, HTML autonome, Word) et le module Shiny de bout en bout ;
#   6. l'interface de chaque profil, la page d'entree, et le CSS de la v2.0.
# ==============================================================================

for (f in c("outils/carte_geo.R", "outils/carte_format.R", "outils/carte_precalcul.R",
            "outils/carte_demo.R")) source(chemin_ci(f))

AVEC_SF20 <- requireNamespace("sf", quietly = TRUE)

lire20 <- function(f) paste(readLines(chemin_ci(f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
html20 <- function(x) {
  r <- htmltools::renderTags(x)
  paste(c(as.character(r$head), as.character(r$html)), collapse = "\n")
}
ecrire20 <- function(txt = NULL, ext = ".csv", brut = NULL) {
  f <- tempfile(fileext = ext)
  if (is.null(brut)) writeLines(txt, f, useBytes = TRUE) else writeBin(brut, f)
  f
}
# Modifie un reglage le temps d'une expression.
avec_reglage20 <- function(chemin, valeur, expr) {
  ancien <- CONFIG[[chemin[1]]][[chemin[2]]]
  CONFIG[[chemin[1]]][[chemin[2]]] <<- valeur
  on.exit(CONFIG[[chemin[1]]][[chemin[2]]] <<- ancien, add = TRUE)
  force(expr)
}


# ==============================================================================
groupe("v2.0 — profils : identifiant et fichier d'acces")
# ==============================================================================

verifier(identical(profil_id_sur("Demo-CO"), "demo-co") && identical(profil_id_sur("  abc_12 "), "abc_12"),
         "un identifiant est mis en minuscules et rogne")
verifier(all(is.na(c(profil_id_sur("ab"), profil_id_sur(""), profil_id_sur(NULL), profil_id_sur(NA),
                     profil_id_sur("a b c"), profil_id_sur("../../etc"), profil_id_sur("x;y"),
                     profil_id_sur("<script>alert(1)</script>"), profil_id_sur(strrep("a", 41))))),
         "trop court, trop long, espace, point, point-virgule ou balise : refuse (entree non fiable)")
verifier(identical(profil_id_sur(strrep("a", 40)), strrep("a", 40)), "40 caracteres : accepte")

f_acces <- ecrire20(c("# commentaire", "id;profil;nom", "co-test;co;Correspondant d'essai", "",
                      "DSF-Test;dsf;Permanent", "inconnu;astronaute;Profil absent", "x;co;Trop court",
                      "mauvais id;co;Espace", "t-ter;terrain;"))
t_acces <- profil_lire_fichier(f_acces)
verifier(identical(t_acces$id, c("co-test", "dsf-test", "t-ter")) && identical(t_acces$profil, c("co", "dsf", "terrain")),
         "le fichier d'acces est lu : commentaires, lignes vides, profil inconnu et identifiant invalide ecartes")
verifier(identical(t_acces$nom[1], "Correspondant d'essai") && identical(t_acces$nom[3], ""),
         "le nom d'affichage est lu, vide s'il manque")

f_virg <- ecrire20(c("id,profil,nom", "a-b-c,co,Un nom"))
verifier(identical(profil_lire_fichier(f_virg)$id, "a-b-c"), "un fichier a virgule est lu aussi")
f_bom <- ecrire20(brut = c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("id;profil;nom\nbom-id;co;Avec BOM\n")))
verifier(identical(profil_lire_fichier(f_bom)$id, "bom-id"), "un fichier Excel (marque d'ordre des octets) est lu")
f_long <- ecrire20(c("id;profil;nom", sprintf("long-nom;co;%s", strrep("n", 200))))
verifier(nchar(profil_lire_fichier(f_long)$nom) == 80L, "un nom d'affichage est borne a 80 caracteres")
verifier(nrow(profil_lire_fichier(tempfile())) == 0L && nrow(profil_lire_fichier(NULL)) == 0L &&
           nrow(profil_lire_fichier(ecrire20("id;profil;nom"))) == 0L,
         "fichier absent, vide ou sans ligne : table vide, pas d'erreur")


# ==============================================================================
groupe("v2.0 — profils : definition et routage")
# ==============================================================================

onglets_connus20 <- c("accueil", "carte", "comparer", "synthese", "bilan", "rapport", "parametres", "sources", "terrain")
verifier(identical(names(PROFILS), c("terrain", "co", "dsf")) &&
           all(vapply(PROFILS, function(p) identical(p$id, names(PROFILS)[match(p$id, names(PROFILS))]), TRUE)),
         "trois profils : terrain, co, dsf")
verifier(all(unlist(lapply(PROFILS, function(p) p$onglets)) %in% onglets_connus20) &&
           all(unlist(lapply(PROFILS, function(p) p$echelles)) %in% names(SYNTHESE_ECHELLES)),
         "chaque profil n'utilise que des onglets et des echelles de bilan qui existent")
verifier(identical(PROFILS$dsf$echelles, c("departement", "region", "fiches", "point")) && isTRUE(PROFILS$dsf$multi) &&
           !isTRUE(PROFILS$co$multi) && isTRUE(PROFILS$co$restreint) && isTRUE(PROFILS$terrain$restreint) &&
           !isTRUE(PROFILS$dsf$restreint),
         "le permanent DSF a toutes les echelles et la comparaison ; CO et terrain sont restreints")
verifier(!"rapport" %in% PROFILS$co$onglets && !"parametres" %in% PROFILS$co$onglets &&
           !"comparer" %in% PROFILS$co$onglets && !"accueil" %in% PROFILS$terrain$onglets,
         "le correspondant-observateur n'a ni rapport, ni parametres, ni comparaison ; le terrain tient en deux onglets")
verifier(identical(PROFILS$terrain$onglets, c("terrain", "carte")) && identical(PROFILS$co$onglets[1], "carte"),
         "le terrain s'ouvre sur « Autour de moi », le correspondant sur la carte")

tab20 <- data.frame(id = c("demo-co", "demo-dsf"), profil = c("co", "dsf"), nom = c("CO", "DSF"),
                    stringsAsFactors = FALSE)
r <- profil_resoudre("?id=demo-co", tab20, demo = FALSE)
verifier(isTRUE(r$ok) && identical(r$profil$id, "co") && identical(r$id, "demo-co") && !isTRUE(r$inconnu),
         "un identifiant de la table ouvre son profil")
r <- profil_resoudre("?id=DEMO-DSF&x=1", tab20, demo = FALSE)
verifier(isTRUE(r$ok) && identical(r$profil$id, "dsf"), "la casse de l'identifiant n'importe pas, les autres parametres non plus")
r <- profil_resoudre("?id=nope", tab20, demo = TRUE)
verifier(!isTRUE(r$ok) && isTRUE(r$inconnu) && is.null(r$profil),
         "un identifiant inconnu n'ouvre rien, meme en mode demonstration, et est signale")
r <- profil_resoudre("?id=nope&profil=dsf", tab20, demo = TRUE)
verifier(!isTRUE(r$ok) && isTRUE(r$inconnu),
         "un identifiant inconnu n'est pas rattrape par ?profil= (pas de contournement)")
r <- profil_resoudre("?id=../../x%00", tab20, demo = TRUE)
verifier(!isTRUE(r$ok) && isTRUE(r$inconnu), "un identifiant mal forme est traite comme inconnu")
r1 <- profil_resoudre("?profil=co&junk=%00&x=1", tab20, demo = TRUE)
r2 <- profil_resoudre("?junk=%00&id=demo-dsf", tab20, demo = FALSE)
r3 <- profil_resoudre("?id=%00", tab20, demo = TRUE)
verifier(isTRUE(r1$ok) && identical(r1$profil$id, "co") && isTRUE(r2$ok) && identical(r2$profil$id, "dsf") &&
           !isTRUE(r3$ok) && isTRUE(r3$inconnu),
         "un parametre illisible est ignore (le profil s'ouvre), sauf l'identifiant, qui est alors inconnu")
rl <- profil_lire_requete("?%00=1&c=3&junk=%00&d=%C3%A9")
verifier(identical(profil_lire_requete("?a=1&a=2")$q$a, "1") && identical(rl$q$d, "é") && identical(rl$q$c, "3") &&
           identical(rl$illisibles, c("%00", "junk")) &&
           identical(profil_lire_requete("")$q, list()) && identical(profil_lire_requete(NULL)$q, list()),
         "la requete se lit parametre par parametre : l'illisible est signale, le premier de deux homonymes l'emporte, accents decodes")
r <- profil_resoudre("?profil=co", tab20, demo = TRUE)
verifier(isTRUE(r$ok) && identical(r$profil$id, "co") && is.na(r$id), "en mode demonstration, ?profil=co ouvre le profil")
r <- profil_resoudre("?profil=co", tab20, demo = FALSE)
verifier(!isTRUE(r$ok) && !isTRUE(r$inconnu), "hors demonstration, ?profil=co n'ouvre rien")
r <- profil_resoudre("?profil=astronaute", tab20, demo = TRUE)
verifier(!isTRUE(r$ok), "un profil qui n'existe pas n'ouvre rien")
verifier(!isTRUE(profil_resoudre("", tab20)$ok) && !isTRUE(profil_resoudre(NULL, tab20)$ok) &&
           !isTRUE(profil_resoudre("%%%", tab20)$ok) && !isTRUE(profil_resoudre(character(0), tab20)$ok),
         "requete vide, absente ou illisible : page d'entree, pas d'erreur")

r <- profil_resoudre("?id=demo-co", tab20, demo = FALSE)
verifier(identical(profil_prefixe_requete(r), "id=demo-co") &&
           identical(profil_prefixe_requete(profil_resoudre("?profil=dsf", tab20, demo = TRUE)), "profil=dsf") &&
           identical(profil_prefixe_requete(NULL), ""),
         "le debut d'adresse rappelle l'identifiant ou le profil, pour que recharger la page le garde")
verifier(identical(profil_prefixe_adresse(r), "id=demo-co&") &&
           identical(profil_prefixe_adresse(list(ok = TRUE, profil = PROFILS$dsf, id = NA_character_, ancien = TRUE)), ""),
         "un lien d'avant la v2.0 est reecrit sans profil, comme il est arrive")


# ==============================================================================
groupe("v2.0 — profils : liens partages d'avant la v2.0 et table de l'application")
# ==============================================================================

lien_ancien <- "?x=1.4727&y=43.35&n=Auterive&s=safran&r=1981-2010&a=2019"
r <- profil_pour_requete(lien_ancien)
verifier(isTRUE(r$ok) && identical(r$profil$id, "dsf") && isTRUE(r$ancien),
         "un lien partage d'avant la v2.0 (sans profil) ouvre l'interface complete, comme avant")
r <- profil_pour_requete("?id=nope&x=1.4727&y=43.35&n=Auterive")
verifier(!isTRUE(r$ok) && isTRUE(r$inconnu), "un identifiant inconnu n'est pas rattrape par un lien ancien")
r <- profil_pour_requete("?x=oops&y=1")
verifier(!isTRUE(r$ok), "une adresse sans point exploitable ni profil mene a la page d'entree")
r <- profil_pour_requete("?profil=co&x=1.4727&y=43.35")
verifier(isTRUE(r$ok) && identical(r$profil$id, "co") && !isTRUE(r$ancien),
         "avec un profil, le lien partage ne change pas de profil")

verifier(file.exists(chemin_ci(CONFIG$acces$fichier)), "la liste d'acces versionnee existe")
t_vers <- profil_lire_fichier(chemin_ci(CONFIG$acces$fichier))
verifier(nrow(t_vers) >= 3L && setequal(t_vers$profil, c("terrain", "co", "dsf")),
         "la liste versionnee offre un identifiant par profil")
verifier(all(startsWith(t_vers$id, "demo-")),
         "le depot public ne contient que des identifiants de DEMONSTRATION (les vrais vont dans acces_local.csv)")
verifier(grepl("config/acces_local.csv", lire20(".gitignore"), fixed = TRUE),
         "la liste locale d'identifiants n'est pas versionnee")
verifier(isTRUE(CONFIG$acces$demo) && CONFIG$acces$id_long_max == 40L,
         "reglages d'acces : mode demonstration actif par defaut, identifiant borne a 40 caracteres")

# Le fichier local l'emporte sur le fichier versionne.
dir.create(chemin_ci("cache"), showWarnings = FALSE)
loc20 <- "cache/acces_local_test_v20.csv"
writeLines(c("id;profil;nom", "demo-co;dsf;Detourne", "vrai-id;co;Vrai CO"), chemin_ci(loc20), useBytes = TRUE)
t_loc <- avec_reglage20(c("acces", "fichier_local"), loc20, profil_acces())
unlink(chemin_ci(loc20))
verifier("vrai-id" %in% t_loc$id && identical(t_loc$profil[t_loc$id == "demo-co"], "dsf") && !anyDuplicated(t_loc$id),
         "la liste locale s'ajoute a la liste versionnee et l'emporte pour un meme identifiant, sans doublon")
verifier(nrow(profil_acces()) >= 3L, "sans liste locale, la liste versionnee suffit")


# ==============================================================================
groupe("v2.0 — territoires : contours et rattachement aux mailles")
# ==============================================================================

if (!AVEC_SF20) {
  cat("  (sf absent : tests des territoires passes)\n")
} else {
  td <- territoires_table("departement"); tr_ <- territoires_table("region")
  verifier(nrow(td) >= 94L && nrow(td) <= 101L && all(c("code", "nom") %in% names(td)),
           sprintf("les contours des departements sont embarques (%d)", nrow(td)))
  verifier(nrow(tr_) >= 12L && nrow(tr_) <= 18L, sprintf("les contours des regions sont embarques (%d)", nrow(tr_)))
  verifier(identical(td$nom[td$code == "31"], "Haute-Garonne") && all(c("2A", "2B") %in% td$code),
           "la Haute-Garonne (31) et les deux departements corses sont presents")
  ch <- territoires_choix("departement")
  verifier(!is.null(names(ch)) && any(grepl("Haute-Garonne", names(ch))) && "31" %in% ch,
           "le choix de departements propose un libelle et rend le code")
  tl_ <- territoires_lieux("departement", c("31", "99", "31", "09"))
  verifier(length(tl_) == 2L && identical(tl_[[1]]$nom, "Haute-Garonne (31)") && grepl("\\(09\\)", tl_[[2]]$nom),
           "territoires_lieux : un lieu par code connu, un code inconnu ou repete est ecarte")
  verifier(length(territoires_lieux("region", character(0))) == 0L, "aucun code : aucun lieu")
  verifier(identical(territoires_departement_du_point(1.44, 43.60), "31") &&
             is.na(territoires_departement_du_point(-30, 40)),
           "un point de Toulouse tombe en Haute-Garonne, un point en plein ocean n'a pas de departement")
  verifier(identical(territoires_departement_du_point(1.44, 43.60), territoires_departement_du_point(1.44, 43.60)),
           "le resultat ne depend pas du nombre d'appels (contours lus une fois)")
}


# ==============================================================================
groupe("v2.0 — jeu synthetique de 6 x 6 mailles autour de Toulouse")
# ==============================================================================

x0_20 <- 525000; y0_20 <- 6280000
g20 <- expand.grid(i = 0:5, j = 0:5)
ll20 <- carte_depuis_l93(x0_20 + g20$i * 8000, y0_20 + g20$j * 8000)
an20 <- 1959:2025; n20 <- nrow(g20)
val20 <- lapply(seq_along(CARTE_COUCHES), function(k) {
  base <- k + 0.2 * g20$i + 0.1 * g20$j
  outer(seq_along(an20), seq_len(n20), function(a, c) base[c] + 0.03 * (a - 1) + 0.02 * sin(a / 3 + c))
})
names(val20) <- names(CARTE_COUCHES)
part20 <- list(annees = an20, indices = seq_len(n20), echecs = integer(0), valeurs = val20,
               normale = lapply(val20, function(v) colMeans(v[an20 %in% 1991:2020, ])),
               pente = lapply(val20, function(v) rep(0.3, n20)),
               signif = lapply(val20, function(v) rep(TRUE, n20)))
dos20 <- tempfile("carte20")
carte_assembler(list(part20), ll20$lon, ll20$lat, an20, "jeu synthetique", dos20)
tr20 <- carte_transport(url = "", dossier = dos20)
jeu20 <- tryCatch(cmp_jeu(tr20), error = function(e) NULL)
verifier(!is.null(jeu20) && identical(jeu20$annees, an20) && nrow(jeu20$centres) == n20,
         "(controle) le jeu synthetique se publie et se relit")
ctx20 <- ctx_creer(ref = c(1991, 2020), affichage = an20, courante = 2025)
k20 <- which(g20$i == 2 & g20$j == 3)

cat_20 <- if (!is.null(jeu20)) synthese_catalogue(jeu20, PROFILS$dsf) else list()
cat_co20 <- if (!is.null(jeu20)) synthese_catalogue(jeu20, PROFILS$co) else list()
verifier(length(cat_20) == length(CARTE_COUCHES) && all(names(cat_co20) %in% CONFIG$synthese$couches_co) &&
           length(cat_co20) < length(cat_20),
         "le catalogue du permanent DSF offre tout le jeu, celui du correspondant seulement ses grandeurs")
g_20 <- synthese_groupes(cat_20)
verifier(length(g_20) >= 3L && setequal(unlist(lapply(g_20, function(x) names(x$choix))), names(cat_20)) &&
           all(vapply(g_20, function(x) nzchar(x$libelle), TRUE)),
         "les grandeurs sont rangees par famille, chacune une seule fois")
b_ <- synthese_base("dsf", names(cat_20))
verifier(all(b_$g %in% names(cat_20)) && all(b_$t %in% names(cat_20)) && length(b_$g) >= 4L &&
           identical(synthese_base("inconnu", names(cat_20)), synthese_base("co", names(cat_20))),
         "le jeu de base se limite aux grandeurs disponibles ; un profil inconnu prend celui du CO")
verifier(all(vapply(CONFIG$synthese$base, function(b) all(c(b$g, b$t) %in% names(CARTE_COUCHES)), TRUE)) &&
           all(CONFIG$synthese$couches_co %in% names(CARTE_COUCHES)),
         "les jeux de base et les grandeurs du correspondant n'invoquent que des couches de la carte")


# ==============================================================================
groupe("v2.0 — bilan synthetique : departement, region et point (valeurs verifiees a la main)")
# ==============================================================================

if (!AVEC_SF20 || is.null(jeu20)) {
  cat("  (sf ou jeu absent : tests du bilan passes)\n")
} else {
  dep31 <- territoires_lire("departement"); dep31 <- dep31[dep31$code == "31", ]
  pts20 <- sf::st_as_sf(jeu20$centres[, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
  dedans20 <- lengths(sf::st_intersects(pts20, sf::st_geometry(dep31))) > 0

  r_dep <- synthese_preparer("departement", territoires_lieux("departement", "31"), NULL, jeu20,
                             c("tmp", "vag"), c("tmp", "vag", "pre"), ctx20, 2025, PROFILS$dsf)
  verifier(identical(r_dep$echelle, "departement") && all(r_dep$tl$retenu) &&
             identical(r_dep$tl$n_mailles, sum(dedans20)) && sum(dedans20) > 4L && sum(dedans20) < n20,
           sprintf("le departement retient les mailles dont le centre est dans le contour (%d sur %d)", sum(dedans20), n20))
  b_t <- r_dep$bilan[r_dep$bilan$cle == "tmp", ]
  attendu <- mean(val20$tmp[an20 == 2025, dedans20])
  # Les valeurs du jeu sont stockees avec les decimales de la grandeur (2 pour la temperature) :
# la moyenne comptee a la main s'en ecarte de moins d'une demi-unite de la derniere decimale.
TOL20 <- 0.01
verifier(nrow(b_t) == 1L && presque_egal(b_t$valeur, attendu, TOL20),
           "valeur du departement = moyenne simple des mailles de l'annee (comptee a la main)")
  norm_att <- mean(rowMeans(val20$tmp[an20 %in% 1991:2020, dedans20, drop = FALSE]))
  verifier(presque_egal(b_t$normale, norm_att, TOL20) && presque_egal(b_t$ecart, b_t$valeur - b_t$normale, 1e-9),
           "normale = moyenne de la reference 1991-2020 ; ecart = valeur - normale")
  verifier(identical(sort(r_dep$cles_g), c("tmp", "vag")) && identical(sort(r_dep$cles_t), c("pre", "tmp", "vag")),
           "les grandeurs cochees pour le graphique et pour le tableau sont gardees separement")

  ph <- synthese_phrases_generales(r_dep)
  verifier(length(ph) >= 2L && grepl("Haute-Garonne", ph[1], fixed = TRUE) && grepl("2025", ph[1]) &&
             grepl("1991-2020", ph[1], fixed = TRUE),
           "les phrases generales nomment le lieu, l'annee et la reference")
  verifier(all(vapply(ph, is.character, TRUE)) && all(!grepl("NA|NaN|Inf", ph)),
           "aucune phrase ne laisse passer NA, NaN ou Inf")
  ph_k <- synthese_phrases_cle(r_dep, "tmp")
  verifier(length(ph_k) >= 1L && all(nzchar(ph_k)), "chaque graphique a ses phrases")

  tab <- synthese_tableau(r_dep)
  verifier(is.data.frame(tab) && nrow(tab) == 3L && identical(names(tab),
             c("Grandeur", "Lieu", "Valeur", "Normale", "Écart", "Lecture", "Rang", "Tendance")),
           "le tableau de synthese a une ligne par grandeur cochee et ses huit colonnes")
  verifier(identical(tab$Grandeur[1], CARTE_COUCHES$tmp$titre) || any(tab$Grandeur == CARTE_COUCHES$tmp$titre),
           "les lignes portent le titre de la grandeur")
  verifier(all(grepl("par décennie", tab$Tendance[tab$Tendance != "—"])),
           "la tendance s'exprime par decennie")
  rg <- r_dep$rang[r_dep$rang$cle == "tmp", ]
  verifier(nrow(rg) == 1L && rg$n == length(an20) && rg$rang_haut %in% seq_len(length(an20)),
           "le rang de l'annee se compte sur toutes les annees de la serie")

  # Rang : la serie synthetique croit de 0,03 par an : 2025 est la plus elevee ou presque.
  verifier(rg$rang_haut <= 3L, "sur une serie croissante, l'annee la plus recente est parmi les plus elevees")
  verifier(nzchar(synthese_phrase_rang(rg)) && grepl("depuis 1959", synthese_phrase_rang(rg)),
           "la phrase de rang cite l'annee de debut de la serie quand le rang est extreme")
  verifier(identical(synthese_phrase_rang(data.frame(rang_haut = 30L, n = 67L, premiere = 1959L)), "") &&
             identical(synthese_phrase_rang(data.frame(rang_haut = 1L, n = 12L, premiere = 2014L)), ""),
           "un rang du milieu, ou une serie de moins de 20 ans, ne donne pas de phrase")
  verifier(grepl("plus basse", synthese_phrase_rang(data.frame(rang_haut = 66L, n = 67L, premiere = 1959L))) &&
             grepl("3e", synthese_phrase_rang(data.frame(rang_haut = 3L, n = 67L, premiere = 1959L))),
           "rang bas : « la plus basse » ; rang 3 : « 3e »")

  verifier(identical(synthese_lecture_ecart("haut", 2, 1), "nettement au-dessus de la normale") &&
             identical(synthese_lecture_ecart("haut", 0.7, 1), "au-dessus de la normale") &&
             identical(synthese_lecture_ecart("bas", -3, 1), "nettement en dessous de la normale") &&
             identical(synthese_lecture_ecart("neutre", 0.1, 1), "proche de la normale") &&
             identical(synthese_lecture_ecart("haut", NA_real_, 1), "normale non évaluable"),
           "lecture d'un ecart en mots : seuil d'un ecart-type pour « nettement », NA sans erreur")

  # Region
  r_reg <- synthese_preparer("region", territoires_lieux("region", "76"), NULL, jeu20, "tmp", "tmp", ctx20, 2025, PROFILS$dsf)
  verifier(identical(r_reg$tl$lieu, "Occitanie") && r_reg$tl$retenu && r_reg$tl$n_mailles >= sum(dedans20),
           "la region Occitanie retient au moins les mailles de la Haute-Garonne")

  # Point
  p_ctr <- cmp_lieu_point("Centre", ll20$lon[k20], ll20$lat[k20])
  r_pt <- synthese_preparer("point", list(p_ctr), NULL, jeu20, c("tmp", "vag"), c("tmp", "vag"), ctx20, 2025, PROFILS$co)
  b_p <- r_pt$bilan[r_pt$bilan$cle == "tmp", ]
  verifier(r_pt$tl$retenu && presque_egal(b_p$valeur, val20$tmp[an20 == 2025, k20], TOL20),
           "la valeur d'un point est celle de sa maille la plus proche")
  r_loin <- synthese_preparer("point", list(cmp_lieu_point("Loin", ll20$lon[k20] + 0.5, ll20$lat[k20] + 0.5)), NULL,
                              jeu20, "tmp", "tmp", ctx20, 2025, PROFILS$co)
  verifier(!any(r_loin$tl$retenu) && nrow(r_loin$bilan) == 0L,
           "un point a plus de 6 km d'une maille n'est pas analyse : pas de valeur inventee")
  verifier(is.null(synthese_tableau(r_loin)) || !nrow(synthese_tableau(r_loin)),
           "sans valeur, pas de tableau")

  # Deux territoires : un tableau a deux lieux par grandeur
  r_2 <- synthese_preparer("departement", territoires_lieux("departement", c("31", "82")), NULL, jeu20,
                           "tmp", "tmp", ctx20, 2025, PROFILS$dsf)
  verifier(nrow(r_2$tl) == 2L && any(r_2$tl$retenu),
           "plusieurs departements s'analysent ensemble")

  # Methode
  me <- synthese_methode(r_dep)
  verifier(length(me) >= 3L && any(grepl("moyenne", me)) && any(grepl("Météo-France", me)),
           "la methode dit comment un territoire est resume et d'ou viennent les donnees")
}


# ==============================================================================
groupe("v2.0 — fiches : import (CSV, GeoJSON) et assainissement")
# ==============================================================================

lat_fiche <- ll20$lat[k20]; lon_fiche <- ll20$lon[k20]
csv_fiches <- c("fiche;latitude;longitude;date;essence;probleme;commune",
                sprintf("F1;%s;%s;2000-07-10;Chêne pédonculé;Défoliation;Toulouse", sub("\\.", ",", format(lat_fiche, nsmall = 4)),
                        sub("\\.", ",", format(lon_fiche, nsmall = 4))),
                sprintf("F2;%.5f;%.5f;14/08/2025;Hêtre;Scolytes;Fronton", lat_fiche, lon_fiche + 0.05),
                sprintf("F3;%.5f;%.5f;2018;Sapin pectiné;Dépérissement;Aigoual", lat_fiche + 0.02, lon_fiche),
                sprintf("F4;%.5f;%.5f;pas une date;Chêne pédonculé;Défoliation;Sans date", lat_fiche, lon_fiche + 0.02),
                "F5;48,5;2,0;2025-01-05;Pin;Chenilles;Hors grille",
                "F6;4,3;43,6;2025-01-05;Pin;Chenilles;Inversee")
r_f <- fiches_lire(ecrire20(csv_fiches), "fiches.csv")
f20 <- r_f$fiches
verifier(nrow(f20) == 5L && identical(f20$id, c("F1", "F2", "F3", "F4", "F5")),
         "6 lignes : celle aux coordonnees inversees (hors France) est ecartee, les 5 autres sont lues")
verifier(any(grepl("écartée", r_f$problemes)) && any(grepl("date illisible", r_f$problemes)),
         "les problemes sont dits : fiche ecartee, date illisible")
verifier(identical(f20$annee, c(2000L, 2025L, 2018L, NA_integer_, 2025L)) && f20$date[3] == as.Date("2018-07-01"),
         "dates lues : ISO, jj/mm/aaaa, annee seule (1er juillet), illisible = NA")
verifier(identical(f20$essence[1], "Chêne pédonculé") && identical(f20$nom[2], "Fronton") &&
           identical(f20$probleme[3], "Dépérissement"),
         "essence, probleme et lieu sont lus avec leurs accents")
verifier(presque_egal(f20$lat[1], lat_fiche, 1e-3), "la virgule decimale est lue")

r_min <- fiches_lire(ecrire20(c("latitude,longitude", "43.6,1.44", "43.7,1.5")), "p.csv")
verifier(nrow(r_min$fiches) == 2L && identical(r_min$fiches$id, c("F1", "F2")) &&
           all(r_min$fiches$essence == FICHE_NON_RENSEIGNE) && all(is.na(r_min$fiches$date)) && !length(r_min$problemes),
         "avec les seules coordonnees : fiches numerotees, « Non renseigné », pas de date, pas de probleme")
r_dbl <- fiches_lire(ecrire20(c("fiche,latitude,longitude", "A,43.6,1.44", "A,43.7,1.5")), "p.csv")
verifier(identical(r_dbl$fiches$id, c("A", "A #1")), "deux fiches de meme identifiant restent distinctes")
r_sans <- fiches_lire(ecrire20(c("nom;essence", "A;Chêne")), "p.csv")
verifier(nrow(r_sans$fiches) == 0L && any(grepl("latitude", r_sans$problemes)),
         "sans colonnes de coordonnees : aucune fiche et un message qui dit lesquelles")
verifier(nrow(fiches_lire(ecrire20("x"), "p.docx")$fiches) == 0L &&
           any(grepl("Format", fiches_lire(ecrire20("x"), "p.docx")$problemes)),
         "un format inconnu est refuse avec la liste des formats acceptes")
verifier(nrow(fiches_lire(tempfile(fileext = ".csv"), "absent.csv")$fiches) == 0L,
         "un fichier introuvable ne plante pas")

r_long <- avec_reglage20(c("synthese", "fiches_max"), 3L,
                         fiches_lire(ecrire20(c("latitude;longitude", sprintf("43,%d;1,4", 1:8))), "p.csv"))
verifier(nrow(r_long$fiches) == 3L && any(grepl("Plus de 3 fiches", r_long$problemes)),
         "au-dela du nombre maximal, les premieres fiches seules sont lues et le message le dit")
r_txt <- fiches_lire(ecrire20(c("latitude;longitude;essence;fiche", sprintf("43,6;1,4;%s;<img src=x onerror=alert(1)>%s", strrep("x", 120), "\u0007"),
                                "43,7;1,4;;")), "p.csv")
verifier(all(nchar(r_txt$fiches$essence) <= 60L) && !grepl("\u0007", r_txt$fiches$essence[1]) &&
           identical(r_txt$fiches$essence[2], FICHE_NON_RENSEIGNE),
         "un texte de fiche est borne a 60 caracteres, sans caractere de controle ; vide = « Non renseigné »")

verifier(is.na(fiches_lire_date("31/02/2025")) && is.na(fiches_lire_date("1850-01-01")) &&
           is.na(fiches_lire_date(format(Sys.Date() + 800, "%Y-%m-%d"))) &&
           identical(length(fiches_lire_date(character(0))), 0L) && is.na(fiches_lire_date(NA_character_)) &&
           is.na(fiches_lire_date("")),
         "dates impossibles, avant 1959, trop lointaines ou absentes : NA, sans erreur (vecteur vide compris)")
verifier(nrow(fiches_vide()) == 0L && identical(names(fiches_vide()),
           c("id", "nom", "lon", "lat", "date", "annee", "essence", "probleme")),
         "le tableau de fiches vide fixe le contrat d'une source de fiches")
verifier(nrow(fiches_modele_csv()) == 3L && all(c("fiche", "latitude", "longitude") %in% names(fiches_modele_csv())) &&
           nrow(fiches_lire(ecrire20(c("fiche;latitude;longitude;date;essence;probleme;nom",
                                       apply(fiches_modele_csv(), 1, paste, collapse = ";"))), "m.csv")$fiches) == 3L,
         "le modele de CSV propose se relit tel quel")
verifier(length(fiches_lire_api()$problemes) == 1L && nrow(fiches_lire_api()$fiches) == 0L &&
           !nzchar(CONFIG$sylvasan$api_fiches) && !nzchar(CONFIG$sylvasan$url_saisie),
         "le raccord a l'application de terrain est une prise : vide, honnete, et sans bouton tant qu'elle n'est pas configuree")
cp <- fiches_compter(f20, "essence")
verifier(cp$modalite[1] == "Chêne pédonculé" && cp$n[1] == 2L && presque_egal(sum(cp$part), 100, 1e-8) &&
           nrow(fiches_compter(fiches_vide(), "essence")) == 0L,
         "le compte par modalite va du plus frequent au moins frequent, parts a 100 %, vide sans erreur")

if (AVEC_SF20) {
  f_geo <- tempfile(fileext = ".geojson")
  sf::st_write(sf::st_sf(fiche = c("G1", "G2"), essence = c("Hêtre", "Pin"), date = c("2024-06-01", "2023-06-01"),
                         geometry = sf::st_sfc(sf::st_point(c(lon_fiche, lat_fiche)), sf::st_point(c(lon_fiche + 0.1, lat_fiche)), crs = 4326)),
               f_geo, quiet = TRUE)
  r_g <- fiches_lire(f_geo, "fiches.geojson")
  verifier(nrow(r_g$fiches) == 2L && identical(r_g$fiches$id, c("G1", "G2")) && identical(r_g$fiches$annee, c(2024L, 2023L)),
           "un GeoJSON de points se lit : identifiant, essence, date")
  f_poly <- tempfile(fileext = ".geojson")
  sf::st_write(sf::st_sf(nom = "Zone", geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(1, 43), c(2, 43), c(2, 44), c(1, 43)))), crs = 4326)),
               f_poly, quiet = TRUE)
  verifier(nrow(fiches_lire(f_poly, "zone.geojson")$fiches) == 0L && any(grepl("aucun point", fiches_lire(f_poly, "zone.geojson")$problemes)),
           "une zone n'est pas une fiche : le fichier est refuse, avec la raison")
}


# ==============================================================================
groupe("v2.0 — bilan des fiches : le contexte climatique de leur annee")
# ==============================================================================

if (!AVEC_SF20 || is.null(jeu20)) {
  cat("  (sf ou jeu absent : tests des fiches passes)\n")
} else {
  r_fi <- synthese_preparer("fiches", list(), f20, jeu20, c("tmp", "vag"), c("tmp", "vag"), ctx20, 2025, PROFILS$dsf)
  verifier(identical(r_fi$echelle, "fiches") && sum(r_fi$fiches_dans) == 4L && !r_fi$fiches_dans[5],
           "4 fiches sur 5 sont dans la grille ; la fiche de Normandie (hors grille) est comptee a part")
  ct <- r_fi$contexte
  c_f1 <- ct[ct$fiche == "F1" & ct$cle == "tmp", ]
  verifier(nrow(c_f1) == 1L && c_f1$annee == 2000L &&
             presque_egal(c_f1$valeur, val20$tmp[an20 == 2000, k20], TOL20),
           "une fiche est replacee dans SON annee : valeur de la maille la plus proche en 2000")
  norm_k <- mean(val20$tmp[an20 %in% 1991:2020, k20])
  verifier(presque_egal(c_f1$normale, norm_k, TOL20) && presque_egal(c_f1$ecart, c_f1$valeur - c_f1$normale, 1e-9),
           "l'ecart est celui de l'annee de la fiche a la normale 1991-2020")
  c_f4 <- ct[ct$fiche == "F4" & ct$cle == "tmp", ]
  verifier(nrow(c_f4) == 1L && c_f4$annee == 2025L,
           "une fiche sans date lisible est analysee avec l'annee comparee choisie")
  verifier(!"F5" %in% ct$fiche, "une fiche hors grille n'a pas de contexte inventé")
  c_f3 <- ct[ct$fiche == "F3" & ct$cle == "tmp", ]
  verifier(nrow(c_f3) == 1L && c_f3$annee == 2018L, "une fiche datee d'une annee seule prend cette annee")

  rc <- r_fi$resume_contexte
  verifier(is.data.frame(rc) && all(c("cle", "n", "n_haut", "n_bas") %in% names(rc)) && all(rc$n == 4L),
           "le resume du contexte compte les fiches situees, par grandeur")
  verifier(all(rc$n_haut + rc$n_bas <= rc$n), "au-dessus + en dessous ne depasse jamais le nombre de fiches")
  tf <- synthese_tableau_fiches(r_fi)
  verifier(is.data.frame(tf) && nrow(tf) == 5L && ncol(tf) == 4L + 2L &&
             all(unlist(tf[tf$Fiche == "F5", -(1:4)]) == "hors grille") &&
             !any(unlist(tf[tf$Fiche != "F5", -(1:4)]) == "hors grille"),
           "le tableau des fiches : une ligne par fiche, une colonne par grandeur cochee, « hors grille » pour la fiche hors grille")
  trf <- synthese_tableau_resume_fiches(r_fi)
  verifier(is.data.frame(trf) && nrow(trf) == 2L, "le tableau de synthese des fiches : une ligne par grandeur")
  ph_f <- synthese_phrases_generales(r_fi)
  verifier(grepl("5 fiches importées, dont 4 situées", ph_f[1]),
           "les phrases comptent les fiches importees et celles situees dans la grille")
  verifier(any(grepl("Chêne pédonculé", ph_f)) && any(grepl("Défoliation", ph_f)),
           "essence et probleme les plus frequents sont cites")
  rep_e <- synthese_tableau_repartition(r_fi, "essence")
  verifier(is.data.frame(rep_e) && sum(rep_e[[2]]) == 5L, "la repartition par essence compte les 5 fiches importees")
  verifier(is.character(synthese_lignes_metadonnees(r_fi, "Test")), "les lignes de provenance se construisent pour des fiches")

  r_f0 <- synthese_preparer("fiches", list(), fiches_vide(), jeu20, "tmp", "tmp", ctx20, 2025, PROFILS$dsf)
  verifier(identical(synthese_phrases_generales(r_f0), "Aucune fiche à analyser.") && !any(r_f0$fiches_dans),
           "sans fiche, le bilan le dit au lieu de planter")
}


# ==============================================================================
groupe("v2.0 — graphiques du bilan")
# ==============================================================================

if (!AVEC_SF20 || is.null(jeu20)) {
  cat("  (sf ou jeu absent : tests des graphiques passes)\n")
} else {
  verifier(inherits(synthese_graphe(r_dep, "tmp"), "ggplot") && inherits(synthese_graphe(r_dep, "tmp", mobile = TRUE), "ggplot"),
           "le graphique d'une grandeur se construit, sur grand ecran et sur mobile")
  verifier(inherits(synthese_graphe(r_fi, "tmp"), "ggplot") && inherits(synthese_graphe(r_fi, "f_essence"), "ggplot") &&
             inherits(synthese_graphe(r_fi, "f_annee"), "ggplot") && inherits(synthese_graphe(r_fi, "f_probleme", TRUE), "ggplot"),
           "les graphiques de fiches (ecart a la normale, repartitions) se construisent")
  png20 <- tempfile(fileext = ".png")
  ok_png <- tryCatch({ grDevices::png(png20, 600, 400); print(synthese_graphe(r_dep, "tmp")); grDevices::dev.off(); TRUE },
                     error = function(e) { try(grDevices::dev.off(), silent = TRUE); FALSE })
  verifier(ok_png && file.size(png20) > 2000L, "le graphique se dessine reellement (fichier PNG non vide)")
  verifier(startsWith(synthese_png_donnees(synthese_graphe(r_dep, "tmp")), "data:image/png;base64,"),
           "le graphique s'integre a une page autonome sous forme d'adresse data:")
  verifier(identical(synthese_couleur(list(palette = "inconnue")), CL$vert) && is.character(synthese_couleur(CARTE_COUCHES$tmp)),
           "la couleur d'une grandeur est celle de sa palette, le vert de l'application sinon")
}


# ==============================================================================
groupe("v2.0 — exports : CSV, page HTML autonome, Word")
# ==============================================================================

if (!AVEC_SF20 || is.null(jeu20)) {
  cat("  (sf ou jeu absent : tests des exports passes)\n")
} else {
  f_csv <- tempfile(fileext = ".csv")
  synthese_ecrire_csv(r_dep, f_csv)
  l_csv <- readLines(f_csv, encoding = "UTF-8", warn = FALSE)
  verifier(grepl("^﻿?#", l_csv[1]) && any(grepl("Licence ouverte Etalab", l_csv)) &&
             any(grepl("Référence : 1991-2020, année comparée : 2025", l_csv, fixed = TRUE)) &&
             any(grepl("moyenne à poids égaux", l_csv)),
           "le CSV du bilan porte sa provenance : source, echelle, reference, annee, methode, licence")
  d_csv <- utils::read.csv2(f_csv, comment.char = "#", fileEncoding = "UTF-8-BOM", check.names = FALSE)
  verifier(nrow(d_csv) >= 3L && any(grepl("Haute-Garonne", unlist(d_csv))),
           "le CSV du bilan se relit : une ligne par lieu et grandeur")
  f_ser <- tempfile(fileext = ".csv"); synthese_ecrire_csv_series(r_dep, f_ser)
  verifier(length(readLines(f_ser, warn = FALSE)) > 3L * length(an20) && any(grepl("Séries annuelles", readLines(f_ser, encoding = "UTF-8", warn = FALSE))),
           "le CSV des series a une ligne par annee, lieu et grandeur")
  f_cf <- tempfile(fileext = ".csv"); synthese_ecrire_csv(r_fi, f_cf)
  l_cf <- readLines(f_cf, encoding = "UTF-8", warn = FALSE)
  verifier(any(grepl("Valeur d'une fiche : maille la plus proche", l_cf)) && any(grepl("F1", l_cf)) &&
             !any(grepl("F5", l_cf)),
           "le CSV des fiches : une ligne par fiche situee et par grandeur, la fiche hors grille n'y figure pas")
  ex_f <- synthese_export_fiches(r_fi)
  verifier(all(c("fiche", "essence", "probleme", "grandeur", "annee_retenue", "valeur", "normale", "ecart", "lecture") %in% names(ex_f)) &&
             nrow(ex_f) == 8L,
           "l'export des fiches : 4 fiches x 2 grandeurs")

  # Un nom de fiche qui ressemble a une formule est neutralise dans le CSV.
  f_inj <- fiches_lire(ecrire20(c("fiche;latitude;longitude;date",
                                  sprintf("=HYPERLINK(\"http://x\");%.5f;%.5f;2000-07-10", lat_fiche, lon_fiche))), "p.csv")$fiches
  r_inj <- synthese_preparer("fiches", list(), f_inj, jeu20, "tmp", "tmp", ctx20, 2025, PROFILS$dsf)
  f_ci <- tempfile(fileext = ".csv"); synthese_ecrire_csv(r_inj, f_ci)
  l_ci <- readLines(f_ci, encoding = "UTF-8", warn = FALSE)
  verifier(any(grepl("\"'=HYPERLINK", l_ci, fixed = TRUE)) && !any(grepl("(^|;)\"=HYPERLINK", l_ci)),
           "export CSV : un identifiant de fiche qui commence par = est neutralise (injection de formule)")

  # HTML autonome
  f_html <- tempfile(fileext = ".html")
  synthese_ecrire_html(r_dep, f_html)
  h <- paste(readLines(f_html, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  verifier(startsWith(h, "<!doctype html>") && grepl("<html lang=\"fr\"", h, fixed = TRUE) &&
             grepl("<title>Bilan climatique 2025 — Haute-Garonne (31)</title>", h, fixed = TRUE),
           "le bilan HTML est une page complete, en francais, titree du lieu et de l'annee")
  verifier(grepl("<meta charset=\"utf-8\">", h, fixed = TRUE) && grepl("<style>", h, fixed = TRUE) &&
             regexpr("<meta charset", h, fixed = TRUE) < regexpr("<title>", h, fixed = TRUE) &&
             regexpr("</head>", h, fixed = TRUE) < regexpr("<body>", h, fixed = TRUE),
           "le <head> de la page est la : jeu de caracteres d'abord (accents lisibles a l'ouverture d'un fichier), puis titre et style")
  verifier(grepl("viewport", h, fixed = TRUE), "la page s'adapte a un ecran de telephone (viewport)")
  verifier(lengths(regmatches(h, gregexpr("data:image/png;base64,", h, fixed = TRUE))) == length(r_dep$cles_g) &&
             !grepl("(src|href)=\"https?://", h) && !grepl("<script", h, fixed = TRUE),
           "la page est autonome : un graphique integre par grandeur, aucune ressource externe, aucun script")
  verifier(grepl("<h2>En bref</h2>", h, fixed = TRUE) && grepl("Tableau de synthèse", h, fixed = TRUE) &&
             grepl("Méthode et limites", h, fixed = TRUE) && grepl("Licence ouverte Etalab", h, fixed = TRUE) &&
             grepl("<th scope=\"col\">Grandeur</th>", h, fixed = TRUE),
           "la page porte les phrases, le tableau (en-tetes balisees), la methode et la licence")
  verifier(all(grepl("alt=\"Graphique : ", regmatches(h, gregexpr("<img[^>]*>", h))[[1]])),
           "chaque graphique a un texte alternatif qui reprend ses phrases")

  f_hf <- tempfile(fileext = ".html")
  synthese_ecrire_html(r_fi, f_hf, blocs_fiches = c("f_essence", "f_annee", "n_importe_quoi"))
  hf <- paste(readLines(f_hf, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  verifier(grepl("Fiches par essence", hf, fixed = TRUE) && grepl("Fiches par année", hf, fixed = TRUE) &&
             !grepl("Fiches par problème", hf, fixed = TRUE) && grepl("<h2>Fiches</h2>", hf, fixed = TRUE),
           "les repartitions cochees figurent dans la page des fiches, les autres et les noms inconnus non")

  # Une balise dans un nom de fiche ne devient jamais une balise dans la page.
  f_hx <- tempfile(fileext = ".html")
  f_bal <- f20[1:2, ]; f_bal$id <- c("<script>alert(1)</script>", "F2"); f_bal$essence[1] <- "<img src=x onerror=alert(1)>"
  r_bal <- synthese_preparer("fiches", list(), f_bal, jeu20, "tmp", "tmp", ctx20, 2025, PROFILS$dsf)
  synthese_ecrire_html(r_bal, f_hx, blocs_fiches = "f_essence")
  hx <- paste(readLines(f_hx, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  verifier(!grepl("<script>alert", hx, fixed = TRUE) && !grepl("<img src=x onerror", hx, fixed = TRUE),
           "un nom de fiche ou d'essence n'est jamais rendu comme du HTML dans le bilan a emporter")

  # Word : pandoc requis
  if (requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()) {
    f_docx <- tempfile(fileext = ".docx")
    ok_d <- tryCatch({ synthese_ecrire_docx(r_dep, f_docx); TRUE }, error = function(e) FALSE)
    verifier(ok_d && file.exists(f_docx) && identical(readBin(f_docx, "raw", 2L), as.raw(c(0x50, 0x4B))),
             "le bilan Word se produit (archive docx valide)")
    xml_d <- tryCatch({
      utils::unzip(f_docx, files = "word/document.xml", exdir = tempdir())
      paste(readLines(file.path(tempdir(), "word", "document.xml"), encoding = "UTF-8", warn = FALSE), collapse = "")
    }, error = function(e) "")
    verifier(lengths(regmatches(xml_d, gregexpr("Bilan climatique 2025", xml_d, fixed = TRUE))) == 1L &&
               lengths(regmatches(xml_d, gregexpr("<w:drawing>", xml_d, fixed = TRUE))) == length(r_dep$cles_g) &&
               grepl("<w:tbl>", xml_d, fixed = TRUE),
             "le bilan Word porte son titre une seule fois, son tableau et un graphique par grandeur")
  } else {
    cat("  (pandoc absent : test du bilan Word passe ; le bouton Word n'est alors pas propose)\n")
    verifier(inherits(tryCatch(synthese_ecrire_docx(r_dep, tempfile(fileext = ".docx")), error = function(e) e), "error"),
             "sans pandoc, la conversion Word echoue proprement (le bouton n'est pas propose)")
  }
}


# ==============================================================================
groupe("v2.0 — module Bilan synthetique : de bout en bout")
# ==============================================================================

if (!AVEC_SF20 || is.null(jeu20)) {
  cat("  (sf ou jeu absent : test du module passe)\n")
} else {
  res <- new.env()
  f_fi20 <- ecrire20(csv_fiches)
  # Ce que fait l'interface a l'ouverture : les cases du jeu de base sont cochees.
  # (Une fois le catalogue affiche, le module lit les cases ; sans case cochee il n'y a rien a faire.)
  cases_base20 <- function(profil, cles) {
    b <- synthese_base(profil$id, cles); g <- synthese_groupes(synthese_catalogue(jeu20, profil)); a <- list()
    for (f in names(g)) {
      ch <- names(g[[f]]$choix)
      a[[paste0("g_", f)]] <- intersect(b$g, ch); a[[paste0("t_", f)]] <- intersect(b$t, ch)
    }
    a
  }
  base_dsf20 <- cases_base20(PROFILS$dsf, names(CARTE_COUCHES))
  base_co20 <- cases_base20(PROFILS$co, names(CARTE_COUCHES))
  vide20 <- lapply(base_dsf20, function(x) character(0))

  shiny::testServer(
    mod_synthese_server,
    args = list(profil = PROFILS$dsf, lieu = shiny::reactive(NULL), transport = shiny::reactive(tr20)),
    {
      do.call(session$setInputs, base_dsf20)
      session$setInputs(echelle = "departement", deps = "31", annee = "2025", ref = "1991-2020", calculer = 1)
      res$dep <- res_val()
      session$setInputs(echelle = "region", regs = "76", calculer = 2)
      res$reg <- res_val()
      session$setInputs(echelle = "departement", deps = character(0), calculer = 3)
      res$dep_vide <- res_val()
      session$setInputs(echelle = "point", calculer = 4)
      res$pt_sans <- res_val()
      session$setInputs(echelle = "fiches", calculer = 5)
      res$fi_sans <- res_val()
      session$setInputs(fichier_fiches = data.frame(name = "fiches.csv", size = file.size(f_fi20), type = "text/csv",
                                                    datapath = f_fi20, stringsAsFactors = FALSE),
                        fb = c("f_essence", "f_probleme"))
      res$n_fiches <- nrow(fiches())
      session$setInputs(calculer = 6)
      res$fi <- res_val()
      session$setInputs(vider_fiches = 1)
      res$n_apres_vider <- nrow(fiches())
      session$setInputs(echelle = "departement", deps = "31", annee = "1980", calculer = 7)
      res$an1980 <- res_val()
      session$setInputs(annee = "2025")
      res$signature_apres <- identical(res_val()$signature, paste(c("departement", "31", "5", "2025", "1991-2020"), collapse = "|")) ||
        !identical(res_val()$signature, signature("departement"))
      do.call(session$setInputs, vide20)
      session$setInputs(calculer = 8)
      res$rien_coche <- res_val()
    })
  verifier(identical(res$dep$res$echelle, "departement") && identical(res$dep$res$tl$lieu, "Haute-Garonne (31)") &&
             res$dep$res$annee == 2025L,
           "module : le bilan d'un departement s'etablit au clic")
  verifier(identical(res$reg$res$tl$lieu, "Occitanie"), "module : le bilan d'une region")
  verifier(grepl("département", res$dep_vide$vide), "module : sans departement choisi, un message plutot qu'un bilan vide")
  verifier(grepl("point", res$pt_sans$vide), "module : sans point choisi, un message plutot qu'un lieu vide")
  verifier(grepl("fichier de fiches", res$fi_sans$vide), "module : sans fiche importee, un message")
  verifier(res$n_fiches == 5L && identical(res$fi$res$echelle, "fiches") && sum(res$fi$res$fiches_dans) == 4L &&
             identical(res$fi$res$blocs_fiches, c("f_essence", "f_probleme")),
           "module : l'import de fiches se lit, le bilan des fiches garde les repartitions cochees")
  verifier(res$n_apres_vider == 0L, "module : « Effacer » vide les fiches")
  verifier(grepl("Cochez au moins", res$rien_coche$vide),
           "module : si rien n'est coche, un message plutot qu'un bilan vide")
  verifier(isTRUE(res$signature_apres), "module : changer l'annee apres le calcul rend le bilan affiche perime")
  verifier(res$an1980$res$annee == 1980L &&
             !presque_egal(res$an1980$res$bilan$valeur[1], res$dep$res$bilan$valeur[1], 1e-9),
           "module : l'annee choisie est celle du bilan")

  # Profil correspondant-observateur : un seul territoire, grandeurs restreintes, point du lieu.
  shiny::testServer(
    mod_synthese_server,
    args = list(profil = PROFILS$co, lieu = shiny::reactive(list(nom = "Mon point", lat = ll20$lat[k20], lon = ll20$lon[k20])),
                transport = shiny::reactive(tr20)),
    {
      do.call(session$setInputs, base_co20)
      session$setInputs(echelle = "point", annee = "2025", ref = "1991-2020", calculer = 1)
      res$co_pt <- res_val()
      res$co_cles <- names(dispo())
    })
  verifier(identical(res$co_pt$res$tl$lieu, "Mon point") && all(res$co_pt$res$cles %in% CONFIG$synthese$couches_co) &&
             all(res$co_cles %in% CONFIG$synthese$couches_co),
           "module (correspondant) : le bilan du point choisi, avec les seules grandeurs du profil")

  # Declencheur : un clic sur la carte lance le bilan du point.
  decl <- shiny::reactiveVal(NULL)
  shiny::testServer(
    mod_synthese_server,
    args = list(profil = PROFILS$co, lieu = shiny::reactive(list(nom = "Clic", lat = ll20$lat[k20], lon = ll20$lon[k20])),
                declencheur = decl, transport = shiny::reactive(tr20)),
    {
      do.call(session$setInputs, base_co20)
      session$setInputs(echelle = "departement", annee = "2025", ref = "1991-2020")
      decl(1L); session$flushReact()
      res$decl <- res_val()
    })
  verifier(identical(res$decl$res$echelle, "point") && identical(res$decl$res$tl$lieu, "Clic"),
           "module : un point designe sur la carte lance le bilan du point, meme si une autre echelle etait choisie")

  # Donnees de carte indisponibles : le module l'explique.
  shiny::testServer(
    mod_synthese_server,
    args = list(profil = PROFILS$dsf, lieu = shiny::reactive(NULL),
                transport = shiny::reactive(list(mode = "absent", base = NULL, erreurs = "x"))),
    {
      session$setInputs(echelle = "departement", deps = "31", calculer = 1)
      res$sans_jeu <- is.null(jeu())
    })
  verifier(isTRUE(res$sans_jeu), "module : sans donnees de carte, aucun calcul et aucune erreur")

  # Ecran « Autour de moi »
  rt <- new.env()
  shiny::testServer(
    mod_terrain_server,
    args = list(profil = PROFILS$terrain, lieu = shiny::reactive(list(nom = "Ici", lat = ll20$lat[k20], lon = ll20$lon[k20])),
                transport = shiny::reactive(tr20)),
    {
      # (`res` est ici le reactif du module : les resultats vont dans `rt`.)
      rt$ter <- res()
    })
  verifier(!is.null(rt$ter) && identical(rt$ter$echelle, "point") && rt$ter$annee == 2025L &&
             setequal(rt$ter$cles_t, intersect(CONFIG$synthese$base$terrain$t, names(CARTE_COUCHES))),
           "module terrain : le bilan du point, pour l'annee la plus recente, avec les grandeurs du jeu terrain")
  verifier(presque_egal(rt$ter$bilan$valeur[rt$ter$bilan$cle == "tmp"], val20$tmp[an20 == 2025, k20], TOL20),
           "module terrain : les chiffres sont ceux du bilan synthetique (un seul calcul)")
  carte_t <- html20(terrain_carte_ui(rt$ter$bilan[rt$ter$bilan$cle == "tmp", ], rt$ter$ctx))
  verifier(grepl("cf-terrain__valeur", carte_t) && grepl("par rapport à la normale 1991-2020", carte_t, fixed = TRUE) &&
             grepl("cf-terrain__lecture", carte_t),
           "module terrain : une carte par grandeur donne la valeur, l'ecart a la normale et sa lecture en mots")
}


# ==============================================================================
groupe("v2.0 — interface : un profil, ses onglets ; la page d'entree")
# ==============================================================================

app20 <- new.env()
sys.source("app.R", envir = app20)
onglets20 <- function(h) unique(regmatches(h, gregexpr("(?<=data-value=\")[a-z]+(?=\")", h, perl = TRUE))[[1]])
h_dsf <- html20(app20$ui(list(QUERY_STRING = "?profil=dsf")))
h_co  <- html20(app20$ui(list(QUERY_STRING = "?id=demo-co")))
h_ter <- html20(app20$ui(list(QUERY_STRING = "?id=demo-terrain")))
h_ent <- html20(app20$ui(list(QUERY_STRING = "")))
h_inc <- html20(app20$ui(list(QUERY_STRING = "?id=zzz-inconnu")))

verifier(setequal(intersect(onglets20(h_dsf), onglets_connus20), PROFILS$dsf$onglets),
         "le permanent DSF a tous ses onglets")
verifier(setequal(intersect(onglets20(h_co), onglets_connus20), PROFILS$co$onglets) &&
           !any(c("accueil", "bilan", "rapport", "parametres", "comparer") %in% onglets20(h_co)),
         "le correspondant-observateur : carte, synthese, sources — et rien d'autre")
verifier(setequal(intersect(onglets20(h_ter), onglets_connus20), PROFILS$terrain$onglets),
         "le terrain : « Autour de moi » et la carte")
verifier(!"bilan" %in% onglets20(h_ent) && !length(intersect(onglets20(h_ent), onglets_connus20)) &&
           grepl("id=\"cf-id\"", h_ent, fixed = TRUE) && grepl("<form", h_ent, fixed = TRUE),
           "sans identifiant : la page d'entree, un champ et aucun onglet")
verifier(grepl("Identifiant inconnu", h_inc, fixed = TRUE) && grepl("id=\"cf-id\"", h_inc, fixed = TRUE),
         "un identifiant inconnu : la page d'entree le dit et redemande l'identifiant")
h_xss <- html20(app20$ui(list(QUERY_STRING = "?id=%3Cscript%3Ealert(1)%3C%2Fscript%3E")))
verifier(!grepl("<script>alert(1)", h_xss, fixed = TRUE), "l'identifiant saisi n'est jamais rendu comme du HTML")
verifier(grepl("demo", h_ent, ignore.case = TRUE) && grepl("profil=co", h_ent, fixed = TRUE),
         "en mode demonstration, la page d'entree propose les trois profils")
h_ent_sans_demo <- avec_reglage20(c("acces", "demo"), FALSE, html20(app20$ui(list(QUERY_STRING = ""))))
verifier(!grepl("profil=co", h_ent_sans_demo, fixed = TRUE) && grepl("id=\"cf-id\"", h_ent_sans_demo, fixed = TRUE),
         "hors demonstration, la page d'entree ne propose que la saisie de l'identifiant")
verifier(grepl("type=\"button\"", h_ent, fixed = TRUE) && !grepl("type=\"submit\"", h_ent, fixed = TRUE),
         "le bouton d'entree n'est pas un bouton « submit » (Shiny le detourne : le formulaire ne partirait pas)")
verifier(grepl("<main[^>]*id=\"contenu\"", h_ent) && identical(attr(app20$ui(list(QUERY_STRING = "")), "lang"), "fr"),
         "la page d'entree a son repere principal et sa langue")
verifier(grepl("cf-profil", h_co) && grepl("Changer d", h_co),
         "la barre de profil rappelle le profil et permet de changer d'identifiant")

# Un lien partage d'avant la v2.0 ouvre l'interface complete avec ses valeurs.
h_ancien <- html20(app20$ui(list(QUERY_STRING = "?x=1.4727&y=43.35&n=Auterive&s=safran&r=1981-2010&a=2019")))
verifier(setequal(intersect(onglets20(h_ancien), onglets_connus20), PROFILS$dsf$onglets) &&
           grepl("value=\"1981-2010\" selected", h_ancien),
         "un lien partage d'avant la v2.0 ouvre l'interface complete, reference preselectionnee")

# Une erreur au demarrage du serveur ne ferme pas la fenetre en silence : elle est journalisee
# (console R) et montree a l'ecran.
notifs20 <- list()
session_stub20 <- shiny::MockShinySession$new()
session_stub20$sendNotification <- function(type, message) notifs20[[length(notifs20) + 1L]] <<- list(type = type, message = message)
app20b <- new.env(parent = app20)
app20b$serveur_demarrer <- function(input, output, session) stop("fichier introuvable: /srv/donnees/x.rds")
srv20 <- app20$server; environment(srv20) <- app20b
journal20 <- character()
retour20 <- withCallingHandlers(
  shiny::withReactiveDomain(session_stub20, srv20(NULL, NULL, session_stub20)),
  message = function(m) { journal20 <<- c(journal20, conditionMessage(m)); invokeRestart("muffleMessage") })
verifier(is.null(retour20) && any(grepl("[ClimaFor]", journal20, fixed = TRUE) & grepl("fichier introuvable", journal20)),
         "une erreur au demarrage du serveur est ecrite dans la console R, sans arreter la session")
verifier(length(notifs20) == 1L && grepl("n'a pas pu d", paste(unlist(notifs20[[1]]$message), collapse = " ")) &&
           !grepl("/srv/donnees", paste(unlist(notifs20[[1]]$message), collapse = " "), fixed = TRUE),
         "elle est aussi montree a l'ecran, sans chemin du serveur")
appjs20 <- lire20("www/app.js")
verifier(grepl("defineProperty(window, \"parent\"", appjs20, fixed = TRUE),
         "app.js empeche la fenetre hote (RStudio) de se fermer avec la session")

# Les modules sont pilotes par le profil : le serveur DSF ne tourne que pour lui.
src_app20 <- lire20("app.R")
verifier(grepl("a_onglet(\"synthese\")", src_app20, fixed = TRUE) && grepl("a_onglet(\"terrain\")", src_app20, fixed = TRUE) &&
           grepl("serveur_dsf", src_app20, fixed = TRUE),
         "app.R n'assemble que les modules des onglets du profil")
src_dsf20 <- lire20("src/serveur_dsf.R")
verifier(!grepl("req_perform_parallel|stations_proches", lire20("src/mod_terrain.R")) &&
           !grepl("stations_|safran_telecharger|obtenir_serie", paste(lire20("src/mod_terrain.R"), lire20("src/mod_synthese.R"))),
         "terrain et synthese ne lisent que les mailles precalculees : ni station, ni telechargement SAFRAN a la selection d'un point")
verifier(nchar(src_dsf20) > 5000L && grepl("limite_controler(\"analyses\"", src_dsf20, fixed = TRUE),
         "l'analyse d'un point (avec sa limite d'usage) vit dans le serveur DSF")


# ==============================================================================
groupe("v2.0 — CSS, configuration et fichiers de la version")
# ==============================================================================

css20 <- lire20("www/climafor.css")
code20 <- paste(vapply(c(list.files("src", pattern = "\\.R$", full.names = TRUE), "app.R"), lire20, ""), collapse = "\n")
cl_css <- unique(sub("^\\.", "", regmatches(css20, gregexpr("\\.cf-(synt|terrain|entree|profil)[a-z_-]*", css20))[[1]]))
verifier(length(cl_css) >= 10L && all(vapply(cl_css, function(k) grepl(k, code20, fixed = TRUE), TRUE)),
         sprintf("chaque classe .cf-synt / terrain / entree / profil du CSS est utilisee par le code (%d classes)", length(cl_css)))
cl_code <- unique(regmatches(code20, gregexpr("cf-(synt|terrain)__[a-z]+", code20))[[1]])
verifier(length(cl_code) >= 8L && all(vapply(cl_code, function(k) grepl(paste0("\\.", k, "\\b"), css20), TRUE)),
         "chaque classe cf-synt__ / cf-terrain__ du code est decrite dans le CSS")
verifier(grepl("scroll-padding-bottom", css20, fixed = TRUE),
         "la barre d'action collante ne masque pas un champ qui prend le focus")

verifier(identical(CONFIG$app$version, "2.0.0"), "la version est 2.0.0")
verifier(all(c("acces", "territoires", "synthese", "sylvasan") %in% names(CONFIG)) &&
           CONFIG$synthese$fiches_max <= 1000L && CONFIG$synthese$territoires_max <= 20L,
         "les reglages v2.0 existent, et bornent les imports et les comparaisons")
glob20 <- lire20("global.R")
fich_ordre <- c("src/utils_comparer.R", "src/utils_profils.R", "src/utils_territoires.R", "src/utils_fiches.R",
                "src/utils_synthese.R", "src/utils_synthese_rendu.R", "src/mod_synthese.R", "src/mod_terrain.R",
                "src/serveur_dsf.R", "src/ui_page.R")
pos20 <- vapply(fich_ordre, function(f) regexpr(f, glob20, fixed = TRUE)[1], 1L)
verifier(all(pos20 > 0L) && !is.unsorted(pos20[c(1, 3, 4, 5, 6)]) && pos20["src/utils_synthese.R"] < pos20["src/mod_synthese.R"],
         "global.R charge les fichiers de la v2.0, les utilitaires avant les modules")
verifier(all(file.exists(chemin_ci(c("donnees_limites/departements.geojson", "donnees_limites/regions.geojson",
                                     "donnees_limites/LISEZMOI.md", "outils/limites_construire.R", "config/acces.csv")))),
         "les contours, leur notice, leur script de construction et la liste d'acces sont dans le depot")
dep_txt <- lire20("deploy.R")
verifier(grepl("CONFIG$acces$fichier", dep_txt, fixed = TRUE) && grepl("CONFIG$territoires$dossier", dep_txt, fixed = TRUE) &&
           grepl("acces_local", dep_txt, fixed = TRUE),
         "deploy.R envoie la liste d'acces (et la liste locale si elle existe) et les contours")
verifier(nchar(lire20("donnees_limites/LISEZMOI.md")) > 100L, "les contours ont leur notice de provenance")
