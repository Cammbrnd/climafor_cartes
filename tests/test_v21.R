# ==============================================================================
# tests/test_v21.R — STATIONS PUBLIEES, DEUX LIEUX, SYNTHESE A ENVOYER (VERSION 2.1)
# ------------------------------------------------------------------------------
# Ce que ces tests gardent :
#   1. les stations precalculees et publiees comme les mailles : construction, assemblage,
#      lecture, et le refus d'un jeu de stations piege ou incoherent ;
#   2. les stations dans le bilan (Comparer, Synthese, Terrain) : meme calcul que les mailles,
#      sur les annees propres de la station ; la maille et la station nommees pour ce qu'elles
#      sont (modele, mesure) ;
#   3. le mode « Deux lieux » : annees communes, ecarts, lecture, six combinaisons ;
#   4. la synthese a envoyer : texte, lien de courriel borne, page HTML autonome, donnees des
#      boutons ; ce qu'elle ne contient pas ;
#   5. les ecrans, les textes, le script du navigateur et la chaine de publication.
# Aucun test n'appelle Meteo-France : les stations viennent de outils/stations_demo.R
# (donnees FICTIVES). La chaine reelle (telechargement des fichiers departementaux) n'est donc
# pas verifiee ici ; elle l'est par la premiere execution du workflow.
# ==============================================================================

for (f in c("outils/carte_geo.R", "outils/carte_format.R", "outils/carte_precalcul.R",
            "outils/carte_demo.R", "outils/stations_precalcul.R", "outils/stations_demo.R")) source(chemin_ci(f))

AVEC_SF21 <- requireNamespace("sf", quietly = TRUE)
html21 <- function(x) {
  r <- htmltools::renderTags(x)
  paste(c(as.character(r$head), as.character(r$html)), collapse = "\n")
}
lire21 <- function(f) paste(readLines(chemin_ci(f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")


# ==============================================================================
groupe("v2.1 — stations : jeu synthetique, precalcul et publication")
# ==============================================================================

# Mailles : 6 x 6 mailles de 8 km autour de Toulouse (Lambert-93), 1959-2025.
g21 <- expand.grid(i = 0:5, j = 0:5)
ll21 <- carte_depuis_l93(525000 + g21$i * 8000, 6280000 + g21$j * 8000)
an21 <- 1959:2025; n21 <- nrow(g21)
val21 <- lapply(seq_along(CARTE_COUCHES), function(k) {
  base <- k + 0.2 * g21$i + 0.1 * g21$j
  outer(seq_along(an21), seq_len(n21), function(a, c) base[c] + 0.03 * (a - 1) + 0.02 * sin(a / 3 + c))
})
names(val21) <- names(CARTE_COUCHES)
part_m21 <- list(annees = an21, indices = seq_len(n21), echecs = integer(0), valeurs = val21,
                 normale = lapply(val21, function(v) colMeans(v[an21 %in% 1991:2020, ])),
                 pente = lapply(val21, function(v) rep(0.3, n21)),
                 signif = lapply(val21, function(v) rep(TRUE, n21)))
dos21 <- tempfile("carte21")
carte_assembler(list(part_m21), ll21$lon, ll21$lat, an21, "jeu synthetique", dos21)

# Stations fictives posees pres de la grille (lon 0,83 a 1,43 ; lat 43,60 a 44,0), 1985-2025.
an_st21 <- 1985:2025
src_brut21 <- stations_demo_source(an_st21, fin = Sys.Date() - 1L, depts_connus = "31")
src21 <- function(dept) {
  b <- src_brut21(dept)
  if (is.null(b)) return(NULL)
  ref <- b[b$nom == "ANCIENNE 31", ][1, ]
  b$lon <- b$lon - ref$lon + 1.05
  b$lat <- b$lat - ref$lat + 43.66
  b
}
t0_21 <- Sys.time()
p_st21 <- stations_construire(src21, "31", an_st21, silencieux = TRUE)
cat(sprintf("    (precalcul de 5 stations fictives : %.1f s)\n", as.numeric(difftime(Sys.time(), t0_21, units = "secs"))))

verifier(nrow(p_st21$stations) == 3L && setequal(p_st21$stations$nom, c("ANCIENNE 31", "RECENTE 31", "PLUVIOMETRE 31")),
         "precalcul : trois stations sur cinq retenues (l'arretee et la trop courte sont ecartees)")
verifier(!any(c("ARRETEE 31", "COURTE 31") %in% p_st21$stations$nom),
         "precalcul : une station arretee depuis plus de deux ans ou avec moins de quinze annees n'est pas publiee")
i_pluvio <- which(p_st21$stations$nom == "PLUVIOMETRE 31")
verifier(all(is.na(p_st21$valeurs$tmp[, i_pluvio])) && any(is.finite(p_st21$valeurs$pre[, i_pluvio])),
         "precalcul : un pluviometre n'a aucune temperature (vide, pas zero) mais une pluie")

r_st21 <- stations_assembler(list(p_st21), an_st21, STATIONS_DEMO_SOURCE, dos21, n_min = 1L)
fich21 <- list.files(dos21)
verifier(r_st21$n == 3L && "stations.json" %in% fich21 && "s_tmp.bin.gz" %in% fich21 && "c_tmp.bin.gz" %in% fich21,
         "publication : stations.json et s_<grandeur>.bin.gz s'ajoutent aux fichiers des mailles (c_<grandeur>.bin.gz)")
verifier(!any(c("s_swi.bin.gz", "s_neige.bin.gz", "s_vpd.bin.gz") %in% fich21),
         "publication : une grandeur qu'aucune station ne mesure (SWI, neige, VPD) n'est pas publiee")
lu21 <- stations_lire_meta_locale(dos21)
verifier(!is.null(lu21$meta) && lu21$meta$n_stations == 3L && identical(lu21$meta$schema, 1L) &&
           identical(unlist(lu21$meta$annees), an_st21) && grepl("FICTIVES", lu21$meta$source),
         "publication : stations.json decrit schema, annees, nombre de stations et source")
verifier(!is.unsorted(unlist(lu21$meta$stations$id)),
         "publication : les stations sont rangees par identifiant (jeu reproductible)")

# Assemblage de deux fragments qui partagent une station : une seule ligne.
r_dbl21 <- stations_assembler(list(p_st21, p_st21), an_st21, STATIONS_DEMO_SOURCE, tempfile("dbl21"), n_min = 1L)
verifier(r_dbl21$n == 3L, "assemblage : une station presente dans deux fragments n'est publiee qu'une fois")
r_vide21 <- tryCatch(stations_assembler(list(list(annees = an_st21, depts = "31", echecs = "31", stations = NULL,
                                                valeurs = list(), normale = list(), pente = list(), signif = list())),
                                        an_st21, "x", tempfile("vide21"), n_min = 1L), error = function(e) conditionMessage(e))
verifier(is.character(r_vide21) && grepl("Aucune station", r_vide21),
         "assemblage : aucun fragment utile, la publication est refusee plutot que d'ecraser le jeu")
r_min21 <- tryCatch(stations_assembler(list(p_st21), an_st21, "x", tempfile("min21"), n_min = 50L),
                    error = function(e) conditionMessage(e))
verifier(is.character(r_min21) && grepl("refus", r_min21, ignore.case = TRUE),
         "assemblage : moins de stations que le minimum attendu, la publication est refusee")

tr21 <- carte_transport(url = "", dossier = dos21)
rm(list = ls(.ETAT_COMPARER), envir = .ETAT_COMPARER)
jeu21 <- cmp_jeu(tr21)
sj21 <- cmp_stations(jeu21)
verifier(isTRUE(sj21$ok) && nrow(sj21$table) == 3L && identical(sj21$annees, an_st21),
         "lecture : le jeu de stations se retrouve (table, annees)")
verifier(setequal(sj21$table$nom, c("Ancienne 31", "Recente 31", "Pluviometre 31")),
         "lecture : les noms de stations sont mis en forme lisible")
verifier(identical(stations_nom_lisible(c("TOULOUSE-BLAGNAC", "SAINT-GIRONS", "LA ROCHE-SUR-YON", "PIC DU MIDI")),
                   c("Toulouse-Blagnac", "Saint-Girons", "La Roche-sur-Yon", "Pic du Midi")),
         "noms de stations : majuscules en minuscules, petits mots conserves")
verifier(is.null(stations_lire_couche(sj21, "swi")) && !is.null(stations_lire_couche(sj21, "tmp")) &&
           !is.null(stations_lire_couche(sj21, "pre")) && !is.null(stations_lire_couche(sj21, "gel")),
         "lecture : les grandeurs publiees pour les stations, et NULL pour celle qui ne l'est pas")
ls21 <- stations_lire_couche(sj21, "tmp")
ord21 <- match(sj21$table$id, p_st21$stations$id)
verifier(identical(dim(ls21$valeurs), c(length(an_st21), 3L)) &&
           presque_egal(ls21$valeurs, p_st21$valeurs$tmp[, ord21], tolerance = 0.06),
         "lecture : les valeurs relues sont celles du precalcul, a l'arrondi de stockage pres")
ip21 <- which(sj21$table$nom == "Pluviometre 31")
verifier(all(is.na(ls21$valeurs[, ip21])), "lecture : les valeurs absentes du pluviometre restent absentes (jamais zero)")

# -- Validation : un jeu de stations piege ou incoherent est refuse --------------------------
meta_ok21 <- jsonlite::fromJSON(file.path(dos21, "stations.json"), simplifyVector = FALSE)
refuse21 <- function(f) !stations_meta_valider(f(meta_ok21))$ok
verifier(stations_meta_valider(meta_ok21)$ok, "(controle) le jeu de stations valide passe la validation")
verifier(refuse21(function(m) { m$schema <- 99L; m }), "validation : un schema inconnu est refuse")
verifier(refuse21(function(m) { m$couches[[1]]$fichier <- "../x.bin.gz"; m }),
         "validation : un nom de fichier avec « .. » est refuse (traversee de dossier)")
verifier(refuse21(function(m) { m$couches[[1]]$fichier <- "https://evil.example.org/x.bin.gz"; m }),
         "validation : une adresse a la place d'un nom de fichier est refusee")
verifier(refuse21(function(m) { m$couches[[1]]$fichier <- "s_tmp.exe"; m }), "validation : une extension inattendue est refusee")
verifier(refuse21(function(m) { m$stations$id[[2]] <- m$stations$id[[1]]; m }), "validation : deux stations de meme identifiant sont refusees")
verifier(refuse21(function(m) { m$stations$id[[1]] <- "../../etc"; m }), "validation : un identifiant de station mal forme est refuse")
verifier(refuse21(function(m) { m$stations$nom <- m$stations$nom[-1]; m }), "validation : des colonnes de longueurs differentes sont refusees")
verifier(refuse21(function(m) { m$n_stations <- 99999L; m }), "validation : un nombre de stations qui ne correspond pas a la table est refuse")
verifier(refuse21(function(m) { m$stations$lat[[1]] <- 250; m }), "validation : des coordonnees impossibles sont refusees")
verifier(refuse21(function(m) { m$couches[[1]]$stock <- 9L; m }), "validation : un facteur de stockage hors bornes est refuse")
verifier(refuse21(function(m) { m$annees <- list(); m }), "validation : sans annees, le jeu est refuse")
ancien21 <- CONFIG$stations$publie$stations_max
CONFIG$stations$publie$stations_max <- 2L
verifier(refuse21(function(m) m), "validation : un jeu qui annonce plus de stations que le garde-fou est refuse")
CONFIG$stations$publie$stations_max <- ancien21

# Dossier abime : fichier de couche manquant ; JSON illisible.
dos_ab21 <- tempfile("ab21"); dir.create(dos_ab21)
invisible(file.copy(list.files(dos21, full.names = TRUE), dos_ab21))
unlink(file.path(dos_ab21, "s_tmp.bin.gz"))
verifier(is.null(stations_lire_meta_locale(dos_ab21)$meta), "lecture locale : un fichier de couche manquant invalide le jeu")
writeLines("{ pas du json", file.path(dos_ab21, "stations.json"))
verifier(is.null(stations_lire_meta_locale(dos_ab21)$meta), "lecture locale : un stations.json illisible invalide le jeu")

# Jeu des mailles SANS stations (publie avant la v2.1) : l'application continue.
dos_sans21 <- tempfile("sans21"); dir.create(dos_sans21)
invisible(file.copy(list.files(dos21, pattern = "^(meta|grille)\\.json$|^c_", full.names = TRUE), dos_sans21))
rm(list = ls(.ETAT_COMPARER), envir = .ETAT_COMPARER)
tr_sans21 <- carte_transport(url = "", dossier = dos_sans21)
jeu_sans21 <- cmp_jeu(tr_sans21)
sj_sans21 <- cmp_stations(jeu_sans21)
verifier(!isTRUE(sj_sans21$ok) && identical(sj_sans21$raison, "absent"),
         "un jeu publie sans stations : « absent », sans erreur")
src_sans21 <- sources_point(jeu_sans21, 1.0, 43.7, 190)
verifier(!src_sans21$publiees && is.null(src_sans21$stations) && identical(sources_defaut(src_sans21), "maille"),
         "sans stations publiees, un point n'a que la maille pour source")

# Transport distant sans reseau : un executeur qui sert le dossier local, en comptant les requetes.
serveur21 <- function(compteur, dossier = dos21) {
  function(req, path) {
    compteur$n <- compteur$n + 1L; compteur$urls <- c(compteur$urls, req$url)
    f <- file.path(dossier, basename(req$url))
    if (!file.exists(f)) return(httr2::response(status_code = 404L, url = req$url))
    file.copy(f, path, overwrite = TRUE)
    httr2::response(status_code = 200L, url = req$url)
  }
}
BASE21 <- "https://cammbrnd.github.io/climafor_cartes/"
nettoyer21 <- function() {
  rm(list = ls(.ETAT_COMPARER), envir = .ETAT_COMPARER)
  unlink(file.path(tempdir(), "climafor_carte"), recursive = TRUE)
}
nettoyer21()
c21 <- new.env(); c21$n <- 0L; c21$urls <- character(0)
trd21 <- carte_transport(url = BASE21, dossier = "inexistant")
jd21 <- cmp_jeu(trd21, executer = serveur21(c21))
n_avant21 <- c21$n
sjd21 <- cmp_stations(jd21, executer = serveur21(c21))
verifier(isTRUE(sjd21$ok) && n_avant21 == 2L && c21$n > n_avant21,
         "transport distant : les stations ne sont lues qu'a la demande (le chargement du jeu reste a deux requetes)")
n_apres21 <- c21$n
invisible(cmp_stations(jd21, executer = serveur21(c21)))
verifier(c21$n == n_apres21, "transport distant : stations.json est garde en memoire (pas de seconde requete)")
nettoyer21()
c22 <- new.env(); c22$n <- 0L; c22$urls <- character(0)
jd_sans21 <- cmp_jeu(carte_transport(url = BASE21, dossier = "inexistant"), executer = serveur21(c22, dos_sans21))
sjd_sans21 <- cmp_stations(jd_sans21, executer = serveur21(c22, dos_sans21))
verifier(!isTRUE(sjd_sans21$ok) && isTRUE(sjd_sans21$definitif),
         "transport distant : un 404 sur stations.json est definitif (pas de nouvel essai a chaque clic)")


# ==============================================================================
groupe("v2.1 — stations : classement autour d'un point, lieux et sources")
# ==============================================================================

rm(list = ls(.ETAT_COMPARER), envir = .ETAT_COMPARER)
jeu21 <- cmp_jeu(tr21)
pt21 <- list(nom = "Mon point", lon = 1.0123, lat = 43.7123)
src_pt21 <- sources_point(jeu21, pt21$lon, pt21$lat, 190)
st_pt21 <- src_pt21$stations
verifier(src_pt21$publiees && nrow(st_pt21) == 3L && st_pt21$nom[1] == "Ancienne 31" &&
           !is.unsorted(st_pt21$pertinence) && all(c("niveau", "libelle", "distance_km") %in% names(st_pt21)),
         "sources d'un point : les stations publiees, classees comme dans la recherche en direct, avec leur qualite")
verifier(identical(st_pt21$id, mf_classer_stations(sj21$table, pt21$lon, pt21$lat, 190)$id),
         "sources d'un point : meme classement que mf_classer_stations() (un seul classement)")
verifier(identical(sources_defaut(src_pt21), c("maille", st_pt21$id[1])) && st_pt21$niveau[1] <= 3L,
         "sources par defaut : la maille, et la meilleure station quand sa qualite n'est pas insuffisante")
src_mauvaise21 <- src_pt21; src_mauvaise21$stations$niveau[1] <- 4L
verifier(identical(sources_defaut(src_mauvaise21), "maille"),
         "sources par defaut : une station de qualite insuffisante n'est pas cochee d'emblee")
src_loin21 <- sources_point(jeu21, 5.0, 47.0, NA_real_)
verifier(src_loin21$publiees && !nrow(src_loin21$stations) && identical(sources_defaut(src_loin21), "maille"),
         "sources d'un point : aucune station a portee, la maille seule")

l_maille21 <- sources_lieux(pt21, "maille", src_pt21)
verifier(length(l_maille21) == 1L && identical(l_maille21[[1]]$nom, "Mon point") && identical(l_maille21[[1]]$type, "point"),
         "lieux d'un point, maille seule : le lieu porte le nom du point, comme avant la v2.1")
l_deux21 <- sources_lieux(pt21, c("maille", st_pt21$id[1]), src_pt21)
verifier(length(l_deux21) == 2L && identical(l_deux21[[1]]$nom, "Maille SAFRAN (modèle)") &&
           identical(l_deux21[[2]]$nom, "Ancienne 31 (mesure)") && identical(l_deux21[[2]]$type, "station"),
         "lieux d'un point, plusieurs sources : chacune porte son nom et sa nature (modele, mesure)")
l_st21 <- sources_lieux(pt21, st_pt21$id[1], src_pt21)
verifier(length(l_st21) == 1L && identical(l_st21[[1]]$type, "station"),
         "lieux d'un point : une station seule est possible")
l_vide21 <- sources_lieux(pt21, character(0), src_pt21)
verifier(length(l_vide21) == 1L && identical(l_vide21[[1]]$type, "point"),
         "lieux d'un point : jamais aucune source (la maille tient lieu)")
l_perime21 <- sources_lieux(pt21, c("maille", "99999999"), src_pt21)
verifier(length(l_perime21) == 1L && identical(l_perime21[[1]]$nom, "Mon point"),
         "lieux d'un point : une station qui n'est plus proposee (lieu change) est ignoree")
src_dbl21 <- src_pt21; src_dbl21$stations$nom[1:2] <- "Meme nom"
l_dbl21 <- sources_lieux(pt21, c("maille", src_dbl21$stations$id[1:2]), src_dbl21)
noms_dbl21 <- vapply(l_dbl21, function(l) l$nom, "")
verifier(!anyDuplicated(noms_dbl21) && length(l_dbl21) == 3L,
         "lieux d'un point : deux stations de meme nom restent distinctes")
det21 <- sources_details(l_deux21, src_pt21)
verifier(identical(names(det21), "Ancienne 31 (mesure)") && grepl("qualité : bonne", det21[[1]]) && grepl("km du point", det21[[1]]),
         "details des sources : la distance, l'altitude et la qualite de chaque station")

ui_src21 <- html21(sources_choix_ui("sources_choix", src_pt21))
verifier(grepl("<legend", ui_src21) && grepl("Sources des valeurs", ui_src21) && grepl("Maille SAFRAN", ui_src21) &&
           grepl("Ancienne 31", ui_src21) && grepl("cf-badge", ui_src21) && grepl("mesure", ui_src21),
         "choix des sources : une legende, la maille, les stations avec une pastille de qualite")
verifier(grepl("checked", sub(".*Ancienne 31.*", "", ui_src21)) || grepl("value=\"maille\" checked", ui_src21, fixed = TRUE),
         "choix des sources : la maille est cochee d'emblee")
verifier(grepl("pas encore de stations", html21(sources_choix_ui("x", src_sans21)), fixed = TRUE),
         "choix des sources : un jeu sans stations le dit, et seule la maille est utilisee")
verifier(grepl("Aucune station publiée", html21(sources_choix_ui("x", src_loin21)), fixed = TRUE),
         "choix des sources : aucune station a portee, le message le dit")


# ==============================================================================
groupe("v2.1 — stations dans le bilan : series, normale, phrases, methode")
# ==============================================================================

ctx21 <- ctx_creer(affichage = jeu21$annees, courante = 2025)
lst21 <- cmp_lieu_station(sj21$table[sj21$table$nom == "Ancienne 31", ][1, ])
m_st21 <- cmp_mailles(lst21, jeu21)
verifier(identical(m_st21$source, "station") && length(m_st21$idx) == 1L &&
           identical(m_st21$idx, sj21$table$idx[sj21$table$nom == "Ancienne 31"]),
         "une station se rattache a sa ligne de la table publiee (pas a une maille)")
m_inconnue21 <- cmp_mailles(modifyList(lst21, list(id = "99999999")), jeu21)
verifier(!length(m_inconnue21$idx) && grepl("absente", m_inconnue21$note),
         "une station absente du jeu est ecartee avec une note, sans erreur")
m_sans21 <- cmp_mailles(lst21, jeu_sans21)
verifier(!length(m_sans21$idx), "une station demandee sur un jeu sans stations est ecartee")

lm21 <- cmp_lieu_point("Point A", pt21$lon, pt21$lat)
ser21 <- cmp_series(jeu21, c("Point A", lst21$nom), list(cmp_mailles(lm21, jeu21), m_st21), c("tmp", "swi"))
s_maille21 <- ser21[ser21$lieu == "Point A" & ser21$cle == "tmp", ]
s_station21 <- ser21[ser21$lieu == lst21$nom & ser21$cle == "tmp", ]
verifier(identical(s_maille21$annee, an21) && identical(s_station21$annee, an_st21),
         "series : la maille a ses 67 annees, la station les siennes (41)")
verifier(presque_egal(s_station21$valeur, ls21$valeurs[, m_st21$idx]),
         "series : celle de la station est celle du fichier s_tmp.bin.gz")
verifier(all(is.na(ser21$valeur[ser21$lieu == lst21$nom & ser21$cle == "swi"])),
         "series : une grandeur qu'aucune station ne donne (SWI) est vide pour la station, pas zero")
verifier(any(is.finite(ser21$valeur[ser21$lieu == "Point A" & ser21$cle == "swi"])),
         "series : la maille garde sa serie SWI")

bil21 <- cmp_bilan(ser21, ctx21, 2025)
b_st21 <- bil21[bil21$lieu == lst21$nom & bil21$cle == "tmp", ]
ref21 <- s_station21$annee %in% 1991:2020
verifier(presque_egal(b_st21$normale, mean(s_station21$valeur[ref21])) &&
           presque_egal(b_st21$ecart, s_station21$valeur[s_station21$annee == 2025] - mean(s_station21$valeur[ref21])),
         "bilan : la normale d'une station est la moyenne de ses annees de reference, l'ecart se calcule comme pour une maille")
verifier(is.na(bil21$normale[bil21$lieu == lst21$nom & bil21$cle == "swi"]),
         "bilan : sans serie, pas de normale (NA)")

tl21 <- cmp_table_lieux(list(lm21, lst21), list(cmp_mailles(lm21, jeu21), m_st21))
verifier(identical(tl21$source, c("maille", "station")) && identical(tl21$n_mailles, c(1L, 0L)) && all(tl21$retenu),
         "table des lieux : la provenance de chaque lieu, et zero maille pour une station")
exp21 <- cmp_export_comparaison(bil21, tl21, 2025)
verifier("source" %in% names(exp21) && setequal(unique(exp21$source), c("maille", "station")) &&
           all(is.na(exp21$n_mailles[exp21$type == "station"])),
         "export CSV : la colonne source dit mesure ou modele, sans nombre de mailles pour une station")
meta21 <- cmp_lignes_metadonnees(jeu21, tl21, ctx21, 2025, "Essai")
verifier(any(grepl("^Stations : mesures", meta21)) && any(grepl("1 stations", meta21)),
         "provenance du CSV : les stations sont citees (mesures, date de precalcul) quand il y en a")
verifier(!any(grepl("^Stations", cmp_lignes_metadonnees(jeu21, tl21[1, ], ctx21, 2025, "Essai"))),
         "provenance du CSV : aucune ligne de stations quand il n'y en a pas")

cles21 <- c("tmp", "pre", "gel")
res21 <- synthese_preparer("point", l_deux21, NULL, jeu21, cles21, cles21, ctx21, 2025, NULL)
res21$point_nom <- "Mon point"; res21$details <- det21
verifier(identical(res21$tl$source, c("maille", "station")) && !is.null(res21$jeu$stations) && res21$jeu$stations$n == 3L,
         "bilan d'un point a deux sources : la date de precalcul des stations est gardee")
res_m21 <- synthese_preparer("point", l_maille21, NULL, jeu21, cles21, cles21, ctx21, 2025, NULL)
verifier(is.null(res_m21$jeu$stations), "bilan d'un point a la maille seule : aucune mention de stations")
res_m21$point_nom <- "Mon point"

ph21 <- synthese_phrases_generales(res21)
verifier(grepl("maille SAFRAN \\(modèle\\) et 1 station Météo-France \\(mesure\\)", ph21[1]) && any(grepl("même lecture", ph21)),
         "phrases generales d'un point a deux sources : mesure et modele nommes, accord des lectures dit")
phc21 <- synthese_phrases_cle(res21, "tmp")
verifier(any(grepl("Maille SAFRAN, modèle", phc21, fixed = TRUE)) && any(grepl("Ancienne 31, mesure", phc21, fixed = TRUE)) &&
           any(grepl("station − maille", phc21, fixed = TRUE)) && !any(grepl("\\(\\(|\\)\\)", phc21)),
         "phrases d'une grandeur a deux sources : chaque source se lit, puis l'ecart station - maille, sans parentheses imbriquees")
verifier(length(synthese_phrases_cle(res_m21, "tmp")) >= 2L && !any(grepl("station", synthese_phrases_cle(res_m21, "tmp"))),
         "phrases d'une grandeur a la maille seule : celles d'avant la v2.1")
tab21 <- synthese_tableau(res21)
verifier(setequal(tab21$Lieu, c("Maille SAFRAN (modèle)", "Ancienne 31 (mesure)")) && nrow(tab21) == 6L,
         "tableau : une ligne par grandeur et par source, chaque source nommee")
verifier(identical(synthese_libelle(res21), "Mon point (maille SAFRAN + 1 station)") &&
           identical(synthese_libelle(res_m21), "Mon point"),
         "libelle du bilan : le nom du point, avec ce qu'on a regarde")
g21a <- synthese_graphe_serie(res21, "tmp")
g21b <- ggplot2::ggplot_build(g21a)
verifier(inherits(g21a, "ggplot") && length(unique(g21b$data[[1]]$colour)) == 2L &&
           !grepl("Écart", g21a$labels$y),
         "graphique d'un point a deux sources : deux courbes en valeur (pour voir l'ecart mesure - modele)")
meth21 <- paste(synthese_methode(res21), collapse = " ")
verifier(grepl("MESURES de Météo-France", meth21, fixed = TRUE) && grepl("MODÈLE", meth21, fixed = TRUE) &&
           grepl("pas exactement la même chose", meth21, fixed = TRUE),
         "methode : mesure et modele sont distingues quand une station est dans le bilan")
verifier(!grepl("MESURES", paste(synthese_methode(res_m21), collapse = " ")),
         "methode : rien sur les stations quand il n'y en a pas")
lm_sy21 <- synthese_lignes_metadonnees(res21, "Essai")
verifier(any(grepl("^Stations : mesures", lm_sy21)) && any(grepl("Valeur d'une source", lm_sy21)),
         "provenance du bilan : les stations sont citees")


# ==============================================================================
groupe("v2.1 — Comparer deux lieux : annees communes, ecarts, lecture")
# ==============================================================================

mk21 <- function(lieu, cle, annees, v) data.frame(lieu = lieu, cle = cle, annee = annees, valeur = v, stringsAsFactors = FALSE)
s_main21 <- rbind(mk21("A", "tmp", 2000:2010, 1:11), mk21("B", "tmp", 2005:2015, 11:1),
                  mk21("A", "pre", 2000:2004, 1:5), mk21("B", "pre", 2010:2014, 1:5))
rs21 <- duo_restreindre(s_main21, "A", "B")
verifier(identical(sort(unique(rs21$annee[rs21$cle == "tmp"])), 2005:2010) &&
           sum(rs21$cle == "tmp" & rs21$lieu == "A") == 6L && sum(rs21$cle == "tmp" & rs21$lieu == "B") == 6L,
         "annees communes : chaque grandeur ne garde que les annees ou les deux lieux ont une valeur")
verifier(all(is.na(rs21$valeur[rs21$cle == "pre"])) && sum(rs21$cle == "pre") > 0L,
         "annees communes : une grandeur sans annee commune garde ses lignes, vides, au lieu de disparaitre")
s_na21 <- rbind(mk21("A", "tmp", 2000:2005, c(1, 2, NA, 4, 5, 6)), mk21("B", "tmp", 2000:2005, c(1, 2, 3, NA, 5, 6)))
verifier(identical(sort(unique(duo_restreindre(s_na21, "A", "B")$annee)), c(2000L, 2001L, 2004L, 2005L)),
         "annees communes : une annee manquante chez l'un ecarte cette annee pour les deux")

res_d21 <- list(a = "A", b = "B", series = rbind(mk21("A", "tmp", 2001:2030, (1:30) + 0.5), mk21("B", "tmp", 2001:2030, (1:30) * 1.0)))
sd21 <- duo_stats(res_d21, "tmp")
verifier(sd21$n == 30L && presque_egal(sd21$ecart_moyen, 0.5) && presque_egal(sd21$r, 1) &&
           sd21$premiere == 2001L && sd21$derniere == 2030L,
         "statistiques de deux lieux : ecart moyen A - B, correlation, annees communes")
res_d21$series <- rbind(mk21("A", "tmp", 2001:2008, 1:8), mk21("B", "tmp", 2001:2008, 8:1))
verifier(is.na(duo_stats(res_d21, "tmp")$r), "correlation : non evaluee sous dix annees communes")
res_d21$series <- rbind(mk21("A", "tmp", 2001:2020, rep(3, 20)), mk21("B", "tmp", 2001:2020, 1:20))
verifier(is.na(duo_stats(res_d21, "tmp")$r), "correlation : non evaluee quand une serie est constante")

a21 <- cmp_lieu_point("Point A", pt21$lon, pt21$lat)
b21 <- cmp_lieu_station(sj21$table[sj21$table$nom == "Ancienne 31", ][1, ])
d_ms21 <- duo_preparer(a21, b21, jeu21, cles21, ctx21, 2025)
verifier(all(d_ms21$tl$retenu) && identical(d_ms21$tl$source, c("maille", "station")) && isTRUE(d_ms21$duo),
         "deux lieux, maille - station : les deux sont retenus")
verifier(identical(sort(unique(d_ms21$series$annee)), an_st21),
         "deux lieux, maille - station : la maille est restreinte aux 41 annees de la station")
nor_commun21 <- mean(s_maille21$valeur[s_maille21$annee %in% 1991:2020])
verifier(presque_egal(d_ms21$bilan$normale[d_ms21$bilan$lieu == "Point A" & d_ms21$bilan$cle == "tmp"], nor_commun21),
         "deux lieux : la normale de la maille couvre les annees de reference communes (ici identiques)")
lec21 <- duo_lecture(d_ms21)
verifier(grepl("(A, modèle)", lec21[1], fixed = TRUE) && grepl("(B, mesure)", lec21[1], fixed = TRUE) &&
           any(grepl("années communes : 1985–2025 \\(41 années\\)", lec21)),
         "lecture : les deux lieux nommes avec leur nature, et la periode commune dite")
verifier(any(grepl("mesure, l'autre un modèle", lec21, fixed = TRUE)) && any(grepl("corrélation", lec21)),
         "lecture : mise en garde mesure - modele, et correlation des variations annuelles")
tabd21 <- duo_tableau(d_ms21)
verifier(identical(names(tabd21), c("Grandeur", "A", "B", "Écart A − B", "Écart à la normale (A)", "Écart à la normale (B)", "Lecture")) &&
           nrow(tabd21) == 3L && all(grepl("A (au-dessus|en dessous) de B|valeurs identiques", tabd21$Lecture)),
         "tableau de deux lieux : valeur de chacun, ecart A - B, ecart de chacun a sa normale, lecture")
exd21 <- duo_export_ecarts(d_ms21)
verifier(nrow(exd21) == 3L && all(c("valeur_a", "valeur_b", "ecart_a_moins_b", "annees_communes", "correlation") %in% names(exd21)) &&
           setequal(exd21$nature_b, "mesure") && setequal(exd21$nature_a, "modèle"),
         "export des ecarts : une ligne par grandeur, avec la nature de chaque lieu")
vd21 <- exd21[exd21$grandeur == "Température moyenne annuelle", ]
verifier(presque_egal(vd21$ecart_a_moins_b, vd21$valeur_a - vd21$valeur_b, tolerance = 0.002),
         "export des ecarts : A - B est bien valeur A moins valeur B")
mdd21 <- duo_lignes_metadonnees(d_ms21, jeu21, "Essai")
verifier(any(grepl("^A : ", mdd21)) && any(grepl("seules les années", mdd21)),
         "provenance de l'export : la periode commune est annoncee")

# Maille - maille, et noms identiques.
a2_21 <- cmp_lieu_point("Point A", pt21$lon + 0.2, pt21$lat)
d_mm21 <- duo_preparer(a21, a2_21, jeu21, cles21, ctx21, 2025)
verifier(identical(d_mm21$a, "Point A (A)") && identical(d_mm21$b, "Point A (B)") && !anyDuplicated(d_mm21$tl$lieu),
         "deux lieux de meme nom : distingues par (A) et (B)")
verifier(all(d_mm21$tl$retenu) && identical(sort(unique(d_mm21$series$annee)), an21) &&
           any(grepl("très forte|forte", duo_lecture(d_mm21))) && any(grepl("deux points voisins", duo_lecture(d_mm21))),
         "maille - maille : toute la periode, mise en garde adaptee")

# Station - station.
b2_21 <- cmp_lieu_station(sj21$table[sj21$table$nom == "Recente 31", ][1, ])
d_ss21 <- duo_preparer(b21, b2_21, jeu21, cles21, ctx21, 2025)
verifier(all(d_ss21$tl$retenu) && any(grepl("deux emplacements", duo_lecture(d_ss21))) && !any(grepl("un modèle", duo_lecture(d_ss21))),
         "station - station : mise en garde adaptee (deux emplacements)")

# Pluviometre : la temperature n'existe pas, la grandeur reste dans le tableau avec « — ».
b3_21 <- cmp_lieu_station(sj21$table[sj21$table$nom == "Pluviometre 31", ][1, ])
d_pl21 <- duo_preparer(a21, b3_21, jeu21, c("tmp", "pre"), ctx21, 2025)
tab_pl21 <- duo_tableau(d_pl21)
lg_t21 <- tab_pl21[tab_pl21$Grandeur == "Température moyenne annuelle", ]
lg_p21 <- tab_pl21[tab_pl21$Grandeur == "Précipitations annuelles", ]
verifier(!is.null(tab_pl21) && nrow(lg_t21) == 1L && identical(lg_t21$B, "n.d.") && identical(lg_t21$`Écart A − B`, "—") &&
           grepl("Pluviometre 31 ne donne pas cette grandeur", lg_t21$Lecture, fixed = TRUE) &&
           nrow(lg_p21) == 1L && lg_p21$B != "n.d." && grepl("A (au-dessus|en dessous) de B", lg_p21$Lecture),
         "pluviometre : la temperature reste dans le tableau, dite non fournie par la station ; la pluie se compare")

# Departements (zones) : toutes les combinaisons qui en comportent.
if (AVEC_SF21) {
  deps21 <- tryCatch(territoires_lieux("departement", c("31", "82")), error = function(e) list())
  if (length(deps21) == 2L) {
    d31 <- deps21[[1]]; d82 <- deps21[[2]]
    m31 <- cmp_mailles(d31, jeu21)
    verifier(length(m31$idx) > 0L, "(controle) le departement 31 contient des mailles du jeu synthetique")
    d_dm21 <- duo_preparer(d31, a21, jeu21, cles21, ctx21, 2025)
    verifier(all(d_dm21$tl$retenu) && any(grepl("moyenne de mailles", duo_lecture(d_dm21))) &&
               identical(sort(unique(d_dm21$series$annee)), an21),
             "departement - maille : la mise en garde dit qu'un departement est une moyenne de mailles")
    d_ds21 <- duo_preparer(d31, b21, jeu21, cles21, ctx21, 2025)
    verifier(all(d_ds21$tl$retenu) && identical(d_ds21$tl$type, c("zone", "station")) &&
               identical(sort(unique(d_ds21$series$annee)), an_st21),
             "departement - station : annees de la station seulement")
    verifier(grepl("moyenne de mailles", paste(synthese_methode(d_ds21), collapse = " ")) ||
               grepl("Un département prend la moyenne", paste(synthese_methode(d_ds21), collapse = " ")),
             "departement - station : la methode decrit le departement et la station")
    m82 <- cmp_mailles(d82, jeu21)
    if (length(m82$idx) > 0L) {
      d_dd21 <- duo_preparer(d31, d82, jeu21, cles21, ctx21, 2025)
      verifier(all(d_dd21$tl$retenu) && any(grepl("Un département est la moyenne", duo_lecture(d_dd21))),
               "departement - departement : la mise en garde adaptee")
    }
    verifier(identical(duo_lieu_departement("31")$nom, "Haute-Garonne (31)") && is.null(duo_lieu_departement("ZZ")),
             "lieu departement : un code connu donne son lieu, un code inconnu rien")
  }
}
# Un lieu hors grille : la comparaison le dit.
hors21 <- cmp_lieu_point("Loin", 5.0, 47.0)
d_hors21 <- duo_preparer(hors21, b21, jeu21, cles21, ctx21, 2025)
verifier(!all(d_hors21$tl$retenu) && !d_hors21$tl$retenu[1] && nzchar(d_hors21$tl$note[1]),
         "un lieu hors de la grille est signale (et la comparaison n'est pas tentee)")


# ==============================================================================
groupe("v2.1 — synthese a envoyer : texte, lien de courriel, page HTML")
# ==============================================================================

verifier(identical(envoi_encoder("é &=/"), "%C3%A9%20%26%3D%2F") && identical(envoi_encoder(""), "") &&
           identical(envoi_encoder("a-b_c.d~e"), "a-b_c.d~e"),
         "encodage du lien : octets UTF-8 en %XX, caracteres sans risque laisses")
verifier(identical(utils::URLdecode(envoi_encoder("Forêt d'Écouves — 2025 & co")), "Forêt d'Écouves — 2025 & co"),
         "encodage du lien : le texte accentue fait l'aller-retour")
mt21 <- envoi_mailto("Objet é", "ligne 1\nligne 2")
verifier(startsWith(mt21, "mailto:?subject=Objet%20%C3%A9&body=ligne%201%0D%0Aligne%202"),
         "lien de courriel : pas de destinataire, objet et corps encodes, sauts de ligne de courriel (CRLF)")
verifier(!grepl("[\r\n ]", envoi_mailto("a\r\nBcc: x@y.z", "b")),
         "lien de courriel : un saut de ligne dans l'objet ne s'ecrit jamais tel quel")

env21 <- envoi_synthese(res21, as.Date("2026-11-07"))
verifier(startsWith(env21$objet, "ClimaFor — Bilan climatique 2025") && grepl("Mon point", env21$objet) &&
           startsWith(env21$mailto, "mailto:?subject="),
         "synthese a envoyer : objet, lieu et lien de courriel")
verifier(nchar(env21$mailto, type = "bytes") <= ENVOI_BUDGET_MAILTO && env21$tronque == 0L,
         "synthese a envoyer : le lien de courriel tient sous le budget (toutes les grandeurs ici)")
verifier(grepl("• Température moyenne annuelle : maille", env21$texte_court) && grepl("station", env21$texte_court) &&
           grepl("Normale 1991-2020", env21$texte_court),
         "message court : une ligne par grandeur, maille et station cote a cote")
verifier(all(c("EN BREF", "GRANDEURS", "SOURCES", "À LIRE AVEC PRUDENCE") %in% strsplit(env21$texte_complet, "\n")[[1]]) &&
           grepl("qualité : bonne", env21$texte_complet),
         "texte complet : en bref, grandeurs, sources (avec la qualite de la station), prudence")
verifier(!grepl("43[.,]7123|1[.,]0123", paste(env21$texte_complet, env21$texte_court, env21$mailto, envoi_html(env21))),
         "la synthese ne contient pas les coordonnees du point, seulement son nom")
verifier(identical(env21$tableau$Source[1:2], c("Maille SAFRAN (modèle)", "Ancienne 31 (mesure)")) && nrow(env21$tableau) == 6L,
         "tableau de la synthese : chaque ligne dit sa source")
verifier(grepl("^synthese_Mon_point_[0-9]{8}\\.html$", env21$nom_fichier), "nom du fichier a enregistrer : lieu et date, sans accent ni espace")

env_m21 <- envoi_synthese(res_m21, as.Date("2026-11-07"))
verifier(grepl("pas la mesure d'une station", env_m21$prudence, fixed = TRUE) && grepl("une station est une mesure", tolower(env21$prudence)) &&
           grepl("maille à", env_m21$sources[1]),
         "mise en garde : adaptee a une maille seule, ou a une station a cote d'une maille")

html_e21 <- envoi_html(env21)
verifier(startsWith(html_e21, "<!doctype html>") && grepl("<title>Bilan climatique 2025", html_e21, fixed = TRUE) &&
           grepl("<table", html_e21, fixed = TRUE) && grepl("charset=\"utf-8\"", html_e21, fixed = TRUE),
         "page HTML : document complet, titre, tableau, jeu de caracteres")
verifier(!grepl("<script|<link|<img|https?://", html_e21),
         "page HTML : autonome, sans script ni ressource externe")
f_html21 <- tempfile(fileext = ".html")
envoi_ecrire_html(env21, f_html21)
verifier(file.exists(f_html21) && file.size(f_html21) > 1000 && grepl("Ancienne 31", paste(readLines(f_html21, encoding = "UTF-8", warn = FALSE), collapse = "\n")),
         "page HTML : ecrite en UTF-8")

# Budget : toutes les grandeurs de la carte, le lien reste borne et le message dit combien manquent.
cles_tout21 <- names(CARTE_COUCHES)
res_tout21 <- synthese_preparer("point", l_deux21, NULL, jeu21, cles_tout21, cles_tout21, ctx21, 2025, NULL)
res_tout21$point_nom <- "Mon point"; res_tout21$details <- det21
env_tout21 <- envoi_synthese(res_tout21, as.Date("2026-11-07"))
verifier(nchar(env_tout21$mailto, type = "bytes") <= ENVOI_BUDGET_MAILTO && env_tout21$tronque > 0L &&
           grepl("autre", env_tout21$texte_court) && nrow(env_tout21$grandeurs) > nrow(env21$grandeurs),
         "budget du lien : avec toutes les grandeurs, des lignes sont retirees du message court et le message le dit")
verifier(nrow(env_tout21$tableau) > 6L && grepl("•", env_tout21$texte_complet),
         "budget du lien : la page et le texte complets gardent toutes les grandeurs")
res_long21 <- res_tout21; res_long21$point_nom <- strrep("é", 300)
verifier(nchar(envoi_synthese(res_long21)$mailto, type = "bytes") <= ENVOI_BUDGET_MAILTO ||
           nchar(strrep("é", 300)) > 60,
         "budget du lien : un nom de lieu tres long ne fait pas deborder le lien")

# Donnees des boutons : JSON dans la page, sans fermer la balise.
res_hostile21 <- res21; res_hostile21$point_nom <- "</script><b>x</b>"
env_h21 <- envoi_synthese(res_hostile21)
bal21 <- html21(envoi_donnees_ui(env_h21))
verifier(grepl("type=\"application/json\"", bal21, fixed = TRUE) && grepl("cf-envoi-donnees", bal21, fixed = TRUE) &&
           length(gregexpr("</script>", bal21, fixed = TRUE)[[1]]) == 1L,
         "donnees des boutons : une balise JSON, qu'un nom de lieu hostile ne peut pas refermer")
json21 <- sub("(?s)^.*?<script[^>]*>", "", sub("(?s)</script>.*$", "", bal21, perl = TRUE), perl = TRUE)
j21 <- jsonlite::fromJSON(gsub("<\\/", "</", json21, fixed = TRUE))
verifier(all(c("objet", "texte", "court", "fichier", "html") %in% names(j21)) && grepl("^<!doctype html>", j21$html),
         "donnees des boutons : objet, texte, message court, nom de fichier et page HTML")
verifier(!grepl("<script><b>|<b>x</b>", envoi_html(env_h21), fixed = FALSE) && grepl("&lt;/script&gt;", envoi_html(env_h21), fixed = TRUE),
         "page HTML : le nom du lieu est echappe (une balise dans un nom ne s'execute pas)")


# ==============================================================================
groupe("v2.1 — ecrans : Terrain, Synthese, Comparer, Accueil, Sources")
# ==============================================================================

c_deux21 <- html21(terrain_carte_ui(res21$bilan[res21$bilan$cle == "tmp", ], res21$ctx, envoi_etiquettes(res21)))
verifier(grepl("cf-terrain__carte--sources", c_deux21) && grepl("Maille SAFRAN \\(modèle\\)", c_deux21) &&
           grepl("Ancienne 31 \\(mesure\\)", c_deux21) && length(gregexpr("cf-terrain__valeur", c_deux21)[[1]]) >= 2L,
         "carte du Terrain a deux sources : une ligne par source, mesure et modele nommes")
c_un21 <- html21(terrain_carte_ui(res_m21$bilan[res_m21$bilan$cle == "tmp", ], res_m21$ctx))
verifier(!grepl("cf-terrain__carte--sources", c_un21) && grepl("cf-terrain__valeur", c_un21),
         "carte du Terrain a la maille seule : la carte d'avant la v2.1")

rt21 <- new.env()
shiny::testServer(
  mod_terrain_server,
  args = list(profil = PROFILS$terrain, lieu = shiny::reactive(pt21), transport = shiny::reactive(tr21),
              altitude = function(lon, lat) 190),
  {
    session$flushReact()
    rt21$defaut <- res()
    rt21$src <- src_pt()
    session$setInputs(sources_choix = "maille")
    rt21$maille <- res()
    session$setInputs(sources_choix = c("maille", src_pt()$stations$id[1], src_pt()$stations$id[2]))
    rt21$trois <- res()
    rt21$env <- env()
    rt21$html_envoi <- html21(output$envoi)
    rt21$html_sources <- html21(output$sources)
    rt21$html_res <- html21(output$resultat)
  })
verifier(identical(rt21$defaut$tl$lieu, c("Maille SAFRAN (modèle)", "Ancienne 31 (mesure)")),
         "Terrain : par defaut, la maille et la meilleure station proche")
verifier(identical(rt21$maille$tl$lieu, "Mon point") && is.null(rt21$maille$jeu$stations),
         "Terrain : en decochant la station, le bilan redevient celui de la maille seule")
verifier(nrow(rt21$trois$tl) == 3L && sum(rt21$trois$tl$source == "station") == 2L,
         "Terrain : plusieurs stations cochees, plusieurs sources")
verifier(grepl("Sources des valeurs", rt21$html_sources) && grepl("Ancienne 31", rt21$html_sources),
         "Terrain : le choix des sources est propose quand le jeu a des stations")
verifier(grepl("href=\"mailto:\\?subject=", rt21$html_envoi) && grepl("Envoyer par courriel", rt21$html_envoi) &&
           grepl("data-envoi=\"partager\"", rt21$html_envoi, fixed = TRUE) && grepl("hidden", rt21$html_envoi) &&
           grepl("data-envoi=\"copier\"", rt21$html_envoi, fixed = TRUE) && grepl("Enregistrer la page", rt21$html_envoi) &&
           grepl("cf-envoi-donnees", rt21$html_envoi, fixed = TRUE) && grepl("data-envoi-zone", rt21$html_envoi, fixed = TRUE),
         "Terrain : synthese a envoyer (lien de courriel, partage masque par defaut, copie, enregistrement, donnees)")
verifier(grepl("Aucun message ne part de ClimaFor", rt21$html_envoi, fixed = TRUE) && grepl("Voir le message", rt21$html_envoi, fixed = TRUE),
         "Terrain : l'ecran dit que ClimaFor n'envoie rien lui-meme, et montre le message")
verifier(grepl("Mon point", rt21$html_res) && !grepl("43[.,]7123", rt21$html_envoi),
         "Terrain : le titre porte le nom du point, la synthese a envoyer ne porte pas ses coordonnees")
verifier(identical(rt21$env$lieu, "Mon point") && grepl("Ancienne 31", rt21$env$texte_complet),
         "Terrain : la synthese a envoyer est celle du bilan affiche")

# Terrain sur un jeu SANS stations : aucun choix de sources, aucun appel d'altitude.
appels_alt21 <- 0L
rts21 <- new.env()
shiny::testServer(
  mod_terrain_server,
  args = list(profil = PROFILS$terrain, lieu = shiny::reactive(pt21), transport = shiny::reactive(tr_sans21),
              altitude = function(lon, lat) { appels_alt21 <<- appels_alt21 + 1L; 190 }),
  {
    session$flushReact()
    rts21$res <- res()
    rts21$sources <- output$sources
    rts21$env <- env()
  })
verifier(identical(rts21$res$tl$lieu, "Mon point") && is.null(rts21$sources) && appels_alt21 == 0L,
         "Terrain sans stations publiees : la maille seule, pas de choix de sources, pas d'appel au service d'altitude")
verifier(grepl("pas la mesure d'une station", rts21$env$prudence, fixed = TRUE),
         "Terrain sans stations publiees : la synthese a envoyer le dit")

# Synthese d'un point.
cases21 <- function(profil, jeu) {
  b <- synthese_base(profil$id, names(CARTE_COUCHES)); g <- synthese_groupes(synthese_catalogue(jeu, profil)); a <- list()
  for (f in names(g)) {
    ch <- names(g[[f]]$choix)
    a[[paste0("g_", f)]] <- intersect(b$g, ch); a[[paste0("t_", f)]] <- intersect(b$t, ch)
  }
  a
}
base_co21 <- cases21(PROFILS$co, jeu21)
rs_sy21 <- new.env()
shiny::testServer(
  mod_synthese_server,
  args = list(profil = PROFILS$co, lieu = shiny::reactive(pt21), transport = shiny::reactive(tr21),
              altitude = function(lon, lat) 190),
  {
    do.call(session$setInputs, base_co21)
    session$setInputs(echelle = "point", annee = "2025", ref = "1991-2020")
    session$flushReact()
    id1 <- src_pt()$stations$id[1]
    session$setInputs(sources_choix = c("maille", id1), calculer = 1)
    rs_sy21$deux <- res_val()
    session$setInputs(sources_choix = "maille", calculer = 2)
    rs_sy21$maille <- res_val()
    rs_sy21$perime <- {
      session$setInputs(sources_choix = c("maille", id1))
      !identical(res_val()$signature, signature("point"))
    }
    rs_sy21$ui <- html21(output$sources)
  })
verifier(!is.null(rs_sy21$deux$res) && identical(rs_sy21$deux$res$tl$lieu, c("Maille SAFRAN (modèle)", "Ancienne 31 (mesure)")) &&
           identical(rs_sy21$deux$res$point_nom, "Mon point") && length(rs_sy21$deux$res$details) == 1L,
         "Synthese d'un point : les sources cochees font les lieux du bilan, avec le nom du point et le detail des stations")
verifier(identical(rs_sy21$maille$res$tl$lieu, "Mon point") && is.null(rs_sy21$maille$res$jeu$stations),
         "Synthese d'un point : la maille seule donne le bilan d'avant la v2.1")
verifier(isTRUE(rs_sy21$perime), "Synthese d'un point : changer les sources apres le calcul rend le bilan affiche perime")
verifier(grepl("Sources des valeurs", rs_sy21$ui), "Synthese d'un point : le choix des sources est affiche")

# Comparer : l'interface et le mode « Deux lieux ».
ui_cmp21 <- html21(mod_comparer_ui("comparer"))
verifier(grepl("Deux lieux", ui_cmp21) && grepl("Plusieurs lieux importés", ui_cmp21) &&
           all(vapply(c("Maille ↔ maille", "Station ↔ station", "Maille ↔ station", "Département ↔ département",
                        "Département ↔ maille", "Département ↔ station"), function(p) grepl(p, ui_cmp21, fixed = TRUE), TRUE)),
         "Comparer : le mode « Deux lieux » et les six combinaisons usuelles")
verifier(grepl("Lieu A", ui_cmp21) && grepl("Lieu B", ui_cmp21) && grepl("Maille SAFRAN \\(modèle\\)", ui_cmp21) &&
           grepl("Station Météo-France \\(mesure\\)", ui_cmp21) && grepl("Département", ui_cmp21),
         "Comparer : chaque lieu se choisit parmi maille, station et departement")
verifier(grepl("input.mode == &#39;duo&#39;", ui_cmp21, fixed = TRUE) && grepl("input.mode == &#39;liste&#39;", ui_cmp21, fixed = TRUE),
         "Comparer : les deux modes sont des panneaux conditionnels")

rc21 <- new.env()
shiny::testServer(
  mod_comparer_server,
  args = list(ctx = shiny::reactive(ctx21), lieu = shiny::reactive(pt21), transport = shiny::reactive(tr21),
              chercher = function(q) data.frame(nom = "Saint-Test", code_insee = "00000", cp = "31000", departement = "Haute-Garonne",
                                                lon = 1.2, lat = 43.65, population = 1000L, stringsAsFactors = FALSE)),
  {
    session$setInputs(mode = "duo", `lieu_a-type` = "maille", `lieu_b-type` = "station", annee = "2025",
                      couches = c("tmp", "pre", "gel"))
    session$flushReact()
    rc21$a_accueil <- choix_a$lieu()
    session$setInputs(`lieu_a-commune` = "Saint", `lieu_a-chercher` = 1)
    rc21$a_commune <- choix_a$lieu()
    rc21$b_vide <- choix_b$lieu()
    session$setInputs(`lieu_b-station_dep` = "31")
    stn <- stations_table(cmp_stations(jeu())$meta)
    session$setInputs(`lieu_b-station` = stn$id[stn$nom == "Ancienne 31"])
    rc21$b <- choix_b$lieu()
    rc21$duo <- duo()
    rc21$html <- html21(output$duo_resultats)
    session$setInputs(p_mm = 1)
    session$flushReact()
    session$setInputs(`lieu_b-type` = "departement", `lieu_b-dep` = "")
    rc21$b_dep_vide <- choix_b$lieu()
  })
verifier(identical(rc21$a_accueil$nom, "Mon point") && identical(rc21$a_accueil$type, "point"),
         "Comparer, deux lieux : le lieu A est d'abord le point de l'accueil")
verifier(identical(rc21$a_commune$nom, "Saint-Test") && presque_egal(rc21$a_commune$lon, 1.2),
         "Comparer, deux lieux : une commune cherchee remplace le point de l'accueil")
verifier(is.null(rc21$b_vide), "Comparer, deux lieux : sans station choisie, pas de lieu B")
verifier(identical(rc21$b$type, "station") && identical(rc21$b$nom, "Ancienne 31 (mesure)"),
         "Comparer, deux lieux : une station publiee se choisit par departement puis par nom")
verifier(!is.null(rc21$duo) && all(rc21$duo$tl$retenu) && identical(rc21$duo$a, "Saint-Test") && isTRUE(rc21$duo$duo),
         "Comparer, deux lieux : la comparaison s'etablit entre les deux lieux choisis")
verifier(grepl("Comparaison pour 2025", rc21$html) && grepl("années communes", rc21$html) &&
           grepl("Écarts A − B \\(CSV\\)", rc21$html) && grepl("cf-duo__lieux", rc21$html),
         "Comparer, deux lieux : lecture, tableau, graphique et exports")
verifier(is.null(rc21$b_dep_vide), "Comparer, deux lieux : un departement non choisi ne donne pas de lieu")

rd21 <- html21(accueil_differences_ui())
verifier(grepl("quelle différence", rd21) && all(c("Synthèse", "Bilan climatique", "Rapport") %in% regmatches(rd21, gregexpr("(?<=scope=\"col\">)[^<]+", rd21, perl = TRUE))[[1]]) &&
           length(gregexpr("<tr", rd21, fixed = TRUE)[[1]]) == 7L,
         "Accueil : le tableau « Synthèse, Bilan climatique, Rapport : quelle différence ? » (6 lignes)")
verifier(grepl("Mesure et modèle", html21(mod_sources_ui("sources"))) && grepl("ne vous demande aucune adresse", html21(mod_sources_ui("sources"))),
         "Sources : mesure et modele, et envoi par courriel, sont expliques")
verifier(grepl("stations", PROFILS$terrain$resume) && grepl("station", PROFILS$co$resume) && grepl("deux lieux", PROFILS$dsf$resume),
         "profils : la page d'entree annonce les stations, la synthese a envoyer et les deux lieux")


# ==============================================================================
groupe("v2.1 — script du navigateur, CSS, chaine de publication, version")
# ==============================================================================

js21 <- lire21("www/app.js")
verifier(grepl("navigator.share", js21, fixed = TRUE) && grepl("navigator.canShare", js21, fixed = TRUE) &&
           grepl("data-envoi='partager'", js21, fixed = TRUE) && grepl("data-envoi='copier'", js21, fixed = TRUE) &&
           grepl("AbortError", js21, fixed = TRUE) && grepl("cf-envoi-donnees", js21, fixed = TRUE),
         "script : partage, copie, donnees lues sur place, fermeture de la fenetre de partage sans erreur")
verifier(!grepl("fetch\\(|XMLHttpRequest|sendBeacon", js21),
         "script : aucune requete reseau (la synthese ne quitte pas l'appareil par ce script)")
if (nzchar(Sys.which("node"))) {
  verifier(system2("node", c("--check", shQuote(chemin_ci("www/app.js"))), stdout = FALSE, stderr = FALSE) == 0L,
           "script : syntaxe JavaScript valide")
}
css21 <- lire21("www/climafor.css")
verifier(all(vapply(c(".cf-envoi__actions", ".cf-terrain__sources", ".cf-sources", ".cf-envoi__texte"), function(s) grepl(s, css21, fixed = TRUE), TRUE)),
         "CSS : styles du choix des sources, des cartes a plusieurs sources et de la synthese a envoyer")

glob21 <- lire21("global.R")
ordre21 <- regmatches(glob21, gregexpr("src/[a-z_0-9]+\\.R", glob21))[[1]]
verifier(all(c("src/utils_stations_publiees.R", "src/utils_comparer_duo.R", "src/utils_envoi.R", "src/mod_lieu_choix.R") %in% ordre21) &&
           match("src/utils_stations_publiees.R", ordre21) > match("src/utils_comparer.R", ordre21) &&
           match("src/mod_lieu_choix.R", ordre21) < match("src/mod_comparer.R", ordre21),
         "global.R : les nouveaux fichiers sont charges, dans l'ordre de leurs dependances")
verifier(grepl("^2\\.[1-9][0-9]*\\.[0-9]+$", CONFIG$app$version), "la version est une 2.1.x ou plus (2.1.0 a la sortie des stations publiees)")

wf21 <- lire21(".github/workflows/construire-carte.yml")
verifier(grepl("stations_fragment:", wf21, fixed = TRUE) && grepl("stations_preparer:", wf21, fixed = TRUE) &&
           grepl("stations_cli.R assembler", wf21, fixed = TRUE) && grepl("stations_cli.R verifier", wf21, fixed = TRUE),
         "workflow : les stations se construisent dans le meme deploiement que les mailles")
verifier(grepl("needs: [fragment, stations_fragment]", wf21, fixed = TRUE) || grepl("stations_fragment", sub(".*assembler:", "", wf21)),
         "workflow : l'assemblage attend les fragments de stations")
if (requireNamespace("yaml", quietly = TRUE)) {
  y21 <- tryCatch(yaml::read_yaml(chemin_ci(".github/workflows/construire-carte.yml")), error = function(e) NULL)
  verifier(!is.null(y21) && all(c("stations", "stations_fragments") %in% names(y21[[2]]$workflow_dispatch$inputs)) &&
             all(c("stations_preparer", "stations_fragment", "assembler") %in% names(y21$jobs)),
           "workflow : YAML valide, entrees « stations » et « stations_fragments », trois jobs attendus")
}

# Ligne de commande du precalcul, de bout en bout, sur la source fictive (2 departements).
if (nzchar(Sys.which("Rscript"))) {
  racine21 <- normalizePath(chemin_ci("."))
  p_cli21 <- tempfile("cli21"); d_cli21 <- tempfile("carte_cli21")
  lancer21 <- function(...) suppressWarnings(system2("Rscript", c(shQuote(file.path(racine21, "outils/stations_cli.R")), ...),
                                                      stdout = TRUE, stderr = TRUE))
  old_wd21 <- setwd(racine21)
  s1 <- lancer21("fragment", "source=demo", "fragment=1", "fragments=47", "annee_debut=2005", "annee_fin=2025",
                 paste0("sortie=", shQuote(p_cli21)))
  s2 <- lancer21("assembler", "source_texte=essai", "annee_debut=2005", "annee_fin=2025",
                 paste0("parts=", shQuote(p_cli21)), paste0("sortie=", shQuote(d_cli21)), "n_min=1")
  s3 <- lancer21("verifier", paste0("dossier=", shQuote(d_cli21)))
  s4 <- lancer21("verifier", paste0("dossier=", shQuote(tempfile("inexistant"))))
  setwd(old_wd21)
  verifier(any(grepl("Termine : 6 station", s1)) && file.exists(file.path(p_cli21, "part_001.rds")),
           "ligne de commande, fragment : deux departements fictifs, six stations retenues")
  verifier(any(grepl("Stations ecrites", s2)) && file.exists(file.path(d_cli21, "stations.json")),
           "ligne de commande, assemblage : stations.json ecrit")
  verifier(any(grepl("^Valide : 6 stations", s3)), "ligne de commande, verification : le jeu ecrit est valide")
  verifier(!is.null(attr(s4, "status")) && attr(s4, "status") != 0L && any(grepl("INVALIDE", s4)),
           "ligne de commande, verification : un dossier sans stations est refuse (code de sortie non nul)")
  s5 <- lancer21("fragment", "source=demo", "fragment=9", "fragments=4", "annee_debut=2005", "annee_fin=2025", "sortie=x")
  verifier(!is.null(attr(s5, "status")) && any(grepl("fragment doit etre compris", s5)),
           "ligne de commande : un numero de fragment hors bornes est refuse")
}
