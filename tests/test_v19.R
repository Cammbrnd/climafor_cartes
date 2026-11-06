# ==============================================================================
# tests/test_v19.R — VAGUES DE CHALEUR, EXPORT ET COMPARAISON (VERSION 1.9)
# ------------------------------------------------------------------------------
# Ce que ces tests gardent :
#   1. le decompte des vagues, sur des series dont le resultat se calcule a la main
#      (bornes, jours manquants, saison, reglages) ;
#   2. le raccord au reste de l'application : registre, parametres, references,
#      glossaire, couches de la carte, ligne du bilan.
# ==============================================================================

# Serie de reference : T max = 15 degC partout, donc normale du jour = 15.
serie19 <- function(annees = 1991:2022) {
  serie_synthetique(annees, t_moy = 10, p_jour = 0, amplitude = 5)
}
# Pose `n` jours a T max = 15 + `plus`, a partir de `debut`.
poser19 <- function(d, debut, n, plus) {
  i <- which(d$date == as.Date(debut))
  d$t_max[i:(i + n - 1)] <- 15 + plus
  d
}
ctx19 <- function(...) ctx_creer(ref = c(1991, 2020), courante = 2022, ...)


# ==============================================================================
groupe("v1.9 — vagues de chaleur : decompte")
# ==============================================================================

d19 <- serie19()
d19 <- poser19(d19, "2021-07-10", 6, 6)                       # 6 jours : une vague
d19 <- poser19(d19, "2021-08-01", 4, 8)                       # 4 jours : pas une vague
d19 <- poser19(d19, "2021-08-10", 3, 6)                       # 3 + un jour froid + 3 : aucune
d19 <- poser19(d19, "2021-08-14", 3, 6)
d19 <- poser19(d19, "2022-06-01", 5, 5)                       # 5 jours pile a +5 : une vague
d19 <- poser19(d19, "2022-07-01", 5, 7)                       # un jour SANS MESURE le 3 : 2 + 2
d19$t_max[d19$date == as.Date("2022-07-03")] <- NA
d19 <- poser19(d19, "2022-07-20", 8, 5.5)                     # 8 jours, un jour froid, 5 jours
d19 <- poser19(d19, "2022-07-29", 5, 5.5)
d19 <- poser19(d19, "2022-01-05", 7, 9)                       # en hiver : hors saison
v19 <- calc_vagues(d19, ctx19())
ligne19 <- function(a) v19[v19$annee == a, ]

verifier(ligne19(2000)$n_vagues == 0 && ligne19(2000)$n_jours_vague == 0 &&
           ligne19(2000)$duree_max == 0 && is.na(ligne19(2000)$ecart_max),
         "une annee sans vague compte zero vague et zero jour, et n'a pas d'ecart (NA, pas 0)")
verifier(ligne19(2021)$n_vagues == 1 && ligne19(2021)$n_jours_vague == 6 &&
           ligne19(2021)$duree_max == 6,
         "six jours a +6 degC : une vague ; quatre jours, ou 3 + 3 separes par un jour froid : aucune")
verifier(ligne19(2022)$n_vagues == 3 && ligne19(2022)$n_jours_vague == 18 &&
           ligne19(2022)$duree_max == 8,
         "2022 : cinq jours a +5 (borne incluse), huit jours, cinq jours ; le jour sans mesure et l'hiver ne comptent pas")
verifier(presque_egal(ligne19(2022)$ecart_max, 5.5) && presque_egal(ligne19(2021)$ecart_max, 6),
         "l'ecart maximal a la normale du jour est celui du jour le plus chaud d'une vague")

ep19 <- vagues_episodes(vagues_detecter(d19, ctx19()))
verifier(nrow(ep19) == 4 && identical(ep19$duree, c(6L, 5L, 8L, 5L)) &&
           identical(format(ep19$debut), c("2021-07-10", "2022-06-01", "2022-07-20", "2022-07-29")) &&
           identical(format(ep19$fin), c("2021-07-15", "2022-06-05", "2022-07-27", "2022-08-02")),
         "les vagues sont rendues une a une, avec leur debut, leur fin et leur duree")

# Reglages : la meme serie, lue avec d'autres seuils.
s_court <- CONFIG$seuils; s_court$vague_duree <- 4
v4_19 <- calc_vagues(d19, ctx19(seuils = s_court))
verifier(v4_19$n_vagues[v4_19$annee == 2021] == 2,
         "quatre jours suffisent quand la duree requise est 4 : le bloc de quatre jours de 2021 compte aussi")
s_haut <- CONFIG$seuils; s_haut$vague_ecart <- 6.5
verifier(sum(calc_vagues(d19, ctx19(seuils = s_haut))$n_vagues) == 0,
         "avec +6,5 degC requis, plus aucune vague (+6 et +5,5 sont en dessous)")
verifier(identical(calc_vagues(d19, ctx_creer(ref = c(1991, 2020), courante = 2022,
                                              seuils = list(chaleur_t2 = 30)))$n_vagues,
                   v19$n_vagues),
         "un contexte sans les deux reglages de vague retombe sur les valeurs par defaut")

# Le seuil est RELATIF : un cycle saisonnier sans anomalie ne donne aucune vague.
saison19 <- serie_synthetique(1991:2022, t_moy = function(doy, a) 16 + 12 * sin(2 * pi * (doy - 110) / 365.25),
                              p_jour = 0, amplitude = 5)
verifier(sum(calc_vagues(saison19, ctx19())$n_vagues) == 0 &&
           any(saison19$t_max > 30),
         "un ete a plus de 30 degC chaque annee n'est pas une vague : l'ecart se mesure a la normale du jour")
# Une annee entiere a +6 degC : une seule vague, de mai a septembre (153 jours).
chaude19 <- saison19
chaude19$t_max[chaude19$annee == 2022] <- chaude19$t_max[chaude19$annee == 2022] + 6
vc19 <- calc_vagues(chaude19, ctx19())
verifier(vc19$n_vagues[vc19$annee == 2022] == 1 && vc19$duree_max[vc19$annee == 2022] == 153,
         "une annee a +6 degC toute entiere : une vague de 153 jours (mai a septembre), pas de vague d'hiver")

# La normale du jour : 366 valeurs, 29 fevrier compris, sans trou.
nj19 <- vague_normale_jour(d19, ctx19())
verifier(length(nj19) == 366 && !anyNA(nj19) && identical(names(nj19)[60], "02-29") &&
           presque_egal(unname(nj19), rep(15, 366)),
         "la normale du jour couvre les 366 jours du calendrier, 29 fevrier compris")

# Reference trop courte : pas de normale du jour, donc pas d'indicateur, et on dit pourquoi.
court19 <- calc_vagues(d19[d19$annee > 2015, ], ctx19())
verifier(!nrow(court19) && identical(attr(court19, "raison"), "reference_insuffisante") &&
           grepl("1991-2020", attr(court19, "message"), fixed = TRUE),
         "reference de moins de dix annees : pas de resultat, et un message qui nomme la reference")

# Aucune donnee : un tableau vide, sans erreur.
verifier(!nrow(calc_vagues(d19[0, ], ctx19())), "un jeu vide ne leve pas d'erreur")

# Annee en cours incomplete : signalee, et sans ecart a la normale.
cour19 <- d19[d19$date <= as.Date("2022-08-15"), ]
vcour <- calc_vagues(cour19, ctx19())
verifier(isTRUE(vcour$partielle[vcour$annee == 2022]) && is.na(vcour$anomalie[vcour$annee == 2022]) &&
           vcour$n_vagues[vcour$annee == 2022] == 3,
         "l'annee en cours est dite partielle, ses vagues deja survenues sont comptees")


# ==============================================================================
groupe("v1.9 — vagues de chaleur : raccord a l'application")
# ==============================================================================

ind19 <- INDICATEURS$vag
verifier(!is.null(ind19) && identical(ind19$famille, "thermique") && "vag" %in% ORDRE_INDICATEURS &&
           identical(ind19$requiert, "t_max"),
         "l'indicateur est au registre, dans l'ordre d'affichage, et exige la temperature maximale")
verifier(all(ind19$colonnes %in% names(v19)),
         "chaque colonne exportee existe dans le tableau annuel")
g19 <- ind19$plot(v19, ctx19(), mobile = FALSE)
verifier(inherits(g19, "ggplot") && "val_y" %in% names(g19$data),
         "le graphique se construit")
verifier(inherits(ind19$plot(data.frame(), ctx19(), mobile = FALSE), "ggplot"),
         "le graphique d'un jeu vide est un message, pas une erreur")
r19 <- commentaire_indicateur(ind19, v19, ctx19())
verifier(nzchar(r19) && grepl("vagues de chaleur en 2022", r19) && grepl("la plus longue a duré 8 jours", r19),
         "le commentaire dit le nombre de vagues, la plus longue et l'ecart maximal de l'annee comparee")
verifier(!"n_vagues" %in% names(calc_chaleur(d19, ctx19())) &&
           !"n_vagues" %in% INDICATEURS$can$colonnes,
         "l'ancien decompte (trois jours au-dessus de 30 degC) a quitte l'indicateur des jours de chaleur")

# Parametres, references, glossaire.
verifier(all(c("vague_ecart", "vague_duree") %in% DESCRIPTIF_SEUILS$id) &&
           all(c("vague_ecart", "vague_duree") %in% names(CONFIG$seuils)),
         "les deux reglages de la vague figurent dans la configuration et dans Parametres")
lg19 <- DESCRIPTIF_SEUILS[DESCRIPTIF_SEUILS$id %in% c("vague_ecart", "vague_duree"), ]
verifier(all(lg19$groupe == "Chaleur") && all(lg19$statut == "convention") &&
           all(grepl("frich2002", lg19$refs)) && "frich2002" %in% REFERENCES$id &&
           grepl("Climate Research", REFERENCES$citation[REFERENCES$id == "frich2002"], fixed = TRUE),
         "les deux reglages sont du groupe Chaleur, statut convention, et citent Frich et al. 2002")
verifier(!grepl("décompte des vagues", DESCRIPTIF_SEUILS$aide[DESCRIPTIF_SEUILS$id == "chaleur_t2"], fixed = TRUE),
         "le seuil de 30 degC ne se dit plus seuil des vagues")
verifier("Vague de chaleur" %in% GLOSSAIRE$terme &&
           !is.na(glossaire_definition("Vagues de chaleur")) &&
           grepl("HWDI", glossaire_definition("vague de chaleur"), fixed = TRUE),
         "le glossaire definit la vague de chaleur, et l'infobulle la repere dans un titre")
verifier(identical(CONFIG$methode$vague_mois, c(5L, 9L)),
         "la saison des vagues (mai a septembre) est une constante de methode, pas un reglage cache")

# Couches de la carte.
verifier(identical(CARTE_COUCHES$vag$source, "vag") && identical(CARTE_COUCHES$vag$colonne, "n_vagues") &&
           identical(CARTE_COUCHES$vagj$colonne, "n_jours_vague") &&
           all(c("n_vagues", "n_jours_vague", "normale") %in% names(v19)),
         "deux couches de carte s'appuient sur l'indicateur : nombre de vagues, jours en vague")

# Ligne du bilan chiffre : meme valeur que l'indicateur, a periode egale.
sy19 <- calc_synthese(d19, ctx19())
lv19 <- sy19[sy19$indicateur == "Vagues de chaleur", ]
verifier(nrow(lv19) == 1 && lv19$valeur == 3 && presque_egal(lv19$normale, 0) &&
           identical(lv19$famille, "thermique") && lv19$sens == "chaud",
         "le bilan chiffre porte une ligne « Vagues de chaleur » egale a celle de l'indicateur")
# Un jeu sans reference suffisante retire la ligne, sans faire echouer le bilan.
sy19c <- calc_synthese(d19[d19$annee > 2015, ], ctx19())
verifier(!"Vagues de chaleur" %in% sy19c$indicateur && nrow(sy19c) > 0,
         "reference trop courte : la ligne disparait, le reste du bilan demeure")


# ==============================================================================
groupe("v1.9 — export CSV du bilan")
# ==============================================================================

don_e <- serie_synthetique(1991:2022, t_moy = function(doy, a) 12 + 9 * sin(2 * pi * (doy - 110) / 365.25),
                           p_jour = 2)
ctx_e <- ctx_creer(ref = c(1991, 2020), courante = 2022)
res_e <- calculer_indicateurs(don_e, ctx_e)
lg_e <- bilan_series_longues(res_e)

verifier(identical(names(lg_e), c("indicateur_id", "indicateur", "grandeur", "annee", "valeur", "partielle")) &&
           nrow(lg_e) > 1000,
         "le tableau long a six colonnes et plus de mille lignes pour toutes les series")
verifier(!any(c("bh", "ombro", "tmp_mois", "tmp_jour", "pre_mois", "vpd_mois", "neige_mois", "vent_mois") %in%
                lg_e$indicateur_id),
         "les profils mensuels et journaliers, sans annee, n'y figurent pas")
verifier(all(c("tmp", "gel", "can", "vag", "pre", "petp", "vent") %in% lg_e$indicateur_id),
         "les series annuelles de tous les autres indicateurs y figurent, dont les vagues de chaleur")
verifier(is.numeric(lg_e$valeur) && !"classe" %in% lg_e$grandeur &&
           !any(c("n_jours", "dernier_jour", "ecart_type_ref", "annee", "partielle") %in% lg_e$grandeur),
         "seules les grandeurs numeriques utiles sont exportees (ni texte, ni colonnes de calcul)")
verifier(!anyDuplicated(lg_e[, c("indicateur_id", "grandeur", "annee")]),
         "une seule ligne par indicateur, grandeur et annee")
# Une valeur du tableau long est celle de l'indicateur.
verifier(presque_egal(lg_e$valeur[lg_e$indicateur_id == "gel" & lg_e$grandeur == "n_gel" & lg_e$annee == 2000],
                      res_e$gel$n_gel[res_e$gel$annee == 2000]) &&
           identical(lg_e$partielle[lg_e$indicateur_id == "tmp"][1], FALSE),
         "une valeur du tableau long est identique a celle du tableau de l'indicateur")
# Annee partielle : le drapeau suit.
part_e <- calculer_indicateurs(don_e[don_e$date <= as.Date("2022-06-30"), ], ctx_e)
lp_e <- bilan_series_longues(part_e)
verifier(all(lp_e$partielle[lp_e$annee == 2022 & lp_e$indicateur_id == "tmp"]) &&
           !any(lp_e$partielle[lp_e$annee == 2021]),
         "l'annee en cours incomplete porte partielle = TRUE, les autres FALSE")
verifier(!nrow(bilan_series_longues(list())) && !nrow(bilan_series_longues(list(tmp = data.frame()))),
         "aucun resultat : tableau vide, sans erreur")

sy_e <- bilan_synthese_export(calc_synthese(don_e, ctx_e))
verifier(identical(names(sy_e), c("indicateur", "unite", "valeur", "normale", "ecart",
                                  "ecart_type_reference", "annees_reference", "lecture")) &&
           "Vagues de chaleur" %in% sy_e$indicateur && all(sy_e$lecture %in% c("", "au-dessus", "en dessous", "proche de la normale")),
         "la synthese exportee reprend les lignes de l'ecran, avec une lecture en clair")
verifier(!nrow(bilan_synthese_export(data.frame())) && !nrow(bilan_synthese_export(NULL)),
         "une synthese vide s'exporte en tableau vide")

# Fichier : BOM, point-virgule, virgule decimale, provenance en tete, relu a l'identique.
f_e <- tempfile(fileext = ".csv")
entete_e <- lignes_metadonnees_csv(list(titre = "Series annuelles de tous les indicateurs", fenetre = FALSE),
                                   ctx_e, don = don_e, lieu = list(nom = "Forêt de Bouconne"), dat = NULL)
ecrire_csv_avec_metadonnees(lg_e, f_e, entete_e)
brut_e <- readBin(f_e, "raw", 3)
relu_e <- utils::read.csv2(f_e, comment.char = "#", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
txt_e <- readLines(f_e, warn = FALSE, encoding = "UTF-8")
verifier(all(as.integer(brut_e) == c(0xEF, 0xBB, 0xBF)) && nrow(relu_e) == nrow(lg_e) &&
           identical(names(relu_e), names(lg_e)) &&
           presque_egal(relu_e$valeur, lg_e$valeur, tolerance = 1e-6),
         "le CSV s'ouvre dans Excel (BOM, point-virgule) et se relit a l'identique")
verifier(any(grepl("^# .*Forêt de Bouconne", txt_e)) && any(grepl("^# .*Référence : 1991-2020", txt_e)) &&
           any(grepl("^# .*Licence ouverte", txt_e)),
         "le fichier porte son lieu, sa reference et sa licence en tete")

# Nom de fichier : sans accent, sans espace, avec la date.
nf_e <- nom_fichier_export("synthese", list(nom = "Forêt de Bouconne (Haute-Garonne)"))
verifier(grepl("^synthese_[A-Za-z0-9_]+_[0-9]{8}\\.csv$", nf_e) && grepl("Bouconne", nf_e),
         "le nom de fichier est sans accent ni espace et porte la date")
verifier(grepl("^synthese_point_[0-9]{8}\\.csv$", nom_fichier_export("synthese", NULL)) &&
           grepl("^synthese_point_", nom_fichier_export("synthese", list(nom = "???"))),
         "sans lieu exploitable, le nom retombe sur « point »")

# Interface : les deux boutons sont au module, et masques a l'impression.
src_bilan_e <- paste(readLines("src/mod_bilan.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
css_e <- paste(readLines("www/climafor.css", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
verifier(grepl('ns("csv_synthese")', src_bilan_e, fixed = TRUE) && grepl('ns("csv_series")', src_bilan_e, fixed = TRUE) &&
           grepl("\\.cf-export[^{]*\\{[^}]*display: none", gsub("\n", " ", css_e)),
         "les deux boutons d'export sont dans le bilan et ne s'impriment pas")
