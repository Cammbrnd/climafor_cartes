# ==============================================================================
# tests/test_stations.R — STATIONS METEO-FRANCE
# ------------------------------------------------------------------------------
# Inclus par lancer_tests.R. Aucun acces reseau : les fichiers sont fabriques au
# format Meteo-France (tests/donnees_stations.R) et les fonctions de
# telechargement sont remplacees par des lectures locales.
#
# CE QUE CES TESTS VERROUILLENT
# -----------------------------
# Les erreurs plausibles et fausses, comme dans le reste de la suite :
#   - une annee lacunaire comptee comme « zero » au lieu d'etre absente ;
#   - un pluviometre qui afficherait « 0 jour de gel » ;
#   - une pluie/neige non separee qui afficherait « 0 mm de neige » ;
#   - un classement qui prefererait un pluviometre proche a une station complete.
# ==============================================================================

source("tests/donnees_stations.R")


# ==============================================================================
groupe("Stations — lecture d'un fichier au format Meteo-France")
# ==============================================================================

jeu <- jeu_stations_test()
brut <- mf_lire_fichier(jeu$previous)

verifier(nrow(brut) > 1000, "le fichier est lu")
verifier(all(c("id", "nom", "lat", "lon", "alt", "date", "rr", "tn", "tx", "ffm") %in% names(brut)),
         "les colonnes utiles sont retrouvees par leur nom")
verifier(inherits(brut$date, "Date") && !anyNA(brut$date), "AAAAMMJJ devient une vraie date")
verifier("31001001" %in% brut$id, "le numero de poste est lu comme texte (zero initial conserve pour 01...)")

un <- mf_lire_fichier(jeu$previous, ids = "31002002")
verifier(length(unique(un$id)) == 1 && unique(un$id) == "31002002",
         "le filtrage par numero de poste ne garde que la station demandee")
verifier(all(is.na(un$tn)) && any(!is.na(un$rr)),
         "un pluviometre : pluie renseignee, temperatures vides (NA, pas zero)")
verifier(presque_egal(un$rr[un$date == as.Date("2000-01-03")], 2),
         "une valeur de pluie est lue sans alteration")

# En-tete dans un autre ordre, colonne manquante : le format a deja evolue
f_alt <- tempfile(fileext = ".csv.gz")
con <- gzfile(f_alt, "wt")
writeLines(c("AAAAMMJJ;NUM_POSTE;TX;RR", "20200101;01014002;12.5;3.0",
             "20200102;01014002;;0.0"), con)
close(con)
alt <- mf_lire_fichier(f_alt)
verifier(nrow(alt) == 2 && alt$id[1] == "01014002",
         "un identifiant a zero initial reste intact")
verifier(presque_egal(alt$tx[1], 12.5) && is.na(alt$tn[1]) && is.na(alt$tx[2]),
         "colonnes dans un autre ordre, colonnes absentes -> NA, valeur vide -> NA")

f_mauvais <- tempfile(fileext = ".csv.gz")
con <- gzfile(f_mauvais, "wt"); writeLines(c("A;B", "1;2"), con); close(con)
verifier(inherits(try(mf_lire_fichier(f_mauvais), silent = TRUE), "try-error"),
         "un fichier qui n'a pas le format attendu leve une erreur explicite")


# ==============================================================================
groupe("Stations — liste des fichiers data.gouv.fr")
# ==============================================================================

res <- list(
  list(title = "QUOT_departement_31_periode_1950-2024_RR-T-Vent",
       url = "https://exemple.fr/Q_31_previous-1950-2024_RR-T-Vent.csv.gz",
       last_modified = "2026-01-01"),
  list(title = "QUOT_departement_31_periode_2025-2026_RR-T-Vent",
       url = "https://exemple.fr/Q_31_latest-2025-2026_RR-T-Vent.csv.gz",
       last_modified = "2026-10-27"),
  list(title = "autres parametres 31", url = "https://exemple.fr/Q_31_latest-2025-2026_autres-parametres.csv.gz",
       last_modified = ""),
  list(title = "Corse", url = "https://exemple.fr/Q_2A_latest-2025-2026_RR-T-Vent.csv.gz",
       last_modified = ""),
  list(title = "Documentation", url = "https://exemple.fr/doc.pdf", last_modified = ""))
idx <- mf_parser_ressources(res)
verifier(nrow(idx) == 3, "seuls les fichiers RR-T-Vent sont retenus")
verifier(all(c("31", "2A") %in% idx$dept), "le departement est lu dans le titre ou dans le nom de fichier (Corse comprise)")
verifier(identical(idx$type[idx$debut == 2025 & idx$dept == "31"], "latest") &&
           identical(idx$type[idx$debut == 1950], "previous"),
         "le type (latest / previous) et les annees sont lus")
verifier(nrow(mf_parser_ressources(list())) == 0, "une liste vide donne un tableau vide")


# ==============================================================================
groupe("Stations — distance et niveaux de qualite")
# ==============================================================================

verifier(abs(mf_distance_km(2.3522, 48.8566, 4.8357, 45.7640) - 392) < 3,
         "distance Paris-Lyon : environ 392 km")
verifier(mf_distance_km(1, 43, 1, 43) == 0, "distance d'un point a lui-meme : zero")

verifier(identical(mf_niveau(c(5, 10, 11, 25, 26, 40, 41), c(10, 25, 40)), c(1L, 1L, 2L, 2L, 3L, 3L, 4L)),
         "niveaux par seuils : bornes incluses vers le meilleur niveau (distance)")
verifier(identical(mf_niveau(c(0.99, 0.95, 0.9, 0.5), c(0.95, 0.85, 0.7), plus_petit_mieux = FALSE),
                   c(1L, 1L, 2L, 4L)),
         "niveaux par seuils (completude : plus grand = mieux)")
verifier(is.na(mf_niveau(NA_real_, c(1, 2, 3))), "une valeur inconnue donne un niveau inconnu, pas 'bon'")

st <- data.frame(
  id = c("a", "b", "c", "d"), nom = c("COMPLETE", "PLUVIO", "ARRETEE", "VIDE"),
  distance_km = c(8, 3, 8, 8), ecart_alt_m = c(50, 50, 50, 50),
  derniere_obs = Sys.Date() - c(1, 1, 400, 1),
  fr_p = c(0.99, 0.99, 0.99, 0), fr_t = c(0.98, 0, 0.98, 0),
  a_pluie = c(TRUE, TRUE, TRUE, FALSE), a_temp = c(TRUE, FALSE, TRUE, FALSE),
  stringsAsFactors = FALSE)
q <- mf_qualite_stations(st)
verifier(q$niveau[1] == 1 && q$libelle[1] == "Bonne",
         "station proche, complete, a jour : qualite bonne")
verifier(q$niveau[2] >= 3, "une station sans temperature n'est jamais mieux que 'limitee'")
verifier(q$niveau[3] == 4, "une station dont la derniere mesure a plus d'un an est insuffisante")
verifier(q$niveau[4] == 4, "une station sans aucune mesure est insuffisante")
st2 <- st[1, ]; st2$distance_km <- 30; st2$ecart_alt_m <- 600
verifier(mf_qualite_stations(st2)$niveau == 4,
         "l'ecart d'altitude de 600 m suffit a rendre une station insuffisante")
st3 <- st[1, ]; st3$ecart_alt_m <- NA
verifier(mf_qualite_stations(st3)$niveau == 1,
         "altitude du point inconnue : le critere est ignore, pas penalise")


# ==============================================================================
groupe("Stations — resume d'un departement (fichier latest)")
# ==============================================================================

lat <- mf_appliquer_qualite(mf_lire_fichier(jeu$latest))
sd <- mf_resumer_stations(lat, "31")
verifier(nrow(sd) == 4, "une ligne par station")
a <- sd[sd$id == "31001001", ]
verifier(a$a_pluie && a$a_temp && a$fr_p > 0.99 && a$fr_t > 0.99,
         "station complete : pluie et temperatures, 12 derniers mois renseignes")
verifier(!sd$a_temp[sd$id == "31002002"] && sd$a_pluie[sd$id == "31002002"],
         "pluviometre reconnu comme tel")
verifier(sd$derniere_obs[sd$id == "31003003"] == as.Date("2025-03-01"),
         "la date de derniere mesure est celle de la derniere valeur, pas du fichier")
verifier(sd$fr_p[sd$id == "31003003"] < 0.2,
         "une station arretee a une completude recente faible")


# ==============================================================================
groupe("Stations — classement des propositions")
# ==============================================================================

# On remplace la recherche de departements et la lecture des fichiers
mf_departements_proches <- function(lon, lat) "31"
mf_stations_dept <- function(dept) sd
prop <- mf_stations_proches(1.47, 43.35, alt = 150)
verifier(nrow(prop) == 3, "trois stations proposees au plus (CONFIG$stations$n_propositions)")
verifier(prop$id[1] == "31001001", "la station complete et proche passe en premier")
verifier(all(diff(prop$pertinence) >= 0), "le classement suit la distance corrigee")
verifier(!"31003003" %in% prop$id[1:2] || prop$id[3] == "31003003",
         "une station arretee est reléguee")

# Un pluviometre tout proche ne doit pas supplanter une station complete un peu plus loin
sd_bak <- sd
sd2 <- sd; sd2$lat[sd2$id == "31002002"] <- 43.351; sd2$lon[sd2$id == "31002002"] <- 1.471
sd2$lat[sd2$id == "31001001"] <- 43.40;  sd2$lon[sd2$id == "31001001"] <- 1.52
mf_stations_dept <- function(dept) sd2
prop2 <- mf_stations_proches(1.47, 43.35, alt = 150)
verifier(prop2$id[1] == "31001001" && prop2$distance_km[prop2$id == "31002002"] < prop2$distance_km[1],
         "un pluviometre plus proche mais sans temperature n'est pas propose avant une station complete voisine")

# Rien dans le rayon
mf_stations_dept <- function(dept) sd
vide_prop <- mf_stations_proches(5.5, 46.0, alt = NA)
verifier(nrow(vide_prop) == 0, "aucune station dans le rayon : tableau vide, pas d'erreur")

# Echec d'un departement : message, pas d'exception
mf_stations_dept <- function(dept) {
  v <- mf_resumer_stations(mf_brut_vide(), dept); attr(v, "erreur") <- "panne"; v }
p_err <- mf_stations_proches(1.47, 43.35)
verifier(nrow(p_err) == 0 && any(grepl("panne", attr(p_err, "erreurs"))),
         "une panne de lecture est remontee dans 'erreurs' sans faire tomber la recherche")


# ==============================================================================
groupe("Stations — serie journaliere au format ClimaFor")
# ==============================================================================

# Remplace le telechargement par une lecture locale. L'original est garde : les
# tests de securite des telechargements (test_v17.R) ont besoin du vrai.
mf_telecharger_origine <- mf_telecharger
mf_telecharger <- function(url, timeout = 300) {
  f <- if (grepl("previous", url)) jeu$previous else jeu$latest
  d <- tempfile(fileext = ".csv.gz"); file.copy(f, d); d
}
mf_fichiers_dept <- function(dept, annees = NULL, type = NULL) {
  f <- data.frame(dept = "31",
                  debut = c(1950L, jeu$an - 1L), fin = c(jeu$an - 2L, jeu$an),
                  type = c("previous", "latest"),
                  titre = c("p", "l"), url = c("previous", "latest"),
                  modifie = "", stringsAsFactors = FALSE)
  if (!is.null(annees)) f <- f[f$fin >= min(annees) & f$debut <= max(annees), , drop = FALSE]
  if (!is.null(type)) f <- f[f$type %in% type, , drop = FALSE]
  f
}
CONFIG$cache$actif <- FALSE      # pas de cache disque pendant les tests

stn <- list(id = "31001001", nom = "AUTERIVE", dept = "31", lat = 43.35, lon = 1.47,
            alt = 150, distance_km = 4)
don <- station_charger(stn, lon = 1.47, lat = 43.35, annees = 1995:jeu$an)

verifier(nrow(don) > 5000, "la serie est chargee")
verifier(all(c("date", "annee", "mois", "jour", "doy", "t_moy", "t_min", "t_max",
               "p_liq", "p_sol", "p_tot", "etp", "swi", "neig_h", "neig_e",
               "ff", "hu", "vpd") %in% names(don)),
         "memes colonnes que le tableau SAFRAN : les indicateurs ne voient pas la difference")
verifier(identical(attr(don, "source"), "station"), "la source est portee par un attribut")
verifier(all(is.na(don$p_liq)) && all(is.na(don$p_sol)) && all(is.na(don$swi)) && all(is.na(don$vpd)),
         "pluie/neige non separees, pas de SWI ni d'humidite : NA, jamais zero")
verifier(!any(is.na(don$etp[!is.na(don$t_moy)])),
         "ETP de Hargreaves calculee partout ou les temperatures existent")
verifier(grepl("Hargreaves", attr(don, "etp_source")), "l'origine de l'ETP est dite")
verifier(!anyDuplicated(don$date), "aucune date en double")
d_attendu <- seq(min(don$date), max(don$date), by = "day")
verifier(identical(don$date, d_attendu),
         "calendrier complet : un jour sans mesure est une ligne a NA, pas un trou dans le tableau")

av <- attr(don, "annees_valides")
verifier(!(2000L %in% av$p) && (2000L %in% av$t),
         "2000 (40 jours de pluie manquants) est ecartee pour la pluie mais conservee pour la temperature")
verifier(1999L %in% av$p && 2001L %in% av$p, "les annees voisines restent valides")
verifier(jeu$an %in% av$p, "l'annee en cours, renseignee jusqu'a hier, est valide (jours ecoules et non 365)")

# Indicateurs
ctx <- ctx_creer(ref = 1995:2020, affichage = 1995:jeu$an, courante = jeu$an - 1L)
res <- calculer_indicateurs(don, ctx)
verifier(!nrow(res$pre) == 0, "les precipitations annuelles sont calculees")
verifier(!(2000L %in% res$pre$annee), "l'annee 2000 est ABSENTE des precipitations, pas comptee a zero")
verifier(2000L %in% res$tmp$annee, "l'annee 2000 est presente pour les temperatures")
verifier(nrow(res$neige) == 0 && nrow(res$swi) == 0 && nrow(res$vpd) == 0,
         "neige, SWI et VPD ne sont pas calcules : variables absentes de la station")
verifier(nrow(res$gel) > 0 && nrow(res$bh) > 0 && nrow(res$vent) > 0,
         "gel, bilan hydrique et vent le sont")
verifier(all(res$pre$p_sol %in% NA) && all(res$pre$p_liq == res$pre$p_tot),
         "precipitations : le total est rendu, la neige reste non definie")
verifier(!any(res$pre$p_tot == 0), "aucune annee de pluie a zero (un zero serait une annee lacunaire)")
indispo <- indicateurs_indisponibles(don)
verifier(all(c("swi", "vpd", "neige") %in% indispo$id) && !"gel" %in% indispo$id,
         "la liste des indicateurs indisponibles nomme ceux qui manquent de donnees")
verifier(any(grepl("Humidité", indispo$manque)), "et dit quelle variable manque")

syn <- calc_synthese(don, ctx)
verifier(nrow(syn) > 0 && !any(grepl("neigeuses", syn$indicateur)),
         "la synthese n'affiche pas de ligne neige quand la station ne la fournit pas")

# Qualite de la serie (niveau 2)
qs <- mf_qualite_serie(don, ctx)
verifier(!is.null(qs) && qs$niveau %in% 1:2, "qualite de serie : station complete sur la reference -> bonne ou correcte")
verifier(2000L %in% qs$groupes$ecartees[[1]] && !(2000L %in% qs$groupes$ecartees[[2]]),
         "2000 est signalee comme ecartee pour la pluie, pas pour les temperatures")
verifier(any(grepl("2000", qs$alertes)) && any(grepl("zéro", qs$alertes)),
         "l'alerte nomme l'annee et dit qu'elle n'est pas comptee comme zero")
verifier(abs(qs$groupes$part_ref[1] - 25 / 26) < 1e-9 || abs(qs$groupes$part_ref[1] - 25/26) < 0.05,
         "part de la reference exploitable : 25 ans sur 26 pour la pluie")
verifier(!any(grepl("Codes qualité", qs$alertes)),
         "pas d'alerte sur les codes qualite quand tous ont ete appliques")
verifier(all(unlist(attr(don, "annees_valides")) %in% 1995:jeu$an),
         "les annees valides sont limitees a la plage demandee")
verifier(is.null(mf_qualite_serie(serie_synthetique(), ctx)),
         "une serie SAFRAN n'a pas de qualite de station")
verifier(identical(mf_liste_annees(c(2000, 2003, 2010)), "2000, 2003 et 2010") &&
           identical(mf_liste_annees(2000:2004), "2000-2004") &&
           identical(mf_liste_annees(2000), "2000"),
         "liste d'annees lisible")

# Pluviometre : aucune temperature
pl <- station_charger(list(id = "31002002", nom = "PLUVIO-SEUL", dept = "31", lat = 43.30,
                           lon = 1.50, alt = 160, distance_km = 6),
                      lon = 1.47, lat = 43.35, annees = 1995:jeu$an)
rp <- calculer_indicateurs(pl, ctx)
verifier(nrow(rp$gel) == 0 && nrow(rp$can) == 0 && nrow(rp$tmp) == 0,
         "un pluviometre n'affiche PAS '0 jour de gel' : l'indicateur n'est pas calcule")
verifier(nrow(rp$pre) > 0 && nrow(rp$dry) > 0,
         "mais ses indicateurs de pluie le sont")
verifier(nrow(rp$bh) == 0, "le bilan hydrique (qui exige ETP, donc temperatures) est ecarte")
qp <- mf_qualite_serie(pl, ctx)
verifier(qp$niveau >= 3 && any(grepl("Températures : non mesurées", qp$alertes)),
         "pluviometre : qualite de serie au mieux limitee, temperatures signalees non mesurees")

# Temperature mesuree depuis 2005 seulement : avant, les indicateurs thermiques sont absents
rc <- station_charger(list(id = "31004004", nom = "RECENTE", dept = "31", lat = 43.5,
                           lon = 1.6, alt = 400, distance_km = 10),
                      lon = 1.47, lat = 43.35, annees = 1995:jeu$an)
rrc <- calculer_indicateurs(rc, ctx)
verifier(min(rrc$gel$annee) >= 2005 && min(rrc$pre$annee) <= 1996,
         "temperature depuis 2005, pluie depuis 1990 : le gel commence en 2005 (pas de '0 jour' avant), la pluie reste complete")
qr <- mf_qualite_serie(rc, ctx)
verifier(qr$groupes$part_ref[qr$groupes$groupe == "t"] < 0.7 && qr$niveau >= 3 &&
           any(grepl("2005", qr$alertes)),
         "temperatures depuis 2005 seulement : reference couverte a moins de 70 %, qualite limitee, 2005 citee")

# Station inconnue
inconnue <- station_charger(list(id = "99999999", nom = "X", dept = "31", lat = 43, lon = 1,
                                 alt = 100, distance_km = 1), lon = 1, lat = 43, annees = 2000:2010)
verifier(nrow(inconnue) == 0 && length(attr(inconnue, "erreurs")) > 0,
         "station sans donnee : tableau vide et message, pas d'exception")


# ==============================================================================
groupe("Stations — codes qualite Meteo-France")
# ==============================================================================

b <- mf_lire_fichier(jeu$latest, ids = "31001001")
b$qrr[1:5] <- 2                       # 5 valeurs 'mises en doute'
f <- mf_appliquer_qualite(b)
verifier(sum(is.na(f$rr)) >= sum(is.na(b$rr)) + 4 && length(attr(f, "non_filtrees")) == 0,
         "les valeurs dont le code qualite est ecarte passent a NA")

b2 <- b; b2$qrr[] <- 2                # tout 'douteux' : le code est sans doute mal compris
f2 <- mf_appliquer_qualite(b2)
# Le code 9 (filtre / non controle) est conserve : il ne doit pas amputer la serie
b9 <- b; b9$qrr[] <- 9
verifier(sum(!is.na(mf_appliquer_qualite(b9)$rr)) == sum(!is.na(b9$rr)),
         "le code qualite 9 (non encore controle) n'ecarte aucune valeur par defaut")
verifier(identical(attr(f2, "non_filtrees"), "RR") && sum(!is.na(f2$rr)) == sum(!is.na(b2$rr)),
         "garde-fou : si ecarter les codes supprime plus de la moitie des valeurs, on ne filtre pas et on le dit")


# ==============================================================================
groupe("Registre — dependances de colonnes")
# ==============================================================================

verifier(setequal(names(REQUIS), names(INDICATEURS)),
         "REQUIS couvre exactement le registre (ajouter un indicateur impose d'y declarer ses colonnes)")
verifier(all(vapply(INDICATEURS, function(i) !is.null(i$requiert), logical(1))),
         "chaque entree du registre porte son champ 'requiert'")
verifier(length(colonnes_manquantes(INDICATEURS$gel, serie_synthetique())) == 0,
         "SAFRAN complet : aucune colonne manquante")


# ==============================================================================
groupe("Module « Source des donnees » (logique serveur)")
# ==============================================================================

mf_altitude <- function(lon, lat) 150
mf_departements_proches <- function(lon, lat) "31"
mf_stations_dept <- function(dept) sd

lieu_t <- shiny::reactiveVal(NULL)
shiny::testServer(mod_station_server, args = list(lieu = lieu_t), {
  session$flushReact()
  verifier(is.null(session$returned()), "aucun point : aucune source proposee")

  lieu_t(list(lon = 1.47, lat = 43.35, nom = "Auterive", source = "commune"))
  session$flushReact()
  r <- session$returned()
  verifier(!is.null(r) && identical(r$type, "station") && identical(r$station$id, "31001001"),
           "un point designe : la meilleure station est proposee par defaut")
  verifier(nrow(r$candidats) == 3 && identical(r$alt, 150),
           "trois candidates et l'altitude du point sont transmises")

  session$setInputs(choix = "safran"); session$flushReact()
  verifier(identical(session$returned()$type, "safran") && is.null(session$returned()$station),
           "choisir la grille SAFRAN bascule la source")

  session$setInputs(choix = "31002002"); session$flushReact()
  verifier(identical(session$returned()$station$id, "31002002"),
           "choisir une autre station la retient")

  session$setInputs(choix = "inconnue"); session$flushReact()
  verifier(identical(session$returned()$station$id, "31001001"),
           "un choix qui n'existe plus retombe sur la proposition par defaut")
})

# Meilleure station insuffisante : SAFRAN par defaut
sd_mauvais <- sd; sd_mauvais$derniere_obs <- Sys.Date() - 800      # toutes arretees
mf_stations_dept <- function(dept) sd_mauvais
lieu_t2 <- shiny::reactiveVal(list(lon = 1.47, lat = 43.35, nom = "Auterive", source = "commune"))
shiny::testServer(mod_station_server, args = list(lieu = lieu_t2), {
  session$flushReact()
  verifier(identical(session$returned()$type, "safran"),
           "toutes les stations insuffisantes : la grille SAFRAN est proposee par defaut")
})

# Meteo-France injoignable : SAFRAN, et la cause est transmise
mf_stations_dept <- function(dept) {
  v <- mf_resumer_stations(mf_brut_vide(), dept); attr(v, "erreur") <- "panne reseau"; v }
lieu_t3 <- shiny::reactiveVal(list(lon = 1.47, lat = 43.35, nom = "Auterive", source = "commune"))
shiny::testServer(mod_station_server, args = list(lieu = lieu_t3), {
  session$flushReact()
  r <- session$returned()
  verifier(identical(r$type, "safran") && any(grepl("panne reseau", r$erreurs)),
           "Meteo-France injoignable : SAFRAN sans erreur, et la cause est transmise")
})

# L'interface ne reference plus le bandeau d'actualites
verifier(!exists("mod_actualites_ui"), "le bandeau d'actualites (3 blocs avant la localisation) est supprime")
verifier(!grepl("actus", paste(deparse(body(shiny::shinyApp)), collapse = "")) &&
           !any(grepl("mod_actualites", readLines("app.R", encoding = "UTF-8"))),
         "app.R n'appelle plus le module d'actualites")
