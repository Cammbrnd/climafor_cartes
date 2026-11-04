# ==============================================================================
# diagnostic_stations.R — PREMIER ESSAI REEL DES STATIONS METEO-FRANCE
# ------------------------------------------------------------------------------
# A lancer UNE FOIS depuis la racine du projet, sur une machine qui accede a
# internet, avant de compter sur les stations :
#
#     source("diagnostic_stations.R")
#
# POURQUOI CE SCRIPT EXISTE
# -------------------------
# Le module des stations (src/utils_stations.R) a ete ecrit et teste sur des
# fichiers SYNTHETIQUES au format Meteo-France, sans acces aux serveurs reels.
# Ce script confronte ce format a la realite, etape par etape, et affiche ce qui
# est effectivement recu. Il ne modifie rien.
#
# CE QU'IL FAUT REGARDER
#   [2] la liste des fichiers : des fichiers « latest » et « previous » pour
#       votre departement ? sinon le motif de nommage a change ;
#   [3] les colonnes : celles attendues sont-elles toutes la ?
#   [4] la repartition des CODES QUALITE, en distinguant les trois derniers mois.
#       Si 9 domine sur les derniers mois et pas avant, c'est « non encore
#       controle » : le garder (reglage actuel). Si 9 est rare et dissemine, il
#       s'agit de valeurs rejetees : ajouter 9 a CONFIG$stations$qualite_ecartee ;
#   [6] la station retenue et sa qualite ;
#   [7] la serie complete : annees valides, temps de chargement.
#
# Point de test par defaut : Limoise (03), 46,6876 N / 3,0541 E (modifiable).
# ==============================================================================

LAT <- 46.6876
LON <- 3.0541
ANNEES <- 1991:2025          # pour le test de chargement complet (etape 7)

# Resolveur de casse (voir global.R) : le partage peut avoir renomme .R en .r
ci <- function(chemin) {
  cur <- "."
  for (p in strsplit(chemin, "/", fixed = TRUE)[[1]]) {
    ls <- list.files(cur, all.files = TRUE, no.. = TRUE); h <- ls[tolower(ls) == tolower(p)]
    cur <- file.path(cur, if (p %in% h || !length(h)) p else h[1])
  }
  sub("^\\./", "", cur)
}
suppressMessages({
  library(httr2)
  source(ci("src/config.R"), encoding = "UTF-8")
  source(ci("src/utils_ui.R"), encoding = "UTF-8")
  source(ci("src/utils_cache.R"), encoding = "UTF-8")   # v1.7 : utils_api.R en depend
  source(ci("src/utils_api.R"), encoding = "UTF-8")
  source(ci("src/utils_calc.R"), encoding = "UTF-8")
  source(ci("src/utils_stations.R"), encoding = "UTF-8")
})
CONFIG$cache$actif <- FALSE      # on veut voir le reseau, pas le cache

titre <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")
ok  <- function(...) cat("  OK   ", ..., "\n", sep = "")
ko  <- function(...) cat("  ECHEC ", ..., "\n", sep = "")
chrono <- function(expr) { t0 <- Sys.time(); v <- force(expr); list(v = v, s = as.numeric(difftime(Sys.time(), t0, units = "secs"))) }

# ------------------------------------------------------------------------------
titre("1. Departements autour du point (API Geo) et altitude (IGN)")
# ------------------------------------------------------------------------------
deps <- mf_departements_proches(LON, LAT)
if (length(deps)) ok("departements : ", paste(deps, collapse = ", ")) else ko("aucun departement (API Geo injoignable ?)")
alt <- mf_altitude(LON, LAT)
if (is.finite(alt)) ok("altitude du point : ", round(alt), " m") else ko("altitude indisponible (le critere d'altitude sera ignore)")
if (!length(deps)) stop("Impossible de continuer sans departement.")

# ------------------------------------------------------------------------------
titre("2. Liste des fichiers Meteo-France (data.gouv.fr)")
# ------------------------------------------------------------------------------
cat("Jeu de donnees : ", CONFIG$stations$dataset_id, "\n", sep = "")
idx <- chrono(mf_index_fichiers(forcer = TRUE))
cat(sprintf("Duree : %.1f s | %d fichiers RR-T-Vent reconnus\n", idx$s, nrow(idx$v)))
if (!is.null(attr(idx$v, "erreurs"))) ko(attr(idx$v, "erreurs"))
if (nrow(idx$v)) {
  cat("Departements couverts : ", length(unique(idx$v$dept)), "\n", sep = "")
  f_dep <- idx$v[idx$v$dept == deps[1], ]
  print(f_dep[order(f_dep$debut), c("type", "debut", "fin", "modifie", "url")], row.names = FALSE)
  if (!all(c("latest", "previous") %in% f_dep$type)) ko("il manque latest ou previous pour ", deps[1])
} else {
  cat("\nLe repli (miroir) sera utilise. Verifiez CONFIG$stations$secours.\n")
}

# ------------------------------------------------------------------------------
titre("3. Fichier « latest » du departement : colonnes et echantillon")
# ------------------------------------------------------------------------------
f_lat <- mf_fichiers_dept(deps[1], type = "latest")
if (!nrow(f_lat)) stop("Pas de fichier latest pour ", deps[1])
f_lat <- f_lat[which.max(f_lat$fin), ]
cat("Fichier : ", f_lat$url, "\n", sep = "")
dl <- chrono(tryCatch(mf_telecharger(f_lat$url), error = function(e) { ko(conditionMessage(e)); NULL }))
if (is.null(dl$v)) stop("Telechargement impossible : voir le message ci-dessus (proxy, pare-feu ?).")
cat(sprintf("Telecharge en %.1f s (%.1f Mo)\n", dl$s, file.size(dl$v) / 1e6))

con <- gzfile(dl$v, "rt"); entete <- readLines(con, n = 3L, warn = FALSE); close(con)
noms <- strsplit(sub("\r$", "", entete[1]), ";", fixed = TRUE)[[1]]
cat("Colonnes (", length(noms), ") : ", paste(utils::head(noms, 30), collapse = " "), if (length(noms) > 30) " ...", "\n", sep = "")
absentes <- setdiff(MF_COLONNES, noms)
if (length(absentes)) {
  ko("colonnes attendues ABSENTES : ", paste(absentes, collapse = ", "), " (elles arriveront a NA)")
} else {
  ok("toutes les colonnes attendues sont presentes")
}
cat("Premiere ligne de donnees :\n  ", substr(entete[2], 1, 200), "\n", sep = "")

brut <- chrono(mf_lire_fichier(dl$v))
cat(sprintf("Lecture : %.1f s | %s lignes | %d stations | du %s au %s\n", brut$s,
            format(nrow(brut$v), big.mark = " "), length(unique(brut$v$id)),
            min(brut$v$date, na.rm = TRUE), max(brut$v$date, na.rm = TRUE)))
if (anyNA(brut$v$date)) ko(sum(is.na(brut$v$date)), " dates illisibles (AAAAMMJJ)")
b <- brut$v

# ------------------------------------------------------------------------------
titre("4. Codes qualite — a LIRE avec attention")
# ------------------------------------------------------------------------------
cat("Configuration actuelle : sont ecartes les codes ",
    paste(CONFIG$stations$qualite_ecartee, collapse = ", "), "\n\n", sep = "")
recent <- b$date > max(b$date, na.rm = TRUE) - 90
for (v in list(c("rr", "qrr"), c("tx", "qtx"), c("tn", "qtn"))) {
  x <- b[[v[1]]]; q <- b[[v[2]]]
  cat(toupper(v[1]), " — part de chaque code (valeurs renseignees seulement)\n", sep = "")
  t_all <- prop.table(table(q[!is.na(x) & !recent], useNA = "ifany"))
  t_rec <- prop.table(table(q[!is.na(x) & recent],  useNA = "ifany"))
  tab <- merge(data.frame(code = names(t_all), avant = round(100 * as.numeric(t_all), 1)),
               data.frame(code = names(t_rec), `3_derniers_mois` = round(100 * as.numeric(t_rec), 1),
                          check.names = FALSE), by = "code", all = TRUE)
  names(tab)[2:3] <- c("% avant", "% 3 derniers mois")
  print(tab, row.names = FALSE)
  cat("\n")
}
cat("Lecture : un code frequent seulement sur les 3 derniers mois = « non encore\n",
    "controle » (a garder). Un code rare et disperse = valeurs rejetees (a ecarter).\n", sep = "")

# ------------------------------------------------------------------------------
titre("5. Stations du departement (resume « latest »)")
# ------------------------------------------------------------------------------
sd <- mf_resumer_stations(mf_appliquer_qualite(b), deps[1])
cat(sprintf("%d stations ; %d avec pluie ; %d avec temperatures\n",
            nrow(sd), sum(sd$a_pluie), sum(sd$a_temp)))
cat(sprintf("Derniere mesure : mediane %s | %d stations a jour (< 8 jours)\n",
            format(stats::median(sd$derniere_obs, na.rm = TRUE)),
            sum(Sys.Date() - sd$derniere_obs < 8, na.rm = TRUE)))

# ------------------------------------------------------------------------------
titre("6. Stations proposees pour le point et leur qualite")
# ------------------------------------------------------------------------------
prop <- chrono(mf_stations_proches(LON, LAT, alt))
cat(sprintf("Recherche complete : %.1f s\n\n", prop$s))
p <- prop$v
if (!nrow(p)) {
  ko("aucune station dans ", CONFIG$stations$rayon_km, " km")
  if (length(attr(p, "erreurs"))) cat(paste(" -", attr(p, "erreurs")), sep = "\n")
  stop("Rien a charger.")
}
print(p[, c("nom", "id", "distance_km", "alt", "ecart_alt_m", "a_pluie", "a_temp",
            "fr_p", "fr_t", "age_j", "niveau", "libelle")], row.names = FALSE, digits = 3)

# ------------------------------------------------------------------------------
titre("7. Chargement complet de la meilleure station")
# ------------------------------------------------------------------------------
st <- as.list(p[1, ])
cat("Station : ", st$nom, " (", st$id, "), annees ", min(ANNEES), "-", max(ANNEES), "\n", sep = "")
ch <- chrono(station_charger(st, LON, LAT, ANNEES,
                             progres = function(f, m) cat(sprintf("  %3.0f %%  %s\n", 100 * f, m))))
d <- ch$v
cat(sprintf("\nDuree totale : %.0f s\n", ch$s))
if (!nrow(d)) {
  ko("aucune donnee")
  print(attr(d, "erreurs"))
} else {
  cat(sprintf("%s jours, %d a %d, derniere mesure %s\n", format(nrow(d), big.mark = " "),
              min(d$annee), max(d$annee), format(attr(d, "derniere_mesure"))))
  av <- attr(d, "annees_valides")
  cat("Annees valides : pluie ", length(av$p), " | temperatures ", length(av$t),
      " | vent ", length(av$ff), "\n", sep = "")
  ctx <- ctx_creer(ref = 1991:2020, courante = max(ANNEES))
  q <- mf_qualite_serie(d, ctx)
  cat("\nQualite de la serie sur 1991-2020 : ", q$libelle, "\n", sep = "")
  if (length(q$alertes)) cat(paste(" -", q$alertes), sep = "\n")
  if (length(attr(d, "erreurs"))) { cat("\nMessages :\n"); cat(paste(" -", attr(d, "erreurs")), sep = "\n") }
}

cat("\n", strrep("=", 78), "\nFin du diagnostic. Joindre cette sortie en cas de probleme.\n", sep = "")
