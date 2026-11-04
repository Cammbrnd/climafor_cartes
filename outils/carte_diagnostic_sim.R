# ==============================================================================
# carte_diagnostic_sim.R — LES FICHIERS SIM EN MASSE VALENT-ILS CE QUE LIT L'APPLICATION ?
# ------------------------------------------------------------------------------
# Constat des diagnostics precedents : le cube GeoSAS interpole (valeurs differentes
# de « position », aucun doublon sur une grille plus fine que les mailles) ; il ne
# peut pas servir a une carte qui doit coincider avec l'analyse d'une maille.
# Piste : Meteo-France publie la source en masse (data.gouv.fr, jeu « Donnees
# changement climatique - SIM quotidienne », licence ouverte) :
# un fichier QUOT_SIM2_AAAA.csv.gz par annee, mailles d'origine (LAMBX/LAMBY en hm,
# Lambert II etendu) et les memes variables que l'API (T, TINF_H, TSUP_H, PRELIQ...).
#
# Ce script ne modifie rien. Il repond a quatre questions :
#   1. quelle taille font les fichiers, et a quel debit les telecharge-t-on ?
#   2. a quoi ressemble le fichier des coordonnees de la grille ?
#   3. pour UNE maille, les valeurs du fichier sont-elles IDENTIQUES a « position » ?
#   4. le centre renvoye par « position » retombe-t-il sur LAMBX/LAMBY (conversion Lambert II etendu) ?
#
# Il telecharge UN fichier annuel (~140 Mo, 200 Mo au plus) et le lit : c'est aussi la
# mesure du cout reel de la lecture pour le calcul de la carte.
# Lancement : source("outils/carte_diagnostic_sim.R") depuis la racine du projet, ou
# workflow « Diagnostic GeoSAS » (etape « sim »).
# ==============================================================================

ANNEE   <- 2020
MAX_MO  <- 200       # octets recus au plus (un fichier annuel fait ~140 Mo)
PAUSE   <- 1
ID_JEU  <- "6569b27598256cc583c917a7"

ci <- function(chemin) {
  cur <- "."
  for (p in strsplit(chemin, "/", fixed = TRUE)[[1]]) {
    ls <- list.files(cur, all.files = TRUE, no.. = TRUE); h <- ls[tolower(ls) == tolower(p)]
    cur <- file.path(cur, if (p %in% h || !length(h)) p else h[1])
  }
  sub("^\\./", "", cur)
}
library(httr2)
source(ci("src/config.R"), encoding = "UTF-8")
source(ci("src/utils_cache.R"), encoding = "UTF-8")
source(ci("src/utils_api.R"), encoding = "UTF-8")
source(ci("outils/carte_geo.R"), encoding = "UTF-8")

titre <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")
UA <- sprintf("%s/%s diagnostic_sim (%s)", CONFIG$app$nom, CONFIG$app$version, CONFIG$app$organisme)

# ------------------------------------------------------------------------------
titre("1. Les ressources du jeu et la taille des fichiers")
# ------------------------------------------------------------------------------
rep <- tryCatch(httr2::req_perform(httr2::req_error(httr2::req_timeout(
  httr2::req_user_agent(httr2::request(paste0("https://www.data.gouv.fr/api/1/datasets/", ID_JEU, "/")), UA), 40),
  is_error = function(r) FALSE)), error = function(e) NULL)
res <- if (!is.null(rep) && httr2::resp_status(rep) < 400)
  httr2::resp_body_json(rep, simplifyVector = FALSE)$resources else list()
url_de <- function(titre_res) {
  for (r in res) if (identical(r$title, titre_res)) return(r$url)
  NA_character_
}
cat(sprintf("%d ressources trouvees\n", length(res)))

# Taille annoncee par le serveur (requete HEAD : aucun octet de donnees).
taille_distante <- function(url) {
  r <- tryCatch(httr2::req_perform(httr2::req_error(httr2::req_timeout(
    httr2::req_user_agent(httr2::req_method(httr2::request(url), "HEAD"), UA), 40),
    is_error = function(r) FALSE)), error = function(e) NULL)
  if (is.null(r)) return(NA_real_)
  as.numeric(httr2::resp_header(r, "content-length") %||% NA)
}
for (a in c(1958, 1990, 2020, 2025, 2026, "latest")) {
  u <- url_de(paste0("QUOT_SIM2_", a))
  if (is.na(u)) { cat(sprintf("QUOT_SIM2_%s : introuvable\n", a)); next }
  cat(sprintf("QUOT_SIM2_%-6s : %8.1f Mo (compresse)\n", a, taille_distante(u) / 1e6))
  Sys.sleep(PAUSE)
}

# ------------------------------------------------------------------------------
titre("2. Le fichier des coordonnees de la grille")
# ------------------------------------------------------------------------------
u_coord <- url_de("coordonnees_grille_safran_lambert-2-etendu.csv")
cat("URL : ", u_coord, "\n", sep = "")
coord <- NULL
if (!is.na(u_coord)) {
  r <- tryCatch(httr2::req_perform(httr2::req_error(httr2::req_timeout(
    httr2::req_user_agent(httr2::request(u_coord), UA), 120), is_error = function(r) FALSE)),
    error = function(e) NULL)
  if (!is.null(r) && httr2::resp_status(r) < 400) {
    txt <- httr2::resp_body_string(r, encoding = "UTF-8")
    lignes <- strsplit(txt, "\r?\n")[[1]]
    cat(sprintf("%d lignes, %.1f Mo\n", length(lignes), nchar(txt, "bytes") / 1e6))
    cat(paste0("   ", substr(head(lignes, 4), 1, 200)), sep = "\n")
    sep <- if (grepl(";", lignes[1], fixed = TRUE)) ";" else ","
    coord <- tryCatch(utils::read.csv(text = txt, sep = sep, dec = ",", stringsAsFactors = FALSE),
                      error = function(e) NULL)
    if (!is.null(coord)) {
      cat("\nColonnes : ", paste(names(coord), collapse = " | "), "\n", sep = "")
      for (nm in names(coord)) if (is.numeric(coord[[nm]]))
        cat(sprintf("   %-28s min %-12s max %s\n", nm, format(min(coord[[nm]], na.rm = TRUE)),
                    format(max(coord[[nm]], na.rm = TRUE))))
    }
  } else cat("Telechargement impossible.\n")
}

# ------------------------------------------------------------------------------
titre(sprintf("3. Telechargement et lecture complets de QUOT_SIM2_%d", ANNEE))
# ------------------------------------------------------------------------------
u_an <- url_de(paste0("QUOT_SIM2_", ANNEE))
fichier <- tempfile(fileext = ".csv.gz")
if (!is.na(u_an)) {
  options(timeout = 900, HTTPUserAgent = UA)
  t0 <- Sys.time()
  ok_dl <- tryCatch(utils::download.file(u_an, fichier, mode = "wb", quiet = TRUE, method = "libcurl") == 0,
                    error = function(e) { cat("ECHEC : ", conditionMessage(e), "\n", sep = ""); FALSE },
                    warning = function(w) { cat("AVERTISSEMENT : ", conditionMessage(w), "\n", sep = ""); FALSE })
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  lus <- if (file.exists(fichier)) file.size(fichier) else 0
  cat(sprintf("%.1f Mo recus en %.1f s : %.1f Mo/s\n", lus / 1e6, dt, lus / 1e6 / max(dt, 0.001)))
}

COLONNES <- c("LAMBX", "LAMBY", "DATE", "T", "TINF_H", "TSUP_H", "PRELIQ", "PRENEI", "ETP",
              "SWI", "HTEURNEIGE", "RESR_NEIGE", "FF", "HU", "EVAP", "PE")
# Lecture d'un fichier annuel : data.table::fread si present (rapide), sinon repli lent.
lire_annee <- function(f) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fread(cmd = sprintf("gzip -dc %s", shQuote(f)), sep = ";", select = COLONNES,
                      showProgress = FALSE, data.table = FALSE)
  } else {
    cat("   (data.table absent : lecture de repli, lente)\n")
    utils::read.csv(pipe(sprintf("gzip -dc %s", shQuote(f))), sep = ";", stringsAsFactors = FALSE)[, COLONNES]
  }
}
sim <- NULL
if (file.exists(fichier) && file.size(fichier) > 1000) {
  t0 <- Sys.time()
  sim <- tryCatch(lire_annee(fichier), error = function(e) { cat("ECHEC lecture : ", conditionMessage(e), "\n"); NULL })
  if (!is.null(sim)) {
    cat(sprintf("Lecture (14 colonnes) : %.1f s | %d lignes | %.0f Mo en memoire\n",
                as.numeric(difftime(Sys.time(), t0, units = "secs")), nrow(sim),
                as.numeric(object.size(sim)) / 1e6))
    cle <- sim$LAMBX * 100000 + sim$LAMBY
    cc <- sim[!duplicated(cle), c("LAMBX", "LAMBY")]
    cat(sprintf("Mailles distinctes : %d | jours par maille : %s\n", nrow(cc),
                paste(unique(range(tabulate(match(cle, cle[!duplicated(cle)])))), collapse = " a ")))
    if (!is.null(coord)) {
      kf <- paste(coord[[1]], coord[[2]]); ks <- paste(cc$LAMBX, cc$LAMBY)
      cat(sprintf("Mailles du fichier de coordonnees absentes du fichier annuel : %d | l'inverse : %d\n",
                  sum(!kf %in% ks), sum(!ks %in% kf)))
    }
  }
} else cat("Fichier annuel absent : etapes 3 (lecture) et 4 sautees.\n")

# ------------------------------------------------------------------------------
titre("4. Quatre mailles : le fichier SIM est-il identique a « position » ?")
# ------------------------------------------------------------------------------
# Correspondance variable API -> colonne du fichier : on retire le suffixe _Q.
api_vers_csv <- function(x) sub("_Q$", "", x)
# Quand une variable differe : a quoi ressemble-t-elle ? (autres colonnes, decalage de jours)
diagnostiquer <- function(v_api, sel, dates_api) {
  cat("      Diagnostic de l'ecart (colonne du fichier, decalage de jours -> ecart max) :\n")
  for (nm in c("ETP", "EVAP", "PE")) for (dec in -2:2) {
    v <- sel[[nm]][match(format(dates_api + dec, "%Y%m%d"), as.character(sel$DATE))]
    ok <- is.finite(v) & is.finite(v_api)
    if (sum(ok) > 300) cat(sprintf("        %-5s %+d j : %.6f (%d jours)\n", nm, dec, max(abs(v[ok] - v_api[ok])), sum(ok)))
  }
  j7 <- which(format(dates_api, "%m") == "07")[1:8]
  cat("      8 jours de juillet :  date | API | ETP | EVAP | PE\n")
  for (k in j7) {
    r <- sel[match(format(dates_api[k], "%Y%m%d"), as.character(sel$DATE)), ]
    cat(sprintf("        %s | %s | %s | %s | %s\n", dates_api[k], format(v_api[k]), r$ETP, r$EVAP, r$PE))
  }
}
if (is.null(sim) || is.null(coord)) {
  cat("Fichier annuel ou coordonnees indisponibles : comparaison sautee.\n")
} else {
  names(coord)[1:4] <- c("LAMBX", "LAMBY", "LAT", "LON")
  # Villes cibles -> maille la plus proche d'apres le fichier des coordonnees (NTF, degres).
  cibles <- list(Toulouse = c(1.444, 43.605), Paris = c(2.35, 48.85), Briancon = c(6.64, 44.9),
                 `Ouessant (maille d'angle)` = c(-4.96, 48.38))
  url_col <- sub("/position/?$", "", CONFIG$api$edr_base)
  # Comme l'application : on ne demande que les variables que la collection expose
  # (en demander une qu'elle n'expose pas donne « erreur de syntaxe »).
  neg <- safran_parametres_utiles()
  cat(sprintf("Variables demandees a « position » : %s\n", paste(neg$api, collapse = ", ")))
  cat(sprintf("Variables NON exposees par la collection (source %s) : %s\n", neg$source,
              if (length(neg$absents)) paste(neg$absents, collapse = ", ") else "aucune"))
  params <- neg$api
  for (nom in names(cibles)) {
    cb <- cibles[[nom]]
    k <- which.min((coord$LAT - cb[2])^2 + ((coord$LON - cb[1]) * cos(cb[2] * pi / 180))^2)
    mx <- coord$LAMBX[k] * 100; my <- coord$LAMBY[k] * 100
    sel <- sim[sim$LAMBX == coord$LAMBX[k] & sim$LAMBY == coord$LAMBY[k], ]
    sel <- sel[order(sel$DATE), ]
    ll <- carte_depuis_l2e(mx, my)
    cat(sprintf("\n-- %s : LAMBX %d, LAMBY %d (x = %d m, y = %d m), %d jours\n   centre en lon/lat : %.5f, %.5f\n",
                nom, coord$LAMBX[k], coord$LAMBY[k], mx, my, nrow(sel), ll$lon, ll$lat))
    req <- requete_avec_params(paste0(url_col, "/position"), list(
      coords = sprintf("POINT(%s %s)", fmt_coord(ll$lon), fmt_coord(ll$lat)),
      `parameter-name` = paste(params, collapse = ","), crs = CONFIG$api$edr_crs,
      datetime = sprintf("%d-01-01/%d-12-31", ANNEE, ANNEE), f = CONFIG$api$edr_format), 120)
    req <- httr2::req_error(httr2::req_user_agent(req, UA), is_error = function(r) FALSE)
    r <- tryCatch(httr2::req_perform(req), error = function(e) {
      cat("   ECHEC RESEAU : ", conditionMessage(e), "\n", sep = ""); NULL })
    Sys.sleep(PAUSE)
    if (is.null(r)) next
    cat(sprintf("   statut %d, %d octets\n", httr2::resp_status(r), length(httr2::resp_body_raw(r))))
    if (httr2::resp_status(r) >= 400 || length(httr2::resp_body_raw(r)) < 1000) {
      cat("   Corps : ", substr(gsub("\\s+", " ", httr2::resp_body_string(r)), 1, 400), "\n", sep = ""); next
    }
    j <- httr2::resp_body_json(r, simplifyVector = FALSE)
    ax <- j$domain$axes
    cx <- as.numeric(unlist(ax$x$values)); cy <- as.numeric(unlist(ax$y$values))
    cat(sprintf("   centre renvoye par « position » : x = %s, y = %s\n",
                paste(round(cx, 5), collapse = ","), paste(round(cy, 5), collapse = ",")))
    if (length(cx) == 1 && length(cy) == 1) {
      b <- carte_vers_l2e(cx, cy)
      cat(sprintf("   ... soit en Lambert II etendu : x = %.0f m, y = %.0f m (attendu %d, %d)\n",
                  b$x, b$y, mx, my))
    }
    dates_api <- as.Date(substr(corriger_horodatage(unlist(ax$t$values)), 1, 10))
    ok_tot <- 0L; n_tot <- 0L
    for (p in params) {
      v_api <- as_num_json(j$ranges[[p]]$values)
      col <- api_vers_csv(p)
      if (!col %in% names(sel)) { cat(sprintf("   %-14s absent du fichier SIM\n", p)); next }
      if (!length(v_api)) { cat(sprintf("   %-14s absent de la reponse API\n", p)); next }
      v_csv <- sel[[col]][match(format(dates_api, "%Y%m%d"), as.character(sel$DATE))]
      m <- !(is.na(v_api) & is.na(v_csv))
      dif <- if (any(m)) max(abs(v_api[m] - v_csv[m]), na.rm = TRUE) else 0
      nna <- sum(is.na(v_api) != is.na(v_csv))
      etendue <- if (any(is.finite(v_csv))) diff(range(v_csv, na.rm = TRUE)) else 0
      cat(sprintf("   %-14s %4d jours | ecart max %-9s | NA differents : %d | etendue des valeurs : %s\n",
                  p, length(v_api), format(round(dif, 6)), nna, format(round(etendue, 3))))
      n_tot <- n_tot + 1L; if (dif < 1e-5 && nna == 0) ok_tot <- ok_tot + 1L
      if (dif >= 1e-5 && nom == "Toulouse") diagnostiquer(v_api, sel, dates_api)
    }
    cat(sprintf("   => %d variable(s) sur %d identiques (ecart < 1e-5 : le service renvoie des flottants simple precision)\n", ok_tot, n_tot))
  }
}

titre("Diagnostic termine")
cat("Copiez l'integralite de cette sortie.\n")
