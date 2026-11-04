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
# Il ne telecharge jamais un fichier annuel en entier : 16 Mo au plus, puis arret.
# Lancement : source("outils/carte_diagnostic_sim.R") depuis la racine du projet, ou
# workflow « Diagnostic GeoSAS » (etape « sim »).
# ==============================================================================

ANNEE   <- 2020
MAX_MO  <- 16        # octets lus au plus dans un fichier annuel
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
    coord <- tryCatch(utils::read.csv(text = txt, sep = sep, stringsAsFactors = FALSE),
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
titre(sprintf("3. Debit et contenu du debut de QUOT_SIM2_%d (%d Mo au plus)", ANNEE, MAX_MO))
# ------------------------------------------------------------------------------
u_an <- url_de(paste0("QUOT_SIM2_", ANNEE))
fichier <- tempfile(fileext = ".csv.gz")
if (!is.na(u_an)) {
  t0 <- Sys.time(); lus <- 0
  con <- file(fichier, "wb")
  r <- tryCatch(httr2::req_perform_connection(httr2::req_timeout(
    httr2::req_user_agent(httr2::request(u_an), UA), 180)), error = function(e) {
      cat("ECHEC : ", conditionMessage(e), "\n", sep = ""); NULL })
  if (!is.null(r)) {
    while (!httr2::resp_stream_is_complete(r) && lus < MAX_MO * 1048576) {
      b <- httr2::resp_stream_raw(r, kb = 512)
      if (!length(b)) break
      writeBin(b, con); lus <- lus + length(b)
    }
    close(r)
  }
  close(con)
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("%.1f Mo lus en %.1f s : %.1f Mo/s\n", lus / 1e6, dt, lus / 1e6 / max(dt, 0.001)))
}

# Lignes decompressees (un fichier tronque donne les premieres lignes, puis une erreur).
lire_debut <- function(f, n = 400000) {
  l <- suppressWarnings(system(sprintf("gzip -dc %s 2>/dev/null | head -n %d", shQuote(f), n), intern = TRUE))
  if (length(l) < 2) return(NULL)
  d <- tryCatch(utils::read.csv(text = paste(l[-length(l)], collapse = "\n"), sep = ";",
                                stringsAsFactors = FALSE), error = function(e) NULL)
  d
}
d <- if (file.exists(fichier) && file.size(fichier) > 1000) lire_debut(fichier) else NULL
if (!is.null(d)) {
  cat(sprintf("\n%d lignes lues | colonnes : %s\n", nrow(d), paste(names(d), collapse = " ")))
  cc <- unique(d[, c("LAMBX", "LAMBY")])
  cat(sprintf("Mailles dans cet extrait : %d (premiere : LAMBX %d, LAMBY %d)\n",
              nrow(cc), cc$LAMBX[1], cc$LAMBY[1]))
  cat(sprintf("Dates de la premiere maille : %s -> %s (%d jours)\n",
              min(d$DATE[d$LAMBX == cc$LAMBX[1] & d$LAMBY == cc$LAMBY[1]]),
              max(d$DATE[d$LAMBX == cc$LAMBX[1] & d$LAMBY == cc$LAMBY[1]]),
              sum(d$LAMBX == cc$LAMBX[1] & d$LAMBY == cc$LAMBY[1])))
}

# ------------------------------------------------------------------------------
titre("4. Une maille : le fichier SIM est-il identique a « position » ?")
# ------------------------------------------------------------------------------
# Correspondance variable API -> colonne du fichier : on retire le suffixe _Q.
api_vers_csv <- function(x) sub("_Q$", "", x)
if (is.null(d)) {
  cat("Pas d'extrait lisible : comparaison sautee.\n")
} else {
  # Les 3 premieres mailles de l'extrait, en coordonnees natives (hm -> m).
  url_col <- sub("/position/?$", "", CONFIG$api$edr_base)
  params <- cfg_params_api()
  for (k in seq_len(min(3, nrow(cc)))) {
    mx <- cc$LAMBX[k] * 100; my <- cc$LAMBY[k] * 100
    sel <- d[d$LAMBX == cc$LAMBX[k] & d$LAMBY == cc$LAMBY[k], ]
    cat(sprintf("\n-- maille %d : LAMBX %d, LAMBY %d (x = %d m, y = %d m), %d jours\n",
                k, cc$LAMBX[k], cc$LAMBY[k], mx, my, nrow(sel)))
    dmin <- as.Date(as.character(min(sel$DATE)), "%Y%m%d"); dmax <- as.Date(as.character(max(sel$DATE)), "%Y%m%d")
    ll <- carte_depuis_l2e(mx, my)       # centre de la maille en lon/lat WGS84
    cat(sprintf("   centre en lon/lat : %.5f, %.5f\n", ll$lon, ll$lat))
    req <- requete_avec_params(paste0(url_col, "/position"), list(
      coords = sprintf("POINT(%s %s)", fmt_coord(ll$lon), fmt_coord(ll$lat)),
      `parameter-name` = paste(params, collapse = ","), crs = CONFIG$api$edr_crs,
      datetime = sprintf("%s/%s", dmin, dmax), f = CONFIG$api$edr_format), 120)
    req <- httr2::req_error(httr2::req_user_agent(req, UA), is_error = function(r) FALSE)
    cat("   URL : ", substr(req$url, 1, 200), "\n", sep = "")
    r <- tryCatch(httr2::req_perform(req), error = function(e) {
      cat("   ECHEC RESEAU : ", conditionMessage(e), "\n", sep = ""); NULL })
    Sys.sleep(PAUSE)
    if (is.null(r)) next
    cat(sprintf("   statut %d, %d octets\n", httr2::resp_status(r), length(httr2::resp_body_raw(r))))
    if (httr2::resp_status(r) >= 400 || length(httr2::resp_body_raw(r)) < 200) {
      cat("   Corps : ", substr(httr2::resp_body_string(r), 1, 300), "\n", sep = ""); next
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
      v_csv <- sel[[col]][match(format(dates_api, "%Y%m%d"), as.character(sel$DATE))]
      if (!length(v_api)) { cat(sprintf("   %-14s absent de la reponse API\n", p)); next }
      m <- !(is.na(v_api) & is.na(v_csv))
      dif <- if (any(m)) max(abs(v_api[m] - v_csv[m]), na.rm = TRUE) else 0
      nna <- sum(is.na(v_api) != is.na(v_csv))
      cat(sprintf("   %-14s %4d jours | ecart max %-8s | NA differents : %d\n", p, length(v_api),
                  format(round(dif, 6)), nna))
      n_tot <- n_tot + 1L; if (dif < 1e-6 && nna == 0) ok_tot <- ok_tot + 1L
    }
    cat(sprintf("   => %d variable(s) sur %d IDENTIQUES\n", ok_tot, n_tot))
  }
}

titre("Diagnostic termine")
cat("Copiez l'integralite de cette sortie.\n")
