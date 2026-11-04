# ==============================================================================
# carte_diagnostic_grille.R — LE CUBE GEOSAS RENVOIE-T-IL LES VALEURS DES MAILLES ?
# ------------------------------------------------------------------------------
# Constat du premier diagnostic (carte_diagnostic_zone.R) : le cube est rapide
# (2 450 mailles x 1 an en 3 s) mais il renvoie une grille REGULIERE en lon/lat
# dont le pas depend de la boite demandee, pas les mailles SAFRAN d'origine.
#
# Question : ses valeurs sont-elles celles de la maille la plus proche (donc
# identiques a ce que l'application lit avec « position »), ou interpolees ?
# Si elles sont interpolees, une carte construite sur le cube ne coinciderait pas
# avec l'analyse d'une maille : il faudrait une autre source.
#
# Ce script ne modifie rien. Il envoie une vingtaine de petites requetes espacees
# d'une seconde. Lancement : source("outils/carte_diagnostic_grille.R") depuis la
# racine du projet, ou workflow « Diagnostic GeoSAS » (etape « grille »).
#
#   1. cube 0,4 deg : combien de series DISTINCTES parmi ses points ?
#   2. memes points interroges avec « position » : valeurs identiques ?
#   3. cube 1 deg : taux de doublons (grille plus fine que les mailles ?)
#   4. transect en longitude : ou changent les mailles, quelles coordonnees
#      « position » renvoie-t-il ?
#   5. repli : fichiers SIM en masse sur data.gouv.fr (liste + 4 premieres lignes).
# ==============================================================================

PARAM <- "T_Q"
LON0  <- 1.2
LAT0  <- 43.4
PAUSE <- 1
DATES <- "2020-01-01/2020-01-31"

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

titre <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")
url_collection <- sub("/position/?$", "", CONFIG$api$edr_base)

# Une requete GeoSAS ; renvoie le JSON (liste) ou NULL. Ne leve jamais d'erreur.
lire_json <- function(suffixe, params, timeout = 120) {
  req <- requete_avec_params(paste0(url_collection, "/", suffixe), params, timeout)
  req <- httr2::req_user_agent(req, sprintf("%s/%s diagnostic_grille (%s)",
    CONFIG$app$nom, CONFIG$app$version, CONFIG$app$organisme))
  req <- httr2::req_error(req, is_error = function(r) FALSE)
  rep <- tryCatch(httr2::req_perform(req), error = function(e) {
    cat("   ECHEC RESEAU : ", conditionMessage(e), "\n", sep = ""); NULL })
  Sys.sleep(PAUSE)
  if (is.null(rep)) return(NULL)
  if (httr2::resp_status(rep) >= 400 || length(httr2::resp_body_raw(rep)) < 200) {
    cat(sprintf("   statut %d, %d octets : inutilisable\n", httr2::resp_status(rep),
                length(httr2::resp_body_raw(rep))))
    return(NULL)
  }
  tryCatch(httr2::resp_body_json(rep, simplifyVector = FALSE), error = function(e) NULL)
}

# Valeurs d'un axe CoverageJSON : explicites, ou regulier (debut/fin/nombre).
axe_valeurs <- function(a) {
  if (is.null(a)) return(numeric(0))
  if (!is.null(a$values)) return(as.numeric(unlist(a$values)))
  seq(as.numeric(a$start), as.numeric(a$stop), length.out = as.integer(a$num))
}

# Serie temporelle d'une plage `rg` au point (ix, iy) d'une grille ; respecte l'ordre
# des axes annonce par la reponse (axisNames), le dernier variant le plus vite.
serie_au_point <- function(rg, ix, iy) {
  ax <- unlist(rg$axisNames); sh <- as.integer(unlist(rg$shape))
  arr <- array(as_num_json(rg$values), dim = rev(sh))
  idx <- lapply(rev(ax), function(a) switch(a, x = ix, y = iy, seq_len(sh[ax == a])))
  as.vector(do.call(`[`, c(list(arr), idx)))
}

signature <- function(v) paste(ifelse(is.na(v), "NA", sprintf("%.3f", v)), collapse = ";")

# Compare deux series (ecart max, NA aux memes endroits).
comparer <- function(a, b) {
  if (length(a) != length(b)) return(sprintf("longueurs differentes (%d / %d)", length(a), length(b)))
  if (any(is.na(a) != is.na(b))) return("trous (NA) differents")
  d <- suppressWarnings(max(abs(a - b), na.rm = TRUE))
  if (!is.finite(d)) return("tout vide")
  if (d == 0) "IDENTIQUES" else sprintf("ecart max %.4f", d)
}

# Reponse « position » : (x, y renvoyes, serie).
position <- function(lon, lat, dates = DATES) {
  j <- lire_json("position", list(coords = sprintf("POINT(%s %s)", fmt_coord(lon), fmt_coord(lat)),
    `parameter-name` = PARAM, crs = CONFIG$api$edr_crs, datetime = dates, f = CONFIG$api$edr_format))
  if (is.null(j)) return(NULL)
  ax <- j$domain$axes
  list(x = axe_valeurs(ax$x), y = axe_valeurs(ax$y),
       serie = as_num_json(j$ranges[[PARAM]]$values))
}

cube <- function(cote, dates = DATES) {
  j <- lire_json("cube", list(bbox = paste(fmt_coord(LON0), fmt_coord(LAT0),
    fmt_coord(LON0 + cote), fmt_coord(LAT0 + cote), sep = ","),
    `parameter-name` = PARAM, crs = CONFIG$api$edr_crs, datetime = dates,
    f = CONFIG$api$edr_format), timeout = 180)
  if (is.null(j)) return(NULL)
  ax <- j$domain$axes
  x <- axe_valeurs(ax$x); y <- axe_valeurs(ax$y); rg <- j$ranges[[PARAM]]
  pts <- expand.grid(ix = seq_along(x), iy = seq_along(y))
  pts$lon <- x[pts$ix]; pts$lat <- y[pts$iy]
  pts$sig <- mapply(function(i, k) signature(serie_au_point(rg, i, k)), pts$ix, pts$iy)
  list(x = x, y = y, rg = rg, pts = pts)
}

# ------------------------------------------------------------------------------
titre("1. Cube 0,4 deg : series distinctes parmi ses points")
# ------------------------------------------------------------------------------
c1 <- cube(0.4)
if (is.null(c1)) {
  cat("Cube illisible : sections 1 a 3 sautees.\n")
} else {
  cat(sprintf("Grille : %d (x) x %d (y) = %d points | pas x %.5f, pas y %.5f\n",
              length(c1$x), length(c1$y), nrow(c1$pts),
              mean(diff(c1$x)), mean(diff(c1$y))))
  cat(sprintf("Series DISTINCTES : %d sur %d points (%d doublons)\n",
              length(unique(c1$pts$sig)), nrow(c1$pts),
              nrow(c1$pts) - length(unique(c1$pts$sig))))

# ------------------------------------------------------------------------------
  titre("2. Memes points interroges avec « position » (ce que lit l'application)")
# ------------------------------------------------------------------------------
  n <- nrow(c1$pts)
  choisis <- unique(c(1, length(c1$x), n - length(c1$x) + 1, n, round(n / 2), round(n / 2) + 7))
  nb_id <- 0L
  for (k in choisis) {
    p <- c1$pts[k, ]
    r <- position(p$lon, p$lat)
    cat(sprintf("\n-- point %d du cube : lon %.5f, lat %.5f\n", k, p$lon, p$lat))
    if (is.null(r)) next
    cmp <- comparer(serie_au_point(c1$rg, p$ix, p$iy), r$serie)
    if (cmp == "IDENTIQUES") nb_id <- nb_id + 1L
    cat(sprintf("   position renvoie x = %s, y = %s\n   cube vs position : %s\n",
                paste(round(r$x, 5), collapse = ","), paste(round(r$y, 5), collapse = ","), cmp))
  }
  cat(sprintf("\nBilan : %d point(s) sur %d avec valeurs IDENTIQUES a « position ».\n",
              nb_id, length(choisis)))

# ------------------------------------------------------------------------------
  titre("3. Cube 1 deg : grille plus fine que les mailles ?")
# ------------------------------------------------------------------------------
  c2 <- cube(1)
  if (!is.null(c2)) {
    km_lon <- 111.32 * cos(43.9 * pi / 180)
    cat(sprintf("Points : %d | series distinctes : %d | doublons : %d\n",
                nrow(c2$pts), length(unique(c2$pts$sig)),
                nrow(c2$pts) - length(unique(c2$pts$sig))))
    cat(sprintf("Mailles SAFRAN de 8 km attendues dans 1 deg x 1 deg a 44 N : environ %.0f\n",
                (111.2 / 8) * (km_lon / 8)))
  }
}

# ------------------------------------------------------------------------------
titre("4. Transect en longitude (lat 43,6) : ou changent les mailles ?")
# ------------------------------------------------------------------------------
cat("lon demandee | x renvoye | y renvoye | empreinte de la serie\n")
tr <- NULL
for (lon in seq(1.2, 1.5, by = 0.03)) {
  r <- position(lon, 43.6)
  if (is.null(r)) next
  sg <- signature(r$serie)
  cat(sprintf("%.3f | %s | %s | %s\n", lon, paste(round(r$x, 5), collapse = ","),
              paste(round(r$y, 5), collapse = ","), substr(sg, 1, 30)))
  tr <- c(tr, sg)
}
if (length(tr)) cat(sprintf("Series distinctes sur le transect : %d (pour %d points)\n",
                            length(unique(tr)), length(tr)))

# ------------------------------------------------------------------------------
titre("5. Repli : fichiers SIM en masse sur data.gouv.fr")
# ------------------------------------------------------------------------------
ID_JEU <- "6569b27598256cc583c917a7"
rep_dg <- tryCatch(httr2::req_perform(httr2::req_error(httr2::req_timeout(
  httr2::request(paste0("https://www.data.gouv.fr/api/1/datasets/", ID_JEU, "/")), 40),
  is_error = function(r) FALSE)), error = function(e) { cat("ECHEC RESEAU : ", conditionMessage(e), "\n"); NULL })
res <- list()
if (!is.null(rep_dg) && httr2::resp_status(rep_dg) < 400) {
  d <- httr2::resp_body_json(rep_dg, simplifyVector = FALSE)
  cat("Jeu : ", d$title, " | licence : ", d$license %||% "?", "\n", sep = "")
  res <- d$resources %||% list()
  cat(sprintf("%d ressources\n\n", length(res)))
  for (r in res) cat(sprintf("%-62s %-6s %9s Mo\n", substr(r$title %||% "?", 1, 62),
                             r$format %||% "?", if (is.null(r$filesize)) "?" else sprintf("%.0f", r$filesize / 1e6)))
}

# Premiers octets de deux fichiers .csv.gz : on lit 64 Ko au plus, jamais le fichier entier.
premiers_octets <- function(url, max_octets = 65536) {
  buf <- raw(0)
  req <- httr2::req_timeout(httr2::req_headers(httr2::request(url), Range = "bytes=0-65535"), 90)
  req <- httr2::req_error(req, is_error = function(r) FALSE)
  tryCatch(httr2::req_perform_stream(req, function(x) {
    buf <<- c(buf, x); length(buf) < max_octets }, buffer_kb = 32),
    error = function(e) cat("   ECHEC : ", conditionMessage(e), "\n", sep = ""))
  buf[seq_len(min(length(buf), max_octets))]
}
cibles <- Filter(function(r) grepl("\\.csv\\.gz($|\\?)", r$url %||% "") , res)
for (r in head(cibles, 2)) {
  cat(sprintf("\n-- debut de : %s\n   %s\n", r$title, r$url))
  b <- premiers_octets(r$url)
  cat(sprintf("   %d octets lus\n", length(b)))
  if (length(b) > 100) {
    f <- tempfile(fileext = ".gz"); writeBin(b, f)
    l <- suppressWarnings(system(sprintf("gzip -dc %s 2>/dev/null | head -n 4", shQuote(f)), intern = TRUE))
    cat(paste0("   ", substr(l, 1, 400)), sep = "\n")
  }
}

titre("Diagnostic termine")
cat("Copiez l'integralite de cette sortie.\n")
