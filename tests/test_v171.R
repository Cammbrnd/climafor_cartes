# ==============================================================================
# tests/test_v171.R — REVUE COMPLETE DU CODE, VERSION 1.7.1
# ------------------------------------------------------------------------------
# La revue cherchait quatre choses : du code mort, du code en doublon ou en
# conflit, des constats d'audit en realite non traites, et des ecarts entre ce
# que dit la documentation et ce que fait le code. Ce fichier garde les trois
# premieres a l'oeil : si quelqu'un ajoute demain une fonction que personne
# n'appelle, un reglage que personne ne lit ou une regle CSS sans element, un
# test rouge le dit.
#
# Ce que ces controles NE voient PAS :
#   - le code atteint seulement par un nom construit (do.call(paste0(...)),
#     get() sur une chaine calculee) : une telle fonction passerait pour
#     « utilisee » si son nom apparait en toutes lettres, et sinon serait
#     signalee a tort — dans ce cas, l'ecrire en toutes lettres quelque part ;
#   - un code atteignable mais jamais execute (une branche qu'aucun parcours
#     n'emprunte) : la couverture mesuree par covr le dit, pas ce fichier ;
#   - le code mort dans les fichiers hors de l'application (outils/,
#     diagnostic*.R, deploy.R), volontairement ecartes ;
#   - src/_gabarit_extension.R, qui n'est pas charge : c'est un modele a copier.
# ==============================================================================

# -- Outils d'analyse ----------------------------------------------------------
lire171 <- function(f) readLines(chemin_ci(f), warn = FALSE, encoding = "UTF-8")

# Code d'un Rmd : blocs R et expressions en ligne (`r ...`).
code_rmd171 <- function(f) {
  l <- lire171(f); out <- character(); dedans <- FALSE
  for (x in l) {
    if (grepl("^```\\{r", x)) { dedans <- TRUE; next }
    if (grepl("^```", x))     { dedans <- FALSE; next }
    if (dedans) out <- c(out, x)
    else {
      m <- regmatches(x, gregexpr("`r [^`]+`", x))[[1]]
      out <- c(out, sub("^`r ", "", sub("`$", "", m)))
    }
  }
  out
}

fichiers_app171 <- c("app.R", "global.R", FICHIERS_R)
rmd171 <- "rapport/rapport_climat.Rmd"
fichiers_r171 <- c(fichiers_app171, file.path("rapport", list.files(chemin_ci("rapport"), pattern = "\\.R$")))
fichiers_r171 <- fichiers_r171[file.exists(chemin_ci(fichiers_r171))]

arbre171 <- lapply(fichiers_app171, function(f) parse(text = lire171(f), keep.source = TRUE))
names(arbre171) <- fichiers_app171
donnees171 <- c(lapply(arbre171, getParseData),
                list(rmd = getParseData(parse(text = code_rmd171(rmd171), keep.source = TRUE))))

# Fonctions definies a la racine d'un fichier : nom <- function(...)
definitions171 <- do.call(rbind, lapply(names(arbre171), function(f) {
  lignes <- lapply(as.list(arbre171[[f]]), function(e) {
    if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") && is.name(e[[2]]))
      data.frame(nom = as.character(e[[2]]), fichier = f,
                 fonction = is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")),
                 stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(data.frame(nom = character(), fichier = character(),
                                   fonction = logical())), lignes))
}))
# Les fonctions telles qu'ECRITES dans les sources : test_stations.R remplace
# certaines (mf_altitude, mf_fichiers_dept...) par des doublures a l'execution,
# et ce sont les originales qu'il faut examiner.
fermetures171 <- list()
for (f in names(arbre171)) {
  for (e in as.list(arbre171[[f]])) {
    if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") && is.name(e[[2]]) &&
        is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")))
      fermetures171[[as.character(e[[2]])]] <- eval(e[[3]], globalenv())
  }
}
symboles171 <- unlist(lapply(donnees171, function(pd)
  pd$text[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")]))
chaines171 <- unlist(lapply(donnees171, function(pd)
  gsub("^[\"']|[\"']$", "", pd$text[pd$token == "STR_CONST"])))


# ==============================================================================
groupe("v1.7.1 — code mort et doublons")
# ==============================================================================

fonctions171 <- definitions171[definitions171$fonction & !grepl("^%", definitions171$nom), ]
# Un nom compte comme utilise s'il apparait ailleurs que sur le cote gauche de
# sa propre definition, ou en toutes lettres dans une chaine (do.call("nom")).
appels171 <- vapply(fonctions171$nom, function(n)
  sum(symboles171 == n) - sum(definitions171$nom == n), numeric(1))
morts171 <- fonctions171$nom[appels171 <= 0 & !fonctions171$nom %in% chaines171]
verifier(!length(morts171),
         if (length(morts171)) paste("fonction(s) jamais appelee(s) :", paste(morts171, collapse = ", "))
         else "aucune fonction definie sans etre appelee")

doublons171 <- unique(definitions171$nom[duplicated(definitions171$nom)])
verifier(!length(doublons171),
         if (length(doublons171)) paste("nom defini plusieurs fois a la racine :", paste(doublons171, collapse = ", "))
         else "aucun nom defini deux fois (le second ecraserait le premier au chargement)")

verifier(!exists("moyenne_mobile", inherits = FALSE) && !exists("meta_de", inherits = FALSE),
         "les deux fonctions mortes de la revue (moyenne_mobile, meta_de) sont supprimees")

# Parametres inutilises : un argument que le corps ne lit jamais promet un
# comportement que la fonction n'a pas (ex. texte_source(don, lieu)).
inutiles171 <- character()
for (n in fonctions171$nom) {
  f <- fermetures171[[n]]
  fm <- formals(f)
  if (!length(fm)) next
  lus <- unique(c(all.names(body(f)), unlist(lapply(fm, function(d) if (is.language(d)) all.names(d)))))
  sans <- setdiff(names(fm), c("...", lus))
  if (length(sans)) inutiles171 <- c(inutiles171, sprintf("%s(%s)", n, paste(sans, collapse = ", ")))
}
verifier(!length(inutiles171),
         if (length(inutiles171)) paste("parametre(s) jamais lu(s) :", paste(inutiles171, collapse = " ; "))
         else "aucune fonction ne declare un parametre qu'elle ne lit pas")

verifier(identical(names(formals(texte_source)), "don") &&
           identical(names(formals(ligne_source)), "don") &&
           identical(names(formals(parametres_analyse)), c("ctx", "don")) &&
           identical(names(formals(mf_normaliser)), "brut") &&
           identical(names(formals(meta_donnees)), "don"),
         "signatures allegees : texte_source, ligne_source, parametres_analyse, mf_normaliser, meta_donnees")

# Variables locales assignees puis jamais lues.
if (requireNamespace("codetools", quietly = TRUE)) {
  msgs171 <- character()
  for (n in fonctions171$nom) {
    codetools::checkUsage(fermetures171[[n]], name = n, all = FALSE,
                          suppressLocalUnused = FALSE,
                          report = function(m) msgs171 <<- c(msgs171, m))
  }
  # Seules les variables locales inutilisees nous interessent ici : les
  # fonctions de app.R (ui_construire...) ne sont pas chargees par ces tests,
  # codetools les croirait absentes.
  msgs171 <- grep("local variable|may not be used", msgs171, value = TRUE)
  verifier(!length(msgs171),
           if (length(msgs171)) paste("codetools :", paste(utils::head(msgs171, 3), collapse = " | "))
           else "codetools ne trouve ni variable locale inutilisee ni appel invalide")
}


# ==============================================================================
groupe("v1.7.1 — CONFIG : chaque reglage est lu, chaque lecture est definie")
# ==============================================================================

code171 <- unlist(lapply(c(setdiff(fichiers_r171, "src/config.R"), rmd171), function(f) {
  l <- if (grepl("\\.Rmd$", f)) code_rmd171(f) else lire171(f)
  l[!grepl("^\\s*#", l)]
}))
feuilles171 <- function(x, pref = character()) {
  if (is.list(x) && length(x) && !is.null(names(x)))
    do.call(c, lapply(names(x), function(n) feuilles171(x[[n]], c(pref, n))))
  else list(pref)
}
# references et fenetres : listes nommees dont les NOMS alimentent les menus
# (names(CONFIG$references)) ; leurs elements ne se lisent pas un par un.
dynamiques171 <- c("references", "fenetres")
orphelines171 <- character()
for (p in feuilles171(CONFIG)) {
  if (p[1] %in% dynamiques171) next
  nom <- p[length(p)]
  lu <- any(grepl(sprintf("\\$%s\\b", nom), code171)) || any(grepl(sprintf("\"%s\"", nom), code171)) ||
        any(grepl(sprintf("\\b%s\\s*=", nom), code171))
  if (!lu) orphelines171 <- c(orphelines171, paste(p, collapse = "$"))
}
verifier(!length(orphelines171),
         if (length(orphelines171)) paste("reglage(s) que rien ne lit :", paste(orphelines171, collapse = ", "))
         else "tout reglage de CONFIG est lu quelque part dans le code")

resout171 <- function(ch) {
  p <- strsplit(sub("^CONFIG\\$", "", ch), "\\$")[[1]]; x <- CONFIG
  for (n in p) { if (!is.list(x) || is.null(x[[n]])) return(FALSE); x <- x[[n]] }
  TRUE
}
lus171 <- unique(unlist(regmatches(code171, gregexpr("CONFIG(\\$[A-Za-z_][A-Za-z0-9_]*)+", code171))))
inconnus171 <- lus171[!vapply(lus171, resout171, logical(1))]
verifier(!length(inconnus171),
         if (length(inconnus171)) paste("CONFIG lu mais non defini :", paste(inconnus171, collapse = ", "))
         else "aucune lecture de CONFIG ne vise un reglage absent (elle renverrait NULL en silence)")

# La documentation ne doit pas citer un reglage qui n'existe pas.
readme171 <- paste(lire171("README.md"), collapse = "\n")
cites171 <- unique(unlist(regmatches(readme171, gregexpr("CONFIG(\\$[A-Za-z_][A-Za-z0-9_]*)+", readme171))))
# Reglages que le README presente expressement comme « a ajouter » (feuille de route).
a_venir171 <- character()
cites_faux171 <- setdiff(cites171[!vapply(cites171, resout171, logical(1))], a_venir171)
verifier(!length(cites_faux171),
         if (length(cites_faux171)) paste("README cite un reglage absent :", paste(cites_faux171, collapse = ", "))
         else "tout reglage cite dans le README existe dans CONFIG")

# La police des graphiques n'a qu'une definition.
police171 <- CONFIG$graphiques$police
verifier(is.character(police171) && length(police171) == 1L && grepl("Open Sans", police171) &&
           grepl("sans-serif", police171) && is.null(CONFIG$graphiques$police_secours),
         "CONFIG$graphiques$police est la pile typographique complete (Open Sans, repli, sans-serif)")
src_plot171 <- lire171("src/utils_plot.R")
src_plot171 <- src_plot171[!grepl("^\\s*#", src_plot171)]
verifier(!any(grepl("Open Sans", src_plot171, fixed = TRUE)) &&
           sum(grepl("CONFIG$graphiques$police", src_plot171, fixed = TRUE)) >= 4L,
         "utils_plot.R lit la police dans CONFIG, sans la recopier")


# ==============================================================================
groupe("v1.7.1 — feuille de style et paquets")
# ==============================================================================

css171 <- paste(lire171("www/climafor.css"), collapse = "\n")
css171 <- gsub("/\\*.*?\\*/", "", css171, perl = TRUE)
classes171 <- unique(gsub("^\\.", "", unlist(regmatches(css171, gregexpr("\\.cf-[A-Za-z0-9_-]+", css171)))))
code_ui171 <- paste(unlist(lapply(c(fichiers_r171, "www/app.js", "www/carte/carte.js"), lire171)), collapse = "\n")
jetons171 <- unique(unlist(regmatches(code_ui171, gregexpr("cf-[A-Za-z0-9_-]+", code_ui171))))
# Une classe est employee si son nom apparait dans le code R/JS, ou si le code
# en construit la fin (paste0("cf-badge--n", niveau) pour cf-badge--n1, n2...).
construits171 <- jetons171[grepl("(--n|-)$", jetons171)]
employee171 <- vapply(classes171, function(c)
  c %in% jetons171 || any(startsWith(c, construits171)), logical(1))
verifier(all(employee171),
         if (!all(employee171)) paste("classe(s) CSS sans element :", paste(classes171[!employee171], collapse = ", "))
         else "toute classe .cf-* de la feuille de style est employee par le code")

variables171 <- unique(regmatches(css171, gregexpr("--cf-[A-Za-z0-9_-]+(?=\\s*:)", css171, perl = TRUE))[[1]])
lues171 <- unique(sub("^var\\(", "", unlist(regmatches(css171, gregexpr("var\\(--cf-[A-Za-z0-9_-]+", css171)))))
verifier(!length(setdiff(variables171, lues171)),
         if (length(setdiff(variables171, lues171))) paste("variable(s) CSS jamais lue(s) :", paste(setdiff(variables171, lues171), collapse = ", "))
         else "toute variable CSS --cf-* definie est lue")

# Tout paquet appele par pkg:: est declare dans DESCRIPTION (hors paquets de base R).
base171 <- c("base", "stats", "utils", "tools", "grid", "methods", "graphics", "grDevices",
             "parallel", "compiler", "datasets", "splines", "stats4", "tcltk")
appeles171 <- unique(sub("::$", "", unlist(regmatches(code171, gregexpr("\\b[A-Za-z][A-Za-z0-9.]+::", code171)))))
appeles171 <- setdiff(appeles171, base171)
desc171 <- read.dcf(chemin_ci("DESCRIPTION"), fields = c("Imports", "Suggests"))
declares171 <- trimws(sub("\\s*\\(.*$", "", strsplit(paste(desc171, collapse = ","), "[,\n]")[[1]]))
declares171 <- declares171[nzchar(declares171) & !is.na(declares171)]
imports171 <- trimws(sub("\\s*\\(.*\\)", "", strsplit(read.dcf(chemin_ci("DESCRIPTION"), fields = "Imports")[1, 1], ",")[[1]]))
libs171 <- unique(sub("^library\\((\\w+)\\).*$", "\\1", grep("^library\\(\\w+\\)", lire171("global.R"), value = TRUE)))
verifier(!length(setdiff(imports171, c(libs171, appeles171))),
         if (length(setdiff(imports171, c(libs171, appeles171)))) paste("declare mais jamais utilise :", paste(setdiff(imports171, c(libs171, appeles171)), collapse = ", "))
         else "chaque paquet de DESCRIPTION/Imports est charge ou appele")
verifier(!length(setdiff(appeles171, declares171)),
         if (length(setdiff(appeles171, declares171))) paste("paquet utilise mais non declare :", paste(setdiff(appeles171, declares171), collapse = ", "))
         else "tout paquet appele par pkg:: est declare dans DESCRIPTION")


# ==============================================================================
groupe("v1.7.1 — typographie des nombres affiches (E6)")
# ==============================================================================

# Les coordonnees s'affichaient « 43.3500 N · 1.4727 E » au milieu d'un texte en
# virgule decimale (le README annoncait pourtant « 0 point decimal »).
verifier(identical(fmt_fr(1.4727, 4), "1,4727") && identical(fmt_fr(43.35, 4), "43,3500"),
         "fmt_fr(coordonnee, 4) donne la virgule decimale")
verifier(identical(fmt_fr(-4.5, 4), "\u22124,5000"),
         "une longitude ouest prend le vrai signe moins")
# Tout sprintf a decimales qui n'est pas une valeur technique doit passer par
# fmt_fr(). Liste des emplois techniques admis (cle de cache, parametres d'URL,
# cle de comparaison, format converti ensuite) : en ajouter un est un choix a faire
# en connaissance de cause, pas un oubli.
brut171 <- unlist(lapply(c(fichiers_r171, rmd171), function(f) {
  l <- if (grepl("\\.Rmd$", f)) code_rmd171(f) else lire171(f)
  l <- l[!grepl("^\\s*#", l)]
  l[grepl("%[0-9+]*\\.[1-9]f", l)]
}))
admis171 <- c('x = sprintf("%.4f", etat$lon), y = sprintf("%.4f", etat$lat)',   # lien partageable
              'sub(".", ",", sprintf("%.1f Mo", octets / 1024^2), fixed = TRUE)', # converti
              'cle <- sprintf("%.1f_%.1f", lon, lat)',                           # cle de cache
              'sprintf("%.4f|%.4f|%s", p$lon, p$lat,',                           # cle interne
              'ligne <- function(lib, unite, val, st, sens = "neutre", format = "%.1f",', # format passe a fmt_fr
              'stat_an(ref_tronq, "swi", moy), "sec", "%.2f",')                  # idem
inattendus171 <- brut171[!vapply(brut171, function(l) any(vapply(admis171, function(a) grepl(a, l, fixed = TRUE), logical(1))), logical(1))]
verifier(!length(inattendus171),
         if (length(inattendus171)) paste("sprintf a decimales hors liste :", paste(trimws(inattendus171), collapse = " | "))
         else "aucun affichage de nombre a decimales ne contourne fmt_fr() (virgule decimale)")


# ==============================================================================
groupe("v1.7.1 — scripts de diagnostic : charger tout ce que le code appelle")
# ==============================================================================

# diagnostic.R et diagnostic_stations.R chargent a la main quelques fichiers de
# src/ (pas global.R, trop lourd). Quand la v1.7 a sorti le cache dans
# utils_cache.R, ils n'ont pas ete mis a jour : le diagnostic lance PRECISEMENT
# quand l'acces aux donnees pose probleme s'arretait sur « could not find
# function "cache_nom_sur" », et masquait la vraie cause. Ce test charge, dans un
# R vierge, les fichiers que chaque script source, puis cherche toute fonction
# appelee qui ne soit definie nulle part.
verif171 <- tempfile("verif_diag_", fileext = ".R")
writeLines(c(
  'args <- commandArgs(TRUE); l <- readLines(args[1], encoding = "UTF-8")',
  'ci <- function(x) x',
  'fich <- sub(\'.*source\\\\(ci\\\\("([^"]+)"\\\\).*\', "\\\\1", grep(\'source\\\\(ci\\\\("\', l, value = TRUE))',
  'suppressMessages(library(httr2))',
  'for (f in fich) suppressMessages(source(f, encoding = "UTF-8"))',
  'manq <- character()',
  'for (n in ls(globalenv())) { f <- get(n, globalenv()); if (!is.function(f)) next',
  '  for (x in codetools::findGlobals(f, merge = FALSE)$functions)',
  '    if (!exists(x, mode = "function")) manq <- c(manq, paste0(n, " -> ", x)) }',
  'cat("FICHIERS", basename(fich), "\\n"); cat(unique(manq), sep = "\\n")'), verif171)
if (requireNamespace("codetools", quietly = TRUE)) {
  for (script171 in c("diagnostic.R", "diagnostic_stations.R")) {
    sortie171 <- tryCatch(
      suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                               c("--vanilla", shQuote(verif171), shQuote(chemin_ci(script171))),
                               stdout = TRUE, stderr = TRUE)),
      error = function(e) paste("ERREUR", conditionMessage(e)))
    fichiers_diag171 <- sub("^FICHIERS ", "", grep("^FICHIERS ", sortie171, value = TRUE))
    reste171 <- setdiff(sortie171[nzchar(sortie171)], grep("^FICHIERS ", sortie171, value = TRUE))
    verifier(length(fichiers_diag171) == 1L && grepl("utils_cache.R", fichiers_diag171, fixed = TRUE) && !length(reste171),
             if (length(reste171)) paste(script171, ": fonction(s) non definie(s) :", paste(utils::head(reste171, 4), collapse = " | "))
             else if (!length(fichiers_diag171)) paste(script171, ": analyse impossible :", paste(utils::head(sortie171, 2), collapse = " | "))
             else paste(script171, "charge utils_cache.R et ne laisse aucune fonction appelee sans definition"))
  }
}
unlink(verif171)


# ==============================================================================
groupe("v1.7.3 — hotes de telechargement : l'hote REEL de Meteo-France")
# ==============================================================================

# La v1.7 n'autorisait que data.gouv.fr, d'apres l'adresse que je croyais
# connaitre : toutes les adresses de la v1.7 a la v1.7.2 etaient des exemples
# ecrits de memoire, aucune n'avait ete observee. Les vrais fichiers sont sur le
# stockage objet de Meteo-France (message d'erreur releve sur un poste, 2 nov. 2026).
reelles173 <- c(
  "https://meteofrance.s3.sbg.io.cloud.ovh.net/data/synchro_ftp/BASE/QUOT/Q_31_latest-2025-2026_RR-T-Vent.csv.gz",
  "https://meteofrance.s3.sbg.io.cloud.ovh.net/data/synchro_ftp/BASE/QUOT/Q_32_latest-2025-2026_RR-T-Vent.csv.gz")
verifier(all(vapply(reelles173, mf_url_autorisee, logical(1))),
         "les adresses reelles des departements 31 et 32 (stockage Meteo-France) sont acceptees")
# On n'ouvre que CE seau, pas le fournisseur : un autre client d'OVHcloud, un
# sous-domaine du seau, un domaine qui le contient ou un autre port restent refuses.
refusees173 <- c(
  "https://autre-client.s3.sbg.io.cloud.ovh.net/data/x.csv.gz",
  "https://s3.sbg.io.cloud.ovh.net/meteofrance/data/x.csv.gz",
  "https://meteofrance.s3.gra.io.cloud.ovh.net/data/x.csv.gz",
  "https://xmeteofrance.s3.sbg.io.cloud.ovh.net/data/x.csv.gz",
  "https://meteofrance.s3.sbg.io.cloud.ovh.net.autre.com/data/x.csv.gz",
  "https://meteofrance.s3.sbg.io.cloud.ovh.net@autre.com/data/x.csv.gz",
  "http://meteofrance.s3.sbg.io.cloud.ovh.net/data/x.csv.gz",
  "https://meteofrance.s3.sbg.io.cloud.ovh.net:8443/data/x.csv.gz")
verifier(!any(vapply(refusees173, mf_url_autorisee, logical(1))),
         "un autre seau OVHcloud, une autre region, un faux hote, HTTP ou un autre port : refuses")
trop_larges173 <- c("ovh.net", "cloud.ovh.net", "io.cloud.ovh.net", "s3.sbg.io.cloud.ovh.net", "gouv.fr", "fr")
verifier(!any(tolower(CONFIG$stations$hotes_autorises) %in% trop_larges173),
         "la liste n'autorise aucun domaine entier d'hebergeur (seulement le seau de Meteo-France)")
# Le test de mf_telecharger() avec un hote refuse reste refuse : la regle n'est pas affaiblie.
verifier(grepl("refus", tryCatch(mf_telecharger_origine("https://autre-client.s3.sbg.io.cloud.ovh.net/x.csv.gz"),
                                 error = function(e) conditionMessage(e))),
         "un telechargement vers un autre hote est toujours refuse avant toute requete")


# ==============================================================================
groupe("v1.7.1 — version et casse des fichiers")
# ==============================================================================

v171 <- as.numeric_version(CONFIG$app$version)
verifier(v171 >= "1.7.1", "la version est au moins 1.7.1")
desc171v <- read.dcf(chemin_ci("DESCRIPTION"))
verifier(identical(unname(desc171v[1, "Version"]), CONFIG$app$version),
         "DESCRIPTION et CONFIG portent la meme version")

# corriger_casse.R doit connaitre chaque fichier du projet : un fichier absent de
# sa liste ne serait pas remis en minuscules (ou en casse attendue) apres une
# copie qui l'a renomme.
casse171 <- paste(lire171("corriger_casse.R"), collapse = "\n")
attendus171 <- unique(unlist(regmatches(casse171, gregexpr("\"[A-Za-z0-9_./-]+\"", casse171))))
attendus171 <- gsub("\"", "", attendus171)
sur_disque171 <- list.files(".", recursive = TRUE, all.files = FALSE)
# config/acces_local.csv : liste personnelle et facultative (hors depot) ; elle n'est pas livree.
sur_disque171 <- sur_disque171[!grepl("^cache/|^rsconnect/|^\\.|\\.Rhistory$|^config/acces_local\\.csv$", sur_disque171)]
manquants171 <- sur_disque171[!vapply(sur_disque171, function(f) any(tolower(attendus171) == tolower(f)), logical(1))]
verifier(!length(manquants171),
         if (length(manquants171)) paste("absent de corriger_casse.R :", paste(manquants171, collapse = ", "))
         else "corriger_casse.R liste tous les fichiers du projet")
