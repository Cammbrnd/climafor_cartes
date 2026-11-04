# ==============================================================================
# tests/test_v17.R — SECURITE DU CODE, VERSION 1.7 (audit, partie 8)
# ------------------------------------------------------------------------------
#   V1  le cache est borne : purge par age et par taille ;
#   V2  limites d'usage par fenetre glissante (session, processus) ;
#   V3  mention de confidentialite, position arrondie ;
#   V4  (deja corrige en v1.2 : contexte du rapport echappe) ;
#   V5  donnees tierces echappees avant insertion dans la page ;
#   V6  telechargements : domaines autorises, HTTPS, taille plafonnee ;
#   V7  cache en format « sans code » controle a la relecture ;
#   V8  bornes des seuils et longueur du contexte verifiees cote serveur.
#
# Ce que ces tests ne voient pas : le comportement d'un serveur deploye sous
# charge, les en-tetes HTTP de l'hebergeur, un client qui forge des messages
# WebSocket (on teste les fonctions qui recoivent ces valeurs, pas le reseau),
# et les redirections reelles de data.gouv.fr (aucun acces reseau ici).
# ==============================================================================

html_de17 <- function(ui) paste(as.character(htmltools::renderTags(ui)$html), collapse = "\n")
lire17 <- function(f) paste(readLines(chemin_ci(f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
code_sans_commentaires17 <- function(f) {
  l <- readLines(chemin_ci(f), warn = FALSE, encoding = "UTF-8")
  paste(l[!grepl("^\\s*#", l)], collapse = "\n")
}

# Dossier de cache isole pour ces tests (le vrai n'est pas touche).
cache_avant17  <- CONFIG$cache
dossier17      <- file.path(tempdir(), "cache_v17")
unlink(dossier17, recursive = TRUE)
CONFIG$cache$repertoire <- dossier17
CONFIG$cache$actif      <- TRUE

tableau17 <- function(n = 5L) {
  data.frame(
    id = c("31555001", "é-ü\"quote\\", rep("x", max(0L, n - 2L)))[seq_len(n)],
    x = c(1.5, NA, 3.25, -0.001, 1e-9)[seq_len(n)],
    k = c(1L, NA, 3L, 4L, 5L)[seq_len(n)],
    ok = c(TRUE, NA, FALSE, TRUE, FALSE)[seq_len(n)],
    d = as.Date(c("2020-02-29", NA, "1959-08-01", "2026-10-31", "2000-01-01"))[seq_len(n)],
    stringsAsFactors = FALSE)
}


# ==============================================================================
groupe("v1.7 — V5 : donnees tierces echappees")
# ==============================================================================

h <- as.character(loc_resultat_html("<script>alert(1)</script>", "31<000", "A&B <b>"))
verifier(!grepl("<script", h, fixed = TRUE) && grepl("&lt;script&gt;", h, fixed = TRUE),
         "un nom de commune contenant <script> s'affiche comme du texte")
verifier(!grepl("<b>", h, fixed = TRUE) && grepl("A&amp;B", h, fixed = TRUE),
         "le departement est echappe aussi (esperluette et balise)")
verifier(!grepl("31<000", h, fixed = TRUE) && grepl("31&lt;000", h, fixed = TRUE),
         "le code postal est echappe aussi")
verifier(grepl("<strong>", h, fixed = TRUE) && grepl("cf-resultat__meta", h, fixed = TRUE),
         "la mise en forme ecrite par l'application (gras, classe) est conservee")
verifier(grepl("Saint-Étienne", as.character(loc_resultat_html("Saint-Étienne", "42000", "Loire")), fixed = TRUE),
         "un nom ordinaire avec accents n'est pas deforme")
verifier(is.character(as.character(loc_resultat_html(NA, NULL, character(0)))),
         "valeurs absentes ou NA : pas d'erreur")

verifier(grepl("htmlEscape(p$nom)", code_sans_commentaires17("src/mod_localisation.R"), fixed = TRUE),
         "l'etiquette du marqueur de la carte (HTML pour Leaflet) est echappee")

# La liste elle-meme, rendue par le module avec une reponse d'API hostile.
orig_chercher17 <- geo_chercher_communes
geo_chercher_communes <- function(requete, limite = 10) {
  data.frame(nom = c("<img src=x onerror=alert(1)>", "Gagnac"),
             code_insee = c("1", "2"), cp = c("31000", "31150"),
             departement = c("Haute-Garonne", "<svg/onload=alert(2)>"),
             lon = c(1.4, 1.3), lat = c(43.6, 43.7), population = c(1L, 2L),
             stringsAsFactors = FALSE)
}
html_liste17 <- tryCatch({
  shiny::testServer(mod_localisation_server, {
    session$setInputs(commune = "gag", chercher = 1)
    h17 <<- as.character(output$resultats$html)
  })
  h17
}, error = function(e) paste("erreur :", conditionMessage(e)))
geo_chercher_communes <- orig_chercher17
verifier(grepl("&lt;img", html_liste17, fixed = TRUE) && !grepl("<img", html_liste17, fixed = TRUE),
         "la liste rendue par le module n'insere pas de balise venue de l'API (nom)")
verifier(!grepl("<svg", html_liste17, fixed = TRUE),
         "la liste rendue par le module n'insere pas de balise venue de l'API (departement)")

verifier(identical(loc_recherche_nettoyee(strrep("a", 5000)), strrep("a", LOC_RECHERCHE_MAX)),
         "le texte de recherche est borne en longueur")
verifier(identical(loc_recherche_nettoyee(NULL), "") && identical(loc_recherche_nettoyee(NA), "") &&
           identical(loc_recherche_nettoyee(list("a", "b")), ""),
         "une entree qui n'est pas un texte donne une recherche vide")
verifier(identical(loc_recherche_nettoyee("  Gagnac\n"), "Gagnac"),
         "espaces et retours a la ligne de bord retires")
verifier(identical(loc_recherche_nettoyee(c("un", "deux")), "un"),
         "un vecteur de plusieurs textes : le premier seulement")

verifier(loc_point_valide(1.44, 43.6) && loc_point_valide(-5.9, 41.1) && loc_point_valide(9.9, 51.9),
         "des points de France metropolitaine sont acceptes (bornes incluses)")
verifier(!loc_point_valide(2.35, 52.5) && !loc_point_valide(55.5, -21.1) && !loc_point_valide(-61.5, 16.2),
         "hors metropole (nord, La Reunion, Guadeloupe) : refuse")
verifier(!loc_point_valide(NA, 45) && !loc_point_valide(Inf, 45) && !loc_point_valide("abc", 45) &&
           !loc_point_valide(NULL, 45) && !loc_point_valide(list(1, 2), 45) &&
           !loc_point_valide(c(1, 2), 45) && !loc_point_valide(1e300, 45),
         "valeurs forgees (NA, infini, texte, NULL, liste, vecteur, enorme) : refusees sans erreur")

# Position envoyee par le navigateur : arrondie avant tout appel distant, et
# refusee si elle est hors de France ou forgee.
appels17 <- list()
orig_pt17 <- geo_commune_par_point
geo_commune_par_point <- function(lon, lat) { appels17[[length(appels17) + 1L]] <<- c(lon, lat); NULL }
res17 <- tryCatch({
  shiny::testServer(mod_localisation_server, {
    session$setInputs(position = list(lon = 1.23456789, lat = 43.98765432, erreur = FALSE))
    try(session$flushReact(), silent = TRUE)
  })
  "ok"
}, error = function(e) conditionMessage(e))
n_ok17 <- length(appels17)
tryCatch(shiny::testServer(mod_localisation_server, {
  session$setInputs(position = list(lon = 100, lat = 43.9, erreur = FALSE))
}), error = function(e) NULL)
tryCatch(shiny::testServer(mod_localisation_server, {
  session$setInputs(position = list(lon = "<script>", lat = "x", erreur = FALSE))
}), error = function(e) NULL)
geo_commune_par_point <- orig_pt17
verifier(n_ok17 >= 1L && isTRUE(all.equal(appels17[[1]], c(1.2346, 43.9877))),
         "la position est arrondie a 4 decimales AVANT l'appel a l'API Geo")
verifier(length(appels17) == n_ok17,
         "une position hors de France ou forgee n'est jamais transmise a l'API Geo")


# ==============================================================================
groupe("v1.7 — V8 : bornes serveur des seuils et du contexte")
# ==============================================================================

d17 <- DESCRIPTIF_SEUILS
id1 <- d17$id[1]; mn1 <- d17$min[1]; mx1 <- d17$max[1]
defaut1 <- CONFIG$seuils[[id1]]
valeurs17 <- function(v) stats::setNames(list(v), id1)

r <- seuils_depuis_champs(valeurs17((mn1 + mx1) / 2))
verifier(identical(r$seuils[[id1]], (mn1 + mx1) / 2) && !length(r$refuses),
         "une valeur dans les bornes est prise en compte")
verifier(identical(seuils_depuis_champs(valeurs17(mn1))$seuils[[id1]], as.numeric(mn1)) &&
           identical(seuils_depuis_champs(valeurs17(mx1))$seuils[[id1]], as.numeric(mx1)),
         "les bornes elles-memes sont admises")
r <- seuils_depuis_champs(valeurs17(mx1 + 1000))
verifier(identical(r$seuils[[id1]], defaut1) && identical(r$refuses, id1),
         "au-dessus du maximum : valeur par defaut ET signalement")
r <- seuils_depuis_champs(valeurs17(mn1 - 1000))
verifier(identical(r$seuils[[id1]], defaut1) && identical(r$refuses, id1),
         "sous le minimum : valeur par defaut ET signalement")
ignorees_ok <- vapply(list(NULL, NA, NA_real_, Inf, -Inf, NaN, "abc", c(1, 2), list(1), TRUE),
                      function(v) {
  r <- seuils_depuis_champs(valeurs17(v))
  identical(r$seuils[[id1]], defaut1) && !length(r$refuses)
}, logical(1))
verifier(all(ignorees_ok),
         "champ vide, NA, infini, texte, vecteur, liste, logique : valeur par defaut, sans erreur")
verifier(!identical(names(seuils_depuis_champs(list(inconnu = 5))$seuils), NULL) &&
           !("inconnu" %in% names(seuils_depuis_champs(list(inconnu = 5))$seuils)),
         "un identifiant de seuil inconnu est ignore")
verifier(identical(seuils_depuis_champs(list())$seuils, CONFIG$seuils),
         "aucune valeur recue : les seuils par defaut, tels quels")
verifier(all(is.finite(d17$min) & is.finite(d17$max) & d17$min < d17$max),
         "chaque seuil a des bornes finies et ordonnees")
verifier(all(d17$id %in% names(CONFIG$seuils)),
         "chaque seuil du descriptif existe dans CONFIG")
ok_dans_bornes <- vapply(seq_len(nrow(d17)), function(i) {
  v <- CONFIG$seuils[[d17$id[i]]]
  is.numeric(v) && v >= d17$min[i] && v <= d17$max[i]
}, logical(1))
verifier(all(ok_dans_bornes),
         "chaque valeur par defaut respecte ses propres bornes (sinon elle serait refusee)")

# Le module : une valeur hors bornes ne passe pas, et l'etat le dit.
mod_hors <- tryCatch({
  shiny::testServer(mod_parametres_server, {
    champs <- stats::setNames(list(mx1 + 1000), paste0("seuil_", id1))
    do.call(session$setInputs, champs)
    sv17 <<- session$returned$seuils()
    etat17 <<- html_de17(output$etat_seuils)
  })
  list(seuils = sv17, etat = etat17)
}, error = function(e) list(seuils = NULL, etat = conditionMessage(e)))
verifier(identical(mod_hors$seuils[[id1]], defaut1),
         "module Parametres : une valeur hors bornes envoyee au serveur n'est pas utilisee")
verifier(grepl("hors limites", mod_hors$etat, fixed = TRUE) && grepl(d17$libelle[1], mod_hors$etat, fixed = TRUE),
         "module Parametres : l'etat des seuils nomme le champ refuse")

verifier(nchar(contexte_borne(strrep("a", 1e6))) == CONFIG$rapport$contexte_max + 1L,
         "le contexte est borne a la reception (limite + 1 caractere)")
verifier(isTRUE(nettoyer_contexte(contexte_borne(strrep("a", 1e6)))$tronque),
         "un contexte borne reste signale comme tronque dans le rapport")
verifier(identical(contexte_borne("court"), "court") && identical(contexte_borne(NULL), "") &&
           identical(contexte_borne(NA), "") && identical(contexte_borne(list("x")), ""),
         "contexte court, absent, NA ou liste : pas d'erreur, pas d'alteration")
verifier(grepl("contexte_borne(input$contexte)", code_sans_commentaires17("src/mod_rapport.R"), fixed = TRUE),
         "le module Rapport borne le contexte avant de le copier dans l'entree du rapport")


# ==============================================================================
groupe("v1.7 — V6 : telechargements limites aux domaines autorises")
# ==============================================================================

verifier(is.character(CONFIG$stations$hotes_autorises) && "data.gouv.fr" %in% CONFIG$stations$hotes_autorises,
         "la liste des domaines autorises est dans la configuration")
acceptees <- c("https://www.data.gouv.fr/fr/datasets/r/abc-123",
               "https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_31_latest-2025-2026_RR-T-Vent.csv.gz",
               "https://data.gouv.fr/x", "HTTPS://WWW.DATA.GOUV.FR/x", "https://www.data.gouv.fr:443/x")
verifier(all(vapply(acceptees, mf_url_autorisee, logical(1))),
         "adresses data.gouv.fr (domaine, sous-domaines, casse, port 443) : acceptees")
refusees <- c(
  "http://www.data.gouv.fr/x",                 # pas HTTPS
  "https://data.gouv.fr.autre.com/x",          # domaine plus long
  "https://mondata.gouv.fr/x",                 # meme fin, autre domaine
  "https://data.gouv.fr@autre.com/x",          # identifiants avant l'hote reel
  "https://user:mdp@www.data.gouv.fr/x",       # identifiants dans l'adresse
  "https://www.data.gouv.fr:8443/x",           # autre port
  "https://autre.com/data.gouv.fr",            # domaine dans le chemin
  "https://autre.com/?u=https://data.gouv.fr", # domaine dans la requete
  "https://data.gouv.fr./x",                   # point final
  "ftp://www.data.gouv.fr/x", "file:///etc/passwd", "//www.data.gouv.fr/x",
  "javascript:alert(1)", "https://127.0.0.1/x", "https://[::1]/x", "https://localhost/x",
  "https://www.data.gouv.fr/x y", "https://www.data.gouv.fr/x\n", "https://www.data.gouv.fr\\@autre.com/",
  paste0("https://www.data.gouv.fr/", strrep("a", 2100)), "", NA_character_)
refus_ok <- vapply(refusees, mf_url_autorisee, logical(1))
if (any(refus_ok)) cat(sprintf("   acceptee a tort : %s\n", paste(substr(refusees[refus_ok], 1, 60), collapse = " | ")))
verifier(!any(refus_ok), "adresses ressemblantes, mal formees ou non HTTPS : toutes refusees")
verifier(!mf_url_autorisee(NULL) && !mf_url_autorisee(character(0)) && !mf_url_autorisee(list("https://www.data.gouv.fr/x")[[1]][0]),
         "adresse absente : refusee sans erreur")
verifier(!mf_url_autorisee("https://www.data.gouv.fr/x", autorises = character(0)),
         "liste de domaines vide : tout est refuse (jamais « tout est permis »)")

# test_stations.R remplace mf_telecharger par une lecture locale : on remet le vrai.
if (exists("mf_telecharger_origine")) mf_telecharger <- mf_telecharger_origine
fichiers_avant17 <- list.files(tempdir(), pattern = "\\.csv\\.gz$")
err17 <- tryCatch(mf_telecharger("http://www.data.gouv.fr/x.csv.gz"), error = function(e) conditionMessage(e))
verifier(is.character(err17) && grepl("refusée", err17),
         "mf_telecharger refuse une adresse non autorisee AVANT toute requete")
verifier(identical(list.files(tempdir(), pattern = "\\.csv\\.gz$"), fichiers_avant17),
         "un telechargement refuse ne laisse aucun fichier temporaire")
verifier(!grepl("\n", mf_url_pour_message("https://x/\ny"), fixed = TRUE) &&
           nchar(mf_url_pour_message(strrep("a", 500))) <= 100L,
         "l'adresse citee dans un message est raccourcie et sans retour a la ligne")

opts17 <- mf_options_telechargement(500)
verifier(is.list(opts17) && identical(opts17$maxredirs, as.integer(CONFIG$stations$redirections_max)),
         "les redirections sont bornees")
verifier(identical(opts17$redir_protocols_str, "https") || identical(opts17$redir_protocols, 2L),
         "les redirections ne peuvent mener qu'a du HTTPS")
verifier(isTRUE(all.equal(opts17$maxfilesize_large, 500 * 1024^2)),
         "la taille maximale est transmise a curl (500 Mo par defaut)")
verifier(is.null(mf_options_telechargement(0)$maxfilesize_large),
         "un plafond de 0 le desactive explicitement")

f_ok <- tempfile(fileext = ".csv.gz"); writeBin(as.raw(rep(1L, 1000)), f_ok)
verifier(identical(mf_verifier_telechargement(f_ok, "https://www.data.gouv.fr/x", 1), invisible(f_ok)) && file.exists(f_ok),
         "un fichier petit, d'une adresse finale autorisee : accepte")
f_gros <- tempfile(fileext = ".csv.gz"); writeBin(raw(2 * 1024^2), f_gros)
e1 <- tryCatch(mf_verifier_telechargement(f_gros, "https://www.data.gouv.fr/x", 1), error = function(e) conditionMessage(e))
verifier(is.character(e1) && grepl("trop volumineux", e1) && !file.exists(f_gros),
         "un fichier plus gros que le plafond est refuse ET supprime")
f_redir <- tempfile(fileext = ".csv.gz"); writeBin(as.raw(1:10), f_redir)
e2 <- tryCatch(mf_verifier_telechargement(f_redir, "https://autre.example/x", 500), error = function(e) conditionMessage(e))
verifier(is.character(e2) && grepl("hôte non autorisé", e2) && !file.exists(f_redir),
         "une redirection finale hors des domaines autorises est refusee ET le fichier supprime")
e3 <- tryCatch(mf_verifier_telechargement(tempfile(), "https://www.data.gouv.fr/x", 1), error = function(e) conditionMessage(e))
verifier(is.character(e3) && grepl("introuvable", e3),
         "un fichier absent apres telechargement est une erreur claire")


# ==============================================================================
groupe("v1.7 — V7 : cache en format « sans code », controle a la relecture")
# ==============================================================================

f1 <- file.path(dossier17, "safran_45_4_2020_s3.json.gz")
t1 <- tableau17()
attr(t1, "secours") <- TRUE
verifier(isTRUE(cache_ecrire_table(t1, f1)) && file.exists(f1),
         "un tableau (texte, nombre, entier, logique, date, NA) s'ecrit")
b1 <- cache_lire_table(f1)
verifier(is.data.frame(b1) && identical(names(b1), names(t1)) && nrow(b1) == nrow(t1),
         "il se relit avec les memes colonnes et le meme nombre de lignes")
verifier(identical(vapply(b1, function(x) class(x)[1], ""), vapply(t1, function(x) class(x)[1], "")),
         "les classes sont rendues a l'identique (character, numeric, integer, logical, Date)")
verifier(isTRUE(all.equal(b1, t1, check.attributes = FALSE)),
         "les valeurs sont rendues a l'identique, NA, accents, guillemets et barre oblique inverse compris")
verifier(identical(is.na(b1$x), is.na(t1$x)) && identical(is.na(b1$d), is.na(t1$d)) &&
           identical(is.na(b1$ok), is.na(t1$ok)) && identical(is.na(b1$k), is.na(t1$k)),
         "les NA restent des NA, colonne par colonne")
verifier(identical(attr(b1, "secours"), TRUE), "l'attribut « secours » est conserve")
verifier(b1$x[5] == 1e-9 && b1$x[4] == -0.001,
         "les petits nombres gardent leur precision")

z0 <- tableau17()[0, ]
fz <- file.path(dossier17, "dept_00.json.gz")
cache_ecrire_table(z0, fz); bz <- cache_lire_table(fz)
verifier(is.data.frame(bz) && nrow(bz) == 0L && identical(vapply(bz, function(x) class(x)[1], ""), vapply(z0, function(x) class(x)[1], "")),
         "un tableau vide garde ses colonnes et leurs classes")
f_un <- file.path(dossier17, "dept_01.json.gz")
cache_ecrire_table(tableau17(1L), f_un); bu <- cache_lire_table(f_un)
verifier(is.data.frame(bu) && nrow(bu) == 1L && bu$id == "31555001", "un tableau d'une seule ligne se relit")

verifier(!any(grepl("^\\.cf_tmp_", list.files(dossier17, all.files = TRUE))),
         "l'ecriture passe par un fichier temporaire renomme : aucun reste apres succes")

# Ce qui ne doit pas pouvoir entrer dans le cache
n_avant <- length(list.files(dossier17, all.files = TRUE))
refus_ecriture <- list(
  liste    = data.frame(a = 1:2)[, "a", drop = FALSE],
  facteur  = data.frame(a = factor(c("u", "v"))),
  temps    = data.frame(a = as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC")),
  colliste = I(list(1, 2)),
  noms     = structure(list(1:2, 3:4), names = c("a b", "c"), class = "data.frame", row.names = 1:2),
  doublons = structure(list(1:2, 3:4), names = c("a", "a"), class = "data.frame", row.names = 1:2))
refus_ecriture$liste$a <- list(1, 2)
refus_ecriture$colliste <- data.frame(id = 1:2); refus_ecriture$colliste$l <- list(1, "a")
verifier(all(vapply(refus_ecriture, function(df)
  inherits(try(cache_ecrire_table(df, file.path(dossier17, "dept_99.json.gz")), silent = TRUE), "try-error"),
  logical(1))),
  "colonne liste, facteur, date-heure, noms de colonnes douteux ou doubles : refuses a l'ecriture")
verifier(inherits(try(cache_ecrire_table(list(a = 1), file.path(dossier17, "dept_99.json.gz")), silent = TRUE), "try-error"),
         "autre chose qu'un tableau : refuse")
verifier(length(list.files(dossier17, all.files = TRUE)) == n_avant,
         "un refus ne laisse ni fichier final ni fichier temporaire")

# Fichiers forges ou abimes
ecrire_gz17 <- function(chemin, lignes) {
  con <- gzfile(chemin, "wb"); writeLines(lignes, con, useBytes = TRUE); close(con)
}
entete_ok17 <- sprintf('{"format":"climafor-cache","schema":%d,"n":2,"classes":{"a":"numeric"},"attributs":[]}', CONFIG$cache$schema)
cas17 <- list(
  "format inconnu"    = c('{"format":"autre","schema":3,"n":1,"classes":{"a":"numeric"},"attributs":[]}', '{"a":[1]}'),
  "classe non admise" = c(sub("numeric", "function", entete_ok17), '{"a":[1,2]}'),
  "classe expression" = c(sub("numeric", "call", entete_ok17), '{"a":[1,2]}'),
  "colonne trop courte" = c(entete_ok17, '{"a":[1]}'),
  "colonnes autres que declarees" = c(entete_ok17, '{"b":[1,2]}'),
  "texte dans une colonne numerique" = c(entete_ok17, '{"a":[1,"x"]}'),
  "objet imbrique" = c(entete_ok17, '{"a":[{"x":1},{"y":2}]}'),
  "nombre de lignes negatif" = c(sub('"n":2', '"n":-2', entete_ok17), '{"a":[1,2]}'),
  "nombre de lignes enorme"  = c(sub('"n":2', '"n":99999999999', entete_ok17), '{"a":[1,2]}'),
  "schema different" = c(sub(sprintf('"schema":%d', CONFIG$cache$schema), '"schema":99', entete_ok17), '{"a":[1,2]}'),
  "date impossible" = c(sub('"numeric"', '"Date"', entete_ok17), '{"a":["2020-13-45","2020-01-01"]}'),
  "entier non entier" = c(sub('"numeric"', '"integer"', entete_ok17), '{"a":[1.5,2]}'),
  "nom de colonne douteux" = c('{"format":"climafor-cache","schema":3,"n":1,"classes":{"a b;":"numeric"},"attributs":[]}', '{"a b;":[1]}'),
  "ligne en trop" = c(entete_ok17, '{"a":[1,2]}', 'x'),
  "JSON invalide" = c(entete_ok17, '{"a":[1,2'),
  "fichier vide" = character(0))
forges_ok <- vapply(names(cas17), function(nom) {
  f <- file.path(dossier17, "serie_forge.json.gz")
  ecrire_gz17(f, cas17[[nom]])
  r <- cache_lire_table(f)
  ok <- is.null(r) && !file.exists(f)
  if (!ok) cat(sprintf("   non refuse ou non supprime : %s\n", nom))
  ok
}, logical(1))
verifier(all(forges_ok),
         sprintf("%d fichiers forges ou abimes : tous refuses (NULL), sans erreur, et supprimes", length(cas17)))

# Un fichier RDS (ancien format, ou forge) n'est JAMAIS lu, meme renomme.
f_rds <- file.path(dossier17, "serie_rds.json.gz")
con <- gzfile(f_rds, "wb"); serialize(list(a = function() stop("execute !"), b = quote(system("echo")) ), con); close(con)
verifier(is.null(cache_lire_table(f_rds)) && !file.exists(f_rds),
         "un objet R serialise place sous un nom de cache est refuse")
f_ro <- file.path(dossier17, "serie_ro.json.gz")
ecrire_gz17(f_ro, c(entete_ok17, '{"a":[1,2]}'))
trunc <- readBin(f_ro, "raw", 1000L); writeBin(trunc[seq_len(max(1L, length(trunc) %/% 2L))], f_ro)
verifier(is.null(cache_lire_table(f_ro)), "un fichier tronque est refuse sans erreur")

# Bombe de decompression : petit fichier, enorme une fois decompresse
f_bombe <- file.path(dossier17, "serie_bombe.json.gz")
con <- gzfile(f_bombe, "wb"); for (i in 1:6) writeBin(raw(1024^2), con); close(con)
t0 <- Sys.time()
verifier(is.null(cache_lire_table(f_bombe, max_mo = 2)) && !file.exists(f_bombe),
         "un fichier qui depasse la taille maximale une fois decompresse est ecarte et supprime")
verifier(as.numeric(difftime(Sys.time(), t0, units = "secs")) < 5, "... sans etre lu en entier")

# Noms de fichiers
verifier(identical(cache_nom_sur("../../etc/passwd"), ".._.._etc_passwd") ||
           !grepl("[/\\\\]", cache_nom_sur("../../etc/passwd")),
         "un identifiant de station n'emporte aucun separateur de dossier")
verifier(!grepl("\\.\\.", cache_nom_sur("a..b")) && nchar(cache_nom_sur(strrep("z", 500))) <= 60L &&
           nzchar(cache_nom_sur("")) && nzchar(cache_nom_sur(NA)),
         "nom assaini : pas de « .. », longueur bornee, jamais vide")
verifier(cache_nom_valide("safran_43.35_1.473_2020_s3.json.gz") && cache_nom_valide("dept_31.json.gz"),
         "des noms de cache ordinaires sont valides")
verifier(!any(vapply(c("../x.json.gz", "a/b.json.gz", ".cache", "x y.json.gz", "", NA, "a\\b", "x..y",
                       strrep("a", 300)), cache_nom_valide, logical(1))),
         "chemins, noms caches, espaces, « .. » et noms trop longs sont invalides")
ch17 <- cache_chemin(1.4727, 43.35, 2020)
verifier(identical(dirname(ch17), CONFIG$cache$repertoire) && grepl("\\.json\\.gz$", ch17) &&
           cache_nom_valide(basename(ch17)),
         "le fichier de cache SAFRAN est dans le dossier de cache, en .json.gz, au nom valide")

dehors17 <- file.path(tempdir(), "dehors_v17.json.gz")
unlink(dehors17)
r_hors <- mf_cache_ecrire("../dehors_v17.json.gz", tableau17())
verifier(!isTRUE(r_hors) && !file.exists(dehors17) &&
           !file.exists(file.path(dossier17, "..", "dehors_v17.json.gz")),
         "ecrire un nom de cache avec « .. » est refuse : rien n'est cree hors du dossier")
verifier(is.null(mf_cache_lire("../../etc/passwd", 24)) && is.null(mf_cache_lire("a/b.json.gz", 24)),
         "lire un nom de cache avec chemin est refuse")
src_st17 <- code_sans_commentaires17("src/utils_stations.R")
verifier(grepl("cache_nom_sur(station$id)", src_st17, fixed = TRUE) && grepl("cache_nom_sur(dept)", src_st17, fixed = TRUE),
         "l'identifiant de station et le departement sont assainis avant de former un nom de fichier")

# Plus de readRDS/saveRDS dans le cache
verifier(!grepl("readRDS|saveRDS|unserialize", paste(code_sans_commentaires17("src/utils_cache.R"),
                                                     code_sans_commentaires17("src/utils_api.R"),
                                                     code_sans_commentaires17("src/utils_stations.R"))),
         "le code du cache n'appelle plus readRDS, saveRDS ni unserialize")

# Aller-retour par les fonctions du cache SAFRAN et des stations
don17 <- serie_synthetique(2019:2019, t_moy = 12, p_jour = 1.5)
verifier(isTRUE(cache_ecrire(1.4727, 43.35, 2019, don17)), "cache SAFRAN : ecriture")
lu17 <- cache_lire(1.4727, 43.35, 2019)
verifier(is.data.frame(lu17) && nrow(lu17) == nrow(don17) && isTRUE(all.equal(lu17$t_moy, don17$t_moy)) &&
           inherits(lu17$date, "Date"),
         "cache SAFRAN : relecture identique, dates comprises")
verifier(is.null(cache_lire(1.4727, 43.36, 2019)), "cache SAFRAN : un autre point n'est pas confondu")
verifier(isTRUE(mf_cache_ecrire("dept_31.json.gz", tableau17())) && is.data.frame(mf_cache_lire("dept_31.json.gz", 24)),
         "cache des stations : aller-retour")
CONFIG$cache$actif <- FALSE
verifier(is.null(cache_lire(1.4727, 43.35, 2019)) && !isTRUE(cache_ecrire(1.4727, 43.35, 2018, don17)),
         "cache coupe : ni lecture ni ecriture")
CONFIG$cache$actif <- TRUE


# ==============================================================================
groupe("v1.7 — V1 : le cache est borne (purge par age et par taille)")
# ==============================================================================

p17 <- file.path(tempdir(), "cache_purge17")
fabriquer17 <- function(nom, ko = 1, age_j = 0, dossier = p17) {
  f <- file.path(dossier, nom)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  writeBin(raw(ko * 1024), f)
  Sys.setFileTime(f, Sys.time() - age_j * 86400)
  f
}
maintenant17 <- Sys.time()

unlink(p17, recursive = TRUE)
vieux <- fabriquer17("safran_45_4_2000_s3.json.gz", age_j = 40)
recent <- fabriquer17("safran_45_4_2001_s3.json.gz", age_j = 2)
sta_v <- fabriquer17("stations/serie_31555001_previous_1950_2024.json.gz", age_j = 45)
sta_r <- fabriquer17("stations/dept_31.json.gz", age_j = 0.1)
b <- cache_purger(p17, maintenant17, taille_max_mo = 100)
verifier(!file.exists(vieux) && !file.exists(sta_v) && file.exists(recent) && file.exists(sta_r),
         "les fichiers plus vieux que la duree de validite sont supprimes, les recents restent (dossier des stations compris)")
verifier(b$supprimes == 2L && b$restants == 2L && b$liberes_o == 2 * 1024,
         "le bilan de purge compte fichiers supprimes, restants et octets liberes")

unlink(p17, recursive = TRUE)
ancien_rds <- fabriquer17("safran_45_4_2000_s2.rds", age_j = 0)
st_rds <- fabriquer17("stations/dept_31.rds", age_j = 0)
neuf <- fabriquer17("safran_45_4_2001_s3.json.gz")
cache_purger(p17, maintenant17, taille_max_mo = 100)
verifier(!file.exists(ancien_rds) && !file.exists(st_rds) && file.exists(neuf),
         "les fichiers .rds d'avant la v1.7 sont supprimes meme s'ils sont recents")

unlink(p17, recursive = TRUE)
etrangers <- c(fabriquer17("notes.txt", age_j = 400), fabriquer17("mon_export.csv", age_j = 400),
               fabriquer17("safran.docx", age_j = 400), fabriquer17("autre/lisez_moi.md", age_j = 400),
               fabriquer17(".gitkeep", age_j = 400))
fabriquer17("safran_45_4_1999_s3.json.gz", age_j = 400)
cache_purger(p17, maintenant17, taille_max_mo = 100)
verifier(all(file.exists(etrangers)),
         "les fichiers qui ne sont pas des fichiers de cache ClimaFor ne sont jamais touches")

unlink(p17, recursive = TRUE)
for (i in 1:10) fabriquer17(sprintf("safran_45_%d_2000_s3.json.gz", i), ko = 100, age_j = 20 - i)
# i = 1 est le plus ancien (19 j), i = 10 le plus recent (10 j)
b <- cache_purger(p17, maintenant17, taille_max_mo = 0.5)
restes <- list.files(p17, pattern = "^safran_")
verifier(b$taille_o <= 0.5 * 1024^2 * 0.8 && b$taille_o > 0,
         "au-dela du plafond, le dossier retombe a 80 % du plafond")
verifier(identical(sort(restes), sort(sprintf("safran_45_%d_2000_s3.json.gz", 7:10))),
         "les fichiers les plus anciens sont supprimes en premier, les plus recents gardes")

unlink(p17, recursive = TRUE)
for (i in 1:3) fabriquer17(sprintf("safran_45_%d_2000_s3.json.gz", i), ko = 100)
verifier(cache_purger(p17, maintenant17, taille_max_mo = 100)$supprimes == 0L,
         "sous le plafond, rien n'est supprime")
verifier(cache_purger(p17, maintenant17, taille_max_mo = 0)$supprimes == 0L &&
           cache_purger(p17, maintenant17, taille_max_mo = NA)$supprimes == 0L,
         "un plafond de 0 ou NA le desactive (la purge par age continue)")

unlink(p17, recursive = TRUE)
t_vieux <- fabriquer17(".cf_tmp_123_abcdefgh", age_j = 0.2)
t_neuf  <- fabriquer17(".cf_tmp_124_abcdefgh", age_j = 0)
cache_purger(p17, maintenant17, taille_max_mo = 100)
verifier(!file.exists(t_vieux) && file.exists(t_neuf),
         "un fichier temporaire d'ecriture abandonne depuis plus d'une heure est supprime, un recent est respecte")

b <- cache_purger(file.path(tempdir(), "n_existe_pas_17"), maintenant17)
verifier(b$supprimes == 0L && b$restants == 0L, "dossier absent : aucune erreur")

# Cadence : la purge apres ecriture ne tourne pas a chaque ecriture
unlink(p17, recursive = TRUE)
rep_avant <- CONFIG$cache$repertoire
CONFIG$cache$repertoire <- p17
.ETAT_CACHE$derniere_purge <- NULL
cache_purger_si_besoin(maintenant17)
vieux2 <- fabriquer17("safran_45_4_1990_s3.json.gz", age_j = 90)
cache_purger_si_besoin(maintenant17 + 60)
verifier(file.exists(vieux2), "une deuxieme purge une minute plus tard n'a pas lieu (cadence)")
cache_purger_si_besoin(maintenant17 + 60 * (CONFIG$cache$purge_intervalle_min + 1))
verifier(!file.exists(vieux2), "... elle a lieu une fois l'intervalle ecoule")
CONFIG$cache$repertoire <- rep_avant

verifier(grepl("cache_purger()", code_sans_commentaires17("global.R"), fixed = TRUE),
         "global.R purge le cache au demarrage")
verifier(grepl("cache_purger_si_besoin()", code_sans_commentaires17("src/utils_api.R"), fixed = TRUE) &&
           grepl("cache_purger_si_besoin()", code_sans_commentaires17("src/utils_stations.R"), fixed = TRUE),
         "chaque ecriture de cache (SAFRAN, stations) declenche la purge cadencee")
verifier(is.finite(CONFIG$cache$taille_max_mo) && CONFIG$cache$taille_max_mo > 0 &&
           CONFIG$cache$schema >= 3L,
         "plafond de taille configure et schema de cache au moins 3")


# ==============================================================================
groupe("v1.7 — V2 : limites d'usage par fenetre glissante")
# ==============================================================================

lim_avant17 <- CONFIG$limites
CONFIG$limites <- list(fenetre_min = 10, analyses_session = 3, rapports_session = 2,
                       telechargements_proc = 2)
for (n in ls(.ETAT_LIMITES)) rm(list = n, envir = .ETAT_LIMITES)
t0 <- 1e6
j <- limite_journal()
for (dt in c(0, 60, 120)) limite_noter(j, 10, t0 + dt)
verifier(limite_attente_s(j, 3, 10, t0 + 130) == 470,
         "trois demandes pour un maximum de 3 : la quatrieme attend la sortie de la plus ancienne (470 s)")
verifier(limite_attente_s(j, 3, 10, t0 + 601) == 0 || abs(limite_attente_s(j, 3, 10, t0 + 601)) < 1e-9,
         "... et passe des que la plus ancienne a quitte la fenetre")
verifier(limite_attente_s(j, 4, 10, t0 + 130) == 0, "sous le maximum : pas d'attente")
verifier(limite_attente_s(j, NA, 10, t0) == 0 && limite_attente_s(j, 0, 10, t0) == 0 &&
           limite_attente_s(j, NULL, 10, t0) == 0 && limite_attente_s(j, -1, 10, t0) == 0,
         "maximum absent, nul, negatif ou NA : limite inactive")
verifier(limite_attente_s(j, 3, NA, t0 + 130) == 0, "fenetre inactive : pas d'attente")
j2 <- limite_journal(); for (k in 1:50) limite_noter(j2, 10, t0 + k)
limite_noter(j2, 10, t0 + 5000)
verifier(length(j2$instants) == 1L, "les instants sortis de la fenetre sont oublies (le journal ne grossit pas)")

verifier(identical(limite_phrase_delai(30), "moins d'une minute") &&
           identical(limite_phrase_delai(61), "environ 2 minutes") &&
           identical(limite_phrase_delai(600), "environ 10 minutes") &&
           identical(limite_phrase_delai(NA), "moins d'une minute"),
         "le delai est dit en mots, arrondi a la minute superieure")

verifier(grepl("demandé 5 rapports", limite_message("rapports", "session", 100, 10, 5), fixed = TRUE),
         "le message des rapports parle de rapports")
sA <- shiny::MockShinySession$new(); sB <- shiny::MockShinySession$new()
r <- lapply(1:3, function(k) limite_controler("analyses", sA, distant = FALSE, maintenant = t0 + k))
verifier(all(vapply(r, function(x) x$ok, logical(1))), "trois analyses d'une session passent")
r4 <- limite_controler("analyses", sA, distant = FALSE, maintenant = t0 + 4)
verifier(!r4$ok && identical(r4$portee, "session") && r4$attente_s > 0,
         "la quatrieme analyse de la meme session est refusee (portee : session)")
verifier(grepl("Réessayez dans", r4$message) && grepl("restent affichés", r4$message) &&
           grepl("lancé 3 analyses en 10 minutes", r4$message, fixed = TRUE),
         "le message dit pourquoi (3 analyses en 10 minutes), quand reessayer, et que les resultats restent affiches")
verifier(limite_controler("analyses", sB, distant = FALSE, maintenant = t0 + 4)$ok,
         "une autre session n'est pas affectee")
verifier(limite_controler("analyses", sA, distant = FALSE, maintenant = t0 + 4 + 601)$ok,
         "la session repasse apres la fenetre")
verifier(limite_controler("rapports", sA, maintenant = t0 + 5)$ok && limite_controler("rapports", sA, maintenant = t0 + 6)$ok &&
           !limite_controler("rapports", sA, maintenant = t0 + 7)$ok,
         "les rapports ont leur propre compteur (2 par fenetre dans ce test)")

for (n in ls(.ETAT_LIMITES)) rm(list = n, envir = .ETAT_LIMITES)
sC <- shiny::MockShinySession$new(); sD <- shiny::MockShinySession$new(); sE <- shiny::MockShinySession$new()
verifier(limite_controler("analyses", sC, distant = TRUE, maintenant = t0)$ok &&
           limite_controler("analyses", sD, distant = TRUE, maintenant = t0 + 1)$ok,
         "deux analyses avec telechargement, de deux sessions, passent")
p3 <- limite_controler("analyses", sE, distant = TRUE, maintenant = t0 + 2)
verifier(!p3$ok && identical(p3$portee, "processus") && grepl("beaucoup d'analyses", p3$message, fixed = TRUE),
         "la troisieme, d'une troisieme session, est refusee par la limite du processus")
verifier(limite_controler("analyses", sE, distant = FALSE, maintenant = t0 + 2)$ok,
         "une analyse servie par le cache ne compte pas dans la limite du processus")
verifier(length(limite_journal_session(sE, "analyses")$instants) == 1L,
         "une demande refusee par le processus ne consomme pas le quota de la session")
CONFIG$limites$telechargements_proc <- NA
for (k in 1:8) limite_controler("analyses", shiny::MockShinySession$new(), distant = TRUE, maintenant = t0 + 10 + k)
verifier(limite_controler("analyses", shiny::MockShinySession$new(), distant = TRUE, maintenant = t0 + 30)$ok,
         "limite du processus desactivee (NA) : tout passe")
CONFIG$limites$analyses_session <- 0
sF <- shiny::MockShinySession$new()
for (k in 1:30) limite_controler("analyses", sF, maintenant = t0 + k)
verifier(limite_controler("analyses", sF, maintenant = t0 + 40)$ok && length(limite_journal_session(sF, "analyses")$instants) == 0L,
         "limite de session desactivee (0) : tout passe et rien n'est journalise")
verifier(limite_controler("analyses", NULL, maintenant = t0)$ok, "hors session (NULL) : pas d'erreur")

CONFIG$limites <- lim_avant17
for (n in ls(.ETAT_LIMITES)) rm(list = n, envir = .ETAT_LIMITES)
verifier(all(c("fenetre_min", "analyses_session", "rapports_session", "telechargements_proc") %in% names(CONFIG$limites)) &&
           CONFIG$limites$analyses_session >= 5,
         "la configuration par defaut laisse un usage normal (au moins 5 analyses par fenetre)")
verifier(grepl('limite_controler("analyses"', code_sans_commentaires17("app.R"), fixed = TRUE) &&
           grepl('limite_controler("rapports"', code_sans_commentaires17("src/mod_rapport.R"), fixed = TRUE),
         "le lancement d'une analyse et la preparation d'un rapport passent par le controle")


# ==============================================================================
groupe("v1.7 — V3 : position et confidentialite")
# ==============================================================================

c17 <- html_de17(confidentialite_ui())
verifier(grepl("Position et confidentialité", c17, fixed = TRUE),
         "la carte « Position et confidentialité » existe")
verifier(all(vapply(c("geo.api.gouv.fr", "api.geosas.fr", "data.gouv.fr", "data.geopf.fr"),
                    function(h) grepl(h, c17, fixed = TRUE), logical(1))),
         "elle nomme les quatre services contactes")
verifier(grepl("10 m", c17, fixed = TRUE) && grepl("100 m", c17, fixed = TRUE),
         "elle dit les deux arrondis (position : 10 m ; noms de fichiers du cache : 100 m)")
verifier(grepl(sprintf("%d jours", CONFIG$cache$duree_j), c17, fixed = TRUE) &&
           grepl(sprintf("%d Mo", CONFIG$cache$taille_max_mo), c17, fixed = TRUE),
         "la duree et le plafond du cache sont lus dans la configuration")
verifier(grepl("adresse de votre appareil", c17, fixed = TRUE) && grepl("adresse du serveur", c17, fixed = TRUE),
         "elle distingue ce que voient les services appeles par le serveur et la Geoplateforme (tuiles, via le navigateur)")
verifier(grepl("lien de partage", tolower(c17), fixed = TRUE) && grepl("coordonnées du point", c17, fixed = TRUE),
         "elle dit que le lien de partage contient les coordonnees")
verifier(grepl("ne remplace", c17, fixed = TRUE) && grepl("responsable du traitement", c17, fixed = TRUE),
         "elle dit ce qu'elle n'est pas : la declaration de l'organisme qui heberge l'application")
verifier(!grepl("RGPD|conforme|garanti|anonym", c17),
         "elle ne revendique ni conformite, ni anonymat, ni garantie")
verifier(grepl("confidentialite", html_de17(mod_sources_ui("s")), fixed = TRUE),
         "l'onglet Sources l'inclut (ancre #confidentialite)")

loc17 <- html_de17(mod_localisation_ui("l"))
verifier(grepl("arrondie à 10 m", loc17, fixed = TRUE) && grepl('id="l-note_position"', loc17, fixed = TRUE),
         "une note accompagne le bouton « Utiliser ma position »")
verifier(grepl('aria-describedby="l-note_position"', loc17, fixed = TRUE),
         "le bouton est relie a sa note pour les lecteurs d'ecran (aria-describedby)")
verifier(grepl("Math.round(position.coords.longitude * 1e4)", lire17("www/app.js"), fixed = TRUE) &&
           grepl("Math.round(position.coords.latitude * 1e4)", lire17("www/app.js"), fixed = TRUE),
         "le navigateur arrondit la position avant de l'envoyer")


# ==============================================================================
groupe("v1.7 — version, architecture")
# ==============================================================================

v17 <- as.numeric_version(CONFIG$app$version)
verifier(v17 >= "1.7.0", "la version est au moins 1.7.0")
desc17 <- read.dcf(chemin_ci("DESCRIPTION"))
verifier(identical(unname(desc17[1, "Version"]), CONFIG$app$version),
         "DESCRIPTION et CONFIG portent la meme version")
fichiers17 <- readLines(chemin_ci("global.R"), warn = FALSE, encoding = "UTF-8")
verifier(any(grepl("src/utils_cache.R", fichiers17, fixed = TRUE)) && any(grepl("src/utils_limites.R", fichiers17, fixed = TRUE)),
         "global.R charge les deux nouveaux fichiers")
verifier(file.exists(chemin_ci("src/utils_cache.R")) && file.exists(chemin_ci("src/utils_limites.R")) &&
           identical(tolower(sub("\\.R$", "", basename(c("src/utils_cache.R", "src/utils_limites.R")))),
                     sub("\\.R$", "", basename(c("src/utils_cache.R", "src/utils_limites.R")))),
         "les nouveaux fichiers ont un nom entierement en minuscules (hebergement sensible a la casse)")

# Le cache de ces tests est jetable ; la configuration est remise en l'etat.
unlink(c(dossier17, p17), recursive = TRUE)
CONFIG$cache <- cache_avant17
CONFIG$cache$actif <- FALSE
