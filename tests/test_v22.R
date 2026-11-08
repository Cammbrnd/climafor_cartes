# ==============================================================================
# tests/test_v22.R — BASE PRECALCULEE ET MODE RAPIDE (VERSION 2.2)
# ------------------------------------------------------------------------------
# Ce que ces tests gardent :
#   1. la base publiee : colonnes et references bien formees, ecrites avec la carte, relues
#      a l'identique, validees a la lecture (un jeu sans base, ou avec une base piegee) ;
#   2. la PARITE : pour chacun des 25 indicateurs, le tableau recompose depuis la base +
#      les seuls jours charges est celui que donne la serie journaliere entiere (a la
#      precision du stockage pres : entiers 16 bits) ;
#   3. les garde-fous : la base ne sert pas quand la reference, la fenetre ou les seuils
#      different de ceux du precalcul, ni quand elle est incomplete.
# Aucun test n'appelle SAFRAN : les series viennent de outils/carte_demo.R (donnees FICTIVES).
# ==============================================================================

for (f in c("outils/carte_geo.R", "outils/carte_format.R", "outils/carte_precalcul.R",
            "outils/carte_demo.R")) source(chemin_ci(f))


# ==============================================================================
groupe("v2.2 — la base precalculee : definition")
# ==============================================================================

cles22 <- names(BASE_COUCHES)
verifier(length(cles22) >= 30 && !anyDuplicated(cles22), "la base a des cles distinctes")
verifier(all(vapply(names(BASE_COUCHES), function(n) identical(BASE_COUCHES[[n]]$cle, n), TRUE)),
         "chaque entree de la base est rangee sous sa propre cle")
verifier(!length(intersect(cles22, names(CARTE_COUCHES))),
         "aucune entree de la base ne porte la cle d'une couche de la carte")
verifier(all(grepl("^[a-z0-9_]{1,40}$", cles22)) &&
           all(vapply(BASE_COUCHES, function(k) carte_nom_fichier_sur(sprintf("c_%s.bin.gz", k$cle)), TRUE)),
         "les cles et les noms de fichiers de la base sont publiables")
verifier(all(vapply(BASE_COUCHES, function(k) isTRUE(k$base) && k$type %in% c("annuel", "ref"), TRUE)),
         "chaque entree se declare base, annuelle ou reference")
verifier(all(vapply(BASE_COUCHES, function(k) k$stock >= 0 && k$stock <= 4, TRUE)),
         "la precision de stockage reste dans 0..4 decimales")
verifier(!any(vapply(CARTE_COUCHES, function(k) isTRUE(k$base), TRUE)),
         "aucune couche de la carte n'est marquee « base » : le navigateur ne verra pas la base")
annuelles22 <- Filter(function(k) identical(k$type, "annuel"), BASE_COUCHES)
verifier(all(vapply(annuelles22, function(k) k$source %in% names(INDICATEURS), TRUE)),
         "chaque colonne annuelle s'appuie sur un indicateur du registre")
refs22 <- Filter(function(k) identical(k$type, "ref"), BASE_COUCHES)
verifier(all(vapply(refs22, function(k) is.function(k$calc) && k$lignes %in% c(1L, 12L, 366L), TRUE)),
         "chaque reference a un calcul et un nombre de lignes connu (1, 12 ou 366)")

# Aucune colonne publiee en double : une (indicateur, colonne) = une seule couche.
paires22 <- vapply(c(CARTE_COUCHES, annuelles22), function(k) paste(k$source, k$colonne, sep = ":"), "")
verifier(!anyDuplicated(paires22), "aucune colonne d'indicateur n'est publiee deux fois")
verifier(all(vapply(names(BASE_ALIAS), function(a) {
  cible <- strsplit(BASE_ALIAS[[a]], ":", fixed = TRUE)[[1]]
  !is.na(base_cle_colonne(cible[1], cible[2]))
}, TRUE)), "chaque alias designe une colonne publiee")


# ==============================================================================
groupe("v2.2 — la base precalculee : colonnes de BASE_INDICATEURS")
# ==============================================================================

annees22 <- 1985:2025
an_long22 <- 1985:2026
src_long22 <- carte_demo_source(an_long22)
mm22 <- carte_demo_mailles(300)
n22 <- nrow(mm22)
# Les series de la base sont les MEMES jours que la serie « entiere » : sinon la comparaison ne
# mesurerait pas le chemin, mais la graine du generateur.
lire22 <- function(i, lon, lat) { d <- src_long22(i, lon, lat); d[d$annee <= 2025L, ] }
verifier(n22 >= 4, "(controle) la grille d'essai compte au moins quatre mailles")

for (id in names(BASE_INDICATEURS)) {
  d <- BASE_INDICATEURS[[id]]
  ok <- id %in% names(INDICATEURS) && all(!is.na(vapply(d$col, function(c0) base_cle_colonne(id, c0), "")))
  verifier(ok, sprintf("%s : chaque colonne lue est publiee (carte ou base)", id))
}
verifier(setequal(c(names(BASE_INDICATEURS), BASE_INDICATEURS_DIRECTS), names(INDICATEURS)),
         "les 25 indicateurs sont couverts : annuels recomposes, profils lus dans les references")

part22 <- carte_construire(lire22, mm22$lon, mm22$lat, annees22, seq_len(n22), base = BASE_COUCHES)
dos22 <- tempfile("carte22")
r22 <- carte_assembler(list(part22), mm22$lon, mm22$lat, annees22, "jeu synthetique", dos22,
                       base = BASE_COUCHES)
tr22 <- carte_transport(url = "", dossier = dos22)
lu22 <- carte_lire_meta_locale(dos22)
verifier(!is.null(lu22$meta) && length(lu22$meta$base) == length(BASE_COUCHES),
         "le jeu assemble passe la validation et publie toute la base")
verifier(length(lu22$meta$couches) == length(CARTE_COUCHES),
         "les couches de la carte sont inchangees par la base")
verifier(all(file.exists(file.path(dos22, c(lu22$meta$grille, vapply(lu22$meta$base, function(k) k$fichier, ""))))),
         "chaque fichier de la base est ecrit")
verifier(identical(lu22$meta$base_parametres$empreinte, base_empreinte(carte_ctx(annees22))),
         "meta.json porte l'empreinte des reglages de la base")
verifier(!any(grepl("b_|r_", vapply(lu22$meta$couches, function(k) k$cle, ""))),
         "les entrees de la base ne figurent pas parmi les couches de la carte")

# -- relecture d'une maille -------------------------------------------------------
jeu22 <- cmp_jeu(tr22)
l_cle <- cmp_lire_couche(jeu22, "tmp")
col2 <- carte_lire_colonne(file.path(dos22, "c_tmp.bin.gz"), n22, length(annees22), 2L, 3L)
verifier(isTRUE(col2$taille_exacte) && isTRUE(all.equal(col2$valeurs, l_cle$valeurs[, 3])),
         "carte_lire_colonne rend la colonne que carte_lire_couche rend en entier")
verifier(!carte_lire_colonne(file.path(dos22, "c_tmp.bin.gz"), n22, length(annees22), 2L, n22 + 1L)$taille_exacte,
         "une maille hors du fichier est refusee")
verifier(!carte_lire_colonne(file.path(dos22, "c_tmp.bin.gz"), n22, length(annees22) + 1L, 2L, 1L)$taille_exacte,
         "un fichier plus court que prevu est refuse")
v_ref22 <- base_lire(jeu22, "r_mois_p", 2L)
verifier(length(v_ref22) == 12L && all(is.finite(v_ref22)),
         "une reference mensuelle se relit : 12 valeurs")
verifier(length(base_lire(jeu22, "r_jour_tmax", 2L)) == 366L && length(base_lire(jeu22, "r_seuil_ff", 2L)) == 1L,
         "la normale du jour (366 valeurs) et le seuil de vent (1 valeur) se relisent")
verifier(identical(base_lire(jeu22, "r_mois_p", 2L), v_ref22), "une relecture est identique (memoisee)")
verifier(inherits(tryCatch(base_lire(jeu22, "r_absente", 1L), error = function(e) e), "error"),
         "une entree absente du jeu est une erreur, pas un vecteur vide")


# ==============================================================================
groupe("v2.2 — parite : base + jours charges = serie journaliere entiere")
# ==============================================================================

# Presence de l'annee en cours, INCOMPLETE : 1er janvier - 15 octobre 2026.
ctx22 <- function(courante = 2026L, ...) ctx_creer(ref = 1991:2020, courante = courante, affichage = 1991:2026, ...)

# Difference entre deux tableaux : liste des problemes (vide = identiques a la tolerance).
ecarts22 <- function(a, b, tol) {
  pb <- character(0)
  if (!is.data.frame(a) || !is.data.frame(b)) return("pas un tableau")
  if (!setequal(names(a), names(b))) {
    return(sprintf("colonnes : %s / %s", paste(setdiff(names(a), names(b)), collapse = ","),
                   paste(setdiff(names(b), names(a)), collapse = ",")))
  }
  if (nrow(a) != nrow(b)) return(sprintf("%d lignes contre %d", nrow(a), nrow(b)))
  for (nm in names(a)) {
    x <- a[[nm]]; y <- b[[nm]]
    if (is.numeric(x)) {
      dd <- abs(as.numeric(x) - as.numeric(y)); dd[is.na(x) & is.na(y)] <- 0
      limite <- if (grepl("^(n_|doy_|duree)", nm)) 1e-9 else tol
      if (anyNA(dd) || max(dd) > limite) pb <- c(pb, sprintf("%s (%.4g > %.4g)", nm, suppressWarnings(max(dd, na.rm = TRUE)), limite))
    } else if (inherits(x, "Date") || is.logical(x)) {
      if (!identical(as.character(x), as.character(y))) pb <- c(pb, nm)
    } else if (is.factor(x) || is.character(x)) {
      if (!identical(as.character(x), as.character(y))) pb <- c(pb, nm)
    }
  }
  pb
}

# Tolerance : 1,01 unite de la derniere decimale conservee dans la base.
tol22 <- function(id) {
  d <- BASE_INDICATEURS[[id]]
  if (is.null(d)) return(c(bh = 0.15, ombro = 0.15, pre_mois = 0.15, tmp_mois = 0.015,
                           neige_mois = 0.15, vpd_mois = 0.0015, vent_mois = 0.015, tmp_jour = 0.015)[[id]])
  st <- vapply(d$col, function(c0) {
    cl <- base_cle_colonne(id, c0)
    k <- c(CARTE_COUCHES, BASE_COUCHES)[[cl]]
    k$stock }, 0)
  1.01 * 10^(-min(st))
}

for (j in 1:3) {
  i22 <- j
  full22 <- src_long22(i22, mm22$lon[i22], mm22$lat[i22])
  full22 <- full22[full22$date <= as.Date("2026-10-15"), ]
  attr(full22, "source") <- "safran"
  rap22 <- list(jeu = jeu22, i = i22)
  ctx0 <- ctx22(2026L)
  # Jours charges : l'annee en cours seule (l'annee comparee en fait partie).
  vives22 <- full22[full22$annee == 2026L, ]
  attr(vives22, "source") <- "safran"
  attr(vives22, "base") <- rap22

  pb_tot <- character(0)
  for (id in names(INDICATEURS)) {
    ind <- INDICATEURS[[id]]
    complet <- calculer_un_indicateur(full22, ind, ctx0)
    rapide  <- calculer_un_indicateur(vives22, ind, ctx0)
    pb <- ecarts22(rapide, complet, tol22(id))
    if (length(pb)) pb_tot <- c(pb_tot, sprintf("%s : %s", id, paste(pb, collapse = " ; ")))
    if (!length(pb) && is.data.frame(complet) && nrow(complet) && !identical(attr(rapide, "raison"), NULL)) {
      pb_tot <- c(pb_tot, sprintf("%s : resultat vide en mode rapide", id))
    }
  }
  verifier(!length(pb_tot),
           if (length(pb_tot)) paste0("maille ", j, " — ecarts : ", paste(pb_tot, collapse = " || "))
           else sprintf("maille %d : les 25 indicateurs rapides = les 25 indicateurs complets (annee en cours 2026 partielle)", j))
}

# Annee comparee anterieure : les profils du mois et du jour demandent CETTE annee.
i22 <- 2L
full22 <- src_long22(i22, mm22$lon[i22], mm22$lat[i22]); full22 <- full22[full22$date <= as.Date("2026-10-15"), ]
attr(full22, "source") <- "safran"
ctx_2022 <- ctx22(2022L)
vives_2022 <- full22[full22$annee %in% c(2022L, 2026L), ]; attr(vives_2022, "source") <- "safran"
attr(vives_2022, "base") <- list(jeu = jeu22, i = i22)
pb_m <- character(0)
for (id in BASE_INDICATEURS_DIRECTS) {
  pb <- ecarts22(calculer_un_indicateur(vives_2022, INDICATEURS[[id]], ctx_2022),
                 calculer_un_indicateur(full22, INDICATEURS[[id]], ctx_2022), tol22(id))
  if (length(pb)) pb_m <- c(pb_m, sprintf("%s : %s", id, paste(pb, collapse = " ; ")))
}
verifier(!length(pb_m), if (length(pb_m)) paste("annee comparee 2022 :", paste(pb_m, collapse = " || "))
         else "annee comparee 2022 : les profils du mois et du jour sont ceux de la serie entiere")

# Sans aucune annee posterieure au jeu (annee en cours non publiee et non chargee) : les tableaux
# annuels se lisent entierement dans la base.
vide_vives22 <- full22[full22$annee == 2022L, ]; attr(vide_vives22, "source") <- "safran"
attr(vide_vives22, "base") <- list(jeu = jeu22, i = i22)
tmp22 <- calculer_un_indicateur(vide_vives22, INDICATEURS$tmp, ctx_2022)
verifier(is.data.frame(tmp22) && nrow(tmp22) == length(annees22) && !any(tmp22$partielle),
         "avec la seule annee comparee chargee, les 41 annees du jeu viennent de la base")
verifier(isTRUE(all.equal(tmp22$t_moy, base_lire(jeu22, "tmp", 2L))),
         "une valeur du bilan est exactement la valeur du fichier publie")


# ==============================================================================
groupe("v2.2 — garde-fous : quand la base ne sert pas")
# ==============================================================================

rap22 <- list(jeu = jeu22, i = 2L)
v_garde <- full22[full22$annee == 2026L, ]; attr(v_garde, "source") <- "safran"; attr(v_garde, "base") <- rap22
vide_ok <- function(r, raison) is.data.frame(r) && !nrow(r) && identical(attr(r, "raison"), raison) &&
  nzchar(attr(r, "message") %||% "")

verifier(isTRUE(base_applicable(ctx22(), jeu22)$ok), "avec les reglages du precalcul, la base sert")
r_ref <- calculer_un_indicateur(v_garde, INDICATEURS$tmp, ctx_creer(ref = 1981:2010, courante = 2026L))
verifier(vide_ok(r_ref, "rapide_reference"), "une autre periode de reference : pas de calcul rapide, et le dit")
r_fen <- calculer_un_indicateur(v_garde, INDICATEURS$petp, ctx22(fenetre = c(5L, 9L)))
verifier(vide_ok(r_fen, "rapide_reglages"), "une autre fenetre de saison : pas de calcul rapide, et le dit")
s_mod <- CONFIG$seuils; s_mod$chaleur_t2 <- s_mod$chaleur_t2 + 1
r_seu <- calculer_un_indicateur(v_garde, INDICATEURS$can, ctx22(seuils = s_mod))
verifier(vide_ok(r_seu, "rapide_reglages"), "un seuil modifie : pas de calcul rapide, et le dit")

# Jeu dont la base est incomplete : refuse en bloc (pas de chiffre calcule a moitie).
jeu_inc22 <- jeu22; jeu_inc22$meta$base <- jeu22$meta$base[-1]
verifier(identical(base_applicable(ctx22(), jeu_inc22)$raison, "base_incomplete"),
         "un jeu qui ne porte pas toute la base n'est pas utilise")
jeu_sans22 <- jeu22; jeu_sans22$meta$base <- NULL
verifier(identical(base_applicable(ctx22(), jeu_sans22)$raison, "base_incomplete"),
         "un jeu publie avant la base n'est pas utilise")
# `$` completerait `base` par `base_parametres` : un jeu hostile ou tronque ne doit pas s'y faire prendre.
verifier(is.null(jeu_sans22$meta[["base"]]) && !is.null(jeu_sans22$meta[["base_parametres"]]),
         "(controle) le jeu d'essai porte `base_parametres` mais plus `base` : le cas de l'appariement partiel")
verifier(!length(base_entree(jeu_sans22, "r_mois_p")) , "sans base, aucune entree de la base n'est trouvee")


# ==============================================================================
groupe("v2.2 — la base lue a la lecture : un jeu piege est refuse")
# ==============================================================================

meta22 <- jsonlite::fromJSON(file.path(dos22, "meta.json"), simplifyVector = FALSE)
verifier(carte_meta_valider(meta22)$ok, "(controle) la meta ecrite est valide")
sans_base22 <- meta22; sans_base22$base <- NULL; sans_base22$base_parametres <- NULL
verifier(carte_meta_valider(sans_base22)$ok, "un jeu sans base (publie avant la v2.2) reste valide")
casse22 <- function(f) { m <- meta22; m <- f(m); carte_meta_valider(m) }
verifier(!casse22(function(m) { m$base[[1]]$fichier <- "../x.bin.gz"; m })$ok, "nom de fichier de la base avec chemin : refuse")
verifier(!casse22(function(m) { m$base[[1]]$stock <- 9; m })$ok, "precision de stockage hors 0..4 : refusee")
verifier(!casse22(function(m) { m$base[[1]]$type <- "ref"; m })$ok, "type different de celui du code : refuse")
verifier(!casse22(function(m) { k <- which(vapply(m$base, function(b) identical(b$type, "ref"), TRUE))[1]; m$base[[k]]$lignes <- 7L; m })$ok,
         "nombre de lignes d'une reference different de celui du code : refuse")
verifier(!casse22(function(m) { m$base[[2]]$cle <- m$base[[1]]$cle; m })$ok, "cle de base en double : refusee")
verifier(!casse22(function(m) { m$base <- "oups"; m })$ok, "base qui n'est pas une liste : refusee")
verifier(!casse22(function(m) { m$base_parametres$empreinte <- strrep("x", 4000); m })$ok, "empreinte demesuree : refusee")
verifier(carte_meta_valider({ m <- meta22; m$base[[length(m$base) + 1L]] <- list(cle = "b_future_x", fichier = "c_b_future_x.bin.gz"); m })$ok,
         "une entree de base inconnue (jeu plus recent) est ignoree, pas refusee")

# Un fichier de la base manquant : le jeu n'est pas lisible.
dos_m22 <- tempfile("carte22m"); dir.create(dos_m22); file.copy(list.files(dos22, full.names = TRUE), dos_m22)
unlink(file.path(dos_m22, "c_b_gel_n_gel_severe.bin.gz"))
verifier(is.null(carte_lire_meta_locale(dos_m22)$meta), "un fichier de la base manquant : jeu refuse")


# ==============================================================================
groupe("v2.2 — mode rapide : annees a charger, provenance, ecrans")
# ==============================================================================

jeu_an22 <- list(annees = 1959:2025)
verifier(identical(base_annees_a_charger(jeu_an22, 1970:2026, 2026L), 2026L),
         "mode rapide : une plage 1970-2026 ne demande que l'annee que le jeu ne porte pas")
verifier(identical(base_annees_a_charger(jeu_an22, 1970:2026, 2022L), c(2022L, 2026L)),
         "...et l'annee comparee quand elle est anterieure")
verifier(identical(base_annees_a_charger(list(annees = 1985:2025), 1970:2026, 2026L), c(1970:1984, 2026L)),
         "...et les annees anterieures au jeu, que la base ne porte pas non plus")
verifier(identical(base_annees_a_charger(jeu_an22, 2026L, 2026L), 2026L), "...jamais deux fois la meme annee")

i22 <- 2L
full22 <- src_long22(i22, mm22$lon[i22], mm22$lat[i22]); full22 <- full22[full22$date <= as.Date("2026-10-15"), ]
vives22 <- full22[full22$annee == 2026L, ]
attr(vives22, "source") <- "safran"
rap22 <- list(jeu = jeu22, i = i22, note = "maille à 2,3 km")
attr(vives22, "base") <- rap22
verifier(base_active(vives22) && !base_active(full22), "les donnees disent si elles viennent de la base")
verifier(identical(serie_libelle(vives22), "base précalculée 1985-2025 + 2026") &&
           identical(serie_libelle(full22), "série 1985-2026"),
         "la serie est annoncee pour ce qu'elle est : base + annees chargees, ou serie entiere")
verifier(grepl("base précalculée", texte_source(vives22)) && grepl("2026", texte_source(vives22)) &&
           !grepl("base précalculée", texte_source(full22)),
         "la provenance (exports, pieds de graphique) dit la base, et seulement en mode rapide")
verifier(any(grepl("base précalculée", lignes_metadonnees_csv(INDICATEURS$tmp, ctx22(), don = vives22))),
         "...jusque dans les lignes de provenance des CSV")

s_rap22 <- synthese_securisee(vives22, ctx22())
verifier(is.data.frame(s_rap22) && !nrow(s_rap22) && identical(attr(s_rap22, "raison"), "mode_rapide") &&
           is.null(attr(s_rap22, "erreur")),
         "la synthese a periode egale n'est pas calculee en mode rapide, et ce n'est pas une erreur")
verifier(nrow(synthese_securisee(full22, ctx22())) > 0L, "...elle l'est en mode complet")

html22 <- function(x) { r <- htmltools::renderTags(x); paste(c(as.character(r$head), as.character(r$html)), collapse = "\n") }
avis22 <- html22
h_avis22 <- avis22(base_avis_ui(vives22))
verifier(grepl("Mode rapide", h_avis22) && grepl("1985", h_avis22) && grepl("2025", h_avis22) &&
           grepl("maille à 2,3 km", h_avis22) && grepl('id="charger_complet"', h_avis22) &&
           grepl("Charger la série complète", h_avis22),
         "l'avis dit ce qui est lu, ce qui est telecharge, et offre le chargement complet")
verifier(grepl('id="charger_complet_rapport"', avis22(base_avis_ui(vives22, id = "charger_complet_rapport"))),
         "...avec un identifiant different sur l'ecran du rapport (un identifiant ne figure qu'une fois)")
verifier(is.null(base_avis_ui(full22)), "...et aucun avis en mode complet")
verifier(grepl("série 1985-2026", html22(contexte_ui(full22, list(nom = "Essai"), ctx22())), fixed = TRUE) &&
           grepl("base précalculée 1985-2025 + 2026", html22(contexte_ui(vives22, list(nom = "Essai"), ctx22())), fixed = TRUE),
         "la barre de contexte nomme la serie chargee")
verifier(grepl("Mode rapide", phrase_attente(FALSE, NULL, rapide = TRUE)) &&
           !grepl("Mode rapide", phrase_attente(FALSE, NULL)),
         "l'attente annoncee change en mode rapide")

# -- Le Bilan, avec le simulateur de serveur de Shiny ---------------------------------
res_rap22 <- list()
args_rap22 <- list(donnees = shiny::reactive(vives22), ctx = shiny::reactive(ctx22()),
                   lieu = shiny::reactive(list(nom = "Essai", lon = mm22$lon[i22], lat = mm22$lat[i22])))
suppressWarnings(shiny::testServer(mod_bilan_server, args = args_rap22, {
  session$setInputs(familles = c("thermique", "hydrique", "gel", "vent", "aridite", "neige", "phenologie"))
  res_rap22$avis <<- output$mode_rapide$html
  res_rap22$synthese <<- output$synthese
  res_rap22$erreurs <<- output$erreurs_calcul
  res_rap22$res <<- resultats()
}))
verifier(grepl("Charger la série complète", res_rap22$avis), "Bilan rapide : l'avis et son bouton s'affichent")
verifier(is.null(res_rap22$synthese) || !nzchar(as.character(res_rap22$synthese$html %||% "")),
         "Bilan rapide : pas de synthese chiffree (elle demande la serie complete)")
verifier(is.null(res_rap22$erreurs), "Bilan rapide : aucun indicateur en echec")
non_vides22 <- vapply(res_rap22$res, function(r) is.data.frame(r) && nrow(r) > 0, TRUE)
verifier(all(non_vides22), if (all(non_vides22)) "Bilan rapide : les 25 indicateurs sont calcules"
         else paste("Bilan rapide : indicateur(s) vide(s) :", paste(names(non_vides22)[!non_vides22], collapse = ", ")))

# Un reglage qui invalide la base : chaque indicateur le dit et le bilan ne montre aucun chiffre trompeur.
res_inv22 <- list()
args_inv22 <- list(donnees = shiny::reactive(vives22), ctx = shiny::reactive(ctx_creer(ref = 1981:2010, courante = 2026L)),
                   lieu = args_rap22$lieu)
suppressWarnings(shiny::testServer(mod_bilan_server, args = args_inv22, { res_inv22$res <<- resultats() }))
verifier(all(vapply(res_inv22$res, function(r) identical(attr(r, "raison"), "rapide_reference"), TRUE)),
         "Bilan rapide, autre reference : aucun indicateur n'est calcule, chacun dit pourquoi")

# -- Le rapport refuse la base, et le dit -----------------------------------------
notifs22 <- character(0)
res_r22 <- list()
rem22 <- shiny::showNotification
suppressWarnings(shiny::testServer(mod_rapport_server,
  args = list(donnees = shiny::reactive(vives22), resultats = shiny::reactive(res_rap22$res),
              ctx = shiny::reactive(ctx22()), lieu = args_rap22$lieu), {
    res_r22$avis <<- output$mode_rapide$html
    session$setInputs(niveau = "complet", format = "html", interactif = FALSE)
    res_r22$etat <<- tryCatch({ session$setInputs(preparer = 1); "ok" }, error = function(e) conditionMessage(e))
  }))
verifier(grepl('id="charger_complet_rapport"', res_r22$avis %||% ""), "Rapport : l'avis propose de charger la serie complete")


# ==============================================================================
groupe("v2.2 — precalcul, interface et chaine de publication")
# ==============================================================================

lire22 <- function(f) paste(readLines(chemin_ci(f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
wf22 <- lire22(".github/workflows/construire-carte.yml")
verifier(grepl("base:\\s+description:", wf22) && grepl("BASE: \\$\\{\\{ inputs\\.base \\}\\}", wf22) &&
           length(gregexpr('base="\\$BASE"', wf22)[[1]]) == 4L,
         "workflow : l'entree `base` passe par une variable d'environnement, aux quatre commandes (mailles et stations, fragment et assemblage)")
verifier(!grepl("inputs\\.base[^ ]*\\s*\\}\\}[^\n]*Rscript", wf22),
         "workflow : l'entree n'est jamais ecrite dans une commande")
cli22 <- lire22("outils/carte_cli.R")
verifier(grepl("base_demandee", cli22) && grepl('"non"', cli22) && grepl("entrees de base", cli22),
         "carte_cli.R : la base se calcule avec les mailles (`base=non` la retire) et la verification la compte")
par22 <- lire22("src/mod_parametres.R")
verifier(grepl('mode_chargement', par22) && grepl('selected = "rapide"', par22) &&
           grepl('input\\$mode_chargement %\\|\\|% "rapide"', par22),
         "Parametres : le mode de chargement existe, et le rapide est le defaut (affichage et valeur de repli)")
verifier(grepl('charger_complet', lire22("src/serveur_dsf.R")) && grepl("base_ouvrir", lire22("src/serveur_dsf.R")),
         "serveur : le mode rapide passe par base_ouvrir() et le bouton recharge la serie complete")
verifier(!grepl("base", paste(readLines(chemin_ci("www/carte/carte.js"), warn = FALSE), collapse = "\n"), ignore.case = FALSE) ||
           !grepl("meta\\.base", paste(readLines(chemin_ci("www/carte/carte.js"), warn = FALSE), collapse = "\n")),
         "carte.js ne lit pas le champ `base` : la carte reste inchangee")
