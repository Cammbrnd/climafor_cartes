# ==============================================================================
# config.R — SOURCE UNIQUE DE VERITE de l'application
# ------------------------------------------------------------------------------
# Toute constante metier (URL d'API, noms de variables SAFRAN, seuils
# bioclimatiques, palettes, periodes de reference) est declaree ICI et NULLE
# PART AILLEURS. Pour faire evoluer l'application, on modifie ce fichier :
# aucun seuil, aucune URL, aucune couleur ne doit etre ecrite en dur dans les
# modules ou les fonctions de calcul.
#
# Convention : CONFIG est une liste imbriquee, accessible en lecture seule
# depuis n'importe quel module (variable globale de l'app Shiny).
# ==============================================================================

CONFIG <- list(

  # ---------------------------------------------------------------------------
  # 1. Identite de l'application
  # ---------------------------------------------------------------------------
  app = list(
    nom          = "ClimaFor",
    sous_titre   = "Bilan météorologique et climatique pour la gestion forestière",
    version     = "2.5.1",
    organisme    = "DSF — Département de la santé des forêts",
    annee        = format(Sys.Date(), "%Y"),
    # Detail technique d'un indicateur en echec, affiche sur sa carte (message
    # d'erreur R nettoye, 300 caracteres au plus). FALSE sur un deploiement ou
    # l'on prefere ne montrer que la phrase generique : le detail reste alors
    # dans le journal seulement.
    detail_erreurs = TRUE
  ),

  # ---------------------------------------------------------------------------
  # 2. Acces aux donnees (APIs)
  # ---------------------------------------------------------------------------
  api = list(
    # Service OGC EDR expose par INRAE / GeoSAS donnant acces a la reanalyse
    # SAFRAN-ISBA (SIM2) de Meteo-France, au pas de temps journalier,
    # sur la grille 8 km x 8 km couvrant la France metropolitaine.
    edr_base       = "https://api.geosas.fr/edr/collections/safran-isba/position",
    edr_format     = "CoverageJSON",
    edr_crs        = "EPSG:4326",

    # API Geo (Etat francais) : recherche de communes et retour du centroide.
    geo_communes   = "https://geo.api.gouv.fr/communes",

    # Fond de carte IGN (Geoplateforme) — WMTS ouvert, sans cle depuis 2024.
    wmts_ign_plan  = paste0(
      "https://data.geopf.fr/wmts?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0",
      "&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&STYLE=normal&TILEMATRIXSET=PM",
      "&FORMAT=image/png&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}"
    ),

    timeout_s      = 90,   # delai max par requete
    n_tentatives   = 2,    # nouvelles tentatives en cas d'echec reseau
    n_paralleles   = 6,    # requetes simultanees (1 requete = 1 annee)

    # Duree supposee d'un lot de `n_paralleles` requetes, en secondes. Sert
    # UNIQUEMENT a annoncer l'attente avant « Lancer l'analyse »
    # (estimer_chargement). C'est une estimation de l'ordre de grandeur reprise
    # du constat « 60 ans = environ une minute » ; elle n'a pas ete mesuree sur le
    # reseau de production. A ajuster apres quelques chargements reels.
    duree_lot_s    = 6
  ),

  # ---------------------------------------------------------------------------
  # 2 bis. Stations Meteo-France (donnees climatologiques de base, quotidiennes)
  # ---------------------------------------------------------------------------
  # Source : fichiers ouverts de Meteo-France publies sur data.gouv.fr, un jeu
  # par departement et par periode (RR-T-Vent : pluie, temperatures, vent).
  # Licence ouverte Etalab 2.0, SANS CLE D'API. Les fichiers « latest » sont
  # mis a jour chaque jour (donnee de la veille environ).
  #
  # POURQUOI CES FICHIERS ET PAS L'API TEMPS REEL
  # ---------------------------------------------
  # L'API d'observation de Meteo-France (portail-api.meteofrance.fr) exige une
  # cle nominative et ne conserve que 24 h d'historique : inutilisable pour
  # une normale 1991-2020. Son equivalent d'archive (API Climatologie) exige
  # aussi une cle et fonctionne par commande asynchrone. Les fichiers ouverts
  # donnent la meme donnee de base, sans secret a gerer ni a renouveler.
  #
  # LES ADRESSES NE SONT PAS ECRITES EN DUR
  # ---------------------------------------
  # Les noms de fichiers changent chaque annee (previous-1950-2025 devient
  # previous-1950-2026) et le miroir « synchro_ftp » a deja cesse d'etre mis a
  # jour une fois (septembre 2026, pour les donnees SYNOP). On interroge donc
  # la LISTE DES FICHIERS publiee par data.gouv.fr, qui reste a jour, et on
  # n'utilise le miroir qu'en dernier recours, avec un avertissement.
  stations = list(
    actif          = TRUE,

    dataset_id     = "6569b51ae64326786e4e8e1a",
    api_v2         = "https://www.data.gouv.fr/api/2/datasets",
    api_v1         = "https://www.data.gouv.fr/api/1/datasets",
    miroir         = "https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT",

    # Altitude du point choisi (IGN, sans cle) : sert a juger l'ecart avec la
    # station. Facultative : sans elle, le critere d'altitude est ignore.
    api_altitude   = "https://data.geopf.fr/altimetrie/1.0/calcul/alti/rest/elevation.json",

    # Recherche
    rayon_km       = 40,          # au-dela, aucune station n'est proposee
    anneaux_km     = c(20, 40),   # rayons des points sondes pour trouver les departements voisins
    n_propositions = 3,           # stations proposees

    # Classement : la distance, corrigee de l'altitude et des variables absentes.
    # 100 m d'ecart d'altitude pesent autant que 10 km (0,1 km par metre) ;
    # une station sans temperature est desavantagee de 15 km, sans pluie de
    # 10 km, apparemment arretee de 25 km. La distance reelle reste affichee.
    poids_alt_km_par_m = 0.10,
    penalite_sans_t_km = 15,
    penalite_sans_p_km = 10,
    penalite_inactive_km = 25,

    # Seuils de qualite. Chaque critere donne un niveau (1 = bonne ... 4 =
    # insuffisante) ; le niveau global est le plus mauvais des criteres.
    seuils = list(
      distance_km  = c(10, 25, 40),        # <= : bonne, correcte, limitee
      ecart_alt_m  = c(100, 250, 500),
      completude   = c(0.95, 0.85, 0.70),  # part de jours valides, 12 derniers mois
      fraicheur_j  = c(7, 30, 90),         # age de la derniere observation
      annees_ref   = c(0.90, 0.80, 0.60)   # part d'annees exploitables sur la reference
    ),

    # Une annee n'est conservee que si CHAQUE variable mesuree par la station y
    # est renseignee sur au moins cette part des jours. Les cumuls de pluie
    # sous-estiment d'autant les jours manquants : 95 % laisse jusqu'a 18 jours.
    seuil_annee    = 0.95,

    # Definition de la temperature moyenne d'une station. Les fichiers portent
    # trois grandeurs : TM (moyenne des 24 valeurs horaires, quand elle existe),
    # TNTXM et (TN + TX) / 2. Les melanger selon les jours fait changer de
    # definition au fil de la serie (TM n'existe souvent que sur la partie
    # recente) et fabrique une derive qui n'a rien de climatique.
    #   "tntx" : (TN + TX) / 2 sur TOUTE la serie, definition homogene dans le
    #            temps (TM seulement si la station ne mesure ni TN ni TX) ;
    #   "tm"   : ancien comportement, TM puis TNTXM puis (TN + TX) / 2.
    definition_tmoy = "tntx",

    # A partir de combien de jours sans mesure dans une annee les graphiques le
    # signalent. Ces jours ne sont PAS compenses : un cumul de pluie ou un
    # comptage de gel les ignore, donc les sous-estime.
    signaler_manquants_j = 3L,

    # Codes qualite Meteo-France (colonnes QRR, QTN, QTX...). NON VERIFIES sur la
    # notice officielle (acces impossible a l'ecriture) : a controler avec
    # diagnostic_stations.R, qui affiche la repartition des codes, en distinguant
    # les trois derniers mois. Seul le code 2 (« douteux ») est ecarte par defaut.
    # Le code 9 (« filtre / non encore controle ») est CONSERVE : s'il etait
    # ecarte alors qu'il marque simplement les derniers jours non valides par un
    # expert, les stations paraitraient arretees et les series seraient amputees
    # de leurs mois recents. Le filtrage s'annule de lui-meme s'il ecarterait
    # plus de la moitie des valeurs d'une variable (voir mf_appliquer_qualite).
    qualite_ecartee = 2,

    # Denomination des fichiers du miroir, UNIQUEMENT pour le dernier recours
    # (liste data.gouv.fr injoignable). A mettre a jour quand les periodes
    # tournent (previous-1950-2024 -> previous-1950-2025, par exemple).
    secours = list(previous = c(1950, 2024), latest = c(2025, 2026)),

    # Caches (heures / jours)
    cache_index_h  = 24,   # liste des fichiers data.gouv.fr
    cache_dept_h   = 12,   # stations d'un departement (fichier « latest »)
    cache_serie_j  = 30,   # serie d'une station (partie historique)

    # Telechargements (audit V6). Les adresses des fichiers viennent de la reponse
    # du catalogue data.gouv.fr : on ne telecharge que depuis ces domaines (et
    # leurs sous-domaines), en HTTPS, et on s'arrete au-dela de `taille_max_mo`
    # (le plus gros fichier departemental fait quelques dizaines de Mo compresses).
    #
    # Les fichiers « latest » et « previous » ne sont PAS servis par data.gouv.fr
    # mais par le stockage objet de Meteo-France chez OVHcloud : adresse relevee le
    # 2 novembre 2026 dans le message d'erreur d'un poste (v1.7.2 refusait alors
    # tout telechargement). On autorise ce seul « seau » (le nom d'hote complet),
    # surtout pas « cloud.ovh.net » ni « ovh.net » : ce serait ouvrir la porte a
    # n'importe quel client d'OVHcloud. Si Meteo-France change de region ou
    # d'hebergeur, le refus nomme l'hote : l'ajouter ici, en connaissance de cause.
    hotes_autorises = c("data.gouv.fr", "meteofrance.s3.sbg.io.cloud.ovh.net"),
    taille_max_mo   = 500,
    redirections_max = 5L,

    # Stations PUBLIEES (v2.1). Comme les mailles de la carte, les indicateurs annuels des
    # stations sont precalcules (outils/stations_precalcul.R) et publies a cote des fichiers
    # de la carte : stations.json (la liste, avec de quoi juger la qualite) et
    # s_<grandeur>.bin.gz (meme format binaire que c_<grandeur>.bin.gz). Cela permet de
    # comparer une station a une maille ou a un departement SANS telecharger de serie
    # journaliere (le Comparer, la Synthese et le Terrain n'appellent aucun service).
    publie = list(
      # Version du FORMAT de stations.json. A incrementer si sa disposition change.
      schema         = 1L,
      fichier        = "stations.json",
      # Une station n'est publiee que si elle a au moins ce nombre d'annees VALIDES (voir
      # seuil_annee) pour la pluie ou pour les temperatures : en deca, ni normale (dix
      # annees de reference au moins) ni tendance (vingt annees au moins) ne sont possibles.
      annees_min     = 15L,
      # Une station dont la derniere mesure a plus de tant de jours n'est pas publiee : elle
      # ne decrit plus le climat d'aujourd'hui. Les stations arretees depuis moins longtemps
      # le sont, avec une qualite « fraicheur » en consequence.
      inactive_max_j = 730L,
      # Garde-fou de lecture : un stations.json qui en annonce davantage est refuse. La base
      # quotidienne compte quelques milliers de postes.
      stations_max   = 8000L
    )
  ),

  # ---------------------------------------------------------------------------
  # 3. Variables SAFRAN mobilisees
  # ---------------------------------------------------------------------------
  # Cle  = nom interne (colonne du tableau journalier manipule dans l'app)
  # $api = nom du parametre EDR (SIM2)
  # $lib = libelle affiche a l'utilisateur
  # $unite, $obligatoire (TRUE = l'app s'arrete si la variable manque)
  #
  # NB IMPORTANT : les precipitations SAFRAN sont fournies en DEUX variables
  # distinctes. PRELIQ_Q ne contient QUE la pluie, PRENEI_Q QUE la neige.
  # Le cumul total est donc TOUJOURS p_liq + p_sol (cf. utils_api.R). Ne jamais
  # utiliser PRELIQ_Q seul comme "precipitations".
  # ---------------------------------------------------------------------------
  variables = list(
    t_moy  = list(api = "T_Q",         lib = "Température moyenne",           unite = "°C",  obligatoire = TRUE),
    t_min  = list(api = "TINF_H_Q",    lib = "Température minimale",          unite = "°C",  obligatoire = TRUE),
    t_max  = list(api = "TSUP_H_Q",    lib = "Température maximale",          unite = "°C",  obligatoire = TRUE),
    p_liq  = list(api = "PRELIQ_Q",    lib = "Précipitations liquides",       unite = "mm",    obligatoire = TRUE),
    p_sol  = list(api = "PRENEI_Q",    lib = "Précipitations solides (neige)",unite = "mm",    obligatoire = TRUE),
    etp    = list(api = "ETP_Q",       lib = "ETP Penman-Monteith",           unite = "mm",    obligatoire = FALSE),
    swi    = list(api = "SWI_Q",       lib = "Indice d'humidité des sols",    unite = "-",     obligatoire = FALSE),
    neig_h = list(api = "HTEURNEIGE_Q",lib = "Hauteur de neige au sol",       unite = "m",     obligatoire = FALSE),
    neig_e = list(api = "RESR_NEIGE_Q",lib = "Équivalent en eau du manteau",  unite = "mm",    obligatoire = FALSE),

    # AJOUTS — vent et humidite relative.
    # Declarees facultatives comme les precedentes : si la collection ne les
    # expose pas, elles arrivent a NA, les indicateurs concernes affichent
    # « Variable non renseignée sur la période » et RIEN D'AUTRE ne bouge.
    # C'est le meme mecanisme que celui qui a deja neutralise HTEURNEIGE_Q.
    #
    # FF_Q est un VENT MOYEN JOURNALIER a 10 m, pas une rafale. Voir la fiche
    # methode de l'indicateur « vent » : cette distinction commande tout ce que
    # l'on peut, et surtout ne peut pas, en conclure sur le risque de chablis.
    ff     = list(api = "FF_Q",        lib = "Vent moyen à 10 m",            unite = "m/s", obligatoire = FALSE),
    hu     = list(api = "HU_Q",        lib = "Humidité relative",            unite = "%",   obligatoire = FALSE)
  ),

  # ---------------------------------------------------------------------------
  # 4. Seuils bioclimatiques (parametrables par l'utilisateur : onglet Parametres)
  # ---------------------------------------------------------------------------
  seuils = list(
    gel_t             = 0,    # degC — jour de gel si t_min < ce seuil
    gel_severe_t      = -5,   # degC — gel fort (v2.4 : trois seuils de gel, 0, -5 et -10)
    gel_extreme_t     = -10,  # degC — gel tres fort
    # GEL TARDIF (v2.4) : un gel qui survient APRES le debourrement. La date du
    # debourrement n'est pas observee : elle est estimee par un modele de forcage
    # thermique (somme des degres-jours depuis le 1er janvier, base `gdd_base`),
    # qui s'adapte au climat du point et a l'annee. Voir la fiche methode du
    # gel tardif (registre_indicateurs.R) pour les sources et les limites.
    gel_tardif_t      = 0,    # degC — gel tardif si t_min < ce seuil. Les seuils de dommage
                              # varient d'une essence a l'autre (de -1 a -7,5 degC,
                              # Bigler et Bugmann 2018) : 0 compte l'EXPOSITION, pas le degat.
    gel_tardif_gdd    = 200,  # degres-jours (base gdd_base) cumules depuis le 1er janvier
                              # pour atteindre le debourrement estime. VALEUR PAR DEFAUT
                              # INDICATIVE, non specifique d'une essence : a caler sur les
                              # observations de debourrement de l'essence visee.
    gel_tardif_fin    = 181,  # jour julien de fin de la saison de gel tardif (30 juin)
    chaleur_t1        = 25,   # degC — jour d'ete (t_max)
    chaleur_t2        = 30,   # degC — forte chaleur
    chaleur_t3        = 35,   # degC — chaleur extreme (seuil de stress majeur)
    chaleur_t4        = 40,   # degC — chaleur tres extreme
    vague_ecart       = 5,    # degC — un jour de vague de chaleur a une T max d'au
                              # moins cet ecart AU-DESSUS DE LA NORMALE DU JOUR
                              # (seuil RELATIF, contrairement a chaleur_t1 a t3)
    vague_duree       = 5,    # jours consecutifs requis pour parler de vague
                              # (indice HWDI : Frich et al. 2002)
    gdd_base          = 5,    # degC — temperature de base des degres-jours
    gdd_plafond       = 30,   # degC — plafonnement de la t_moy journaliere
    veg_seuil_t       = 5,    # degC — seuil de demarrage/arret de vegetation
    veg_n_jours       = 5,    # nb de jours consecutifs requis
    veg_doy_min       = 32,   # recherche du debut a partir du 1er fevrier
    veg_doy_split     = 213,  # recherche de la fin a partir du 1er aout
    sec_seuil_mm      = 1,    # mm/j — journee consideree seche en-deca
    sec_duree_alerte  = 15,   # jours — duree de sequence seche declenchant une alerte
    swi_stress        = 0.5,  # SWI en-deca duquel le sol est contraignant
    swi_severe        = 0.3,  # SWI de stress severe
    vent_fort_ms      = 8,    # m/s — journee venteuse, seuil ABSOLU
    vent_percentile   = 0.95, # percentile de reference definissant le vent
                              # « soutenu pour ce point » — seuil RELATIF
    swi_sature        = 0.9,  # SWI au-dela duquel le sol est considere gorge
                              # d'eau, donc l'ancrage racinaire affaibli
    vpd_stress_kpa    = 1.5   # kPa — repere CONVENTIONNEL d'un air tres sec, applique
                              # au VPD MOYEN journalier (v1.5, audit M5). Ce n'est
                              # pas « la » valeur de fermeture des stomates : cette
                              # fermeture est progressive et propre a chaque essence
                              # (Grossiord et al. 2020), et elle se produit au
                              # VPD d'apres-midi, plus eleve que la moyenne du jour.
  ),

  # ---------------------------------------------------------------------------
  # 4b. Constantes de methode (NON reglables dans l'application)
  # ---------------------------------------------------------------------------
  # Ce ne sont pas des seuils d'interpretation : elles delimitent ce que mesure
  # un indicateur. Elles vivent ici plutot que dans le code (audit, C4 : « source
  # unique de verite »), mais ne figurent ni dans Parametres ni dans l'annexe des
  # rapports, qui ne listent que ce que l'on peut regler.
  methode = list(
    # Dernier jour de l'annee (jour julien 181 = 30 juin) retenu pour « date du
    # dernier gel de printemps ». Au-dela, un gel est tardif pour une autre raison
    # (altitude) et fausserait la date de fin de la saison de gel.
    gel_printemps_doy_max = 181L,
    # Vagues de chaleur : la saison ou on les cherche (mai a septembre) et la
    # fenetre qui lisse la normale du jour. Hors de ces mois, +5 degC pendant cinq
    # jours est un redoux, pas une vague de chaleur, et la compter fausserait le
    # nombre de vagues. La fenetre de cinq jours est celle de l'indice HWDI.
    vague_mois = c(5L, 9L),
    vague_fenetre_normale = 5L,
    # Annees de reference (de plus de 300 jours renseignes) sans lesquelles la
    # normale du jour n'est pas calculee : chaque jour du calendrier n'a que
    # trente valeurs au mieux, dix est un plancher.
    vague_ref_annees_min = 10L
  ),

  # ---------------------------------------------------------------------------
  # 5. Periodes de reference climatologiques
  # ---------------------------------------------------------------------------
  references = list(
    "1991-2020" = c(1991, 2020),   # normale OMM en vigueur
    "1981-2010" = c(1981, 2010),
    "1971-2000" = c(1971, 2000),
    # 1961-1990 : repere anterieur a l'essentiel du rechauffement recent. Ce
    # n'est PAS la base « pre-industrielle » du GIEC, qui est 1850-1900 (audit
    # M9) ; SAFRAN, qui commence en 1959, ne peut d'ailleurs pas la fournir.
    "1961-1990" = c(1961, 1990)
  ),
  reference_defaut = "1991-2020",

  # Bornes temporelles de la reanalyse SAFRAN
  annee_min = 1959,

  # ---------------------------------------------------------------------------
  # 6. Fenetres saisonnieres predefinies (P-ETP, bilans estivaux)
  # ---------------------------------------------------------------------------
  fenetres = list(
    "Année civile (jan-déc)"    = c(1, 12),
    "Saison de végétation (avr-oct)" = c(4, 10),
    "Étendue estivale (mai-sep)"= c(5, 9),
    "Cœur d'été (juin-août)"   = c(6, 8),
    "Printemps (mars-mai)"      = c(3, 5)
  ),
  fenetre_defaut = c(4, 10),

  # ---------------------------------------------------------------------------
  # 7. Charte graphique — verts et terres de la foret (voir www/climafor.css)
  # ---------------------------------------------------------------------------
  couleurs = list(
    vert          = "#00664B",  # liens, boutons, point choisi sur la carte
    alerte        = "#B5472E",  # terre cuite : gel tardif, chaleur
    texte         = "#1E2A23",
    texte_mention = "#56635B",
    bordure       = "#D9DFD6",
    # Couleurs fonctionnelles
    succes        = "#18753C",
    attention     = "#B34000",
    erreur        = "#CE0500",
    # Palette metier utilisee dans les graphiques
    pluie         = "#2D6F94",   # ciel
    neige         = "#8DB6CC",
    temperature   = "#B5472E",   # terre cuite
    etp           = "#A8561A",   # ocre
    reference     = "#56635B",
    annee_courante= "#12382B",   # vert sapin
    deficit       = "#B5472E",
    excedent      = "#2D6F94",
    neutre        = "#7C8A80",   # ecart faible : ni au-dessus ni en dessous
    en_cours      = "#A9B8A8"    # annee en cours, incomplete
  ),

  # ---------------------------------------------------------------------------
  # 8. Rendu graphique
  # ---------------------------------------------------------------------------
  graphiques = list(
    # Interrupteur unique. FALSE ramene toute l'application a des images
    # statiques, sans autre modification : les fonctions plot_* produisent
    # toujours des ggplot, plotly n'est qu'une couche de rendu.
    interactif = TRUE,

    hauteur_web    = 380,  # px
    hauteur_mobile = 300,  # px
    # Pile typographique des graphiques interactifs (plotly) : une seule
    # definition, lue par utils_plot.R. Open Sans est servie par l'application
    # (www/fonts) ; Arial et sans-serif prennent le relais a defaut.
    police         = "Open Sans, Arial, sans-serif",
    lissage_annees = 10,   # fenetre de la moyenne mobile (evolutions annuelles)
    dpi_export     = 150,
    largeur_export = 10,   # pouces
    hauteur_export = 5.5
  ),

  # ---------------------------------------------------------------------------
  # 9. Cache disque (accelere les rechargements, vide a chaque redeploiement)
  # ---------------------------------------------------------------------------
  cache = list(
    actif      = TRUE,
    repertoire = "cache",
    duree_j    = 30,      # duree de validite d'une entree
    precision  = 3,       # arrondi des coordonnees (3 dec. ~ 100 m < maille 8 km)
    # Version du FORMAT des fichiers de cache. A incrementer des que la forme des
    # donnees brutes change : les anciennes entrees ne sont alors plus lues. Les
    # colonnes derivees (p_tot, vpd) n'en dependent pas : elles sont recalculees
    # a chaque lecture (safran_deriver), un correctif de formule s'applique donc
    # tout de suite, y compris aux annees deja en cache.
    # 3 : fichiers JSON compresses (.json.gz) a la place de RDS (audit V7).
    schema     = 3L,
    # Bornes du dossier de cache (audit V1). Au-dela de `taille_max_mo`, les
    # fichiers les plus anciens sont supprimes jusqu'a retomber a 80 % du
    # plafond. La purge tourne au demarrage puis, au plus, toutes les
    # `purge_intervalle_min` minutes apres une ecriture.
    taille_max_mo        = 300,
    purge_intervalle_min = 10,
    # Taille maximale (decompressee) d'un fichier relu : un fichier plus gros est
    # ignore, jamais charge en memoire.
    lecture_max_mo       = 100
  ),

  # ---------------------------------------------------------------------------
  # 9b. Limites d'usage (audit V2)
  # ---------------------------------------------------------------------------
  # Un seul processus R sert toutes les sessions : une rafale de demandes par
  # une personne ralentit les autres et sollicite des services tiers. Les
  # limites ci-dessous sont des fenetres glissantes ; 0 ou NA desactive la limite.
  # Elles se comptent PAR SESSION et PAR PROCESSUS, jamais par adresse IP : le
  # serveur d'application ne voit pas l'adresse de l'utilisateur de facon fiable.
  limites = list(
    fenetre_min          = 10,   # longueur de la fenetre glissante
    analyses_session     = 10,   # analyses lancees par une session
    rapports_session     = 5,    # rapports demandes par une session
    telechargements_proc = 40    # analyses qui interrogent un service distant,
                                 # toutes sessions confondues
  ),

  # ---------------------------------------------------------------------------
  # 10. Tendances et lecture des ecarts
  # ---------------------------------------------------------------------------
  tendance = list(
    # Une pente sur moins de 20 annees completes est trop instable pour etre
    # affichee : l'application le dit au lieu de tracer une droite.
    n_min        = 20L,
    niveau       = 0.95,   # niveau de confiance des intervalles
    # Un ecart est dit « proche de la normale » tant qu'il reste inferieur a cette
    # fraction de l'ecart-type des annees de reference. Au-dela seulement, il est
    # colore : colorer 0,01 degC d'ecart comme 2 degC serait une fausse precision.
    bande_neutre = 0.5
  ),

  # ---------------------------------------------------------------------------
  # 11. Rapport
  # ---------------------------------------------------------------------------
  rapport = list(
    niveaux       = c("decideur", "ingenieur", "technique"),
    niveau_defaut = "ingenieur",
    contexte_max  = 1500L,   # caracteres du champ « contexte de la demande »
    dpi           = 110,     # resolution des figures (150 alourdissait l'HTML de 40 %)
    delai_max_s   = 300      # au-dela, la generation en arriere-plan est abandonnee
  ),

  # ---------------------------------------------------------------------------
  # 12. Onglet Carte (v1.8) — voir src/utils_carte.R
  # ---------------------------------------------------------------------------
  carte = list(
    # Version du FORMAT des fichiers publies. A incrementer si leur disposition
    # change : un jeu d'une autre version n'est pas affiche, au lieu d'etre mal lu.
    schema        = 1L,
    # Adresse https des fichiers publies (GitHub Pages du depot climafor_cartes,
    # par exemple « https://<compte>.github.io/climafor_cartes/ »).
    # Premier calcul publie le 4 novembre 2026. Mettre "" pour utiliser le dossier local
    # ci-dessous (ou definir CLIMAFOR_CARTE_URL="" ). La variable
    # d'environnement CLIMAFOR_CARTE_URL, si elle existe, la remplace : pratique
    # pour essayer un jeu sans modifier le code.
    url_donnees   = Sys.getenv("CLIMAFOR_CARTE_URL", "https://cammbrnd.github.io/climafor_cartes/"),
    # Dossier servi par l'application elle-meme quand url_donnees est vide :
    # aucun tiers n'est alors contacte, au prix d'un redeploiement a chaque mise
    # a jour des donnees.
    dossier_local = "donnees_carte",
    ressource     = "carte-donnees",   # prefixe d'adresse du dossier local
    # Garde-fou de lecture : un meta.json qui annonce plus de mailles que cela
    # est refuse (la grille SAFRAN de la France metropolitaine en compte environ 9 900).
    n_mailles_max = 20000L
  ),

  # ---------------------------------------------------------------------------
  # 13. Onglet Comparer (v1.9)
  # ---------------------------------------------------------------------------
  # Points et zones importes par l'utilisateur, ramenes aux mailles de la carte.
  # Ces bornes protegent le serveur (un seul processus R pour toutes les sessions) :
  # elles ne sont pas des reglages d'analyse.
  comparer = list(
    lieux_max         = 40L,    # lieux comparables a la fois
    lignes_max        = 500L,   # lignes lues dans un CSV de points
    entites_max       = 100L,   # entites lues dans un fichier de zones
    fichier_max_mo    = 10,     # taille d'un fichier importe (= shiny.maxRequestSize)
    zip_fichiers_max  = 30L,    # fichiers dans une archive de shapefile
    zip_taille_max_mo = 100,    # taille de l'archive une fois decompressee
    # Un point a plus de 6 km du centre de la maille la plus proche n'est pas dans la
    # grille SAFRAN : une maille de 8 km est a 5,7 km de ses coins, au plus. Au-dela,
    # c'est la mer ou l'etranger.
    distance_max_m    = 6000,
    mailles_zone_max  = 3000L,  # une zone plus grande est refusee (la France en compte ~9 900)
    taille_carte_max_mo = 30,   # un fichier de la carte telecharge par le serveur
    cache_h           = 24,     # duree de conservation des fichiers de la carte telecharges
    annees_ref_min    = 10L,    # annees de reference sans lesquelles il n'y a pas de normale
    couches_defaut    = c("tmp", "can", "vag", "gel", "pre", "petp"),
    # Okabe-Ito (sans le jaune, illisible sur fond blanc) : distinguables par un
    # daltonien. Au-dela de huit lieux, le graphique ne trace que les huit premiers.
    palette = c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
                "#E69F00", "#56B4E9", "#444444", "#8C510A")
  ),

  # ---------------------------------------------------------------------------
  # 14. Profils et acces (v2.0)
  # ---------------------------------------------------------------------------
  # TROIS PROFILS : « terrain » (telephone, consultation), « co » (correspondant-
  # observateur, vue simplifiee) et « dsf » (permanents, tous les outils). Le profil
  # se lit dans l'adresse (?id=...) : la liste des identifiants est un fichier,
  # config/acces.csv (et config/acces_local.csv s'il existe, qui n'est pas versionne
  # et qui l'emporte). CECI ORIENTE, NE PROTEGE PAS : un identifiant est lisible dans
  # l'adresse, et quiconque le connait obtient le profil. Une vraie authentification
  # viendra avec l'API (voir README).
  acces = list(
    fichier       = "config/acces.csv",
    fichier_local = "config/acces_local.csv",
    # TRUE : la page d'entree propose les trois profils et accepte ?profil=co, utile
    # pour une demonstration. FALSE en exploitation : seuls les identifiants du
    # fichier ouvrent un profil.
    demo          = TRUE,
    id_long_max   = 40L
  ),

  # ---------------------------------------------------------------------------
  # 15. Territoires et bilan synthetique (v2.0)
  # ---------------------------------------------------------------------------
  territoires = list(dossier = "donnees_limites"),

  synthese = list(
    fiches_max      = 500L,    # fiches lues dans un import
    territoires_max = 12L,     # departements ou regions compares a la fois
    # Jeu de base de chaque profil : grandeurs dont le graphique (g) et le tableau (t)
    # sont coches d'emblee. Les autres se cochent a la main.
    base = list(
      terrain = list(g = c("tmp", "can", "vag", "pre", "petp"),
                     t = c("tmp", "can", "vag", "pre", "petp")),
      co      = list(g = c("tmp", "can", "pre", "petp"),
                     t = c("tmp", "can", "vag", "pre", "petp", "swi")),
      dsf     = list(g = c("tmp", "can", "vag", "pre", "petp", "swi"),
                     t = c("tmp", "can", "vag", "gel", "pre", "petp", "swi", "dry"))
    ),
    # Territoires preselectionnes quand aucun point n'est choisi : un bilan s'affiche
    # des l'ouverture, ce qui sert la demonstration. Le departement du point choisi,
    # s'il y en a un, l'emporte.
    defaut = list(departement = "31", region = "76"),
    # Grandeurs offertes au correspondant-observateur et au terrain (le profil dsf
    # voit tout le jeu publie).
    couches_co = c("tmp", "jja_t", "can", "can3", "vag", "pre", "jja_p", "petp", "swi", "dry",
                   "gel", "geltd")
  ),

  # Application de saisie terrain (Sylvasan) : le bilan ne saisit rien, il s'y rattache.
  # url_saisie : adresse ouverte par le bouton « Saisir une fiche » (vide : pas de
  # bouton). api_fiches : adresse de l'API de lecture des fiches (vide : import de
  # fichier seulement).
  sylvasan = list(url_saisie = "", api_fiches = "")
)

# ------------------------------------------------------------------------------
# Helpers de lecture de la configuration
# ------------------------------------------------------------------------------

#' Noms des parametres EDR a demander a l'API
#' @return vecteur de chaines, ex. c("T_Q","TINF_H_Q",...)
cfg_params_api <- function() {
  vapply(CONFIG$variables, function(v) v$api, character(1), USE.NAMES = FALSE)
}

#' Table de correspondance nom API -> nom interne
cfg_table_variables <- function() {
  stats::setNames(names(CONFIG$variables), cfg_params_api())
}

#' Libelle des mois en francais, independamment de la locale du serveur
#' (les serveurs shinyapps.io sont en locale C : on ne peut pas se fier a
#' format(date, "%b"), qui renverrait des abreviations anglaises.)
MOIS_COURT <- c("Jan", "Fév", "Mar", "Avr", "Mai", "Juin",
                "Juil", "Août", "Sep", "Oct", "Nov", "Déc")
MOIS_LONG  <- c("janvier", "février", "mars", "avril", "mai", "juin",
                "juillet", "août", "septembre", "octobre", "novembre", "décembre")
