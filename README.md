# ClimaFor — bilan météorologique et climatique pour la gestion forestière

Application R/Shiny produisant, pour n'importe quel point de France
métropolitaine, un diagnostic climatique complet appuyé, au choix, sur la
**station Météo-France la plus proche** (observations, avec une évaluation de
sa qualité) ou sur la réanalyse **SAFRAN-ISBA (SIM2)**, interrogée via l'API
OGC EDR de **GeoSAS (INRAE)**.

Destinataires : techniciens, ingénieurs et décideurs. Trois niveaux de rapport
sont prévus pour cela.

**Depuis la version 2.0**, trois interfaces (terrain sur téléphone, correspondant-observateur, permanent
DSF) et un **bilan synthétique** par département, région, fiches importées ou point : voir
[Version 2.0](#version-20--trois-profils-et-bilan-synthétique). Pour une démonstration :
`?profil=co`, `?profil=dsf` ou `?profil=terrain` à la suite de l'adresse de l'application.

**Depuis la version 2.1**, les stations Météo-France sont publiées avec la carte (mesure et modèle côte à côte), l'onglet
Comparer compare deux lieux de n'importe quelle sorte (maille, station, département) et l'écran terrain produit une
synthèse à envoyer par courriel : voir [Version 2.1](#version-21--mesures-et-modèle-côte-à-côte-deux-lieux-synthèse-à-envoyer).

**Depuis la version 2.2**, le Bilan d'un point peut se lire dans un jeu **précalculé** (mode rapide) au lieu d'être
recalculé année par année ; **depuis la 2.3**, cela vaut aussi pour les stations Météo-France : voir
[Version 2.2](#version-22--une-base-précalculée-pour-le-bilan-dun-point-mode-rapide). L'historique des versions
est dans [NEWS.md](NEWS.md).

---

## Démarrage

```r
# Depuis la racine du projet
install.packages(c("shiny", "htmltools", "ggplot2", "leaflet",
                   "httr2", "jsonlite", "rmarkdown", "knitr"))
shiny::runApp()
```

Déploiement sur shinyapps.io :

```r
source("deploy.R")
```

`deploy.R` vérifie les dépendances et la cohérence du registre avant l'envoi.

Le fichier `.rscignore` (un nom par ligne, sans joker, les deux casses listées) écarte du paquet les fichiers de développement — tests, diagnostics, `deploy.R`, maquette, cache. Il sert au bouton « Publish » de RStudio ; `deploy.R` fournit déjà sa propre liste.

**Partage réseau qui passe `.R` en `.r`.** Certains partages renomment tout en minuscules (`config.r`, `app.r`, `rapport_climat.rmd`). Windows ne le voit pas, Linux et le serveur de déploiement si, et l'application ne démarrait plus (« application failed to start »). Le code retrouve désormais ses fichiers quelle que soit la casse : `chemin_ci()` dans `global.R` pour `src/` et `rapport/`, un motif sans casse dans `app.R` et `deploy.R`. Rien à renommer. L'avertissement RStudio « Filepaths are case-sensitive » peut donc subsister : il est sans effet. Seul `.Rprofile` (devenu `.rprofile`) n'est plus lu par R, sans conséquence ici. `corriger_casse.R` reste disponible pour remettre les vraies casses si le disque le permet.

---

## Ce que fait l'application

**Désigner un point en un geste** — recherche de commune ou code postal, ou
« Utiliser ma position » (géolocalisation du terminal, utile en tournée). Le
placement sur la carte IGN et la saisie de coordonnées restent à un clic.

**Source des données : station ou grille** — dès qu'un point est désigné,
l'application propose la station Météo-France la plus pertinente avec sa
qualité (voir « Stations Météo-France » ci-dessous), deux autres stations, et
la grille SAFRAN en alternative.

**Quatorze indicateurs**, regroupés en six thématiques :

| Thématique | Indicateurs |
|---|---|
| Eau et bilan hydrique | Bilan hydrique mensuel, P − ETP sur fenêtre choisie, SWI estival, séquences sèches, précipitations annuelles |
| Températures | Évolution thermique annuelle, été JJA, jours de chaleur |
| Gel | Jours de gel et gel tardif |
| Aridité et bioclimat | Diagramme ombrothermique, indice de De Martonne |
| Phénologie | Degrés-jours de croissance, saison de végétation |
| Neige | Précipitations neigeuses |

**Période de référence au choix** — 1991-2020 (norme OMM), 1981-2010,
1971-2000, 1961-1990, ou toute période personnalisée. L'année comparée et la
plage affichée se règlent indépendamment.

**Fenêtre du P − ETP configurable** — juin-août pour le stress de pointe,
avril-octobre pour la saison de végétation, mai-septembre, ou tout couple de
mois, y compris à cheval sur deux années civiles.

**Rapport** — HTML autonome ou Word modifiable, à trois niveaux de détail.

**Export CSV** du bilan (synthèse et séries annuelles) et **comparaison de lieux**
importés (points ou zones) avec leur bilan climatique : voir « Version 1.9 ».

---

## Stations Météo-France

**D'où viennent les données.** Fichiers ouverts « Données climatologiques de
base — quotidiennes » (data.gouv.fr, licence Etalab 2.0, sans clé), un jeu de
fichiers par département. Ils sont mis à jour environ une fois par jour :
c'est du **quasi quotidien, pas du temps réel**. L'API officielle temps réel
(DPObs) exige une clé et ne conserve que 24 h : elle ne convient pas à un bilan
climatique. La liste des fichiers est lue sur data.gouv.fr à chaque session
(cache 24 h) plutôt qu'écrite en dur, parce que les noms de fichiers changent
chaque année.

**Quelle station.** Les stations à moins de 40 km sont classées sur une
distance corrigée : 1 m d'écart d'altitude compte pour 100 m de distance, et
une station sans températures, sans pluie ou arrêtée est pénalisée. Une
station de fond de vallée n'est donc pas proposée pour un point de crête, ni un
pluviomètre voisin à la place d'une station complète un peu plus loin. Les
réglages sont dans `CONFIG$stations`.

**Deux niveaux de qualité.**

1. *À la proposition* — le plus mauvais de quatre critères : distance, écart
   d'altitude, fraîcheur de la dernière mesure, part des 12 derniers mois
   renseignée. Niveaux : bonne, correcte, limitée, insuffisante. Une station qui
   ne mesure que la pluie ou que les températures n'est jamais mieux que
   « limitée ». Si la meilleure station est « insuffisante », ou si Météo-France
   ne répond pas, la grille SAFRAN est proposée par défaut, et le dit.
2. *À l'analyse* — part des années de la période de référence choisie qui sont
   exploitables, années écartées nommées. C'est elle qui dit si une normale
   1991-2020 est possible sur cette station.

**Calculer avec ce qui existe.** Une année n'est utilisée pour un indicateur que
si au moins 95 % de ses jours sont renseignés pour les variables qu'il emploie.
Une année lacunaire est **absente** du graphique, jamais comptée comme zéro. Un
indicateur dont une variable n'est pas mesurée (gel sans températures, par
exemple) n'est pas affiché, plutôt que de montrer « 0 jour de gel » ; une note en
fin de bilan liste ce qui manque et pourquoi.

**Ce qu'une station ne donne pas.** Ni humidité du sol (SWI), ni hauteur de
neige, ni humidité de l'air, ni séparation pluie/neige. L'ETP est estimée par
Hargreaves-Samani à partir des températures (pas d'ETP mesurée). Les
indicateurs correspondants disparaissent d'une analyse sur station ; la grille
SAFRAN les fournit tous.

**À FAIRE AU PREMIER ESSAI RÉEL.** Le module a été écrit et testé sur des
fichiers synthétiques au format Météo-France, **sans accès aux serveurs
réels** (réseau restreint à l'écriture). Avant de compter dessus :

```r
source("diagnostic_stations.R")
```

Le script vérifie, étape par étape, la liste des fichiers, les colonnes, la
**répartition des codes qualité** (leur signification n'a pas pu être vérifiée :
seul le code 2, « douteux », est écarté par défaut, voir `CONFIG$stations$qualite_ecartee`),
la station retenue pour un point de test, et le chargement d'une série complète
avec son temps de réponse. Un premier chargement de l'historique peut prendre
de quelques secondes à une minute selon la taille du fichier du département.

---

## Deux points de méthode qui méritent attention

### Précipitations totales

SAFRAN distingue `PRELIQ_Q` (pluie) et `PRENEI_Q` (neige, en équivalent en
eau). **Le total est toujours la somme des deux.** La règle est appliquée une
seule fois, dans `safran_parser()`, et la colonne `p_tot` en découle : aucune
fonction en aval ne peut se tromper. Retenir `PRELIQ_Q` seul sous-estimerait de
plus de 30 % les cumuls au-dessus de 1 200 m dans les Alpes du Nord.

### Comparaison à période égale

L'année en cours est incomplète et SAFRAN accuse quelques jours de retard.
Comparer ses cumuls à une normale annuelle complète produirait un déficit
artificiel. `calc_synthese()` tronque donc la référence au dernier jour
disponible avant toute comparaison, et l'interface indique la date de coupure.

---

## En cas de « Aucune donnée pour ce point »

Lancez depuis la racine du projet :

```r
source("diagnostic.R")
```

Le script interroge l'API pas à pas — métadonnées de la collection, requête
minimale, requête complète, puis variable par variable — et affiche à chaque
étape l'URL réellement envoyée et la réponse réellement reçue, code HTTP et
corps d'erreur compris.

La cause la plus fréquente est qu'une collection EDR n'expose pas tous les
paramètres SIM2 : demander un paramètre absent fait échouer **toute** la
requête, pas seulement ce paramètre. L'application négocie désormais les
variables au démarrage (lecture des métadonnées de la collection, puis
intersection avec ce qu'elle sait exploiter) et signale dans l'interface celles
qui ont été écartées. Si `ETP_Q` manque, l'ETP est recalculée par
Hargreaves-Samani ; si `TINF_H_Q` manque, la température minimale est estimée
par symétrie — dans les deux cas l'interface le dit explicitement.

---

## Historique des versions

Le détail de chaque version est dans [NEWS.md](NEWS.md). Pour qui reprend le projet : la v1.2 à la v1.7 sont des
campagnes de corrections issues d'un audit (données, ergonomie, rigueur scientifique, solidité, sécurité) ;
les versions suivantes ajoutent des fonctions et sont décrites dans ce README.

| Version | Ajout | Où |
|---|---|---|
| 2.3 | Mode rapide pour les stations ; tests automatiques | [Version 2.2](#version-22--une-base-précalculée-pour-le-bilan-dun-point-mode-rapide) |
| 2.2 | Base précalculée, mode rapide (mailles) | [Version 2.2](#version-22--une-base-précalculée-pour-le-bilan-dun-point-mode-rapide) |
| 2.1 | Stations publiées, deux lieux, synthèse du terrain | [Version 2.1](#version-21--mesures-et-modèle-côte-à-côte-deux-lieux-synthèse-à-envoyer) |
| 2.0 | Trois profils, bilan synthétique | [Version 2.0](#version-20--trois-profils-et-bilan-synthétique) |
| 1.9 | Vagues de chaleur, export CSV, Comparer | [Version 1.9](#version-19--vagues-de-chaleur-export-csv-onglet-comparer) |
| 1.8 | Onglet Carte | [Version 1.8](#version-18--onglet-carte) |
| 1.2 → 1.7.3 | Audit : corrections et durcissement | [NEWS.md](NEWS.md) |

## Version 1.8 — onglet Carte

Un nouvel onglet **Carte** affiche un indicateur annuel pour toute la France, maille
SAFRAN par maille SAFRAN (8 km), avec un dégradé de couleur : sa **valeur** une année
donnée, son **écart à la normale**, ou sa **tendance** 1959–dernière année. Un curseur
et un bouton « Lecture » font défiler les années ; un clic sur une maille affiche ses
valeurs et une courbe, et « Analyser cette maille » ouvre l'analyse complète de ce point
(onglet Bilan, source SAFRAN). 18 indicateurs sont cartographiables
(`src/carte_couches.R`) ; les profils mensuels et journaliers ne le sont pas.

### Pourquoi c'est réactif

Une carte Shiny classique redessine ses polygones et renvoie leurs données par le
websocket à chaque changement d'année. Ici Shiny ne sert que de squelette et de
passerelle ; **tout le reste se passe dans le navigateur** (`www/carte/carte.js`,
MapLibre GL JS embarqué) :

- la géométrie des mailles est chargée **une fois** ;
- les valeurs de **toutes les années** d'un indicateur arrivent dans **un seul** fichier
  d'entiers 16 bits compressé (`c_<indicateur>.bin.gz`) ;
- changer d'année, de mode ou d'indicateur ne fait que **recolorer** (`setFeatureState`),
  sans aller-retour avec le serveur ;
- MapLibre et les fichiers ne sont chargés qu'à la **première ouverture de l'onglet**.

### D'où viennent les valeurs

L'application **ne calcule pas la carte**. Un précalcul (`outils/carte_*.R`, lancé par
GitHub Actions) appelle, pour chaque maille, **les fonctions de calcul mêmes de
l'application** (`INDICATEURS[[id]]$calc`, `tendance()`, `stats_ref()`) : une valeur de
la carte est celle que l'onglet d'analyse afficherait pour ce point. `tests/test_v18.R`
le vérifie au bit près. Les années incomplètes (SAFRAN publie avec retard) ne sont
cartographiées pour aucun indicateur.

Les séries journalières viennent par défaut des **fichiers annuels en masse de
Météo-France** (source `sim` : « Données changement climatique — SIM quotidienne »,
data.gouv.fr, Licence Ouverte 2.0), c'est-à-dire la source même de GeoSAS : la carte ne
fait **aucune requête à GeoSAS** (hors une lecture de métadonnées), et la grille est la
grille native de 9 892 mailles, enumérée par le fichier de coordonnées de Météo-France.
Les carrés sont dessinés en Lambert II étendu (`outils/carte_geo.R`) : leurs bords sont
exacts.

```
GitHub Actions ──► outils/carte_cli.R ──► fichiers (meta.json, grille.json, c_*.bin.gz)
 (calcul planifié)   (fragments, assemblage)        │
                                                    ▼  GitHub Pages  ou  donnees_carte/
                                  navigateur ◄── carte.js ◄── onglet Carte (squelette Shiny)
```

Format des fichiers : décrit en tête de `src/utils_carte.R` (version `schema` 1).
`meta.json` est **validé** avant d'être transmis au navigateur (noms de fichiers,
palettes, bornes) ; le navigateur le revalide ; aucun texte venu des fichiers n'est
inséré comme du HTML.

### Où l'application lit les fichiers

1. **Adresse distante** : variable d'environnement `CLIMAFOR_CARTE_URL` (ou
   `CONFIG$carte$url_donnees`), en `https`, par exemple
   `https://<compte>.github.io/climafor_cartes/`. Les fichiers sont alors chargés
   **directement par le navigateur** ; cet hébergeur voit l'adresse IP du visiteur
   (précisé dans « Sources et méthodes > Position et confidentialité »).
2. À défaut, le dossier **`donnees_carte/`** à côté de l'application, servi par
   l'application elle-même (aucun tiers).
3. À défaut, l'onglet explique qu'il n'y a pas encore de données.

`deploy.R` **refuse** de publier si `donnees_carte/` contient le jeu fictif.

### Essayer sans réseau (données FICTIVES)

```
Rscript outils/carte_cli.R fragment source=demo mailles=demo:32 annee_debut=1959 annee_fin=2025 fragment=1 fragments=1 sortie=essai/parts
Rscript outils/carte_cli.R assembler "source_texte=DONNEES FICTIVES (essai)" mailles=demo:32 annee_debut=1959 annee_fin=2025 parts=essai/parts sortie=donnees_carte
```

≈ 570 mailles, ≈ 5 minutes. La mention « DONNEES FICTIVES » s'affiche sous la carte.
**Supprimez `donnees_carte/` avant de déployer** (`deploy.R` s'y oppose de toute façon).

### Avant le premier calcul réel — ce qui reste à faire

1. **Dépôt** public `climafor_cartes` (créé), projet poussé, *Settings > Pages > Source :
   GitHub Actions* (à confirmer dans l'onglet Settings : l'API de Pages n'est pas
   interrogeable depuis les workflows).
2. Lancer **« Construire la carte »** avec les valeurs par défaut (`source=sim`,
   `mailles=sim`, 3 fragments, 3 en parallèle). Chaque fragment télécharge les ≈ 68
   fichiers annuels (≈ 140 Mo chacun, ≈ 9,5 Go par fragment, ≈ 26 Go en tout, depuis le
   serveur de Météo-France), puis calcule ses mailles. Prévoir **de l'ordre d'une heure
   par fragment** (non mesuré de bout en bout).
3. L'adresse `https://cammbrnd.github.io/climafor_cartes/` est la valeur par défaut de
   `CONFIG$carte$url_donnees` ; la variable `CLIMAFOR_CARTE_URL` la remplace. Redéployer
   l'application (`deploy.R`) pour que l'onglet Carte lise ce jeu.
4. Lancer **« Vérifier la publication »** (onglet Actions) : il lit les fichiers sur l'adresse
   publique et affiche statut, CORS, compression, cohérence de `meta.json` et de la grille.

**Premier calcul réel (4 novembre 2026)** : 9 888 mailles, 1959–2025, 18 couches, 14 Mo,
publié sur GitHub Pages. Quatre mailles du domaine SAFRAN (frontière suisse/autrichienne,
lon 10,00–10,04) dépassent la borne est de l'application (10°) et sont écartées, avec un
message dans le journal. Chaque fragment a pris ≈ 25 min de calcul (≈ 11 min de lecture des
68 fichiers, ≈ 13 min de calcul) ; la mise en place de R peut, elle, prendre jusqu'à 30 min.

« **Veille** » (workflow mensuel) relance la construction (`source=sim`) quand une année
complète supplémentaire existe — **jamais** le premier calcul.

Les sources `position` (une requête par maille et par année sur GeoSAS, ≈ 660 000
requêtes) et `demo` restent disponibles ; `position` est à éviter. Le workflow
« Diagnostic GeoSAS » (étapes `zone`, `grille`, `sim`) a servi à établir ce qui suit.

### Ce que le diagnostic a établi

- GeoSAS n'expose que `position` et `cube` pour `safran-isba`. Le `cube` interpole sur une
  grille régulière qui dépend de l'emprise demandée : ses valeurs **diffèrent** de celles
  de la maille (rejeté pour la carte). `position` renvoie bien la valeur de la maille.
- Les fichiers SIM de Météo-France, comparés à `position` sur 4 mailles terrestres × 366
  jours, sont **identiques** (écart ≤ 6·10⁻⁶, bruit de codage en float32) pour toutes les
  variables, **sauf l'ETP** : l'ETP de GeoSAS est supérieure de quelques % (jusqu'à
  1,3–1,9 mm/jour) à celle du fichier SIM, sans décalage de jour.
- Conséquence : la couche **P − ETP** (seule à utiliser l'ETP) porte une note visible
  dans la carte (« l'ETP vient du fichier SIM de Météo-France : elle s'écarte de quelques %
  de l'ETP lue par l'analyse d'une maille »). Les 17 autres couches sont identiques à
  celles de l'analyse.
- GeoSAS n'expose pas la hauteur de neige (`HTEURNEIGE`) : l'analyse d'une maille la
  laisse vide, la carte aussi (parité).
- Coordonnées : la projection Lambert II étendu écrite en R est comparée à PROJ (0,3 m) ;
  les centres de GeoSAS tombent sur la grille de 8 km à 1 m près.

### Ce qui a été mesuré, et ce qui ne l'a pas été

- Mesuré (Chromium, rendu WebGL **logiciel**, sans GPU), ≈ 9 100 mailles : changement
  d'année ≈ 47 ms (médiane, p95 71 ms) de bout en bout ; première ouverture ≈ 2,3 s en
  local ; changement d'indicateur 0,1–0,5 s ; animation ≈ 6 années/s. **Pas mesuré** :
  vrai GPU, mobiles, connexion lente.
- Mesuré : calcul des 18 couches ≈ 0,45 s par maille (≈ 75 min de calcul pour ≈ 9 900
  mailles, répartissable en fragments).
- Mesuré sur un exécuteur GitHub : téléchargement d'un fichier annuel ≈ 69 Mo/s ; lecture
  (`data.table::fread`) de 3,6 millions de lignes (9 892 mailles × 366 jours) en ≈ 4 s ;
  calcul complet sur 68 années en 3 fragments sans dépassement de mémoire.
- Mesuré sur la publication : `access-control-allow-origin: *`, `meta.json` servi en gzip,
  18 fichiers de couches en 200, 9 888 identifiants de maille distincts (0 à 9 887).
- **Non vérifié** : si la réactivation automatique du workflow « Veille » suffit à éviter la
  désactivation des tâches planifiées après 60 jours sans activité (sinon GitHub prévient
  le propriétaire du dépôt) ; l'affichage dans un vrai navigateur avec GPU, et sur mobile.

### Limites connues

- **Accessibilité partielle** : étiquettes, annonces (`aria-live`), tableau des 10 mailles
  extrêmes accessible au clavier ; mais la carte elle-même n'est pas parcourable maille
  par maille au clavier, et il n'y a pas d'équivalent tableau complet.
- Avec la source `sim`, le tracé d'une maille est le carré exact de 8 km en Lambert II
  étendu. Avec un CSV de mailles sans `lambx`/`lamby`, il est approché autour du centre.
- Les mailles sont celles de la grille SAFRAN-ISBA-MODCOU (9 892, dont des mailles marines
  ou frontalières) ; celles sans valeur exploitable sont signalées dans le journal.
- P − ETP est calculé sur la fenêtre par défaut de l'application (avril–octobre) ;
  les seuils personnalisés de l'onglet Paramètres ne s'appliquent pas à la carte.
- Le fond de carte IGN est chargé par le navigateur (comme l'onglet Accueil).

### Tests

`tests/test_v18.R` (+ `tests/carte_verif_js.js`) : couches, projection (comparée à PROJ),
format binaire relu **par R et par le code du navigateur exécuté sous Node**, parité avec
l'analyse d'un point, bornes de couleur, assemblage en fragments, refus d'un jeu troué,
validation des entrées (noms de fichiers, adresse, clic renvoyé par le navigateur),
raccord squelette R / `carte.js`, comportement du raccord « clic → analyse ».

---

## Version 1.9 — vagues de chaleur, export CSV, onglet Comparer

Trois ajouts, issus de la question « que manque-t-il à l'application ? » :

### 1. Vagues de chaleur (25ᵉ indicateur, `vag`)

L'ancien compte « vagues » de l'indicateur *Jours de chaleur* (trois jours de suite
au-dessus du seuil absolu `chaleur_t2`) est remplacé par un indicateur à part entière,
qui suit la logique du HWDI (Frich et al. 2002) : une **vague** est une suite d'**au moins
5 jours** dont la température maximale dépasse de **5 °C ou plus la normale du jour**.

- La **normale du jour** est la moyenne des T max de chaque jour du calendrier sur la période
  de référence choisie (10 années au moins), lissée sur 5 jours. Sans référence suffisante,
  l'indicateur le dit au lieu de compter sur une normale bancale.
- Les deux seuils (`vague_ecart`, `vague_duree`) sont réglables dans **Paramètres**.
- Le graphique donne, par année, le **nombre de vagues** ; la lecture ajoute le nombre de
  jours en vague, la durée de la plus longue et l'écart maximal.
- Deux couches de la carte, `vag` (nombre de vagues) et `vagj` (jours en vague).

**Deux choix d'implémentation, à connaître** (le HWDI d'origine ne les fait pas) :
les jours chauds ne sont cherchés que de **mai à septembre** (`CONFIG$methode$vague_mois`),
et l'on **compte les vagues** par année au lieu de ne retenir que la plus longue. Un jour
sans mesure interrompt la suite : on ne devine pas qu'il était chaud.

### 2. Export CSV du bilan

Sous les filtres de l'onglet **Bilan** : la **synthèse** (une ligne par indicateur : valeur,
normale, écart, lecture) et les **séries annuelles** de tous les indicateurs (format long).
Les fichiers s'ouvrent dans Excel (UTF-8 avec marque d'ordre des octets, séparateur « ; »,
virgule décimale) et commencent par des lignes `#` de provenance : lieu, source, référence,
seuils, licence.

### 3. Onglet Comparer

Importez des **points** (CSV) ou des **zones** (GeoJSON, GeoPackage, shapefile zippé) et
obtenez, par lieu, la valeur d'une année, son écart à la normale et sa tendance depuis 1959,
sur les grandeurs de la carte. Aucune série n'est téléchargée : tout vient des **mailles
précalculées** de l'onglet Carte (les mêmes fichiers, lus par le serveur).

- **Point** → maille SAFRAN la plus proche (6 km au plus, sinon « hors de la grille »).
  **Zone** → moyenne des mailles dont le **centre** est dans la zone ; une zone plus petite
  qu'une maille prend la maille la plus proche, et le dit.
- La **normale** est recalculée sur la série du lieu, avec la période de référence choisie sur
  l'accueil (10 années au moins) ; la **tendance** est la pente de Sen avec le test de
  Mann-Kendall corrigé de l'autocorrélation (20 années au moins), comme pour un point.
- Le **bilan par lieu** donne les cinq écarts les plus marqués de l'année, rapportés à
  l'écart-type de la référence, chacun avec sa tendance. Un tableau compare les lieux, un
  graphique superpose leurs séries (en valeur, ou en **écart à la normale de chaque lieu**,
  seule lecture qui compare des climats différents).
- Deux exports CSV (comparaison, séries) avec leurs lignes de provenance.
- Un bouton ajoute le **point choisi sur l'accueil** ; un modèle de CSV est téléchargeable.

**Un fichier importé est une entrée non fiable.** Taille (10 Mo), nombre de lignes (500),
d'entités (100) et de lieux (40) sont bornés ; seuls trois pilotes de lecture sont autorisés
(`GeoJSON`, `GPKG`, `ESRI Shapefile`) ; une archive est refusée si elle sort de son dossier
(`..`, chemin absolu), compte plus de 30 fichiers, se décompresse au-delà de 100 Mo ou ne
porte pas exactement un `.shp` ; un shapefile **sans `.prj`** est refusé plutôt que deviné
(Lambert-93 ou WGS84 : se tromper place les lieux à des centaines de kilomètres) ; les noms
de lieux sont assainis, et ceux qui commenceraient par `=`, `+`, `-` ou `@` sont neutralisés
dans les CSV exportés (injection de formule).

**Nouvelle dépendance : `sf`** (lecture de GeoJSON, GeoPackage, shapefile et rattachement
aux mailles). Sans elle, l'onglet l'explique et l'application reste utilisable ; seul
l'import de zones et le rattachement sont perdus.

### Limites connues

- Les valeurs de Comparer et de la carte sont des **valeurs de maille** (8 km), calculées
  avec les **seuils par défaut** : les réglages de l'onglet Paramètres ne s'y appliquent pas.
  En terrain accidenté, la valeur d'une maille peut s'écarter de celle d'une station.
- Les couches `vag` et `vagj` **n'existent qu'après un nouveau précalcul** : relancer le
  workflow « Construire la carte » (≈ 75 min de calcul). Jusque-là, la carte et Comparer
  n'offrent que les 18 couches déjà publiées.
- L'année comparée par défaut est la dernière du jeu publié, qui peut précéder l'année
  courante de l'application.
- Le rendu dans un vrai navigateur (tableau de comparaison, graphique, mobile) n'a pas
  été vérifié : les tests couvrent les calculs, le module Shiny et le HTML produit.
- **SPI et SPEI** (indices de sécheresse) : calculables avec les données déjà disponibles
  (précipitations et ETP, ou P − ETP, cumulés sur 1 à 12 mois puis normalisés sur la période
  de référence), mais **non implémentés** ; ils feraient l'objet d'une version suivante.

### Tests

`tests/test_v19.R` : décompte des vagues sur des séries dont le résultat se calcule à la main
(bornes, jours manquants, saison, réglages), raccord au registre, aux paramètres, aux
références, au glossaire et à la carte, export CSV. `tests/test_v19_comparer.R` : import de
points (séparateurs, encodages, BOM, inversions, bornes), de zones (GeoJSON, Lambert-93,
GeoPackage, shapefile zippé) et archives piégées écrites à la main, téléchargement des
fichiers de la carte sans réseau (redirection, 404, taille, cache de 24 h), rattachement aux
mailles, bilan à valeurs vérifiables, exports, échappement du HTML, module Shiny de bout en
bout (`testServer`) et raccord à l'application.

---

## Version 2.0 — trois profils et bilan synthétique

Un seul code, **trois interfaces** : ce que voit chaque personne dépend d'un *profil*, pas d'une
autre application. Né d'un retour de terrain : les permanents du DSF, les correspondants-observateurs
et l'équipe en tournée n'ont ni les mêmes besoins ni le même temps.

| Profil | Pour qui | Onglets | Échelles du bilan |
|---|---|---|---|
| `terrain` | en tournée, sur téléphone | Autour de moi · Carte | un point |
| `co` | correspondant-observateur | Carte · Synthèse · Sources | un point, un département |
| `dsf` | permanents du DSF | Accueil · Carte · Comparer · Synthèse · Bilan · Rapport · Paramètres · Sources | départements (plusieurs), régions, fiches importées, un point |

Un profil est une **configuration** (`PROFILS`, `src/utils_profils.R`) : les onglets, les échelles, les
grandeurs offertes (`CONFIG$synthese$couches_co` pour `co` et `terrain`). Le même module sert les trois.

### Comment on arrive dans un profil

Par l'adresse : `?id=xxx`. La table des identifiants est `config/acces.csv`
(`id;profil;nom`), complétée par `config/acces_local.csv` s'il existe : ce second fichier **n'est pas
versionné** (`.gitignore`) et l'emporte pour un même identifiant. `deploy.R` l'envoie avec l'application.
Sans identifiant reconnu, la page d'entrée le demande. Tant que `CONFIG$acces$demo` vaut `TRUE`,
`?profil=co|dsf|terrain` ouvre aussi un profil sans identifiant : utile pour une démonstration, à passer à
`FALSE` pour une exploitation.

> **Ce n'est pas une authentification.** L'identifiant figure dans l'adresse ; quiconque la connaît (ou
> reçoit un lien partagé) obtient le profil. Le mécanisme *oriente* chaque personne vers la bonne
> interface, rien de plus : il ne protège aucune donnée. Les données affichées sont de toute façon les
> mailles publiques (Licence ouverte). Le dépôt étant public, `acces.csv` ne contient que des
> identifiants `demo-…` ; les vrais vont dans `acces_local.csv`. Une vraie authentification viendra avec
> la version Python/TypeScript : seule `profil_resoudre()` change, le reste ne lit que le profil rendu.

Les liens partagés d'avant la v2.0 (`?x=…&y=…`, sans profil) continuent d'ouvrir l'interface complète.
Un identifiant donné mais inconnu n'est jamais rattrapé par un lien ancien ni par `?profil=`.

### Le bilan synthétique

Un bilan court, **quelques phrases par graphique ou tableau**, que l'on compose en cochant les grandeurs
(graphiques d'un côté, lignes du tableau de l'autre ; « Jeu de base », « Tout cocher », « Tout décocher »).

- **Départements et régions** : valeur = moyenne simple (poids égaux) des mailles de 8 km dont le **centre**
  est dans le contour ; une pondération par la surface forestière pourra s'ajouter plus tard.
  Contours dans `donnees_limites/` (voir son `LISEZMOI.md`, script `outils/limites_construire.R`).
- **Point** : maille la plus proche, à 6 km au plus ; au-delà, pas de valeur (rien d'inventé).
- **Fiches importées** : voir ci-dessous. Les fiches sont un *contexte*, pas un échantillon.
- Pour chaque lieu et grandeur : valeur de l'année, normale (référence au choix, 10 ans au moins), écart et
  sa lecture en mots (« proche de la normale » = moins d'un demi écart-type), rang dans la série, tendance
  (Sen et Mann-Kendall, 20 ans au moins). Les phrases sont des **gabarits déterministes**, sans modèle de
  langage : un même calcul donne toujours le même texte.
- **Exports** : bilan à emporter en **HTML autonome** (images intégrées, aucune ressource externe), en
  **Word** (conversion par pandoc ; le bouton n'apparaît que si pandoc est disponible) et tableaux en **CSV**
  avec leurs lignes de provenance (source, échelle, référence, licence).
- **Limites à connaître** : les valeurs sont celles des mailles précalculées, avec les seuils par défaut du
  précalcul (les réglages de l'onglet Paramètres ne s'y appliquent pas) ; une maille n'est pas une station ;
  un territoire plus petit qu'une maille est signalé.

### Fiches et application de terrain (Sylvasan)

ClimaFor **ne saisit aucune fiche** : c'est le rôle de l'application de terrain (en développement). Il lit
des fiches déjà saisies et les replace dans leur contexte climatique : pour chaque fiche, la valeur de **sa
propre année** à la maille la plus proche, l'écart à la normale, et la répartition par essence, problème et
année.

- Format : CSV (colonnes `latitude`, `longitude`, et si possible `fiche`, `date`, `essence`, `probleme`,
  `nom`), GeoJSON, GeoPackage ou shapefile zippé. Dates lues : `2025-07-14`, `14/07/2025`, ou une année
  seule (comptée au 1ᵉʳ juillet). Une fiche sans date lisible est analysée avec l'année du bilan. 500 fiches
  au plus ; textes bornés et assainis ; un modèle de CSV se télécharge dans l'interface.
- **Le raccord à l'API est une prise, pas un branchement** : `fiches_lire_api()` (`src/utils_fiches.R`) rend
  aujourd'hui un message honnête ; quand l'API existera, elle devra produire le même tableau que
  `fiches_vide()`. `CONFIG$sylvasan$url_saisie` ajoute un bouton « Saisir une fiche » dans l'écran terrain
  (vide : pas de bouton) et `CONFIG$sylvasan$api_fiches` un bouton « Charger depuis l'application de terrain ».

### Déployer cette démonstration

`source("deploy.R")` suffit : il vérifie la liste d'accès et les contours, puis les envoie (avec
`acces_local.csv` s'il existe). Ouvrir ensuite `https://<compte>.shinyapps.io/climafor/?profil=co` (ou
`?id=demo-dsf`, `?id=demo-terrain`). Le paquet `sf` est requis (déjà dans `deploy.R`).

### Sous RStudio : la fenêtre ne se ferme plus avec la session

Chaque fois que la connexion se coupe, Shiny envoie « disconnected » à la fenêtre qui contient la page ;
RStudio (volet Viewer, « Run in Window ») répond en **fermant sa fenêtre**, sans un mot. Une erreur du
serveur, qui arrête la session, avait donc le même effet qu'un plantage silencieux. Deux garde-fous :
`www/app.js` n'annonce plus la page qu'à elle-même (la fenêtre reste ouverte, le voile gris « Déconnecté du
serveur » apparaît), et `server()` rattrape toute erreur de démarrage : elle est écrite dans la console R
(`[ClimaFor] le demarrage de l'application en échec : …`) et montrée à l'écran. Une erreur survenue plus tard
dans un `observe()` arrête toujours la session (comportement de Shiny), mais elle est dans la console.
Pour diagnostiquer : `shiny::runApp(launch.browser = TRUE)` ouvre le navigateur habituel, et la console R
affiche l'erreur en rouge.

### Organisation du code

`app.R` n'assemble plus que la page et les modules du profil. Nouveaux fichiers : `src/utils_profils.R`
(profils, acces), `src/ui_page.R` (page et page d'entrée), `src/serveur_dsf.R` (l'analyse d'un point :
stations, SAFRAN, rapport, lien partagé — elle ne tourne que pour le profil `dsf`), `src/utils_territoires.R`,
`src/utils_fiches.R`, `src/utils_synthese.R` (calcul, phrases, tableaux, graphiques),
`src/utils_synthese_rendu.R` (CSV, HTML, Word), `src/mod_synthese.R` et `src/mod_terrain.R`. Le terrain et
la synthèse ne lisent que les mailles précalculées : aucun appel à un service distant au choix d'un point.

### Vérifications

`tests/test_v20.R` (valeurs comptées à la main sur un jeu synthétique de 36 mailles autour de Toulouse :
moyennes de territoire, normale, écart, rang, contexte des fiches, exports, module de bout en bout, interface
de chaque profil, page d'entrée). L'audit axe-core automatisé a été rejoué sur la page d'entrée, sur le bilan de chaque profil
(départements, régions, fiches, point) et sur l'écran terrain, sur grand écran et sur téléphone ;
l'onglet Carte n'a pas été rejoué (fonds de carte inaccessibles dans l'environnement de test). Trois défauts ont été attrapés par ces tests avant livraison : un bilan HTML sans
`<head>` (jeu de caractères et style perdus), un identifiant de fiche en `=…` non neutralisé dans le CSV, un
identifiant illisible (`%00`) qui laissait la page d'entrée muette.

### Passage à Python / TypeScript

Prévu après cette version. Ce qui se porte tel quel : les **profils** (configuration pure, à passer en
JSON), la **liste d'accès** (à remplacer par une vraie authentification), le **moteur de bilan** (fonctions
pures sur les couches précalculées : moyenne des mailles d'un contour, normale, écart, rang, tendance), les
**gabarits de phrases** et les **contours**. `tests/test_v20.R` sert de spécification : chaque valeur y est
vérifiable à la main.

---

## Version 2.1 — mesures et modèle côte à côte, deux lieux, synthèse à envoyer

Trois besoins venus du terrain : (1) **garder les stations Météo-France** à côté du modèle SAFRAN
(la mesure réelle et la valeur modélisée, ensemble) ; (2) **comparer deux lieux de n'importe quelle
sorte** ; (3) **produire sur le terrain une synthèse envoyable par courriel**.

### Stations publiées comme les mailles

Les indicateurs annuels des stations sont **précalculés et publiés à côté des mailles**, dans le même
format et par le même calcul : `stations.json` (la liste : identifiant, nom, département, position,
altitude, dernière mesure, complétude pluie et températures, nombre d'années) et un fichier
`s_<grandeur>.bin.gz` par grandeur (entiers 16 bits compressés, comme les `c_<grandeur>.bin.gz` des
mailles). L'application ne télécharge donc **aucune série journalière** au choix d'une station : elle lit
le jeu publié, une fois, au premier besoin (`cmp_stations()`).

- **Un jeu sans stations n'est pas une erreur** : un jeu publié avant la v2.1 continue de marcher, et
  l'interface dit simplement qu'il n'y a pas encore de stations (404 mémorisé pour la session).
- **Validation à la lecture** (`stations_meta_valider()`) : schéma, noms de fichiers sans chemin,
  identifiants uniques et bien formés, colonnes de même longueur, nombre de stations annoncé, coordonnées,
  années, plafond `CONFIG$stations$publie$stations_max`. Un jeu invalide est refusé en entier.
- **Qui est publiée** : au moins `annees_min` (15) années valides pour la pluie ou les températures ; pas
  d'arrêt de mesure depuis plus de `inactive_max_j` (730) jours.
- **Calcul** : le workflow `construire-carte.yml` a deux entrées, `stations` (oui/non, « oui » par défaut) et
  `stations_fragments` (nombre de jobs, 4 par défaut ; chacun lit environ 25 départements). Les stations se construisent
  **dans le même calcul que les mailles** parce que le déploiement GitHub Pages remplace tout le site :
  un calcul séparé effacerait l'un ou l'autre. Si les stations sont demandées et échouent, **rien n'est
  publié** (un site sans stations remplacerait un site avec). `outils/stations_cli.R` (fragment, assembler),
  `outils/stations_precalcul.R` et, pour les essais, `outils/stations_demo.R` (stations **fictives**, jamais
  publiées sur Pages).
- **Vérifié** par `.github/workflows/verifier-publication.yml` (bloc `stations.json`).

### Les sources d'un point : le modèle et la mesure, ensemble

Pour un point (commune, coordonnées, point de l'accueil), le bilan peut s'appuyer sur la **maille SAFRAN**
(modèle) et sur les **stations voisines** (mesure), côte à côte. Les stations proposées viennent du même
classement que la recherche en direct (`mf_classer_stations()` : distance, écart d'altitude, fraîcheur,
complétude ; `CONFIG$stations$n_propositions` = 3). Par défaut : la maille et la meilleure station si sa
qualité est correcte (niveau 3 au plus). Quand plusieurs sources sont retenues, les lieux s'appellent
« Maille SAFRAN (modèle) » et « <Station> (mesure) » ; avec la seule maille, le nom du point est conservé.
Chaque grandeur est lue dans la source qui la fournit : une station sans températures n'en invente pas.
La normale d'une station est calculée sur **ses** années de mesure (référence d'au moins dix ans).

### Deux lieux (onglet Comparer)

Le mode « Deux lieux » (par défaut) compare **deux lieux de sorte quelconque** : maille SAFRAN, station
Météo-France ou département (`src/mod_lieu_choix.R`), soit toutes les combinaisons : maille↔maille,
station↔station, maille↔station, département↔département, département↔maille, département↔station. Le mode
« Plusieurs lieux importés » reste disponible.

- **Période commune** : pour chaque grandeur, les deux séries sont restreintes aux années où **les deux**
  ont une valeur, puis normales et tendances sont recalculées sur ces années (`duo_restreindre()`).
- **Tableau** : Grandeur · A · B · écart A − B · écart de chacun à sa normale · lecture en mots. Quand un
  lieu ne fournit pas une grandeur, ou qu'aucune année n'est commune, le tableau le dit (pas de case vide).
- **Corrélation** (Pearson) annoncée seulement à 10 années communes au moins.
- **Mesure et modèle** : le texte rappelle qu'une station mesure en un point et que la maille SAFRAN est une
  moyenne de 8 km ; un écart entre les deux n'est pas une erreur de l'une des deux.
- Code : `src/utils_comparer_duo.R` (`duo_*`, `sources_*`), `src/mod_comparer.R`.

### Une synthèse du terrain, à envoyer par courriel

L'écran terrain (« Autour de moi ») produit une synthèse du point : quelques phrases, un petit tableau,
les sources et la prudence d'usage. Quatre façons de l'emporter :

| Bouton | Ce qu'il fait |
|---|---|
| **Envoyer par courriel** | ouvre la messagerie de l'appareil avec objet et texte rédigés (lien `mailto:`) |
| **Partager…** | ouvre le partage du téléphone, avec la page HTML en pièce jointe quand l'appareil le permet |
| **Enregistrer la page** | télécharge la synthèse en HTML autonome |
| **Copier le texte** | place le texte dans le presse-papiers |

**Choix assumés.** L'application **n'envoie aucun courriel elle-même** : il faudrait un serveur de
messagerie, un compte expéditeur et des secrets, et le message partirait d'une adresse de l'application et
non de celle du forestier. Avec `mailto:`, il part de sa messagerie, sous son nom, et il le relit avant
d'envoyer. Un lien `mailto:` **ne porte pas de pièce jointe** (aucun navigateur ne le permet) : la page HTML
s'enregistre ou se partage à part. Le texte est limité à 1 900 octets une fois encodé (certaines
messageries tronquent au-delà) en retirant d'abord les lignes de grandeurs les moins importantes. **Les
coordonnées ne figurent jamais dans le message**, seulement le nom du lieu. `src/utils_envoi.R`,
`src/mod_terrain.R`, `www/app.js` (section 8).

### Sources et accueil

L'onglet Sources décrit les deux natures de donnée (« Mesure et modèle, côte à côte »). L'accueil du
profil `dsf` comporte un tableau qui explique en une ligne chacune des différences entre **Synthèse**
(un lieu, quelques phrases), **Bilan climatique** (le détail chiffré, plusieurs lieux) et **Rapport** (le
document complet à diffuser).

### Limites à connaître

- **Les stations réelles de Météo-France n'ont été essayées que sur des données de démonstration** : le
  calcul des stations sur les vrais fichiers (workflow « Construire la carte » avec `stations = oui`, valeur par
  défaut, et une source autre que `demo`) reste à lancer et à vérifier une première fois sur GitHub Actions.
- Une station n'est publiée qu'avec assez d'années ; une station récente ou arrêtée n'apparaît pas.
- Le courriel dépend de la messagerie de l'appareil ; sans messagerie configurée, utiliser « Copier le
  texte » ou « Enregistrer la page ».
- Une station mesure en un point ; sa valeur n'est pas celle de la maille qui la contient.

### Vérifications

`tests/test_v21.R` : précalcul et lecture des stations (validation, refus des jeux altérés, 404
définitif), bilans mesure + modèle sur un jeu synthétique, toutes les combinaisons de deux lieux, période
commune, synthèse à envoyer (texte, lien `mailto:` borné, HTML autonome, aucune coordonnée), écrans
(terrain, synthèse d'un point, Comparer en mode deux lieux), `outils/stations_cli.R`, `www/app.js`, feuille
de style et workflow.

---

## Version 2.2 — une base précalculée pour le bilan d'un point (mode rapide)

Jusqu'ici, le Bilan d'un point calculait ses 25 indicateurs depuis la série **journalière** de la maille
SAFRAN : une requête par année, soixante-six requêtes pour 1959-2025. Le précalcul de la carte avait déjà
calculé, pour chaque maille et chaque année complète, presque tout ce que le Bilan montre. La v2.2 **publie
ce qui manquait** et laisse le Bilan **lire** ce jeu au lieu de le recalculer. La v2.3 fait de même pour les
**stations Météo-France** (voir plus bas : une station a des trous, ce qui change ce qu'il faut publier).

### Ce qui est publié en plus (champ `base` de `meta.json`)

Le navigateur (onglet Carte) ne lit pas ce champ : **la carte est inchangée**.

| Quoi | Combien | Pour quoi faire |
|---|---|---|
| **Colonnes annuelles** | 24 fichiers `c_b_<indicateur>_<colonne>.bin.gz` | les colonnes des tableaux annuels que la carte ne montre pas (jours de gel sévère, date du dernier gel, pluie liquide et solide, durée de la plus longue vague…) |
| **Normales mensuelles** | 6 fichiers `c_r_mois_*.bin.gz`, 12 valeurs par maille | profils du mois (précipitations, température, neige, ETP, déficit de vapeur, vent) |
| **Normale du jour** | 2 fichiers `c_r_jour_*.bin.gz`, 366 valeurs par maille | courbe de température de l'année, vagues de chaleur de l'année en cours |
| **Seuil de vent soutenu** | 1 fichier `c_r_seuil_ff.bin.gz`, 1 valeur par maille | percentile 95 du vent de la référence |

Même format binaire que les couches de la carte (entiers 16 bits compressés) : une « année » y est une ligne,
qu'elle soit une année, un mois, un jour ou rien. La liste est dans `src/carte_couches_base.R` ; `meta.json` porte
aussi l'**empreinte des réglages** (fenêtre de saison, seuils) avec lesquels la base a été calculée.

### Le Bilan en mode rapide (Paramètres → Mode de chargement)

**Le mode complet reste le mode par défaut.** En mode rapide, pour une maille SAFRAN :

1. l'application trouve la maille du point dans le jeu publié (`cmp_mailles`, comme l'onglet Comparer) ;
2. elle lit les années publiées dans la base (quelques fichiers de 0,5 à 5 Mo, téléchargés une fois par le
   serveur et gardés 24 h, comme pour Comparer) ;
3. elle ne demande à SAFRAN que **les années que le jeu ne porte pas** (l'année en cours) **et l'année comparée** ;
4. le tableau annuel de chaque indicateur est recomposé : lignes lues + lignes calculées, puis `enrichir_annuel()`
   sur l'ensemble. **Même normale, même écart-type, même anomalie** qu'en mode complet (`src/utils_base.R`).

Aucune formule n'est écrite deux fois : les années chargées passent par la fonction de calcul de l'indicateur, les
valeurs qui ne dépendent pas de l'année (normale mensuelle, normale du jour, seuil de vent) sont glissées dans le
contexte (`ctx$base`) que ces fonctions consultent avant de recalculer.

**Quand la base ne sert pas** (et l'écran le dit, avec la raison) : autre période de référence, autre fenêtre de
saison, un seuil modifié, jeu publié sans base ou avec une base incomplète, point hors grille, station absente du
jeu publié. Jamais de nombre calculé avec d'autres réglages que ceux affichés.

**Ce que le mode rapide ne fait pas** : la **synthèse chiffrée « à période égale »** compare les jours de l'année
en cours aux mêmes jours de chaque année de référence, et le **Rapport** la reprend : ils demandent la série
journalière de la référence. Un bouton « Charger la série complète » (Bilan et Rapport) lance le chargement
complet.

### Les stations en mode rapide (v2.3)

Une maille n'a pas de trou : une année y est toujours complète, et une normale mensuelle ne dépend que de la
variable. Une station, si. Le bilan d'une station **écarte d'abord** les années où moins de 95 % des jours sont
renseignés (variable par variable : pluie, température, vent), puis calcule sur ce qui reste ; ses normales
dépendent donc de l'indicateur qui les demande. Pour que le mode rapide rende **exactement** ce que rend le chargement
complet, la base d'une station publie, en plus des colonnes annuelles des mailles (`src/carte_couches_base_stations.R`) :

| Quoi | Pour quoi faire |
|---|---|
| **Validité de chaque année** et **part des jours renseignés**, par groupe (pluie, température, vent) | écarter les mêmes années ; la note « années écartées » des graphiques ; la qualité de la série. Le drapeau « année valide » est publié à part : comparer la part *arrondie* au seuil ferait basculer une année à 94,96 %. |
| **Jours manquants** par année, pour chaque couple (variables, fenêtre) que les indicateurs utilisent | la colonne `n_manquants` et la note « 2012 (10 j) » |
| **Références par indicateur** (10 fichiers `s_rs_<indicateur>_<nom>.bin.gz`) | normale mensuelle de la pluie *pour le bilan hydrique* (années valides pour la pluie **et** la température) distincte de celle du profil des pluies ; normale du jour, seuil de vent |

Le jeu de stations (`stations.json` et `s_<clé>.bin.gz`, à côté de `meta.json`) porte le même champ `base` et la
même **empreinte des réglages** que celui des mailles. Une entrée qu'**aucune station** ne renseigne (SWI, neige au
sol, humidité : une station ne les mesure pas) n'est pas écrite ; `base_parametres$vides` la nomme et sa lecture
rend des `NA`.

En mode rapide, une station :

1. est retrouvée par son identifiant dans le jeu publié (`base_ouvrir_station`) ;
2. n'est interrogée que pour **l'année en cours** et l'année comparée (le fichier « latest » de Météo-France) ;
3. voit ses attributs de qualité **complétés** par ceux des années publiées (`base_station_attributs`) : c'est ce qui
   fait écarter les mêmes années qu'en mode complet ;
4. est déclarée « variable non mesurée » **selon la base**, pas selon l'année chargée (`base_station_colonnes_absentes`) :
   un capteur de vent en panne cette année a mesuré le vent les années précédentes.

Si la station n'a aucune mesure récente (arrêtée depuis la publication), l'application charge la série complète
plutôt que d'échouer, et le dit.

### Précision

La base est stockée en entiers 16 bits : une valeur lue diffère du calcul complet de **moins d'une unité de la
dernière décimale conservée** (comptages de jours : exacts). La classe d'aridité est publiée en code pour ne jamais
basculer d'une classe à l'autre à cause d'un arrondi.

### Vérifications

`tests/test_v22.R` : base bien formée et relue à l'identique ; **parité indicateur par indicateur** (les 25) entre
le calcul complet et le calcul rapide, sur trois mailles, avec une année en cours partielle et avec une année
comparée antérieure ; garde-fous (référence, fenêtre, seuils, base incomplète) ; refus d'une `meta.json` piégée.

`tests/test_v22_stations.R` : la même parité sur des **stations aux trous réels** (une année absente, un mois
lacunaire, dix jours manquants dans une année valide, un pluviomètre, un capteur de vent arrêté), y compris les
années écartées, les jours manquants et la qualité de la série ; un `stations.json` piégé est refusé.

La suite entière (`Rscript tests/lancer_tests.R`, une dizaine de minutes) tourne aussi **à chaque envoi sur GitHub**
(workflow « Tester le code ») : un test qui échoue se voit sur la demande de fusion.

### À lancer

Le workflow « Construire la carte » calcule la base avec les mailles **et avec les stations** (entrée `base`, `oui`
par défaut). Après la publication, « Vérifier la publication » valide le jeu avec la base. Mesuré sur la première
publication réelle (9 892 mailles, 1959-2025) : la base des mailles ajoute **22,4 Mo** au site.

### Limites à connaître

- **Stations** : seules les stations du jeu publié ont un mode rapide ; une station trouvée dans la liste de
  Météo-France mais absente du jeu (trop courte, arrêtée depuis plus de deux ans) se charge en mode complet.
- La base publiée avec la source `sim` (fichiers SIM de Météo-France) a une ETP qui s'écarte de quelques % de
  l'ETP lue par l'analyse d'un point : en mode rapide, la jonction entre les années publiées et l'année chargée peut
  faire apparaître cet écart sur P − ETP.
- La maille lue dans la base est la plus proche du point ; celle que renvoie GeoSAS peut différer pour un point
  situé à la limite de deux mailles. L'écran donne la distance à la maille.
- Premier usage d'un point : le serveur télécharge jusqu'à une quarantaine de fichiers (quelques dizaines de Mo)
  puis les garde 24 h pour tous les utilisateurs de ce processus.

---

## Architecture

```
climafor/
├── app.R                       Assemblage UI + serveur, aucun métier
├── global.R                    Chargement des dépendances et du code source
├── deploy.R                    Contrôles + déploiement shinyapps.io
├── diagnostic.R                Contrôle de l'accès à SAFRAN
├── diagnostic_stations.R       Premier essai réel des stations Météo-France
├── tests/                      Suite de tests (Rscript tests/lancer_tests.R)
│                               + simulation_tendance.R (faux positifs, puissance)
│                               + simulation_vpd.R (VPD : ancienne formule et FAO-56)
│                               test_v12.R … test_v17.R, test_v171.R (correctifs et revue)
│                               test_v18.R + carte_verif_js.js (onglet Carte)
│                               test_v19.R, test_v19_comparer.R (vagues, export, Comparer)
│                               test_v21.R, test_v22.R, test_v22_stations.R (stations, deux lieux ;
│                               base précalculée, mode rapide des mailles et des stations)
├── DESCRIPTION                 Paquets et versions requis (v2.3.0)
├── NEWS.md                     Historique des versions (audits 1.2 → 1.7.3 en détail)
├── docs/                       maquette_interface.html (maquette d'origine, plus mise à jour)
├── .github/workflows/          Tester le code (à chaque envoi), Construire la carte, Vérifier la
│                               publication, Veille, Diagnostic GeoSAS, Aperçu de la carte
├── outils/                     carte_*.R : précalcul de la carte (format, géométrie, calcul,
│                               sources, SIM, démo, ligne de commande, veille, diagnostic)
│                               construire_modele_docx.py (modèle Word)
│                               couverture.R (couverture de lignes, covr)
│                               figer_versions.R (écrit renv.lock)
├── src/                        (surtout pas « R/ » : voir note ci-dessous)
│   ├── config.R                ★ Source unique de vérité
│   ├── utils_ui.R              Icônes SVG en ligne, replis, blocs d'app, journal d'erreurs
│   ├── utils_cache.R           Cache disque : format JSON contrôlé, noms sûrs, purge (v1.7)
│   ├── utils_limites.R         Limites d'usage par fenêtre glissante (v1.7)
│   ├── utils_api.R             Accès aux données (SAFRAN, API Géo, cache)
│   ├── utils_stations.R        Stations Météo-France : fichiers, qualité, série
│   ├── utils_partage.R         Lien partageable : encodage/décodage de l'état (v1.4)
│   ├── carte_couches.R         Indicateurs cartographiables (v1.8)
│   ├── utils_carte.R           Format des fichiers de la carte, validation, transport (v1.8)
│   ├── carte_couches_base.R    Base précalculée : colonnes et références que la carte ne montre pas (v2.2)
│   ├── carte_couches_base_stations.R  Base précalculée d'une station : qualité, jours manquants, références par indicateur (v2.3)
│   ├── utils_base.R            Lecture de la base, garde-fous, recomposition du Bilan en mode rapide (v2.2 ; stations v2.3)
│   ├── mod_carte.R             Onglet Carte : squelette, passerelle, ouverture d'une maille (v1.8)
│   ├── utils_comparer.R        Comparer : import de lieux, mailles, séries, bilan, exports (v1.9)
│   ├── mod_comparer.R          Onglet Comparer : interface et serveur (v1.9)
│   ├── glossaire.R             Définitions et infobulles des termes techniques (v1.4)
│   ├── references.R            Références, statut des seuils, corrections de méthode (v1.5)
│   ├── utils_calc.R            Calcul des indicateurs
│   ├── utils_plot.R            Thème graphique + fabriques de graphiques
│   ├── registre_indicateurs.R  ★ Cœur générique
│   ├── mod_localisation.R      Où ? (recherche + position, carte et coordonnées repliées)
│   ├── mod_station.R           Source des données : station proposée et sa qualité
│   ├── mod_periode.R           Quand ?
│   ├── mod_graphique.R         Module générique, une instance par indicateur
│   ├── mod_bilan.R             Tableau de bord
│   ├── utils_rapport.R         Rapports : neutralisation, annexe, CSV, arrière-plan
│   ├── mod_rapport.R           Génération documentaire
│   ├── mod_parametres.R        Seuils ajustables
│   ├── mod_sources.R           Documentation
│   └── _gabarit_extension.R    Modèle pour les évolutions (non chargé)
├── www/
│   ├── climafor.css            Habillage (vert, sauge, Open Sans), responsive
│   ├── fonts/                  Open Sans (woff2, licence OFL)
│   ├── app.js                  Géolocalisation, resize, largeur d'écran, sommaire, copie du lien
│   ├── carte/                  carte.js (composant) + lib/ MapLibre GL JS 5.6.0 (BSD-3, embarqué)
│   └── favicon.png             Icône de l'onglet (pictogramme du DSF)
├── rapport/
│   ├── rapport_climat.Rmd      Modèle de rapport
│   ├── generer_rapport.R       Processus séparé de génération
│   ├── modele_dsfr.docx        Modèle Word (fabriqué par outils/)
│   └── styles_rapport.css
└── cache/                      Cache disque, borné par âge et par taille (éphémère sur shinyapps.io)
```

### Le principe de conception

Deux fichiers portent la généricité de l'ensemble.

**`config.R`** concentre toute constante métier : URL d'API, noms des variables
SAFRAN, seuils bioclimatiques, périodes de référence, palette. Aucun seuil et
aucune couleur n'apparaît en dur ailleurs. Changer le seuil de canicule ou la
couleur primaire se fait à un seul endroit.

**`registre_indicateurs.R`** décrit chaque indicateur *comme une donnée* : son
titre, sa famille, sa fonction de calcul, sa fonction de tracé, son texte de
méthode, son texte de lecture, son commentaire automatique. Les modules Shiny
ne connaissent aucun indicateur en particulier — ils parcourent le registre.

Conséquence directe : l'onglet Bilan, le filtre thématique, les exports CSV et
PNG, l'aperçu du rapport, le corps du rapport et les fiches méthodologiques de
l'onglet Sources se remplissent **tous** à partir du registre.

---

## Ajouter un indicateur

Trois étapes, aucune modification des modules.

**1.** Une fonction de calcul dans `src/utils_calc.R`, respectant le contrat
`calc_xxx(don, ctx) -> data.frame` :

```r
calc_mon_indicateur <- function(don, ctx) {
  res <- agreger_annuel(don, "t_max", "moyenne", nom = "valeur")
  enrichir_annuel(res, don, ctx, "valeur")   # ajoute normale, anomalie, complétude
}
```

**2.** Une fonction de tracé dans `src/utils_plot.R`. `graphe_annuel()` apporte
gratuitement la coloration des anomalies, la ligne de normale, la moyenne
mobile, la tendance et les graduations adaptées à la largeur d'écran :

```r
plot_mon_indicateur <- function(dat, ctx, mobile = FALSE) {
  graphe_annuel(dat, "valeur", "Mon libellé (unité)", ctx,
                type = "barres", sens = "chaud", mobile = mobile)
}
```

**3.** Une entrée dans `INDICATEURS`, puis son identifiant dans
`ORDRE_INDICATEURS`. C'est tout.

`src/_gabarit_extension.R` contient un exemple complet et fonctionnel : le calcul
des générations potentielles du typographe par cumul de degrés-jours.

---

## Feuille de route

Les trois extensions annoncées ont été anticipées dans la structure.

### Module ravageurs

Générations potentielles de *Ips typographus* (typographe), *Pityogenes
chalcographus* (chalcographe), *Ips sexdentatus* (sténographe), *Monochamus
galloprovincialis*, et cycle de *Thaumetopoea pityocampa* (processionnaire du
pin).

Tous reposent sur le même mécanisme, déjà en place : un cumul de degrés-jours
au-dessus d'un seuil spécifique à l'espèce, divisé par son exigence thermique.
`calc_gdd()` fournit la brique de base. Chaque espèce devient une entrée du
registre dans une famille `ravageurs`, avec ses paramètres déclarés dans
`CONFIG`. **Les paramètres phénologiques devront être validés avec le
Département de la santé des forêts** : ils varient sensiblement selon les
auteurs et selon les régions.

### Module aléas

Neige lourde, grêle, zones à fort risque de sécheresse.

`calc_neige()` calcule déjà le cumul maximal sur trois jours glissants, qui est
le proxy usuel du risque de bris de cimes ; le seuil correspondant (de l'ordre
de 30 mm d'équivalent-eau en 72 h) reste à ajouter à `CONFIG$seuils`. Le volet grêle nécessitera une source externe :
les données radar de Météo-France sont librement accessibles depuis 2024.

### Module territoire

Bilans départementaux et régionaux : **faits en v2.0** (bilan synthétique, moyenne des mailles du contour).
Reste la cartographie en aplats de couleur (carte choroplèthe), non faite.

C'est l'évolution la plus structurante : elle change la nature de l'accès aux
données. `src/_gabarit_extension.R` détaille trois stratégies (grille
d'échantillonnage, requête EDR sur emprise, précalcul livré avec
l'application) avec leurs implications de dimensionnement. **La troisième est
la seule tenable sur l'instance gratuite** pour un rendu cartographique
régional.

---

## Interface

**Version 4 (interface allégée, station Météo-France).** Le bandeau
« actualités » (trois blocs avant la localisation : pluie 30 jours, prévision,
restrictions d'eau) est supprimé, avec son module. La localisation n'est plus
un sélecteur à trois onglets : recherche et « Utiliser ma position » sont
visibles d'emblée, la carte et les coordonnées sont repliées. Sous 900 px
(téléphone, tablette en portrait) : en-tête compacte, graphiques en images
(plotly.js n'est pas envoyé), une seule thématique affichée à la fois dans une
rangée de pastilles, sous-titres de graphiques repliés. Sur écran tactile, les
graphiques interactifs perdent zoom et déplacement au doigt, qui volaient le
défilement. Le bilan et le rapport nomment le point et la source *réellement
analysés*, même si l'on change de point sur l'accueil sans relancer.

### Organisation générale

Cinq onglets, dans l'ordre du parcours : **Accueil**, **Bilan climatique**,
**Rapport**, **Paramètres**, **Sources**.

L'accueil porte le choix du lieu et de la période. « Lancer l'analyse » bascule
automatiquement sur l'onglet Bilan — le changement d'onglet *est* le retour
visible de l'action. En cas d'échec, on reste sur l'accueil, là où se trouvent
les messages d'erreur et les réglages à corriger. La barre de contexte en tête
du bilan porte le chemin du retour.

Les deux onglets conservent leur état : revenir modifier un réglage ne détruit
ni la carte ni les résultats déjà calculés.

> **Ne pas remplacer `bootstrapPage()` par un `tagList()` dans `app.R`.**
> Les fonctions de page de Shiny sont ce qui déclare la dépendance Bootstrap.
> Sans elle, `tabsetPanel()` émet bien son balisage mais il ne reste qu'une
> liste à puces : ni mise en forme, ni greffon JavaScript pour basculer d'un
> panneau à l'autre.

### Progressivité

L'accueil présente une procédure numérotée — **1. Où ? 2. Quand ?** — sur deux
colonnes au-delà de 1000 px, et ancre « Lancer l'analyse » dans une barre
collante en bas de page, toujours visible. Les réglages secondaires (plage
affichée, fenêtre du P − ETP) sont repliés : leurs valeurs par défaut
conviennent presque toujours, et les afficher au même niveau que la période de
référence laissait croire qu'il fallait statuer sur tout avant de pouvoir
lancer quoi que ce soit.

Le même principe s'applique à l'onglet Paramètres : les champs d'abord, les
quinze justifications de seuils dans un dépliant « Pourquoi ces valeurs ».

Point technique : la carte Leaflet et les curseurs `ion.rangeSlider` mesurent
leur conteneur **à la construction**. Construits dans un panneau inactif ou un
`<details>` replié, ils resteraient larges de quelques pixels. `www/app.js`
émet donc un évènement `resize` à chaque retour à la visibilité — changement
d'onglet, dépliage d'un `<details>`. C'est le correctif standard, et il évite
tout appel aux API internes de ces bibliothèques.

### Vent : ce que les données permettent et ne permettent pas

SAFRAN fournit un vent **moyen journalier** à 10 m (`FF_Q`), jamais de rafale.
Un chablis se déclenche sur une rafale de quelques secondes : la tempête du
26 décembre 1999 apparaît à peine sur une telle série. Aucun des quatre
indicateurs de vent n'autorise donc une lecture en termes de risque de chablis.
Pour cela il faut ERA5 et sa variable `fg10`.

Ce qu'ils permettent :

| Indicateur | Ce qu'il mesure |
|---|---|
| `vent` | régime moyen, seuil absolu réglable |
| `vent_mois` | saisonnalité, avec sélecteur d'année |
| `vent_soutenu` | journées au-delà du 95ᵉ percentile **du point** |
| `vent_sol` | conjonction vent soutenu + sol saturé |

`vent_soutenu` répond à une question qui a un sens partout : cette année a-t-elle
été plus ventée que d'habitude *ici* ? Un seuil absolu ne le permet pas — 8 m/s
de moyenne journalière est exceptionnel dans le Bourbonnais et ordinaire sur une
crête cévenole. La comparaison est **stricte** : le vent moyen journalier étant
fortement discrétisé, un test large compterait toutes les journées égales au
percentile, soit la quasi-totalité de l'année sur un point peu venté.

`vent_sol` croise vent et `SWI_Q`. Un arbre ne tombe pas seulement parce qu'il
vente, mais parce qu'il vente alors que son ancrage est affaibli : sur sol
saturé, la cohésion du substrat s'effondre. C'est pourquoi les grandes tempêtes
de chablis surviennent en hiver après des semaines de pluie, et non l'été où le
vent peut être aussi fort. L'indicateur mesure une **exposition**, pas un risque.

### Graphiques interactifs

Les fonctions `plot_*` produisent toujours des **ggplot** : c'est la source de
vérité. plotly n'est qu'une couche de rendu appliquée à l'écran par
`graphe_interactif()`. Trois raisons de ne pas les réécrire en plotly :
l'export PNG et le rapport Word ont besoin d'une image, deux versions
divergeraient au premier correctif, et un ggplot se teste hors interface.

`CONFIG$graphiques$interactif <- FALSE` ramène toute l'application à des
images, sans autre modification.

**Deux tracés restent en image**, marqués `interactif = FALSE` dans le
registre : le diagramme ombrothermique et les températures estivales. Tous deux
utilisent un axe secondaire, que `ggplotly()` **perd sans lever d'erreur** — le
graphique s'afficherait, avec une courbe de température dont l'échelle a
disparu. Un graphique faux et plausible est pire qu'un graphique absent. Deux
tests verrouillent ce choix.

Les sous-titres sont eux aussi perdus à la conversion. Ils portent des
informations de lecture indispensables (seuils retenus, sens des couleurs) : le
module les récupère sur l'objet ggplot et les réaffiche sous le graphique.

Coût de plotly, pour que la décision reste révisable : environ 60 à 80 Mo de
mémoire par processus R, 3,5 Mo de JavaScript à la première page, et autant
ajouté à chaque rapport HTML interactif.

### Rapports

**HTML** — graphiques interactifs, activables par une case à cocher (cochée par
défaut). La décocher produit un document léger, transmissible par courriel.
Les widgets sont émis depuis une boucle `results='asis'`, ce qui empêche knitr
de collecter leurs dépendances JavaScript : `emettre_widget()` les réinjecte
explicitement, et un widget d'amorce garantit le chargement de plotly.js.

**Word** — mise en forme par `rapport/modele_dsfr.docx`, fabriqué par
`outils/construire_modele_docx.py` :

```bash
python3 outils/construire_modele_docx.py
```

Un `.docx` versionné tel quel ne se relit ni ne se compare ; le script, si.
Pandoc en reprend les styles, l'en-tête et le pied de page — les logos se
répètent donc sur chaque page. Si le fichier manque, le rapport sort avec la
mise en forme par défaut plutôt que d'échouer.

> **Marianne doit être installée sur le poste qui ouvre le document.** Word
> n'embarque pas les polices : sans elle, il substitue silencieusement une
> police approchante. Rien ne casse, mais le document n'est plus à la charte.

### Tests

```bash
Rscript tests/lancer_tests.R
```

Aucune dépendance : le harnais (`tests/harnais.R`) tient en quarante lignes de
base R, et le script sort avec un code non nul en cas d'échec — branchable tel
quel sur une intégration continue.

Les tests portent sur des **séries synthétiques**, jamais sur des données
SAFRAN réelles. Une série réelle ne fournit aucune vérité de référence : on ne
saurait dire si le résultat est juste, seulement s'il est plausible — ce qui
laisse passer exactement les erreurs de méthode que l'on cherche à attraper.
Les jeux d'essai sont construits pour que le résultat se calcule à la main : il
pleut 2 mm par jour, donc janvier fait 62 mm et l'année 730.

Trois familles de contrôles :

- **méthode** — normale mensuelle calculée dans le bon ordre (moyenne des
  cumuls mensuels, pas cumul global divisé), circularité du lissage journalier,
  convexité de la pression de saturation dans le VPD ;
- **intégrité du registre** — identifiants cohérents, familles déclarées,
  `ORDRE_INDICATEURS` exhaustif, champs obligatoires renseignés ;
- **exécution de bout en bout** — les 24 indicateurs tournent sur données
  complètes *et* sur données vides, et l'indicateur fautif est nommé.

Ajouter une entrée au registre la soumet automatiquement aux deux derniers.

### Sélecteur d'année par graphique

Six graphiques portent un menu déroulant d'année dans l'en-tête de leur carte :
bilan hydrique mensuel, diagramme ombrothermique, précipitations mensuelles,
températures mensuelles, températures journalières, neige mensuelle. Ils sont
déclarés par `selecteur_annee = TRUE` dans le registre.

Choisir une année **ne modifie pas le réglage global**. La carte construit un
*contexte local* — copie du contexte global dont seul `courante` est remplacé —
et recalcule son indicateur avec. Les deux besoins sont distincts : le réglage
de l'accueil dit « quelle année ce bilan documente-t-il », et doit rester le
même partout jusque dans le rapport ; le sélecteur d'une carte dit « et si je
regardais 2003 sur celle-ci ? », et ne doit contaminer ni les autres
graphiques, ni la synthèse, ni le document produit.

Coût : un recalcul par carte et par changement d'année, sur une agrégation de
quelques dizaines de milliers de lignes. Les douze autres cartes continuent de
consommer le calcul unique de `mod_bilan`. Les exports PNG et CSV suivent
l'année affichée, et le nom de fichier la mentionne.

### Résultats

Les quatorze graphiques sont **regroupés par thématique**, sous un titre de
section. Les familles existaient déjà dans le registre — elles ne servaient
qu'à filtrer, elles structurent désormais la page. Le filtre est présenté en
pastilles, avec deux raccourcis « tout / aucun ».

Une **barre de contexte** rappelle en permanence le point, la référence et
l'année comparée. Elle remplace la duplication de la v1 (récapitulatif de
l'application *et* bandeau du module bilan, mot pour mot identiques) ; le
bandeau ne ressort qu'à l'impression, où la barre disparaît.

### Accessibilité

- lien d'évitement en premier élément focusable ;
- icônes SVG décoratives (`aria-hidden`), jamais seules porteuses de sens ;
- cibles tactiles de 44 px sous `pointer: coarse` — la règle porte sur le type
  de pointeur, pas sur la largeur : un écran tactile de 1200 px a les mêmes
  doigts qu'un téléphone ;
- cinq tailles de texte, plancher à 14 px, contrastes AA, focus non supprimé ;
- `prefers-reduced-motion` respecté.

### Web, tablette, mobile

- `www/app.js` remonte la largeur de fenêtre au serveur ; sous 900 px, tous les
  graphiques basculent en version compacte — moins de graduations, typographie
  réduite. Le paramètre `mobile` traverse déjà toute la chaîne de tracé ;
- feuille d'impression dédiée.

Pour une application mobile native, la voie la plus économe serait
d'encapsuler cette application dans une PWA plutôt que de la réécrire : la
structure responsive est déjà là.

---

## Charte graphique

`www/climafor.css` porte tout l'habillage de l'interface, à partir de variables
`--cf-*` (section 1 du fichier) : changer une couleur ou le rayon des angles se
fait à un seul endroit.

- **Fond de page** vert très pâle (`#EEF3EA`), **blocs blancs** à filets fins,
  angles à 4 px.
- **Sauge** (`#D5E4D0`) pour le bandeau d'onglets et les titres d'étape ;
  **onglet actif** en vert foncé (`#00664B`), texte blanc, sans liseré.
- **Vert foncé** pour ce qui se clique (liens, boutons) ; focus clavier ocre
  (`#A8561A`), jamais supprimé ; contrastes AA vérifiés.
- **Police Open Sans**, servie par l'application (`www/fonts/`, licence OFL,
  sous-ensemble latin en trois graisses) : aucun appel à un service tiers.
- **Icône d'onglet** : le pictogramme du DSF, `www/favicon.png` (64 px,
  transparent).
- Les couleurs des graphiques sont dans `CONFIG$couleurs` (`src/config.R`) ;
  `theme_climafor()` (`src/utils_plot.R`) les lit.

Le rapport (HTML et Word, dossier `rapport/`) garde pour l'instant sa propre
mise en forme.

---

## Limites à rappeler aux utilisateurs

La maille de 8 km lisse le relief : ce bilan décrit un climat local, pas une
station forestière. Le sol simulé par ISBA est un sol standard — la réserve
utile réelle module fortement le stress hydrique effectif, et un sondage à la
tarière reste indispensable. L'ETP Penman-Monteith est calée sur un couvert
herbacé : les valeurs de P − ETP se lisent en relatif, entre années et entre
stations, plus qu'en absolu. Enfin une normale décrit un climat passé, pas une
trajectoire : pour anticiper, ce diagnostic se complète des projections DRIAS
et de Climadiag Forêt.

Avec une station : elle mesure en un point, à plusieurs kilomètres et parfois
plusieurs centaines de mètres d'altitude d'écart de la parcelle ; la qualité
affichée en tient compte mais ne corrige rien. Les données sont quasi
quotidiennes (mise à jour environ une fois par jour), pas en temps réel.

Ces limites figurent dans l'onglet Sources et dans chaque rapport généré.

---

## Licence et attribution

Données SAFRAN : Météo-France, licence ouverte Etalab 2.0. Toute réutilisation
doit mentionner la source et la date d'extraction, qui figure sur chaque
rapport.
