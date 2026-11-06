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

## Version 1.2 — corrections issues de l'audit (données, graphiques, rapports)

Cette version traite les constats D1 à D7 (données analysées), G1 à G10
(graphiques) et R1 à R6 (rapports) de l'audit de la version 1.1. Les parties
ergonomie, simplicité, sécurité et solidité du code n'étaient pas dans le
périmètre.

### Données analysées

- **Année en cours (D1).** Une année est dite *partielle* lorsqu'il lui manque
  plus de deux jours (ou, pour un indicateur saisonnier, plus de deux jours de
  sa saison). Elle est affichée en gris, nommée « partielle » dans la légende,
  exclue de la normale, de la moyenne mobile et de la tendance. Auparavant, une
  année arrêtée fin octobre était comparée à des années entières.
- **Jours manquants (D2).** Comptés, et signalés sous le graphique à partir de
  `CONFIG$stations$signaler_manquants_j` (3 jours). **Non compensés, à dessein :**
  extrapoler un cumul fausserait la valeur plus sûrement que ne le fait
  l'oubli, et la compensation changerait silencieusement les chiffres déjà
  publiés. Le cumul reste donc un minimum.
- **Température moyenne des stations (D3).** Une seule définition,
  (TN + TX) / 2 (`CONFIG$stations$definition_tmoy = "tntx"`). `"tm"` rétablit la
  valeur mesurée Météo-France ; la définition retenue figure dans les rapports et
  les CSV.
- **Ruptures d'homogénéité (D4).** Test de Pettitt sur la température annuelle
  des stations (seuil p < 0,01), affiché comme **avertissement indicatif**. Aucune
  homogénéisation n'est faite : elle demande des séries voisines et de
  l'historique de métadonnées que l'application n'a pas.
- **Altitude (D5).** L'altitude du *point* (IGN) est affichée, avec un
  avertissement quant à l'écart avec la maille SAFRAN de 8 km. L'altitude de la
  maille elle-même n'est pas fournie par l'API : elle n'est pas affichée.
- **Cache (D6).** Le schéma des colonnes dérivées (`CONFIG$cache$schema`) est
  inclus dans le nom du fichier de cache : changer le calcul d'une colonne
  invalide l'ancien cache sans intervention. Les anciens fichiers (sans `_s2`)
  ne sont plus lus (et, depuis la v1.7, sont purgés) ; on peut aussi vider `cache/`.
- **ETP (D7).** La source de l'ETP (Penman-Monteith SAFRAN ou calcul à partir des
  observations de station) est indiquée sur les graphiques concernés, dans le
  rapport et dans les CSV : les deux ne sont pas comparables en valeur absolue.

### Graphiques

- **Tendance (G4).** Pente de Theil-Sen, test de Mann-Kendall **corrigé de
  l'autocorrélation d'ordre 1** (inflation de variance), intervalle de confiance
  à 95 % de Sen, au moins 20 années complètes. Le R² n'est plus affiché : il
  n'a pas de sens pour une pente robuste. Sur 2 000 simulations par cas
  (`Rscript tests/simulation_tendance.R`), le taux de faux positifs à 5 % est :

  | n | autocorrélation φ | régression MCO | v1.2 |
  |---|---|---|---|
  | 30 | 0,0 | 5,1 % | 4,2 % |
  | 60 | 0,0 | 5,4 % | 4,0 % |
  | 30 | 0,3 | 13,1 % | 5,9 % |
  | 60 | 0,5 | 26,3 % | 6,3 % |
  | 60 | 0,7 | 41,8 % | 6,6 % |

  Le prix de cette prudence est une puissance moindre : pour une tendance vraie
  de 0,3 écart-type de bruit par décennie avec φ = 0,3, la détection tombe de
  90,6 % à 74,8 % sur 60 ans, et de 33,6 % à 15,6 % sur 30 ans. Une tendance
  « non significative » sur une série courte n'est donc pas une absence de
  tendance.
- **Couleurs (G7).** Un écart n'est coloré (au-dessus / en dessous) que s'il
  dépasse `CONFIG$tendance$bande_neutre` (0,5) fois l'écart-type des années de
  référence ; sinon il est neutre. Un écart qui s'affiche « 0 » est toujours
  « proche de la normale ».
- **Libellés (G1, G2, G3, G6).** Légendes en mots (plus de codes hexadécimaux),
  virgule décimale partout, année exclue de la normale signalée.
- **Accessibilité (G5).** Texte alternatif décrivant chaque graphique, y compris
  pour les graphiques interactifs.
- **Export CSV (G8).** En-tête de lignes `# ` : source, point, altitude, période
  de référence, définition de la température moyenne, seuils, versions du
  logiciel, date d'extraction, licence. En R :
  `read.csv2(f, comment.char = "#", fileEncoding = "UTF-8-BOM")`.
- **Erreurs (G9).** Un échec de calcul et un échec de tracé sont distingués, avec
  un message en français et le détail technique replié.
- **Code mort (G10).** Paramètre `lissage` inutilisé retiré des appels.

### Rapports

- **Texte libre (R1).** Le contexte saisi par l'utilisateur est neutralisé
  (`echapper_md()` dans `src/utils_rapport.R`) : balises HTML, liens
  `javascript:`, gras, parenthèses interprétées comme formules apparaissent comme
  du texte littéral, en HTML comme en Word. Limité à
  `CONFIG$rapport$contexte_max` caractères, avec mention de la troncature.
- **Génération (R2).** Dans un processus R séparé
  (`rapport/generer_rapport.R`), l'application interroge l'état une fois par
  seconde. Délai maximal `CONFIG$rapport$delai_max_s` (300 s) puis arrêt du
  processus. Si le lancement échoue, repli sur la génération synchrone.
- **Traçabilité (R3).** Annexe « Paramètres de l'analyse » : source, point,
  altitude, périodes, seuils (avec ceux qui diffèrent des valeurs par défaut),
  définition de la température moyenne, source de l'ETP, versions de R et des
  paquets, date.
- **Héritage (R4).** Le rapport reprend les mêmes calculs que l'écran : année
  partielle, tendance corrigée, couleurs à seuil.
- **Poids (R5).** Images à 110 dpi sans doublement « retina », sans coloration
  syntaxique ni MathJax : rapport HTML statique de 3,7 à 1,3 Mo, rapport
  décideur 0,5 Mo, aucune ressource distante. Word technique : de 163 à 43 pages
  (styles `Table` et `Compact` manquants dans le modèle, désormais fabriqués par
  `outils/construire_modele_docx.py`).
- **Niveau (R6).** `CONFIG$rapport$niveau_defaut` ; un niveau inconnu fait
  échouer la génération (`valider_niveau()`) au lieu de produire un autre
  rapport sans le dire.

### Ce qui n'est volontairement pas corrigé

- **D2** : jours manquants signalés, pas compensés (voir plus haut).
- **D4** : détection indicative seulement, pas d'homogénéisation.
- **D5** : altitude de la maille SAFRAN non disponible.
- **M1** : formule de VPD inchangée dans cette passe (corrigée en version 1.5).

### Tests

240 contrôles (`Rscript tests/lancer_tests.R`), dont `tests/test_v12.R` :
année partielle, tendance, couleurs, CSV (aller-retour), neutralisation du texte
hostile, génération de bout en bout, délai dépassé. La suite a aussi été
rejouée sur une copie dont **tous** les fichiers sont en minuscules
(`global.r`, `app.r`, `rapport_climat.rmd`…) : l'application démarre et les
rapports se génèrent.

---

## Version 1.3 — ergonomie et accessibilité (audit, partie 4)

Cette version traite les constats E1 à E8 de l'audit d'ergonomie. Mesure :
axe-core 4.10 (règles WCAG 2.0 à 2.2 AA et bonnes pratiques)
dans Chromium, sur l'application alimentée en données synthétiques, bureau
1 366 px et téléphone 390 px.

| Constat | Correction | Mesure avant → après |
|---|---|---|
| **E1** page sans langue | `lang = "fr"` sur `<html>` (`app.R`) | règle `html-has-lang` : 6 écrans sur 6 → 0 |
| **E2** champs sans étiquette | champ commune et curseur d'étendue : étiquette masquée à l'écran mais lue ; listes d'année des cartes : étiquette `for` qui nomme le graphique (« Année comparée du graphique « … » ») ; listes courtes (période, mois, année) en `<select>` natif | règle `label` : 10 nœuds → 0 |
| **E3** pas de repère `<main>` | `<main id="contenu">`, cible du lien d'évitement (focus vérifié) | `landmark-one-main`, `region` : 0 |
| **E4** cibles trop petites | cases et boutons radio à 24 px ; pastilles de thématique entièrement cliquables ; poignées de curseur à 26 px ; 44 px au doigt (`pointer: coarse`) | onglet Rapport : 30 cibles < 24 px → 0 ; bilan mobile : 8 cibles < 44 px → 0 |
| **E5** Bilan très long | sommaire d'ancres collé sous les onglets (grand écran), avec le nombre d'indicateurs par thématique et la section courante signalée au défilement (`aria-current`) | voir ci-dessous |
| **E6** typographie | déjà traité en v1.2 (virgule décimale, vrai signe moins) ; vérifié sur le texte visible des écrans | 0 point décimal, 0 tiret court |
| **E7** titre sauté | `h4` → `h3` dans l'onglet Rapport | règle `heading-order` : 1 → 0 |
| **E8** pas de favicon | `www/favicon.png` (pictogramme du DSF) déclaré dans `<head>` | — |

Au total, 64 nœuds en échec sur les six écrans testés → 0.

**Ce qui n'a pas changé : la longueur du Bilan.** Il fait toujours une quinzaine
d'écrans (14 800 px avec les 24 indicateurs SAFRAN). Le sommaire règle la
navigation, pas le volume : atteindre la dernière thématique prend un clic au
lieu de dix écrans de défilement, mais la page reste longue. Sur téléphone, une
seule thématique est construite à la fois (pastilles), comme avant.

**Écarts avec ce que mesure axe.** Les poignées des curseurs ne sont pas
focalisables, axe ne les évalue donc pas ; elles ont été portées à 26 px sur la
foi de la règle WCAG 2.5.8, pas d'un résultat d'outil.

**Non vérifié** (inchangé par rapport à l'audit) : lecteur d'écran réel (NVDA,
VoiceOver), Safari et Firefox, zoom à 200 %, contraste des graphiques plotly,
test auprès d'utilisateurs. Un résultat axe à zéro ne vaut pas conformité RGAA.

### Effets de bord

- Les listes déroulantes de la période et des cartes sont des `<select>` natifs :
  plus de champ de recherche dans la liste, mais la frappe au clavier saute à
  l'option, et le téléphone ouvre son propre sélecteur. Selectize n'est plus
  chargé.
- Les cases à cocher sont plus grandes (24 px) : l'onglet Rapport gagne en
  lisibilité sur écran tactile, et les lignes de la liste d'indicateurs ont
  désormais toutes la même hauteur.
- Le fichier `strftime-min.js` peut apparaître en erreur 404 avec le paquet
  Debian de Shiny 1.8.0 (le fichier y est nommé `strftime.min.js`) : c'est un
  défaut de ce paquet, sans lien avec l'application, et sans effet sur l'affichage.

### Tests

258 contrôles, dont `tests/test_v13.R` : langue, repère principal, favicon,
étiquettes, listes natives, titres, règles CSS de cibles, présence du suivi de
section. Ces tests lisent le balisage ; ce qu'ils ne voient pas (taille rendue,
ordre de tabulation, contraste) a été contrôlé par axe-core dans le navigateur
(`outils/` ne contient pas ce script : il s'appuie sur Playwright et axe-core,
non installés avec l'application).

---

## Version 1.4 — simplicité d'utilisation (audit, partie 5)

Cette version traite les constats S1 à S5 de l'audit. Mesure : l'application
alimentée en données synthétiques (réseau simulé), dans Chromium, bureau 1 366 px,
avant puis après ; contrôle d'accessibilité axe-core 4.10 rejoué sur tous les
écrans.

| Constat | Correction | Mesure avant → après |
|---|---|---|
| **S1** trop de réglages, pas de « par où commencer » | onglet **Rapport** : la liste des indicateurs est repliée derrière une ligne d'état (« Les 24 indicateurs sont inclus ») ; « Tout inclure / Tout retirer » restent visibles. Onglet **Paramètres** : les seuils sont repliés derrière une ligne qui dit s'ils sont aux valeurs par défaut ou lesquels ont changé. En tête du **Bilan**, un bloc « À retenir » nomme les trois écarts les plus marqués, avec un lien vers leur thématique | contrôles visibles : Rapport 34 → 10, Paramètres 23 → 3 ; « À retenir » : 0 → 1 |
| **S2** aucun lien, un rechargement perd tout | l'adresse de la page porte l'analyse (point, source, période, année, plage, fenêtre, seuils **modifiés**) et se met à jour après chaque analyse ; bouton « Copier le lien » dans la barre de contexte ; ouvrir le lien relance l'analyse | adresse après analyse : aucune → `?v=1&x=…&y=…&s=safran&r=1991-2020&a=2026…` ; après rechargement : contexte et commune retrouvés (avant : page vide) |
| **S3** vocabulaire d'expert seulement dans « Méthode et lecture » | 19 définitions réunies dans `src/glossaire.R` : infobulles sur les termes des titres de cartes et de la synthèse (ETP, P − ETP, SWI, VPD, GDD, De Martonne, écart-type…), et carte « Glossaire » dans Sources | termes définis hors dépliant : 0 → 13 dans le Bilan, 19 dans le glossaire |
| **S4** attente du premier chargement annoncée dans Paramètres seulement | la phrase sous « Lancer » compte les années à télécharger d'après le cache disque : « 57 années à télécharger (environ une minute) », « …, 12 déjà en cache », ou « 57 années déjà en cache : chargement immédiat ». Pour une station Météo-France (un fichier par département), la phrase reste une fourchette | « environ une minute » (valeur fixe) → nombre d'années + durée estimée |
| **S5** échec d'un indicateur : message générique | une alerte en tête du Bilan nomme les indicateurs dont le calcul a échoué ; la carte montre la raison et, dans un bloc replié, le détail technique (désactivable : `CONFIG$app$detail_erreurs`) | message générique, détail seulement dans le journal du serveur → raison et détail à l'écran |

Au contrôle d'accessibilité : 0 nœud en échec sur Accueil, Bilan, Rapport,
Paramètres, Sources, sur téléphone, sur un bilan rouvert depuis un lien dont les
réglages sont modifiés et sur les pages dont tous les replis sont ouverts (même
résultat qu'en v1.3).

### Le lien partageable

```
?v=1&x=1.4727&y=43.3500&n=Auterive%20(31190)&s=safran&r=1991-2020&a=2026
 &p=1970-2026&f=4-10&c=1970-2026&t=chaleur_t2:28
```

`x, y` coordonnées WGS 84 · `n` nom affiché · `s` source (`safran` ou identifiant
de station) · `r` référence · `a` année comparée · `p` plage affichée · `f` fenêtre
de mois · `c` plage téléchargée · `t` seuils modifiés seulement.

- On y met des **nombres**, pas des libellés : le lien reste valable si un preset
  est renommé.
- **Une adresse est une entrée non fiable.** Chaque champ est converti, borné,
  puis rejeté (valeur par défaut) s'il sort des limites ; la source suit une liste
  blanche ; le nom du point, seul texte libre, est nettoyé ; un lien sans point
  valide est ignoré. Le lien ne contient aucune donnée privée : ni texte de
  rapport, ni fichier, ni identifiant de session.
- Les réglages qui diffèrent des valeurs habituelles ouvrent d'eux-mêmes le bloc
  replié qui les contient : on ne cache pas à un collègue ce que le lien a changé.
- Le codage est dans `src/utils_partage.R` (fonctions pures, testées sans
  navigateur) ; l'interface est construite à la demande (`ui <- function(request)`),
  ce qui fait naître les champs avec les valeurs du lien plutôt que de les
  corriger après coup.

### Deux défauts préexistants trouvés en route

- **« Tout retirer » dans Rapport valait « tout ».** Shiny renvoie `NULL` pour un
  groupe de cases entièrement décochées ; le code le lisait comme « aucune
  sélection, donc tout », à l'aperçu comme à la génération. Corrigé
  (`indicateurs_choisis()`) : un rapport sans indicateur est refusé, avec une
  notification.
- **Cause racine de S5.** L'opérateur `%||%` du projet traite un vecteur de
  longueur zéro comme absent : un tableau vide était remplacé par un tableau neuf
  qui perdait la raison de l'échec. D'où le message générique sur la carte, dans le
  Bilan comme dans le gabarit de rapport. `resultat_indicateur()` conserve
  désormais la raison, le message et le détail.

### Ce qui n'est volontairement pas corrigé, ou pas mesuré

- **L'estimation de durée n'est pas mesurée sur le vrai réseau.** Elle part d'un
  coût fixe par lot de requêtes (`CONFIG$api$duree_lot_s`, 6 s pour 6 requêtes
  parallèles) ; elle est donc donnée en ordre de grandeur (« quelques secondes »,
  « environ une minute »). À étalonner avec une première mesure réelle. L'année
  en cours n'est jamais en cache et compte toujours pour une requête.
- **La longueur du Bilan ne change pas** (≈ 15 000 px, 16,7 écrans). « À retenir »
  et le sommaire disent par où commencer ; ils ne raccourcissent pas la page.
- **Replier n'est pas retirer.** Les champs repliés restent dans la page et
  transmettent leurs valeurs ; seul ce qu'on lit en premier change.
- **Les infobulles ne s'affichent qu'à la souris.** Au doigt et au clavier, le
  repère est la carte « Glossaire » (le Bilan y renvoie par un lien).
- **L'alerte d'échec regroupe les erreurs de calcul.** Une erreur de tracé
  n'apparaît que sur la carte concernée.
- **`maquette_interface.html` n'est pas mise à jour** pour cette version.
- Non revérifié : lecteur d'écran réel, Safari et Firefox (voir version 1.3).

### Tests

370 contrôles, dont `tests/test_v14.R` : aller-retour du lien (point, source,
références personnalisées, plage, fenêtre, seuils), entrées hostiles (valeurs hors
bornes, scripts dans le nom, seuils inconnus, paramètres manquants), interface née
avec les valeurs du lien, infobulles (une par définition et par texte, texte
inchangé), phrase d'attente avec et sans cache, « À retenir » et replis, échec
d'un indicateur (raison conservée, « Tout retirer » ≠ « tout »), version, CSS et
JavaScript du bouton de copie. Le comportement dans le navigateur (adresse mise à
jour, rechargement, copie, alerte d'échec, axe-core) a été contrôlé à part par
Playwright, non fourni avec l'application.

---

## Version 1.5 — rigueur scientifique (audit, partie 6)

Cette version traite les constats M1 à M9 de l'audit. Une erreur de calcul est
corrigée (M1) ; le reste concerne ce que l'application **affirme** : chaque seuil
dit désormais s'il est une convention ou un repère appuyé par la littérature, et
les textes qui allaient plus loin que les sources ont été ramenés à ce qu'elles
permettent.

| Constat | Correction | Mesure avant → après |
|---|---|---|
| **M1** VPD surestimé de 5 à 20 % | `vpd_fao56()` (`utils_api.R`) : pression de saturation = moyenne de es(Tmax) et es(Tmin) (FAO-56, éq. 12), pression réelle = es × HU / 100 (éq. 19). Avant, la pression réelle était calculée sur es(Tmoy). Un VPD ne peut plus être négatif | cinq cas de l'audit : +19,9 / +12,2 / +11,9 / +9,2 / +5,2 % → 0 % ; exemple FAO-56 (Tn 18, Tx 25, HR 68 %) : ea = 1,7788 kPa retrouvé |
| **M2** p-value de tendance trop optimiste | fait en **v1.2** : Theil-Sen + Mann-Kendall avec correction de l'autocorrélation AR(1). Revérifié ici sur une pente calculée à la main | — |
| **M3** tendance dès 5 ans, sans intervalle | fait en **v1.2** : 20 années minimum, intervalle de confiance de la pente. Revérifié : 19 ans refusés, 20 acceptés | — |
| **M4** aucune incertitude affichée | l'intervalle de confiance des tendances date de la v1.2. L'erreur de SAFRAN et l'effet de la maille de 8 km sont **documentés** dans Sources avec les valeurs publiées (Quintana-Seguí et al. 2008) ; l'application ne les chiffre pas | texte absent → présent ; une valeur sans source (« l'erreur dépasse 30 % au-dessus de 1 200 m ») retirée |
| **M5** seuil de 1,5 kPa présenté comme une fermeture stomatique | la grandeur reste le **VPD moyen journalier** ; le libellé, le glossaire et la lecture disent que 1,5 kPa est un repère d'air très sec, pas le seuil de fermeture des stomates (Grossiord et al. 2020). Seuil toujours réglable | libellé « Seuil de VPD élevé » → « … (kPa, moyenne journalière) » |
| **M6** seuils et affirmations sans référence | `src/references.R` : 23 références (dont 7 déjà présentes, deux entrées générales en plus), une colonne **statut** et une colonne **références** pour chacun des 19 seuils (Paramètres, « Pourquoi ces valeurs ») : 16 « convention d'usage », 3 « repère indicatif ». Affirmations absolues atténuées (photosynthèse à 35 °C, SWI et scolytes, vent et chablis) | entrées de la carte « Références » : 9 → 25 ; seuils sans statut : 19 → 0 |
| **M7** gel tardif : fenêtre fixe, sans altitude | le calcul ne change pas (fenêtre calendaire, température sous abri) ; l'aide le dit. À partir de 400 m, le Bilan ajoute une note : retard de débourrement de 2 à 3,5 jours par 100 m (Vitasse et al. 2018), p. ex. à 850 m « 17 à 30 jours », et suggère de reculer la fin de fenêtre dans Paramètres | note d'altitude : aucune → à partir de 400 m |
| **M8** tests aveugles à M1 | `tests/test_v15.R` : VPD contre des valeurs écrites en dur (FAO-56, cinq cas de l'audit), Hargreaves (exemple 8), tendance et synthèse chiffrées à la main. Le générateur de données de test appelle la vraie fonction au lieu de recopier la formule | 370 → 431 contrôles |
| **M9** « pré-industrielle du GIEC » | 1961-1990 est présenté comme un repère antérieur à l'essentiel du réchauffement récent ; la base pré-industrielle du GIEC (1850-1900) n'est pas couverte par SAFRAN (`config.R`, glossaire, aide de l'onglet Période) | — |

### M1 en chiffres

`Rscript tests/simulation_vpd.R` rejoue la mesure de l'audit (30 étés de 92 jours,
Tmoy 20,5 ± 3,2 °C, demi-amplitude 7 ± 2 °C, HU 64 ± 11 %, 200 tirages) :

| | ancienne formule (≤ 1.4) | FAO-56 (1.5) |
|---|---|---|
| VPD moyen d'été | 1,10 kPa | 0,96 kPa |
| jours à 1,5 kPa ou plus, par été | 12,4 | 6,8 (÷ 1,8) |

L'audit donnait 11,5 → 6,1 jours : le rapport (près de 2) est le même, le niveau
dépend du tirage aléatoire et de la convention d'amplitude.

**Effet sur ce qui a déjà été produit.** Tout VPD calculé par une version ≤ 1.4
est trop haut de 5 à 20 %, et le nombre de jours de stress VPD environ deux fois
trop grand. Les rapports et exports CSV déjà diffusés avec cet indicateur sont à
refaire. L'application l'écrit dans Sources (carte « Corrections de méthode »), avec les
corrections de tendance et d'année partielle de la v1.2.
Aucune invalidation de cache n'est nécessaire : le VPD est recalculé à chaque
lecture à partir des températures et de l'humidité, y compris pour les années en
cache. Les stations Météo-France n'ont pas de VPD (valeur absente), comme avant.

### Les deux statuts des seuils

- **Convention d'usage** : repère courant en gestion forestière, sans étude qui le
  fonde ; à lire comme un curseur à ajuster, pas comme une limite biologique.
- **Repère indicatif** : l'ordre de grandeur est appuyé par la littérature citée,
  mais la valeur exacte reste un choix. Trois seuils portent ce statut : le SWI
  de stress (0,5 par défaut, à rapprocher du seuil d'eau extractible relative
  d'environ 0,4 de Granier 1999 et Bréda 2006), la fin de fenêtre de gel tardif
  (Zohner et al. 2020, Vitasse et al. 2018) et 35 °C de chaleur extrême
  (Sage et Kubien 2007).

Seules les références **vérifiées en ligne** (titre, auteurs, revue, année) ont été
ajoutées. Certains volumes et pages ont été complétés de mémoire ; la carte
« Références » le dit. La bibliographie d'origine est reprise telle quelle, non
revérifiée.

### Ce qui n'est volontairement pas corrigé, ou pas mesuré

- **L'erreur de SAFRAN n'est pas chiffrée dans l'application.** Elle est citée
  (température et humidité sans biais notable, précipitations sans biais mais
  RMSE de 2,4 mm/jour, vent biaisé de −0,3 m/s, rayonnement biaisé surtout sur la
  côte) ; la propager aux indicateurs demanderait une étude propre.
- **Le VPD reste un VPD journalier moyen** : il est plus bas que celui de
  l'après-midi, qui gouverne la fermeture des stomates. Un VPD de l'après-midi
  (à partir de Tmax) n'est pas proposé.
- **Les seuils restent des conventions.** Aucun seuil de SWI, de VPD ou de gel
  n'est établi pour une essence et un site donnés ; le statut le dit, il ne
  remplace pas une étude.
- **Le gel tardif garde sa fenêtre fixe.** La note d'altitude conseille, elle ne
  décale pas la fenêtre ; le débourrement réel n'est pas modélisé.
- **La correction d'autocorrélation des tendances est la version simplifiée**
  (variance multipliée par (1+r1)/(1−r1)), pas celle de Hamed et Rao 1998.
- **Les références sont ajoutées, pas relues en entier.** Une référence vérifiée
  en ligne (titre, auteurs, revue, année) n'est pas une référence relue : le lien
  entre chaque phrase et la page qui l'appuie reste à la charge d'un relecteur
  forestier.
- **`maquette_interface.html` n'est pas mise à jour** pour cette version.

### Tests

431 contrôles, dont `tests/test_v15.R` : VPD (valeurs écrites en dur, exemple
FAO-56, cinq cas de l'audit, écart de l'ancienne formule supérieur à 5 %, bornes,
valeurs absentes, indépendance vis-à-vis de Tmoy, VPD périmé d'un cache
recalculé), simulation (1,10 → 0,96 kPa), Hargreaves (exemple 8 de la FAO-56),
tendance à la main (pente de Sen, seuil de 20 ans, série constante), synthèse
chiffrée, cohérence des références (identifiants existants, années concordantes,
chaque citation « Auteur année » des textes présente dans la table), absence
d'affirmations absolues, note d'altitude du gel tardif, version, feuille de style.
L'affichage dans le navigateur (cartes Sources et Paramètres, note à 850 m,
axe-core sur Bilan, Paramètres, Sources et téléphone) a été contrôlé à part par
Playwright, non fourni avec l'application.

---

## Version 1.6 — solidité du code (audit, partie 7)

Cette version traite les constats C1 à C9 de l'audit. Plusieurs étaient déjà
réglés par les versions précédentes ; ils sont revérifiés, pas refaits. Ce qui
change ici : une option qui touchait tous les utilisateurs d'un coup, des erreurs
que le code avalait sans rien dire, et des blocs de serveur trop gros pour être
testés.

| Constat | État | Ce qui a été fait |
|---|---|---|
| **C1** test rouge (indicateurs sur tableau vide) | déjà réglé | la suite passe ; un test fait tourner les 24 indicateurs sur un jeu vide et vérifie qu'aucun ne lève d'erreur |
| **C2** « cache local » modifiait `CONFIG` pour tout le processus | **corrigé** | le choix est gardé dans la session (`session$userData`), lu par `cache_est_actif()` ; plus aucun `<<-` sur `CONFIG` |
| **C3** échecs silencieux | **corrigé** là où une erreur de calcul était avalée (liste plus bas) | journal du serveur avec contexte + message à l'écran + mention dans le rapport |
| **C4** « source unique de vérité » | **corrigé** | le jour 181 du « dernier gel de printemps » vient de `CONFIG$methode` ; le libellé de chaleur lisait déjà le contexte, le 0,95 de `calc_petp` n'existe plus |
| **C5** cache sans version de formule | déjà réglé | le nom du fichier porte le schéma (`CONFIG$cache$schema`), et les colonnes dérivées (`p_tot`, `vpd`) sont recalculées à chaque lecture, y compris pour les années en cache |
| **C6** paquets non verrouillés | **en partie** | `DESCRIPTION` (paquets, R ≥ 4.1, httr2 ≥ 1.0) et `outils/figer_versions.R` qui écrit `renv.lock` chez vous ; le verrou lui-même n'est pas livré (voir plus bas) |
| **C7** fonctions trop longues | **en partie** | quatre blocs extraits en fonctions testables ; les serveurs restent longs (chiffres plus bas) |
| **C8** tests incomplets | **mesuré et complété** | tendance, Hargreaves et synthèse : v1.2 et v1.5 ; rendu du rapport : v1.2, et v1.6 pour le cas d'échec ; **couverture mesurée : 64,0 % → 74,8 %** |
| **C9** « Paramètrès » | **corrigé** | plus « dès que » avec son accent, dans la même phrase |

### C2 — le cache est une option de session

Avant, décocher « Utiliser le cache local » exécutait `CONFIG$cache$actif <<- …` :
la configuration du **processus** changeait, donc celle de toutes les sessions
qu'il sert. Maintenant `mod_parametres` écrit le choix dans `session$userData`, et
`cache_est_actif()` le relit par la session courante (`getDefaultReactiveDomain()`)
— sans le passer en argument à travers toute la chaîne SAFRAN et stations. Hors
session (tests, scripts), c'est `CONFIG$cache$actif` qui décide. La phrase
« X années déjà en cache » dépend du choix et se met à jour quand on le change.

### C3 — ce qui n'est plus avalé

Deux fonctions : `journaliser_erreur()` (une ligne au journal, avec le contexte :
« Commentaire de 'tmp' en échec : … ») et `commentaire_indicateur()` /
`synthese_securisee()`, qui ne lèvent jamais d'erreur mais **portent la cause**
dans un attribut. Sont concernés :

- **la synthèse du Bilan** : un calcul qui échoue affiche « Synthèse chiffrée
  indisponible » avec le détail technique replié, au lieu de faire disparaître le
  bloc (et avec lui les mises en garde sur l'année incomplète) ;
- **le commentaire automatique d'une carte** : « Le commentaire automatique n'a pas
  pu être produit pour cet indicateur », avec son détail, au lieu d'un vide qui
  ressemblait à « rien à signaler » ;
- **l'aperçu du rapport** et **le rapport lui-même** : « Commentaire non produit
  pour : … », et, si la synthèse échoue, un point d'attention disant que les mises
  en garde sur l'année incomplète n'ont pas pu être calculées ;
- le journal garde aussi la trace d'un graphique de rapport, de la qualité de série
  d'une station, d'un repli de mise en forme et d'une conversion en graphique
  interactif qui échouent.

Les replis qui restent sont voulus : un fichier de cache illisible vaut « absent »,
un lien d'adresse invalide est ignoré, un service réseau qui ne répond pas est
signalé à l'utilisateur par ses propres messages.

### C7 — ce qui a été extrait, et ce qui reste long

Extrait en fonctions qui prennent des valeurs et rendent des balises (testables
sans Shiny) : `synthese_carte`, `synthese_notes`, `synthese_retenir_ui`,
`synthese_ui`, `indicateurs_non_calculables`, `qualite_serie_ui` (dans
`mod_bilan.R`) ; `contexte_ui`, `messages_service_ui`, `phrase_attente`,
`etat_chargement_ui` (dans `utils_ui.R`). Le HTML de la synthèse a été comparé
avant/après sur les mêmes données : **identique à l'octet**.

| Serveur | v1.5 | v1.6 |
|---|---|---|
| module Bilan | 450 lignes | 301 |
| `server` d'`app.R` | 346 | 271 |
| module graphique | 263 | 271 (+8 : message d'échec du commentaire) |
| module rapport | 210 | 215 |

Ils restent longs. Ce qui y reste est de la mise en réseau de réactifs (observateurs,
sélection de famille, instanciation d'un module par indicateur), que l'on ne
découpe pas sans risque de casser l'ordre d'exécution ; je ne l'ai pas fait sans
pouvoir le mesurer dans un vrai navigateur sur chaque changement.

### C8 — la couverture, mesurée pour la première fois

`Rscript outils/couverture.R` (nécessite `covr`) rejoue la suite en instrumentant le
code. Lignes exécutées par les tests :

| Fichier | v1.5 | v1.6 |
|---|---|---|
| `mod_graphique.R` | 0 % | 76,6 % |
| `mod_bilan.R` | 1,4 % | 82,2 % |
| `mod_parametres.R` | 33,3 % | 82,4 % |
| `mod_rapport.R` | 2,2 % | 44,2 % |
| `utils_ui.R` | 82,1 % | 95,7 % |
| `utils_calc.R` | 93,5 % | 94,2 % |
| **ensemble de `src/`** | **64,0 %** (1 633 / 2 551 lignes) | **74,8 %** (1 939 / 2 592) |

Les modules ont gagné leur couverture grâce au simulateur de serveur de Shiny
(`shiny::testServer`). Restent presque non exécutés : `mod_localisation.R` (3 %,
carte Leaflet) et `utils_api.R` (28 %, code réseau). `app.R` n'entre pas dans la
mesure (son serveur n'est pas chargé par les tests), pas plus que le code du
modèle de rapport (`.Rmd`), exécuté seulement par la génération de bout en bout. Une ligne exécutée n'est pas une ligne vérifiée : le pourcentage dit ce
que les tests touchent, pas ce qu'ils contrôlent.

### C6 — verrouiller les versions

`DESCRIPTION` déclare les dix paquets chargés par `global.R`, R ≥ 4.1 (le code
utilise le tube natif `|>`) et httr2 ≥ 1.0 (téléchargements en parallèle). Il est
écarté du bouton « Publish » (`.rscignore`) : le déploiement ne change pas.
Pour figer vos versions, une fois, sur la machine où tout marche :

```r
source("outils/figer_versions.R")   # écrit renv.lock
renv::restore(lockfile = "renv.lock")  # pour retrouver cet état ailleurs
```

Le verrou n'est **pas livré** : il décrirait les paquets de ma machine d'essai, pas
les vôtres. Son principe a été vérifié ici (il lit `DESCRIPTION` et liste 80
paquets avec leurs versions). `renv.lock` est aussi écarté de « Publish », où
rsconnect enregistre déjà ses propres versions.

### Ce qui n'est pas corrigé, ou pas mesuré

- **Les serveurs restent longs** (tableau ci-dessus). C'est le constat C7 pour
  moitié seulement.
- **Les vraies sessions simultanées ne sont pas testées.** Le simulateur de Shiny
  en fait vivre deux, sans navigateur ni réseau ; le comportement sous charge reste
  non mesuré (déjà noté par l'audit).
- **La suite a tourné avec un `leaflet` de substitution** dans mon environnement
  d'essai : la carte de localisation réelle n'a pas été exercée par les tests
  (elle l'est, à part, par le navigateur de contrôle).
- **Pas de verrou de versions livré** (C6).
- **La suite de tests elle-même suppose les noms d'origine** (`global.R`, `tests/*.R`) :
  elle ne tourne pas sur une copie entièrement en minuscules, alors que l'application,
  si (démarrage et lien partagé vérifiés sur une telle copie).
- **`maquette_interface.html` n'est pas mise à jour** pour cette version.

### Tests

528 contrôles, dont `tests/test_v16.R` : deux sessions simulées au cache différent
(lecture, écriture, annonce), module Paramètres, absence de `<<-` sur `CONFIG`,
journal et attributs d'erreur, **Bilan complet** sous `testServer` (cas normal,
note de gel à 850 m, synthèse en échec), module graphique (commentaire en échec),
aperçu du rapport, **rapport généré de bout en bout avec un commentaire en échec**,
constante de méthode lue dans `CONFIG`, chaque fonction extraite (couleurs des
cartes, notes, lien « À retenir », qualité de série, contexte avec nom de station
piégé, messages tronqués à douze, phrases d'attente, états), taille des serveurs,
cohérence de `DESCRIPTION` avec `global.R` et avec la version. Vérifié aussi dans un
navigateur (Playwright, non fourni) : Bilan, cache coché/décoché, synthèse et
commentaire en échec, axe-core 0 sur tous les écrans y compris mobile.

## Version 1.7 — sécurité (audit, partie 8)

Cette version traite les constats V1 à V8. **Modèle de menace retenu** (celui de
l'audit) : application web publique, sans compte, sur un hébergeur de type
shinyapps.io. Rien n'a été testé contre un serveur déployé : les contrôles portent
sur le code, sur des essais locaux et sur un navigateur de contrôle.

| Constat | État | Ce qui a été fait | Mesuré |
|---|---|---|---|
| **V1** cache non borné | **corrigé, sauf la clé** | `cache_purger()` (`utils_cache.R`) : supprime les fichiers de plus de 30 jours, puis les plus anciens jusqu'à 80 % du plafond (300 Mo). Au démarrage, puis au plus toutes les 10 minutes après une écriture. Ne touche que les fichiers de cache de ClimaFor | 300 points vieillis de 400 jours : la v1.6 garde tout (301 entrées, aucune fonction de purge) ; la v1.7 passe de **300 fichiers à 0** ; 300 points récents, plafond ramené à 0,3 Mo : 122 fichiers, 245 Ko |
| **V2** aucune limite d'usage | **en partie** | fenêtre glissante de 10 minutes (`utils_limites.R`, `CONFIG$limites`) : 10 analyses et 5 rapports par session, 40 analyses avec téléchargement pour tout le processus. Refus avec une phrase : pourquoi, dans combien de temps, résultats conservés | tests de la fenêtre ; navigateur, limite abaissée à 2 : la 3e analyse est refusée avec le message |
| **V3** aucune mention de confidentialité | **corrigé** | carte « Position et confidentialité » dans Sources ; note sous « Utiliser ma position » ; la position est arrondie à 10 m **dans le navigateur** puis de nouveau côté serveur, avant tout appel à l'API Géo | 0 occurrence dans la v1.6 → carte + note ; axe-core 0 |
| **V4** contexte du rapport non échappé | déjà réglé (v1.2, R1) | — | — |
| **V5** insertion HTML de données tierces | **corrigé** | nom, code postal et département échappés (`loc_resultat_html`) ; **second endroit trouvé en route** : l'étiquette du marqueur de la carte, que Leaflet lit comme du HTML, ne l'était pas non plus | réponse d'API piégée simulée : v1.6 `window.__xss = 1` (l'attribut `onerror` s'exécute) → v1.7 rien, le texte s'affiche tel quel |
| **V6** téléchargements sans liste d'hôtes ni plafond | **corrigé** | `mf_url_autorisee()` : HTTPS, `data.gouv.fr` et sous-domaines, **plus, depuis la v1.7.3, le seul « seau » de Météo-France chez OVHcloud** (domaine entier comparé), sans identifiants ni port étranger ; redirections limitées à HTTPS et à 5 ; adresse finale recontrôlée ; plafond de 500 Mo | serveur local : v1.6 télécharge 30 Mo sans limite et suit une redirection vers du HTTP en clair ; v1.7 coupe à 5 Mo (plafond d'essai) et refuse la redirection |
| **V7** `readRDS()` sur le cache | **corrigé** | le cache n'est plus un RDS mais un tableau JSON compressé (`.json.gz`), **relu avec contrôle** : en-tête, schéma, classes admises (texte, nombre, entier, logique, date), noms de colonnes, longueurs, dates réelles, taille décompressée plafonnée à 100 Mo. Un fichier qui échoue est supprimé et la donnée retéléchargée. Noms de fichiers assainis. Écriture atomique | v1.6 : un fichier forgé est rendu tel quel (liste contenant une fonction) ; v1.7 : refusé et supprimé |
| **V8** bornes non vérifiées côté serveur | **corrigé** | `seuils_depuis_champs()` : valeur hors des bornes → valeur par défaut **et** signalement dans l'état des seuils ; contexte du rapport borné à la réception | seuil de gel à 999 : v1.6 « 1 seuil modifié » → v1.7 « Valeur hors limites, non prise en compte » |

### V7 — pourquoi pas un RDS, et ce que ça coûte

`readRDS()` relit des objets R arbitraires, pas seulement des tableaux ; c'est le
risque noté par l'audit. Le cache ne contient pourtant que des tableaux de nombres,
de textes et de dates. Il est donc écrit en JSON compressé, dans lequel il n'existe
pas de fonction ni d'expression, puis revalidé colonne par colonne à la lecture. Le
schéma de cache passe à **3** : les anciens fichiers ne sont plus lus et sont purgés.

Le coût, mesuré sur des séries **synthétiques** (bruit gaussien, pas des données
SAFRAN réelles ; l'audit mesurait environ 16 Ko par année réelle) :

| | RDS (v1.6) | JSON compressé (v1.7) |
|---|---|---|
| une année SAFRAN | 5,3 Ko | 6,8 Ko |
| 68 années d'un point | 356 Ko, relues en 0,04 s | 450 Ko, relues en 0,32 s |
| 40 ans d'une station (18 colonnes) | 98 Ko, relus en 0,016 s | 149 Ko, relus en 0,15 s |

Deux écarts de fidélité à connaître : les nombres sont écrits avec 15 chiffres
significatifs, et `NaN` et l'infini reviennent en `NA` (aucun calcul de
l'application ne les distingue).

### Ce que la v1.7 ajoute en route

- **Position, clic sur la carte et commune choisie sont validés** (nombres finis,
  France métropolitaine) avant tout appel distant. Avant, seule la saisie manuelle
  l'était : une position hors de France ou une valeur forgée partait vers l'API Géo.
  Une commune d'outre-mer de la liste est désormais refusée avec un message.
- Le texte de recherche est borné à 100 caractères.
- `tests/test_stations.R` garde maintenant l'original de `mf_telecharger` (il le
  remplace par une lecture locale) pour que les tests de sécurité puissent l'exercer.

### Ce qui n'est pas corrigé, ou pas mesuré

- **Les adresses réelles des fichiers n'avaient pas pu être observées** (pas de
  réseau ici), et ce risque, annoncé ici comme « le risque de régression de cette
  version », **s'est réalisé** : de la v1.7 à la v1.7.2, tout téléchargement de station
  était refusé (voir le correctif 1.7.3 plus bas). Si Météo-France change encore
  d'hébergement, le refus nomme l'hôte ; il suffit de l'ajouter à
  `CONFIG$stations$hotes_autorises`, en connaissance de cause.
- **La clé du cache SAFRAN reste arrondie à 3 décimales (environ 100 m)**, alors que
  la maille fait 8 km (l'audit proposait de l'aligner). Les mailles SAFRAN ne sont
  pas alignées sur la latitude et la longitude, et je n'ai pas pu vérifier comment le
  service choisit la maille de réponse ; un arrondi plus grossier risquerait de
  servir à un point les données de la maille voisine. Le plafond de taille règle le
  risque disque ; la limite d'usage borne l'amplification vers GeoSAS.
- **Aucune limite par adresse IP** : derrière le mandataire d'un hébergeur, le
  serveur ne la voit pas de façon fiable. Un script qui ouvre une nouvelle session à
  chaque requête contourne la limite par session ; seule la limite du processus le
  freine. **Les seuils 10 / 5 / 40 sont des estimations**, pas calibrées sur une
  charge réelle.
- **Un téléchargement de fichier départemental bloque toujours le processus** (R n'a
  qu'un fil) : la taille est plafonnée, le délai de 300 s demeure. Le rendre
  asynchrone n'est pas fait.
- **Non vérifiés** (hors de portée d'un audit de code) : en-têtes HTTP et politique
  de sécurité du contenu (ils dépendent de l'hébergeur), vulnérabilités connues des
  versions de paquets déployées, accès à l'hébergement, comportement sous attaque
  réelle. Aucun test d'intrusion.
- **La carte de confidentialité décrit ce que fait le code**, pas le traitement de
  l'organisme qui héberge l'application : responsable du traitement, journaux d'accès
  et cookies de l'hébergeur restent à déclarer par lui. Ce n'est pas une déclaration
  de conformité.
- Le fichier d'entrée du rapport (`entree.rds`, `utils_rapport.R` et
  `rapport/generer_rapport.R`) reste un RDS : il est écrit par l'application dans un
  dossier temporaire privé de la session et relu par le processus qu'elle lance,
  jamais depuis un dossier partagé ou persistant.
- `maquette_interface.html` n'est pas mise à jour pour cette version.

### Tests

664 contrôles, dont `tests/test_v17.R` : échappement avec une réponse d'API
piégée rendue par le module, bornes des seuils (module Paramètres sous `testServer`),
adresses de téléchargement ressemblantes ou mal formées (22 cas refusés), plafond de taille et
redirection finale, aller-retour du cache (NA, accents, dates, tableaux vides),
**16 fichiers de cache forgés ou abîmés**, bombe de décompression, noms de fichiers
hostiles, purge (âge, plafond, fichiers étrangers intacts, cadence), fenêtre
glissante avec horloge injectée, textes de la carte de confidentialité. Vérifié aussi
dans un navigateur (Playwright, non fourni) : liste de communes piégée, seuil hors
bornes, limite d'analyses abaissée à 2, axe-core 0 sur l'accueil, les paramètres et
Sources.

---

## Version 1.7.1 — revue complète du code

Aucune fonctionnalité nouvelle. Cette version répond à une question : après sept
séries de corrections, reste-t-il du code mort, du code en doublon ou en conflit,
et les constats de l'audit sont-ils réellement traités ? **Méthode** : analyse
syntaxique des 23 fichiers chargés (21 de `src/`, `app.R`, `global.R` ; définitions et
références de chaque fonction,
paramètres jamais lus, variables locales inutilisées avec `codetools`), lecture de
chaque réglage de `CONFIG` et de chaque classe CSS contre le code qui les emploie,
relecture des 62 constats de l'audit contre le code actuel, puis comparaison de ce
que dit le README avec ce que fait le code.

### Ce que la revue a trouvé

| Nature | Constat | Traitement |
|---|---|---|
| **Fonctions mortes** (2) | `moyenne_mobile()` (remplacée par `lissage_annuel()` en v1.2) et `meta_de()` n'étaient appelées nulle part | supprimées ; la documentation de `lissage_annuel()` ne renvoie plus à la première |
| **Paramètres jamais lus** (5 fonctions) | `texte_source(don, lieu)`, `ligne_source(don, lieu)`, `parametres_analyse(ctx, don, lieu)`, `mf_normaliser(brut, lat)`, `meta_donnees(don, ctx)` promettaient une information qu'elles n'utilisaient pas | retirés, appelants et tests ajustés ; `lieu` reste là où il sert (métadonnées du CSV) |
| **Variable locale inutilisée** | `st <- system2(...)` dans `lancer_rapport_arriere_plan()` | affectation retirée |
| **Réglages jamais lus** (11) | `app$contact`, `api$wmts_ign` (orthophotos ; seul le plan IGN est utilisé), `stations$dataset_slug`, `seuils$neige_lourde_mm`, `delai_donnees_j`, `couleurs$bleu_survol`, `fond`, `fond_alt`, `info`, `graphiques$police_secours` | supprimés. Le README de la feuille de route citait `neige_lourde_mm` comme existant : corrigé |
| **Doublon en conflit** | la pile typographique des graphiques interactifs était écrite **quatre fois** en dur dans `utils_plot.R` alors que `CONFIG$graphiques$police` (« Marianne ») existait sans être lue | une seule définition, `police = "Marianne, Arial, sans-serif"`, lue aux quatre endroits |
| **CSS mort** (−3,1 ko, −75 lignes) | `.cf-segments*` (onglets en segments, abandonnés), `.cf-etapes*`, `.cf-recap*`, `.cf-entete` et `__interieur`, `.cf-barre__actions`, `.cf-btn--fantome`, `.cf-alerte--succes`, la variable `--cf-rouge` | supprimés. **Rendu comparé pixel à pixel** avant/après (accueil, point choisi, bilan, paramètres, rapport, sources, impression ; 1 366 px et 390 px) : identique. Seul écart : les graphiques interactifs du Bilan sur ordinateur, au premier passage ; il est de même nature et de même taille que l'écart entre **deux passages de l'ancienne version**, et un second passage de l'ancienne version est identique à la nouvelle |
| **Constat d'audit annoncé traité, ne l'était qu'en partie** (E6) | le README disait « 0 point décimal », or les coordonnées s'affichaient « 43.3500 N · 1.4727 E » (carte de l'accueil, ligne de contexte du bilan, en-tête du rapport) au milieu de textes en virgule | affichées avec `fmt_fr()` : « 43,3500 N · 1,4727 E » ; vrai signe moins pour une longitude ouest. Vérifié dans le navigateur et dans un rapport HTML et Word généré |
| **Fichiers inconnus de `corriger_casse.R`** (6) | `DESCRIPTION`, `outils/couverture.R`, `outils/figer_versions.R`, `rapport/generer_rapport.R`, `www/favicon.png`, `www/favicon.svg` : après une copie qui les aurait renommés, le script ne les aurait pas remis en ordre. Le contrôle fait en v1.7 ne les avait pas vus | ajoutés ; un test compare désormais la liste au disque |
| **Paquet utilisé, non déclaré** | `curl::curl_options()` est appelé (`utils_stations.R`) sans que `curl` figure dans `DESCRIPTION` (il n'arrive que via `httr2`) | déclaré |
| **Commentaire périmé** | `app.js` parlait des « segments de la localisation », qui n'existent plus | reformulé |

### Les 62 constats de l'audit

Relus un par un contre le code actuel : **aucune régression constatée**. Cinquante-deux
constats ont un test qui les nomme, et il passe. Pour les dix autres (D2, D4, D5, D7,
G2, G6, G10, R4, R5, E6), la vérification a porté sur le code et, quand c'était possible,
sur le résultat (note de l'ETP sur les graphiques, altitude du point, légendes en mots,
année exclue nommée, mise en garde sur l'année incomplète dans le rapport, point
décimal visible dans le navigateur). Un seul écart entre le README et le code : **E6**,
rectifié ci-dessus. Je n'ai pas rejoué les mesures chiffrées de l'audit initial
(p-values simulées, durées de téléchargement) : elles reposent sur les tests existants.
Les constats laissés « en partie » ou « non traités » par les versions précédentes
le restent : C7 (fonctions de serveur longues : `mod_bilan_server` 301 lignes,
`mod_graphique_server` 271), D4 (détection de rupture, pas d'homogénéisation), D2
(jours manquants comptés, pas compensés), V2 (pas de limite par adresse IP).

### Garde-fous ajoutés

`tests/test_v171.R` (27 contrôles) empêche le retour de ces défauts : une fonction
définie mais jamais appelée, un nom défini deux fois, un paramètre jamais lu, une
variable locale inutilisée, un réglage de `CONFIG` que rien ne lit, une lecture de
`CONFIG` vers un réglage absent, un réglage cité dans le README qui n'existe pas, une
classe ou une variable CSS sans emploi, un paquet appelé par `pkg::` et non déclaré
(ou déclaré et jamais utilisé), un fichier du projet absent de `corriger_casse.R`,
un affichage décimal qui contourne `fmt_fr()`. **Vérifié par injection** : sur une
copie, une fonction morte, un paramètre inutile, une variable locale, un réglage, une
classe et une variable CSS, un paquet non déclaré et une citation fausse du README
ont chacun fait échouer le test attendu.

### Ce que cette revue ne couvre pas

- **Le code atteint par un nom construit** (`do.call(paste0(...))`) : une fonction
  appelée ainsi passerait pour morte si son nom n'apparaît nulle part en toutes
  lettres. Il n'y en a aucune aujourd'hui ; le test le dirait.
- **Le code atteignable mais jamais exécuté** : seule la couverture mesurée (`covr`,
  74,8 % en v1.6) le dit ; elle **n'a pas été remesurée** pour cette version.
- `outils/`, `diagnostic*.R`, `deploy.R` et `src/_gabarit_extension.R` (modèle à
  copier, non chargé) ne sont pas analysés.
- **Les 22 graphiques non regardés à l'écran** par l'audit initial le restent : la
  comparaison avant/après porte sur les écrans et l'impression, avec les données de
  démonstration, pas sur la lisibilité de chaque courbe.
- Une erreur JavaScript « `GLOBAL is not defined` » apparaît dans le navigateur de
  contrôle à l'ouverture du Bilan sur ordinateur, **identique avant et après cette
  revue** ; le serveur y répond 404 pour `strftime-min.js`, un fichier de plotly que
  l'installation Debian de ce poste d'essai n'a pas. Elle tient très probablement à cet
  environnement, **non vérifié sur un vrai déploiement**.
- `maquette_interface.html` n'est pas mise à jour (elle n'emploie aucune des classes
  supprimées).

### Correctif 1.7.2 — scripts de diagnostic cassés par la v1.7

`diagnostic.R` et `diagnostic_stations.R` chargent à la main quelques fichiers de
`src/`. La v1.7 a sorti le cache dans `utils_cache.R` sans les mettre à jour : à
l'étape 6, `diagnostic.R` s'arrêtait sur « could not find function "cache_nom_sur" »
(reproduit hors ligne). C'est le script à lancer précisément quand l'accès aux
données pose problème : il masquait la vraie cause. Les deux scripts chargent
maintenant `utils_cache.R` (et `utils_ui.R` pour les stations). Le défaut n'était pas
dans l'application elle-même, qui charge tout par `global.R` : le test de démarrage,
le parcours dans un navigateur et les rapports l'avaient vérifiée, pas ces deux
scripts. Un test (2 contrôles) charge désormais, dans un R vierge, les fichiers que
chaque script source et cherche toute fonction appelée sans définition ; vérifié en
retirant volontairement la ligne fautive.

Ce défaut n'était **pas** la cause du problème de connexion signalé ensuite sur un
poste : c'était le refus de l'hôte de téléchargement, corrigé en 1.7.3.

### Correctif 1.7.3 — l'hôte réel des fichiers de stations

**Symptôme** (relevé sur un poste) : « Département 31 : adresse de téléchargement
refusée (hôte non autorisé) : https://meteofrance.s3.sbg.io.cloud.ovh.net/data/synchro_ftp/
BASE/QUOT/Q_31_latest-2025-2026_RR-T… » (idem pour le 32). **Cause** : les fichiers de
Météo-France ne sont pas servis par `data.gouv.fr` mais par son stockage objet chez
OVHcloud ; la v1.7 (V6) n'autorisait que `data.gouv.fr`, d'après une adresse que
j'avais écrite de mémoire sans l'avoir observée. Les tests de la v1.7 vérifiaient la
règle contre cette même hypothèse : ils ne pouvaient pas la contredire. Aucune station
n'était téléchargeable de la v1.7 à la v1.7.2 (la grille SAFRAN, elle, n'était pas
concernée).

**Correctif** : `CONFIG$stations$hotes_autorises` contient désormais
`meteofrance.s3.sbg.io.cloud.ovh.net`, le **nom d'hôte complet** de ce seau, et non
`ovh.net` ni `cloud.ovh.net` : ce serait autoriser n'importe quel client d'OVHcloud.
Un autre seau, une autre région (`gra`), un faux hôte, HTTP et un autre port restent
refusés (tests). La carte « Position et confidentialité » nomme ce service tiers.

**Ce qui reste non vérifié** : je n'ai toujours pas pu télécharger un vrai fichier
depuis l'environnement où je travaille. La correction s'appuie sur l'adresse relevée
par vous, pas sur un essai réel ; la lecture du fichier lui-même (colonnes, codes
qualité) reste couverte par des fichiers synthétiques et par `diagnostic_stations.R`.


### Tests

691 contrôles (664 + 27), 0 échec. `test_v16.R` : l'assertion « DESCRIPTION déclare
exactement les paquets que `global.R` charge » devient « tous les paquets chargés
sont déclarés », puisque `curl` s'y ajoute et que le nouveau fichier contrôle le
reste.

---

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
├── DESCRIPTION                 Paquets et versions requis (v1.9.0)
├── .github/workflows/          Diagnostic GeoSAS, Construire la carte, Veille (v1.8)
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
