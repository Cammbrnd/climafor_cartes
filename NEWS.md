# ClimaFor — historique des versions

Ce fichier garde la mémoire de ce qui a changé, version après version, et pourquoi. Le
[README](README.md) décrit l'application **telle qu'elle est** ; ce fichier dit **comment elle l'est devenue**.

**Tenue du fichier.** Chaque version ajoute une entrée en tête du tableau ci-dessous, avec la date,
ce qui change pour l'utilisateur, ce qui change pour qui maintient le code, et ce qui est
volontairement laissé de côté. La version est déclarée à deux endroits qui doivent concorder
(`src/config.R` et `DESCRIPTION`) ; la suite de tests le vérifie.

## En bref

| Version | Ce qui change | Détail |
|---|---|---|
| **2.4** | **Bilan climatique juste après l'Accueil.** Jours de chaleur à **25, 30, 35 et 40 °C**, jours de gel à **0, −5 et −10 °C**, **premier jour à plus de 25 °C et à plus de 30 °C**, et **gel tardif** défini par le **débourrement estimé** (somme de degrés-jours), avec sa définition sourcée. La base précalculée est à reconstruire. | [README, version 2.4](README.md#version-24--seuils-de-chaleur-et-de-gel-premiers-jours-chauds-gel-tardif) |
| **2.3** | Le **mode rapide** s'étend aux **stations Météo-France** : le bilan d'une station lit ses années passées dans le jeu précalculé et ne télécharge que l'année en cours. Le mode rapide devient le **mode par défaut**. Tests automatiques à chaque envoi sur GitHub. Historique des versions dans ce fichier. | [README, version 2.2](README.md#version-22--une-base-précalculée-pour-le-bilan-dun-point-mode-rapide) |
| **2.2** | **Base précalculée** : le précalcul de la carte publie aussi les colonnes annuelles et les références dont le bilan d'une maille a besoin ; **mode rapide** (Paramètres → Mode de chargement). | [README](README.md#version-22--une-base-précalculée-pour-le-bilan-dun-point-mode-rapide) |
| **2.1** | Stations publiées avec la carte (mesure et modèle côte à côte), comparaison de **deux lieux**, **synthèse du terrain** à envoyer par courriel. | [README](README.md#version-21--mesures-et-modèle-côte-à-côte-deux-lieux-synthèse-à-envoyer) |
| **2.0** | **Trois profils** (terrain, correspondant-observateur, permanent DSF) et **bilan synthétique** par département, région, fiches ou point. 2.0.1 : la fenêtre RStudio ne se ferme plus avec la session. | [README](README.md#version-20--trois-profils-et-bilan-synthétique) |
| **1.9** | Vagues de chaleur (25ᵉ indicateur), export CSV du bilan, onglet Comparer. | [README](README.md#version-19--vagues-de-chaleur-export-csv-onglet-comparer) |
| **1.8** | Onglet **Carte** : un indicateur annuel pour toute la France, maille SAFRAN par maille. | [README](README.md#version-18--onglet-carte) |
| **1.7 → 1.7.3** | Sécurité (V1–V8), puis revue complète du code (62 constats) et correctifs. | [ci-dessous](#version-17--sécurité-audit-partie-8) |
| **1.6** | Solidité du code (C1–C9). | [ci-dessous](#version-16--solidité-du-code-audit-partie-7) |
| **1.5** | Rigueur scientifique (M1–M9) : une erreur de calcul corrigée, le statut de chaque seuil. | [ci-dessous](#version-15--rigueur-scientifique-audit-partie-6) |
| **1.4** | Simplicité d'utilisation (S1–S5), lien partageable. | [ci-dessous](#version-14--simplicité-dutilisation-audit-partie-5) |
| **1.3** | Ergonomie et accessibilité (E1–E8). | [ci-dessous](#version-13--ergonomie-et-accessibilité-audit-partie-4) |
| **1.2** | Corrections de l'audit : données (D1–D7), graphiques (G1–G10), rapports (R1–R6). | [ci-dessous](#version-12--corrections-issues-de-laudit-données-graphiques-rapports) |

---

# Journal détaillé des audits (versions 1.2 à 1.7.3)

Les sections ci-dessous sont reprises telles quelles du README, où elles occupaient plus de sept cents
lignes. Elles décrivent des corrections ponctuelles et ce qui a été mesuré avant et après ; le
comportement actuel de l'application est décrit dans le README.

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
- **`docs/maquette_interface.html` n'est pas mise à jour** pour cette version.
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
- **`docs/maquette_interface.html` n'est pas mise à jour** pour cette version.

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
- **`docs/maquette_interface.html` n'est pas mise à jour** pour cette version.

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
- `docs/maquette_interface.html` n'est pas mise à jour pour cette version.

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
- `docs/maquette_interface.html` n'est pas mise à jour (elle n'emploie aucune des classes
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
