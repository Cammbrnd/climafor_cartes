# Krigeage des couches de l'onglet Carte (exploration)

Calcul exploratoire, **hors application** : il vit sur la branche `krigeage-p-etp-estival` et ne doit pas
être fusionné dans `main` tel quel. Il lit les fichiers que ClimaFor publie déjà sur GitHub Pages et n'y
écrit rien.

## Ce que fait le calcul

1. Lit `stations.json` et `s_<grandeur>.bin.gz` (adresse `https://cammbrnd.github.io/climafor_cartes/`) :
   la valeur annuelle de chaque couche à chaque station publiée, **telle que l'application la calcule**
   (même fonction, année retenue si 95 % des jours sont renseignés, code qualité douteux écarté).
   Seules les stations publiées servent : au moins 15 années valides et une mesure de moins de 730 jours.
2. Pour chaque couche, évalue par validation croisée (10 groupes de stations tirés au hasard, graine 2026,
   mêmes groupes pour les trois formules) :
   - krigeage ordinaire (sans altitude),
   - krigeage à dérive externe sur l'altitude (dérive réestimée dans chaque voisinage de 40 stations),
   - krigeage de régression (tendance globale sur l'altitude, puis krigeage des résidus).
3. Retient la meilleure. Un candidat est valide si au plus 0,5 % des stations restent sans prédiction et si
   son RMSE est inférieur à 5 fois celui du krigeage ordinaire ; la dérive locale est gardée si la tendance
   globale gagne moins de 1 % ; la tendance doit battre le krigeage ordinaire, sinon celui-ci est retenu.
4. Krige une surface à 500 m (altitude moyenne de chaque maille tirée des tuiles AWS Terrain, zoom 12).
   L'altitude des mailles est ramenée à la plage des stations, sauf pour la température, les degrés-jours.
   La surface est ramenée dans les bornes physiques de la grandeur (pas de pluie ou de jours négatifs).

## Les 22 couches

| Même méthode que les cartes estivales | tmp, jja_t, gdd, pre, jja_p, petp, veg, mart, gel, gel5, gel10, can1, can, can3, can4 |
|---|---|
| À lire avec prudence (`prudence = TRUE` dans le tableau de sortie) | chaud1, chaud2 (date jamais atteinte : station absente), vag, vagj (événements rares), geltd (débourrement estimé), dry (bruitée), vent (emplacement des anémomètres) |
| Non faisables depuis des stations | swi, vpd, vpdm, neige |

## Sorties (`resultats/<annee>/`)

| Fichier | Contenu |
|---|---|
| `validation_couches_<annee>.csv` | les 22 couches réunies, avec `verdict_nash` (tient : Nash ≥ 0,80 ; reserve : 0,60 à 0,80 ; fragile : < 0,60), calculé après coup sur la validation croisée |
| `validation_couches_<annee>_<groupe>.csv` | une ligne par couche : nombre de stations, corrélation avec l'altitude, RMSE / écart absolu / Nash des trois formules, formule retenue, variogramme (pépite, palier, portée), gain, prédiction min/max, écart-type médian |
| `carte_<couche>_<annee>_0.5km.png` | la carte (stations en points, même échelle de couleur) |
| `diagnostic_<couche>_<annee>.png` | variogramme, validation croisée, valeur selon l'altitude |
| `stations_couches_<annee>_<groupe>.csv` | valeurs par station |
| `donnees_stations_<annee>_<groupe>.csv` | résumé des valeurs lues (stations, min, max, part de zéros) |
| `journal_couches_<annee>_<groupe>.txt` | journal du calcul |

Les GeoTIFF (`<couche>_<annee>_0.5km.tif`, 2 bandes : valeur, écart-type ; environ 10 Mo chacun) restent en
**artefact** du lancement, 30 jours : trop lourds pour la branche.

## Lancer

- Modifier `parametres.env` sur la branche relance le calcul ; ou *Actions > Krigeage des couches de la
  carte > Run workflow* (entrées : année, lecture seule).
- `KRIGEAGE_LECTURE_SEULE=oui` lit et résume les valeurs publiées, sans calcul (quelques minutes).
- Les couches sont réparties en 3 jobs (température et eau ; gel et chaleur ; événements). Compter environ
  5 minutes de téléchargement des tuiles d'altitude, puis 2 à 3 minutes par couche.
- Hors GitHub : `KRIGEAGE_DOSSIER_DONNEES=<dossier>` lit les fichiers dans un dossier local, et
  `KRIGEAGE_MNT_FICHIER=<GeoTIFF>` remplace les tuiles d'altitude.

## Limites à connaître

- La validation croisée emploie l'altitude **déclarée** des stations : elle juge si une valeur se prédit
  depuis les autres stations, pas la finesse de la maille ni du MNT.
- Les groupes de validation sont tirés au hasard, pas par zones : la validation est optimiste là où le réseau
  est dense, et plus encore pour les phénomènes très locaux.
- Une station n'est publiée que si elle compte assez d'années : le réseau krigé est donc plus petit que celui
  des cartes estivales (qui lisent toutes les stations de Météo-France).
- Les normales et les écarts à la normale de l'onglet ne sont pas cartographiés ici ; la normale de chaque
  station est dans `stations_couches_*.csv` (colonnes `<couche>_normale`).
