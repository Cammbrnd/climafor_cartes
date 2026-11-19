# Krigeage de P − ETP estival (exploration)

Calcul **hors application**, pour voir ce que donne un krigeage des stations Météo-France.
Cette branche (`krigeage-p-etp-estival`) n'est pas destinée à être fusionnée dans `main` : la suite
de tests de l'application exige que chaque fichier du dépôt soit listé dans `corriger_casse.R`, ce qui
n'a pas de sens pour des résultats générés.

## Ce que fait le calcul

1. Télécharge les fichiers quotidiens `RR-T-Vent` (un par département, période « latest ») du jeu
   Météo-France « Données climatologiques de base — quotidiennes » (licence ouverte Etalab 2.0).
2. Pour chaque station : P − ETP sur **juin–août** (même définition d'« estival » que ClimaFor).
   - ETP de Hargreaves-Samani à partir de TN et TX (les fichiers n'ont pas d'ETP), fonction identique à celle de l'application ;
   - code qualité 2 (« douteux ») écarté ;
   - les jours sans pluie ou sans ETP sont retirés des **deux** sommes (jamais comptés comme zéro) ;
   - station gardée si au moins 95 % des jours sont exploitables (`KRIGEAGE_SEUIL`).
3. Krigeage (`gstat`), voisinage de 40 stations, variogramme Exp / Sph / Mat (le meilleur est retenu),
   validation croisée en 10 blocs.
4. **Avec MNT** (`KRIGEAGE_MNT=oui`, par défaut) : krigeage à dérive externe, l'altitude moyenne de chaque maille est la
   tendance. Le MNT vient des tuiles ouvertes « terrarium » (AWS, altitude en mètres), moyennées sur chaque maille ; on peut
   fournir son propre GeoTIFF avec `KRIGEAGE_MNT_FICHIER` (sur un poste). Le script calcule aussi le krigeage ordinaire
   pour comparer : si l'altitude n'améliore pas la validation croisée, il garde le krigeage ordinaire et le dit.

## Lancer

- **Automatique** : tout envoi sur la branche qui touche `exploration/krigeage/` (par exemple `parametres.env`).
- **Manuel** : Actions → « Krigeage P-ETP estival » → Run workflow (disponible seulement si le fichier de workflow est aussi sur la branche par défaut ; sinon, modifier `parametres.env`).
- **Sur un poste** : `Rscript krigeage_p_etp_estival.R` (paquets : sf, gstat, ggplot2, scales, httr2), réglages par variables `KRIGEAGE_*`.

## Résultats

`resultats/<année>/` : `carte_p_etp_jja_<année>.png`, `incertitude_krigeage_<année>.png`,
`diagnostic_krigeage_<année>.png` (variogramme et validation croisée), `stations_p_etp_jja_<année>.csv`.
Avec MNT, les fichiers portent le suffixe `_mnt` (et `_1km`, `_2km`… si la maille n'est pas celle par défaut de 5 km) (le diagnostic ajoute P − ETP selon l'altitude).
`p_etp_jja_<année>[_mnt].tif` : la surface krigée (Lambert-93, 2 bandes : P − ETP et écart-type de prédiction), à ouvrir dans QGIS.
Les mêmes fichiers sont gardés 30 jours en artefact du calcul.

## Limites à garder en tête

- La tendance n'est pas extrapolée au-delà de la plus haute station (altitude plafonnée dans les mailles plus hautes).
- Le MNT est moyenné sur la maille (5 km par défaut) : le relief plus fin que la maille n'apparaît pas.
- ETP de Hargreaves : ignore vent et humidité.
- La validation croisée garde le variogramme ajusté sur toutes les stations.

## Covariables et comparaison avec Météo-France

- `KRIGEAGE_COVARIABLES` (vide par défaut = altitude seule ; `expo,mer` pour les tester) : en plus de l'altitude, le script teste l'exposition des versants (pente orientée vers
  le nord / l'est, calculée sur le MNT à une ou plusieurs échelles, `KRIGEAGE_EXPO_ECHELLE_KM=1,2,5` : 1 à 2 km = versant
  de vallée, 5 km = relief régional ; au moins 1 km) et la proximité de la mer
  (`exp(−distance à la mer / KRIGEAGE_MER_ECHELLE_KM)`, 50 km par défaut). Les combinaisons sont évaluées par validation
  croisée (mêmes blocs) et la meilleure est retenue ; un gain de moins de 1 % ne justifie pas une covariable de plus.
  Le tableau est dans le journal et dans `validation_tendances_<année>.csv` (un candidat instable y est marqué
  `valide = FALSE`). Les covariables des stations sont dans `stations_covariables_<année>.csv`.
- Essais de 2026 : aucune covariable n'a été retenue (exposition à 1, 2 et 5 km : erreur plus grande que l'altitude seule ;
  distance à la mer : gain trop faible). Le tableau complet est gardé dans `validation_tendances_covariables_2026.csv`.
- `exposition_echelles_<année>.csv` : pour chaque échelle, ce qu'il reste d'exposition dans les résidus de la validation
  croisée de l'altitude seule (corrélations, R²) et la variation de RMSE (en %, négative = meilleure) des formules avec
  exposition par rapport à l'altitude seule, en tendance globale et en dérive locale (NA si la dérive locale diverge).
- Plusieurs covariables : la tendance est estimée une fois sur toutes les stations puis les résidus sont krigés
  (krigeage de régression). La dérive locale, qui réestime la tendance dans chaque voisinage, est instable avec plusieurs
  covariables (l'exposition est presque nulle en plaine) ; elle reste utilisée pour l'altitude seule.
- `comparaison_etp_mf.R` compare l'ETP Hargreaves-Samani des stations à l'ETP Penman-Monteith spatialisée de Météo-France
  (grille de 0,125°, data.gouv.fr) : `comparaison_etp_<année>.png` et `.csv`. Le format du fichier n'étant pas documenté,
  les colonnes sont reconnues d'après leur nom et l'en-tête est affiché dans le journal. Hors GitHub, on peut fournir le
  fichier avec `KRIGEAGE_ETP_FICHIER`.
