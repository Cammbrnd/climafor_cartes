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
3. Krigeage ordinaire (`gstat`), voisinage de 40 stations, variogramme Exp / Sph / Mat (le meilleur est retenu),
   validation croisée en 10 blocs.

## Lancer

- **Automatique** : tout envoi sur la branche qui touche `exploration/krigeage/` (par exemple `parametres.env`).
- **Manuel** : Actions → « Krigeage P-ETP estival » → Run workflow (disponible seulement si le fichier de workflow est aussi sur la branche par défaut ; sinon, modifier `parametres.env`).
- **Sur un poste** : `Rscript krigeage_p_etp_estival.R` (paquets : sf, gstat, ggplot2, scales, httr2), réglages par variables `KRIGEAGE_*`.

## Résultats

`resultats/<année>/` : `carte_p_etp_jja_<année>.png`, `incertitude_krigeage_<année>.png`,
`diagnostic_krigeage_<année>.png` (variogramme et validation croisée), `stations_p_etp_jja_<année>.csv`.
Les mêmes fichiers sont gardés 30 jours en artefact du calcul.

## Limites à garder en tête

- Pas d'altitude dans le krigeage : la surface est lissée en montagne.
- ETP de Hargreaves : ignore vent et humidité.
- La validation croisée garde le variogramme ajusté sur toutes les stations.
