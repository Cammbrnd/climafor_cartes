# Mailles à cartographier

Le workflow « Construire la carte » lit ici `mailles_safran.csv` : **une ligne par maille
SAFRAN réelle** (≈ 9 900 pour la France métropolitaine, Corse comprise).

```
lon,lat
-4.7512,48.4003
-4.6486,48.4011
...
```

- `lon`, `lat` : centre de la maille, degrés décimaux WGS84 ; l'ordre des lignes fixe
  le rang de chaque maille (et donc l'`id` du fichier `grille.json`) ;
- pas de doublon ; coordonnées dans les bornes de `PARTAGE_BORNES` ;
- au plus `CONFIG$carte$n_mailles_max` lignes.

**Ce fichier n'existe pas encore** : l'énumération de la grille réelle reste à faire
(voir « Avant le premier calcul réel » dans le README). Le contour grossier de
`outils/carte_demo.R` ne convient qu'au jeu fictif.
