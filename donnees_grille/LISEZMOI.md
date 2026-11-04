# Mailles à cartographier

**Par défaut (`mailles=sim`), il n'y a rien à fournir.** Le workflow « Construire la carte »
télécharge le fichier de coordonnées de Météo-France
(`coordonnees_grille_safran_lambert-2-etendu.csv`, 9 892 mailles, data.gouv.fr, Licence
Ouverte) et en tire, dans l'ordre du fichier, le centre de chaque maille en WGS84
(`outils/carte_sim.R`, `carte_sim_mailles()`), avec ses coordonnées d'origine
`LAMBX`/`LAMBY` (hectomètres, Lambert II étendu). Les carrés de la carte sont dessinés dans
ce repère natif : leurs bords sont exacts.

## Fichier personnalisé (facultatif)

Pour cartographier un sous-ensemble (ou une autre grille), indiquer dans « mailles » le
chemin d'un CSV de ce dépôt :

```
lon,lat,lambx,lamby
-4.96215,48.38206,600,24010
...
```

- `lon`, `lat` : centre de la maille, degrés décimaux WGS84 ; l'ordre des lignes fixe le
  rang de chaque maille (et donc l'`id` du fichier `grille.json`) ;
- `lambx`, `lamby` : coordonnées d'origine en hectomètres ; **obligatoires pour la source
  `sim`**, facultatives sinon (sans elles, les carrés sont construits autour du centre en
  Lambert-93, avec des bords approchés) ;
- pas de doublon ; coordonnées dans les bornes de `PARTAGE_BORNES` ;
- au plus `CONFIG$carte$n_mailles_max` lignes.
