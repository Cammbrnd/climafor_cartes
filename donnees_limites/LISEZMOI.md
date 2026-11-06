# Contours des départements et des régions

`departements.geojson` (96 départements de la France métropolitaine, propriétés `code`, `nom`,
`region`) et `regions.geojson` (13 régions, `code`, `nom`), en WGS84, version simplifiée
(quatre décimales).

- **Tracé** : IGN, Admin Express COG, édition 2018 (Licence ouverte Etalab 2.0).
- **Noms et codes** : INSEE, code officiel géographique (millésime 2018).
- **Mise en forme** : dépôt `gregoiredavid/france-geojson` (versions simplifiées), filtrées sur la
  métropole par `outils/limites_construire.R`.

Ces contours servent à désigner un département ou une région dans l'onglet « Bilan synthétique » :
la valeur d'un territoire est la moyenne des mailles SAFRAN dont le centre est dans le contour. Un
tracé simplifié suffit pour cet usage (une maille fait 8 km), pas pour mesurer une surface.
Le fichier est du GeoJSON : il se relit tel quel en Python ou en TypeScript.
