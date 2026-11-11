/* tests/stations_verif_js.js — LIT stations.json AVEC LE CODE DU NAVIGATEUR
   Usage : node tests/stations_verif_js.js <dossier du jeu>
   Charge www/carte/carte.js (sans navigateur), lit stations.json avec verifierStations() et ecrit
   sur la sortie standard un JSON que tests/test_v25.R compare a ce que R a ecrit : les deux cotes
   doivent lire la meme table de stations. Verifie aussi que les fichiers altres sont refuses. */
"use strict";
const fs = require("fs"), path = require("path");
global.window = global;
global.addEventListener = function () {};
global.document = { addEventListener: function () {} };
require(path.join(__dirname, "..", "www", "carte", "carte.js"));
const I = window.CarteClimaFor._interne;

const dossier = process.argv[2];
const json = JSON.parse(fs.readFileSync(path.join(dossier, "stations.json"), "utf8"));
const st = I.verifierStations(json, 1);
const nan = (x) => (Number.isFinite(x) ? x : null);

const sortie = {
  n: st.n,
  id: st.stations.map((s) => s.id), nom: st.stations.map((s) => s.nom),
  lon: st.stations.map((s) => s.lon), lat: st.stations.map((s) => s.lat),
  alt: st.stations.map((s) => nan(s.alt)), derniere: st.stations.map((s) => s.derniere),
  pluie: st.stations.map((s) => s.pluie), temp: st.stations.map((s) => s.temp),
  rejets: {}
};

// Entrees alterees : chacune doit etre refusee.
const rejette = (f) => { try { f(); return false; } catch (e) { return true; } };
const copie = () => JSON.parse(JSON.stringify(json));
const R = sortie.rejets;
R.schema = rejette(() => I.verifierStations(Object.assign(copie(), { schema: 99 }), 1));
R.illisible = rejette(() => I.verifierStations(null, 1)) && rejette(() => I.verifierStations("x", 1));
R.n_trop_grand = rejette(() => I.verifierStations(Object.assign(copie(), { n_stations: 99999 }), 1));
R.n_zero = rejette(() => I.verifierStations(Object.assign(copie(), { n_stations: 0 }), 1));
R.colonne_absente = rejette(() => { const x = copie(); delete x.stations.lat; I.verifierStations(x, 1); });
R.longueurs = rejette(() => { const x = copie(); x.stations.nom = x.stations.nom.slice(1); I.verifierStations(x, 1); });
R.id_chemin = rejette(() => { const x = copie(); x.stations.id[0] = "../../etc"; I.verifierStations(x, 1); });
R.id_html = rejette(() => { const x = copie(); x.stations.id[0] = "<img src=x>"; I.verifierStations(x, 1); });
R.id_double = rejette(() => { const x = copie(); x.stations.id[1] = x.stations.id[0]; I.verifierStations(x, 1); });
R.lat_hors_territoire = rejette(() => { const x = copie(); x.stations.lat[0] = 250; I.verifierStations(x, 1); });
R.lon_hors_territoire = rejette(() => { const x = copie(); x.stations.lon[0] = 55.5; I.verifierStations(x, 1); });
// `null` (valeur absente) ne doit pas devenir 0 : une station sans coordonnee est refusee, une
// altitude absente reste absente.
R.lat_nulle = rejette(() => { const x = copie(); x.stations.lat[0] = null; I.verifierStations(x, 1); });
sortie.alt_nulle_reste_absente = (() => {
  const x = copie(); x.stations.alt[0] = null;
  return Number.isNaN(I.verifierStations(x, 1).stations[0].alt);
})();
// Un nom hostile reste du TEXTE (carte.js n'insere jamais de HTML) et est borne.
sortie.nom_borne = (() => {
  const x = copie(); x.stations.nom[0] = "<IMG SRC=X ONERROR=ALERT(1)>" + "A".repeat(500);
  return I.verifierStations(x, 1).stations[0].nom.length <= 80;
})();
sortie.nom_lisible = I.nomLisible("LA ROCHE-SUR-YON") + "|" + I.nomLisible("TOULOUSE-BLAGNAC") + "|" + I.nomLisible("PIC DU MIDI");
process.stdout.write(JSON.stringify(sortie));
