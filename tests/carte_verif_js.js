/* tests/carte_verif_js.js — LIT UN JEU DE CARTE AVEC LE CODE DU NAVIGATEUR
   Usage : node tests/carte_verif_js.js <dossier du jeu>
   Charge www/carte/carte.js (sans navigateur), decode le jeu avec ses propres
   fonctions et ecrit sur la sortie standard un JSON que tests/test_v18.R compare a
   ce que R a ecrit : les deux cotes du format doivent dire la meme chose. Verifie
   aussi que les entrees alterees sont refusees. */
"use strict";
const fs = require("fs"), path = require("path"), zlib = require("zlib");
// Navigateur minimal : carte.js n'a besoin que de ces deux points d'ancrage au chargement.
global.window = global;
global.addEventListener = function () {};
global.document = { addEventListener: function () {} };
require(path.join(__dirname, "..", "www", "carte", "carte.js"));
const I = window.CarteClimaFor._interne;

const dossier = process.argv[2];
const lire = (f) => fs.readFileSync(path.join(dossier, f));
const meta = JSON.parse(lire("meta.json").toString("utf8"));
const m = I.verifierMeta(meta, 1);
const grille = I.verifierGrille(JSON.parse(lire(m.grille).toString("utf8")), m.n);

const sortie = { n: m.n, annees: m.annees, reference: m.reference, couches: {},
                 centres: Array.from(grille.cx).map((x, i) => [x, grille.cy[i]]), rejets: {} };
m.couches.forEach((c) => {
  const b = zlib.gunzipSync(lire(c.fichier));
  const ab = b.buffer.slice(b.byteOffset, b.byteOffset + b.byteLength);
  const d = I.decoderCouche(ab, m.n, m.annees.length, c.stock);
  const val = [];
  for (let k = 0; k < m.annees.length; k++) {
    const ligne = [];
    for (let i = 0; i < m.n; i++) {
      const v = d.valeurs[k * m.n + i];
      ligne.push(v === I.MANQUANT ? null : v / d.echelle);
    }
    val.push(ligne);
  }
  const nan = (x) => (Number.isFinite(x) ? x : null);
  sortie.couches[c.cle] = {
    normale: Array.from(d.normale).map(nan), pente: Array.from(d.pente).map(nan),
    signif: Array.from(d.signif), valeurs: val, domaine: c.domaine };
});

// Entrees alterees : chacune doit etre refusee.
const rejette = (f) => { try { f(); return false; } catch (e) { return true; } };
const copie = () => JSON.parse(JSON.stringify(meta));
const R = sortie.rejets;
R.schema = rejette(() => I.verifierMeta(Object.assign(copie(), { schema: 2 }), 1));
R.chemin = rejette(() => { const x = copie(); x.couches[0].fichier = "../secret.json"; I.verifierMeta(x, 1); });
R.sous_dossier = rejette(() => { const x = copie(); x.couches[0].fichier = "a/b.bin.gz"; I.verifierMeta(x, 1); });
R.url = rejette(() => { const x = copie(); x.couches[0].fichier = "http://evil.example/x.json"; I.verifierMeta(x, 1); });
R.palette = rejette(() => { const x = copie(); x.couches[0].palette = "arc_en_ciel"; I.verifierMeta(x, 1); });
R.domaine = rejette(() => { const x = copie(); x.couches[0].domaine = [5, 1]; I.verifierMeta(x, 1); });
R.n_trop_grand = rejette(() => I.verifierMeta(Object.assign(copie(), { n_mailles: 50000 }), 1));
R.annees_desordre = rejette(() => I.verifierMeta(Object.assign(copie(), { annees: [2000, 1999, 2001] }), 1));
R.couche_tronquee = rejette(() => {
  const c = m.couches[0], b = zlib.gunzipSync(lire(c.fichier));
  const ab = b.buffer.slice(b.byteOffset, b.byteOffset + b.byteLength - 2);
  I.decoderCouche(ab, m.n, m.annees.length, c.stock);
});
R.grille_hors_france = rejette(() => {
  const g = JSON.parse(lire(m.grille).toString("utf8"));
  g.features[0].geometry.coordinates[0][0][0] = 80;
  I.verifierGrille(g, m.n);
});
R.grille_id = rejette(() => {
  const g = JSON.parse(lire(m.grille).toString("utf8"));
  g.features[1].properties.id = 7;
  I.verifierGrille(g, m.n);
});
process.stdout.write(JSON.stringify(sortie));
