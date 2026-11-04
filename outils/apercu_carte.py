#!/usr/bin/env python3
"""Apercu autonome de l'onglet Carte : UN fichier HTML (feuille de style de l'application,
MapLibre et carte.js embarques) qui lit les fichiers publies de la carte a l'ouverture.

    python3 outils/apercu_carte.py apercu_carte.html [adresse_des_fichiers]
    python3 outils/apercu_carte.py apercu_hors_ligne.html --hors-ligne DOSSIER_DES_FICHIERS

Avec --hors-ligne, les fichiers de la carte (meta.json, grille.json, c_*.bin.gz) sont EMBARQUES
dans la page : elle n'a plus besoin du reseau (hors fond de carte IGN). Utile quand l'afficheur
bloque les requetes (apercu integre a une messagerie, par exemple).

Rien n'est calcule ici. Le HTML de l'onglet et les icones viennent de apercu_carte_ui.py (copies de
mod_carte_ui() et cf_icone(), dont tests/test_v18.R verifie qu'elles n'ont pas derive).
Le bloc « Diagnostic technique » de la page dit ce que le navigateur sait faire (WebGL, Web Worker)
et ce qu'il a rendu, pour comprendre une carte qui n'affiche rien. Bibliotheque standard seule.
"""
import base64
import json
import sys
from pathlib import Path

from apercu_carte_ui import ICONES, ONGLETS, UI_CARTE

RACINE = Path(__file__).resolve().parent.parent
BASE_DEFAUT = "https://cammbrnd.github.io/climafor_cartes/"
TUILES_IGN = ("https://data.geopf.fr/wmts?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0"
              "&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&STYLE=normal&TILEMATRIXSET=PM"
              "&FORMAT=image/png&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}")

SCRIPT_PAGE = r"""
(function () {
  "use strict";
  var DEFAUT = __DEFAUT__;
  var TUILES = __TUILES__;
  var base = DEFAUT;
  var D = window.__DONNEES_EMBARQUEES || null;
  if (D) {
    // Mode hors ligne : les fichiers sont dans la page ; on repond a leur place aux fetch() de carte.js.
    base = "hors-ligne/";
    var fetchReel = window.fetch.bind(window);
    window.fetch = function (url, opts) {
      var u = String(url);
      if (u.indexOf(base) !== 0) return fetchReel(url, opts);
      var b64 = D.fichiers[u.slice(base.length)];
      if (!b64) return Promise.resolve(new Response("", { status: 404 }));
      var bin = atob(b64), oct = new Uint8Array(bin.length);
      for (var i = 0; i < bin.length; i++) oct[i] = bin.charCodeAt(i);
      return Promise.resolve(new Response(oct, { status: 200,
        headers: { "Content-Type": /\.json$/.test(u) ? "application/json" : "application/gzip" } }));
    };
  }
  try {
    var p = new URLSearchParams(location.search).get("base");
    if (!D && p && /^https?:\/\/[A-Za-z0-9.:\/_-]+$/.test(p)) base = p;
  } catch (e) {}
  if (base.charAt(base.length - 1) !== "/") base += "/";

  var note = document.querySelector('[data-ap="note"]');
  var bandeau = document.querySelector('[data-ap="analyse"]');
  var pre = document.querySelector('[data-ap="diag-texte"]');
  function log(t) { pre.textContent += t + "\n"; }
  note.textContent = "Aperçu autonome de l'onglet Carte de ClimaFor (même code que l'application). " +
    (D ? "Données embarquées dans cette page (copie publiée le " + D.genere_le + " : " + D.source + "). "
       : "Données lues sur " + base + " au moment de l'ouverture. ") +
    "Les autres onglets ne sont pas actifs ici.";

  // Onglets : seul « Carte » existe dans cet apercu.
  Array.prototype.forEach.call(document.querySelectorAll("[data-apercu-onglet]"), function (a) {
    a.addEventListener("click", function (e) {
      e.preventDefault();
      if (a.getAttribute("data-apercu-onglet") === "carte") return;
      bandeau.hidden = false;
      bandeau.textContent = "Aperçu : seul l'onglet Carte est actif dans cette page.";
    });
  });

  // « Analyser cette maille » ouvre, dans l'application, l'analyse complete du point.
  var racine = document.getElementById("carte-racine");
  racine.addEventListener("cf-carte-analyser", function (e) {
    var d = (e && e.detail) || {};
    bandeau.hidden = false;
    bandeau.textContent = "Dans l'application, ce bouton ouvre l'analyse complète de la maille" +
      (isFinite(d.lon) && isFinite(d.lat) ? " (lon " + Number(d.lon).toFixed(3) + ", lat " + Number(d.lat).toFixed(3) + ")" : "") +
      " dans l'onglet Bilan.";
  });

  // -- Diagnostic technique ---------------------------------------------------------------------
  log("Navigateur : " + navigator.userAgent);
  log("Adresse de la page : " + location.protocol + "//" + (location.host || "(fichier local)"));
  log(D ? "Données : embarquées dans la page (mode hors ligne)" : "Adresse des fichiers : " + base);
  var gl = false;
  try { var c = document.createElement("canvas"); gl = !!(c.getContext("webgl2") || c.getContext("webgl")); } catch (e) {}
  log("WebGL : " + (gl ? "oui" : "NON — la carte ne peut pas s'afficher"));
  try {
    var c2 = document.createElement("canvas"), g2 = c2.getContext("webgl2") || c2.getContext("webgl");
    var ext = g2 && g2.getExtension("WEBGL_debug_renderer_info");
    log("Version WebGL : " + (g2 ? (typeof WebGL2RenderingContext !== "undefined" && g2 instanceof WebGL2RenderingContext ? "2" : "1") : "-") +
        " | carte graphique : " + (ext ? g2.getParameter(ext.UNMASKED_RENDERER_WEBGL) : "inconnue") +
        " | texture max : " + (g2 ? g2.getParameter(g2.MAX_TEXTURE_SIZE) : "-"));
  } catch (e) { log("Infos WebGL : " + (e && e.message)); }
  log("Écran : " + window.innerWidth + "×" + window.innerHeight + " px, densité " + window.devicePixelRatio +
      " | MapLibre " + (window.maplibregl && window.maplibregl.getVersion ? window.maplibregl.getVersion() : "?"));
  log("DecompressionStream : " + (typeof DecompressionStream !== "undefined" ? "oui" : "NON"));
  var diagnostic = { worker: "en cours" };
  try {
    var w = new Worker(URL.createObjectURL(new Blob(["postMessage('ok')"], { type: "text/javascript" })));
    var t = setTimeout(function () { diagnostic.worker = "NON (aucune réponse) — MapLibre ne pourra pas préparer les mailles"; log("Web Worker (blob) : " + diagnostic.worker); }, 4000);
    w.onmessage = function () { clearTimeout(t); diagnostic.worker = "oui"; log("Web Worker (blob) : oui"); w.terminate(); };
    w.onerror = function () { clearTimeout(t); diagnostic.worker = "NON (erreur)"; log("Web Worker (blob) : NON (erreur) — MapLibre ne pourra pas préparer les mailles"); };
  } catch (e) { log("Web Worker (blob) : NON (" + (e && e.message) + ") — MapLibre ne pourra pas préparer les mailles"); }
  window.addEventListener("error", function (e) { log("Erreur : " + (e.message || e.type)); });
  window.addEventListener("unhandledrejection", function (e) { log("Promesse rejetée : " + (e.reason && e.reason.message || e.reason)); });

  window.CarteClimaFor.demarrer({ racine: "carte-racine", entree: "carte-clic", base: base, tuiles: TUILES, schema: 1 })
    .then(function (carte) {
      log("Fichiers lus : " + carte.meta.n + " mailles, " + carte.meta.couches.length + " indicateurs, " +
          carte.meta.annees[0] + "–" + carte.meta.annees[carte.meta.annees.length - 1]);
      var tuilesKO = 0;
      carte.map.on("error", function (e) {
        var m = String((e && e.error && e.error.message) || "");
        if (/tile|geopf|AJAXError|Failed to fetch/i.test(m)) { tuilesKO++; return; }   // fond IGN : compte a part
        log("MapLibre : " + m);
      });
      var cv = carte.map.getCanvas();
      cv.addEventListener("webglcontextlost", function () { log("!! Contexte WebGL PERDU (la carte graphique a abandonné le dessin)"); });
      cv.addEventListener("webglcontextrestored", function () { log("Contexte WebGL restauré"); });
      var t0 = Date.now();
      var instantane = function (quand) {
        try {
          var m = carte.map, n = m.queryRenderedFeatures({ layers: ["cellules"] }).length;
          var cl = m.getContainer().getBoundingClientRect();
          log("[" + quand + "] mailles dessinées : " + n + " | carte au repos : " + (m.loaded() ? "oui" : "non") +
              " | mailles prêtes : " + (m.isSourceLoaded("cellules") ? "oui" : "non") +
              " | zoom " + m.getZoom().toFixed(2) + " | cadre " + Math.round(cl.width) + "×" + Math.round(cl.height) +
              " | canevas " + cv.width + "×" + cv.height);
        } catch (e) { log("[" + quand + "] impossible à relever : " + (e && e.message)); }
      };
      [3, 6, 12, 25, 45].forEach(function (s) { setTimeout(function () { instantane(s + " s"); }, s * 1000); });
      setTimeout(function () {
        log("Fond de carte IGN : " + (tuilesKO ? tuilesKO + " tuile(s) non chargée(s) — fond gris, sans conséquence sur les mailles" : "aucune erreur"));
      }, 6000);
      var b = document.createElement("button");
      b.type = "button"; b.className = "cf-btn cf-btn--sec"; b.textContent = "Relever maintenant";
      b.addEventListener("click", function () { instantane("à " + Math.round((Date.now() - t0) / 1000) + " s"); });
      pre.parentNode.appendChild(b);
    })
    .catch(function (e) {
      log("Chargement : ÉCHEC — " + (e && e.message));
      note.textContent += " Si la carte ne se charge pas, vérifiez la connexion, ou ouvrez cette page avec ?base=<adresse des fichiers>.";
    });
})();
"""


def lire(chemin):
    return Path(chemin).read_text(encoding="utf-8")


def b64(chemin):
    return base64.b64encode(Path(chemin).read_bytes()).decode()


def donnees_embarquees(dossier):
    """Lit meta.json et les fichiers qu'il annonce ; renvoie le script qui les place dans la page."""
    d = Path(dossier)
    meta = json.loads((d / "meta.json").read_text(encoding="utf-8"))
    noms = ["meta.json", meta["grille"]] + [c["fichier"] for c in meta["couches"]]
    fichiers = {n: base64.b64encode((d / n).read_bytes()).decode() for n in noms}
    return ("window.__DONNEES_EMBARQUEES = " + json.dumps(
        {"genere_le": meta.get("genere_le"), "source": meta.get("source"), "fichiers": fichiers}) + ";")


def construire(base_defaut=BASE_DEFAUT, dossier_donnees=None):
    css_app = lire(RACINE / "www/dsfr-lite.css")
    css_map = lire(RACINE / "www/carte/lib/maplibre-gl.css")
    js_map = lire(RACINE / "www/carte/lib/maplibre-gl.js")
    js_carte = lire(RACINE / "www/carte/carte.js")
    for nom, src in (("maplibre-gl.js", js_map), ("carte.js", js_carte)):
        assert "</script" not in src.lower(), nom
    nav = "\n".join(
        '    <li%s><a href="#" role="tab" aria-selected="%s" data-apercu-onglet="%s">%s%s</a></li>' % (
            ' class="active"' if cle == "carte" else "",
            "true" if cle == "carte" else "false", cle, ICONES[cle], titre)
        for cle, titre in ONGLETS)
    embarque = ("<script>\n" + donnees_embarquees(dossier_donnees) + "\n</script>\n") if dossier_donnees else ""
    script = (SCRIPT_PAGE.replace("__DEFAUT__", repr(base_defaut)).replace("__TUILES__", repr(TUILES_IGN)))
    return f"""<!DOCTYPE html>
<html lang="fr">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">
<title>ClimaFor — aperçu de l'onglet Carte</title>
<style>
{css_app}
</style>
<style>
{css_map}
</style>
<style>
  /* Propre a cet apercu : l'application n'a pas ces elements. */
  .ap-note {{ margin: var(--e-4) 0 0; }}
  .ap-bandeau {{ margin-top: var(--e-4); }}
  .ap-bandeau[hidden] {{ display: none; }}
  .ap-diag {{ margin-top: var(--e-4); }}
  .ap-diag pre {{ white-space: pre-wrap; overflow-wrap: anywhere; font-size: var(--t-xs); }}
</style>
</head>
<body>

<header class="cf-barre">
  <div class="cf-barre__interieur">
    <div class="cf-entete-marques">
      <img class="cf-logo cf-logo--rf" src="data:image/png;base64,{b64(RACINE / 'www/republique_francaise.png')}" alt="République française — Liberté, Égalité, Fraternité">
      <img class="cf-logo cf-logo--operateur" src="data:image/png;base64,{b64(RACINE / 'www/logo_dsf.png')}" alt="Correspondant observateur — Département de la santé des forêts">
    </div>
    <div class="cf-marque">
      <div class="cf-marque__organisme">DSF — Département de la santé des forêts</div>
      <h1 class="cf-marque__titre">ClimaFor</h1>
      <div class="cf-marque__soustitre">Bilan météorologique et climatique pour la gestion forestière</div>
    </div>
  </div>
</header>

<main class="cf-onglets" id="contenu" tabindex="-1">
 <div class="tabbable">
  <ul class="nav nav-tabs" role="tablist">
{nav}
  </ul>
  <div class="tab-content">
   <div class="tab-pane active">
    <div class="cf-conteneur">
{UI_CARTE}
     <div class="cf-alerte cf-alerte--info ap-bandeau" role="status" data-ap="analyse" hidden></div>
     <p class="cf-mention ap-note" data-ap="note"></p>
     <details class="cf-details ap-diag">
      <summary>Diagnostic technique de cet aperçu</summary>
      <div class="cf-details__corps"><pre data-ap="diag-texte"></pre></div>
     </details>
    </div>
   </div>
  </div>
 </div>
</main>

{embarque}<script>
/* MapLibre GL JS (BSD-3-Clause, voir www/carte/lib/LICENSE-maplibre-gl.txt) */
{js_map}
</script>
<script>
{js_carte}
</script>
<script>
{script}
</script>
</body>
</html>
"""


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    sortie = Path(sys.argv[1])
    if len(sys.argv) > 3 and sys.argv[2] == "--hors-ligne":
        html = construire(dossier_donnees=sys.argv[3])
    else:
        html = construire(sys.argv[2] if len(sys.argv) > 2 else BASE_DEFAUT)
    sortie.write_text(html, encoding="utf-8")
    print(f"{sortie} : {sortie.stat().st_size // 1024} Ko")
