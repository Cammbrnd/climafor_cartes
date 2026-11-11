/* =============================================================================
   carte.js — ONGLET CARTE : composant navigateur
   -----------------------------------------------------------------------------
   Affiche un indicateur annuel par maille SAFRAN, avec un degrade de couleur, et
   recolore la carte SANS aller-retour avec le serveur.

   COMMENT C'EST RAPIDE
   --------------------
   La geometrie des mailles est chargee UNE fois. Les valeurs de TOUTES les
   annees d'un indicateur arrivent dans un seul tableau d'entiers 16 bits
   (c_<cle>.bin.gz, voir src/utils_carte.R pour la disposition). Changer d'annee,
   de mode ou d'indicateur ne fait que recalculer une couleur par maille et la
   poser avec setFeatureState : pas de reconstruction de polygones, pas de JSON
   renvoye par le serveur. (Mesure du prototype, rendu logiciel sans GPU : la
   recoloration de ~9 600 mailles ajoute environ 12 a 17 ms ; changer
   l'expression de peinture, elle, coutait environ 2,4 s.)

   SECURITE
   --------
   meta.json et les fichiers viennent d'un hote tiers. Aucune adresse n'est
   construite sans passer NOM_SUR, aucun texte n'est insere avec innerHTML : tout
   passe par textContent. Les formes des fichiers sont verifiees avant usage.

   CHARGEMENT PARESSEUX
   --------------------
   Ce fichier est leger. MapLibre (940 Ko) et les donnees ne sont charges qu'a la
   premiere ouverture de l'onglet, quand le serveur envoie "cf-carte-init".
   ========================================================================== */

(function () {
  "use strict";

  /* ---------- Constantes -------------------------------------------------- */

  var NOM_SUR = /^[a-z0-9_]{1,60}\.(json|bin\.gz)$/;
  var MANQUANT = -32768;
  var SOURCE = "cellules";

  // Stations Meteo-France publiees (stations.json). Memes regles que stations_meta_valider() (R) :
  // identifiants de 3 a 12 caracteres alphanumeriques, coordonnees dans les bornes du territoire
  // (PARTAGE_BORNES), au plus CONFIG$stations$publie$stations_max stations.
  var NOM_STATION = /^[A-Za-z0-9]{3,12}$/;
  var BORNES = { lon: [-6, 10], lat: [41, 52] };
  var STATIONS_MAX = 8000;
  // Ce qu'une station mesure : 0 pluie et temperatures, 1 pluie seule, 2 temperatures seules, 3 rien
  // d'exploitable (ne se publie pas ; garde par prudence). Le noir plein, le blanc cerne et le gris
  // different par leur CLARTE et non par une teinte : ils se lisent aussi sans les couleurs. Les
  // pastilles du squelette (climafor.css, .cf-carto__pastille) reprennent ces trois rendus.
  var CATEGORIES = { complete: 0, pluie: 1, temp: 2 };
  var COULEUR_STATION = ["match", ["get", "cat"], 0, "#1b1b1b", 1, "#ffffff", "#8a8a8a"];
  var CONTOUR_STATION = ["match", ["get", "cat"], 1, "#1b1b1b", "#ffffff"];
  var EPAISSEUR_STATION = ["match", ["get", "cat"], 1, 2, 1.2];

  // Degrades ColorBrewer (sans danger pour le daltonisme), du plus bas au plus haut.
  var SEQ = {
    chaud:  ["#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026"],
    eau:    ["#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58"],
    vert:   ["#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529"],
    violet: ["#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d"],
    froid:  ["#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b"]
  };
  var RDBU = ["#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"];
  var BRBG = ["#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#f5f5f5", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30"];
  var PRGN = ["#40004b", "#762a83", "#9970ab", "#c2a5cf", "#e7d4e8", "#f7f7f7", "#d9f0d3", "#a6dba0", "#5aae61", "#1b7837", "#00441b"];
  function inverse(a) { return a.slice().reverse(); }
  // Le nom dit la couleur du sens POSITIF de l'ecart.
  var DIV = {
    positif_rouge:  inverse(RDBU),
    positif_bleu:   RDBU,
    positif_vert:   BRBG,
    positif_violet: inverse(PRGN)
  };

  var FAMILLES = {
    hydrique: "Eau et bilan hydrique", thermique: "Températures", gel: "Gel",
    aridite: "Aridité et bioclimat", phenologie: "Phénologie", neige: "Neige", vent: "Vent"
  };

  /* ---------- Utilitaires -------------------------------------------------- */

  function txt(x, max) { return String(x == null ? "" : x).slice(0, max || 400); }
  function nombre(x) { var v = Number(x); return isFinite(v) ? v : NaN; }
  // `null` (valeur absente de stations.json) donnerait 0 avec Number() : on le garde absent.
  function nombreOuNaN(x) { return x == null || x === "" ? NaN : nombre(x); }

  function el(tag, attrs, enfants) {
    var e = document.createElement(tag);
    if (attrs) Object.keys(attrs).forEach(function (k) {
      if (k === "texte") e.textContent = attrs[k];
      else if (k === "classe") e.className = attrs[k];
      else e.setAttribute(k, attrs[k]);
    });
    (enfants || []).forEach(function (c) { if (c) e.appendChild(typeof c === "string" ? document.createTextNode(c) : c); });
    return e;
  }

  var formateurs = {};
  function formater(v, dec, signe) {
    if (!isFinite(v)) return "n.d.";
    var cle = dec + (signe ? "s" : "");
    if (!formateurs[cle]) {
      formateurs[cle] = new Intl.NumberFormat("fr-FR", {
        minimumFractionDigits: dec, maximumFractionDigits: dec,
        signDisplay: signe ? "exceptZero" : "auto"
      });
    }
    return formateurs[cle].format(v);
  }
  function avecUnite(v, couche, signe) {
    return formater(v, couche.decimales, signe) + (couche.unite ? " " + couche.unite : "");
  }

  function lireFichier(url) {
    return fetch(url, { credentials: "omit" }).then(function (r) {
      if (!r.ok) throw new Error("HTTP " + r.status);
      return r;
    });
  }

  /* ---------- Lecture et verification des fichiers ------------------------- */

  function verifierMeta(meta, schema) {
    if (!meta || typeof meta !== "object") throw new Error("meta.json illisible");
    if (Number(meta.schema) !== Number(schema)) throw new Error("version du format " + txt(meta.schema, 10) + " non prise en charge");
    var annees = Array.isArray(meta.annees) ? meta.annees.map(Number) : [];
    if (annees.length < 2 || annees.some(function (a, i) { return !isFinite(a) || (i && a <= annees[i - 1]); })) throw new Error("années invalides");
    var n = Number(meta.n_mailles);
    if (!(n >= 1 && n <= 20000) || n !== Math.floor(n)) throw new Error("nombre de mailles invalide");
    if (!NOM_SUR.test(String(meta.grille))) throw new Error("fichier de grille invalide");
    if (!Array.isArray(meta.couches) || !meta.couches.length) throw new Error("aucune couche");
    var couches = meta.couches.map(function (k) {
      if (!k || !NOM_SUR.test(String(k.fichier)) || !/^[a-z0-9_]{1,40}$/.test(String(k.cle))) throw new Error("couche invalide");
      var dom = function (d) {
        var a = Array.isArray(d) ? d.map(Number) : [];
        if (a.length !== 2 || !isFinite(a[0]) || !isFinite(a[1]) || a[0] >= a[1]) throw new Error("domaine invalide (" + txt(k.cle, 20) + ")");
        return a;
      };
      var palette = String(k.palette), ecart = String(k.ecart);
      if (!SEQ[palette] || !DIV[ecart]) throw new Error("palette inconnue (" + txt(k.cle, 20) + ")");
      var stock = Number(k.stock);
      if (!(stock >= 0 && stock <= 4)) throw new Error("précision invalide");
      return {
        cle: String(k.cle), fichier: String(k.fichier), famille: txt(k.famille, 30),
        titre: txt(k.titre, 200), unite: txt(k.unite, 20), decimales: Math.min(4, Math.max(0, Number(k.decimales) || 0)),
        stock: stock, palette: palette, ecart: ecart,
        domaine: dom(k.domaine), domaine_ecart: dom(k.domaine_ecart), domaine_pente: dom(k.domaine_pente),
        methode: txt(k.methode, 1500), lecture: txt(k.lecture, 1500), note: txt(k.note, 600)
      };
    });
    return {
      annees: annees, n: n, grille: String(meta.grille), couches: couches,
      reference: txt(meta.reference, 12), genere_le: txt(meta.genere_le, 12),
      source: txt(meta.source, 200), annee_partielle: meta.annee_partielle == null ? null : Number(meta.annee_partielle)
    };
  }

  // « TOULOUSE-BLAGNAC » -> « Toulouse-Blagnac » (meme regle que stations_nom_lisible() en R : les
  // petits mots restent en minuscules sauf en tete).
  var PETITS = ["de", "du", "des", "la", "le", "les", "sur", "sous", "en", "et", "aux", "au", "d", "l"];
  function nomLisible(x) {
    var morceaux = String(x == null ? "" : x).toLowerCase().match(/[^ '’-]+|[ '’-]+/g) || [], premier = true;
    return morceaux.map(function (m) {
      if (/^[ '’-]+$/.test(m)) return m;
      var garde = !premier && PETITS.indexOf(m) >= 0;
      premier = false;
      return garde ? m : m.charAt(0).toUpperCase() + m.slice(1);
    }).join("");
  }

  // stations.json : un vecteur par colonne, tous de la longueur annoncee. Chaque champ est controle
  // avant usage (le fichier vient d'un hote tiers) ; une station mal formee fait refuser le fichier.
  function verifierStations(j, schema) {
    if (!j || typeof j !== "object") throw new Error("stations.json illisible");
    if (Number(j.schema) !== Number(schema)) throw new Error("version du format des stations " + txt(j.schema, 10) + " non prise en charge");
    var n = Number(j.n_stations);
    if (!(n >= 1 && n <= STATIONS_MAX) || n !== Math.floor(n)) throw new Error("nombre de stations invalide");
    var t = j.stations;
    ["id", "nom", "dept", "lon", "lat", "alt", "derniere_obs", "fr_p", "fr_t", "a_pluie", "a_temp", "premiere", "n_p", "n_t"].forEach(function (c) {
      if (!t || !Array.isArray(t[c]) || t[c].length !== n) throw new Error("table des stations incomplète");
    });
    var vus = {}, out = [];
    for (var i = 0; i < n; i++) {
      var id = String(t.id[i]), lon = nombreOuNaN(t.lon[i]), lat = nombreOuNaN(t.lat[i]);
      if (!NOM_STATION.test(id) || vus[id]) throw new Error("identifiants de stations absents, en double ou mal formés");
      if (!(lon >= BORNES.lon[0] && lon <= BORNES.lon[1] && lat >= BORNES.lat[0] && lat <= BORNES.lat[1])) throw new Error("coordonnées de stations hors du territoire");
      vus[id] = true;
      var jour = String(t.derniere_obs[i] == null ? "" : t.derniere_obs[i]);
      var part = function (v) { var f = nombreOuNaN(v); return isFinite(f) ? Math.min(1, Math.max(0, f)) : NaN; };
      out.push({
        idx: i, id: id, nom: nomLisible(txt(t.nom[i], 80)) || "Station", dept: txt(t.dept[i], 3),
        lon: lon, lat: lat, alt: nombreOuNaN(t.alt[i]),
        derniere: /^[0-9]{4}-[0-9]{2}-[0-9]{2}$/.test(jour) ? jour : "",
        fr_p: part(t.fr_p[i]), fr_t: part(t.fr_t[i]),
        pluie: t.a_pluie[i] === true, temp: t.a_temp[i] === true,
        cat: t.a_pluie[i] === true && t.a_temp[i] === true ? 0 : t.a_pluie[i] === true ? 1 : t.a_temp[i] === true ? 2 : 3,
        n_p: nombreOuNaN(t.n_p[i]), n_t: nombreOuNaN(t.n_t[i])
      });
    }
    return { stations: out, n: n, genere_le: txt(j.genere_le, 12) };
  }

  // Decoupe le tampon d'une couche. La longueur est controlee : un fichier tronque
  // par un telechargement interrompu est refuse, pas affiche de travers.
  function decoderCouche(buf, n, nAnnees, stock) {
    var pad = (4 - (n % 4)) % 4;
    var attendu = 8 * n + n + pad + 2 * nAnnees * n;
    if (buf.byteLength !== attendu) throw new Error("fichier de couche de taille inattendue (" + buf.byteLength + " au lieu de " + attendu + ")");
    var o = 0, normale = new Float32Array(buf, o, n); o += 4 * n;
    var pente = new Float32Array(buf, o, n); o += 4 * n;
    var signif = new Uint8Array(buf, o, n); o += n + pad;
    var valeurs = new Int16Array(buf, o, nAnnees * n);
    return { normale: normale, pente: pente, signif: signif, valeurs: valeurs, echelle: Math.pow(10, stock) };
  }

  function degzip(buf) {
    var o = new Uint8Array(buf);
    if (o.length > 2 && o[0] === 0x1f && o[1] === 0x8b) {          // signature gzip
      if (typeof DecompressionStream === "undefined") throw new Error("ce navigateur ne sait pas décompresser les données (navigateur trop ancien)");
      var flux = new Blob([buf]).stream().pipeThrough(new DecompressionStream("gzip"));
      return new Response(flux).arrayBuffer();
    }
    return Promise.resolve(buf);   // l'hote a deja decompresse (en-tete Content-Encoding)
  }

  function verifierGrille(g, n) {
    if (!g || g.type !== "FeatureCollection" || !Array.isArray(g.features) || g.features.length !== n) throw new Error("grille inattendue");
    var cx = new Float32Array(n), cy = new Float32Array(n);
    var x0 = 1e9, y0 = 1e9, x1 = -1e9, y1 = -1e9;
    g.features.forEach(function (f, i) {
      var p = f && f.geometry && f.geometry.type === "Polygon" ? f.geometry.coordinates[0] : null;
      if (!p || p.length < 4 || !f.properties || Number(f.properties.id) !== i) throw new Error("maille " + i + " invalide");
      var sx = 0, sy = 0, m = p.length - 1;
      for (var j = 0; j < m; j++) {
        var x = Number(p[j][0]), y = Number(p[j][1]);
        if (!(x > -10 && x < 15 && y > 38 && y < 55)) throw new Error("coordonnées hors de la France");
        sx += x; sy += y;
        if (x < x0) x0 = x; if (x > x1) x1 = x; if (y < y0) y0 = y; if (y > y1) y1 = y;
      }
      cx[i] = sx / m; cy[i] = sy / m;
    });
    return { cx: cx, cy: cy, emprise: [[x0, y0], [x1, y1]] };
  }

  /* ---------- Couleurs ------------------------------------------------------ */

  function arrets(couleurs, lo, hi) {
    var out = [];
    couleurs.forEach(function (c, i) { out.push(lo + (hi - lo) * i / (couleurs.length - 1), c); });
    return out;
  }

  // Expression de peinture : la couleur vient de l'etat « x » de chaque maille.
  function expressionCouleur(couleurs, lo, hi) {
    return ["case", ["==", ["feature-state", "ok"], 1],
      ["interpolate", ["linear"], ["coalesce", ["feature-state", "x"], lo]].concat(arrets(couleurs, lo, hi)),
      "rgba(0,0,0,0)"];
  }

  /* ---------- Le composant -------------------------------------------------- */

  function Carte(racine, opts, meta, grille, maplibre) {
    var self = this;
    this.racine = racine; this.opts = opts; this.meta = meta; this.grille = grille;
    this.ml = maplibre;
    this.etat = { couche: null, mode: "valeur", k: meta.annees.length - 1, sel: -1, station: -1, lecture: null };
    this.stations = null;     // stations publiees (verifierStations), ou null
    this.cache = {};          // cle de couche -> donnees decodees (4 au plus)
    this.ordre = [];
    this.xs = new Float32Array(meta.n);   // valeur affichee de chaque maille (NaN = absente)

    function q(role) { return racine.querySelector('[data-role="' + role + '"]'); }
    this.q = q;
    this.dom = {
      couche: q("couche"), annee: q("annee"), anneeSortie: q("annee-sortie"), lecture: q("lecture"),
      carte: q("carte"), info: q("info"), legende: q("legende"), etat: q("etat"),
      methode: q("methode"), liste: q("liste"), listeDetails: q("liste") && q("liste").closest("details"),
      stations: q("stations"), stationsBloc: q("stations-bloc"), stationsNote: q("stations-note"),
      stationsTypes: q("stations-types")
    };
    this.typesStation = Array.prototype.slice.call(racine.querySelectorAll('input[data-role="station-type"]'));
    this.modes = Array.prototype.slice.call(racine.querySelectorAll('input[data-role="mode"]'));
    this.construireSelecteur();
    this.construireCarte();
    this.brancher();
    var an = meta.annees;
    this.dom.annee.min = an[0]; this.dom.annee.max = an[an.length - 1];
    this.dom.annee.value = an[this.etat.k];
    this.dom.anneeSortie.textContent = an[this.etat.k];
    this.modes.forEach(function (m) {
      var s = m.parentNode.querySelector("span");
      if (m.value === "ecart") s.textContent = "Écart à la normale " + meta.reference;
      if (m.value === "tendance") s.textContent = "Tendance " + an[0] + "–" + an[an.length - 1];
    });
  }

  Carte.prototype.message = function (t, erreur) {
    this.dom.etat.textContent = t;
    this.dom.etat.classList.toggle("cf-carto__etat--erreur", !!erreur);
  };

  Carte.prototype.construireSelecteur = function () {
    var self = this, parFamille = {};
    this.meta.couches.forEach(function (c) { (parFamille[c.famille] = parFamille[c.famille] || []).push(c); });
    Object.keys(FAMILLES).concat(Object.keys(parFamille).filter(function (f) { return !FAMILLES[f]; })).forEach(function (f) {
      if (!parFamille[f]) return;
      var g = el("optgroup", { label: FAMILLES[f] || f });
      parFamille[f].forEach(function (c) { g.appendChild(el("option", { value: c.cle, texte: c.titre })); });
      self.dom.couche.appendChild(g);
    });
  };

  Carte.prototype.construireCarte = function () {
    var self = this, ml = this.ml, meta = this.meta;
    var style = { version: 8, sources: {}, layers: [{ id: "fond-uni", type: "background", paint: { "background-color": "#eef1f4" } }] };
    if (typeof this.opts.tuiles === "string" && /^https:\/\/[^\s]+\{z\}[^\s]*\{x\}[^\s]*\{y\}|^https:\/\/[^\s]+\{z\}[^\s]*\{y\}[^\s]*\{x\}/.test(this.opts.tuiles)) {
      style.sources.fond = { type: "raster", tileSize: 256, tiles: [this.opts.tuiles], minzoom: 5, maxzoom: 16, attribution: "© IGN" };
      style.layers.push({ id: "fond", type: "raster", source: "fond" });
    }
    var map = this.map = new ml.Map({
      container: this.dom.carte, style: style, bounds: this.grille.emprise,
      fitBoundsOptions: { padding: 24 }, minZoom: 4, maxZoom: 12,
      attributionControl: { compact: true }, fadeDuration: 0, dragRotate: false
    });
    map.touchZoomRotate.disableRotation();
    map.addControl(new ml.NavigationControl({ showCompass: false }), "top-right");
    this.tooltip = el("div", { classe: "cf-carto__bulle", role: "presentation" });
    this.dom.carte.appendChild(this.tooltip);
    this.prete = new Promise(function (ok, ko) {
      map.once("load", function () {
        try {
          map.addSource(SOURCE, { type: "geojson", data: self.donneesGrille, promoteId: "id" });
          map.addLayer({ id: "cellules", type: "fill", source: SOURCE, paint: {
            "fill-color": "rgba(0,0,0,0)",
            "fill-opacity": ["case", ["==", ["feature-state", "sig"], 0], 0.35, 0.82],
            "fill-outline-color": "rgba(0,0,0,0.10)" } });
          map.addLayer({ id: "selection", type: "line", source: SOURCE, filter: ["==", ["get", "id"], -1],
            paint: { "line-color": "#000091", "line-width": 3 } });
          ok();
        } catch (e) { ko(e); }
      });
      map.on("error", function (e) { if (e && e.error && !/tile|Failed to fetch/i.test(String(e.error.message))) ko(e.error); });
    });
  };

  Carte.prototype.brancher = function () {
    var self = this, map = this.map;
    this.dom.couche.addEventListener("change", function () { self.choisirCouche(self.dom.couche.value); });
    this.modes.forEach(function (m) { m.addEventListener("change", function () { if (m.checked) self.choisirMode(m.value); }); });
    var attente = null;
    this.dom.annee.addEventListener("input", function () {
      if (attente !== null) return;            // une mise a jour par image affichee
      attente = requestAnimationFrame(function () {
        attente = null;
        self.choisirAnnee(self.meta.annees.indexOf(Number(self.dom.annee.value)));
      });
    });
    this.dom.lecture.addEventListener("click", function () { self.basculerLecture(); });
    if (this.dom.listeDetails) this.dom.listeDetails.addEventListener("toggle", function () { if (this.open) self.remplirListe(); });
    if (this.dom.stations) this.dom.stations.addEventListener("change", function () { self.afficherStations(self.dom.stations.checked); });
    this.typesStation.forEach(function (t) { t.addEventListener("change", function () { self.filtrerStations(); }); });
    map.on("click", function (e) {
      if (!map.getLayer("cellules")) return;
      // Une station recouvre une maille : le clic va a la station, sur ou pres de son point.
      var st = self.stationPres(e.point, 9);
      if (st) { self.selectionnerStation(st.idx); return; }
      var f = map.queryRenderedFeatures(e.point, { layers: ["cellules"] })[0];
      if (f) self.selectionner(Number(f.properties.id));
    });
    map.on("mousemove", function (e) {
      if (!map.getLayer("cellules") || e.originalEvent.pointerType === "touch") return;
      var st = self.stationPres(e.point, 6);
      if (st) {
        map.getCanvas().style.cursor = "pointer";
        self.tooltip.textContent = st.nom + (isFinite(st.alt) ? " · " + formater(st.alt, 0) + " m" : "") + " (station)";
        self.tooltip.style.display = "block";
        self.tooltip.style.left = (e.point.x + 14) + "px"; self.tooltip.style.top = (e.point.y + 14) + "px";
        return;
      }
      var f = map.queryRenderedFeatures(e.point, { layers: ["cellules"] })[0];
      if (!f) { self.tooltip.style.display = "none"; map.getCanvas().style.cursor = ""; return; }
      var i = Number(f.properties.id), x = self.xs[i];
      map.getCanvas().style.cursor = "pointer";
      self.tooltip.textContent = isFinite(x) ? self.libelleValeur(x) : "Pas de valeur";
      self.tooltip.style.display = "block";
      self.tooltip.style.left = (e.point.x + 14) + "px"; self.tooltip.style.top = (e.point.y + 14) + "px";
    });
    map.on("mouseout", function () { self.tooltip.style.display = "none"; });
  };

  /* ---- Etat : couche, mode, annee -------------------------------------------- */

  Carte.prototype.couche = function () { return this.etat.couche; };

  Carte.prototype.charger = function (c) {
    var self = this;
    if (this.cache[c.cle]) return Promise.resolve(this.cache[c.cle]);
    return lireFichier(this.opts.base + c.fichier).then(function (r) { return r.arrayBuffer(); })
      .then(degzip).then(function (buf) {
        var d = decoderCouche(buf, self.meta.n, self.meta.annees.length, c.stock);
        self.cache[c.cle] = d; self.ordre.push(c.cle);
        while (self.ordre.length > 4) delete self.cache[self.ordre.shift()];
        return d;
      });
  };

  Carte.prototype.choisirCouche = function (cle) {
    var self = this, c = this.meta.couches.filter(function (k) { return k.cle === cle; })[0];
    if (!c) return Promise.resolve();
    this.message("Chargement de « " + c.titre + " »…");
    return this.charger(c).then(function (d) {
      self.etat.couche = c; self.donnees = d;
      self.dom.couche.value = c.cle;
      self.majMethode();
      self.appliquer(true);
      self.message(self.meta.n + " mailles · " + self.meta.source + " · " + self.meta.annees[0] + "–" +
        self.meta.annees[self.meta.annees.length - 1] + " · calculé le " + self.meta.genere_le +
        (self.meta.annee_partielle ? " · " + self.meta.annee_partielle + " est en cours : non cartographiée" : ""));
    }).catch(function (e) { self.message("Cette couche n'a pas pu être chargée : " + txt(e && e.message, 160), true); });
  };

  Carte.prototype.choisirMode = function (mode) {
    this.etat.mode = mode;
    var tend = mode === "tendance";
    this.dom.annee.disabled = tend; this.dom.lecture.disabled = tend;
    if (tend) this.arreterLecture();
    this.appliquer(true);
  };

  Carte.prototype.choisirAnnee = function (k) {
    if (k < 0 || k >= this.meta.annees.length) return;
    this.etat.k = k;
    this.dom.annee.value = this.meta.annees[k];
    this.dom.anneeSortie.textContent = this.meta.annees[k];
    this.appliquer(false);
  };

  Carte.prototype.basculerLecture = function () {
    if (this.etat.lecture) return this.arreterLecture();
    var self = this, n = this.meta.annees.length;
    if (this.etat.k >= n - 1) this.choisirAnnee(0);
    this.dom.lecture.textContent = "Pause";
    var dernier = 0;
    var pas = function (t) {
      if (!self.etat.lecture) return;
      if (t - dernier >= 160) {
        dernier = t;
        if (self.etat.k >= n - 1) return self.arreterLecture();
        self.choisirAnnee(self.etat.k + 1);
      }
      self.etat.lecture = requestAnimationFrame(pas);
    };
    this.etat.lecture = requestAnimationFrame(pas);
  };

  Carte.prototype.arreterLecture = function () {
    if (this.etat.lecture) cancelAnimationFrame(this.etat.lecture);
    this.etat.lecture = null; this.dom.lecture.textContent = "Lecture";
  };

  // Calcule la valeur affichee de chaque maille puis la pose sur la carte.
  // `refaireCouleurs` : le mode ou la couche a change, l'expression de peinture
  // doit etre refaite (pas pour un simple changement d'annee).
  Carte.prototype.appliquer = function (refaireCouleurs) {
    var c = this.etat.couche, d = this.donnees, n = this.meta.n, map = this.map;
    if (!c || !d) return;
    var mode = this.etat.mode, xs = this.xs, k = this.etat.k;
    var dec = k * n, ech = d.echelle, v, x, i;
    for (i = 0; i < n; i++) {
      x = NaN;
      if (mode === "tendance") x = d.pente[i];
      else {
        v = d.valeurs[dec + i];
        if (v !== MANQUANT) x = mode === "ecart" ? v / ech - d.normale[i] : v / ech;
      }
      xs[i] = x;
    }
    if (refaireCouleurs) this.peindre();
    var tend = mode === "tendance";
    for (i = 0; i < n; i++) {
      x = xs[i];
      map.setFeatureState({ source: SOURCE, id: i },
        { x: isFinite(x) ? x : 0, ok: isFinite(x) ? 1 : 0, sig: tend && !d.signif[i] ? 0 : 1 });
    }
    this.majInfo();
    if (this.dom.listeDetails && this.dom.listeDetails.open) this.remplirListe();
  };

  Carte.prototype.domaine = function () {
    var c = this.etat.couche;
    return this.etat.mode === "valeur" ? c.domaine : this.etat.mode === "ecart" ? c.domaine_ecart : c.domaine_pente;
  };
  Carte.prototype.couleurs = function () {
    var c = this.etat.couche;
    return this.etat.mode === "valeur" ? SEQ[c.palette] : DIV[c.ecart];
  };

  Carte.prototype.peindre = function () {
    var dom = this.domaine(), cols = this.couleurs();
    this.map.setPaintProperty("cellules", "fill-color", expressionCouleur(cols, dom[0], dom[1]));
    this.majLegende(dom, cols);
  };

  Carte.prototype.libelleValeur = function (x) {
    var c = this.etat.couche, mode = this.etat.mode;
    if (mode === "valeur") return avecUnite(x, c);
    if (mode === "ecart") return avecUnite(x, c, true) + " par rapport à la normale";
    return avecUnite(x, c, true) + " par décennie";
  };

  Carte.prototype.majLegende = function (dom, cols) {
    var c = this.etat.couche, mode = this.etat.mode, L = this.dom.legende;
    while (L.firstChild) L.removeChild(L.firstChild);
    var titre = mode === "valeur" ? c.titre + (c.unite ? " (" + c.unite + ")" : "")
      : mode === "ecart" ? "Écart à la normale " + this.meta.reference + (c.unite ? " (" + c.unite + ")" : "")
      : "Tendance par décennie" + (c.unite ? " (" + c.unite + ")" : "") + " — les mailles pâles ne sont pas significatives";
    var barre = el("div", { classe: "cf-carto__degrade", role: "img", "aria-label": "Degradé de couleurs de " + formater(dom[0], c.decimales) + " à " + formater(dom[1], c.decimales) });
    barre.style.background = "linear-gradient(90deg," + cols.join(",") + ")";
    var signe = mode !== "valeur";
    L.appendChild(el("div", { classe: "cf-carto__legende-titre", texte: titre }));
    L.appendChild(barre);
    L.appendChild(el("div", { classe: "cf-carto__legende-bornes" }, [
      el("span", { texte: formater(dom[0], c.decimales, signe) }),
      signe ? el("span", { texte: "0" }) : null,
      el("span", { texte: formater(dom[1], c.decimales, signe) + (mode === "valeur" ? "" : "") })
    ]));
    L.appendChild(el("div", { classe: "cf-carto__legende-note",
      texte: "Les couleurs sont fixes pour toutes les années : deux années se comparent d'un coup d'œil." }));
  };

  Carte.prototype.majMethode = function () {
    var c = this.etat.couche, M = this.dom.methode;
    while (M.firstChild) M.removeChild(M.firstChild);
    [["Méthode", c.methode], ["Lecture", c.lecture], ["À savoir", c.note]].forEach(function (p) {
      if (p[1]) M.appendChild(el("p", {}, [el("strong", { texte: p[0] + " : " }), p[1]]));
    });
  };

  /* ---- Maille choisie ----------------------------------------------------------- */

  Carte.prototype.selectionner = function (i) {
    if (!(i >= 0 && i < this.meta.n)) return;
    this.etat.sel = i;
    this.map.setFilter("selection", ["==", ["get", "id"], i]);
    if (this.etat.station >= 0) this.deselectionnerStation();
    this.majInfo();
  };

  Carte.prototype.serie = function (i) {
    var d = this.donnees, n = this.meta.n, an = this.meta.annees, out = [];
    for (var k = 0; k < an.length; k++) {
      var v = d.valeurs[k * n + i];
      out.push(v === MANQUANT ? NaN : v / d.echelle);
    }
    return out;
  };

  Carte.prototype.courbe = function (serie, normale, k) {
    var W = 240, H = 56, P = 4, NS = "http://www.w3.org/2000/svg";
    var ok = serie.filter(isFinite);
    if (ok.length < 2) return null;
    var lo = Math.min.apply(null, ok.concat(isFinite(normale) ? [normale] : [])),
        hi = Math.max.apply(null, ok.concat(isFinite(normale) ? [normale] : []));
    if (hi === lo) { hi += 1; lo -= 1; }
    var X = function (j) { return P + (W - 2 * P) * j / (serie.length - 1); };
    var Y = function (v) { return H - P - (H - 2 * P) * (v - lo) / (hi - lo); };
    var svg = document.createElementNS(NS, "svg");
    svg.setAttribute("viewBox", "0 0 " + W + " " + H); svg.setAttribute("class", "cf-carto__courbe");
    svg.setAttribute("role", "img");
    svg.setAttribute("aria-label", "Courbe de " + this.meta.annees[0] + " à " + this.meta.annees[this.meta.annees.length - 1] +
      ", de " + formater(Math.min.apply(null, ok), this.etat.couche.decimales) + " à " + formater(Math.max.apply(null, ok), this.etat.couche.decimales));
    if (isFinite(normale)) {
      var l = document.createElementNS(NS, "line");
      l.setAttribute("x1", P); l.setAttribute("x2", W - P); l.setAttribute("y1", Y(normale)); l.setAttribute("y2", Y(normale));
      l.setAttribute("class", "cf-carto__courbe-normale"); svg.appendChild(l);
    }
    var pts = [];
    serie.forEach(function (v, j) { if (isFinite(v)) pts.push(X(j).toFixed(1) + "," + Y(v).toFixed(1)); });
    var pl = document.createElementNS(NS, "polyline");
    pl.setAttribute("points", pts.join(" ")); pl.setAttribute("class", "cf-carto__courbe-serie"); svg.appendChild(pl);
    if (isFinite(serie[k])) {
      var pt = document.createElementNS(NS, "circle");
      pt.setAttribute("cx", X(k)); pt.setAttribute("cy", Y(serie[k])); pt.setAttribute("r", 3.5);
      pt.setAttribute("class", "cf-carto__courbe-point"); svg.appendChild(pt);
    }
    return svg;
  };

  function ligneInfo(lib, val) { return el("div", { classe: "cf-carto__ligne" }, [el("dt", { texte: lib }), el("dd", { texte: val })]); }

  Carte.prototype.majInfo = function () {
    if (this.etat.station >= 0 && this.stations) return this.infoStation();
    var I = this.dom.info, i = this.etat.sel, c = this.etat.couche, d = this.donnees;
    while (I.firstChild) I.removeChild(I.firstChild);
    if (i < 0 || !c || !d) {
      I.appendChild(el("p", { classe: "cf-mention", texte: "Cliquez sur une maille pour lire ses valeurs." }));
      return;
    }
    var an = this.meta.annees[this.etat.k], v = d.valeurs[this.etat.k * this.meta.n + i];
    var valeur = v === MANQUANT ? NaN : v / d.echelle, normale = d.normale[i], pente = d.pente[i];
    var ligne = ligneInfo;
    I.appendChild(el("h3", { classe: "cf-carto__info-titre", texte: "Maille " + formater(this.grille.cy[i], 2) + " °N, " + formater(this.grille.cx[i], 2) + " °E" }));
    var dl = el("dl", { classe: "cf-carto__valeurs" });
    dl.appendChild(ligne(String(an), isFinite(valeur) ? avecUnite(valeur, c) : "pas de valeur"));
    dl.appendChild(ligne("Normale " + this.meta.reference, isFinite(normale) ? avecUnite(normale, c) : "n.d."));
    dl.appendChild(ligne("Écart", isFinite(valeur) && isFinite(normale) ? avecUnite(valeur - normale, c, true) : "n.d."));
    dl.appendChild(ligne("Tendance", isFinite(pente)
      ? avecUnite(pente, c, true) + " par décennie (" + (d.signif[i] ? "significative" : "non significative") + ")" : "non estimée"));
    I.appendChild(dl);
    var cb = this.courbe(this.serie(i), normale, this.etat.k);
    if (cb) I.appendChild(cb);
    var self = this;
    I.appendChild(el("button", { type: "button", classe: "cf-btn cf-btn--principal cf-btn--pleine", texte: "Analyser cette maille" }))
      .addEventListener("click", function () { self.analyser(i); });
  };

  Carte.prototype.analyser = function (i) {
    var detail = { id: i, lon: this.grille.cx[i], lat: this.grille.cy[i] };
    if (window.Shiny && window.Shiny.setInputValue && this.opts.entree) {
      window.Shiny.setInputValue(this.opts.entree, detail, { priority: "event" });
    }
    this.racine.dispatchEvent(new CustomEvent("cf-carte-analyser", { detail: detail }));
  };

  /* ---- Stations Meteo-France ------------------------------------------------------- */
  // Les stations publiees (stations.json) sont posees EN POINTS au-dessus des mailles. Un clic ouvre
  // leur fiche ; « Analyser cette station » envoie son identifiant au serveur, qui retrouve la
  // position et le nom dans sa propre copie du fichier (le navigateur ne dicte que l'identifiant).

  Carte.prototype.chargerStations = function () {
    var self = this, o = this.opts;
    if (!o.stations || !NOM_SUR.test(String(o.stations))) return Promise.resolve(null);
    return lireFichier(o.base + o.stations)
      .then(function (r) { return r.json(); })
      .then(function (j) { return verifierStations(j, o.schema_stations); })
      .then(function (st) { self.stations = st; self.ajouterStations(st); return st; })
      .catch(function (e) {
        // Un jeu publie sans stations (HTTP 404) n'a rien a dire. Tout autre echec est signale.
        if (!/HTTP 404/.test(String(e && e.message))) self.noteStations("Les stations n'ont pas pu être chargées (" + txt(e && e.message, 80) + ").");
        return null;
      });
  };

  Carte.prototype.noteStations = function (t) {
    if (!this.dom.stationsBloc) return;
    this.dom.stationsBloc.hidden = false;
    if (this.dom.stations) this.dom.stations.disabled = true;
    if (this.dom.stationsNote) this.dom.stationsNote.textContent = t;
  };

  Carte.prototype.ajouterStations = function (st) {
    var map = this.map;
    var fc = { type: "FeatureCollection", features: st.stations.map(function (s) {
      return { type: "Feature", id: s.idx, properties: { idx: s.idx, cat: s.cat }, geometry: { type: "Point", coordinates: [s.lon, s.lat] } };
    }) };
    var visible = !this.dom.stations || this.dom.stations.checked;
    map.addSource("stations", { type: "geojson", data: fc });
    map.addLayer({ id: "stations", type: "circle", source: "stations",
      layout: { visibility: visible ? "visible" : "none" },
      paint: { "circle-radius": ["interpolate", ["linear"], ["zoom"], 4, 2.5, 8, 4.5, 12, 7],
               "circle-color": COULEUR_STATION, "circle-stroke-color": CONTOUR_STATION, "circle-stroke-width": EPAISSEUR_STATION } });
    map.addLayer({ id: "stations-sel", type: "circle", source: "stations", filter: ["==", ["get", "idx"], -1],
      layout: { visibility: visible ? "visible" : "none" },
      paint: { "circle-radius": 10, "circle-color": "rgba(0,0,0,0)", "circle-stroke-color": "#000091", "circle-stroke-width": 3 } });
    if (this.dom.stationsBloc) this.dom.stationsBloc.hidden = false;
    if (this.dom.stationsTypes) this.dom.stationsTypes.hidden = false;
    if (this.dom.stationsNote) this.dom.stationsNote.textContent = st.n + " stations (Météo-France, publiées le " + (st.genere_le || "?") + ")";
    // Effectif de chaque type, ecrit a cote de sa case.
    var compte = [0, 0, 0, 0];
    st.stations.forEach(function (s) { compte[s.cat]++; });
    this.typesStation.forEach(function (t) {
      var n = t.parentNode.querySelector("[data-count]");
      if (n) n.textContent = "(" + formater(compte[CATEGORIES[t.getAttribute("data-type")]], 0) + ")";
    });
  };

  // Les types de station affiches : un filtre sur la categorie. Une station « sans rien d'exploitable »
  // (categorie 3, qui ne se publie pas) reste affichee, faute de case pour la masquer.
  Carte.prototype.filtrerStations = function () {
    if (!this.stations) return;
    var actifs = [];
    this.typesStation.forEach(function (t) { if (t.checked) actifs.push(CATEGORIES[t.getAttribute("data-type")]); });
    this.map.setFilter("stations", ["any", ["==", ["get", "cat"], 3],
      ["match", ["get", "cat"], actifs.length ? actifs : [-1], true, false]]);
    var sel = this.etat.station;
    if (sel >= 0) {
      var c = this.stations.stations[sel].cat;
      if (c !== 3 && actifs.indexOf(c) < 0) { this.deselectionnerStation(); this.majInfo(); }
    }
    this.tooltip.style.display = "none";
  };

  Carte.prototype.afficherStations = function (visible) {
    if (!this.stations) return;
    var v = visible ? "visible" : "none";
    this.typesStation.forEach(function (t) { t.disabled = !visible; });
    this.map.setLayoutProperty("stations", "visibility", v);
    this.map.setLayoutProperty("stations-sel", "visibility", v);
    if (!visible && this.etat.station >= 0) { this.deselectionnerStation(); this.majInfo(); }
    if (!visible) this.tooltip.style.display = "none";
  };

  // La station la plus proche (a `rayon` pixels pres) du point de l'ecran, ou null. Une station est
  // un petit disque : sans cette tolerance, un doigt ne la toucherait presque jamais.
  Carte.prototype.stationPres = function (point, rayon) {
    var map = this.map;
    if (!this.stations || !map.getLayer("stations") || map.getLayoutProperty("stations", "visibility") === "none") return null;
    var fs = map.queryRenderedFeatures([[point.x - rayon, point.y - rayon], [point.x + rayon, point.y + rayon]], { layers: ["stations"] });
    var tous = this.stations.stations, meilleure = null, d0 = Infinity;
    fs.forEach(function (f) {
      var s = tous[Number(f.properties.idx)];
      if (!s) return;
      var p = map.project([s.lon, s.lat]), d = Math.pow(p.x - point.x, 2) + Math.pow(p.y - point.y, 2);
      if (d < d0) { d0 = d; meilleure = s; }
    });
    return meilleure;
  };

  Carte.prototype.selectionnerStation = function (i) {
    if (!this.stations || !(i >= 0 && i < this.stations.stations.length)) return;
    this.etat.station = i;
    this.map.setFilter("stations-sel", ["==", ["get", "idx"], i]);
    if (this.etat.sel >= 0) { this.etat.sel = -1; this.map.setFilter("selection", ["==", ["get", "id"], -1]); }
    this.majInfo();
  };

  Carte.prototype.deselectionnerStation = function () {
    this.etat.station = -1;
    if (this.map.getLayer("stations-sel")) this.map.setFilter("stations-sel", ["==", ["get", "idx"], -1]);
  };

  Carte.prototype.infoStation = function () {
    var self = this, I = this.dom.info, s = this.stations.stations[this.etat.station];
    while (I.firstChild) I.removeChild(I.firstChild);
    var pourcent = function (f) { return isFinite(f) ? formater(100 * f, 0) + " %" : "n.d."; };
    var mesures = [s.pluie ? "précipitations" : null, s.temp ? "températures" : null].filter(Boolean).join(" et ");
    var jour = s.derniere ? s.derniere.split("-").reverse().join("/") : "n.d.";
    I.appendChild(el("h3", { classe: "cf-carto__info-titre", texte: s.nom }));
    I.appendChild(el("p", { classe: "cf-mention", texte: "Station Météo-France : une mesure, pas un modèle." }));
    var dl = el("dl", { classe: "cf-carto__valeurs" });
    dl.appendChild(ligneInfo("Département", s.dept || "n.d."));
    dl.appendChild(ligneInfo("Altitude", isFinite(s.alt) ? formater(s.alt, 0) + " m" : "n.d."));
    dl.appendChild(ligneInfo("Mesures", mesures || "aucune exploitable"));
    dl.appendChild(ligneInfo("Dernière mesure", jour));
    var comp = [s.pluie ? "pluie " + pourcent(s.fr_p) : null, s.temp ? "T " + pourcent(s.fr_t) : null].filter(Boolean).join(" · ");
    dl.appendChild(ligneInfo("Complétude (12 mois)", comp || "n.d."));
    var annees = [s.pluie && isFinite(s.n_p) ? "pluie " + formater(s.n_p, 0) : null, s.temp && isFinite(s.n_t) ? "T " + formater(s.n_t, 0) : null].filter(Boolean).join(" · ");
    dl.appendChild(ligneInfo("Années exploitables", annees || "n.d."));
    I.appendChild(dl);
    if (this.opts.analyse_station && this.opts.entree_station) {
      I.appendChild(el("button", { type: "button", classe: "cf-btn cf-btn--principal cf-btn--pleine", texte: "Analyser cette station" }))
        .addEventListener("click", function () { self.analyserStation(s); });
    } else {
      I.appendChild(el("p", { classe: "cf-mention", texte: "Cliquez ailleurs pour revenir aux mailles." }));
    }
  };

  Carte.prototype.analyserStation = function (s) {
    // Le serveur ne retient que l'identifiant : position et nom viennent de sa copie de stations.json.
    var detail = { id: s.id };
    if (window.Shiny && window.Shiny.setInputValue && this.opts.entree_station) {
      window.Shiny.setInputValue(this.opts.entree_station, detail, { priority: "event" });
    }
    this.racine.dispatchEvent(new CustomEvent("cf-carte-analyser-station", { detail: { id: s.id, lon: s.lon, lat: s.lat, nom: s.nom } }));
  };

  /* ---- Alternative au clavier : les mailles extremes --------------------------------- */

  Carte.prototype.remplirListe = function () {
    var self = this, L = this.dom.liste, n = this.meta.n, xs = this.xs, c = this.etat.couche;
    if (!L || !c) return;
    while (L.firstChild) L.removeChild(L.firstChild);
    var idx = [];
    for (var i = 0; i < n; i++) if (isFinite(xs[i])) idx.push(i);
    idx.sort(function (a, b) { return xs[b] - xs[a]; });
    var table = function (legende, liste) {
      var tb = el("tbody");
      liste.forEach(function (i) {
        var b = el("button", { type: "button", classe: "cf-btn cf-btn--sec", texte: "Analyser" });
        b.addEventListener("click", function () { self.selectionner(i); self.analyser(i); });
        tb.appendChild(el("tr", {}, [
          el("td", { texte: formater(self.grille.cy[i], 2) + " °N, " + formater(self.grille.cx[i], 2) + " °E" }),
          el("td", { texte: self.libelleValeur(xs[i]) }), el("td", {}, [b])]));
      });
      return el("table", { classe: "cf-tableau" }, [el("caption", { texte: legende }),
        el("thead", {}, [el("tr", {}, [el("th", { scope: "col", texte: "Maille" }), el("th", { scope: "col", texte: "Valeur" }), el("th", { scope: "col", texte: "Action" })])]), tb]);
    };
    if (!idx.length) { L.appendChild(el("p", { texte: "Aucune valeur à afficher." })); return; }
    L.appendChild(table("Les 10 valeurs les plus élevées", idx.slice(0, 10)));
    L.appendChild(table("Les 10 valeurs les plus basses", idx.slice(-10).reverse()));
  };

  /* ---------- Demarrage ------------------------------------------------------------ */

  var chargeurMapLibre = null;
  function chargerMapLibre() {
    if (window.maplibregl) return Promise.resolve(window.maplibregl);
    if (chargeurMapLibre) return chargeurMapLibre;
    chargeurMapLibre = new Promise(function (ok, ko) {
      var css = el("link", { rel: "stylesheet", href: "carte/lib/maplibre-gl.css" });
      document.head.appendChild(css);
      var s = el("script", { src: "carte/lib/maplibre-gl.js" });
      s.onload = function () { window.maplibregl ? ok(window.maplibregl) : ko(new Error("MapLibre indisponible")); };
      s.onerror = function () { ko(new Error("MapLibre n'a pas pu être chargé")); };
      document.head.appendChild(s);
    });
    return chargeurMapLibre;
  }

  var instances = {};

  function demarrer(opts) {
    var racine = document.getElementById(opts.racine);
    if (!racine) return Promise.reject(new Error("racine introuvable"));
    if (instances[opts.racine]) return instances[opts.racine];
    var base = String(opts.base || "");
    if (base && base.charAt(base.length - 1) !== "/") base += "/";
    opts = Object.assign({}, opts, { base: base });
    var message = function (t, e) {
      var m = racine.querySelector('[data-role="etat"]');
      racine.hidden = false;
      if (m) { m.textContent = t; m.classList.toggle("cf-carto__etat--erreur", !!e); }
    };
    instances[opts.racine] = Promise.all([
      chargerMapLibre(),
      lireFichier(base + "meta.json").then(function (r) { return r.json(); }).then(function (m) { return verifierMeta(m, opts.schema); })
    ]).then(function (r) {
      var meta = r[1];
      return lireFichier(base + meta.grille).then(function (g) { return g.json(); }).then(function (g) {
        var grille = verifierGrille(g, meta.n);
        racine.hidden = false;
        var carte = new Carte(racine, opts, meta, grille, r[0]);
        carte.donneesGrille = g;
        return carte.prete.then(function () {
          carte.chargerStations();      // sans attendre : une carte sans stations reste une carte
          return carte.choisirCouche(meta.couches[0].cle).then(function () { return carte; });
        });
      });
    }).catch(function (e) {
      message("La carte n'a pas pu être chargée : " + txt(e && e.message, 200), true);
      throw e;
    });
    instances[opts.racine].catch(function () { /* deja signale a l'utilisateur */ });
    return instances[opts.racine];
  }

  window.CarteClimaFor = {
    demarrer: demarrer,
    // Promesse du composant d'une racine (ou null) : point d'entree des tests de bout en bout.
    instance: function (racine) { return instances[racine] || null; },
    _interne: { verifierMeta: verifierMeta, decoderCouche: decoderCouche, verifierGrille: verifierGrille,
                verifierStations: verifierStations, nomLisible: nomLisible,
                NOM_SUR: NOM_SUR, SEQ: SEQ, DIV: DIV, MANQUANT: MANQUANT }
  };

  // Raccord avec Shiny. Selon l'ordre de chargement des scripts, Shiny peut ne pas
  // exister encore quand ce fichier s'execute : on reessaie a la fin du chargement de
  // la page, puis a son ouverture complete. Le message n'arrive qu'a la premiere
  // ouverture de l'onglet, bien apres ; il suffit donc d'etre enregistre avant.
  var enregistre = false;
  function enregistrer() {
    if (enregistre || !(window.Shiny && window.Shiny.addCustomMessageHandler)) return;
    window.Shiny.addCustomMessageHandler("cf-carte-init", function (opts) { demarrer(opts); });
    enregistre = true;
  }
  enregistrer();
  if (!enregistre) {
    document.addEventListener("DOMContentLoaded", enregistrer);
    window.addEventListener("load", enregistrer);
  }
})();
