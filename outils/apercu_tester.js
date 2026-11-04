// Teste l'apercu de l'onglet Carte dans un vrai Chromium (rendu WebGL logiciel) :
//   node outils/apercu_tester.js apercu_carte.html [adresse_des_fichiers]
// Affiche l'etat, les erreurs, le nombre de mailles dessinees, le diagnostic de la page et une
// capture (JPEG en base64 entre deux reperes) pour la lire dans un journal.
const { chromium } = require('playwright');
const [,, fichier, base] = process.argv;
(async () => {
  const b = await chromium.launch({ args: ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader', '--ignore-gpu-blocklist'] });
  const page = await (await b.newContext({ viewport: { width: 1360, height: 900 } })).newPage();
  const erreurs = [], echecs = [];
  page.on('pageerror', e => erreurs.push('pageerror: ' + e.message));
  page.on('console', m => { if (m.type() === 'error') erreurs.push('console: ' + m.text().slice(0, 240)); });
  page.on('requestfailed', r => echecs.push(r.url().slice(0, 120) + ' : ' + (r.failure() || {}).errorText));
  page.on('response', r => { if (r.status() >= 400) echecs.push(r.status() + ' ' + r.url().slice(0, 120)); });
  await page.goto('file://' + require('path').resolve(fichier) + (base ? '?base=' + encodeURIComponent(base) : ''));
  await page.waitForFunction(() => { const r = document.getElementById('carte-racine'); return r && !r.hidden; }, null, { timeout: 60000 }).catch(() => {});
  await page.waitForTimeout(12000);
  const mesure = await page.evaluate(async () => {
    const out = {};
    out.etat = (document.querySelector('[data-role="etat"]') || {}).textContent;
    out.indicateurs = document.querySelectorAll('#carte-couche option').length;
    out.annee = document.querySelector('[data-role="annee-sortie"]').textContent;
    try {
      const c = await window.CarteClimaFor.instance('carte-racine');
      out.couche = c.etat.couche.cle;
      out.rendues = c.map.queryRenderedFeatures({ layers: ['cellules'] }).length;
      const xs = c.xs; let nb = 0, mn = Infinity, mx = -Infinity;
      for (let i = 0; i < xs.length; i++) { const v = xs[i]; if (isFinite(v)) { nb++; if (v < mn) mn = v; if (v > mx) mx = v; } }
      out.valeurs = { finies: nb, sur: xs.length, min: mn, max: mx };
      out.domaine = c.etat.couche.domaine || null;
      out.emprise = c.grille.emprise;
      const id0 = 4000; out.etat_maille_4000 = c.map.getFeatureState({ source: 'cellules', id: id0 });
    } catch (e) { out.erreur_instance = String(e && e.message); }
    out.diagnostic = document.querySelector('[data-ap="diag-texte"]').textContent;
    return out;
  });
  console.log('MESURE ' + JSON.stringify(mesure, null, 1));
  console.log('ERREURS ' + JSON.stringify(erreurs.filter(e => !/geopf|tile|ERR_TUNNEL|Failed to load resource/i.test(e)).slice(0, 12)));
  console.log('REQUETES EN ECHEC ' + JSON.stringify(echecs.filter(e => !/geopf/.test(e)).slice(0, 12)));
  const jpg = await page.screenshot({ type: 'jpeg', quality: 55 });
  console.log('JPEG_DEBUT'); console.log(jpg.toString('base64')); console.log('JPEG_FIN');
  await b.close();
})();
