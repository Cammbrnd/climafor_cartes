"""Donnees de apercu_carte.py : COPIES de ce que produisent mod_carte_ui() (HTML de l'onglet) et
cf_icone() (icones des onglets). tests/test_v18.R verifie qu'elles n'ont pas derive de l'application."""

UI_CARTE = r'''<h2 class="cf-titre-section">Carte des indicateurs</h2>
<p class="cf-chapeau">Un indicateur climatique forestier pour chaque maille de 8 km de la grille SAFRAN : sa valeur une année donnée, son écart à la normale ou sa tendance depuis 1959. Cliquez sur une maille pour l'analyser en détail.</p>
<div id="carte-indisponible" class="shiny-html-output"></div>
<div id="carte-racine" class="cf-carto" data-cf-carto="1" hidden="hidden">
  <div class="cf-carto__barre">
    <div class="cf-carto__champ">
      <label for="carte-couche">Indicateur</label>
      <select id="carte-couche" class="cf-champ" data-role="couche"></select>
    </div>
    <fieldset class="cf-carto__modes">
      <legend>Affichage</legend>
      <label class="cf-carto__mode">
        <input type="radio" name="carte-mode" value="valeur" data-role="mode" checked/>
        <span data-mode="valeur">Valeur de l'année</span>
      </label>
      <label class="cf-carto__mode">
        <input type="radio" name="carte-mode" value="ecart" data-role="mode"/>
        <span data-mode="ecart">Écart à la normale</span>
      </label>
      <label class="cf-carto__mode">
        <input type="radio" name="carte-mode" value="tendance" data-role="mode"/>
        <span data-mode="tendance">Tendance</span>
      </label>
    </fieldset>
    <div class="cf-carto__champ">
      <label for="carte-annee">
        Année : 
        <output data-role="annee-sortie"></output>
      </label>
      <div class="cf-carto__annee">
        <input id="carte-annee" type="range" step="1" data-role="annee"/>
        <button type="button" class="cf-btn cf-btn--sec" data-role="lecture">Lecture</button>
      </div>
    </div>
  </div>
  <div class="cf-carto__scene">
    <div class="cf-carto__carte" data-role="carte" role="region" aria-label="Carte des mailles SAFRAN"></div>
    <aside class="cf-carto__info" data-role="info" aria-live="polite">
      <p class="cf-mention">Cliquez sur une maille pour lire ses valeurs.</p>
    </aside>
  </div>
  <div class="cf-carto__legende" data-role="legende"></div>
  <p class="cf-mention" data-role="etat" role="status"></p>
  <details class="cf-details">
    <summary>Lecture et méthode</summary>
    <div class="cf-details__corps" data-role="methode"></div>
  </details>
  <details class="cf-details">
    <summary>Mailles extrêmes (accès au clavier)</summary>
    <div class="cf-details__corps cf-tableau-defile" data-role="liste"></div>
  </details>
</div>'''

ICONES = {
    'accueil': '<svg class="cf-icone" viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M4 11l8-7 8 7"/><path d="M6 10v10h12V10"/><path d="M10 20v-6h4v6"/></svg>',
    'carte': '<svg class="cf-icone" viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M9 4L3 6v14l6-2 6 2 6-2V4l-6 2z"/><path d="M9 4v14M15 6v14"/></svg>',
    'bilan': '<svg class="cf-icone" viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M3 3v18h18"/><path d="M7 15l3.5-4.5 3 3L19 6"/></svg>',
    'rapport': '<svg class="cf-icone" viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M14 3H7a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V8z"/><path d="M14 3v5h5"/><path d="M9 13h6M9 17h4"/></svg>',
    'parametres': '<svg class="cf-icone" viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M4 6h10M18 6h2M4 12h4M12 12h8M4 18h8M16 18h4"/><circle cx="16" cy="6" r="2"/><circle cx="10" cy="12" r="2"/><circle cx="14" cy="18" r="2"/></svg>',
    'sources': '<svg class="cf-icone" viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="M4 5.5A2.5 2.5 0 0 1 6.5 3H19v16H6.5A2.5 2.5 0 0 0 4 21.5z"/><path d="M4 19.5V5.5"/></svg>',
}

ONGLETS = [("accueil", "Accueil"), ("carte", "Carte"), ("bilan", "Bilan climatique"),
           ("rapport", "Rapport"), ("parametres", "Paramètres"), ("sources", "Sources")]
