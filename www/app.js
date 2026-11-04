/* =============================================================================
   app.js — complément client
   -----------------------------------------------------------------------------
   Sept fonctions que Shiny ne peut pas assurer côté serveur :
     1. la géolocalisation, qui est une API du navigateur ;
     2. la validation par la touche Entrée dans le champ commune ;
     3. le recalcul des composants qui étaient masqués au moment de leur
        construction (carte Leaflet, curseurs ion.rangeSlider) ;
     4. la remontée de la largeur de fenêtre, qui pilote la version compacte
        des graphiques ;
     5. la détection d'un écran tactile (pointeur grossier), qui retire le zoom
        et le déplacement au doigt des graphiques interactifs ;
     6. la thématique courante du sommaire du bilan, repérée au défilement ;
     7. (v1.4) le partage de l'analyse (copie de l'adresse) et la navigation
        interne : lien « Voir les graphiques » du bloc À retenir, lien vers le
        glossaire de l'onglet Sources.

   POINT TECHNIQUE : POURQUOI LE POINT 3 EST INDISPENSABLE
   ------------------------------------------------------
   Leaflet et ion.rangeSlider mesurent leur conteneur A LA CONSTRUCTION. Un
   composant construit dans un élément masqué (onglet inactif, <details>
   replié, tiroir fermé) mesure une largeur nulle et reste ensuite affiché sur
   quelques pixels. Les deux bibliothèques écoutent en revanche l'évènement
   "resize" de la fenêtre. On émet donc un resize à chaque changement de
   visibilité : ouverture du tiroir, dépliage d'un <details>, changement
   d'onglet. C'est le correctif standard, et il évite tout appel direct aux
   API internes de ces bibliothèques.
   ========================================================================== */

(function () {
  "use strict";


  /* -- Utilitaire : forcer les composants visibles à se remesurer ---------- */
  function remesurer() {
    window.dispatchEvent(new Event("resize"));
  }
  /* Deux passes : une immédiate, une après la transition CSS (240 ms). */
  function remesurerApresTransition() {
    remesurer();
    setTimeout(remesurer, 280);
  }

  /* -- 1. Géolocalisation -------------------------------------------------- */
  Shiny.addCustomMessageHandler("cf_geolocaliser", function (message) {
    var cible = message.cible;

    if (!navigator.geolocation) {
      Shiny.setInputValue(cible, { erreur: true, motif: "non_supporte" },
                          { priority: "event" });
      return;
    }

    navigator.geolocation.getCurrentPosition(
      function (position) {
        Shiny.setInputValue(cible, {
          /* Minimisation : 4 décimales (environ 10 m) suffisent, la maille de
             la grille fait 8 km ; la position exacte ne quitte pas le navigateur. */
          lon: Math.round(position.coords.longitude * 1e4) / 1e4,
          lat: Math.round(position.coords.latitude * 1e4) / 1e4,
          precision: position.coords.accuracy,
          erreur: false
        }, { priority: "event" });
      },
      function (erreur) {
        Shiny.setInputValue(cible, { erreur: true, motif: erreur.message },
                            { priority: "event" });
      },
      /* Une position à quelques centaines de mètres suffit : la maille SAFRAN
         fait 8 km. On privilégie donc la rapidité sur la précision, et on
         accepte une position mise en cache de moins de 5 minutes. */
      { enableHighAccuracy: false, timeout: 12000, maximumAge: 300000 }
    );
  });

  /* -- 2. Validation à la touche Entrée ------------------------------------ */
  $(document).on("keydown", "input[id$='-commune']", function (e) {
    if (e.key === "Enter") {
      e.preventDefault();
      /* On transmet la valeur du champ avec l'évènement : Shiny n'envoie le
         texte saisi qu'après 250 ms d'inactivité, et une validation par Entrée
         (ou « OK » du clavier tactile) plus rapide recherchait l'ancienne valeur. */
      Shiny.setInputValue(this.id.replace(/-commune$/, "-commune_entree"),
                          { valeur: this.value, t: Date.now() },
                          { priority: "event" });
    }
  });

  /* -- 3. Recalcul des composants redevenus visibles ----------------------- */
  /* Dépliage d'un <details> : curseurs des réglages avancés. */
  document.addEventListener("toggle", function (e) {
    if (e.target && e.target.tagName === "DETAILS" && e.target.open) {
      remesurer();
    }
  }, true);

  /* Changement d'onglet, à tous les niveaux (onglets principaux et onglets
     internes d'un panneau). C'est le cas le plus fréquent depuis que les réglages
     vivent dans l'onglet Accueil : la carte Leaflet et le curseur des années
     n'existent QUE dans un panneau qui peut être inactif au moment de leur
     construction. Bootstrap 3 émet shown.bs.tab ; Shiny émet en outre
     shiny:visualchange sur les sorties concernées. On couvre les deux. */
  $(document).on("shown.bs.tab", remesurerApresTransition);

  /* -- 4. Largeur de fenêtre ----------------------------------------------- */
  /* Remontée à l'ouverture puis à chaque redimensionnement, avec anti-rebond
     pour ne pas saturer la liaison websocket pendant un glissement. */
  var minuteur = null;

  function transmettreLargeur() {
    Shiny.setInputValue("cf_largeur", window.innerWidth);
  }

  /* -- 5. Appareil tactile --------------------------------------------------- */
  /* « pointer: coarse » : le pointeur principal est un doigt. Un écran tactile
     ET une souris (portable hybride) répond « fine » : on garde alors le zoom. */
  function transmettreTactile() {
    var tactile = window.matchMedia && window.matchMedia("(pointer: coarse)").matches;
    Shiny.setInputValue("cf_tactile", !!tactile);
  }

  $(document).on("shiny:connected", function () {
    transmettreLargeur();
    transmettreTactile();
  });

  window.addEventListener("resize", function () {
    clearTimeout(minuteur);
    minuteur = setTimeout(transmettreLargeur, 250);
  });

  /* -- 6. Sommaire du bilan : thématique courante --------------------------- */
  /* Le sommaire (nav.cf-sommaire) est reconstruit avec les graphiques. À chaque
     reconstruction, on observe les sections .cf-famille et on marque d'un
     aria-current="location" le lien de la dernière section visible dans la
     bande de lecture (sous les barres collantes, jusqu'aux 40 % de la fenêtre). Sans IntersectionObserver, le sommaire reste un
     simple menu d'ancres. */
  var observateur = null;

  function suivreSections() {
    if (observateur) { observateur.disconnect(); observateur = null; }
    var nav = document.querySelector(".cf-sommaire");
    if (!nav || !("IntersectionObserver" in window)) return;

    var liens = {}, ordre = [], visibles = {};
    nav.querySelectorAll("a[data-famille]").forEach(function (a) {
      liens[a.getAttribute("data-famille")] = a;
      ordre.push(a.getAttribute("data-famille"));
    });

    function marquer() {
      /* La dernière section visible dans la bande de lecture : quand deux
         sections s'y côtoient, c'est celle dont le titre vient d'y entrer. */
      var courante = null;
      for (var i = 0; i < ordre.length; i++) {
        if (visibles[ordre[i]]) courante = ordre[i];
      }
      if (courante === null) return;          /* entre deux sections : on garde */
      ordre.forEach(function (f) {
        if (f === courante) liens[f].setAttribute("aria-current", "location");
        else liens[f].removeAttribute("aria-current");
      });
    }

    observateur = new IntersectionObserver(function (entrees) {
      entrees.forEach(function (e) {
        visibles[e.target.id.replace(/^fam-/, "")] = e.isIntersecting;
      });
      marquer();
    }, { rootMargin: "-135px 0px -60% 0px" });

    document.querySelectorAll(".cf-famille").forEach(function (s) {
      observateur.observe(s);
    });
  }

  /* L'évènement est émis sur l'élément de sortie : on lit son id (« bilan-graphiques »). */
  $(document).on("shiny:recalculated", function (e) {
    var id = (e.target && e.target.id) || "";
    if (/-graphiques$/.test(id)) setTimeout(suivreSections, 150);
  });

  /* -- 7. Partage de l'analyse et navigation interne (v1.4) ------------------ */

  /* 7a. Copier le lien. app.R tient l'adresse de la page à jour après chaque
     analyse (updateQueryString) : copier l'adresse, c'est copier l'analyse.
     L'API du presse-papiers exige un contexte sécurisé (https ou localhost) ;
     sinon, on retombe sur la copie par un champ temporaire. La confirmation est
     écrite dans une zone « status » : sans elle, un lecteur d'écran ne saurait
     pas que la copie a eu lieu. */
  function copierTexte(texte) {
    if (navigator.clipboard && window.isSecureContext) {
      return navigator.clipboard.writeText(texte);
    }
    return new Promise(function (ok, ko) {
      var t = document.createElement("textarea");
      t.value = texte;
      t.setAttribute("readonly", "");
      t.style.position = "fixed";
      t.style.opacity = "0";
      document.body.appendChild(t);
      t.select();
      var reussi = false;
      try { reussi = document.execCommand("copy"); } catch (err) { reussi = false; }
      document.body.removeChild(t);
      if (reussi) ok(); else ko();
    });
  }

  $(document).on("click", "[data-copier-lien]", function () {
    var bouton = this;
    var lib = bouton.querySelector(".cf-lien-lib");
    var etat = bouton.parentNode.querySelector("[data-lien-etat]");
    var texteInitial = lib ? lib.textContent : "";

    function annoncer(message, copie) {
      if (etat) etat.textContent = message;
      if (lib) lib.textContent = copie ? "Lien copié" : "Copie impossible";
      bouton.classList.toggle("cf-btn--copie", copie);
      setTimeout(function () {
        if (!document.body.contains(bouton)) return;
        if (lib) lib.textContent = texteInitial;
        bouton.classList.remove("cf-btn--copie");
        if (etat) etat.textContent = "";
      }, 2500);
    }

    copierTexte(window.location.href).then(
      function () { annoncer("Lien copié dans le presse-papiers.", true); },
      function () {
        annoncer("Copie impossible : copiez l'adresse dans la barre du navigateur.", false);
      });
  });

  /* 7b. Défilement jusqu'à une section. Elle peut ne pas exister encore : le
     lien « Voir les graphiques » peut demander une thématique masquée, que le
     serveur affiche d'abord. On réessaie donc pendant deux secondes. Le focus
     passe au titre de la section, pour le clavier et les lecteurs d'écran. */
  function defilerVers(id, essais) {
    var el = document.getElementById(id);
    if (!el) {
      if (essais > 0) setTimeout(function () { defilerVers(id, essais - 1); }, 100);
      return;
    }
    var reduit = window.matchMedia &&
                 window.matchMedia("(prefers-reduced-motion: reduce)").matches;
    el.scrollIntoView({ behavior: reduit ? "auto" : "smooth", block: "start" });
    var titre = el.querySelector("h2, h3") || el;
    if (!titre.hasAttribute("tabindex")) titre.setAttribute("tabindex", "-1");
    try { titre.focus({ preventScroll: true }); } catch (err) { titre.focus(); }
  }

  $(document).on("click", "a[data-ouvrir-famille]", function (e) {
    e.preventDefault();
    var famille = this.getAttribute("data-ouvrir-famille");
    var entree = this.getAttribute("data-entree");
    if (entree && window.Shiny) {
      Shiny.setInputValue(entree, { f: famille, t: Date.now() }, { priority: "event" });
    }
    defilerVers("fam-" + famille, 20);
  });

  /* 7c. Lien vers un point d'un autre onglet (ex. le glossaire de Sources). */
  $(document).on("click", "a[data-onglet]", function (e) {
    e.preventDefault();
    var onglet = this.getAttribute("data-onglet");
    var cible = (this.getAttribute("href") || "").replace(/^#/, "");
    var lien = document.querySelector('a[data-toggle="tab"][data-value="' + onglet + '"]');
    if (lien) $(lien).tab("show");
    if (cible) setTimeout(function () { defilerVers(cible, 10); }, 150);
  });
})();
