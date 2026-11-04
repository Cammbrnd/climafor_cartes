# -*- coding: utf-8 -*-
"""
outils/construire_modele_docx.py — FABRIQUE DU MODELE WORD DE REFERENCE
==============================================================================
Produit rapport/modele_dsfr.docx, le document que pandoc utilise comme
`reference_docx` pour la sortie Word du rapport.

    python3 outils/construire_modele_docx.py

POURQUOI UN SCRIPT PLUTOT QU'UN FICHIER BINAIRE POSE LA
-------------------------------------------------------
Un .docx est un binaire : versionne tel quel, il ne se relit pas, ne se compare
pas d'une version a l'autre, et personne ne peut dire six mois plus tard
pourquoi telle couleur a ete choisie. Le script, lui, est du texte : il se lit,
se commente, se rejoue. Le .docx qu'il produit est un artefact, au meme titre
qu'un binaire compile.

COMMENT PANDOC UTILISE CE FICHIER
---------------------------------
Il n'en reprend PAS le contenu, seulement :
  - les definitions de styles (Normal, Heading 1..6, Title, Caption...) ;
  - l'en-tete et le pied de page, recopies tels quels ;
  - la mise en page de la section (marges, format).
Les paragraphes de demonstration en fin de script ne servent qu'a forcer la
creation des styles dans le fichier ; ils n'apparaitront pas dans le rapport.
Les styles « Table » (tableaux) et « Compact » (cellules) sont ceux que pandoc
ecrit : ils DOIVENT exister dans le modele (voir plus bas).

MARIANNE — LIMITE A CONNAITRE
-----------------------------
Word n'embarque pas les polices. Marianne doit etre INSTALLEE sur le poste qui
ouvre le document, sans quoi Word substituera silencieusement une police
approchante. La substitution ne casse rien, mais le document n'est alors plus a
la charte. Marianne est libre d'usage pour les administrations et se telecharge
depuis le site du systeme de design de l'Etat.
"""

import os

from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Cm, Pt, RGBColor

# -- Charte -------------------------------------------------------------------
BLEU   = RGBColor(0x00, 0x00, 0x91)   # bleu France
TEXTE  = RGBColor(0x1E, 0x1E, 0x1E)
GRIS   = RGBColor(0x5A, 0x5A, 0x5A)
POLICE = "Marianne"

RACINE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
IMAGES = os.path.join(RACINE, "rapport", "images")
SORTIE = os.path.join(RACINE, "rapport", "modele_dsfr.docx")


def fixer_police(style, nom=POLICE, taille=None, couleur=None, gras=None):
    """Applique une police a un style.

    python-docx n'ecrit que l'attribut `ascii`. Word a besoin en plus de
    `hAnsi`, `cs` et `eastAsia` pour ne pas retomber sur sa police par defaut
    des qu'un caractere sort de l'ASCII — ce qui, en francais, arrive des le
    premier accent.
    """
    police = style.font
    police.name = nom
    if taille is not None:
        police.size = Pt(taille)
    if couleur is not None:
        police.color.rgb = couleur
    if gras is not None:
        police.bold = gras

    rpr = style.element.get_or_add_rPr()
    rfonts = rpr.find(qn("w:rFonts"))
    if rfonts is None:
        rfonts = OxmlElement("w:rFonts")
        rpr.append(rfonts)
    for attribut in ("w:ascii", "w:hAnsi", "w:cs", "w:eastAsia"):
        rfonts.set(qn(attribut), nom)


def bordure_basse(paragraphe, couleur="000091", epaisseur=12):
    """Filet sous un paragraphe — rappel discret du liseré institutionnel."""
    ppr = paragraphe._p.get_or_add_pPr()
    bordures = OxmlElement("w:pBdr")
    bas = OxmlElement("w:bottom")
    bas.set(qn("w:val"), "single")
    bas.set(qn("w:sz"), str(epaisseur))
    bas.set(qn("w:space"), "4")
    bas.set(qn("w:color"), couleur)
    bordures.append(bas)
    ppr.append(bordures)


doc = Document()

# -- Mise en page -------------------------------------------------------------
for section in doc.sections:
    section.top_margin = Cm(2.5)
    section.bottom_margin = Cm(2.0)
    section.left_margin = Cm(2.2)
    section.right_margin = Cm(2.2)
    section.header_distance = Cm(1.0)
    section.footer_distance = Cm(1.0)

# -- Styles -------------------------------------------------------------------
# Normal commande tout le reste : les styles qui ne redefinissent pas leur
# police en heritent.
fixer_police(doc.styles["Normal"], taille=10.5, couleur=TEXTE)
mise_en_forme = doc.styles["Normal"].paragraph_format
mise_en_forme.space_after = Pt(8)
mise_en_forme.line_spacing = 1.15

fixer_police(doc.styles["Title"], taille=26, couleur=BLEU, gras=True)
doc.styles["Title"].paragraph_format.space_after = Pt(4)

for niveau, taille in ((1, 18), (2, 14), (3, 12)):
    style = doc.styles["Heading %d" % niveau]
    fixer_police(style, taille=taille, couleur=BLEU, gras=True)
    style.paragraph_format.space_before = Pt(16 if niveau < 3 else 12)
    style.paragraph_format.space_after = Pt(6)
    # Un titre ne doit jamais rester seul en bas d'une page.
    style.paragraph_format.keep_with_next = True

noms_existants = [s.name for s in doc.styles]
for nom, taille in (("Caption", 9), ("Subtitle", 12)):
    if nom in noms_existants:
        style = doc.styles[nom]
        fixer_police(style, taille=taille, couleur=GRIS)
        style.font.italic = True

# -- Styles utilises par pandoc ----------------------------------------------
# Pandoc ecrit les tableaux avec le style de tableau « Table » et leurs cellules
# avec le style de paragraphe « Compact ». Le gabarit par defaut de python-docx
# ne les definit ni l'un ni l'autre : Word les traitait alors comme du texte
# courant, et LibreOffice empilait les cellules d'un tableau les unes sous les
# autres, une par ligne (un rapport technique passait a plus de 160 pages).
# Ils sont donc definis ici, aux couleurs de la charte.
from docx.enum.style import WD_STYLE_TYPE
from docx.oxml import parse_xml
from docx.oxml.ns import nsdecls

compact = doc.styles.add_style("Compact", WD_STYLE_TYPE.PARAGRAPH)
compact.base_style = doc.styles["Normal"]
compact.paragraph_format.space_before = Pt(2)
compact.paragraph_format.space_after = Pt(2)
compact.paragraph_format.line_spacing = 1.0
compact.quick_style = True

doc.styles.element.append(parse_xml(
    '<w:style %s w:type="table" w:styleId="Table">'
    '<w:name w:val="Table"/><w:basedOn w:val="TableNormal"/>'
    '<w:uiPriority w:val="59"/><w:qFormat/>'
    '<w:tblPr><w:tblInd w:w="0" w:type="dxa"/>'
    '<w:tblBorders>'
    '<w:top w:val="single" w:sz="4" w:space="0" w:color="DDDDDD"/>'
    '<w:bottom w:val="single" w:sz="4" w:space="0" w:color="DDDDDD"/>'
    '<w:insideH w:val="single" w:sz="4" w:space="0" w:color="DDDDDD"/>'
    '</w:tblBorders>'
    '<w:tblCellMar><w:top w:w="40" w:type="dxa"/><w:left w:w="108" w:type="dxa"/>'
    '<w:bottom w:w="40" w:type="dxa"/><w:right w:w="108" w:type="dxa"/></w:tblCellMar>'
    '</w:tblPr>'
    # Ligne d'en-tete : gras, fond gris clair, filet bleu France dessous.
    '<w:tblStylePr w:type="firstRow">'
    '<w:rPr><w:b/></w:rPr>'
    '<w:tcPr><w:tcBorders><w:bottom w:val="single" w:sz="8" w:space="0" w:color="000091"/>'
    '</w:tcBorders><w:shd w:val="clear" w:color="auto" w:fill="F6F6F6"/></w:tcPr>'
    '</w:tblStylePr>'
    '</w:style>' % nsdecls("w")))

# -- En-tete : les deux logos -------------------------------------------------
# Pandoc recopie l'en-tete du modele : les logos se repeteront donc sur CHAQUE
# page, ce qu'un bloc insere dans le corps du document ne permettrait pas.
entete = doc.sections[0].header
paragraphe_logos = entete.paragraphs[0]
paragraphe_logos.alignment = WD_ALIGN_PARAGRAPH.LEFT
serie = paragraphe_logos.add_run()

chemin_rf = os.path.join(IMAGES, "republique_francaise.png")
chemin_dsf = os.path.join(IMAGES, "logo_dsf.png")

if os.path.exists(chemin_rf):
    serie.add_picture(chemin_rf, height=Cm(1.7))
    serie.add_text("    ")
if os.path.exists(chemin_dsf):
    serie.add_picture(chemin_dsf, height=Cm(1.1))
bordure_basse(paragraphe_logos)

# -- Pied de page -------------------------------------------------------------
pied = doc.sections[0].footer.paragraphs[0]
pied.alignment = WD_ALIGN_PARAGRAPH.LEFT
mention = pied.add_run(
    "ClimaFor — DSF, Département de la santé des forêts.  "
    "Données : réanalyse SAFRAN-ISBA, Météo-France.")
mention.font.size = Pt(8)
mention.font.color.rgb = GRIS
mention.font.name = POLICE

# -- Paragraphes de demonstration --------------------------------------------
# Leur seule fonction est de forcer la creation des styles dans le fichier.
doc.add_paragraph("Titre du document", style="Title")
doc.add_paragraph("Titre de niveau 1", style="Heading 1")
doc.add_paragraph("Titre de niveau 2", style="Heading 2")
doc.add_paragraph("Titre de niveau 3", style="Heading 3")
doc.add_paragraph("Corps de texte courant du rapport.")
doc.add_paragraph("Légende de figure", style="Caption")

doc.save(SORTIE)
print("Modèle écrit :", SORTIE)
