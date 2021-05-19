---
title: "Especificacions tècniques"
author: "Mireia Banqué, Víctor Granda"
date: "18/05/2021"
output: 
  html_document: 
    keep_md: yes
---



### Introducció

L'aplicació DEBOSCAT App neix de la mà del projecte de seguiment de l'Estat dels Boscos de Catalunya DEBOSCAT,
impulsat des del 2012[^1] per la Direcció General d'Ecosistemes Forestals i Gestió del Medi. Aquesta app ofereix
informació anual sobre els episodis de decaïment forestal registrats a Catalunya en relació a la sequera. El
seguiment al llarg dels anys ens ha de permetre conèixer quines zones i quines espècies són més vulnerables al
canvi climàtic,  conèixer quins són els principals factors desencadenants de l’afectació i la capacitat de
recuperació de la vegetació arbòria.

### Metodologia

Les dades s'han recollit anualment des del 2012 fins l'actualitat durant el mes de setembre de cada any, just
després de l'època típicament més seca: l'estiu. El responsable de dur a terme la campanya de camp és el Cos
d'Agents Rurals (CAR) de Catalunya, que permet que es pugui treballar a escala local. Els Agents de cada comarca
fan una prospecció exhaustiva dels seus boscos i introdueixen la informació en una aplicació online.  
La metodologia consisteix en detectar, delimitar i registrar les zones arbrades (s’exclouen les superfície
forestals no arbrades com els matollars) que hagin estat afectades per decaïment.  S’estableix com a zona
afectada qualsevol unitat forestal de, com a mínim, 3 hectàrees on alguna de les espècies forestals presenti
símptomes de decaïment. Cal registrar l'episodi sempre que el percentatge d’arbres morts sigui, com a mínim
del 5%, o el percentatge de defoliació (pèrdua de fulles respecte les que podria tenir el mateix arbre si
estigués completament sa) i/o decoloració (fulles que no són verdes) sigui com a mínim del 50%. Les zones
afectades es delimiten en un mapa seguint criteris d’homogeneïtat topogràfica, de la composició de la vegetació,
dels efectes observats i de les causes de l’episodi.  
Es registren aquells episodis on hi hagi com a mínim una espècie afectada que compleixi els criteris anteriors.
També hi pot haver altres espècies amb afectacions menors que també es registren. No obstant, les espècies que
no estan afectades no es mostren a la app.

### Cicatritzacions

El seguiment dels episodis registrats es fa, com a mínim, durant 3 anys consecutius. Si durant aquest temps
l'afectació de l'episodi millora o es manté igual es considera que l'episodi està cicatritzat i per tant no
cal seguir registrant-lo a l'aplicatiu. Això explica que els episodis poden deixar d'aparèixer. Si passat un
temps es torna a detectar un empitjorament de l'estat de l'episodi, es reobre amb el mateix codi, de manera
que pot reaparèixer.

### Organització de les dades i variables de sortida

#### Explorador de comarques

Les dades poden ser consultats per tots els episodis, episodis nous (aquells que apareixen per primera
vegada l'any seleccionat) i epsodios antics (aquells que ja són presents en anys previs a l'
seleccionat). Així mateix, les dades també poden ser desglossats per espècies.

  > Quan es resumeix per comarques, les dades han de ser normalitzats pel que fa a la cobertura de cada
   espècie i calculats pel que fa a l'àrea original de l'episodi (ja que algunes espècies poden
   compartir el mateix episodi). Això vol dir que si tots els valors per a cada espècie i comarca
   (quan el desglossat per espècies està actiu) són sumats, els valors no seran els mateixos que els
   mostrats en el resum de comarques sense desglossat.

Independentment del desglossament i el tipus d'episodis seleccionats, les variables disponibles són les
següents:

1. Nombre d'episodis [recompte]: Episodis registrats per a l'any seleccionat a cada comarca (i espècie,
  si el desglossat està actiu).

1. Àrea total de l'episodi [ha]: Suma de l'àrea total dels episodis delimitats pels Agents Rurals per
  cada comarca (i espècie, si el desglossat està actiu).
  
1. Àrea total de coberta dels arbres [ha]: Suma de l'àrea total coberta per les espècies afectades dels
  episodis en cada comarca (i espècie, si el desglossat està actiu).
  
1. Àrea total afectada [ha]: Suma total de l'àrea afectada en els episodis de cada comarca (i espècie, si el
  desglossat està actiu).

1. Àrea total descolorida [ha]: Suma total de l'àrea descolorida en els episodis de cada comarca (i espècie,
  si el desglossat està actiu). Només disonible des 2014 en endavant.
  
1. Àrea total defoliada [ha]: Suma total de l'àrea defoliada en els episodis de cada comarca (i espècie,
  si el desglossat està actiu). Només disonible des 2014 en endavant.
  
1. Àrea total amb mortalitat [ha]: Suma total de l'àrea amb moratalidad present en els episodis de cada
  comarca (i espècie, si el desglossat està actiu). Només disonible des 2014 en endavant.

La següent figura mostra una representació visual de les variables, a) sense desglossament per espècies, b) amb
desglossament per espècies:


<img src="images/tech_specs_figure_1.png" width="829" style="display: block; margin: auto;" />

#### Explorador d'episodis

L'explorador d'episodis ofereix les dades de cada combinació d'episodi individual, any i espècie. Les
següents espècies estan disponibles:

1. ID d'episodi: Identificador únic de cada episodi.

1. Any: Any.

1. Espècie: Nom de l'espècie.

1. Comarca: Nom de la comarca.

1. ID de comarca: Identificador únic de comarca.

1. Cobertura [%]: Percentatge de l'episodi cobert per l'espècie.

1. Arbres afectats [%]: Percentatge d'arbres afectats.

1. Mortalitat [%]: Percentatge d'arbres morts.

1. Defoliació [%]: Percentatge d'arbres defoliats.

1. Decoloració [%]: Percentatge d'arbres descolorits.

1. Àrea de l'episodi [ha]: Àrea total de l'episodi.

1. Episodi nou [lògic]: True si l'episodi és nou en aquest any, False si l'episodi ja existia prèviament.

1. Distribució arbres afectats [text]: Descripció de la distribució arbòria a l'àrea afectada.

1. Afectació general de l'episodi [%]: Afectació general de l'episodi, calculada com:
  $$
  \frac{\sum_{sp=1}^{sp} (cover_{sp}*affectation_{sp})}{\sum_{sp=1}^{sp} cover_{sp}}
  $$





[^1]: Les primeres campanyes van ser el 2010 i el 2011 i van servir per testar i posar a punt la metodologia.
