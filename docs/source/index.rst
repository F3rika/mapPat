.. HaploCoV documentation master file, created by
   sphinx-quickstart on Wed Jul 20 11:58:46 2022.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to mapPat documentation!
======================================

Welcome to mapPat, a `Shiny <https://shiny.posit.co/>`_ application for the interactive tracking of pathogens’s variants, lineages and mutations in space and time. mapPat facilitates genomic surveillance of pathogen’s by summarising their distribution and evolution through intuitive data visuals.

| The dashboard of the app consists of three different tabs: Variants, Lineages and Mutations that provide a rich graphical representation of genomic features and evolution of pathogens. The behaviour of each plot can be customised by the user by adjusting a series of parameters through the control panel at the bottom of the interface.
| Currently, mapPat features data from SARS-CoV-2 and mPox.

A more detailed description of the control panel, customizable parameters and plots included in the dashboard is reported in the following sections of this manual.

.. toctree::
   :maxdepth: 2
   :caption: DATA REPRESENTATION 
   
   graphIntro.rst
   graphFreqDef.rst
   graphVarTab.rst
   graphLinTab.rst
   graphMutTab.rst

.. toctree::
   :maxdepth: 2
   :caption: DATA CUSTOMISATION
   
   widgIntro.rst
   widgGen.rst
   widgVarTab.rst
   widgLinTab.rst
   widgMutTab.rst

.. toctree::
   :maxdepth: 2
   :caption: HANDS ON & CUSTOMISATION
   
   handsOnPersIntro.rst
   sysReq.rst
   inReqIntro.rst
   inReqCounts.rst
   inReqConfig.rst
   useGuide.rst
   customGuide.rst
