.. HaploCoV documentation master file, created by
   sphinx-quickstart on Wed Jul 20 11:58:46 2022.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Tracker Name documentation!
======================================

| Welcome to TrakerName, a Shiny [ref] based dashboard for interactively tracking the distribution of SARS-CoV-2 variants, lineages and mutations of interest through space and time. By doing so TrakerName facilitates the genomic surveillance of SARS-CoV-2 distribution and evolution by reporting the prevalence of its variants, lineages and spike mutations in different countries and time frames.
| The dashboard consists of three different tabs, one for each type of SARS-CoV-2 characteristic of interest (Variants Tab, Lineages Tab and Mutations Tab respectively), that collect a number of graphical representations of the data. The behaviour of each plot can be personalised by the user by adjusting a series of parameters through the control panel at the bottom of the interface.
| A more detailed description of the input requirements, control panel, customizable parameters and plots included in the dashboard is reported in the following sections of this manual.

.. toctree::
   :maxdepth: 2
   :caption: INPUT DATA
   
   inputs.rst

.. toctree::
   :maxdepth: 2
   :caption: DATA REPRESENTATION 
   
   graphIntro.rst
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
   :caption: REQUIREMENTS & APPLICATION
   
   sysReq.rst
   quickGuide.rst