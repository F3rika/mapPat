# Welcome to mapPat!

mapPat is a [Shiny](https://shiny.rstudio.com/) application for the interactive tracking of pathogens’s variants, lineages and mutations in space and time. mapPat facilitates genomic surveillance of pathogen’s by summarising their distribution and evolution through intuitive data visuals.

The dashboard of the app consists of three different tabs: Variants, Lineages and Mutations that provide a rich graphical representation of genomic features and evolution of pathogens. The behaviour of each plot can be customised by the user by adjusting a series of parameters through the control panel at the bottom of the interface. 
Currently, mapPat features data from SARS-CoV-2 and mPox.

A more detailed description of the control panel, customizable parameters and plots included in the dashboard is reported in the [mapPat Manual](https://mappat.readthedocs.io/en/latest/) on Read the Docs.

If you have any inquiries about mapPat please feel free to contact us at matteo.chiara@unimi.it, erika.ferrandi@gmail.com or here on GitHub.

## mapPat quick use guide

Here is a quick guide on how to download and run a local installation of mapPat:

1. Download the repository and unzip it (if needed).
2. Enter the `App` folder of the repository.
3. Run mapPat using R or R Studio.

   >If your operating system is Microsoft Windows or MAC OSX open the `mapPat_app.R` script in R Studio and set the working directory to the `App` folder using the dedicated menu (Session > Set Working Directory > Choose Directory). Then run the `mapPat_app.R` script clicking on the “Run App” button in the top right corner of the “Source” panel.
   >
   >If your operating system is MAC OSX or Linux and you prefer to use a shell environment, run the `mapPat_appShell.sh` script from the `App` folder. Make sure to make the script executable before using it. Then copy the generated url on any browser to correctly visualise mapPat.
   >
   >Please mind that if mapPat is not run from the `App` folder:
   >  - The `mapPat_app.R` file must be updated so that paths pointing to required `.R` files in the `Scripts` folder are correct.
   >  - The `mapPat_config.R` file must be updated so that the `inputs_path` variable at line 44 of the file matches the path to the `Input` folder.

Please, when testing that mapPat is correctly installed and functioning use data from Italy (for SARS-CoV-2) or United States (for mPox).
