mapPat troubleshooting guide
----------------------------

| mapPat strongly relies on R, RStudio and R packages installations to correctly function.
| Here we list a collection of useful resources to resolve the most common issues encountered while installing mapPat requirements:

+ **Download and install R**
	| The complete guide to download and install R can be found `here <https://cran.mirror.garr.it/CRAN/>`_.

+ **Dowload and install RStudio**
	| The complete guide to download and install RStudio can be found `here <https://posit.co/download/rstudio-desktop/>`_.

+ **Setting a CRAN mirror in R**
	| mapPat is programmed to install required R packages automatically any time they are not already available on the user device. However, in some cases, it could be necessary to manually set a CRAN mirror from which packages will be installed. The suggested CRAN mirror for mapPat is ``https://cloud.r-project.org``.
	| The complete guide to set a CRAN mirror in R is available `here <https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages>`_.

+ **Setting a CRAN mirror in RStudio**
	| mapPat is programmed to install required R packages automatically any time they are not already available on the user device. However, in some cases, it could be necessary to manually set a CRAN mirror from which packages will be installed. The suggested CRAN mirror for mapPat is ``https://cloud.r-project.org``.
	| The complete guide to set a CRAN mirror in RStudio is available `here <https://docs.posit.co/ide/user/ide/guide/environments/r/packages.html>`_.

+ **Installing sf (for MAC OSX users)**
	| Some packages used by mapPat present ``sf`` among their dependencies, which installation may require the ``gdal`` and ``udunits`` system packages to be present on the user device.
	| The complete guide to install ``sf`` is available `here <https://r-spatial.github.io/sf/#installing>`_
