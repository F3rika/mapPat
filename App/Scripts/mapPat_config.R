##################################################################
#      DEFINING ALL THE CONFIGURATION PARAMETERS OF THE APP      #
##################################################################

#######CHECKING REQUIREMENTS#######
#Checking if all required packages are already installed.
requiredPackages <- c("shiny", "RColorBrewer", "ggplot2", "pheatmap", "rgeoboundaries", "leaflet", "htmltools")
installedPackages <- rownames(installed.packages())

isInstalled <- requiredPackages%in%installedPackages

#Installing missing packages.
if (sum(isInstalled)<7) {
  
  toInstall <- requiredPackages[!isInstalled]
  
  #Installing "rgeoboundaries" requires some additional steps.
  if ("rgeoboundaries"%in%toInstall) {
    
    toInstall <- toInstall[toInstall!="rgeoboundaries"]
    install.packages(toInstall)
    
    install.packages("remotes")
    remotes::install_github("wmgeolab/rgeoboundaries")
    
  } else {
    
    install.packages(toInstall)
    
  }
}

#######UPLOAD REQUIRED PACKAGES#######
library(shiny)
library(RColorBrewer)
library(ggplot2)
library(pheatmap)
library(rgeoboundaries)
library(leaflet)
library(htmltools)
gb_get_cache(create = TRUE)

#######DEFINING PATHS FOR INPUT FILES#######
inputs_path <- "../Input/"
config_path <- paste0(inputs_path,"Config/")
var_path <- paste0(inputs_path,"Var/")
allLin_path <- paste0(inputs_path,"allLin/")
heatChoromap_path <- paste0(inputs_path,"HeatChoromap/")
mut_path <- paste0(inputs_path,"Mut/")
totReg_path <- paste0(inputs_path,"totReg/")

#######UNZIPPING INPUT FILES#######
#Unzipping all the input files in the inputData folder.
zippedFiles <- list.files(path = inputs_path,
                          pattern = "*\\.tar\\.gz$")

if (length(zippedFiles)>0) {
  
  toUnzip <- sapply(inputs_path, paste0, zippedFiles)
  
  sapply(toUnzip, untar, compressed = TRUE, exdir = inputs_path)
  
}

#######OPENING USEFUL FILES#######
#Opening the Pathogen Selection Configuration Table. This table allows to associate each pathogen
#with associated information used for data default and widgets definition.
pathogenSelConf_Table <- read.table(paste0(config_path, "PathogenSelection_ConfigTab.txt"),
                                    sep = "\t",
                                    header = T,
                                    check.names = F,
                                    comment.char = "",
                                    quote = "",
                                    fileEncoding = "UTF-8")

#Opening the Country ISO-ADM Association table. This table allows to associate each country name in the
#dropdown menu to the corresponding ISO-3 codes and ADM level (used for the Choropleth Maps).
countryISOADMConvertion_Table <- read.table(paste0(config_path, "CountryISOADM_AssocTab.txt"),
                                             sep = "\t",
                                             header = T,
                                             check.names = F,
                                             comment.char = "",
                                             quote = "",
                                             fileEncoding = "UTF-8")

#Opening the Lineage to Variant Tracker Conversion table. This table allows to associate each Lineage to the
#corresponding Variant, Status (according to WHO risk classification) and if it is currently under monitoring
#(VBM).
variantsConvertion_Table <- read.table(paste0(config_path, "LinVar_ConvTabTracker.txt"),
                                       sep = "\t",
                                       header = T,
                                       check.names = F,
                                       comment.char = "",
                                       quote = "",
                                       fileEncoding = "UTF-8")

#######DEFINING WIDGETS CONTENT#######
#Defining the content of the Pathogen selection drop down menu.
pathogenNames <- pathogenSelConf_Table$PathogenName

pathogenAbbr <- pathogenSelConf_Table$PathogenAbbr

names(pathogenAbbr) <- pathogenNames

pathogenList <- as.list(pathogenAbbr)

#Defining the content of the Variant category selection drop down menu.
status <- unique(variantsConvertion_Table$Status)

status <- append(status, c("VBM", "All"))

status <- status[order(status)]

names(status) <- status

statusList <- as.list(status)

#######DEFINING COLOR PALETTES#######
#Defining the color palettes characterizing the color theme of each tab in the App.
#Variants Tab Theme.
varTheme <- brewer.pal(9, "YlOrRd")

#Lineages Tab Theme.
allLinTheme <- brewer.pal(9, "YlGnBu")

#Mutations Tab Theme.
mutTheme <- brewer.pal(9, "PuBuGn")
