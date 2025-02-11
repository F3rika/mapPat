##################################################################
#      DEFINING ALL THE CONFIGURATION PARAMETERS OF THE APP      #
##################################################################

#######CHECKING REQUIREMENTS#######
#Checking if all required packages are already installed.
requiredPackages <- c("shiny", "RColorBrewer", "ggplot2", "pheatmap", "rgeoboundaries", "leaflet", "htmltools", "remotes", "hoardr")
installedPackages <- rownames(installed.packages())

isInstalled <- requiredPackages%in%installedPackages

#Setting the CRAN mirror from which missing packages are installed.
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org"
  options(repos = r)
})

#Installing missing packages.
if (sum(isInstalled)<length(requiredPackages)) {
  
  toInstall <- requiredPackages[!isInstalled]
  
  if ("rgeoboundaries"%in%toInstall) {
    
    toInstall <- toInstall[!toInstall%in%"rgeoboundaries"]
    
    install.packages(toInstall)
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

#######CHECK DATASET UPDATE#######
#Defining path to Datasets folder.
datasets_path <- "./Datasets/"

#Dowload the most recent copy of the Input Tables Updated Availability configuration File.
download.file("https://raw.githubusercontent.com/F3rika/mapPat/refs/heads/mapPat_Current/App/Datasets/mapPat_inTabUpdates_Availability.txt", destfile = paste0(datasets_path, "mapPat_inTabUpdates_Availability.txt"))

#Opening the Input Tables Updated Availability configuration File.
inTabAvail_Table <- read.table(paste0(datasets_path, "mapPat_inTabUpdates_Availability.txt"),
                               sep = "\t",
                               header = T,
                               check.names = F,
                               comment.char = "",
                               quote = "",
                               fileEncoding = "UTF-8")

#######DEFINING DEFAULT CONFIGURATION#######
#Downloading the default dataset for the App and unzipping it if required.
checkUpd <- list.files(datasets_path)
checkDef<- inTabAvail_Table[1,]$UpdateFile

if (!(checkDef %in% checkUpd)) {
  
  download.file(inTabAvail_Table[1,]$UpdateURL, destfile = paste0(datasets_path, inTabAvail_Table[1,]$UpdateFile))
  
  untar(paste0(datasets_path, inTabAvail_Table[1,]$UpdateFile), exdir = paste0(datasets_path, inTabAvail_Table[1,]$UpdateFolder))
  
}

#Defining paths for input files.
inputs_pathDEF <- paste0(datasets_path, inTabAvail_Table[1,]$UpdatePath)
config_pathDEF <- paste0(inputs_pathDEF,"Config/")
var_pathDEF <- paste0(inputs_pathDEF,"Var/")
allLin_pathDEF <- paste0(inputs_pathDEF,"allLin/")
heatChoromap_pathDEF <- paste0(inputs_pathDEF,"HeatChoromap/")
mut_pathDEF <- paste0(inputs_pathDEF,"Mut/")
totReg_pathDEF <- paste0(inputs_pathDEF,"totReg/")

#Opening the Pathogen Selection Configuration Table. This table allows to associate each pathogen
#with associated information used for data default and widgets definition.
pathogenSelConf_TableDEF <- read.table(paste0(config_pathDEF, "PathogenSelection_ConfigTab.txt"),
                                       sep = "\t",
                                       header = T,
                                       check.names = F,
                                       comment.char = "",
                                       quote = "",
                                       fileEncoding = "UTF-8")

#Opening the Country ISO-ADM Association table. This table allows to associate each country name in the
#dropdown menu to the corresponding ISO-3 codes and ADM level (used for the Choropleth Maps).
countryISOADMConvertion_TableDEF <- read.table(paste0(config_pathDEF, "CountryISOADM_AssocTab.txt"),
                                               sep = "\t",
                                               header = T,
                                               check.names = F,
                                               comment.char = "",
                                               quote = "",
                                               fileEncoding = "UTF-8")

#Opening the Lineage to Variant Tracker Conversion table. This table allows to associate each Lineage to the
#corresponding Variant, Status (according to WHO risk classification) and if it is currently under monitoring
#(VBM).
variantsConvertion_TableDEF <- read.table(paste0(config_pathDEF, "LinVar_ConvTabTracker.txt"),
                                          sep = "\t",
                                          header = T,
                                          check.names = F,
                                          comment.char = "",
                                          quote = "",
                                          fileEncoding = "UTF-8")

#Defining the content of the Dataset selection drop down menu.
datasetNames <- inTabAvail_Table$UpdateName

datasetFile <- inTabAvail_Table$UpdateFolder

names(datasetFile) <- datasetNames

datasetList <- as.list(datasetFile)

#Defining the content of the Pathogen selection drop down menu.
pathogenNames <- pathogenSelConf_TableDEF$PathogenName

pathogenAbbr <- pathogenSelConf_TableDEF$PathogenAbbr

names(pathogenAbbr) <- pathogenNames

pathogenListDEF <- as.list(pathogenAbbr)

#Defining the content of the Variant category selection drop down menu.
status <- unique(variantsConvertion_TableDEF$Status)

status <- append(status, c("VBM", "All"))

status <- status[order(status)]

names(status) <- status

statusListDEF <- as.list(status)

#######DEFINING COLOR PALETTES#######
#Defining the color palettes characterizing the color theme of each tab in the App.
#Variants Tab Theme.
varTheme <- brewer.pal(9, "PiYG")
varThemeHC <- brewer.pal(9, "RdPu")

#Lineages Tab Theme.
allLinTheme <- brewer.pal(9, "PuOr")
allLinThemeHC <- brewer.pal(9, "BuPu")

#Mutations Tab Theme.
mutTheme <- brewer.pal(9, "BrBG")
mutThemeHC <- brewer.pal(9, "YlOrBr")
