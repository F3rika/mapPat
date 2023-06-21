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
  
  install.packages(toInstall)
}

#######UPLOAD REQUIRED PACKAGES#######
library(shiny)
library(RColorBrewer)
library(ggplot2)
library(pheatmap)
library(rgeoboundaries)
library(leaflet)
library(htmltools)

#######DEFINIG PATHS FOR INPUT FILES#######
inputs_path <- "../InputData/"
config_path <- paste0(inputs_path,"Config/")
var_path <- paste0(inputs_path,"Var/")
allLin_path <- paste0(inputs_path,"allLin/")
heatChoromap_path <- paste0(inputs_path,"Heatmaps_Choroplethmaps/")
mut_path <- paste0(inputs_path,"Mut/")
totReg_path <- paste0(inputs_path,"totReg/")

#######UNZIPPING INPUT FILES#######
#Unzipping all the input files in the inputData folder.
zippedFiles <- list.files(path = inputs_path,
                          pattern = "*\\.tar\\.gz$")

toUnzip <- sapply(inputs_path, paste0, zippedFiles)

sapply(toUnzip, untar, compressed = TRUE, exdir = inputs_path)

#######OPENING USEFUL FILES#######
#Opening the Countries List Tracker file. This file collects the ISO-3 codes for all the countries
#with a total number of sequenced genomes equal or higher than 1000. By developer choice only
#countries respecting this requirement are considered.
countryPass_List <- scan(paste0(config_path,"countriesListTracker.txt"),
                         what = "character",
                         sep = "\n",
                         comment.char = "",
                         quote = "")

#Opening the Country Regions Converter table. This table allows to associate each country name in the
#dropdown menu to the corresponding ISO-3 codes and regions (used for the Choropleth Maps).
countryRegionsConvertion_Table <- read.table(paste0(config_path, "countryRegionsConverter.txt"),
                                             sep = "\t",
                                             header = T,
                                             check.names = F,
                                             comment.char = "",
                                             quote = "")

#Opening the Variants Converter Tracker table. This table allows to associate each Lineage to the
#corresponding Variant or Variant subgroup.
variantsConvertion_Table <- read.table(paste0(config_path, "variantsConverterTracker.txt"),
                                       sep = "\t",
                                       header = T,
                                       check.names = F,
                                       comment.char = "",
                                       quote = "")
colnames(variantsConvertion_Table) <- c("Lin","Var","Cat","OmiGroup")

#######DEFINING WIDGETS CONTENT#######
#Defining the maximum limit of the week selection slider.
maxWeek <- ncol(read.table(paste0(allLin_path, "Epiweek.","ITA",".csv"),
                           sep = " ",
                           row.names = 1,
                           header = T,
                           check.names = F))

#Defining the content of the country selection dropdown menu. Only countries with a total number
#of sequenced genomes equal or higher than 1000 are considered.
countryNames <- unique(countryRegionsConvertion_Table[countryRegionsConvertion_Table$Country_ISO%in%countryPass_List,]$Country)

countryISO <- unique(countryRegionsConvertion_Table[countryRegionsConvertion_Table$Country_ISO%in%countryPass_List,]$Country_ISO)

names(countryISO) <- countryNames

countryList <- as.list(countryISO)

#Defining the content of the Variant category selection dropdown menù.
cat <- unique(variantsConvertion_Table$Cat)

omiGr <- unique(variantsConvertion_Table$OmiGroup)

omiGr <- omiGr[!omiGr=="None"]

categoryNames <- c(cat, omiGr)

names(categoryNames) <- categoryNames

categoryList <- as.list(categoryNames)

categoryList <- append(categoryList, list(All = "All"))

#######DEFINING COLOR PALETTES#######
#Defining the color palettes characterizing the color theme of each tab in the App.
#Variants Tab Theme.
varTheme <- brewer.pal(9, "YlOrRd")

#Lineages Tab Theme.
allLinTheme <- brewer.pal(9, "YlGnBu")

#Mutations Tab Theme.
mutTheme <- brewer.pal(9, "PuBuGn")
