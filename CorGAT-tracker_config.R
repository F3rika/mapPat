#######UPLOAD REQUIRED PACKAGES#######
library(shiny)
library(RColorBrewer)
library(rgeoboundaries)
library(leaflet)
library(htmltools)

#######DEFINIG PATHS FOR INPUT FILES#######
var_path <- "" #insert path to variants input folder
allLin_path <- "" #insert path to allLin input folder
spLin_path <- "" #insert path to specialLin input folder
mut_path <- "" #insert path to Mut input folder
config_path <- "" #insert path to Config input folder
map_path <- "" #insert path to Map input folder

#######OPENING USEFUL FILES#######
#Opening Lineage+ to Lineage conversion table. This table allows to define the association
#between each Lineage+ and the corresponding "parental" Lineage.
specialLin_to_Lineage <- read.table(paste0(config_path,"specialLin_to_Lineage.csv"),
                                    sep = "\t",
                                    row.names = 1,
                                    check.names = F)

#Opening the Country Converter table. This table allows to associate each country name in the
#dropdown menu to the corresponding spelling in other input files names and to the corresponding
#ISO-3 codes.
countryConvertion_Table <- read.table(paste0(config_path, "countryConverter.txt"),
                                     sep = "\t",
                                     row.names = 1,
                                     check.names = F,
                                     comment.char = "")

#######DEFINING WIDGETS CONTENT#######
#Defining the maximum limit of the week selection slider.
maxWeek <- ncol(read.table(paste0(allLin_path, "Epiweek.","Italy",".csv"), sep = " ", check.names = F))

#Defining the content of the country selection dropdown menu.
countryNames <- row.names(countryConvertion_Table)

names(countryNames) <- countryNames

countryList <- as.list(countryNames)

#######DEFINING COLOR PALETTES#######
#Defining the color palette for VOC and non VOC lineages
#Each Variant of Concern (VOC) is assigned to a specific color
#The correspondence between color and VOC is defined in a specific
#configuration file named paletteVOC.txt
colVoc <- read.table(paste0(config_path, "paletteVOC.txt"),
                     sep = "\t",
                     row.names = 1,
                     check.names = F,
                     comment.char = "")

#All non VOC lineages are assigned to a random color
randomColors <- brewer.pal(n = 6, name = "Pastel2")


#Defining the color palette for MOC and non MOC mutations
#Each Mutation of Concern (MOC) is assigned to a specific color
#The correspondence between color and MOC is defined in a specific
#configuration file named paletteMOC.txt
colMoc <- read.table(paste0(config_path,"paletteMOC.txt"),
                     sep = "\t",
                     row.names = 1,
                     check.names = F)

#All non MOC mutations are assigned to a random color
mut_randomColors <- brewer.pal(n = 11, name = "Set3")


#Defining the color palette for each map in the Map panel.
#The palette consist of ten colors each one defining an interval of
#frequency (%). Colors in the palette are used to fill each region
#of the Country of interest according to the frequency of a lineage of interest
##PENSO A UNA NUOVA SERIE DI COLORI##
map_referencePalette <- c("#F7FCFD", "#E2EDF5", "#C6D8E9", "#A9C3DE", "#94A6CE", "#8C82BC", "#8A5DAA", "#863595", "#7B0D76", "#4D004B")
