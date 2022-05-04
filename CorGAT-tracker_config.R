#Defining path for input files
allLin_inputFilesPath <- "" #insert path to allLin input folder
specialLin_inputFilesPath <- "" #insert path to specialLin input folder
mut_inputFilesPath <- "" #insert path to Mut input folder
config_inputFilePath <- "" #insert path to the Config input folder

#Defining max value for "Weeks range" widget
maxWeek <- ncol(read.table(paste0(allLin_inputFilesPath, "Epiweek.","Italy",".csv"), sep = " ", check.names = F))

#Defining choices for "Country" widget
countryNames <- unique(unlist(strsplit(Sys.glob(paste0(allLin_inputFilesPath, "Epiweek.*.csv")), ".", fixed = T)))

countryNames <- countryNames[countryNames!="csv"&countryNames!=paste0(allLin_inputFilesPath, "Epiweek")]

countryNames <- sort(countryNames)

names(countryNames) <- countryNames

countryList <- as.list(countryNames)

#Defining color palettes
library(RColorBrewer)

colVoc <- read.table(paste0(config_inputFilePath,"paletteVOC.txt"),
                         sep = "\t",
                         row.names = 1,
                         check.names = F,
                         comment.char = "")

randomColors <- brewer.pal(n = 6, name = "Pastel2")

colMoc <- read.table(paste0(config_inputFilePath,"paletteMOC.txt"),
                     sep = "\t",
                     row.names = 1,
                     check.names = F)

mut_randomColors <- brewer.pal(n = 11, name = "Set3")

#Opening Lineage+ to Lineage conversion table
specialLin_to_Lineage <- read.table(paste0(config_inputFilePath,"specialLin_to_Lineage.csv"),
                     sep = "\t",
                     row.names = 1,
                     check.names = F)
