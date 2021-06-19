#Defining path for input files
allLin_inputFilesPath <- "" #insert path to allLin input folder
specialLin_inputFilesPath <- "" #insert path to specialLin input folder
mut_inputFilesPath <- "" #insert path to Mut input folder

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

Voc <- c("B.1.1.7", "P.1", "B.1.351", "B.1.617.2", "Others")
colVoc <- c("chartreuse", "cyan", "gold", "darkorchid2", "mediumblue")
names(colVoc) <- Voc

randomColors <- brewer.pal(n = 6, name = "Pastel2")

Moc <- c("N501Y", "E484K", "Others")
colMoc <- c("green", "deeppink", "mediumblue")
names(colMoc) <- Moc

mut_randomColors <- brewer.pal(n = 11, name = "Set3")
