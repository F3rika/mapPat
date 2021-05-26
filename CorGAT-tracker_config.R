#Defining max value for "Weeks range" widget
maxWeek <- 70

#Defining choices for "Country" widget
countryList <- list("Australia"="Australia",
                    "Brazil"="Brazil",
                    "Denmark"="Denmark",
                    "France"="France",
                    "India"="India",
                    "Italy"="Italy",
                    "Spain"="Spain",
                    "Sweden"="Sweden",
                    "The United Kingdom"="UnitedKingdom",
                    "United States of America"="USA"
                    )

#Defining path for input files
inputFilesPath <- "C:/Users/Erika/Desktop/CNR/Projects/CorGAT/ShinyApps/CorGAT_byWeek-App/inputTables/"

#Defining color palette
library(RColorBrewer)

Voc <- c("B.1.1.7", "P.1", "B.1.351", "B.1.427", "B.1.429", "Others")
colVoc <- c("chartreuse", "cyan", "gold", "maroon1", "darkorchid2", "mediumblue")
names(colVoc) <- Voc

randomColors <- brewer.pal(n = 6, name = "Pastel2")
