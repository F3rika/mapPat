inputs <- commandArgs(trailingOnly=TRUE)

countsTable <- read.table(inputs,
                          sep = "\t",
                          header = T,
                          check.names = F,
                          comment.char = "",
                          quote = "",
                          fileEncoding = "UTF-8")

findMax <- apply(countsTable[,3:4], 1, which.max)
admMax <- ifelse(findMax==1, "ADM1", "ADM2")

outTable <- data.frame(Country=countsTable$Country, Country_ADM=admMax)

write.table(outTable,
            "CountryADM_AssocTab.txt",
            sep = "\t",
            col.names = T,
            row.names = F,
            quote = F,
            fileEncoding = "UTF-8")
