############################################################
#      DEFINING ALL THE FUNCTIONS REQUIRED BY THE APP      #
############################################################

#######DATA SUBSETTING FUNCTIONS#######
# >>Subsetting input data by frequency and max n. of explicitly represented elements -----
#Defining the function that allows to subset the input data according to the following parameters:
#1. The min frequency (%) calculated on the whole user-selected period of interest.
#2. The max number of elements that can be explicitly represented in a plot.
#This function requires the following inputs:
#1. dataTable -> The input data table of per week counts.
#2. totSequences -> The total number of genomes sequenced in the user-selected time period of
#   interest.
#3. freqThreshold -> The minimum frequency (%) required to an element from the input table
#   to be represented.
#4. maxNum_vizElem <- The maximum number of elements surviving the frequency filter that can be
#   explicitly represented in the final plot.
#The function gives as an output a per weeks count table subsetted by the above mentioned parameters.
freqViz_Subsetter <- function(dataTable, totSequences, freqThreshold, maxNum_vizElem) {
  #Data are ordered in decreasing order according to the frequency (%)
  #of each element.
  dataOrdering <- cbind(dataTable,
                        Freq = rowSums(dataTable)/totSequences)
  
  dataOrdering <- dataOrdering[order(dataOrdering$Freq, decreasing = T),]
  
  #Elements with a frequency (%) above or equal to the selected
  #frequency (%) threshold are stored in the dataPassing variable.
  dataPassing <- dataOrdering[dataOrdering$Freq>=as.numeric(freqThreshold),]
  
  #Elements with a frequency (%) below the selected frequency (%) threshold
  #are collapsed together and stored in the dataNotPassing variable.
  dataNotPassing <- colSums(dataOrdering[dataOrdering$Freq<as.numeric(freqThreshold),])
  
  #If the dataPassing variable (table) contains more than a user-selected number of elements only
  #the first n (defined by the maxNum_vizElem argument) most frequent are explicitly represented.
  #All other elements are collapsed together with the elements of the dataNotPassing variable, which
  #already contains data from elements below the frequency (%) threshold, in a single row called Others.
  if (nrow(dataPassing)>(maxNum_vizElem+1)) {
    
    outputTable <- dataPassing[1:maxNum_vizElem,]
    
    others <- colSums(dataPassing[(maxNum_vizElem+1):nrow(dataPassing),])
    
    others <- others + dataNotPassing
    
    outputTable <- rbind(outputTable,
                         Others = others)
    
  } else if (nrow(dataPassing)==(maxNum_vizElem+1)) {
    
    outputTable <- dataPassing[1:maxNum_vizElem,]
    
    others <- dataPassing[(maxNum_vizElem+1),]
    
    others <- others + dataNotPassing
    
    outputTable <- rbind(outputTable, Others = others)
    
  } else if (nrow(dataPassing)>=1&nrow(dataPassing)<(maxNum_vizElem+1)) {
    
    outputTable <- rbind(dataPassing, Others = dataNotPassing)
    
  } else {
    
    names(dataNotPassing) <- colnames(dataOrdering)
    
    outputTable <- as.data.frame(t(dataNotPassing), row.names = "Others")
  }
  
  #The final table is filtered again for the frequency (%).
  #In fact Others could be excluded from the representation by this filter, even
  #if its frequency (%) is the sum of many elements from the original input table.
  outputTable <- outputTable[outputTable$Freq>=as.numeric(freqThreshold),]
  
  outputTable$Freq <- NULL
  
  return(outputTable)
}


# >>Subsetting the input data by max n. of explicitly represented elements -----
# Defining the function that allows to subset the input data according to the following parameters:
#1. The max number of elements that can be explicitly represented in a plot.
#This function requires the following inputs:
#1. dataTable -> The input datatable.
#2. totSequences -> The total number of genomes sequenced in the user-selected time period of
#   interest.
#3. maxNum_vizElem <- The maximum number of elements that can be explicitly represented
#   in the final plot.
dataViz_Subsetter <- function(dataTable, totSequences, maxNum_vizElem) {
  dataOrdering <- cbind(dataTable,
                        Freq = rowSums(dataTable)/totSequences)
  
  dataOrdering <- dataOrdering[order(dataOrdering$Freq, decreasing = T),]
  
  if (nrow(dataOrdering)>(maxNum_vizElem+1)) {
    
    outputTable <- dataOrdering[1:maxNum_vizElem,]
    
    others <- colSums(dataOrdering[(maxNum_vizElem+1):nrow(dataOrdering),])
    
    outputTable <- rbind(outputTable, Others = others)

  } else if (nrow(dataOrdering)==(maxNum_vizElem+1)) {
    
    outputTable <- dataOrdering[1:maxNum_vizElem,]
    
    others <- dataOrdering[(maxNum_vizElem+1),]
    
    outputTable <- rbind(outputTable, Others = others)
    
  } else {
    
    outputTable <- dataOrdering
    
  }
  
  outputTable$Freq <- NULL
  
  return(outputTable)
}


# >>Subsetting the input data by frequency -----
##Defining the function that allows to subset the input data according to the following parameters:
#1. The min frequency (%) calculated on the whole user-selected period of interest.
#This function requires the following inputs:
#1. dataTable -> The input data table of per week counts.
#2. totSequences -> The total number of genomes sequenced in the user-selected time period of
#   interest.
#3. freqThreshold -> The minimum frequency (%) required to an element from the input table
#   to be represented.
#The function gives as an output a per weeks count table subsetted by the above mentioned parameters
#(data passing the filter only).
dataFreq_Subsetter <- function(dataTable, totSequences, freqThreshold) {
  #Data are ordered in decreasing order according to the frequency (%)
  #of each element.
  dataOrdering <- cbind(dataTable,
                        Freq = rowSums(dataTable)/totSequences)
  
  dataOrdering <- dataOrdering[order(dataOrdering$Freq, decreasing = T),]
  
  #Elements with a frequency (%) above or equal to the selected
  #frequency (%) threshold are stored in the dataPassing variable.
  dataPassing <- dataOrdering[dataOrdering$Freq>=as.numeric(freqThreshold),]
  
  dataPassing$Freq <- NULL
  
  return(dataPassing)
}


#######DATA NORMALIZATION FUNCTION#######
# >>Normalizing input or subsetted data -----
#Defining the function that allows to normalize the input data.
#This function requires the following inputs:
#1. dataTable -> The input data table of per week counts (subsetted or not).
#2. perWeek_totSequences -> The total number of genomes sequenced each week during the user-selected
#   time period of interest.
#The function gives as an output a per weeks frequency (%) table.
dataNormalizer <- function(dataTable, perWeek_totSequences) {
  #Normalized data usually give a more insightful information, especially when
  #used to produce plots (for example barplots). For this reason the frequency (%)
  #is calculated for every input table.
  #Frequency (%) is calculated for each week and it equals to the ratio between
  #the number of weekly occurrences of an element and the total number of elements
  #identified each week during the user-selected time lapse of interest.
  dataFreq <- mapply("/", dataTable, perWeek_totSequences)
  
  if (is.matrix(dataFreq)) {
    row.names(dataFreq) <- row.names(dataTable)
  } else {
    dataFreq <- as.matrix(t(dataFreq))
    row.names(dataFreq) <- row.names(dataTable)
  }
  
  return(dataFreq)
}


#######COLOR PALETTES PRODUCTION FUNCTIONS#######
# >>Producing the color palette for part of the plots (mainly Barplots and Pie Charts) -----
#Defining the function that allows to produce a color palette to associate to the
#main plots (frequency barplots and pie charts).
#This function accepts as inputs:
#1. dataTable -> The input data table.
#2. randomPalette <- A palette of n. random colors.
#3. specialPalette <- A palette in which a specific color is assigned to peculiar elements
#   of the initial pool of data.
#The function gives as an output the vector of colors assigned to each element in the input
#table.
plotPalette <- function(dataTable, randomPalette, specialPalette) {
  if (missing(specialPalette)) {
    #Names of the elements collected in the dataTable iput table are stored in the
    #dataElements variables.
    dataElements <- row.names(dataTable)
    
    #A n. of colors equal to the n. of elements in the dataElements variable
    #is extracted from the random palette given by the user through the
    #randomPalette argument. Colors are stored in the outputColors variable.
    outputColors <- randomPalette[1:length(dataElements)]
    
    #If the specialPalette argument is not defined the final palette consists of
    #colors from the randomPalette argument. However, if "Others" belongs to the
    #elements in the dataElements variable the color corresponding to its position
    #is replaced with a specific color (the same for all the possible palettes and
    #plots that include "Others" as an element).
    if ("Others"%in%dataElements) {
      outputColors[dataElements%in%"Others"] <- "mediumblue"
    }
    
  } else {
    #Names of the elements collected in the dataTable iput table are stored in the
    #myElements variables.
    dataElements <- row.names(dataTable)
    
    #A n. of colors equal to the n. of elements in the dataElements variable
    #is extracted from the random palette given by the user through the
    #randomPalette argument. Colors are stored in the outputColors variable.
    outputColors <- randomPalette[1:length(dataElements)]
    
    #If the specialPalette argument is defined the final color palette has to take
    #in account also colors previosly assigned to "special" elements if they are
    #among the elements in the dataElements variable.
    #Checking if any of the elements from the dataElements variants is also present
    #among the elements in the user palette specified through the specialPalette
    #argument and storing the index of TRUE values in the isSpecial variable.
    isSpecial <- dataElements%in%row.names(specialPalette)
    
    #If at least a single element from the specialPalette argument is present in the
    #dataElements variable, the random color (in the outputColors palette) corresponding
    #to its index is replaced by the specific color assigned to that element from
    #the specialPalette argument given by the user.
    if (sum(isSpecial)>=1){
      mySpecial <- dataElements[isSpecial]
      outputColors[isSpecial] <- specialPalette[mySpecial,1]
    }
    
  }
  
  names(outputColors) <- dataElements
  
  return(outputColors)
}


# >>Producing the color palette for the choropleth maps -----
#Defining the function that allows to produce the color palette for choropleth maps.
#This function as accepts as inputs:
#1. dataValue <- A frequency (%) value.
#2. referencePalette <- A palette defining the color corresponding to each possible frequency
#   (%) range.
#The function gives as an output the vector of colors assigned to each element in the input
#table.
mapPalette <- function(dataValue, referencePalette) {
  colorIndex <- ceiling(as.numeric(dataValue)/10)
  
  if (colorIndex==0) {
    idx <- 1
    refColor <- referencePalette[idx]
  } else {
    idx <- colorIndex
    refColor <- referencePalette[idx]
  }
  
  return(refColor)
}


#######PLOTTING FUNCTIONS#######
# >>Producing the barplot that allows to graphically represent the data frequency -----
#Defining the function that allows to produce the barplot (BP) representing the
#the input data of interest.
#This function accepts as inputs:
#1. dataTable <- The input data table.
#2. dataPalette <- The color palette previously produced for the input table.
#3. plotMain <- The main title of the BP.
#The function gives as an output a barplot representing the data of interest.
dataPlotter_BP <- function(dataTable, dataPalette, plotMain) {
  #Producing a barplot and the corresponding legend for the input data table.
  par(mar = c(4,5,2,0), fig = c(0,0.75,0,1))
  
  barplot(dataTable,
          col=dataPalette,
          xlab = "Week",
          ylab = "#Genomes",
          main = plotMain,
          cex.lab = 1.5,
          cex.axis = 1.5,
          cex.main = 1.5)
  
  #Generating the legend in a secondary space of the plot area.
  par(mar = c(0,0,0,0), fig = c(0.75,1,0,1), new = T)
  
  barplot(0,
          xaxt = "n",
          yaxt = "n",
          col.axis = "white",
          border = "white")
  
  legend(legend = row.names(dataTable),
         fill = dataPalette,
         cex = 0.85,
         x = "topleft",
         bty = "n",
         xpd = TRUE)
}


# >>Producing the barplot that allows ro graphically represent the per week total number of element -----
#Defining the function that allow to produce the barplot representing the total number
#of elements identified for each week of the user-selected time lapse of interest.
#This function acceps ad an input:
#1. dataTable -> The input data table.
#2. dataVector -> The input data vector, calculated as the per week sum of elements in the
#   the original data table. Represents the weekly total number of elements in the original data.
#The function gives as an output a barplot representing the data of interest.
perWeekTotSeqPlotter_BP <- function(dataTable, dataVector) {
  #Producing a barplot desplaying the total number of elements from the original
  #input data tables identified for each week of the user-selected time period
  #of interest.
  par(mar = c(4,5,3,0), fig = c(0,0.75,0,1))
  
  perWeekTotSeq_BP <- barplot(dataVector,
                              col = "darkgrey",
                              ylim = c(max(dataVector), 0),
                              ylab = "#Genomes",
                              xaxt = "n",
                              cex.lab = 1.5,
                              cex.axis = 1.5)
  
  axis(side = 3,
       at = perWeekTotSeq_BP,
       labels = colnames(dataTable),
       tick = 0)
  
  text(perWeekTotSeq_BP,
       dataVector,
       labels = dataVector,
       srt = 90,
       cex = 0.80,
       adj = c(1,0.5),
       xpd = TRUE)
}


# >>Producing the pie chart that allows to graphically represent the input data -----
#Defining the functin that allows to produce the pie chart (PC) graphicallly representing the input data
#of interest.
#This function accepts as an input:
#1. dataTable -> The input datatable.
#2. dataPalette -> The color palette previously produced for the input table.
#3. plotMain -> The main title for the pie chart.
#The function gives as an output a pie chart representing the data of interest.
dataPlotter_PC <- function(dataTable, dataPalette, plotMain) {
  #Generating the pie chart representing the input data and its
  #legend.
  if (nrow(dataTable)>=2) {
    inPie <- rowSums(dataTable)
    inLabels <- row.names(dataTable)
  } else {
    inPie <- sum(dataTable)
    inLabels <- row.names(dataTable)
  }
  
  par(mar = c(0,0,0,0), fig = c(0,0.75,0,1))
  
  pie(inPie, labels = NA, col = dataPalette)
  title(main = plotMain, cex.main = 1.5, line = -1.5)
  
  #The legend in a secondary space of the plot area.
  par(mar = c(0,0,0,0), fig = c(0.75,1,0,1), new = T)
  
  barplot(0,
          xaxt = "n",
          yaxt = "n",
          col.axis = "white",
          border = "white")
  
  legend(legend = inLabels,
         fill = dataPalette,
         cex = 0.85,
         x = "topleft",
         bty = "n",
         xpd = TRUE)
}


# >>Producing the scatterplot for a user-selected element from the input data table -----
#Defining the function that allows to produce the scatterplot representing a user-selected
#element from the input data table.
#This function accepts as an input:
#1. dataTable -> The input data table.
#2. perWeek_totSequences -> The total number of genomes sequenced each week during the user-selected
#   time period of interest.
#3. dataElement -> The user-selected element to be graphically represented.
#4. dataPalette -> The color palette previously produced for the input table.
#The function gives as an output the scatterplot for the data of interest.
dataPlotter_SP <- function(dataTable, perWeek_totSequences, dataElement, dataPalette) {
  #Values for the element of interest are collected from the input data table and
  #stored under the selElement variable.
  selElement <- dataTable[dataElement,]
  
  #A scatterplot of the element of interest is produced.
  plot(colnames(dataTable),
       selElement,
       type = "b",
       col = dataPalette[dataElement],
       lwd = 3,
       pch = 15,
       lty = 13,
       ylim = c(0, max(perWeek_totSequences)),
       main = dataElement,
       xlab = "Week",
       ylab = "#Genomes", cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
  
  #A scatterplot of all sequenced genomes is produced.
  lines(colnames(dataTable),
        perWeek_totSequences,
        type = "b",
        col = "dimgray",
        lwd=3,
        pch=15,
        lty=13)
  
  #The legend of the plot is produced.
  legend(legend = c(dataElement, "Total Genomes"),
         col = c(dataPalette[dataElement], "darkgrey"),
         pch = 15,
         cex = 1.25,
         x = "topleft",
         inset = c(0.05,0),
         bty = "n",
         xpd = TRUE)
}


# >>Producing the choropleth map for a user-selected element -----
#Defining the function that allows to produce the choropleth map representing the regional
#frequency (%) of a user-selected element in a country and time period of interest.
#This functio accepts as inputs:
#1. dataTable -> The input data table.
#2. dataElement -> The user-selected element to be graphically represented.
#3. geomData -> The input table containig the geometrical data that allows to generate the map.
#4. dataPalette -> The color palette previously produced for the input table.
#5. referencePalette -> A palette defining the color corresponding to each possible frequency
#   (%) range.
#The function gives as an output the choropleth map for the data of interest.
dataPlotter_ChoroMap <- function(dataTable, dataElement, geomData, dataPalette, referencePalette) {
  #Defining the labels for the map and the legend.
  mapLabels <- paste(names(dataTable),
                     "<br/>",
                     paste(dataElement, "frequency:"),
                     paste0(as.numeric(dataTable), "%")) %>%
    lapply(HTML)
  
  legendLabels <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100")
  
  leaflet(geomData) %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%
    addPolygons(stroke = T,
                color = "black",
                weight = 1.5,
                fillColor = as.vector(dataPalette),
                fillOpacity = 1,
                label = mapLabels) %>%
    addLegend(colors = referencePalette,
              labels = legendLabels,
              opacity = 1,
              title = paste(dataElement, "frequency (%)"),
              position = "topright")
}


#######WIDGET PRODUCING FUNCTIONS#######
# >>Producing a drop down menu widget -----
#Defining the function that allows to produce a drop down menu widget. The latter is used to select an
#element of interest (from the corresponding) input table to be represented in a plot.
#This function accepts as an input:
#1. dataTable -> The input data table.
#2. widgetName -> The name that allows to call widget-defined variables in a Shiny App.
#3. widgetMain -> The title of the widget in the User Interface.
#4. ShinyIn -> The Shiny input function (input). It allows to use a widget defined
#   input in the code of the App Server. Default is input.
#The function gives as an output a drop down menu listing all the elements from
#the input table.
dropdownM_widgetGenerator <- function(dataTable, widgetName, widgetMain, shinyIn) {
  #The list of elements in the menu depends on which elements are
  #present in the input table.
  widgetElements <- row.names(dataTable)
  
  names(widgetElements) <- widgetElements
  
  #Storing the element selected by the user in a variable called widgetDefault that,
  #if the element is still present in the input table, is used as a default selection
  #anytime the menu is generated.
  widgetDefault <- isolate(shinyIn$widgetName)
  
  freezeReactiveValue(shinyIn, widgetName)
  
  selectInput(widgetName,
              widgetMain,
              choices = as.list(widgetElements),
              selected = widgetDefault)
}


#######SPECIAL INPUTS PRODUCING FUNCTIONS#######
# >>Producing the input table for the Lineage+ VS Lineage Tab -----
#Defining the function that allows to produce the input table required by the plots in the
#Lineage+ VS Lineage Tab of the Shin App. This function is specifically designed to be used
#in the above mentioned tab only.
#This function accepts as an input:
#1. dataTable -> The input data table.
#2. dataElement -> The user-selected element of interest (in this case a Lineage+).
#3. convertionTable -> The table from the configuration files collection that allows to associate
#   the Lineage+ of interest to its "parental" Lineage.
#The function gives as an output the input table required by the plots of the Lineage+ VS Lineage Tab.
spLinVSLin_inTableProducer <- function(dataTable, dataElement, convertionTable) {
  #The "parental" Lineage of the Lineage+ of interest (defined by the dataElement input value)
  #is retrieved from the convertion table and stored in the dataConvertion variable.
  dataConvertion <- convertionTable[dataElement,1]
  
  #Data for the Lineage+ of interest and its "parent" Lineage are retrieved from the
  #input data table.
  selElement <- dataTable[dataElement,]
  convElement <- dataTable[dataConvertion,]
  
  #Data for all the other elements in the input data table are collapsed together and stored in the
  #others variable.
  others <- colSums(dataTable[row.names(dataTable)!=dataElement&row.names(dataTable)!=dataConvertion,])
  
  #The final table is generated by binding together:
  #1. Data of the Lineage+ of interest.
  #2. Data of its "parental" Lineage.
  #3. Data of Lineages and Lineages+ different from the Lineage+
  #   of interest and its "parental" Lineage. These data were previously collapsed and stored in the others
  #   variable.
  outputTable <- rbind(Others = others,
                       convElement)
  
  outputTable <- rbind(outputTable,
                       selElement)
  
  return(outputTable)
}
