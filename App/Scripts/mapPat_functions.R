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
#The function gives as an output a per weeks count table subsetted by the above mentioned parameters
#(including data both passing and non-passing the filters. Non-passing data are collapsed under Others).
dataFreqViz_Subsetter <- function(dataTable, totSequences, freqThreshold, maxNum_vizElem) {
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
  if (nrow(dataPassing)>(as.numeric(maxNum_vizElem)+1)) {
    
    outputTable <- dataPassing[1:as.numeric(maxNum_vizElem),]
    
    others <- colSums(dataPassing[(as.numeric(maxNum_vizElem)+1):nrow(dataPassing),])
    
    others <- others + dataNotPassing
    
    outputTable <- rbind(outputTable,
                         Others = others)
    
  } else if (nrow(dataPassing)==(as.numeric(maxNum_vizElem)+1)) {
    
    outputTable <- dataPassing[1:as.numeric(maxNum_vizElem),]
    
    others <- dataPassing[(as.numeric(maxNum_vizElem)+1),]
    
    others <- others + dataNotPassing
    
    outputTable <- rbind(outputTable, Others = others)
    
  } else if (nrow(dataPassing)>=1&nrow(dataPassing)<(as.numeric(maxNum_vizElem)+1)) {
    
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


# >>Subsetting input data by frequency and max n. of explicitly represented elements (No Others Filter) -----
#Defining the function that allows to subset the input data according to the following parameters:
#1. The min frequency (%) calculated on the whole user-selected period of interest.
#2. The max number of elements that can be explicitly represented in a plot.
#This function in particular does not filter the Others (see below) variable by min frequency (%) in order to
#avoid any possible loss of data.
#This function requires the following inputs:
#1. dataTable -> The input data table of per week counts.
#2. totSequences -> The total number of genomes sequenced in the user-selected time period of
#   interest.
#3. freqThreshold -> The minimum frequency (%) required to an element from the input table
#   to be represented.
#4. maxNum_vizElem <- The maximum number of elements surviving the frequency filter that can be
#   explicitly represented in the final plot.
#The function gives as an output a per weeks count table subsetted by the above mentioned parameters
#(including data both passing and non-passing the filters. Non-passing data are collapsed under Others).
dataFreqViz_noOthers_Subsetter <- function(dataTable, totSequences, freqThreshold, maxNum_vizElem) {
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
  if (nrow(dataPassing)>(as.numeric(maxNum_vizElem)+1)) {
    
    outputTable <- dataPassing[1:as.numeric(maxNum_vizElem),]
    
    others <- colSums(dataPassing[(as.numeric(maxNum_vizElem)+1):nrow(dataPassing),])
    
    others <- others + dataNotPassing
    
    outputTable <- rbind(outputTable,
                         Others = others)
    
  } else if (nrow(dataPassing)==(as.numeric(maxNum_vizElem)+1)) {
    
    outputTable <- dataPassing[1:as.numeric(maxNum_vizElem),]
    
    others <- dataPassing[(as.numeric(maxNum_vizElem)+1),]
    
    others <- others + dataNotPassing
    
    outputTable <- rbind(outputTable, Others = others)
    
  } else if (nrow(dataPassing)>=1&nrow(dataPassing)<(as.numeric(maxNum_vizElem)+1)) {
    
    outputTable <- rbind(dataPassing, Others = dataNotPassing)
    
  } else {
    
    names(dataNotPassing) <- colnames(dataOrdering)
    
    outputTable <- as.data.frame(t(dataNotPassing), row.names = "Others")
  }
  
  outputTable$Freq <- NULL
  
  return(outputTable)
}


# >>Subsetting the input data by frequency -----
#Defining the function that allows to subset the input data according to the following parameters:
#1. The min frequency (%) calculated on the whole user-selected period of interest.
#This function requires the following inputs:
#1. dataTable -> The input data table of per week counts.
#2. totSequences -> The total number of genomes sequenced in the user-selected time period of
#   interest.
#3. freqThreshold -> The minimum frequency (%) required to an element from the input table
#   to be represented.
#The function gives as an output a per weeks count table subsetted by the above mentioned parameters
#(including only data passing the filter).
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


# >>Subsetting input data by number of occurrences -----
#Defining the function that allows to subset the input data according to the following parameters:
#1. The min number of occurrences of an element from the input table calculated on the whole
#   user-selected period of interest.
#This function requires the following inputs:
#1. dataTable -> The input data table of per week counts.
#2. occurThreshold -> The minimum number of occurrences required to an element from the input table
#   to be represented.
#The function gives as an output a per weeks count table subsetted by the above mentioned parameters
#(includng only data passing the filter). Defaults to 1 occurrence.
dataOccur_Subsetter <- function(dataTable, occurThreshold) {
  #Data are ordered in decreasing order according to the number of occurences
  #of each element.
  dataOrdering <- cbind(dataTable,
                        Occur = rowSums(dataTable))
  
  dataOrdering <- dataOrdering[order(dataOrdering$Occur, decreasing = T),]
  
  #Elements with a n. of occurrences above or equal to the selected
  #threshold are stored in the dataPassing variable.
  dataPassing <- dataOrdering[dataOrdering$Occur>=as.numeric(occurThreshold),]
  
  dataPassing$Occur <- NULL
  
  return(dataPassing)
}


#######DATA NORMALIZATION FUNCTION#######
# >>Normalizing input data -----
#Defining the function that allows to normalize the input data.
#This function requires the following inputs:
#1. dataTable -> The input data table of per week counts (subsetted or not).
#2. perWeek_totSequences -> The total number of genomes sequenced each week during the user-selected
#   time period of interest.
#The function gives as an output a per weeks frequency (%) table.
dataNormalizer <- function(dataTable, perWeek_totSequences) {
  #Normalized data usually give a more insightful information, especially when
  #used to produce plots. For this reason the frequency (%) is calculated for
  #every input table.
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
  
  dataFreq[is.nan(dataFreq)] <- 0
  
  return(dataFreq)
}


#######COLOR PALETTES PRODUCTION FUNCTIONS#######
# >>Producing the color palette for part of the plots (Stacked Area Charts, ScatterPlots and HeatMaps) -----
#Defining the function that allows to produce a color palette to associate to the
#main plots (Stacked Area Charts, ScatterPlots and HeatMaps).
#This function accepts as inputs:
#1. tabPalette -> The color theme defining a certain tab of the App.
#2. dataTable -> The input data table. Omitted if the colorsNum argument is specified.
#3. colorsNum -> The number of colors required to produce the final palette. Omitted
#   if the dataTable argument is specified.
#The function gives as an output the vector of colors required to produce the plot of interest.
plotPalette <- function(tabPalette, dataTable, colorsNum) {
  if (missing(colorsNum)) {
    #The number of elements of the input data table is stored in
    #the dataNum variable. This value is used to define the number
    #of colors required to produce the final palette.
    dataNum <- nrow(dataTable)
    
    #Each tab of the App is characterized by a specific color theme
    #which is defined by the tabPalette argument.
    #This argument allows to produce a customized palette for each
    #plot of each tab of the App using the colorRampPalette function.
    outputColors <- colorRampPalette(tabPalette)(dataNum)
    
  } else if (missing(dataTable)) {
    #If the dataTable argument is missing the color palette for the
    #plot of interest can still be produced by specifying the number
    #of required colors through the colorsNum argument.
    outputColors <- colorRampPalette(tabPalette)(colorsNum)
  }
  
  return(outputColors)
}


# >>Producing the color palette for the Choropleth Maps -----
#Defining the function that allows to produce the color palette for Choropleth Maps.
#This function as accepts as inputs:
#1. dataValue <- A frequency (%) value.
#2. referencePalette <- A palette defining the color corresponding to each possible frequency
#   (%) range.
#The function gives as an output the vector of colors assigned to each element in the input
#table. Colors are retrived from the previously produced reference palette according to the
#index derived from each frequency (%) value.
mapPalette <- function(dataValue, referencePalette) {
  #The frequency (%) value defined by the dataValue argumet is converted into an integer value
  #that represents the index of the corresponding color in referencePalette. This value is stored
  #in the colorIndex variable.
  colorIndex <- ceiling(as.numeric(dataValue)/10)
  
  #Colors are extracted from referencePalette by index. If colorIndex equals to 0 the index of the
  #corresponding color is manually set to 1, otherwise it equals the value of colorIndex.
  if (colorIndex==0|is.nan(colorIndex)) {
    idx <- 1
    refColor <- referencePalette[idx]
  } else {
    idx <- colorIndex
    refColor <- referencePalette[idx]
  }
  
  return(refColor)
}


#######PLOTTING FUNCTIONS#######
# >>Producing the Stacked Area Chart that allows to graphically represent the frequency (%) of the data -----
#Defining the function that allows to produce the Stacked Area Chart (SAC) representing the input data
#of interest.
#This function accepts as inputs:
#1. dataTable <- The input data table.
#2. dataPalette <- The color palette produced for the input table.
#3. plotMain <- The main title of the SAC.
#4. timeUn <- The time unit on the x axis.
#The function gives as an output a Stacked Area Chart (SAC) representing the data of interest.
dataPlotter_SAC <- function(dataTable, dataPalette, plotMain, timeUn) {
  #Producing a Stacked Area Chart (SAC) and the corresponding legend for the
  #input data table.
  #The original input data table is converted to a ggplot2 suitable format.
  xValues <- as.numeric(rep(colnames(dataTable), each = nrow(dataTable)))
  yValues <- as.vector(unlist(dataTable))
  fillValues <- rep(row.names(dataTable), times = ncol(dataTable))
  
  plotData <- data.frame(xValues, yValues, fillValues)
  xEnd <- max(xValues)
  
  #Plotting the SAC using ggplot2.
  ggplot(plotData,
         aes(x = xValues, y = yValues, fill = fillValues)) +
    geom_area(colour = "black",
              linewidth = 0.2) +
    scale_fill_manual(values = dataPalette) +
    theme_classic() +
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          legend.title = element_text(colour = "transparent"),
          legend.position = "top") +
    scale_x_continuous(breaks = seq(0, xEnd, by = 5),
                       expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = plotMain,
         x = timeUn,
         y = "Frequency (%)")
}


# >>Producing the BarPlot that allows to graphically represent the per week total number of elements -----
#Defining the function that allows to produce the BarPlot (BP) representing the total number
#of elements identified for each week of the user-selected time lapse of interest.
#This function acceps ad an input:
#1. dataVector -> The input data vector, calculated as the per week sum of elements in the
#   the original data table. Represents the weekly total number of elements in the original data.
#The function gives as an output a BarPlot (BP) representing the data of interest.
perWeekTotSeqPlotter_BP <- function(dataVector) {
  #Producing a BarPlot (BP) displaying the total number of elements from the original
  #input data tables identified for each week of the user-selected time period
  #of interest.
  #Converting the original input data vector to a ggplot2 suitable format.
  xValues <- as.numeric(names(dataVector))
  yValues <- dataVector
  fillValues <- rep("placeH", times = length(yValues))
  
  plotData <- data.frame(xValues, yValues, fillValues)
  xEnd <- max(xValues)
  
  #Plotting the BP using ggplot2.
  ggplot(plotData,
         aes(x = xValues, y = yValues, fill = fillValues)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = "#696969",
                      guide = guide_legend(override.aes = list(alpha = 0))) +
    theme_classic() +
    theme(text = element_text(size = 15),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          legend.title = element_text(colour = "transparent"),
          legend.text = element_text(colour = "transparent"),
          legend.position = "bottom") +
    scale_x_continuous(breaks = seq(0, xEnd, by = 5),
                       expand = c(0,0),
                       position = "top") +
    scale_y_continuous(expand = c(0,0),
                       trans = "reverse") +
    geom_text(aes(label = yValues),
              position = position_stack(vjust = 0.5),
              angle = 90,
              size = 5,
              colour = "white") +
    labs(x = NULL,
         y = "#Genomes")
}


# >>Producing the ScatterPlot for a user-selected number of elements from the input data table -----
#Defining the function that allows to produce the ScatterPlot (SP) representing a user-selected
#number of elements from the input data table.
#This function accepts as inputs:
#1. dataTable <- The input data table.
#2. dataPalette <- The color palette produced for the input table.
#3. plotMain <- The main title of the SP.
#4. timeUn <- The time unit on the x axis.
#The function gives as an output a ScatterPlot (SP) representing the data of interest.
dataPlotter_SP <- function(dataTable, dataPalette, plotMain, timeUn) {
  #Producing a ScatterPlot (SP) and the corresponding legend for the
  #input data table.
  #The original input data table is converted to a ggplot2 suitable format.
  xValues <- as.numeric(rep(colnames(dataTable), each = nrow(dataTable)))
  yValues <- as.vector(unlist(dataTable))
  colValues <- rep(row.names(dataTable), times = ncol(dataTable))
  
  plotData <- data.frame(xValues, yValues, colValues)
  xEnd <- max(xValues)
  
  #Plotting the SP using ggplot2.
  ggplot(plotData,
         aes(x = xValues, y = yValues, colour = colValues)) +
    geom_point(size = 2) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = dataPalette) +
    theme_classic() +
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          legend.title = element_text(colour = "transparent"),
          legend.position = "top") +
    scale_x_continuous(breaks = seq(0, xEnd, by = 5),
                       expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = plotMain,
         x = timeUn,
         y = "#Genomes",
         colour = plotMain)
}


# >>Producing the HeatMap that allows to graphically represent the Regional Frequency (%) of the data -----
#Defining the function that allows to produce the HeatMap (HM) representing the regional frequency (%)
#of the elements in the input data table.
#This function accepts as an input:
#1. dataTable <- The input data table.
#2. dataNum <- The number of elements from dataTable to be represented.
#3. dataPalette <- The color palette produced for the input table.
#4. plotMain <- The main title of the HM.
#5. tagColor <- Color of the tags reporting the numerical values of the represented data.
#The function gives as an output a HeatMap (HM) representing the data of interest.
dataPlotter_HM <- function(dataTable, dataNum, dataPalette, plotMain, tagColor) {
  #Producin the HeatMap (HM) and the corresponding legend for the input data table.
  #Due to graphical constrains only a limited umber of elements (defined by the dataNum argument) is represented.
  #By default the first 25 elements with the higher cumulative frequency (%) in the selected time period are represented.
  inTable <- dataTable[,1:as.numeric(dataNum)]
  
  #Plotting.
  pheatmap(inTable,
           color = dataPalette,
           main = plotMain,
           display_numbers = T,
           number_color = tagColor,
           fontsize = 13,
           fontsize_col =  13,
           fontsize_row = 13,
           fontsize_number = 13,
           cellheight = 23,
           cellwidth = 38,
           treeheight_row = 0,
           treeheight_col = 0)
}


# >>Producing the BarPlot that allows to graphically represent the Regional Frequency (%) of a single element from the data -----
#Defining the function that allows to produce the BarPlot (BP) representing the regional freqeuncy (%)
#of a single element in the input data table.
#This function accepts as an input:
#1. dataTable <- The input data table.
#2. dataPalette <- The color palette produced for the input table.
#3. plotMain <- The main title of the HM.
#The function gives as an output a BarPlot (BP) representing the data of interest.
dataPlotter_regBP <- function(dataTable, dataPalette, plotMain) {
  #Converting the original input data table to a ggplot2 suitable format.
  xValues <- colnames(dataTable)
  yValues <- dataTable[1,]
  
  plotData <- data.frame(xValues, yValues)
  
  #Plotting the BP using ggplot2.
  ggplot(plotData,
         aes(x = xValues, y = yValues)) +
    geom_bar(stat = "identity", fill = dataPalette) +
    theme_classic() +
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          legend.title = element_text(colour = "transparent"),
          legend.position = "top") +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = plotMain,
         x = "Regions",
         y = "Frequency (%)")
}


# >>Producing the BarPlot that allows to graphically represent comparisons between elements of interest -----
#Defining the function that allows to produce the BarPlot (BP) representing the comparison between
#elements of interest.
#This function acceps as inputs:
#1. dataTable <- The input data table.
#2. dataPalette <- The color palette produced for the input table.
#3. plotMain <- The main title of the BP.
#4. timeUn <- The time unit on the x axis.
#The function gives as an output a BarPlot (BP) representing the data of interest, which is different
#depending on the Tab. More in detail:
#1. In the Variants Tab this BP represents the composition (defined as proportion of Lineages)
#   of a user-selected variant of interest for each week of the user-selected time lapse of interest.
#2. In the Mutations Tab this BP represents the comparison between the frequency (%) of genomes of the
#   user-selected Lineage presenting a mutation of interest and that of genomes without the mutation.
dataPlotter_BP <- function(dataTable, dataPalette, plotMain, timeUn) {
  #Converting the original input data table to a ggplot2 suitable format.
  xValues <- as.numeric(rep(colnames(dataTable), each = nrow(dataTable)))
  yValues <- as.vector(unlist(dataTable))
  fillValues <- rep(row.names(dataTable), times = ncol(dataTable))
  
  plotData <- data.frame(xValues, yValues, fillValues)
  xEnd <- max(xValues)
  
  #Plotting the BP using ggplot2.
  ggplot(plotData,
         aes(x = xValues, y = yValues, fill = fillValues)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = dataPalette) +
    theme_classic() +
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
          legend.title = element_text(colour = "transparent"),
          legend.position = "top") +
    scale_x_continuous(breaks = seq(0, xEnd, by = 5),
                       expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = plotMain,
         x = timeUn,
         y = "Frequency (%)")
}


# >>Producing the Choropleth Map for a user-selected element -----
#Defining the function that allows to produce the choropleth map (CM) representing the regional
#frequency (%) of a user-selected element in a country and time period of interest.
#This function accepts as inputs:
#1. dataTable -> The input data table.
#2. dataElement -> The user-selected element to be graphically represented.
#3. geomData -> The input table containing the geometrical data that allows to generate the map.
#4. dataPalette -> The color palette produced for the input table.
#5. referencePalette -> A palette defining the color corresponding to each possible frequency
#   (%) range.
#The function gives as an output the choropleth map for the data of interest.
dataPlotter_ChoroMap <- function(dataTable, dataElement, geomData, dataPalette, referencePalette) {
  #Producing a choropleth map (CM) and the corresponding legend for the input data table.
  #Defining the labels for the map and the legend.
  mapLabels <- paste(rownames(dataTable),
                     "<br/>",
                     paste(dataElement, "frequency:"),
                     paste0(as.numeric(dataTable), "%")) %>%
    lapply(HTML)
  
  legendLabels <- c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100")
  
  #Plotting the choropleth map (CM) using leaflet.
  leaflet(geomData) %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%
    addPolygons(stroke = T,
                color = "black",
                weight = 1.5,
                fillColor = as.vector(dataPalette),
                fillOpacity = 1,
                label = mapLabels,
                labelOptions = labelOptions(textsize = "13px")) %>%
    addLegend(colors = referencePalette,
              labels = legendLabels,
              opacity = 1,
              title = paste(dataElement, "frequency (%)"),
              position = "topright")
}


#######WIDGET PRODUCING FUNCTIONS#######
# >>Producing a drop down menu widget -----
#Defining the function that allows to produce a drop down menu widget. The latter is used to select an
#element of interest (from the corresponding input table) to be represented in a plot.
#This function accepts as an input:
#1. dataTable -> The input data table.
#2. widgetName -> The name that allows to call widget-defined variables in a Shiny App.
#3. widgetMain -> The title of the widget in the User Interface.
#4. posDefault -> The position in the selectable list of elements of the default selection
#   of the widget. Defaults to 1 or 2.
#The function gives as an output a drop down menu listing all the elements from
#the input table.
dropdownM_widgetGenerator <- function(dataTable, widgetName, widgetMain, posDefault) {
  #The list of elements in the menu depends on which elements are
  #present in the input table.
  widgetElements <- row.names(dataTable)
  
  names(widgetElements) <- widgetElements
  
  widgetElements <- as.list(widgetElements)
  
  #Storing the default widget element in a variable named widgetDefault.
  #Depending on the widget this value defaults to the first or second element
  #of the list of selectable elements.
  widgetDefault <- widgetElements[as.numeric(posDefault)]
  
  selectInput(widgetName,
              widgetMain,
              choices = widgetElements,
              selected = widgetDefault)
}


#######SPECIAL INPUTS PRODUCING FUNCTIONS#######
# >>Producing the input table for the Stacked Area Chart in the Variants Tab -----
#Defining the function that allows to produce the input table for the production of the
#Stacked Area Chart (SAC) in the Variants Tab. This function allows to subset the initial input
#data table in order to retain only those Variants that belong to a user-selected category.
#This function accepts as inputs:
#1. dataTable -> The input data table.
#2. refTable -> The table defining the correspondence between each Variant and its category.
#3. dataElement -> The name of the user-selected Variant category of interest.
#The function gives as an output a table containing data for all the Variants belonging to the
#user selected category of interest.
varCat_dataSelector <- function(dataTable, refTable, dataElement) {
  #The VBM subgroup collects a series of Lineages that are currently under monitoring by WHO.
  #Since, at present, Omicron is the only circulating Variant all VBM lineages belong to this
  #Variant. This subgroup requires careful treatment while producing the input data table
  #for the SAC in order to avoid data repetition.
  if (dataElement=="All") {
    #If the user-selected Variants category (dataElement) is "All" the complete list of Variants
    #from dataTable (which survived previous filters) is collected.
    #The dataSel variable collects the complete list of Variants.
    dataSel <- row.names(dataTable)
    
    if ("Omicron"%in%dataSel) {
      #If the Omicron variant is included in dataSel it needs to be treated accordingly.
      #The Omicron variant, in fact, includes also Lineages belonging to the VBM subgroup,
      #so data corresponding to Omicron are adjusted in order to avoid counts repetition.
      omiData <- dataTable["Omicron",]
      vbmData <- dataTable["VBM",]
      omiData <- omiData-vbmData
      
      outputTable <- dataTable[dataSel,]
      outputTable["Omicron",] <- omiData
      
    } else {
      #If the Omicron Variant does not belong to dataSel data for all Variants in the
      #variable are collected to produce the final table.
      outputTable <- dataTable[dataSel,]
      
    }
    
  } else if (dataElement=="VBM") {
    #If dataElement equals "VBM" only data from this subgroup is included in the final data table.
    outputTable <- dataTable[dataElement,]
    
  } else {
    #If dataElement is equal neither to "All" nor to "VBM", data to include in the final input
    #table for the SAC are collected according to a specific set of conditions.
    #The dataSel variable collects the complete list of Variants belonging to the user-selected
    #category (dataElement).
    dataSel <- unique(refTable[refTable$Status==dataElement,]$Variant)
    
    if ("Omicron"%in%dataSel) {
      #If the Omicron Variant belongs to dataSel, input data are adjusted in order to avoid repetitions
      #(see above). Data from other Variants in dataSel are collected normally and data from the VBM
      #subgroup is also added to the final table.
      omiData <- dataTable["Omicron",]
      vbmData <- dataTable["VBM",]
      omiData <- omiData-vbmData
      
      outputTable <- dataTable[dataSel,]
      outputTable["Omicron",] <- omiData
      outputTable <- rbind(outputTable, vbmData)
      
    } else {
      #If the Omicron Variant does not belong to dataSel, data for all Variants in the variable
      #are collected to produce the final table.
      outputTable <- dataTable[dataSel,]
      
    }
  }
  
  return(outputTable)
}


# >>Producing the table of regional counts for each Variant -----
#Defining the function that allows to produce the input table collecting the regional counts for
#a Variant. Applying the function on the complete collection of Variants allows to produce a complete
#table of regional counts. The latter will be used as a base to produce the input table for the HeatMap
#(HM) in the Variants Tab.
#This function accepts as inputs:
#1. dataElement -> The name of the Variant of interest.
#2. dataTable -> The input data table.
#3. refTable -> The table defining the correspondence between each Variant and Lineage.
#The function gives as an output a table containing the regional counts for the Variant of interest.
#When applied to the complete collection of Variants the output is a list of tables of regional counts,
#each one associated to a Variant of interest.
varReg_tableProducer <- function(dataElement, dataTable, refTable) {
  #The regOrder variable collects the original order of the regions in dataTable. Maintaining the
  #original order of the data is essential to correctly represent them in graphical representations.
  regOrder <- unique(dataTable$reg)
  
  #The VBM subgroup collects a series of Lineages that are currently under monitoring by WHO.
  #Since, at present, Omicron is the only circulating Variant all VBM lineages belong to this
  #Variant. This subgroup requires careful treatment while producing the regional counts table
  #and the corresponding list of lineages is collected in a slightly different way.
  if (dataElement=="VBM") {
    
    linElement <- refTable[refTable$isVBM,]$Lin
    
  } else {
    
    linElement <- refTable[refTable$Variant==dataElement,]$Lin
    
  }
  
  #Data for the Lineages corresponding to dataElement are collected from dataTable.
  regData <-dataTable[dataTable$lin%in%linElement,]
  
  #Only if the Lineages corresponding to dataElement are available in dataTable it is possible
  #to sum them by Region.
  if (nrow(regData!=0)) {
    regData <- aggregate(regData$totSeq,
                         by = list(regData$reg),
                         FUN = sum)
    colnames(regData) <- c("reg", "totSeq")
    
    #The final table consists of three different columns:
    #1. Variant name (var).
    #2. Region name (reg).
    #3. Total number of regional appearances of the Variant (totSeq).
    nRep <- nrow(regData)
    outputTable <- cbind(var = rep(dataElement, each = nRep),
                         regData)
    
    #The final output is ordered by region according to the order collected in the regOrder variable.
    outputTable <- outputTable[match(regOrder, outputTable$reg),]
    
  } else {
    
    nRep <- length(regOrder)
    outputTable <- data.frame(var = rep(dataElement, each = nRep),
                              reg = regOrder,
                              totSeq = rep(0, each = nRep))
    
  }
  
  return(outputTable)
}


# >>Producing the Regions x Variants table for the HeatMap in the Variants Tab -----
#Defining the function that allows to produce the input table for the production of the
#HeatMap (HM) in the Variants Tab. This function allows to correctly associate each Variant
#with its regional counts. Applying the function on the complete collection of Variants allows
#to produce a complete counts table.
#This function accepts as inputs:
#1. dataElement -> The name of the Variant of interest.
#2. dataTable -> The input data table.
#The function gives as an output the vector of data corresponding to the Variant of interest
#(dataElement). When applied to the complete collection of Variants the output is a matrix of
#the data associated to all the Variants in the selection.
varReg_dataSelector <- function(dataElement, dataTable) {
  #Retrieving from dataTable the data corresponding to dataElement.
  elementData <- dataTable[dataTable$var==dataElement,]$totSeq
  
  return(elementData)
}


# >>Producing the final input table for the HeatMap in the Variants Tab -----
#Defining the function that allows to produce the final input table for the production of the
#HeatMap (HM) in the Variants Tab. This function allows to subset the initial input
#data table in order to retain only those Variants that belong to a user-selected category.
#This function accepts as inputs:
#1. dataTable -> The input data table.
#2. refTable -> The table defining the correspondence between each Variant and category.
#3. dataElement -> The name of the user-selected Variant category of interest.
#The function gives as an output a table containing the regional counts for all the Variants
#belonging to the user selected category of interest.
regVarCat_dataSelector <- function(dataTable, refTable, dataElement) {
  #The VBM subgroup collects a series of Lineages that are currently under monitoring by WHO.
  #Since, at present, Omicron is the only circulating Variant all VBM lineages belong to this
  #Variant. This subgroup requires careful treatment while producing the input data table
  #for the HM in order to avoid data repetition.
  if (dataElement=="All") {
    #If the user-selected Variants category (dataElement) is "All" the complete list of Variants
    #from dataTable (which survived previous filters) is collected.
    #The dataSel variable collects the complete list of Variants.
    dataSel <- colnames(dataTable)
    
    if ("Omicron"%in%dataSel) {
      #If the Omicron variant is included in dataSel it needs to be treated accordingly.
      #The Omicron variant, in fact, includes also Lineages belonging to the VBM subgroup,
      #so data corresponding to Omicron are adjusted in order to avoid counts repetition.
      omiData <- dataTable[,"Omicron"]
      vbmData <- dataTable[,"VBM"]
      omiData <- omiData-vbmData
      
      outputTable <- dataTable
      outputTable[,"Omicron"] <- omiData
      
    } else {
      #If the Omicron Variant does not belong to dataSel, data for all Variants in the variable
      #are collected to produce the final table.
      outputTable <- dataTable[,dataSel]
      
    }
    
  } else if (dataElement=="VBM") {
    #If dataElement equals "VBM" only data from this subgroup is included in the final data table.
    outputTable <- dataTable[,dataElement]
    
  } else {
    #If dataElement is neither equal to "All" nor part of the omiGr variable, data to include in the
    #final input table for the HM are collected according to a specific set of conditions.
    #The dataSel variable collects the complete list of Variants belonging to the user-selected
    #category (dataElement).
    dataSel <- unique(refTable[refTable$Status==dataElement,]$Variant)
    
    if ("Omicron"%in%dataSel) {
      #If the Omicron Variant belongs to dataSel, input data are adjusted in order to avoid repetitions
      #(see above). Data from other Variants in dataSel are collected normally and data from the VBM
      #subgroup is also added to the final table.
      omiData <- dataTable[,"Omicron"]
      vbmData <- dataTable[,"VBM"]
      omiData <- omiData-vbmData
      
      outputTable <- dataTable[,dataSel]
      outputTable[,"Omicron"] <- omiData
      outputTable <- cbind(outputTable, vbmData)
      
    } else {
      #If the Omicron Variant does not belong to dataSel, data for all Variants in the dataSel variable
      #are collected to produce the final table.
      outputTable <- dataTable[,dataSel]
      
    }
  }
  
  return(outputTable)
}


# >>Producing the input table for the HeatMap in the Lineages Tab -----
#Defining the function that allows to produce the input table for the production of the
#HeatMap (HM) in the Lineages Tab. This function allows to correctly associate each Lineage
#with its regional counts. Applying the function on the complete collection of Lineages allows
#to produce a complete counts table.
#This function accepts as inputs:
#1. dataElement -> The name of the Lineage of interest.
#2. dataTable -> The input data table.
#The function gives as an output the vector of data corresponding to the Lineage of interest
#(dataElement). When applied to the complete collection of Lineages the output is a matrix of
#the data associated to all the Lineages in the selection.
linReg_dataSelector <- function(dataElement, dataTable) {
  #Retrieving from dataTable the data corresponding to dataElement.
  elementData <- dataTable[dataTable$lin==dataElement,]$totSeq
  
  return(elementData)
}


# >>Subsetting and sorting the input table for the HeatMap in the Lineages and Mutations Tabs -----
#Defining the function that allows to subset and order the data in the input table for the
#HeatMap (HM) in the Lineages and Mutations Tab.
#This function allows to remove Lineages/Mutations with no sequenced genomes in the time period of
#interest and to order them in decreasing order by cumulative frequency (%).
#This function accepts as inputs:
#1. dataTable -> The input data table.
#2. totSeq -> The total number of sequenced genomes in the time period of interest. This value is
#   calculated starting from regional data for the Lineages Tab and from data for the user-selectd
#   Lineage of interest for the Mutations Tab.
#The function gives as an output the input data table subsetted and ordered according to the above
#mentioned parameters.
dataReg_SubSorter <- function(dataTable, totSeq) {
  #Removing from dataTable all the elements that have a total of 0 sequenced genomes in the time
  #period of interest.
  dataSubsetting <- dataTable[,colSums(dataTable)>0]
  
  if (ncol(as.data.frame(dataSubsetting))>1) {
    #Ordering dataTable in decreasing order according to cumulative freqeuncy (%).
    dataOrdering <- as.data.frame(cbind(t(dataSubsetting),
                                        Freq=colSums(dataSubsetting)/totSeq))
    
    dataOrdering <- dataOrdering[order(dataOrdering$Freq, decreasing = T),]
    
    dataOrdering$Freq <- NULL
    
    outTable <- t(dataOrdering)
    
    return(outTable)
    
  } else if (ncol(as.data.frame(dataSubsetting))==1) {
    #If only a single feature of interest survives previous filters no ordering is required.
    regions <- row.names(dataTable)
    mutations <- names(which(colSums(dataTable)>0))
    
    outTable <- matrix(dataSubsetting)
    row.names(outTable) <- regions
    colnames(outTable) <- mutations
    
    return(outTable)
    
  }
  
}


# >>Selecting data for the input table of the Barplots (BP) in the Mutations Tab -----
#Defining the function that allows to select the data to include in the input table for the Barplots (BP)
#in the Mutations Tab and sorting them. The final table will include counts for either the first or second
#most frequent non-defining Mutation of the user-selected Lineage of interest and the total number of
#genomes without the Mutation of interest.
#This function accepts as inputs:
#1. dataTable -> The input data table.
#2. countrySel -> The ISO code of the Country which data are analyzed. Given that the input table contains
#   both national and regional counts, the countrySel input allows to select only the national data required
#   to produce the BP.
#3. totSeq -> The total number of sequenced genomes for the user-selected Lineage in the time period of interest.
#4. dataPos -> The position (index) of the Mutation of interest. For the input table of the BP its value is either
#   1 or 2.
#5. perWeek_totSequences <- The total number of genomes of the user-selected Lineage sequenced every week.
#The function gives as an output the input data table for the BP in the Mutations Tab.
mutBP_SelSorter <- function(dataTable, countrySel, totSequences, dataPos, perWeek_totSequences) {
  #Selecting national counts from the input data table.
  dataTable_countrySel <- dataTable[dataTable$region%in%countrySel,]
  row.names(dataTable_countrySel) <- dataTable_countrySel$mutation
  dataTable_countrySel$mutation <- NULL
  dataTable_countrySel$region <- NULL
  
  #Sorting counts table in decreasing order according to their comulative frequency in the time period of interest (Freq).
  dataTableSorted <- cbind(dataTable_countrySel, Freq = rowSums(dataTable_countrySel)/totSequences)
  dataTableSorted <- dataTableSorted[order(dataTableSorted$Freq, decreasing = TRUE),]
  dataTableSorted$Freq <- NULL
  
  #Selecting the mutation of interest and producing the final table.
  outTable <- rbind(dataTableSorted[dataPos,], Others = (perWeek_totSequences-dataTableSorted[dataPos,]))
  
  return(outTable)
}


# >>Producing the input table for the HeatMap in the Mutations Tab -----
#Defining the function that allows to produce the input table for the production of the
#HeatMap (HM) in the Mutations Tab. This function allows to correctly associate each Mutation
#with its regional counts. Applying the function on the complete collection of Mutations allows
#to produce a complete counts table.
#This function accepts as inputs:
#1. dataElement -> The name of the Mutation of interest.
#2. dataTable -> The input data table.
#The function gives as an output the vector of data corresponding to the Mutation of interest
#(dataElement). When applied to the complete collection of Mutations the output is a matrix of
#the data associated to all the Mutations in the selection.
mutReg_dataSelector <- function(dataElement, dataTable) {
  #Retrieving from dataTable the data corresponding to dataElement.
  elementData <- dataTable[dataTable$mutation==dataElement,]$totSeq
  
  return(elementData)
}
