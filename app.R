#CorGAT Lineages by Week App

#Upload packages and data -----
library(shiny)
source("config.R")

#Define UI -----
ui <- fluidPage(
  titlePanel("Lineages distribution by week"),
  
  #Define the layout of the output region.
  #Each plot is generated in a different panel.
  tabsetPanel(
    tabPanel("Barplot", plotOutput("byWeek_barplot")),
    tabPanel("Pie Chart", plotOutput("byWeek_piechart")),
    tabPanel("Scatterplot", plotOutput("byWeek_scatterplot"))
  ),
  
  hr(),
  
  #Define the layout of the input region.
  #Each interactive widget occupies a region of width=3 columns
  #and is accompanied by an help text briefly explaining its function.
  fluidRow(
    column(3,
           offset = 1,
           #Generates a drop down menu that allows to select.
           #a country to analyze.
           #Default is Italy.
           selectInput("country",
                       "Country",
                       choices = countryList,
                       selected = "Italy"),
           helpText("Visualize data for the selected country"),
           
           br(),
           
           #Generates a slider that allows to select a time lapse to analyze.
           #Time is calculated in weeks from a fixed date (2019-12-30).
           #Default is from week 1 to maxWeek.
           sliderInput("weeksRange",
                       "Weeks range",
                       min = 1,
                       max = maxWeek,
                       value = c(1, maxWeek)),
           helpText("Time lapse of interest (number of weeks from a fixed date)")),
    
    column(3,
           offset = 1,
           #Generates a radio buttons selection that allows to select the
           #minimum number of sequenced genomes that each lineage should
           #have to be represented in the graphics.
           #Default is 100.
           radioButtons("nGenomes",
                        "Min number of genomes",
                        choices = list("1"=1,
                                       "25"=25,
                                       "50"=50,
                                       "100"=100,
                                       "500"=500,
                                       "1000"=1000),
                        selected = 100),
           helpText("Minimum number of sequenced genomes required to display a lineage")),
    
    column(3,
           #Generates a drop down menu that allows to select a lineage
           #for which produce a scatter plot.
           #Lineages in the menu are selected among those surviving
           #previous filters. The widget changes dynamically depending
           #on the selected time lapse and n genomes threshold.
           #No default set.
           uiOutput("lineage"),
           helpText("Produce a scatterplot for the selected lineage"))
  )
)

#Define server -----
server <- function(input, output){
  #Reading the input table for the selected country.
  country_selector <- reactive({
    read.table(paste0(inputFilesPath, "Epiweek.", input$country, ".csv"),
               sep = " ",
               check.names = F)
  })
  
  #Subsetting the input table respect to the selected time lapse.
  weeks_selector <- reactive({
    country_selector()[,which(colnames(country_selector())==as.character(input$weeksRange[1])):which(colnames(country_selector())==as.character(input$weeksRange[2]))]
  })
  
  #Subsetting the input table respect to the minimum number of sequenced
  #genomes.
  nGenomes_selector <- reactive({
    #The selected time lapse MUST be > 1 week otherwise the app rises
    #a warning.
    #If a single week is selected, the input data are stored in a vector
    #(not a table), which has dim()=NULL.
    #Shiny considers a NULL value as a failure and rises a warning.
    validate(need(dim(weeks_selector()),
                  "Select a weeks range"))
    
    #Lineages with a number of sequenced genomes above the selected
    #threshold are selected.
    weeks_selector()[rowSums(weeks_selector())>=as.numeric(input$nGenomes),]
  })
  
  #Processing the input table.
  inTablePreparation <- reactive({
    #The final input table MUST contain at least 1 lineage.
    validate(need(nrow(nGenomes_selector())>0,
                  paste("0 lineages with more than",
                        input$nGenomes,
                        "sequenced genomes in the selected weeks range")))
    
    #Data are ordered in decreasing order according to the total number
    #of sequenced genomes for each lineage.
    processedData <- cbind(nGenomes_selector(),
                           Total = rowSums(nGenomes_selector()))
    
    processedData <- processedData[order(processedData$Total, decreasing = T),]
    
    processedData$Total <- NULL
    
    #If the input table contains at least 6 different lineages only the 5
    #most numerous are explicitly represented.
    #All other lineages are collapsed in a single row called Others.
    if (nrow(processedData)>6) {
     processedData <- rbind(processedData[1:5,], Others = colSums(processedData[6:nrow(processedData),])) 
    } else if (nrow(processedData)==6) {
      processedData <- rbind(processedData[1:5,], Others = processedData[6,])
    }
    
    return(processedData)
  })
  
  #Converting the input table (a data frame) into a matrix.
  inTable <- reactive({
    as.matrix(inTablePreparation())
  })
  
  #Creating a custom palette.
  inPalette <- reactive({
    #Names of the lineages collected in the input table are stored in
    #the lineages variable.
    lineages <- row.names(inTable())
    
    #A n of colors equal to the n of elements in the lineages variable is
    #is extracted from the random palette (called randomColors) defined in the
    #config.R configuration file and stored in the myColors variable.
    myColors <- randomColors[1:length(lineages)]
    
    #Checking if any of the lineages from the input table is a Variant of Concern
    #(VOC) and storing the index of TRUE values in the isVoc variable.
    isVoc <- lineages%in%names(colVoc)
    
    #If at least a single VOC is present in the lineages variable, the random color
    #(in the myColors palette) corresponding to its index is replaced by the
    #specific color assigned to that VOC from the colVoc. The latter is defined in
    #the config.R configuration file.
    if (sum(isVoc)>=1){
      myVoc <- lineages[isVoc]
      myColors[isVoc] <- colVoc[myVoc]
    }
    
    return(myColors)
  })
  
  #Generating a bar plot and the corresponding legend.
  output$byWeek_barplot <- renderPlot({
    par(mar=c(5,5,2,10), mfcol=c(1,1))
    
    barplot(inTable(),
            col=inPalette(),
            legend = TRUE,
            args.legend = list(x = "topright", bty = "n", inset=c(-0.10, 0), xpd = TRUE),
            xlab = "Week",
            ylab = "#Genomes",
            cex.lab=1.5,
            cex.axis=1.5)
  })
  
  #Generating a pie chart.
  output$byWeek_piechart <- renderPlot({
    if (nrow(inTable())>=2) {
      inPie <- rowSums(inTable())
      inLab <- row.names(inTable())
    } else {
      inPie <- sum(inTable())
      inLab <- row.names(inTable())
    }
    
    pie(inPie, labels = inLab, col = inPalette())
  })
  
  #Generating the drop down menu that allows to select a lineage.
  output$lineage <- renderUI({
    #The final input table MUST contain at least 1 lineage.
    validate(need(nrow(nGenomes_selector())>0,
                  "Can not generate lineage selection"))
    
    #The list of lineages in the menu depends on which lineages are
    #present in the input table.
    myLineages <- row.names(inTable())
    
    names(myLineages) <- row.names(inTable())
    
    selectInput("selLineage",
                "Lineage",
                choices = as.list(myLineages))

  })
  
  #Generating a scatter plot that allows to compare the lineage of interest
  #with all other lineages (if present) and the corresponding legend.
  output$byWeek_scatterplot <- renderPlot({
    #Values for the lineage of interest (selected using the appropriate widget)
    #are stored in the mainLineage variable.
    mainLineage <- inTable()[input$selLineage,]
    
    #All other lineages (if present) are collapsed together.
    if (nrow(inTable())>2) {
      secLineage <- colSums(inTable()[row.names(inTable())!=input$selLineage,])
    } else {
      secLineage <- inTable()[row.names(inTable())!=input$selLineage,]
    }

    plot(colnames(inTable())[mainLineage>0],
         mainLineage[mainLineage>0],
         type = "b",
         col = ifelse(input$selLineage%in%names(colVoc), colVoc[input$selLineage],randomColors[4]),
         lwd=3,
         pch=15,
         lty=13,
         ylim = c(0, max(colSums(inTable()))),
         main = input$selLineage,
         xlab = "Week",
         ylab = "#Genomes",
         cex.main=1.5,
         cex.lab=1.5,
         cex.axis=1.5)
    
    lines(colnames(inTable())[secLineage>0],
          secLineage[secLineage>0],
          type = "b",
          col = "darkgrey",
          lwd=3,
          pch=15,
          lty=13)
    
    legend(legend = c(input$selLineage, "Other lineages"),
           col = c(ifelse(input$selLineage%in%names(colVoc), colVoc[input$selLineage], randomColors[4]), "darkgrey"),
           pch = 15,
           cex = 1.25,
           x = "topleft",
           inset = c(0.05,0),
           bty = "n",
           xpd = TRUE)
  })
}

#Launch App
shinyApp(ui = ui, server = server)
