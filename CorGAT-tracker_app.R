#CorGAT Lineages by Week App

#Upload packages and data -----
library(shiny)
source("config.R")

#Define UI -----
ui <- fluidPage(
  titlePanel("CorGAT-tracker"),
  
  #Define the layout of the output region.
  #Each plot is generated in a different panel.
  tabsetPanel(
    tabPanel("README",
             br(),
             p("Welcome to",
               strong("CorGAT-tracker"),
               "a",
               a(href = "https://shiny.rstudio.com/", "Shiny"),
               "based app that allows the visualization of the prevalence of SARS-CoV-2 lineages and mutations of concern (MOC) as annotated by",
               a(href = "https://doi.org/10.1093/bioinformatics/btaa1047", "CorGAT"),
               ", in an interactive form.", style = "margin:0"),
             p("Several parameters can be adjusted by the user, including for example the minimum number of genomes required for a lineage to be included in the visualization (see below) and/or the time interval to consider (in weeks, see below).", style = "margin:0"),
             p("The tool produces 3 different types of plot, each contained in a different tab/panel:"),
             p(strong(">"), strong(em("Barplot panel")), style = "margin:0"),
             p("A barplot representing the total number of genomes sequenced on a weekly basis. Data are stratified by lineage, however only the top 5 most abundant lineages are represented. Other lineages are collapsed under",
               em("Others"),
               ".", style = "margin:0"),
             p(strong(">"), strong(em("Pie chart panel")), style = "margin:0"),
             p("A pie chart showing the cumulative (meaning not divided by week) prevalence of SARS-CoV-2 lineages in the interval of time selected by the user. Similar to the barplot only the top 5 most-abundant lineages are represented, while remaining lineages are collapsed under",
               em("Others"),
               ".", style = "margin:0"),
             p(strong(">"), strong(em("Scatterplot panel")), style = "margin:0"),
             p("A scatterplot that represents the number of genomic sequences associated with a user-selected lineage, collected at every time point (week, x axis). To facilitate the comparison, the total number of genomes not associated with the lineage is also reported.  Only lineages displayed in the barplot and/or pie chart panel can be selected, meaning that, for every time interval selected by the user, only the top 5 most prevalent individual lineages and the",
               em("Others"),
               "collapsed values can be represented."),
             h3("Control and customization of the plots"),
             p("Users can interact with",
               strong("CorGAT-tracker"),
               "by the means of the set of widgets displayed under the main plot area, including:",
               style = "margin:0"),
             p(strong(">"), "The",
               strong(em("Country")),
               "drop down menu, which allows the user to select the country of origin of the data to be visualized. Default is Italy.",
               style = "margin:0"),
             p(strong(">"), "The",
               strong(em("Weeks range")),
               "slider, which allows the selection of the interval of  time to be displayed. Intervals of time are computed in the form of non-overlapped windows of 7 days (or weeks if you prefer) starting from 2019-12-30 the reported date of isolation of the first SARS-CoV-2 genomic sequence. This widget defaults to the entire frame of time included in the latest version of the analysis (week 1 to current week).",
               style = "margin:0"),
             p(strong(">"), "The",
               strong(em("Min number of genomes")),
               "radio button, which enables the user to select a lower bound for the numerosity of sequenced genomes required to a lineage to be represented in the plots. Only lineages surpassing this minimum threshold will be represented. Please be aware, irrespective of the selection,",
               strong("CorGAT-tracker"),
               "is configured to allow the visualization of a maximum of 5 lineages at every time-point. Only the 5 most numerous lineages will be shown by default, remaining lineages are collapsed under",
               em("Others."),
               "Default threshold for this widget is 100.",
               style = "margin:0"),
             p(strong(">"), "The",
               strong(em("Lineage")),
               "drop down menu allows the user to select a lineage of interest to be used  in the scatter-plot. This widget is generated dynamically based on the current selection of the user. Please be aware that the lineages that are available for this type of plot might change and might not be consistent across different countries or between different intervals of time. No default value."),
             br(),
             h4("Please be aware that to present the user with a more meaningful and compact representation, CorGAT-tracker was designed explicitly to show only the five most numerous lineages for any selection performed by the user. The category",
                em("Others"),
                "is used to collapse all the lineages that do not pass the threshold on for being included in the visualization (see above,",
                em("Min number of genomes)"),
                "and/or those lineages that do not rank among the 5 most abundant. If you are dissatisfied with this behaviour and/or would like to modify the default values, please feel free to contact us at",
                em("e.ferrandi@ibiom.cnr.it."))),
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
    country_selector()[,input$weeksRange[1]:input$weeksRange[2]]
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
    
    #Data are ordered in decreasing order according to the total number
    #of sequenced genomes for each lineage.
    ordering <- cbind(weeks_selector(),
                      Total = rowSums(weeks_selector()))
    
    ordering <- ordering[order(ordering$Total, decreasing = T),]
    
    #Lineages with a number of sequenced genomes above the selected
    #threshold are stored in the nGenomes_table variable.
    nGenomes_table <- ordering[ordering$Total>=as.numeric(input$nGenomes),]
    
    #If the nGenomes_table variable contains at least a lineage a new row
    #called Others, which collapses the n genomes values of all lineages
    #below threshold, is added to the variable.
    #Otherwise the nGenomes_table is define as a dataframe of 1 row (called Others)
    #and n weeks columns, which collapses the n genomes of all lineages below
    #threshold.
    if (nrow(nGenomes_table)>=1) {
      nGenomes_table <- rbind(nGenomes_table,
                              Others = colSums(ordering[ordering$Total<as.numeric(input$nGenomes),]))
    } else {
      Others <- colSums(ordering[ordering$Total<as.numeric(input$nGenomes),])
      names(Others) <- colnames(ordering)
      nGenomes_table <- as.data.frame(t(Others), row.names = "Others")
    }
    
    return(nGenomes_table)
  })
  
  #Processing the input table.
  inTablePreparation <- reactive({
    processedData <- nGenomes_selector()
    
    #If the input table contains at least 6 different lineages only the 5
    #most numerous are explicitly represented.
    #All other lineages are collapsed in a single row called Others, which
    #already contains data from lineages below the n genomes threshold.
    if (nrow(processedData)>6) {
      processedData <- rbind(processedData[1:5,], Others = colSums(processedData[6:nrow(processedData),])) 
    } else if (nrow(processedData)==6) {
      processedData <- rbind(processedData[1:5,], Others = processedData[6,])
    }
    
    #The input table is filtered again for the minimum number of sequenced genomes.
    #In fact Others could be excluded from the representation by this filter, even
    #if it contains the by column sum of the n genomes of many lineages.
    processedData <- processedData[processedData$Total>=as.numeric(input$nGenomes),]
    
    processedData$Total <- NULL
    
    return(processedData)
  })
  
  #Converting the input table (a data frame) into a matrix.
  inTable <- reactive({
    #The final input table MUST contain at least 1 lineage.
    validate(need(nrow(inTablePreparation())>0,
                  paste("0 lineages with more than",
                        input$nGenomes,
                        "sequenced genomes in the selected weeks range")))
    
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
    par(mar=c(5,5,2,10),mfcol=c(1,1))
    
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
    
    par(mar = c(0,0,0,0))
    
    pie(inPie, labels = inLab, col = inPalette())
  })
  
  #Generating the drop down menu that allows to select a lineage.
  output$lineage <- renderUI({
    #The final input table MUST contain at least 1 lineage.
    validate(need(nrow(inTablePreparation())>0,
                  "Can not generate lineage selection"))
    
    #The list of lineages in the menu depends on which lineages are
    #present in the input table.
    myLineages <- row.names(inTable())
    
    names(myLineages) <- row.names(inTable())
    
    #Storing the lineage selected by the user in a variable called lineageDefault that,
    #if the lineage is still present in the input table, is used as a default selection
    #anytime the menu is generated.
    lineageDefault <- isolate(input$selLineage)
    
    freezeReactiveValue(input, "selLineage")
    
    selectInput("selLineage",
                "Lineage",
                choices = as.list(myLineages),
                selected = lineageDefault)

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
    
    plot(colnames(inTable()),
         mainLineage,
         type = "b",
         col = ifelse(input$selLineage%in%names(colVoc), colVoc[input$selLineage], "darksalmon"),
         lwd=3,
         pch=15,
         lty=13,
         ylim = c(0, max(colSums(inTable()))),
         main = input$selLineage,
         xlab = "Week",
         ylab = "#Genomes", cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
    
    lines(colnames(inTable()),
          secLineage,
          type = "b",
          col = "dimgray",
          lwd=3,
          pch=15,
          lty=13)
    
    legend(legend = c(input$selLineage, "Other lineages"),
           col = c(ifelse(input$selLineage%in%names(colVoc), colVoc[input$selLineage], "darksalmon"), "darkgrey"),
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