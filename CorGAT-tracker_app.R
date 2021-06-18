#CorGAT-tracker App

#Upload packages and data -----
library(shiny)
source("CorGAT-tracker_config.R")

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
               "based dashboard for the mapping of SARS-CoV-2 lineages, and mutations of concern (MOC) in space and time in an interactive form.",
               style = "margin:0"),
             p("This app facilitates the genomic surveillance of SARS-CoV-2 by reporting the prevalence of SARS-CoV-2 Pango lineages and specific mutations (known as Mutations of Concern or MOC) in the spike glycoprotein in different countries over time. To enable a more efficient tracking of emerging lineages, in",
               strong("CorGAT-tracker"),
               "Pango lineage annotations are augmented by reporting the list of MOC that are observed in a genome, but are not specific to a lineage. We call these augmented annotations Lineages+ (see below for a more detailed explanation).",
               style = "margin:0"),
             p("CorGAT-tracker incorporates data from more than 50 countries, and a total of more than 1.5M genomes deposited in the",
             a(href = "https://www.gisaid.org/", "GISAID"),
             "database."),
             p("The dashboard is composed of three different tab/panels each providing a different graphical representation of the data. The behaviour of each plot can be tweaked by the user by adjusting the parameters in the control panel at the bottom of the page.",
               style = "margin:0"),
             h3("Plots panels"),
             p(strong(">"), strong(em("Barplots panel")), style = "margin:0"),
             p("Barplots are used to represent the total number of SARS-CoV-2 genomes sequenced on a weekly basis in different countries. Three different barplots are displayed and their data are stratified by 1) Pango Lineages, 2) Lineages+ (see below) and 3) MOC Mutations respectively. To keep the visualization compact only the top 5 most abundant Lineages/Lineages+ and the 10 most numerous Mutations are represented. Remaining Lineages, Lineages+ or Mutations are aggregated under",
               em("Others."),
               style = "margin:0"),
             p(strong(">"), strong(em("Pie charts panel")), style = "margin:0"),
             p("Three pie charts are used to illustrate the cumulative prevalence of SARS-CoV-2 Lineages, Lineages+ and Mutations in a user-selected interval of time. Similar to barplots only the top 5 most-abundant Lineages/Lineages+ and the 10 most numerous Mutations are represented, while everything else is collapsed under",
               em("Others."),
               style = "margin:0"),
             p(strong(">"), strong(em("Scatterplots panel")), style = "margin:0"),
             p("Scatterplots are used to represent the number of genomic sequences associated with a user-selected Lineage or Mutation, in time (week, x axis). To facilitate the comparison, the total number of genomes not associated with the selected Lineage/Mutation is also displayed. Only Lineages and Mutations shown in the barplot and/or pie chart panel can be selected for a scatterplot, meaning that, for any selection of country and time, only the top 5 most prevalent individual Lineages and the 10 most numerous Mutations can be individually represented, all the remaining Lineages/Mutations, can still be visualized but only in a collapsed form under",
               em("Others.")),
             h3("Control and customization of the plots"),
             p("Users can interact with",
               strong("CorGAT-tracker"),
               "by means of the set of widgets under the main plot area. These include:",
               style = "margin:0"),
             p(strong(">"), "The",
               strong(em("Country")),
               "drop down menu, which allows to select the country of origin of the data to be visualized. Default is Italy.",
               style = "margin:0"),
             p(strong(">"), "The",
               strong(em("Weeks range")),
               "slider, which allows the selection of the interval of  time to be displayed. Intervals of time are computed in the form of non-overlapped windows of 7 days (or weeks if you prefer) starting from 2019-12-30 the reported date of isolation of the first SARS-CoV-2 genomic sequence. This widget defaults to the entire frame of time included in the latest version of the analysis (week 1 to current week).",
               style = "margin:0"),
             p(strong(">"), "The",
               strong(em("Min number of genomes (Lineages)")),
               "radio buttons, which enables the user to select a lower bound for the numerosity required to a Lineage to be represented in the plots. Only Lineages surpassing this minimum threshold will be represented. Please be aware, irrespective of the selection,",
               strong("CorGAT-tracker"),
               "is configured to allow the visualization of a maximum of 5 Lineages at every time-point. Only the 5 most numerous Lineages will be shown by default, remaining Lineages are collapsed under",
               em("Others."),
               "Default threshold for this widget is 100.",
               style = "margin:0"),
             p(strong(">"),
               strong(em("Min number of genomes (Lineages+)")),
               "radio buttons, which enables the user to select a lower bound for the numerosity required for a Lineage+ to be represented in the plots. Only Lineages+ surpassing this minimum threshold will be represented. In",
               strong("CorGAT-tracker"),
               "a Lineage+ is defined as a collection of genomes assigned to a Pango lineage which carry one or more Mutations of Concern in the spike glycoprotein that are not typical of that lineage (e.g. B.1.1.7+L18F). Please be aware, irrespective of the selection,",
               strong("CorGAT-tracker"),
               "is configured to allow the visualization of a maximum of 5 Lineages+ at every time-point. Only the 5 most numerous Lineages+ will be shown by default, the remaining Lineages+ are collapsed under",
               em("Others."),
               "Defaults to 15.",
               style = "margin:0"),
             p(strong(">"), "The",
               strong(em("Lineage")),
               "drop down menu allows the user to select a Lineage of interest to be Visualized in the scatter-plot. This widget is generated dynamically based on the current user selection. Please be aware that Lineages available for this type of plot might change and might not be consistent across different countries or between different intervals of time. No default value.",
               style = "margin:0"),
             p(strong(">"), "The",
               strong(em("Mutation")),
               "drop down menu allows the user to select a Mutation of interest to be represented in a scatter-plot. This widget is generated dynamically based on the current user selection. Please be aware, irrespective of the selection,",
               strong("CorGAT-tracker"),
               "is configured to allow the visualization of a maximum of 10 Mutations at every time point. Only the 10 most abundant Mutations will be shown by default, remaining Mutations are collapsed under",
               em("Others."),
               "Since the widget is generated in a dynamic manner, Mutations available for the scatter-plot might change and might not be consistent across different countries or between different intervals of time. No default value."),
             br(),
             h4("Please be aware that to present the user with a more meaningful and compact representation, CorGAT-tracker was designed explicitly to show only the five most numerous Lineages/Lineages+ for any selection performed by the user. The category",
                em("Others"),
                "is used to collapse all the Lineages/Lineages+ that do not pass this threshold (see above,",
                em("Min number of genomes (Lineages)"),
                "and",
                em("Min number of genomes (Lineages+))"),
                "and/or those Lineages/Lineages+ that do not rank among the five most abundant in the interval of time and country selected by the user. Similarly, only the ten most numerous Mutations are explicitly shown, independently from the user selection. In this case the category",
                em("Others"),
                "is used to collapse all the Mutations that do not rank among the top ten most abundant. If you are dissatisfied with this behaviour and/or would like to modify the default values, please feel free to contact me at",
                em("e.ferrandi@ibiom.cnr.it."))),
    
    #The Barplots tab contains three different barplots, one for each of the input
    #tables considered (all lineages, "special" lineages (lineages+) as annotated
    #by CorGAT and mutations).
    tabPanel("Barplots",
             plotOutput("allLin_barplot"),
             plotOutput("specialLin_barplot"),
             plotOutput("mut_barplot")),
    
    #The Pie Charts tab is organized in three different areas of width = 4 columns.
    #One contains the pie chart representing the proportion of sequenced
    #genomes for lineages in the time lapse of interest.
    #The second one contains the pie chart representing the proportion of
    #sequenced genomes for "special" lineages (lineages+) as annotated by CorGAT in the
    #time lapse of interest.
    #The last one contains a pie chart representing the proportion
    #of sequenced genomes for mutations of interest in the selected time lapse.
    tabPanel("Pie Charts",
             fluidRow(
               column(4,
                      plotOutput("allLin_piechart")),
               column(4,
                      plotOutput("specialLin_piechart")),
               column(4,
                      plotOutput("mut_piechart")))),
    
    #The scatterplot tab contains two different scatterplots, one for a user-selected
    #lineage from the all lineages input table and one for a user-selected mutation
    #from the mutations input table.
    tabPanel("Scatterplots",
             plotOutput("allLin_scatterplot"),
             plotOutput("mut_scatterplot"))
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
           radioButtons("allLin_nGenomes",
                        "Min number of genomes (Lineages)",
                        choices = list("1"=1,
                                       "25"=25,
                                       "50"=50,
                                       "100"=100,
                                       "500"=500,
                                       "1000"=1000),
                        selected = 100),
           helpText("Minimum number of sequenced genomes required to display a Lineage"),
           
           br(),
           
           #Generates a radio buttons selection that allows to select the
           #minimum number of sequenced genomes that each "special" lineage
           #(lineages+) should have to be represented in the graphics.
           #Default is 15.
           radioButtons("specialLin_nGenomes",
                        "Min number of genomes (Lineages+)",
                        choices = list("1"=1,
                                       "5"=5,
                                       "10"=10,
                                       "15"=15,
                                       "25"=25,
                                       "50"=50),
                        selected = 15),
           helpText("Minimum number of sequenced genomes required to display a Lineage+")),
    
    column(3,
           #Generates a drop down menu that allows to select a lineage
           #for which produce a scatter plot.
           #Lineages in the menu are selected among those surviving
           #previous filters. The widget changes dynamically depending
           #on the selected time lapse and n genomes threshold.
           #No default set.
           uiOutput("lineage"),
           helpText("Produce a scatterplot for the selected Lineage"),
           
           br(),
           
           #Generates a drop down menu that allows to select a mutation
           #for which produce a scatter plot.
           #Mutations in the menu are selected among those surviving
           #previous filters. The widget changes dynamically depending
           #on the selected time lapse.
           #No default set.
           uiOutput("mutation"),
           helpText("Produce a scatterplot for the selected Mutation")
           )
  )
)

#Define server -----
server <- function(input, output){
  #Reading the input tables for the selected countries.
  country_selector <- reactive({
    allLin_countrySel <- read.table(paste0(allLin_inputFilesPath, "Epiweek.", input$country, ".csv"),
                                    sep = " ",
                                    check.names = F)
    
    specialLin_countrySel <- read.table(paste0(specialLin_inputFilesPath, "Epiweek.SpecialLin.", input$country, ".csv"),
                             sep = " ",
                             check.names = F)
    
    mut_countrySel <- read.table(paste0(mut_inputFilesPath, "Epiweek.MutsSpike.", input$country, ".csv"),
                                 sep = " ",
                                 check.names = F)
    
    return(list(allLin=allLin_countrySel,
                specialLin=specialLin_countrySel,
                mut=mut_countrySel))
  })
  
  #Subsetting the input tables respect to the selected time lapse.
  weeks_selector <- reactive({
    allLin_weeksSel <- country_selector()$allLin[,input$weeksRange[1]:input$weeksRange[2]]
    
    specialLin_weeksSel <- country_selector()$specialLin[,input$weeksRange[1]:input$weeksRange[2]]
    
    mut_weeksSel <- country_selector()$mut[,input$weeksRange[1]:input$weeksRange[2]]
    
    return(list(allLin=allLin_weeksSel,
                specialLin=specialLin_weeksSel,
                mut=mut_weeksSel))
  })
  
  #Subsetting the input tables for all lineages and "special" lineages respect
  #to the minimum number of sequenced genomes.
  nGenomes_selector <- reactive({
    #The selected time lapse MUST be >= 1 week otherwise the app rises
    #a warning.
    validate(need((input$weeksRange[2]-input$weeksRange[1])>=1,
                  "Select a Weeks range"))
    
    #Data from the all lineages input table are processed in order to
    #select the minimum number of sequenced genomes required to a lineage
    #to be represented. The whole process is described below.
    #Data are ordered in decreasing order according to the total number
    #of sequenced genomes for each lineage.
    allLin_ordering <- cbind(weeks_selector()$allLin,
                      Total = rowSums(weeks_selector()$allLin))
    
    allLin_ordering <- allLin_ordering[order(allLin_ordering$Total, decreasing = T),]
    
    #Lineages with a number of sequenced genomes above the selected
    #threshold are stored in the allLin_nGenomes_table variable.
    allLin_nGenomes_table <- allLin_ordering[allLin_ordering$Total>=as.numeric(input$allLin_nGenomes),]
    
    #If the allLin_nGenomes_table variable contains at least a lineage, a new row
    #called Others, which collapses the n genomes values of all lineages
    #below threshold, is added to the variable.
    #Otherwise the allLin_nGenomes_table is define as a dataframe of 1 row (called Others)
    #and n weeks columns, which collapses the n genomes of all lineages below
    #threshold.
    if (nrow(allLin_nGenomes_table)>=1) {
      allLin_nGenomes_table <- rbind(allLin_nGenomes_table,
                              Others = colSums(allLin_ordering[allLin_ordering$Total<as.numeric(input$allLin_nGenomes),]))
    } else {
      allLin_Others <- colSums(allLin_ordering[allLin_ordering$Total<as.numeric(input$allLin_nGenomes),])
      names(allLin_Others) <- colnames(allLin_ordering)
      allLin_nGenomes_table <- as.data.frame(t(allLin_Others), row.names = "Others")
    }
    
    #Data from the "special" lineages (lineages+) input table are processed in the same way
    #explained above.
    specialLin_ordering <- cbind(weeks_selector()$specialLin,
                             Total = rowSums(weeks_selector()$specialLin))
    
    specialLin_ordering <- specialLin_ordering[order(specialLin_ordering$Total, decreasing = T),]
    
    specialLin_nGenomes_table <- specialLin_ordering[specialLin_ordering$Total>=as.numeric(input$specialLin_nGenomes),]
    
    if (nrow(specialLin_nGenomes_table)>=1) {
      specialLin_nGenomes_table <- rbind(specialLin_nGenomes_table,
                                     Others = colSums(specialLin_ordering[specialLin_ordering$Total<as.numeric(input$specialLin_nGenomes),]))
    } else {
      specialLin_Others <- colSums(specialLin_ordering[specialLin_ordering$Total<as.numeric(input$specialLin_nGenomes),])
      names(specialLin_Others) <- colnames(specialLin_ordering)
      specialLin_nGenomes_table <- as.data.frame(t(specialLin_Others), row.names = "Others")
    }
    
    return(list(allLin=allLin_nGenomes_table,
                specialLin=specialLin_nGenomes_table))
  })
  
  #Processing the input tables.
  inTablePreparation <- reactive({
    #Data from the all lineages input table are processed first. The whole
    #process is explained below.
    allLin_processedData <- nGenomes_selector()$allLin
    
    #If the input table contains at least 6 different lineages only the 5
    #most numerous are explicitly represented.
    #All other lineages are collapsed in a single row called Others, which
    #already contains data from lineages below the n genomes threshold.
    if (nrow(allLin_processedData)>6) {
      allLin_processedData <- rbind(allLin_processedData[1:5,], Others = colSums(allLin_processedData[6:nrow(allLin_processedData),])) 
    } else if (nrow(allLin_processedData)==6) {
      allLin_processedData <- rbind(allLin_processedData[1:5,], Others = allLin_processedData[6,])
    }
    
    #The input table is filtered again for the minimum number of sequenced genomes.
    #In fact Others could be excluded from the representation by this filter, even
    #if it contains the by column sum of the n genomes of many lineages.
    allLin_processedData <- allLin_processedData[allLin_processedData$Total>=as.numeric(input$allLin_nGenomes),]
    
    allLin_processedData$Total <- NULL
    
    #Data from the "special" lineages (lineages+) input table are processed using the same
    #method explained above.
    specialLin_processedData <- nGenomes_selector()$specialLin
    
    if (nrow(specialLin_processedData)>6) {
      specialLin_processedData <- rbind(specialLin_processedData[1:5,], Others = colSums(specialLin_processedData[6:nrow(specialLin_processedData),])) 
    } else if (nrow(specialLin_processedData)==6) {
      specialLin_processedData <- rbind(specialLin_processedData[1:5,], Others = specialLin_processedData[6,])
    }
    
    specialLin_processedData <- specialLin_processedData[specialLin_processedData$Total>=as.numeric(input$specialLin_nGenomes),]
    
    specialLin_processedData$Total <- NULL
    
    #Data from the mutations input table are ordered in decreasing order and
    #processed such that only the 10 most numerous mutations are explicitly
    #represented. All others are collapsed in the Others variable.
    mut_ordering <- cbind(weeks_selector()$mut,
                             Total = rowSums(weeks_selector()$mut))
    
    mut_ordering <- mut_ordering[order(mut_ordering$Total, decreasing = T),]
    
    if (nrow(mut_ordering)>11) {
      mut_processedData <- rbind(mut_ordering[1:10,], Others = colSums(mut_ordering[11:nrow(mut_ordering),]))
    } else if (nrow(mut_ordering)==11) {
      mut_processedData <- rbind(mut_ordering[1:10,], Others = mut_ordering[11,])
    } else {
      mut_processedData <- mut_ordering
    }
    
    mut_processedData$Total <- NULL
    
    return(list(allLin=allLin_processedData,
                specialLin=specialLin_processedData,
                mut=mut_processedData))
  })
  
  #Converting the input tables (data frames) into a matrices.
  inTable <- reactive({
    allLin_matrix <- as.matrix(inTablePreparation()$allLin)
    specialLin_matrix <- as.matrix(inTablePreparation()$specialLin)
    mut_matrix <- as.matrix(inTablePreparation()$mut)
    
    return(list(allLin=allLin_matrix,
                specialLin=specialLin_matrix,
                mut=mut_matrix))
  })
  
  #Creating a custom palette.
  inPalette <- reactive({
    #Names of the lineages collected in the all lineages input table are stored in
    #the allLin_lineages variable.
    allLin_lineages <- row.names(inTable()$allLin)
    
    #A n of colors equal to the n of elements in the allLin_lineages variable is
    #is extracted from the random palette (called randomColors) defined in the
    #config.R configuration file and stored in the allLin_myColors variable.
    allLin_myColors <- randomColors[1:length(allLin_lineages)]
    
    #Checking if any of the lineages from the input table is a Variant of Concern
    #(VOC) and storing the index of TRUE values in the isVoc variable.
    isVoc <- allLin_lineages%in%names(colVoc)
    
    #If at least a single VOC is present in the allLin_lineages variable, the random color
    #(in the allLin_myColors palette) corresponding to its index is replaced by the
    #specific color assigned to that VOC from the colVoc. The latter is defined in
    #the config.R configuration file.
    if (sum(isVoc)>=1){
      myVoc <- allLin_lineages[isVoc]
      allLin_myColors[isVoc] <- colVoc[myVoc]
    }
    
    #A random palette is produced also for the "special" lineages (lineages+) input table.
    specialLin_myColors <- randomColors[1:nrow(inTable()$specialLin)]
    
    if ("Others"%in%row.names(inTable()$specialLin)) {
      specialLin_myColors[row.names(inTable()$specialLin)%in%"Others"] <- "mediumblue"
    }
    
    #Finally a palette is produced for the mutations input table. Mutations Of Concern
    #(MOC) have pre-determined colors which are defined by the colMoc variable
    #in the config.R configuration file. The process is similar to the one used for VOC
    #(see above)
    allMut <- row.names(inTable()$mut)
    
    mut_myColors <- mut_randomColors[1:nrow(inTable()$mut)]
    
    isMoc <- allMut%in%names(colMoc)
    
    if (length(isMoc)>=1) {
      myMoc <- allMut[isMoc]
      mut_myColors[isMoc] <- colMoc[myMoc]
    }
    
    return(list(allLinColors=allLin_myColors,
                specialLinColors=specialLin_myColors,
                mutColors=mut_myColors))
  })
  
  #Generating a bar plot and the corresponding legend for the all lineages
  #input table.
  output$allLin_barplot <- renderPlot({
    #The final input table MUST contain at least 1 lineage.
    validate(need(nrow(inTable()$allLin)>0,
                  paste("0 Lineages with more than",
                        input$allLin_nGenomes,
                        "sequenced genomes in the selected weeks range")))
    
    par(mar=c(5,5,2,10), mfcol=c(1,1))
    
    barplot(inTable()$allLin,
            col=inPalette()$allLinColors,
            legend = TRUE,
            args.legend = list(x = "topright", bty = "n", inset=c(-0.10, 0), xpd = TRUE),
            xlab = "Week",
            ylab = "#Genomes",
            main = "Lineages",
            cex.lab=1.5,
            cex.axis=1.5,
            cex.main=1.5)
  })
  
  #Generating a bar plot and the corresponding legend for the "special" lineages
  #(lineages+) input table.
  output$specialLin_barplot <- renderPlot({
    #The final input table MUST contain at least 1 lineage.
    validate(need(nrow(inTable()$specialLin)>0,
                  paste("0 Lineages+ with more than",
                        input$specialLin_nGenomes,
                        "sequenced genomes in the selected weeks range")))
    
    par(mar=c(5,5,2,10), mfcol=c(1,1))
    
    barplot(inTable()$specialLin,
            col=inPalette()$specialLinColors,
            legend = TRUE,
            args.legend = list(x = "topright", bty = "n", inset=c(-0.12, 0), xpd = TRUE, cex = 0.80),
            xlab = "Week",
            ylab = "#Genomes",
            main = "Lineages+",
            cex.lab=1.5,
            cex.axis=1.5,
            cex.main=1.5 )
  })
  
  #Generating a bar plot and the corresponding legend for the mutations
  #input table.
  output$mut_barplot <- renderPlot({
    #The final input table MUST contain at least 1 mutation.
    validate(need(sum(inTable()$mut)>0,
                  "0 Mutations of interest in the selected weeks range"))
    
    par(mar=c(5,5,2,10), mfcol=c(1,1))
    
    barplot(inTable()$mut,
            col=inPalette()$mutColors,
            legend = TRUE,
            args.legend = list(x = "topright", bty = "n", inset=c(-0.10, 0), xpd = TRUE),
            xlab = "Week",
            ylab = "#Genomes",
            main = "Mutations",
            cex.lab=1.5,
            cex.axis=1.5,
            cex.main=1.5 )
  })
  
  #Generating a pie chart for all lineages input table.
  output$allLin_piechart <- renderPlot({
    #The final input table MUST contain at least 1 lineage.
    validate(need(nrow(inTable()$allLin)>0,
                  paste("0 Lineages with more than",
                        input$allLin_nGenomes,
                        "sequenced genomes in the selected weeks range")))
    
    if (nrow(inTable()$allLin)>=2) {
      allLin_inPie <- rowSums(inTable()$allLin)
      allLin_inLab <- row.names(inTable()$allLin)
    } else {
      allLin_inPie <- sum(inTable()$allLin)
      allLin_inLab <- row.names(inTable()$allLin)
    }
    
    par(mar = c(0,0,0,0), xpd = T)
    
    pie(allLin_inPie, labels = NA, col = inPalette()$allLinColors)
    legend("bottomleft", legend = allLin_inLab, fill = inPalette()$allLinColors, bty = "n", cex = 0.80)
    title(main = "Lineages", cex.main = 1.5, line = -1.5)
  })
  
  #Generating a pie chart for "special" lineages (lineages+) input table.
  output$specialLin_piechart <- renderPlot({
    #The final input table MUST contain at least 1 lineage.
    validate(need(nrow(inTable()$specialLin)>0,
                  paste("0 Lineages+ with more than",
                        input$specialLin_nGenomes,
                        "sequenced genomes in the selected weeks range")))
    
    if (nrow(inTable()$specialLin)>=2) {
      specialLin_inPie <- rowSums(inTable()$specialLin)
      specialLin_inLab <- row.names(inTable()$specialLin)
    } else {
      specialLin_inPie <- sum(inTable()$specialLin)
      specialLin_inLab <- row.names(inTable()$specialLin)
    }
    
    par(mar = c(0,0,0,0), xpd = T)
    
    pie(specialLin_inPie, labels = NA, col = inPalette()$specialLinColors)
    legend("bottomleft", legend = specialLin_inLab, fill = inPalette()$specialLinColors, bty = "n", cex = 0.80)
    title(main = "Lineages+", cex.main = 1.5, line = -1.5)
    
  })
  
  #Generating a pie chart for mutations input table.
  output$mut_piechart <- renderPlot({
    #The final input table MUST contain at least 1 mutation.
    validate(need(sum(inTable()$mut)>0,
                  "0 Mutations of interest in the selected weeks range"))
    
    if (nrow(inTable()$mut)>=2) {
      mut_inPie <- rowSums(inTable()$mut)
      mut_inLab <- row.names(inTable()$mut)
    } else {
      mut_inPie <- sum(inTable()$mut)
      mut_inLab <- row.names(inTable()$mut)
    }
    
    par(mar = c(0,0,0,0), xpd = T)
    
    pie(mut_inPie, labels = NA, col = inPalette()$mutColors)
    legend("bottomleft", legend = mut_inLab, fill = inPalette()$mutColors, bty = "n", cex = 0.80)
    title(main = "Mutations", cex.main = 1.5, line = -1.5)
  })
  
  #Generating the drop down menu that allows to select a lineage.
  output$lineage <- renderUI({
    #The final input table MUST contain at least 1 lineage.
    validate(need(nrow(inTable()$allLin)>0,
                  "Can not generate Lineage selection"))
    
    #The list of lineages in the menu depends on which lineages are
    #present in the input table (lineages).
    myLineages <- row.names(inTable()$allLin)
    
    names(myLineages) <- row.names(inTable()$allLin)
    
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
  
  #Generating the drop down menu that allows to select a mutation.
  output$mutation <- renderUI({
    #The final input table MUST contain at least 1 mutation.
    validate(need(sum(inTable()$mut)>0,
                  "Can not generate Mutation selection"))
    
    #The list of mutations in the menu depends on which mutations are
    #present in the input table.
    myMutations <- row.names(inTable()$mut)
    
    names(myMutations) <- row.names(inTable()$mut)
    
    #Storing the mutation selected by the user in a variable called mutationDefault that,
    #if the mutation is still present in the input table, is used as a default selection
    #anytime the menu is generated.
    mutationDefault <- isolate(input$selMutation)
    
    freezeReactiveValue(input, "selMutation")
    
    selectInput("selMutation",
                "Mutation",
                choices = as.list(myMutations),
                selected = mutationDefault)
    
  })
  
  #Generating a scatter plot that allows to compare the lineage of interest
  #with all other lineages (if present) and the corresponding legend.
  output$allLin_scatterplot <- renderPlot({
    #The final input table MUST contain at least 1 lineage.
    validate(need(nrow(inTable()$allLin)>0,
                  paste("0 Lineages with more than",
                        input$allLin_nGenomes,
                        "sequenced genomes in the selected weeks range")))
    
    #Values for the lineage of interest (selected using the appropriate widget)
    #are stored in the mainLineage variable.
    mainLineage <- inTable()$allLin[input$selLineage,]
    
    #All other lineages (if present) are collapsed together.
    if (nrow(inTable()$allLin)>2) {
      secLineage <- colSums(inTable()$allLin[row.names(inTable()$allLin)!=input$selLineage,])
    } else {
      secLineage <- inTable()$allLin[row.names(inTable()$allLin)!=input$selLineage,]
    }
    
    plot(colnames(inTable()$allLin),
         mainLineage,
         type = "b",
         col = ifelse(input$selLineage%in%names(colVoc), colVoc[input$selLineage], "darksalmon"),
         lwd=3,
         pch=15,
         lty=13,
         ylim = c(0, max(colSums(inTable()$allLin))),
         main = input$selLineage,
         xlab = "Week",
         ylab = "#Genomes", cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
    
    
    if (length(secLineage)!=0) {
      lines(colnames(inTable()$allLin),
            secLineage,
            type = "b",
            col = "dimgray",
            lwd=3,
            pch=15,
            lty=13)
    }
    
    legend(legend = c(input$selLineage, "Not selected lineages"),
           col = c(ifelse(input$selLineage%in%names(colVoc), colVoc[input$selLineage], "darksalmon"), "darkgrey"),
           pch = 15,
           cex = 1.25,
           x = "topleft",
           inset = c(0.05,0),
           bty = "n",
           xpd = TRUE)
    
  })
  
  #Generating a scatter plot that allows to compare the mutation of interest
  #with all other mutations (if present) and the corresponding legend.
  output$mut_scatterplot <- renderPlot({
    #The final input table MUST contain at least 1 mutation.
    validate(need(sum(inTable()$mut)>0,
                  "0 Mutations of interest in the selected weeks range"))
    
    #Values for the mutation of interest (selected using the appropriate widget)
    #are stored in the mainMutation variable.
    mainMutation <- inTable()$mut[input$selMutation,]
    
    #All other mutations (if present) are collapsed together.
    if (nrow(inTable()$mut)>2) {
      secMutation <- colSums(inTable()$mut[row.names(inTable()$mut)!=input$selMutation,])
    } else {
      secMutation <- inTable()$mut[row.names(inTable()$mut)!=input$selMutation,]
    }
    
    plot(colnames(inTable()$mut),
         mainMutation,
         type = "b",
         col = ifelse(input$selMutation%in%names(colMoc), colMoc[input$selMutation], "darksalmon"),
         lwd=3,
         pch=15,
         lty=13,
         ylim = c(0, max(colSums(inTable()$mut))),
         main = input$selMutation,
         xlab = "Week",
         ylab = "#Genomes", cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
    
    if (length(secMutation)!=0) {
      lines(colnames(inTable()$mut),
            secMutation,
            type = "b",
            col = "dimgray",
            lwd=3,
            pch=15,
            lty=13)
    }
    
    legend(legend = c(input$selMutation, "Not selected mutations"),
           col = c(ifelse(input$selMutation%in%names(colMoc), colMoc[input$selMutation], "darksalmon"), "darkgrey"),
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