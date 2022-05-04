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
    #tables considered (all Lineages, Lineages+ as annotated
    #by CorGAT and Mutations).
    tabPanel("Barplots",
             plotOutput("allLin_barplot"),
             plotOutput("specialLin_barplot"),
             plotOutput("mut_barplot")),
    
    #The Pie Charts tab is organized in three different areas of width = 4 columns.
    #One contains the pie chart representing the proportion of sequenced
    #genomes for Lineages in the time lapse of interest.
    #The second one contains the pie chart representing the proportion of
    #sequenced genomes for Lineages+ as annotated by CorGAT in the
    #time lapse of interest.
    #The last one contains a pie chart representing the proportion
    #of sequenced genomes for Mutations of interest in the selected time lapse.
    tabPanel("Pie Charts",
             fluidRow(
               column(4,
                      plotOutput("allLin_piechart")),
               column(4,
                      plotOutput("specialLin_piechart")),
               column(4,
                      plotOutput("mut_piechart")))),
    
    #The scatterplot tab contains two different scatterplots, one for a user-selected
    #Lineage from the all Lineages input table and one for a user-selected Mutation
    #from the Mutations input table.
    tabPanel("Scatterplots",
             plotOutput("allLin_scatterplot"),
             plotOutput("mut_scatterplot")),
    
    #The Lineage+ VS Lineage tab contains two different barplots:
    #1. One allows the comparison between the frequency
    #(%) of a Lineage+ of interest and the "parental" Lineage it
    #derives from.
    #2. The other displays the total number of genome sequences
    #available for each week of the time period of interest.
    tabPanel("Lineage+ VS Lineage",
             plotOutput("spLvsL_barplot"),
             plotOutput("perWeek_totGenomes_barplot"))
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
           helpText("Time lapse of interest (number of weeks from a fixed date)"),
           
           br(),
           
           #Generates a radio buttons selection that allows to select the
           #minimum number of sequenced genomes required to each Lineage
           #to be represented in the graphics.
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
           helpText("Minimum number of sequenced genomes required to display a Lineage")),
    
    column(3,
           offset = 1,
           #Generates a radio buttons selection that allows to select the
           #minimum number of sequenced genomes required to each Lineage+
           #to be represented in the graphics.
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
           helpText("Minimum number of sequenced genomes required to display a Lineage+"),
           
           br(),
           
           #Generates a radio button selection that allows so selected the minimum
           #frequency (%) required to each Lineage+ to be selected to produce
           #a Lineage+ VS Lineage barplot.
           #Default is 1%.
           radioButtons("specialLin_pctGenomes",
                        "Min % of genomes (Lineages+)",
                        choices = list("1%"=0.01,
                                       "2,5%"=0.025,
                                       "5%"=0.05,
                                       "10%"=0.1),
                        selected = 0.01),
           helpText("Minimum frequency (%) required to a Lineage+ to be selected")),
    
    column(3,
           #Generates a drop down menu that allows to select a Lineage
           #for which produce a scatter plot.
           #Lineages in the menu are selected among those surviving
           #previous filters. The widget changes dynamically depending
           #on the selected time lapse and n genomes threshold.
           #No default set.
           uiOutput("lineage"),
           helpText("Produce a scatterplot for the selected Lineage"),
           
           br(),
           
           #Generates a drop down menu that allows to select a Mutation
           #for which produce a scatter plot.
           #Mutations in the menu are selected among those surviving
           #previous filters. The widget changes dynamically depending
           #on the selected time lapse.
           #No default set.
           uiOutput("mutation"),
           helpText("Produce a scatterplot for the selected Mutation"),
           
           br(),
           
           #Generates a drop down menu that allows to select the Lineage+ (and,
           #consequentially, its "parent" Lineage) for which produce the
           #corresponding Lineage+ VS Lineage barplot.
           #Lineages+ in the menu are selected among those surviving previous
           #filters. The widget changes dynamically depending on the selected
           #time lapse and the prevalence (%) threshold.
           #No default set.
           uiOutput("specialLineage"),
           helpText("Produce a Lineage+ VS Lineage barplot for the selected Lineage+"))
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
    
    return(list(allLin = allLin_countrySel,
                specialLin = specialLin_countrySel,
                mut = mut_countrySel))
  })
  
  #Subsetting the input tables respect to the selected time lapse.
  weeks_selector <- reactive({
    allLin_weeksSel <- country_selector()$allLin[,input$weeksRange[1]:input$weeksRange[2]]
    
    specialLin_weeksSel <- country_selector()$specialLin[,input$weeksRange[1]:input$weeksRange[2]]
    
    mut_weeksSel <- country_selector()$mut[,input$weeksRange[1]:input$weeksRange[2]]
    
    return(list(allLin = allLin_weeksSel,
                specialLin = specialLin_weeksSel,
                mut = mut_weeksSel))
  })
  
  #Subsetting the input tables for all Lineages and Lineages+ respect
  #to the minimum number of sequenced genomes.
  nGenomes_selector <- reactive({
    #The selected time lapse MUST be >= 1 week otherwise the app rises
    #a warning.
    validate(need((input$weeksRange[2]-input$weeksRange[1])>=1,
                  "Select a Weeks range"))
    
    #Data from the all Lineages input table are processed in order to
    #select the minimum number of sequenced genomes required to a Lineage
    #to be represented. The whole process is described below.
    #Data are ordered in decreasing order according to the total number
    #of sequenced genomes for each Lineage.
    allLin_ordering <- cbind(weeks_selector()$allLin,
                             Total = rowSums(weeks_selector()$allLin))
    
    allLin_ordering <- allLin_ordering[order(allLin_ordering$Total, decreasing = T),]
    
    #Lineages with a number of sequenced genomes above the selected
    #threshold are stored in the allLin_nGenomes_table variable.
    allLin_nGenomes_table <- allLin_ordering[allLin_ordering$Total>=as.numeric(input$allLin_nGenomes),]
    
    #If the allLin_nGenomes_table variable contains at least a Lineage, a new row
    #called Others, which collapses the n genomes values of all Lineages
    #below threshold, is added to the variable.
    #Otherwise the allLin_nGenomes_table is define as a dataframe of 1 row (called Others)
    #and n weeks columns, which collapses the n genomes of all Lineages below
    #threshold.
    if (nrow(allLin_nGenomes_table)>=1) {
      allLin_nGenomes_table <- rbind(allLin_nGenomes_table,
                                     Others = colSums(allLin_ordering[allLin_ordering$Total<as.numeric(input$allLin_nGenomes),]))
    } else {
      allLin_Others <- colSums(allLin_ordering[allLin_ordering$Total<as.numeric(input$allLin_nGenomes),])
      names(allLin_Others) <- colnames(allLin_ordering)
      allLin_nGenomes_table <- as.data.frame(t(allLin_Others), row.names = "Others")
    }
    
    #Data from the Lineages+ input table are processed in the same way
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
    
    return(list(allLin = allLin_nGenomes_table,
                specialLin = specialLin_nGenomes_table))
  })
  
  #Processing the input tables and converting them to matrices.
  inTable <- reactive({
    #Data from the all Lineages input table are processed first. The whole
    #process is explained below.
    allLin_processedData <- nGenomes_selector()$allLin
    
    #If the input table contains at least 6 different Lineages only the 5
    #most numerous are explicitly represented.
    #All other Lineages are collapsed in a single row called Others, which
    #already contains data from lineages below the n genomes threshold.
    if (nrow(allLin_processedData)>6) {
      allLin_processedData <- rbind(allLin_processedData[1:5,], Others = colSums(allLin_processedData[6:nrow(allLin_processedData),])) 
    } else if (nrow(allLin_processedData)==6) {
      allLin_processedData <- rbind(allLin_processedData[1:5,], Others = allLin_processedData[6,])
    }
    
    #The input table is filtered again for the minimum number of sequenced genomes.
    #In fact Others could be excluded from the representation by this filter, even
    #if it contains the by column sum of the n genomes of many Lineages.
    allLin_processedData <- allLin_processedData[allLin_processedData$Total>=as.numeric(input$allLin_nGenomes),]
    
    allLin_processedData$Total <- NULL
    
    #Data from the Lineages+ input table are processed using the same
    #method explained above.
    specialLin_processedData <- nGenomes_selector()$specialLin
    
    if (nrow(specialLin_processedData)>6) {
      specialLin_processedData <- rbind(specialLin_processedData[1:5,], Others = colSums(specialLin_processedData[6:nrow(specialLin_processedData),])) 
    } else if (nrow(specialLin_processedData)==6) {
      specialLin_processedData <- rbind(specialLin_processedData[1:5,], Others = specialLin_processedData[6,])
    }
    
    specialLin_processedData <- specialLin_processedData[specialLin_processedData$Total>=as.numeric(input$specialLin_nGenomes),]
    
    specialLin_processedData$Total <- NULL
    
    #Data from the Mutations input table are ordered in decreasing order and
    #processed such that only the 10 most numerous Mutations are explicitly
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
    
    allLin_matrix <- as.matrix(allLin_processedData)
    specialLin_matrix <- as.matrix(specialLin_processedData)
    mut_matrix <- as.matrix(mut_processedData)
    
    return(list(allLin = allLin_matrix,
                specialLin = specialLin_matrix,
                mut = mut_matrix))
  })
  
  #Creating a custom palette.
  inPalette <- reactive({
    #Names of the Lineages collected in the all Lineages input table are stored in
    #the allLin_lineages variable.
    allLin_lineages <- row.names(inTable()$allLin)
    
    #A n of colors equal to the n of elements in the allLin_lineages variable is
    #is extracted from the random palette (called randomColors) defined in the
    #config.R configuration file and stored in the allLin_myColors variable.
    allLin_myColors <- randomColors[1:length(allLin_lineages)]
    
    #Checking if any of the Lineages from the input table is a Variant of Concern
    #(VOC) and storing the index of TRUE values in the isVoc variable.
    isVoc <- allLin_lineages%in%row.names(colVoc)
    
    #If at least a single VOC is present in the allLin_lineages variable, the random color
    #(in the allLin_myColors palette) corresponding to its index is replaced by the
    #specific color assigned to that VOC from the colVoc table. The latter is defined in
    #the config.R configuration file.
    if (sum(isVoc)>=1){
      myVoc <- allLin_lineages[isVoc]
      allLin_myColors[isVoc] <- colVoc[myVoc,1]
    }
    
    #A random palette is produced also for the Lineages+ input table.
    specialLin_myColors <- randomColors[1:nrow(inTable()$specialLin)]
    
    if ("Others"%in%row.names(inTable()$specialLin)) {
      specialLin_myColors[row.names(inTable()$specialLin)%in%"Others"] <- "mediumblue"
    }
    
    #Finally a palette is produced for the Mutations input table. Mutations Of Concern
    #(MOC) have pre-determined colors which are defined by the colMoc table
    #in the config.R configuration file. The process is similar to the one used for VOC
    #(see above)
    allMut <- row.names(inTable()$mut)
    
    mut_myColors <- mut_randomColors[1:nrow(inTable()$mut)]
    
    isMoc <- allMut%in%row.names(colMoc)
    
    if (length(isMoc)>=1) {
      myMoc <- allMut[isMoc]
      mut_myColors[isMoc] <- colMoc[myMoc,1]
    }
    
    return(list(allLinColors = allLin_myColors,
                specialLinColors = specialLin_myColors,
                mutColors = mut_myColors))
  })
  
  #Generating a bar plot and the corresponding legend for the all Lineages
  #input table.
  output$allLin_barplot <- renderPlot({
    #The final input table MUST contain at least 1 Lineage.
    validate(need(nrow(inTable()$allLin)>0,
                  paste("0 Lineages with more than",
                        input$allLin_nGenomes,
                        "sequenced genomes in the selected weeks range")))
    
    par(mar = c(4,5,2,0), fig = c(0,0.75,0,1))
    
    barplot(inTable()$allLin,
            col=inPalette()$allLinColors,
            xlab = "Week",
            ylab = "#Genomes",
            main = "Lineages",
            cex.lab = 1.5,
            cex.axis = 1.5,
            cex.main = 1.5)
    
    #Generating the legend in a secondary
    #space of the plot area.
    par(mar = c(0,0,0,0), fig = c(0.75,1,0,1), new = T)
    
    barplot(0,
            xaxt = "n",
            yaxt = "n",
            col.axis = "white",
            border = "white")
    
    legend(legend = row.names(inTable()$allLin),
           fill = inPalette()$allLinColors,
           cex = 0.85,
           x = "topleft",
           bty = "n",
           xpd = TRUE)
  })
  
  #Generating a bar plot and the corresponding legend for the Lineages+
  #input table.
  output$specialLin_barplot <- renderPlot({
    #The final input table MUST contain at least 1 Lineage+.
    validate(need(nrow(inTable()$specialLin)>0,
                  paste("0 Lineages+ with more than",
                        input$specialLin_nGenomes,
                        "sequenced genomes in the selected weeks range")))
    
    par(mar = c(4,5,2,0), fig = c(0,0.75,0,1))
    
    barplot(inTable()$specialLin,
            col=inPalette()$specialLinColors,
            xlab = "Week",
            ylab = "#Genomes",
            main = "Lineages+",
            cex.lab = 1.5,
            cex.axis = 1.5,
            cex.main = 1.5 )
    
    #Generating the legend in a secondary
    #space of the plot area.
    par(mar = c(0,0,0,0), fig = c(0.75,1,0,1), new = T)
    
    barplot(0,
            xaxt = "n",
            yaxt = "n",
            col.axis = "white",
            border = "white")
    
    legend(legend = row.names(inTable()$specialLin),
           fill = inPalette()$specialLinColors,
           cex = 0.85,
           x = "topleft",
           bty = "n",
           xpd = TRUE)
  })
  
  #Generating a bar plot and the corresponding legend for the Mutations
  #input table.
  output$mut_barplot <- renderPlot({
    #The final input table MUST contain at least 1 Mutation.
    validate(need(sum(inTable()$mut)>0,
                  "0 Mutations of interest in the selected weeks range"))
    
    par(mar = c(4,5,2,0), fig = c(0,0.75,0,1))
    
    barplot(inTable()$mut,
            col=inPalette()$mutColors,
            xlab = "Week",
            ylab = "#Genomes",
            main = "Mutations",
            cex.lab = 1.5,
            cex.axis = 1.5,
            cex.main = 1.5 )
    
    #Generating the legend in a secondary
    #space of the plot area.
    par(mar = c(0,0,0,0), fig = c(0.75,1,0,1), new = T)
    
    barplot(0,
            xaxt = "n",
            yaxt = "n",
            col.axis = "white",
            border = "white")
    
    legend(legend = row.names(inTable()$mut),
           fill = inPalette()$mutColors,
           cex = 0.85,
           x = "topleft",
           bty = "n",
           xpd = TRUE)
  })
  
  #Generating a pie chart for all Lineages input table.
  output$allLin_piechart <- renderPlot({
    #The final input table MUST contain at least 1 Lineage.
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
    
    par(mar = c(0,0,0,0), fig = c(0,1,0.2,1))
    
    pie(allLin_inPie, labels = NA, col = inPalette()$allLinColors)
    title(main = "Lineages", cex.main = 1.5, line = -1.5)
    
    #Generating the legend in a secondary
    #space of the plot area.
    par(mar = c(0,0,0,0), fig = c(0,1,0,0.2), new = T)
    
    barplot(0,
            xaxt = "n",
            yaxt = "n",
            col.axis = "white",
            border = "white")
    
    legend(legend = allLin_inLab,
           fill = inPalette()$allLinColors,
           cex = 0.85,
           x = "topleft",
           bty = "n",
           xpd = TRUE)
  })
  
  #Generating a pie chart for Lineages+ input table.
  output$specialLin_piechart <- renderPlot({
    #The final input table MUST contain at least 1 Lineage+.
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
    
    par(mar = c(0,0,0,0), fig = c(0,1,0.2,1))
    
    pie(specialLin_inPie, labels = NA, col = inPalette()$specialLinColors)
    title(main = "Lineages+", cex.main = 1.5, line = -1.5)
    
    #Generating the legend in a secondary
    #space of the plot area.
    par(mar = c(0,0,0,0), fig = c(0,1,0,0.2), new = T)
    
    barplot(0,
            xaxt = "n",
            yaxt = "n",
            col.axis = "white",
            border = "white")
    
    legend(legend = specialLin_inLab,
           fill = inPalette()$specialLinColors, 
           cex = 0.85,
           x = "topleft",
           bty = "n",
           xpd = TRUE)
  })
  
  #Generating a pie chart for Mutations input table.
  output$mut_piechart <- renderPlot({
    #The final input table MUST contain at least 1 Mutation.
    validate(need(sum(inTable()$mut)>0,
                  "0 Mutations of interest in the selected weeks range"))
    
    if (nrow(inTable()$mut)>=2) {
      mut_inPie <- rowSums(inTable()$mut)
      mut_inLab <- row.names(inTable()$mut)
    } else {
      mut_inPie <- sum(inTable()$mut)
      mut_inLab <- row.names(inTable()$mut)
    }
    
    par(mar = c(0,0,0,0), fig = c(0,1,0.2,1))
    
    pie(mut_inPie, labels = NA, col = inPalette()$mutColors)
    title(main = "Mutations", cex.main = 1.5, line = -1.5)
    
    #Generating the legend in a secondary
    #space of the plot area.
    par(mar = c(0,0,0,0), fig = c(0,1,0,0.2), new = T)
    
    barplot(0,
            xaxt = "n",
            yaxt = "n",
            col.axis = "white",
            border = "white")
    
    legend(legend = mut_inLab,
           fill = inPalette()$mutColors,
           cex = 0.85,
           x = "topleft",
           bty = "n",
           ncol = 2,
           xpd = TRUE)
  })
  
  #Generating the drop down menu that allows to select a Lineage.
  output$lineage <- renderUI({
    #The final input table MUST contain at least 1 Lineage.
    validate(need(nrow(inTable()$allLin)>0,
                  "Can not generate Lineage selection"))
    
    #The list of lineages in the menu depends on which Lineages are
    #present in the input table.
    myLineages <- row.names(inTable()$allLin)
    
    names(myLineages) <- row.names(inTable()$allLin)
    
    #Storing the Lineage selected by the user in a variable called lineageDefault that,
    #if the Lineage is still present in the input table, is used as a default selection
    #anytime the menu is generated.
    lineageDefault <- isolate(input$selLineage)
    
    freezeReactiveValue(input, "selLineage")
    
    selectInput("selLineage",
                "Lineage",
                choices = as.list(myLineages),
                selected = lineageDefault)
  })
  
  #Generating the drop down menu that allows to select a Mutation.
  output$mutation <- renderUI({
    #The final input table MUST contain at least 1 Mutation.
    validate(need(sum(inTable()$mut)>0,
                  "Can not generate Mutation selection"))
    
    #The list of Mutations in the menu depends on which Mutations are
    #present in the input table.
    myMutations <- row.names(inTable()$mut)
    
    names(myMutations) <- row.names(inTable()$mut)
    
    #Storing the Mutation selected by the user in a variable called mutationDefault that,
    #if the Mutation is still present in the input table, is used as a default selection
    #anytime the menu is generated.
    mutationDefault <- isolate(input$selMutation)
    
    freezeReactiveValue(input, "selMutation")
    
    selectInput("selMutation",
                "Mutation",
                choices = as.list(myMutations),
                selected = mutationDefault)
  })
  
  #Generating a scatter plot that allows to compare the Lineage of interest
  #with all other Lineages (if present) and the corresponding legend.
  output$allLin_scatterplot <- renderPlot({
    #The final input table MUST contain at least 1 Lineage.
    validate(need(nrow(inTable()$allLin)>0,
                  paste("0 Lineages with more than",
                        input$allLin_nGenomes,
                        "sequenced genomes in the selected weeks range")))
    
    #Values for the Lineage of interest (selected using the appropriate widget)
    #are stored in the mainLineage variable.
    mainLineage <- inTable()$allLin[input$selLineage,]
    
    #All other Lineages (if present) are collapsed together.
    if (nrow(inTable()$allLin)>2) {
      secLineage <- colSums(inTable()$allLin[row.names(inTable()$allLin)!=input$selLineage,])
    } else {
      secLineage <- inTable()$allLin[row.names(inTable()$allLin)!=input$selLineage,]
    }
    
    plot(colnames(inTable()$allLin),
         mainLineage,
         type = "b",
         col = ifelse(input$selLineage%in%row.names(colVoc), colVoc[input$selLineage,1], "darksalmon"),
         lwd = 3,
         pch = 15,
         lty = 13,
         ylim = c(0, max(colSums(inTable()$allLin))),
         main = input$selLineage,
         xlab = "Week",
         ylab = "#Genomes", cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
    
    
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
           col = c(ifelse(input$selLineage%in%row.names(colVoc), colVoc[input$selLineage,1], "darksalmon"), "darkgrey"),
           pch = 15,
           cex = 1.25,
           x = "topleft",
           inset = c(0.05,0),
           bty = "n",
           xpd = TRUE)
  })
  
  #Generating a scatter plot that allows to compare the Mutation of interest
  #with all other Mutations (if present) and the corresponding legend.
  output$mut_scatterplot <- renderPlot({
    #The final input table MUST contain at least 1 Mutation.
    validate(need(sum(inTable()$mut)>0,
                  "0 Mutations of interest in the selected weeks range"))
    
    #Values for the Mutation of interest (selected using the appropriate widget)
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
         col = ifelse(input$selMutation%in%row.names(colMoc), colMoc[input$selMutation,1], "darksalmon"),
         lwd = 3,
         pch = 15,
         lty = 13,
         ylim = c(0, max(colSums(inTable()$mut))),
         main = input$selMutation,
         xlab = "Week",
         ylab = "#Genomes", cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
    
    if (length(secMutation)!=0) {
      lines(colnames(inTable()$mut),
            secMutation,
            type = "b",
            col = "dimgray",
            lwd = 3,
            pch = 15,
            lty = 13)
    }
    
    legend(legend = c(input$selMutation, "Not selected mutations"),
           col = c(ifelse(input$selMutation%in%row.names(colMoc), colMoc[input$selMutation,1], "darksalmon"), "darkgrey"),
           pch = 15,
           cex = 1.25,
           x = "topleft",
           inset = c(0.05,0),
           bty = "n",
           xpd = TRUE)
  })
  
  #Subsetting the input table for Lineages+ respect to minimum frequency (%).
  pctGenomes_selector <- reactive({
    #The selected time lapse MUST be >= 1 week otherwise the app rises
    #a warning.
    validate(need((input$weeksRange[2]-input$weeksRange[1])>=1,
                  "Select a Weeks range"))
    
    #Data from the Lineage+ input table are processed in order to select the
    #minimum prevalence (%) required to a Lineage+ to be represented in the
    #Lineage+ VS Lineage barplot. The whole process is described below.
    #Calculating the total number of genomes sequenced in the time period of interest.
    #Both Lineages+ and Lineages sequences are considered.
    totGenomes <- sum(weeks_selector()$allLin)
    
    #The total number of genomes sequenced in the time period of interest MUST be >0
    #otherwise the app rises a warning.
    validate(need(totGenomes>0,
                  "0 sequenced genomes in the selected weeks range"))
    
    #Calculating the total number of sequences available for each Lineage+ in the
    #selected time period.
    totGenomes_specialLin <- rowSums(weeks_selector()$specialLin)
    
    #Calculating the prevalence (%) of each Lineage+. Prevalence (%) equals to the ratio
    #between the total number of sequences for each Lineage+ and the total number of genomes
    #(both Lineages+ and Lineages) sequenced in the time lapse of interest.
    pctGenomes_specialLin <- totGenomes_specialLin/totGenomes
    
    #Only Lineages+ with a prevalence (%) above a user selected value are retained.
    pctFiltered_specialLin <- pctGenomes_specialLin[pctGenomes_specialLin>=as.numeric(input$specialLin_pctGenomes)]
    
    #The names of Lineages+ passing the filter are stored in the mySpecialLin variable
    #and used to produce the drop down menu that allows the user to select the Lineage+
    #to represent in the Lineage+ VS Lineage barplot.
    mySpecialLin <- names(pctFiltered_specialLin)
    
    return(mySpecialLin)
  })
  
  #Generating the drop down menu that allows to select a Lineage+.
  output$specialLineage <- renderUI({
    #The final input table MUST contain at least 1 Lineage+.
    validate(need(length(pctGenomes_selector())>0,
                  "Can not generate Lineage+ selection"))
    
    #The list of Lineages+ in the menu depends on which Lineages+ are
    #present in the input vector.
    mySpecialLin <- pctGenomes_selector()
    
    names(mySpecialLin) <- pctGenomes_selector()
    
    #Storing the Lineage+ selected by the user in a variable called specialLinDefault that,
    #if the Lineage+ is still present in the input vector, is used as a default selection
    #anytime the menu is generated.
    specialLinDefault <- isolate(input$selSpecialLin)
    
    freezeReactiveValue(input, "selSpecialLin")
    
    selectInput("selSpecialLin",
                "Lineage+",
                choices = as.list(mySpecialLin),
                selected = specialLinDefault)
  })
  
  #Generating the input table for the Lineage+ VS Lineage barplot and converting
  #it to a matrix.
  pct_inTable <- reactive({
    #Data in the Lineage+ VS Lineage barplot need to be normalized to give a
    #significant information, that's why the frequency (%) is calculated for
    #both the Lineage+ of interest and its "parental" Lineage. The frequency
    #of all others Lineages+ and Lineages is also calculated for comparison and
    #collapsed under the Others variable.
    #Calculating the total weekly number of sequenced genomes. Both Lineages+
    #and Lineages are considered.
    perWeek_totGenomes <- colSums(weeks_selector()$allLin)
    
    #Calculating the weekly frequency (%) for each Lineage. The frequency (%)
    #equals to the ratio between the number of genomes sequenced for each
    #Lineage and the total number of genomes (both Lineage and Lineage+) sequenced
    #every week in the time lapse of interest.
    pct_allLin <- as.data.frame(t(apply(weeks_selector()$allLin, 1, "/", perWeek_totGenomes)))
    
    ##Calculating the weekly frequency (%) for each Lineage+. The frequency (%)
    #equals to the ratio between the number of genomes sequenced for each
    #Lineage+ and the total number of genomes (both Lineage and Lineage+) sequenced
    #every week in the time lapse of interest.
    pct_specialLin <- as.data.frame(t(apply(weeks_selector()$specialLin, 1, "/", perWeek_totGenomes)))
    
    #The final input table is generated by binding together:
    #1. The weekly frequency (%) of the Lineage+ of interest.
    #2. The weekly frequency (%) of its "parental" Lineage.
    #3. The weekly frequency (%) of Lineages and Lineages+ different from the Lineage+
    #of interest and its "parental" Lineage. These data are collapsed under Others.
    pct_dataProcessing <- rbind(Others = colSums(pct_allLin[row.names(pct_allLin)!=specialLin_to_Lineage[input$selSpecialLin,1]&row.names(pct_allLin)!=input$selSpecialLin,]),
                                pct_allLin[specialLin_to_Lineage[input$selSpecialLin,1],])
    
    pct_dataProcessing <- rbind(pct_dataProcessing,
                                pct_specialLin[input$selSpecialLin,])
    
    #The final input table is converted to a matrix.
    pct_matrix <- as.matrix(pct_dataProcessing)
    
    return(list(pct_inData = pct_matrix,
                totGenomes_inData = perWeek_totGenomes))
  })
  
  #Generating the barplot and the corresponding table for the Lineage+ VS Lineage
  #analysis.
  output$spLvsL_barplot <- renderPlot({
    #The final input table MUST contain at least 1 Lineage+.
    validate(need(length(pctGenomes_selector())>0,
                  paste("0 Lineages+ with a % of sequenced genomes higher than",
                        paste0(as.numeric(input$specialLin_pctGenomes)*100,"%"),
                        "in the selected weeks range")))
    
    #Creating a custom palette for the Lineage+ VS Lineage barplot.
    #Names of all the data in the input matrix are stored in the
    #pctLineages variable.
    pctLineages <- row.names(pct_inTable()$pct_inData)
    
    #A n of colors equal to the n of elements in the pctLineages variable
    #is extracted from the random palette (called randomColors) defined in the
    #config.R configuration file and stored in the pctPalette variable.
    pctPalette <- randomColors[1:length(pctLineages)]
    
    #Checking if any of the data from the input table is a Variant of Concern
    #(VOC) and storing the index of TRUE values in the pct_isVoc variable.
    pct_isVoc <- pctLineages%in%row.names(colVoc)
    
    #If at least a single VOC is present in the pctLineages variable, the random
    #color (in the pctPalette) corresponding to its index is replaced by the
    #specific color assigned to that VOC from the colVoc table. The latter is
    #defined in the config.R configuration file.
    if (length(pct_isVoc)>=1) {
      pct_myVoc <- pctLineages[pct_isVoc]
      pctPalette[pct_isVoc] <- colVoc[pct_myVoc,1]
    }
    
    #Generating the Lineage+ VS Lineage barplot.
    par(mar = c(4,5,2,0), fig = c(0,0.75,0,1))
    
    barplot(pct_inTable()$pct_inData,
            col = pctPalette,
            xlab = "Week",
            ylab = "%Genomes",
            main = paste(input$selSpecialLin, "VS", specialLin_to_Lineage[input$selSpecialLin,1]),
            cex.lab = 1.5,
            cex.axis = 1.5,
            cex.main = 1.5)
    
    #Generating the Lineage+ VS Lineage legend in a secondary
    #space of the plot area.
    par(mar = c(0,0,0,0), fig = c(0.75,1,0,1), new = T)
    
    barplot(0,
            xaxt = "n",
            yaxt = "n",
            col.axis = "white",
            border = "white")
    
    legend(legend = row.names(pct_inTable()$pct_inData),
           fill = pctPalette,
           cex = 0.90,
           x = "topleft",
           bty = "n",
           xpd = TRUE)
  })
  
  #Generating the barplot displaying the number of genome
  #sequences available for each week of the selected time
  #period
  output$perWeek_totGenomes_barplot <- renderPlot({
    #The selected time lapse MUST be >= 1 week otherwise the app rises
    #a warning.
    validate(need((input$weeksRange[2]-input$weeksRange[1])>=1,
                  "Select a Weeks range"))
    
    #The total number of genomes sequenced in the time period of interest MUST be >0
    #otherwise the app rises a warning.
    validate(need(sum(pct_inTable()$totGenomes_inData)>0,
                  "0 sequenced genomes in the selected weeks range"))
    
    #The final input table MUST contain at least 1 Lineage+.
    validate(need(length(pctGenomes_selector())>0,
                  paste("0 Lineages+ with a % of sequenced genomes higher than",
                        paste0(as.numeric(input$specialLin_pctGenomes)*100,"%"),
                        "in the selected weeks range")))
    
    par(mar = c(4,5,3,0), fig = c(0,0.75,0,1))
    
    myBarplot <- barplot(pct_inTable()$totGenomes_inData,
                                          col = "darkgrey",
                                          ylim = c(max(pct_inTable()$totGenomes_inData), 0),
                                          ylab = "#Genomes",
                                          xaxt = "n",
                                          cex.lab = 1.5,
                                          cex.axis = 1.5)
    
    axis(side = 3,
         at = myBarplot,
         labels = colnames(pct_inTable()$pct_inData),
         tick = 0)
    
    text(myBarplot,
         pct_inTable()$totGenomes_inData,
         labels = pct_inTable()$totGenomes_inData,
         srt = 90,
         cex = 0.80,
         adj = c(1,0.5),
         xpd = TRUE)
  })
}

#Launch App -----
shinyApp(ui = ui, server = server)
