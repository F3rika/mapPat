#########################################################
#      DEFINING ALL THE ELEMENTS OF THE APP SERVER      #
#########################################################

#Define server -----
server <- function(input, output){
  #######COMMON WIDGETS GENERATION AND INPUTS TABLES AVAILABILITY CHECK#######
  #####Generating widgets to control parameters that are common to multiple tabs.
  #####Checking if all the required inputs tables are available for the country of interest.
  #Generating the drop down menu for country selection.
  output$countryAll <- renderUI({
    #Defining inputs.
    pathogenSel <- input$pathogen
    countryDefault <- pathogenSelConf_Table[pathogenSelConf_Table$PathogenAbbr==pathogenSel,]$CountryDef
    
    #Opening the Countries List Tracker file. This file collects the ISO-3 codes
    #for all the countries with the minimal number of sequenced genomes of the
    #pathogen of interest equal or higher than 1000. By developer design countries
    #need at least 1000 sequenced genomes of the pathogen of interest to be considered for
    #analysis.
    #This information is essential to build the dedicated country selection drop down menu.
    countryPass_List <- scan(paste0(config_path, pathogenSel, "/","countriesListTracker.txt"),
                             what = "character",
                             sep = "\n",
                             comment.char = "",
                             quote = "",
                             fileEncoding = "UTF-8")
    
    #Defining the content of the country selection drop down menu. Only countries listed in
    #countryPass_List are considered.
    countryNames <- countryISOADMConvertion_Table[countryISOADMConvertion_Table$Country_ISO%in%countryPass_List,]$Country
    
    countryISO <- countryISOADMConvertion_Table[countryISOADMConvertion_Table$Country_ISO%in%countryPass_List,]$Country_ISO
    
    names(countryISO) <- countryNames
    
    countryList <- as.list(countryISO)
    
    #Generating the country selection drop down menu.
    selectInput("country",
                "Country",
                choices = countryList,
                selected = countryDefault)
    
  })
  
  #Generating the slider for week selection.
  output$timeRangeAll <- renderUI({
    #Defining inputs.
    pathogenSel <- input$pathogen
    countryDefault <- pathogenSelConf_Table[pathogenSelConf_Table$PathogenAbbr==pathogenSel,]$CountryDef
    timeUnit <- pathogenSelConf_Table[pathogenSelConf_Table$PathogenAbbr==pathogenSel,]$TimeUn
    
    #Defining the maximum limit of the week selection slider.
    maxTime <- ncol(read.table(paste0(allLin_path, pathogenSel, "/", "Epiweek.", countryDefault,".csv"),
                               sep = " ",
                               row.names = 1,
                               header = T,
                               check.names = F,
                               fileEncoding = "UTF-8"))
    
    #Generating the weeks selection slider.
    sliderInput("timeRange",
                "Time range",
                min = 1,
                max = maxTime,
                step = 5,
                value = c(maxTime-20, maxTime))
    
  })
  
  #Checking input tables availability.
  checkInTabs <- reactive({
    #Defining inputs.
    pathogenSel <- input$pathogen
    countrySel <- input$country
    
    #Opening the Input Tables Availability Check file. This file associates countries
    #that can be analyzed (listed by their ISO-3 codes) with the complete list of input
    #tables available for each. This information allows to evaluate which tabs are populated
    #and which graphical representations can be produced. If an input table is unavailable
    #the tool will return a warning message.
    checkInTabs_Table <- read.table(paste0(config_path, pathogenSel, "/", "inTab_avCheck.txt"),
                                sep = "\t",
                                header = T,
                                check.names = F,
                                comment.char = "",
                                quote = "",
                                fileEncoding = "UTF-8")
    
    #Selecting the row from the Input Tables Availability Check file regarding the country of
    #interest.
    checkInTabs_countrySel <- checkInTabs_Table[checkInTabs_Table$Country==countrySel,]
    
    #Collecting the information about single input tables availability in dedicated variables.
    #This information will be used in availability controls further down in the code. If an input
    #table is missing the tool will return a warning.
    check_var <- checkInTabs_countrySel$Var
    check_allLin <- checkInTabs_countrySel$Lin
    check_mut <- checkInTabs_countrySel$Mut
    check_heatChoroM <- checkInTabs_countrySel$HeatChoroMap
    check_totReg <- checkInTabs_countrySel$TotReg
    
    return(list(var = check_var,
                allLin = check_allLin,
                mut = check_mut,
                heatChoroM = check_heatChoroM,
                totReg = check_totReg))
  })
  
  #######COMMON DATA UPLOAD AND SUBSETTING#######
  #####Reading input tables and subsetting parameters that are common to multiple Tabs.
  #Reading the input tables for the selected country.
  countrySelector <- reactive({
    #Defining inputs.
    pathogenSel <- input$pathogen
    
    check_var <- checkInTabs()$var
    check_allLin <- checkInTabs()$allLin
    check_mut <- checkInTabs()$mut
    check_heatChoroM <- checkInTabs()$heatChoroM
    check_totReg <- checkInTabs()$totReg
    
    separ <- " "
    encod <- "UTF-8"
    
    #Reading the input files.
    if (!is.na(check_var)) {
      
      var_countrySel <- read.table(paste0(var_path, pathogenSel, "/", check_var),
                                   sep = separ,
                                   row.names = 1,
                                   header = T,
                                   check.names = F,
                                   comment.char = "",
                                   fileEncoding = encod)
      
    } else {
      
      var_countrySel <- NA
      
    }
    
    if (!is.na(check_allLin)) {
      
      allLin_countrySel <- read.table(paste0(allLin_path, pathogenSel, "/", check_allLin),
                                      sep = separ,
                                      row.names = 1,
                                      header = T,
                                      check.names = F,
                                      comment.char = "",
                                      fileEncoding = encod)
      
    } else {
      
      allLin_countrySel <- check_allLin
      
    }
    
    if (!is.na(check_mut)) {
      
      mut_countrySel <- read.table(paste0(mut_path, pathogenSel, "/", check_mut),
                                   sep = separ,
                                   header = T,
                                   check.names = F,
                                   comment.char = "",
                                   fileEncoding = encod)
      
    } else {
      
      mut_countrySel <- NA
      
    }
    
    if (!is.na(check_heatChoroM)) {
      
      heatChoromap_countrySel <- read.table(paste0(heatChoromap_path, pathogenSel, "/", check_heatChoroM),
                                            sep = separ,
                                            header = T,
                                            check.names = F,
                                            comment.char = "",
                                            fileEncoding = encod)
      
    } else {
      
      heatChoromap_countrySel <- NA
      
    }
    
    if (!is.na(check_totReg)) {
      
      totReg_countrySel <- read.table(paste0(totReg_path, pathogenSel, "/", check_totReg),
                                      sep = separ,
                                      row.names = 1,
                                      header = T,
                                      check.names = F,
                                      comment.char = "",
                                      fileEncoding = encod)
      
    } else {
      
      totReg_countrySel <- NA
      
    }
    
    return(list(var = var_countrySel,
                allLin = allLin_countrySel,
                mut = mut_countrySel,
                heatChoroM = heatChoromap_countrySel,
                totReg = totReg_countrySel))
  })
  
  #Subsetting the input tables respect to the selected time lapse.
  timeSelector <- reactive({
    #Defining inputs.
    check_var <- checkInTabs()$var
    check_allLin <- checkInTabs()$allLin
    check_mut <- checkInTabs()$mut
    check_heatChoroM <- checkInTabs()$heatChoroM
    check_totReg <- checkInTabs()$totReg
    
    startTime <- input$timeRange[1]
    endTime <- input$timeRange[2]
    
    var_inTable <- countrySelector()$var
    allLin_inTable <- countrySelector()$allLin
    mut_inTable <- countrySelector()$mut
    heatChoromap_inTable <- countrySelector()$heatChoroM
    totReg_inTable <- countrySelector()$totReg
    
    #The selected time lapse MUST be >= 1 week otherwise the app rises
    #a warning.
    validate(need((endTime-startTime)>=1,
                  "Select a time range"))
    
    #Subsetting the initial input tables according to the user-selected
    #time period.
    if (!is.na(check_var)) {
      
      var_timeSel <- var_inTable[,startTime:endTime]
      
    } else {
      
      var_timeSel <- NA
      
    }
    
    if (!is.na(check_allLin)) {
      
      allLin_timeSel <- allLin_inTable[,startTime:endTime]
      
    } else {
      
      allLin_timeSel <- NA
      
    }
    
    if (!is.na(check_mut)) {
      
      mut_timeSel <- mut_inTable[,1:3]
      mut_timeSel <- cbind(mut_timeSel, mut_inTable[,startTime:endTime+3])
      
    } else {
      
      mut_timeSel <- NA
      
    }
    
    if (!is.na(check_heatChoroM)) {
      
      heatChoromap_timeSel <- heatChoromap_inTable[,1:2]
      heatChoromap_timeSel <- cbind(heatChoromap_timeSel, heatChoromap_inTable[,startTime:endTime+2])
      
    } else {
      
      heatChoromap_timeSel <- NA
      
    }
    
    if (!is.na(check_totReg)) {
      
      totReg_timeSel <- totReg_inTable[,startTime:endTime]
      
    } else {
      
      totReg_timeSel <- NA
      
    }
    
    
    return(list(var = var_timeSel,
                allLin = allLin_timeSel,
                mut = mut_timeSel,
                heatChoroM = heatChoromap_timeSel,
                totReg = totReg_timeSel))
  })
  
  #Calculating the total number of:
  #1. Sequences in the user-selected time lapse.
  #2. Per week sequences in the user-selected time lapse.
  #Starting from national counts.
  #These values will be used to subset data by frequency (%) and
  #to normalize data.
  seqSum <- reactive({
    #Defining inputs.
    check_allLin <- checkInTabs()$allLin
    
    inTable <- timeSelector()$allLin
    
    #Calculating the total number of sequences in the user-selected time
    #lapse of interest.
    totSeq <- sum(inTable)
    
    #Calculating the total number of sequences for each week in the
    #user-selected time lapse of interest
    if (!is.na(check_allLin)) {
      
      totSeq_perWeek <- colSums(inTable)
      
    } else {
      
      totSeq_perWeek <- NA
      
    }
    
    
    #The total number of genomes sequenced in the time period of interest MUST be >0
    #otherwise the app rises a warning.
    if (!is.na(check_allLin)) {
      
      validate(need(totSeq>0,
                    "0 sequenced genomes in the selected weeks range"))
      
    }
    
    
    return(list(totSeq = totSeq,
                totSeq_perWeek = totSeq_perWeek))
  })
  
  #Calculating the total number of:
  #1. Sequences in the user-selected time lapse.
  #2. Per Region sequences in the user selected time lapse.
  #Starting from regional counts.
  #These values will be used to normalize data for the HeatMaps (HM)
  #and Choropleth Maps (CM).
  seqSumReg <- reactive({
    #Defining inputs.
    check_totReg <- checkInTabs()$totReg
    
    inTable <- timeSelector()$totReg
    
    #Calculating the total number of sequences in the user-selected time
    #lapse of interest.
    totReg <- sum(inTable)
    
    #Calculating the total number of sequences for each Region in the
    #user-selected time lapse of interest
    if (!is.na(check_totReg)) {
      
      totSeq_perReg <- rowSums(inTable)
      
    } else {
      
      totSeq_perReg <- NA
      
    }
    
    
    #The total number of genomes sequenced in the time period of interest MUST be >0
    #otherwise the app rises a warning.
    if (!is.na(check_totReg)) {
      
      validate(need(totReg>0,
                    "0 sequenced genomes in the selected weeks range"))
      
    }
    
    
    return(list(totReg = totReg,
                totSeq_perReg = totSeq_perReg))
  })
  
  #Reading the input file containing the geometric data that allows
  #to draw the map of the user-selected country.
  geomDataSelector <- reactive({
    #Defining inputs.
    check_totReg <- checkInTabs()$totReg
    
    geomDataSelector_CountryISO <- input$country
    geomDataSelector_CountryADM <- countryISOADMConvertion_Table[countryISOADMConvertion_Table$Country_ISO==geomDataSelector_CountryISO,]$Country_ADM
    geomDataSelector_CountryRegList <- rownames(countrySelector()$totReg)
    
    #Opening the geometric data file that allows to draw the map of the
    #user-selected country. Each country requires a map at a specific ADM
    #level (either ADM1 or ADM2) depending on data availability. The association
    #between each country and the corresponding ADM is stored in the
    #CountryISOADM_AssocTab.txt configuration file.
    if (geomDataSelector_CountryADM=="ADM1" & !is.na(check_totReg)) {
      
      geomData <- gb_adm1(geomDataSelector_CountryISO)
      geomData <- geomData[geomData$shapeName %in% geomDataSelector_CountryRegList,]
      
    } else if (geomDataSelector_CountryADM=="ADM2" & !is.na(check_totReg)) {
      
      geomData <- gb_adm2(geomDataSelector_CountryISO)
      geomData <- geomData[geomData$shapeName %in% geomDataSelector_CountryRegList,]
      
    } else if (is.na(check_totReg)) {
      
      geomData <- NA
      
    }
    
    return(list(geomData = geomData))
  })
  
  #######VARIANTS TAB#######
  #####Analyzing Variants data.
  #Variants Tab is shown/hidden depending on the user-selected pathogen of interest.
  observeEvent(input$pathogen, {
    
    if (input$pathogen=="SARS-CoV-2") {
      
      showTab(inputId = "allTabs", target = "Variants")
      
    } else {
      
      hideTab(inputId = "allTabs", target = "Variants")
      
    }
    
  })
  
  ###INPUT TABLES AND PERSONALISATION WIDGETS###
  ###Checking for the availability of input tables required by the Variants Tab.
  ###Producing the widgets allowing to personalize data and plots from the Variants Tab.
  #Checking on input tables availability.
  varTab_checkInTabs <- reactive  ({
    #Defining inputs.
    check_var <- checkInTabs()$var
    check_allLin <- checkInTabs()$allLin
    check_heatChoroM <- checkInTabs()$heatChoroM
    check_totReg <- checkInTabs()$totReg
    
    check_InputTabs <- c(check_var, check_allLin, check_heatChoroM, check_totReg)
    names(check_InputTabs) <- c("Var", "allLin", "HeatChoromap", "totReg")
    
    missing_InputTabs <- is.na(check_InputTabs)
    missing_InputTabs_names <- names(check_InputTabs[missing_InputTabs])
    
    validate(need(!any(missing_InputTabs),
                  paste0("Function not available. Missing ", toString(missing_InputTabs_names), " input table(s).")))
    
  })
  
  #Generating the drop down menu for variant category selection.
  output$varCategory <- renderUI({
    #Checking input tables availability.
    varTab_checkInTabs()
    
    #Generating the variant category selection drop down menu.
    selectInput("variantCategory",
                "Category",
                choices = statusList,
                selected = "All")
    
  })
  
  #Generating the radio buttons for minimum frequency (%) selection.
  output$varFrequency <- renderUI({
    #Checking input tables availability.
    varTab_checkInTabs()
    
    #Generating the minimum frequency (%) selection radio buttons.
    radioButtons("variantsFreq",
                 "Min % of genomes",
                 choices = list("1%"=0.01,
                                "2.5%"=0.025,
                                "5%"=0.05,
                                "7.5%"=0.075,
                                "10%"=0.10),
                 selected = 0.01)
    
  })
  
  ###STACKED AREA CHART AND BARPLOT###
  ###Producing the inputs for and plotting the Stacked Area Chart (SAC) and BarPlot (BP).
  #Collecting data about Variants belonging to the user-selected
  #category of interest.
  varSAC_dataSelector <- reactive({
    #Checking input tables availability.
    varTab_checkInTabs()

    #Defining inputs.
    varSAC_dataSelector_inTable <- timeSelector()$var
    varSAC_dataSelector_refTable <- variantsConvertion_Table
    varSAC_dataSelector_catSel <- input$variantCategory
    
    #Selecting data.
    varSAC_dataSelector_varTable <- varCat_dataSelector(varSAC_dataSelector_inTable,
                                                        varSAC_dataSelector_refTable,
                                                        varSAC_dataSelector_catSel)
    
    return(list(varSAC = varSAC_dataSelector_varTable))
  })
  
  #Subsetting the Variants input table respect to the minimum number
  #of occurrences required to each element.
  varSAC_Subsetting <- reactive({
    #Defining inputs.
    varSAC_Subsetting_inTable <- varSAC_dataSelector()$varSAC
    varSAC_Subsetting_minOccur <- 1
    
    #Subsetting data.
    varSAC_Subsetted <- dataOccur_Subsetter(varSAC_Subsetting_inTable,
                                            varSAC_Subsetting_minOccur)
    
    #The final input table MUST contain at least 1 Variant.
    validate(need(nrow(varSAC_Subsetted)>0,
                  "0 Variants with a n. of sequenced genomes higher than 1 in the selected weeks range"))
    
    return(list(varSAC = varSAC_Subsetted))
  })
  
  #Normalizing the Variants input table.
  varSAC_Normalization <- reactive({
    #Defining inputs.
    varSAC_Normalization_inTable <- varSAC_Subsetting()$varSAC
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    
    #Normalizing data.
    varSAC_Normalized <- dataNormalizer(varSAC_Normalization_inTable,
                                        totSeq_perWeek)
    varSAC_Normalized <- varSAC_Normalized*100
    
    return(list(varSAC = varSAC_Normalized))
  })
  
  #Producing the color palette for the Stacked Area Chart (SAC).
  varSAC_Palette <- reactive({
    #Defining inputs.
    varSAC_tabPal <- varTheme
    varSAC_Palette_inTable <- varSAC_Subsetting()$varSAC
    
    #Producing the color palette.
    varSAC_Pal <- plotPalette(varSAC_tabPal,
                              dataTable = varSAC_Palette_inTable)
    
    return(list(varSAC = varSAC_Pal))
  })
  
  #Generating a Stacked Area Chart (SAC) and the corresponding legend
  #for the Variants input table.
  output$variantsSAC <- renderPlot({
    #Defining inputs.
    pathogenSel <- input$pathogen
    
    varSAC_inTable <- varSAC_Normalization()$varSAC
    varSAC_Palette <- varSAC_Palette()$varSAC
    varSAC_plotMain <- "Variants"
    varSAC_timeUnit <- pathogenSelConf_Table[pathogenSelConf_Table$PathogenAbbr==pathogenSel,]$TimeUn
    
    #Plotting
    dataPlotter_SAC(varSAC_inTable,
                    varSAC_Palette,
                    varSAC_plotMain,
                    varSAC_timeUnit)
  })
  
  #Generating the BarPlot (BP) displaying the number of genome sequences
  #available for each week of the selected time period.
  output$varTab_perWeekSeq <- renderPlot({
    #Checking input tables availability.
    varTab_checkInTabs()
    
    #Defining inputs.
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    
    #Plotting.
    perWeekTotSeqPlotter_BP(totSeq_perWeek)
  })
  
  ###BARPLOT###
  ###Producing the inputs for and plotting the BarPlot (BP).
  #Generating the drop down menu that allows to select a Variant for
  #which produce a BarPlot (BP) displaying its composition (proportion
  #of Lineages).
  output$variant <- renderUI({
    #Checking input tables availability.
    varTab_checkInTabs()
    
    #Defining inputs.
    varSel_varBP_inTable <- varSAC_Subsetting()$varSAC
    varSel_varBP_widgetName <- "variantSel_BP"
    linSel_allLinCM1_widgetMain <- "Variant (barplot)"
    posDefault <- 1
    
    #Producing the drop down menu.
    dropdownM_widgetGenerator(varSel_varBP_inTable,
                              varSel_varBP_widgetName,
                              linSel_allLinCM1_widgetMain,
                              posDefault)
  })
  
  #Calculating the total number of:
  #1. Sequences in the user-selected time lapse.
  #2. Per week sequences in the user-selected time lapse.
  #These values will be used to subset data by frequency (%) and
  #to normalize data. In the case of the Variants Tab BarPlot (BP) these
  #values are calculated starting from the Variants input table and for
  #the user-selected Variant of interest.
  varBP_seqSum <- reactive({
    #Checking input tables availability.
    varTab_checkInTabs()
    
    #Defining inputs.
    varBP_seqSum_inTable <- timeSelector()$var
    varBP_seqSum_varSel <- input$variantSel_BP
    
    #Calculating the total number of sequences in the user-selected time
    #lapse of interest.
    varBP_totSeq <- sum(varBP_seqSum_inTable[varBP_seqSum_varSel,])
    
    #Calculating the total number of sequences for each week in the
    #user-selected time lapse of interest
    varBP_totSeq_perWeek <- varBP_seqSum_inTable[varBP_seqSum_varSel,]
    
    #The total number of genomes sequenced in the time period of interest MUST be >0
    #otherwise the app rises a warning.
    validate(need(varBP_totSeq>0,
                  "0 sequenced genomes for the Variant of iterest in the selected weeks range"))
    
    return(list(varBP_totSeq = varBP_totSeq,
                varBP_totSeq_perWeek = varBP_totSeq_perWeek))
  })
  
  #Collecting data about Lineages belonging to the user-selected Variant
  #from the allLin input table.
  varBP_dataSelector <- reactive({
    #Checking input tables availability.
    varTab_checkInTabs()
    
    #Defining inputs.
    varBP_dataSelector_inTable <- timeSelector()$allLin
    varBP_dataSelector_refTable <- variantsConvertion_Table
    varBP_dataSelector_varSel <- input$variantSel_BP
    
    #Collecting the data.
    if (varBP_dataSelector_varSel=="VBM") {
      
      varBP_dataSelector_linSel <- varBP_dataSelector_refTable[varBP_dataSelector_refTable$isVBM,]$Lin
      varBP_dataSelector_linTable <- varBP_dataSelector_inTable[row.names(varBP_dataSelector_inTable)%in%varBP_dataSelector_linSel,]
      
    } else {
      
      varBP_dataSelector_linSel <- varBP_dataSelector_refTable[varBP_dataSelector_refTable$Variant==varBP_dataSelector_varSel,]$Lin
      varBP_dataSelector_linTable <- varBP_dataSelector_inTable[row.names(varBP_dataSelector_inTable)%in%varBP_dataSelector_linSel,]
      
    }
    
    return(list(varBP = varBP_dataSelector_linTable))
  })
  
  #Subsetting the data table of Lineages belonging to the user-selected
  #Variant respect to minimum frequency (%).
  varBP_Subsetting <- reactive({
    #Defining inputs.
    varBP_Subsetting_inTable <- varBP_dataSelector()$varBP
    varBP_totSeq <- varBP_seqSum()$varBP_totSeq
    varBP_Subsetting_minFreq <- input$variantsFreq
    varBP_Subsetting_maxViz <- 5
    
    #Subsetting data.
    varBP_Subsetted <- dataFreqViz_noOthers_Subsetter(varBP_Subsetting_inTable,
                                                      varBP_totSeq,
                                                      varBP_Subsetting_minFreq,
                                                      varBP_Subsetting_maxViz)
    
    #The final input table MUST contain at least 1 Lineage.
    validate(need(nrow(varBP_Subsetted)>0,
                  paste("0 Lineages with a % of sequenced genomes higher than",
                        paste0(as.numeric(varBP_Subsetting_minFreq)*100,"%"),
                        "for the Variant of iterest in the selected weeks range")))
    
    return(list(varBP = varBP_Subsetted))
  })
  
  #Normalizing the data of Lineages belonging to the user-selected Variant
  #of interest.
  varBP_Normalization <- reactive({
    #Defining inputs.
    varBP_Normalization_inTable <- varBP_Subsetting()$varBP
    varBP_totSeq_perWeek <- varBP_seqSum()$varBP_totSeq_perWeek
    
    #Normalizing data.
    varBP_Normalized <- dataNormalizer(varBP_Normalization_inTable,
                                       varBP_totSeq_perWeek)
    varBP_Normalized <- varBP_Normalized*100
    
    return(list(varBP = varBP_Normalized))
  })
  
  #Producing the color palette for the BarPlot (BP).
  varBP_Palette <- reactive({
    #Defining inputs.
    varBP_tabPal <- varTheme
    varBP_Palette_inTable <- varBP_Subsetting()$varBP
    
    #Producing the color palette.
    varBP_Pal <- plotPalette(varBP_tabPal,
                             dataTable = varBP_Palette_inTable)
    
    return(list(varBP = varBP_Pal))
  })
  
  #Generating the BarPlot (BP) displaying the proportion of Lineages composing
  #the user-selected Variant of interest and the corresponding legend.
  output$variantsBP_LinVar <- renderPlot({
    #Defining inputs.
    pathogenSel <- input$pathogen
    
    varBP_inTable <- varBP_Normalization()$varBP
    varBP_Palette <- varBP_Palette()$varBP
    varBP_varSel <- input$variantSel_BP
    varBP_plotMain <- paste0("Lineages of ", varBP_varSel)
    varBP_timeUnit <- pathogenSelConf_Table[pathogenSelConf_Table$PathogenAbbr==pathogenSel,]$TimeUn
    
    #Plotting.
    dataPlotter_BP(varBP_inTable,
                   varBP_Palette,
                   varBP_plotMain,
                   varBP_timeUnit)
  })
  
  ###HEATMAP & MAPS###
  ###Producing the common tables for the HeatMap (HM) and the Choropleth Maps (CM).
  #Calculating the total number of sequenced genomes for each Lineage at
  #regional level in the user-selected time lapse.
  varHMxCM_totSeq <- reactive({
    #Checking input tables availability.
    varTab_checkInTabs()
    
    #Defining inputs.
    varHMxCM_totSeq_inTable <- timeSelector()$heatChoroM
    
    #Calculating the total number of sequenced genomes.
    varHMxCM_totSeq_Table <- varHMxCM_totSeq_inTable[,c(1,2)]
    varHMxCM_totSeq_Table <- cbind(varHMxCM_totSeq_Table,
                                   totSeq = rowSums(varHMxCM_totSeq_inTable[,3:length(varHMxCM_totSeq_inTable)]))
    
    return(list(varHMxCM = varHMxCM_totSeq_Table))
  })
  
  #Producing the table containing the regional counts for each Variant.
  varHMxCM_tableProducer <- reactive({
    #Defining inputs.
    varHMxCM_tableProducer_inTable <- varHMxCM_totSeq()$varHMxCM
    varHMxCM_tableProducer_refTable <- variantsConvertion_Table
    
    varHMxCM_tableProducer_Var <- unique(varHMxCM_tableProducer_refTable$Variant)
    varHMxCM_tableProducer_dataElement <- c(varHMxCM_tableProducer_Var, "VBM")
    
    #Producing the table.
    varHMxCM_tableProducer_dataTable <- lapply(varHMxCM_tableProducer_dataElement,
                                               varReg_tableProducer,
                                               varHMxCM_tableProducer_inTable,
                                               varHMxCM_tableProducer_refTable)
    
    varHMxCM_tableProducer_dataTable <- do.call(rbind,
                                                varHMxCM_tableProducer_dataTable)
    
    return(list(varHMxCM = varHMxCM_tableProducer_dataTable))
  })
  
  ###HEATMAP###
  ###Producing the input tables for and plotting the HeatMap (HM).
  #Generating the input table for the Heatmap (HM).
  varHM_dataSelector <- reactive({
    #Defining inputs.
    varHM_dataSelector_inTable <- varHMxCM_tableProducer()$varHMxCM
    varHM_dataSelector_refTable <- variantsConvertion_Table
    varHM_dataSelector_regNames <- unique(varHM_dataSelector_inTable$reg)
    varHM_dataSelector_varNames <- unique(varHM_dataSelector_inTable$var)
    varHM_dataSelector_catSel <- input$variantCategory
    
    #Producing the Regions x Variants table.
    varHM_RegVar_dataTable <- sapply(varHM_dataSelector_varNames,
                                     varReg_dataSelector,
                                     varHM_dataSelector_inTable)
    
    row.names(varHM_RegVar_dataTable) <- varHM_dataSelector_regNames
    
    #Producing the input table of data from the user-selected Variants category.
    varHM_RegVarCat_dataTable <- regVarCat_dataSelector(varHM_RegVar_dataTable,
                                                        varHM_dataSelector_refTable,
                                                        varHM_dataSelector_catSel)
    
    #Subsetting the table in order to retain only Variants with at least 1 sequenced genome.
    varHM_RegVarCat_dataTableSubset <- varHM_RegVarCat_dataTable[,colSums(varHM_RegVarCat_dataTable)>0]
    
    #If only a single Variant survives the previous filter the final table has to be built
    #accordingly.
    if (ncol(as.data.frame(varHM_RegVarCat_dataTableSubset))==1) {
      
      regions <- row.names(varHM_RegVarCat_dataTable)
      variants <- names(which(colSums(varHM_RegVarCat_dataTable)>0))
      
      varHM_RegVarCat_dataTableSubset <- matrix(varHM_RegVarCat_dataTableSubset)
      row.names(varHM_RegVarCat_dataTableSubset) <- regions
      colnames(varHM_RegVarCat_dataTableSubset) <- variants
      
    }
    
    #The final input table MUST contain at least 1 Variant.
    validate(need(ncol(varHM_RegVarCat_dataTableSubset)>0,
                  "0 Variants with at least 1 sequenced genome in the selected weeks range"))
    
    return(list(varHM = varHM_RegVarCat_dataTableSubset))
  })
  
  #Normalizing the HeatMap (HM) input table.
  varHM_Normalization <- reactive({
    #Defining inputs.
    varHM_Normalization_inTable <- varHM_dataSelector()$varHM
    totSeq_perReg <- seqSumReg()$totSeq_perReg
    
    #Normalizing data.
    varHM_Normalized <- varHM_Normalization_inTable/totSeq_perReg
    varHM_Normalized <- varHM_Normalized*100
    varHM_Normalized[is.nan(varHM_Normalized)] <- 0
    
    return(list(varHM = varHM_Normalized))
  })
  
  #Producing the color palette for the HeatMap (HM).
  varHM_Palette <- reactive({
    #Defining inputs.
    varHM_tabPal <- varTheme
    varHM_Palette_inTable <- varHM_Normalization()$varHM
    varHM_Palette_colorsNum <- ceiling(max(varHM_Palette_inTable)/10)
    
    #Producing the color palette.
    varHM_Pal <- plotPalette(varHM_tabPal,
                             colorsNum = varHM_Palette_colorsNum)
    
    return(list(varHM = varHM_Pal))
  })
  
  #Generating the plot area for the HeatMap (HM).
  output$varHM_plotUI <- renderUI({
    #Defining inputs.
    varHM_plotUI_inTable <- varHM_Normalization()$varHM
    varHM_plotUI_nReg <- nrow(varHM_plotUI_inTable)
    varHM_plotUI_plotHeight <- 15*varHM_plotUI_nReg+100
    
    #Generating the plot area.
    if (varHM_plotUI_plotHeight>500) {
      
      plotOutput("variantsHM",
                 height = paste0(varHM_plotUI_plotHeight, "px"))
      
    } else {
      
      plotOutput("variantsHM",
                 height = "500px")
      
    }
    
  })
  
  #Generating the HeatMap (HM) and the corresponding legend.
  output$variantsHM <- renderPlot({
    #Defining inputs.
    varHM_inTable <- varHM_Normalization()$varHM
    varHM_dataNum <- ncol(varHM_inTable)
    varHM_Palette <- varHM_Palette()$varHM
    varHM_Main <- "Variants Regional Frequency (%)"
    varHM_tagColor <- "gray49"

    #Plotting.
    if (ncol(varHM_inTable)>1) {
      
      dataPlotter_HM(varHM_inTable,
                     varHM_dataNum,
                     varHM_Palette,
                     varHM_Main,
                     varHM_tagColor)
      
    } else if (ncol(varHM_inTable)==1) {
      
      varHM_inTable <- t(varHM_inTable)
      varHM_Palette <- "#FD8D3C"
      varHM_Main <- paste(row.names(varHM_inTable), "Regional Frequency (%)")
      
      dataPlotter_regBP(varHM_inTable,
                        varHM_Palette,
                        varHM_Main)
      
    }
    
  })
  
  ###MAPS###
  ###Producing the input tables for and plotting the Choropleth Maps (CM).
  #Generating the drop down menu that allows to select a Variant to represent
  #in the first Choropleth Map (CM1).
  output$variantCM1 <- renderUI({
    #Checking input tables availability.
    varTab_checkInTabs()
    
    #Defining inputs.
    linSel_varCM1_inTable <- varSAC_Subsetting()$varSAC
    linSel_varCM1_widgetName <- "variantSel_CM1"
    linSel_varCM1_widgetMain <- "Variant (Map 1)"
    posDefault <- 1
    
    #Producing the drop down menu.
    dropdownM_widgetGenerator(linSel_varCM1_inTable,
                              linSel_varCM1_widgetName,
                              linSel_varCM1_widgetMain,
                              posDefault)
  })
  
  #Generating the drop down menu that allows to select a Variant to represent
  #in the second Choropleth Map (CM2).
  output$variantCM2 <- renderUI({
    #Checking input tables availability.
    varTab_checkInTabs()
    
    #Defining inputs.
    linSel_varCM2_inTable <- varSAC_Subsetting()$varSAC
    linSel_varCM2_widgetName <- "variantSel_CM2"
    linSel_varCM2_widgetMain <- "Variant (Map 2)"
    posDefault <- 2
    
    #The final input table MUST contain at least 2 Variants to produce CM2 as well.
    validate(need(nrow(linSel_varCM2_inTable)>1,
                  "Only 1 Variant with a n. of sequenced genomes higher than 1 in the selected weeks range"))
    
    #Producing the drop down menu.
    dropdownM_widgetGenerator(linSel_varCM2_inTable,
                              linSel_varCM2_widgetName,
                              linSel_varCM2_widgetMain,
                              posDefault)
  })
  
  #Collecting data for the selected Variants and producing the corresponding
  #input tables for both Choropleth Maps (CM1 and CM2).
  varCM1_dataSelector <- reactive({
    #Defining inputs.
    varCM1_dataSelector_inTable <- varHMxCM_tableProducer()$varHMxCM
    varCM1_dataSelector_regNames <- unique(varCM1_dataSelector_inTable$reg)
    
    varCM1_dataSelector_LineageSel <- input$variantSel_CM1

    #Reading the input data table.
    varCM1_dataSel <- varCM1_dataSelector_inTable[varCM1_dataSelector_inTable$var%in%varCM1_dataSelector_LineageSel,]$totSeq
    varCM1_dataTable <- data.frame(totSeq = varCM1_dataSel,
                                   row.names = varCM1_dataSelector_regNames)
    varCM1_dataTable <- as.matrix(varCM1_dataTable)
    
    return(list(varCM1 = varCM1_dataTable))
  })
  
  varCM2_dataSelector <- reactive({
    #The final input table MUST contain at least 2 Variants to produce CM2 as well.
    validate(need(nrow(varSAC_Subsetting()$varSAC)>1,
                  "Only 1 Variant with a n. of sequenced genomes higher than 1 in the selected weeks range"))
    
    #Defining inputs.
    varCM2_dataSelector_inTable <- varHMxCM_tableProducer()$varHMxCM
    varCM2_dataSelector_regNames <- unique(varCM2_dataSelector_inTable$reg)
    
    varCM2_dataSelector_LineageSel <- input$variantSel_CM2
    
    #Reading the input data table.
    varCM2_dataSel <- varCM2_dataSelector_inTable[varCM2_dataSelector_inTable$var%in%varCM2_dataSelector_LineageSel,]$totSeq
    varCM2_dataTable <- data.frame(totSeq = varCM2_dataSel,
                                   row.names = varCM2_dataSelector_regNames)
    varCM2_dataTable <- as.matrix(varCM2_dataTable)
    
    return(list(varCM2 = varCM2_dataTable))
  })
  
  #Normalizing the input tables for the selected Variants at a regional
  #level in the country of interest for both Choropleth Maps (CM1 and CM2).
  varCM1_Normalization <- reactive({
    #Defining inputs.
    varCM1_Normalization_inTable <- varCM1_dataSelector()$varCM1
    totReg <- seqSumReg()$totSeq_perReg
    
    #Normalizing data.
    varCM1_Normalized <- varCM1_Normalization_inTable/totReg
    varCM1_Normalized <- varCM1_Normalized*100
    varCM1_Normalized[is.nan(varCM1_Normalized)] <- 0
    varCM1_Normalized <- signif(varCM1_Normalized, digits = 3)
    
    return(list(varCM1 = varCM1_Normalized))
  })
  
  varCM2_Normalization <- reactive({
    #Defining inputs.
    varCM2_Normalization_inTable <- varCM2_dataSelector()$varCM2
    totReg <- seqSumReg()$totSeq_perReg
    
    #Normalizing data.
    varCM2_Normalized <- varCM2_Normalization_inTable/totReg
    varCM2_Normalized <- varCM2_Normalized*100
    varCM2_Normalized[is.nan(varCM2_Normalized)] <- 0
    varCM2_Normalized <- signif(varCM2_Normalized, digits = 3)
    
    return(list(varCM2 = varCM2_Normalized))
  })
  
  #Defining the general color palette for the maps in the Variants Tab.
  #The palette consist of ten colors each one defining an interval of
  #frequency (%). Colors in the palette are used to fill each region
  #of the country of interest according to the frequency (%) of the Variant
  #of interest.
  varCM_generalPalette <- reactive ({
    #Defining inputs.
    varCM_generalPalette_tabPal <- varTheme
    varCM_generalPalette_colorsNum <- 10
    
    #Producing the color palette.
    varCM_Pal <- plotPalette(varCM_generalPalette_tabPal,
                             colorsNum = varCM_generalPalette_colorsNum)
    
    return(list(varCM = varCM_Pal))
  })
  
  #Defining the color palette for each of the Choropleth Maps (CM1 and CM2).
  varCM1_Palette <- reactive({
    #Defining inputs.
    varCM1_Palette_inTable <- varCM1_Normalization()$varCM1
    refPalette <- varCM_generalPalette()$varCM
    
    #Producing the color palette for each map.
    varCM1_Pal <- sapply(varCM1_Palette_inTable, mapPalette, refPalette)

    return(list(varCM1 = varCM1_Pal))
  })
  
  varCM2_Palette <- reactive({
    #Defining inputs.
    varCM2_Palette_inTable <- varCM2_Normalization()$varCM2
    refPalette <- varCM_generalPalette()$varCM
    
    #Producing the color palette for each map.
    varCM2_Pal <- sapply(varCM2_Palette_inTable, mapPalette, refPalette)
    
    return(list(varCM2 = varCM2_Pal))
  })
  
  #Generating the first Choropleth Map (CM1) and the corresponding legend.
  output$variantsCM_1 <- renderLeaflet({
    #Defining inputs.
    varCM1_inTable <- varCM1_Normalization()$varCM1
    varCM1_linSel <- input$variantSel_CM1
    varCMData <- geomDataSelector()$geomData
    varCM1_Palette <- varCM1_Palette()$varCM1
    refPalette <- varCM_generalPalette()$varCM
    
    #Generating the first Choropleth Map (CM1).
    dataPlotter_ChoroMap(varCM1_inTable,
                         varCM1_linSel,
                         varCMData,
                         varCM1_Palette,
                         refPalette)
  })
  
  #Generating the second Choropleth Map (CM2) and the corresponding legend.
  output$variantsCM_2 <- renderLeaflet({
    #Defining inputs.
    varCM2_inTable <- varCM2_Normalization()$varCM2
    varCM2_linSel <- input$variantSel_CM2
    varCMData <- geomDataSelector()$geomData
    varCM2_Palette <- varCM2_Palette()$varCM2
    refPalette <- varCM_generalPalette()$varCM
    
    #Generating the second Choropleth Map (CM2).
    dataPlotter_ChoroMap(varCM2_inTable,
                         varCM2_linSel,
                         varCMData,
                         varCM2_Palette,
                         refPalette)
  })
  
  #######LINEAGES TAB#######
  #####Analysing Lineages data.
  ###INPUT TABLES AND PERSONALISATION WIDGETS###
  ###Checking for the availability of input tables required by the Lineages Tab.
  ###Producing the widgets allowing to personalize data and plots from the Lineages Tab.
  #Checking on input tables availability.
  linTab_checkInTabs <- reactive  ({
    #Defining inputs.
    check_allLin <- checkInTabs()$allLin
    check_heatChoroM <- checkInTabs()$heatChoroM
    check_totReg <- checkInTabs()$totReg
    
    check_InputTabs <- c(check_allLin, check_heatChoroM, check_totReg)
    names(check_InputTabs) <- c("allLin", "HeatChoromap", "totReg")
    
    missing_InputTabs <- is.na(check_InputTabs)
    missing_InputTabs_names <- names(check_InputTabs[missing_InputTabs])
    
    validate(need(!any(missing_InputTabs),
                  paste0("Function not available. Missing ", toString(missing_InputTabs_names), " input table(s).")))
    
  })
  
  #Generating the radio buttons for minimum frequency (%) selection.
  output$linFrequency <- renderUI({
    #Checking input tables availability.
    linTab_checkInTabs()
    
    #Generating the minimum frequency (%) selection radio buttons.
    radioButtons("lineagesFreq",
                 "Min % of genomes",
                 choices = list("1%"=0.01,
                                "2.5%"=0.025,
                                "5%"=0.05,
                                "7.5%"=0.075,
                                "10%"=0.10),
                 selected = 0.01)
    
  })
  
  #Generating the drop down menu for the selection number of lineages selection.
  output$linNumber <- renderUI({
    #Checking input tables availability.
    linTab_checkInTabs()
    
    #Generating the number of lineages selection drop down menu.
    selectInput("lineagesNum",
                "Lineages number",
                choices = list("1"=1,
                               "2"=2,
                               "3"=3,
                               "4"=4,
                               "5"=5,
                               "6"=6,
                               "7"=7,
                               "8"=8,
                               "9"=9,
                               "10"=10),
                selected = 5)
    
  })
  
  ###STACKED AREA CHART AND BARPLOT###
  ###Producing the inputs for and plotting the Stacked Area Chart (SAC) and BarPlot (BP).
  #Subsetting the Lineages input table respect to the minimum
  #frequency (%) and the maximum number of displayed Lineages.
  #The latter is selected by the user through the corresponding
  #widget.
  allLinSAC_Subsetting <- reactive({
    #Checking input tables availability.
    linTab_checkInTabs()
    
    #Defining inputs.
    allLinSAC_Subsetting_inTable <- timeSelector()$allLin
    totSeq <- seqSum()$totSeq
    allLinSAC_Subsetting_minFreq <- input$lineagesFreq
    allLinSAC_Subsetting_maxViz <- input$lineagesNum
    
    #Subsetting data.
    allLinSAC_Subsetted <- dataFreqViz_Subsetter(allLinSAC_Subsetting_inTable,
                                                 totSeq,
                                                 allLinSAC_Subsetting_minFreq,
                                                 allLinSAC_Subsetting_maxViz)
    
    #The final input table MUST contain at least 1 Lineage.
    validate(need(nrow(allLinSAC_Subsetted)>0,
                  paste("0 Lineages with a % of sequenced genomes higher than",
                        paste0(as.numeric(allLinSAC_Subsetting_minFreq)*100,"%"),
                        "in the selected weeks range")))
    
    return(list(allLinSAC = allLinSAC_Subsetted))
  })
  
  #Normalizing the Lineages input table.
  allLinSAC_Normalization <- reactive({
    #Defining inputs.
    allLinSAC_Normalization_inTable <- allLinSAC_Subsetting()$allLinSAC
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    
    #Normalizing data.
    allLinSAC_Normalized <- dataNormalizer(allLinSAC_Normalization_inTable,
                                           totSeq_perWeek)
    allLinSAC_Normalized <- allLinSAC_Normalized*100
    
    return(list(allLinSAC = allLinSAC_Normalized))
  })
  
  #Producing the color palette for the Stacked Area Chart (SAC).
  allLinSAC_Palette <- reactive({
    #Defining inputs.
    allLinSAC_tabPal <- allLinTheme
    allLinSAC_Palette_inTable <- allLinSAC_Subsetting()$allLinSAC
    
    #Producing the color palette.
    allLinSAC_Pal <- plotPalette(allLinSAC_tabPal,
                                 dataTable = allLinSAC_Palette_inTable)
    
    return(list(allLinSAC = allLinSAC_Pal))
  })
  
  #Generating a Stacked Area Chart (SAC) and the corresponding legend
  #for the Lineages input table.
  output$lineagesSAC <- renderPlot({
    #Defining inputs.
    pathogenSel <- input$pathogen
    
    allLinSAC_inTable <- allLinSAC_Normalization()$allLinSAC
    allLinSAC_Palette <- allLinSAC_Palette()$allLinSAC
    allLinSAC_plotMain <- "Lineages"
    allLinSAC_timeUnit <- pathogenSelConf_Table[pathogenSelConf_Table$PathogenAbbr==pathogenSel,]$TimeUn
    
    #Plotting
    dataPlotter_SAC(allLinSAC_inTable,
                    allLinSAC_Palette,
                    allLinSAC_plotMain,
                    allLinSAC_timeUnit)
  })
  
  #Generating the BarPlot (BP) displaying the number of genome sequences
  #available for each week of the selected time period.
  output$linTab_perWeekSeq <- renderPlot({
    #Checking input tables availability.
    linTab_checkInTabs()
    
    #Defining inputs.
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    
    #Plotting.
    perWeekTotSeqPlotter_BP(totSeq_perWeek)
  })
  
  ###SCATTERPLOT###
  ###Producing the input for and plotting the ScatterPlot (SP).
  #Subsetting the Lineages input table respect to the minimum
  #frequency (%) and the maximum number of displayed Lineages.
  #The latter is selected by the user through the corresponding
  #widget.
#  allLinSP_Subsetting <- reactive({
    #Checking input tables availability.
#    linTab_checkInTabs()
    
    #Defining inputs.
#    allLinSP_Subsetting_inTable <- timeSelector()$allLin
#    totSeq <- seqSum()$totSeq
#    allLinSP_Subsetting_minFreq <- input$lineagesFreq
#    allLinSP_Subsetting_maxViz <- input$lineagesNum
#    allLinSP_timeUnit <- pathogenSelConf_Table[pathogenSelConf_Table$PathogenAbbr==pathogenSel,]$TimeUn
    
    #Subsetting data.
#    allLinSP_Subsetted <- dataFreqViz_Subsetter(allLinSP_Subsetting_inTable,
#                                                totSeq,
#                                                allLinSP_Subsetting_minFreq,
#                                                allLinSP_Subsetting_maxViz,
#                                                allLinSP_timeUnit)
    
    #The final input table MUST contain at least 1 Lineage.
#    validate(need(nrow(allLinSP_Subsetted)>0,
#                  paste("0 Lineages with a % of sequenced genomes higher than",
#                        paste0(as.numeric(allLinSP_Subsetting_minFreq)*100,"%"),
#                        "in the selected weeks range")))
    
#    return(list(allLinSP = allLinSP_Subsetted))
#  })
  
  #Producing the color palette for the ScatterPlot (SP).
#  allLinSP_Palette <- reactive({
    #Defining inputs.
#    allLinSP_tabPal <- allLinTheme
#    allLinSP_Palette_inTable <- allLinSP_Subsetting()$allLinSP
    
    #Producing the color palette.
#    allLinSP_Pal <- plotPalette(allLinSP_tabPal,
#                                dataTable = allLinSP_Palette_inTable)
    
#    return(list(allLinSP = allLinSP_Pal))
#  })
  
  #Generating a ScatterPlot (SP) that allows to compare the number
  #of occurrences of a number n of Lineages of interest vs all other
  #Lineages (if present) and the corresponding legend.
#  output$lineagesSP <- renderPlot({
    #Defining inputs.
#    allLinSP_inTable <- allLinSP_Subsetting()$allLinSP
#    allLinSP_Palette <- allLinSP_Palette()$allLinSP
#    allLinSP_plotMain <- "Lineages"
    
    #Plotting.
#    dataPlotter_SP(allLinSP_inTable,
#                   allLinSP_Palette,
#                   allLinSP_plotMain)
#  })
  
  ###HEATMAP & MAPS###
  ###Producing the common tables for the HeatMap (HM) and the Choropleth Maps (CM).
  #Calculating the total number of sequenced genomes for each Lineage
  #at regional level in the user-selected time lapse.
  allLinHMxCM_totSeq <- reactive({
    #Checking input tables availability.
    linTab_checkInTabs()
    
    #Defining inputs.
    allLinHMxCM_totSeq_inTable <- timeSelector()$heatChoroM
    
    #Calculating the total number of sequenced genomes.
    allLinHMxCM_totSeq_Table <- allLinHMxCM_totSeq_inTable[,1:2]
    allLinHMxCM_totSeq_Table <- cbind(allLinHMxCM_totSeq_Table,
                                      totSeq = rowSums(allLinHMxCM_totSeq_inTable[,3:length(allLinHMxCM_totSeq_inTable)]))
    
    return(list(allLinHMxCM = allLinHMxCM_totSeq_Table))
  })
  
  ###HEATMAP###
  ###Producing the input tables for and plotting the HeatMap (HM).
  #Generating the input table for the HeatMap (HM).
  allLinHM_dataSelector <- reactive({
    #Defining inputs.
    allLinHM_dataSelector_inTable <- allLinHMxCM_totSeq()$allLinHMxCM
    allLinHM_dataSelector_regNames <- unique(allLinHM_dataSelector_inTable$reg)
    allLinHM_dataSelector_linNames <- unique(allLinHM_dataSelector_inTable$lin)
    
    #Producing the Regions x Lineages table.
    allLinHM_dataSel <- sapply(allLinHM_dataSelector_linNames,
                               linReg_dataSelector,
                               allLinHM_dataSelector_inTable)
    
    rownames(allLinHM_dataSel) <- allLinHM_dataSelector_regNames
    
    return(list(allLinHM = allLinHM_dataSel))
  })
  
  #Subsetting and sorting the HeatMap (HM) input table.
  allLinHM_SubsetSort <- reactive({
    #Defining inputs.
    allLinHM_SubsetSort_inTable <- allLinHM_dataSelector()$allLinHM
    totReg <- seqSumReg()$totReg
    
    #Subsetting and sorting the input table.
    allLinHM_SubsetSorted <- dataReg_SubSorter(allLinHM_SubsetSort_inTable,
                                               totReg)
    
    #The final input table MUST contain at least 1 Lineage.
    validate(need(ncol(allLinHM_SubsetSorted)>0,
                  "0 Lineages with at least 1 sequenced genome in the selected weeks range"))
    
    return(list(allLinHM = allLinHM_SubsetSorted))
  })
  
  #Normalizing the HeatMap (HM) input table.
  allLinHM_Normalization <- reactive({
    #Defining inputs.
    allLinHM_Normalization_inTable <- allLinHM_SubsetSort()$allLinHM
    totSeq_perReg <- seqSumReg()$totSeq_perReg
    
    #Normalizing data.
    allLinHM_Normalized <- allLinHM_Normalization_inTable/totSeq_perReg
    allLinHM_Normalized <- allLinHM_Normalized*100
    allLinHM_Normalized[is.nan(allLinHM_Normalized)] <- 0
    
    return(list(allLinHM = allLinHM_Normalized))
  })
  
  #Producing the color palette for the HeatMap (HM).
  allLinHM_Palette <- reactive({
    #Defining inputs.
    allLinHM_tabPal <- allLinTheme
    allLinHM_Palette_inTable <- allLinHM_Normalization()$allLinHM
    allLinHM_Palette_colorsNum <- ceiling(max(allLinHM_Palette_inTable)/10)
    
    #Producing the color palette.
    allLinHM_Pal <- plotPalette(allLinHM_tabPal,
                                colorsNum = allLinHM_Palette_colorsNum)
    
    return(list(allLinHM = allLinHM_Pal))
  })
  
  #Generating the plot area for the HeatMap (HM).
  output$allLinHM_plotUI <- renderUI({
    #Checking input tables availability.
    linTab_checkInTabs()
    
    #Defining inputs.
    allLinHM_plotUI_inTable <- allLinHM_Normalization()$allLinHM
    allLinHM_plotUI_nReg <- nrow(allLinHM_plotUI_inTable)
    allLinHM_plotUI_plotHeight <- 15*allLinHM_plotUI_nReg+100
    
    #Generating the plot area.
    if (allLinHM_plotUI_plotHeight>500) {
      
      plotOutput("lineagesHM",
                 height = paste0(allLinHM_plotUI_plotHeight, "px"))
      
    } else {
      
      plotOutput("lineagesHM",
                 height = "500px")
      
    }
    
  })
  
  #Generating the HeatMap (HM) and the corresponding legend.
  output$lineagesHM <- renderPlot({
    #Defining inputs.
    allLinHM_inTable <- allLinHM_Normalization()$allLinHM
    allLinHM_dataNum <- ifelse(ncol(allLinHM_inTable)>=25, 25, ncol(allLinHM_inTable))
    allLinHM_Palette <- allLinHM_Palette()$allLinHM
    allLinHM_Main <- "Lineages Regional Frequency (%)"
    allLinHM_tagColor <- "gray49"
    
    #Plotting.
    if (ncol(allLinHM_inTable)>1) {
      
      dataPlotter_HM(allLinHM_inTable,
                     allLinHM_dataNum,
                     allLinHM_Palette,
                     allLinHM_Main,
                     allLinHM_tagColor)
      
    } else if (ncol(allLinHM_inTable)==1) {
      
      allLinHM_inTable <- t(allLinHM_inTable)
      allLinHM_Palette <- "#41B6C4"
      allLinHM_Main <- paste(row.names(allLinHM_inTable), "Regional Frequency (%)")
      
      dataPlotter_regBP(allLinHM_inTable,
                        allLinHM_Palette,
                        allLinHM_Main)
      
    }
    
  })
  
  ###MAPS###
  ###Producing the input tables for and plotting the Choropleth Maps (CM).
  #Subsetting the input table for Lineages respect to minimum frequency (%).
  #Final results are used to generate the list of Lineages that can be
  #represented in the Choropleth Maps (CM).
  allLinCM_dataSubsetting <- reactive({
    #Checking input tables availability.
    linTab_checkInTabs()
    
    #Defining inputs.
    allLinCM_dataSubsetting_inTable <- timeSelector()$allLin
    totSeq <- seqSum()$totSeq
    allLinCM_dataSubsetting_minFreq <- input$lineagesFreq
    
    #Subsetting data.
    allLinCM_dataSubsetted <- dataFreq_Subsetter(allLinCM_dataSubsetting_inTable,
                                                 totSeq,
                                                 allLinCM_dataSubsetting_minFreq)
    
    #The final input table MUST contain at least 1 Lineage.
    validate(need(nrow(allLinCM_dataSubsetted)>0,
                  paste("0 Lineages with a % of sequenced genomes higher than",
                        paste0(as.numeric(allLinCM_dataSubsetting_minFreq)*100,"%"),
                        "in the selected weeks range")))
    
    return(list(allLinCM = allLinCM_dataSubsetted))
  })
  
  #Generating the drop down menu that allows to select a Lineage to represent
  #in the first Choropleth Map (CM1).
  output$lineageCM1 <- renderUI({
    #Checking input tables availability.
    linTab_checkInTabs()
    
    #Defining inputs.
    linSel_allLinCM1_inTable <- allLinCM_dataSubsetting()$allLinCM
    linSel_allLinCM1_widgetName <- "lineageSel_CM1"
    linSel_allLinCM1_widgetMain <- "Lineage (Map 1)"
    posDefault <- 1
    
    #Producing the drop down menu.
    dropdownM_widgetGenerator(linSel_allLinCM1_inTable,
                              linSel_allLinCM1_widgetName,
                              linSel_allLinCM1_widgetMain,
                              posDefault)
  })
  
  #Generating the drop down menu that allows to select a Lineage to represent
  #in the second Choropleth Map (CM2).
  output$lineageCM2 <- renderUI({
    #Checking input tables availability.
    linTab_checkInTabs()
    
    #Defining inputs.
    linSel_allLinCM2_inTable <- allLinCM_dataSubsetting()$allLinCM
    linSel_allLinCM2_widgetName <- "lineageSel_CM2"
    linSel_allLinCM2_widgetMain <- "Lineage (Map 2)"
    posDefault <- 2
    
    #The final input table MUST contain at least 2 Lineages to produce CM2 as well.
    validate(need(nrow(linSel_allLinCM2_inTable)>1,
                  paste("Only 1 Lineage with a % of sequenced genomes higher than",
                        paste0(as.numeric(input$lineagesFreq)*100,"%"),
                        "in the selected weeks range")))
    
    #Producing the drop down menu.
    dropdownM_widgetGenerator(linSel_allLinCM2_inTable,
                              linSel_allLinCM2_widgetName,
                              linSel_allLinCM2_widgetMain,
                              posDefault)
  })
  
  #Collecting data for the selected Lineages and producing the corresponding input
  #tables for both Choropleth Maps (CM1 and CM2).
  allLinCM1_dataSelector <- reactive({
    #Defining inputs.
    allLinCM1_dataSelector_inTable <- allLinHMxCM_totSeq()$allLinHMxCM
    allLinCM1_dataSelector_regNames <- unique(allLinCM1_dataSelector_inTable$reg)
    
    allLinCM1_dataSelector_LineageSel <- input$lineageSel_CM1

    #Reading the input data tables.
    allLinCM1_dataSel <- allLinCM1_dataSelector_inTable[allLinCM1_dataSelector_inTable$lin%in%allLinCM1_dataSelector_LineageSel,]$totSeq
    allLinCM1_dataTable <- data.frame(totSeq = allLinCM1_dataSel,
                                      row.names = allLinCM1_dataSelector_regNames)
    allLinCM1_dataTable <- as.matrix(allLinCM1_dataTable)
    
    return(list(allLinCM1 = allLinCM1_dataTable))
  })
  
  allLinCM2_dataSelector <- reactive({
    #The final input table MUST contain at least 2 Lineages to produce CM2 as well.
    validate(need(nrow(allLinCM_dataSubsetting()$allLinCM)>1,
                  paste("Only 1 Lineage with a % of sequenced genomes higher than",
                        paste0(as.numeric(input$lineagesFreq)*100,"%"),
                        "in the selected weeks range")))
    
    #Defining inputs.
    allLinCM2_dataSelector_inTable <- allLinHMxCM_totSeq()$allLinHMxCM
    allLinCM2_dataSelector_regNames <- unique(allLinCM2_dataSelector_inTable$reg)
    
    allLinCM2_dataSelector_LineageSel <- input$lineageSel_CM2
    
    #Reading the input data tables.
    allLinCM2_dataSel <- allLinCM2_dataSelector_inTable[allLinCM2_dataSelector_inTable$lin%in%allLinCM2_dataSelector_LineageSel,]$totSeq
    allLinCM2_dataTable <- data.frame(totSeq = allLinCM2_dataSel,
                                      row.names = allLinCM2_dataSelector_regNames)
    allLinCM2_dataTable <- as.matrix(allLinCM2_dataTable)
    
    return(list(allLinCM2 = allLinCM2_dataTable))
  })
  
  #Normalizing the input tables for the selected Lineages at a regional
  #level in the Country of interest for both Choropleth Maps (CM1 and CM2).
  allLinCM1_Normalization <- reactive({
    #Defining inputs.
    allLinCM1_Normalization_inTable <- allLinCM1_dataSelector()$allLinCM1
    totReg <- seqSumReg()$totSeq_perReg
    
    #Normalizing data.
    allLinCM1_Normalized <- allLinCM1_Normalization_inTable/totReg
    allLinCM1_Normalized <- allLinCM1_Normalized*100
    allLinCM1_Normalized[is.nan(allLinCM1_Normalized)] <- 0
    allLinCM1_Normalized <- signif(allLinCM1_Normalized, digits = 3)

    return(list(allLinCM1 = allLinCM1_Normalized))
  })
  
  allLinCM2_Normalization <- reactive({
    #Defining inputs.
    allLinCM2_Normalization_inTable <- allLinCM2_dataSelector()$allLinCM2
    totReg <- seqSumReg()$totSeq_perReg
    
    #Normalizing data.
    allLinCM2_Normalized <- allLinCM2_Normalization_inTable/totReg
    allLinCM2_Normalized <- allLinCM2_Normalized*100
    allLinCM2_Normalized[is.nan(allLinCM2_Normalized)] <- 0
    allLinCM2_Normalized <- signif(allLinCM2_Normalized, digits = 3)
    
    return(list(allLinCM2 = allLinCM2_Normalized))
  })
  
  #Defining the general color palette for the maps in the Lineages Tab.
  #The palette consist of ten colors each one defining an interval of
  #frequency (%). Colors in the palette are used to fill each region
  #of the country of interest according to the frequency (%) of the Lineage
  #of interest.
  allLinCM_generalPalette <- reactive ({
    #Defining inputs.
    allLinCM_generalPalette_tabPal <- allLinTheme
    allLinCM_generalPalette_colorsNum <- 10
    
    #Producing the color palette.
    allLinCM_Pal <- plotPalette(allLinCM_generalPalette_tabPal,
                                colorsNum = allLinCM_generalPalette_colorsNum)
    
    return(list(allLinCM = allLinCM_Pal))
  })
  
  #Defining the color palette for each of the Choropleth Maps (CM1 and CM2).
  allLinCM1_Palette <- reactive({
    #Defining inputs.
    allLinCM1_Palette_inTable <- allLinCM1_Normalization()$allLinCM1
    refPalette <- allLinCM_generalPalette()$allLinCM
    
    #Producing the color palette for each map.
    allLinCM1_Pal <- sapply(allLinCM1_Palette_inTable, mapPalette, refPalette)

    return(list(allLinCM1 = allLinCM1_Pal))
  })
  
  allLinCM2_Palette <- reactive({
    #Defining inputs.
    allLinCM2_Palette_inTable <- allLinCM2_Normalization()$allLinCM2
    refPalette <- allLinCM_generalPalette()$allLinCM
    
    #Producing the color palette for each map.
    allLinCM2_Pal <- sapply(allLinCM2_Palette_inTable, mapPalette, refPalette)
    
    return(list(allLinCM2 = allLinCM2_Pal))
  })
  
  #Generating the first Choropleth Map (CM1) and the corresponding legend.
  output$lineagesCM_1 <- renderLeaflet({
    #Defining inputs.
    allLinCM1_inTable <- allLinCM1_Normalization()$allLinCM1
    allLinCM1_linSel <- input$lineageSel_CM1
    allLinCMData <- geomDataSelector()$geomData
    allLinCM1_Palette <- allLinCM1_Palette()$allLinCM1
    refPalette <- allLinCM_generalPalette()$allLinCM
    
    #Generating the first Choropleth Map (CM1).
    dataPlotter_ChoroMap(allLinCM1_inTable,
                         allLinCM1_linSel,
                         allLinCMData,
                         allLinCM1_Palette,
                         refPalette)
  })
  
  #Generating the second Choropleth Map (CM2) and the corresponding legend.
  output$lineagesCM_2 <- renderLeaflet({
    #Defining inputs.
    allLinCM2_inTable <- allLinCM2_Normalization()$allLinCM2
    allLinCM2_linSel <- input$lineageSel_CM2
    allLinCMData <- geomDataSelector()$geomData
    allLinCM2_Palette <- allLinCM2_Palette()$allLinCM2
    refPalette <- allLinCM_generalPalette()$allLinCM
    
    #Generating the second Choropleth Map (CM2).
    dataPlotter_ChoroMap(allLinCM2_inTable,
                         allLinCM2_linSel,
                         allLinCMData,
                         allLinCM2_Palette,
                         refPalette)
  })
  
  #######MUTATIONS TAB#######
  #####Analysing Mutations data.
  ###INPUT TABLES AND PERSONALISATION WIDGETS###
  ###Checking for the availability of input tables required by the Mutations Tab.
  ###Producing the widgets allowing to personalize data and plots from the Mutations Tab.
  #Checking on input tables availability.
  mutTab_checkInTabs <- reactive  ({
    #Defining inputs.
    check_allLin <- checkInTabs()$allLin
    check_mut <- checkInTabs()$mut
    check_heatChoroM <- checkInTabs()$heatChoroM
    check_totReg <- checkInTabs()$totReg
    
    check_InputTabs <- c(check_allLin, check_mut, check_heatChoroM, check_totReg)
    names(check_InputTabs) <- c("allLin", "Mut", "HeatChoromap", "totReg")
    
    missing_InputTabs <- is.na(check_InputTabs)
    missing_InputTabs_names <- names(check_InputTabs[missing_InputTabs])
    
    validate(need(!any(missing_InputTabs),
                  paste0("Function not available. Missing ", toString(missing_InputTabs_names), " input table(s).")))
    
  })
  
  ###ALL PLOTS###
  ###Producing inputs common to all the plots in the Mutations Tab.
  #Generating the drop down menu that allows to select a Lineage which
  #non-defining Mutations are analyzed in the Mutations Tab.
  output$mutationsLineage <- renderUI({
    #Checking input tables availability.
    mutTab_checkInTabs()
    
    #Defining inputs.
    linSel_mutTab_inTable <- allLinCM_dataSubsetting()$allLinCM
    linSel_mutTab_widgetName <- "lineageSel_mutTab"
    linSel_mutTab_widgetMain <- "Lineage"
    posDefault <- 1
    
    #Producing the drop down menu.
    dropdownM_widgetGenerator(linSel_mutTab_inTable,
                              linSel_mutTab_widgetName,
                              linSel_mutTab_widgetMain,
                              posDefault)
  })
  
  #Calculating the total number of:
  #1. Sequences in the user-selected time lapse for the user-selected
  #   Lineage.
  #2. Per week sequences in the user-selected time lapse for the
  #   user-selected Lineage.
  #Starting from national counts.
  #3. Per region sequences in the user-selected time lapse for the
  #   user-selected Lineage
  #Starting from regional counts.
  #These values will be used to sort data by frequency (%) and
  #to normalize data.
  mutTab_seqSum <- reactive({
    #Defining inputs.
    mutTab_seqSum_inTable <- timeSelector()$allLin
    mutTab_seqSum_inTable_perReg <- timeSelector()$heatChoroM
    mutTab_seqSum_linSel <- input$lineageSel_mutTab
    
    #Selecting data for the user-selected Lineage.
    mutTab_seqSum_dataSel <- mutTab_seqSum_inTable[row.names(mutTab_seqSum_inTable)%in%mutTab_seqSum_linSel,]
    mutTab_seqSum_dataSel_perReg <- mutTab_seqSum_inTable_perReg[mutTab_seqSum_inTable_perReg$lin%in%mutTab_seqSum_linSel,]
    mutTab_seqSum_dataSel_perReg$lin <- NULL
    row.names(mutTab_seqSum_dataSel_perReg) <- mutTab_seqSum_dataSel_perReg$reg
    mutTab_seqSum_dataSel_perReg$reg <- NULL
    
    #Calculating the total number of sequences in the user-selected time
    #lapse of interest.
    mutTab_seqSum_totSeq <- sum(mutTab_seqSum_dataSel)
    
    #Calculating the total number of sequences for each week in the
    #user-selected time lapse of interest
    mutTab_seqSum_totSeq_perWeek <- mutTab_seqSum_dataSel
    
    #Calculating the regional number of sequences in the user-selected time
    #lapse of interest.
    mutTab_seqSum_totSeq_perReg <- rowSums(mutTab_seqSum_dataSel_perReg)
    
    #The total number of genomes sequenced in the time period of interest MUST be >0
    #otherwise the app rises a warning.
    validate(need(mutTab_seqSum_totSeq>0,
                  "0 sequenced genomes in the selected weeks range"))
    
    return(list(mutTab_totSeq = mutTab_seqSum_totSeq,
                mutTab_totSeq_perWeek = mutTab_seqSum_totSeq_perWeek,
                mutTab_totSeq_perReg = mutTab_seqSum_totSeq_perReg))
  })
  
  #Collecting data for the user-selected Lineage of interest from the
  #Mutations input table. Data consists of counts for non-defining
  #mutations of the selected Lineage evaluated both at national and
  #regional level.
  mutTab_dataSelector <- reactive({
    #Checking input tables availability.
    mutTab_checkInTabs()
    
    #Defining inputs.
    mutTab_dataSelector_inTable <- timeSelector()$mut
    mutTab_dataSelector_linSel <- input$lineageSel_mutTab
    
    #Selecting data.
    mutTab_dataSelector_dataSel <- mutTab_dataSelector_inTable[mutTab_dataSelector_inTable$lineage%in%mutTab_dataSelector_linSel,]
    mutTab_dataSelector_dataSel$lineage <- NULL

    return(list(mutTab = mutTab_dataSelector_dataSel))
  })
  
  ###BARPLOTS###
  ###Producing the inputs for and plotting the BarPlots (BP).
  #Selecting data for the input table of the BarPlot (BP) and sorting them.
  #The input table for each of the BP includes counts for either the first or
  #second most frequent non-defining Mutation of the user-selected Lineage
  #and the total number of genomes without that specific Mutation.
  mutBP_SelectSort <- reactive({
    #Defining inputs.
    mutBP_SelectSort_inTable <- mutTab_dataSelector()$mutTab
    mutBP_SelectSort_countrySel <- input$country
    mutTab_totSeq <- mutTab_seqSum()$mutTab_totSeq
    mutTab_totSeq_perWeek <- mutTab_seqSum()$mutTab_totSeq_perWeek
    mutBP1_SelectSort_dataPos <- 1
    mutBP2_SelectSort_dataPos <- 2
    
    #Selecting and sorting data.
    mutBP1_SelectSorted <- mutBP_SelSorter(mutBP_SelectSort_inTable,
                                           mutBP_SelectSort_countrySel,
                                           mutTab_totSeq,
                                           mutBP1_SelectSort_dataPos,
                                           mutTab_totSeq_perWeek)
    
    mutBP2_SelectSorted <- mutBP_SelSorter(mutBP_SelectSort_inTable,
                                           mutBP_SelectSort_countrySel,
                                           mutTab_totSeq,
                                           mutBP2_SelectSort_dataPos,
                                           mutTab_totSeq_perWeek)
    
    return(list(mutBP1 = mutBP1_SelectSorted,
                mutBP2 = mutBP2_SelectSorted))
  })
  
  #Normalizing the input table for the BarPlot (BP).
  mutBP_Normalization <- reactive({
    #Defining inputs.
    mutBP1_Normalization_inTable <- mutBP_SelectSort()$mutBP1
    mutBP2_Normalization_inTable <- mutBP_SelectSort()$mutBP2
    mutTab_totSeq_perWeek <- mutTab_seqSum()$mutTab_totSeq_perWeek
    
    #Normalizing data.
    mutBP1_Normalized <- dataNormalizer(mutBP1_Normalization_inTable,
                                        mutTab_totSeq_perWeek)
    mutBP1_Normalized <- mutBP1_Normalized*100
    
    mutBP2_Normalized <- dataNormalizer(mutBP2_Normalization_inTable,
                                        mutTab_totSeq_perWeek)
    mutBP2_Normalized <- mutBP2_Normalized*100
    
    return(list(mutBP1 = mutBP1_Normalized,
                mutBP2 = mutBP2_Normalized))
  })
  
  #Generating the first BarPlot (BP1) and the corresponding legend.
  output$mutationsBP_1 <- renderPlot({
    #Defining inputs.
    pathogenSel <- input$pathogen
    
    mutBP1_inTable <- mutBP_Normalization()$mutBP1
    mutBP1_Palette <- c("#3690C0", "#014636")
    mutBP1_plotMain <- paste(row.names(mutBP1_inTable)[1], "Frequency (%)")
    mutBP1_timeUnit <- pathogenSelConf_Table[pathogenSelConf_Table$PathogenAbbr==pathogenSel,]$TimeUn
    
    #Plotting.
    dataPlotter_BP(mutBP1_inTable,
                   mutBP1_Palette,
                   mutBP1_plotMain,
                   mutBP1_timeUnit)
  })
  
  #Generating the second BarPlot (BP2) and the corresponding legend.
  output$mutationsBP_2 <- renderPlot({
    #Defining inputs.
    pathogenSel <- input$pathogen
    
    mutBP2_inTable <- mutBP_Normalization()$mutBP2
    mutBP2_Palette <- c("#3690C0", "#014636")
    mutBP2_plotMain <- paste(row.names(mutBP2_inTable)[1], "Frequency (%)")
    mutBP2_timeUnit <- pathogenSelConf_Table[pathogenSelConf_Table$PathogenAbbr==pathogenSel,]$TimeUn
    
    #The input table MUST contain numeric data to produce BP2 as well. If only a single
    #non-defining Mutation is identified for the Lineage of interest BP2 can not be
    #prodiced.
    validate(need(!all(is.na(mutBP2_inTable)),
                  "Only 1 Mutations with sequenced genomes in the selected weeks range"))
    
    #Plotting.
    dataPlotter_BP(mutBP2_inTable,
                   mutBP2_Palette,
                   mutBP2_plotMain,
                   mutBP2_timeUnit)
  })
  
  #Generating the BarPlot (BP) displaying the number of genome sequences
  #available for each week of the selected time period for the user-selected
  #Lineage.
  output$mutTab_perWeekSeq <- renderPlot({
    #Checking input tables availability.
    mutTab_checkInTabs()
    
    #Defining inputs.
    mutTab_totSeq_perWeek <- unlist(mutTab_seqSum()$mutTab_totSeq_perWeek)
    
    #Plotting.
    perWeekTotSeqPlotter_BP(mutTab_totSeq_perWeek)
  })
  
  ###HEATMAP & MAPS###
  ###Producing the common tables for the HeatMap (HM) and the Choropleth Maps (CM).
  #Calculating the total number of sequenced genomes for each Mutation at
  #regional level in the user-selected time lapse. The national counts are
  #removed from the tables as they are not represented in these plots.
  mutHMxCM_totSeq <- reactive({
    #Defining inputs.
    mutHMxCM_totSeq_countrySel <- input$country
    mutHMxCM_totSeq_inTable <- mutTab_dataSelector()$mutTab
    mutHMxCM_totSeq_inTable <- mutHMxCM_totSeq_inTable[mutHMxCM_totSeq_inTable$region!=mutHMxCM_totSeq_countrySel,]
    
    #Calculating the total number of sequenced genomes.
    mutHMxCM_totSeq_Table <- mutHMxCM_totSeq_inTable[,1:2]
    mutHMxCM_totSeq_Table <- cbind(mutHMxCM_totSeq_Table,
                                   totSeq = rowSums(mutHMxCM_totSeq_inTable[,3:length(mutHMxCM_totSeq_inTable)]))
    
    return(list(mutHMxCM = mutHMxCM_totSeq_Table))
  })
  
  ###HEATMAP###
  ###Producing the input tables for and plotting the HeatMap (HM).
  #Generating the input table for the Heatmap (HM).
  mutHM_dataSelector <- reactive({
    #Defining inputs.
    mutHM_dataSelector_inTable <- mutHMxCM_totSeq()$mutHMxCM
    mutHM_dataSelector_regNames <- unique(mutHM_dataSelector_inTable$region)
    mutHM_dataSelector_mutNames <- unique(mutHM_dataSelector_inTable$mutation)
    
    #Producing the Regions x Mutation table.
    mutHM_dataSel <- sapply(mutHM_dataSelector_mutNames,
                            mutReg_dataSelector,
                            mutHM_dataSelector_inTable)
    
    rownames(mutHM_dataSel) <- mutHM_dataSelector_regNames
    
    return(list(mutHM = mutHM_dataSel))
  })
  
  #Subsetting and sorting the HeatMap (HM) input table.
  mutHM_SubsetSort <- reactive({
    #Defining inputs.
    mutHM_SubsetSort_inTable <- mutHM_dataSelector()$mutHM
    mutTab_totSeq <- mutTab_seqSum()$mutTab_totSeq
    
    #Subsetting and sorting the input table.
    mutHM_SubsetSorted <- dataReg_SubSorter(mutHM_SubsetSort_inTable,
                                            mutTab_totSeq)
    
    #The final input table MUST contain at least 1 Mutation.
    validate(need(ncol(mutHM_SubsetSorted)>0,
                  "0 Mutations with at least 1 appearance in the selected weeks range"))
    
    return(list(mutHM = mutHM_SubsetSorted))
  })
  
  #Normalizing the HeatMap (HM) input table.
  mutHM_Normalization <- reactive({
    #Defining inputs.
    mutHM_Normalization_inTable <- mutHM_SubsetSort()$mutHM
    mutTab_totSeq_perReg <- mutTab_seqSum()$mutTab_totSeq_perReg

    #Normalizing data.
    mutHM_Normalized <- mutHM_Normalization_inTable/mutTab_totSeq_perReg
    mutHM_Normalized <- mutHM_Normalized*100
    mutHM_Normalized[is.nan(mutHM_Normalized)] <- 0
    
    return(list(mutHM = mutHM_Normalized))
  })
  
  #Producing the color palette for the HeatMap (HM)
  mutHM_Palette <- reactive({
    #Defining inputs.
    mutHM_tabPal <- mutTheme
    mutHM_Palette_inTable <- mutHM_Normalization()$mutHM
    mutHM_Palette_colorsNum <- ceiling(max(mutHM_Palette_inTable)/10)
    
    #Producing the color palette.
    mutHM_Pal <- plotPalette(mutHM_tabPal,
                             colorsNum = mutHM_Palette_colorsNum)
    
    return(list(mutHM = mutHM_Pal))
  })
  
  #Generating the plot area for the HeatMap (HM).
  output$mutHM_plotUI <- renderUI({
    #Defining inputs.
    mutHM_plotUI_inTable <- mutHM_Normalization()$mutHM
    mutHM_plotUI_nReg <- nrow(mutHM_plotUI_inTable)
    mutHM_plotUI_plotHeight <- 15*mutHM_plotUI_nReg+100
    
    #Generating the plot area.
    if (mutHM_plotUI_plotHeight>500) {
      
      plotOutput("mutationsHM",
                 height = paste0(mutHM_plotUI_plotHeight, "px"))
      
    } else {
      
      plotOutput("mutationsHM",
                 height = "500px")
      
    }
    
  })
  
  #Generating the HeatMap (HM) and the corresponding legend.
  output$mutationsHM <- renderPlot({
    #Defining inputs.
    mutHM_inTable <- mutHM_Normalization()$mutHM
    mutHM_dataNum <- ifelse(ncol(mutHM_inTable)>=25, 25, ncol(mutHM_inTable))
    mutHM_Palette <- mutHM_Palette()$mutHM
    mutHM_Main <- "Mutations Regional Frequency (%)"
    mutHM_tagColor <- "gray49"
    
    #Plotting.
    if (ncol(mutHM_inTable)>1) {
      
      dataPlotter_HM(mutHM_inTable,
                     mutHM_dataNum,
                     mutHM_Palette,
                     mutHM_Main,
                     mutHM_tagColor)
      
    } else if (ncol(mutHM_inTable)==1) {
      
      mutHM_inTable <- t(mutHM_inTable)
      mutHM_Palette <- "#67A9CF"
      mutHM_Main <- paste(row.names(mutHM_inTable), "Regional Frequency (%)")
      
      dataPlotter_regBP(mutHM_inTable,
                        mutHM_Palette,
                        mutHM_Main)
      
    }
    
  })
  
  ###MAPS###
  ###Producing the input tables for and plotting the Choropleth Maps (CM).
  #Subsetting the input table for Mutations respect to minimum frequency (%).
  #In the case of the Mutations Tab the frequency (%) limit is manually defined
  #and set at 1%. The process is performed on national data from the initial
  #Mutations input table. Final results are used to generate the list of Mutations
  #that can be represented in the Choropleth Maps (CM).
  mutCM_dataSubsetting <- reactive({
    #Defining inputs.
    mutCM_dataSubsetting_countrySel <- input$country
    mutCM_dataSubsetting_inTable <- mutTab_dataSelector()$mutTab
    mutTab_totSeq <- mutTab_seqSum()$mutTab_totSeq
    mutCM_dataSubsetting_minFreq <- 0.01
    
    #Selecting required data.
    mutCM_dataSubsetting_inTable <- mutCM_dataSubsetting_inTable[mutCM_dataSubsetting_inTable$region%in%mutCM_dataSubsetting_countrySel,]
    mutCM_dataSubsetting_inTable$region <- NULL
    row.names(mutCM_dataSubsetting_inTable) <- mutCM_dataSubsetting_inTable$mutation
    mutCM_dataSubsetting_inTable$mutation <- NULL
    
    #Subsetting data.
    mutCM_dataSubsetted <- dataFreq_Subsetter(mutCM_dataSubsetting_inTable,
                                              mutTab_totSeq,
                                              mutCM_dataSubsetting_minFreq)
    
    #The final input table MUST contain at least 1 Mutation.
    validate(need(nrow(mutCM_dataSubsetted)>0,
                  "0 Mutations with a % of sequenced genomes higher than 1% in the selected weeks range"))
    
    return(list(mutCM = mutCM_dataSubsetted))
  })
  
  #Generating the drop down menu that allows to select a Mutation to represent
  #in the first Choropleth Map (CM1).
  output$mutationCM1 <- renderUI({
    #Checking input tables availability.
    mutTab_checkInTabs()
    
    #Defining inputs.
    mutSel_mutCM1_inTable <- mutCM_dataSubsetting()$mutCM
    mutSel_mutCM1_widgetName <- "mutationSel_CM1"
    mutSel_mutCM1_widgetMain <- "Mutation (Map 1)"
    posDefault <- 1
    
    #Producing the drop down menu.
    dropdownM_widgetGenerator(mutSel_mutCM1_inTable,
                              mutSel_mutCM1_widgetName,
                              mutSel_mutCM1_widgetMain,
                              posDefault)
  })
  
  #Generating the drop down menu that allows to select a Mutation to represent
  #in the second Choropleth Map (CM2).
  output$mutationCM2 <- renderUI({
    #Checking input tables availability.
    mutTab_checkInTabs()
    
    #Defining inputs.
    mutSel_mutCM2_inTable <- mutCM_dataSubsetting()$mutCM
    mutSel_mutCM2_widgetName <- "mutationSel_CM2"
    mutSel_mutCM2_widgetMain <- "Mutation (Map 2)"
    posDefault <- 2
    
    #The input table MUST contain at least 2 Mutations to produce CM2 as well.
    validate(need(nrow(mutSel_mutCM2_inTable)>1,
                  "Only 1 Mutations with a % of sequenced genomes higher than 1% in the selected weeks range"))

    #Producing the drop down menu.
    dropdownM_widgetGenerator(mutSel_mutCM2_inTable,
                              mutSel_mutCM2_widgetName,
                              mutSel_mutCM2_widgetMain,
                              posDefault)
  })
  
  #Collecting data for the selected Mutations and producing the corresponding input
  #tables for both Choropleth Maps (CM1 and CM2).
  mutCM1_dataSelector <- reactive({
    #Defining inputs.
    mutCM1_dataSelector_inTable <- mutHMxCM_totSeq()$mutHMxCM
    mutCM1_dataSelector_regNames <- unique(mutCM1_dataSelector_inTable$region)
    
    mutCM1_dataSelector_LineageSel <- input$mutationSel_CM1

    #Reading the input data tables.
    mutCM1_dataSel <- mutCM1_dataSelector_inTable[mutCM1_dataSelector_inTable$mutation%in%mutCM1_dataSelector_LineageSel,]$totSeq
    mutCM1_dataTable <- data.frame(totSeq = mutCM1_dataSel,
                                   row.names = mutCM1_dataSelector_regNames)
    mutCM1_dataTable <- as.matrix(mutCM1_dataTable)
    
    return(list(mutCM1 = mutCM1_dataSel))
  })
  
  mutCM2_dataSelector <- reactive({
    #The input table MUST contain at least 2 Mutations to produce CM2 as well.
    validate(need(nrow(mutCM_dataSubsetting()$mutCM)>1,
                  "Only 1 Mutations with a % of sequenced genomes higher than 1% in the selected weeks range"))
    
    #Defining inputs.
    mutCM2_dataSelector_inTable <- mutHMxCM_totSeq()$mutHMxCM
    mutCM2_dataSelector_regNames <- unique(mutCM2_dataSelector_inTable$region)
    
    mutCM2_dataSelector_LineageSel <- input$mutationSel_CM2
    
    #Reading the input data tables.
    mutCM2_dataSel <- mutCM2_dataSelector_inTable[mutCM2_dataSelector_inTable$mutation%in%mutCM2_dataSelector_LineageSel,]$totSeq
    mutCM2_dataTable <- data.frame(totSeq = mutCM2_dataSel,
                                   row.names = mutCM2_dataSelector_regNames)
    mutCM2_dataTable <- as.matrix(mutCM2_dataTable)
    
    return(list(mutCM2 = mutCM2_dataSel))
  })
  
  #Normalizing the input tables for both Choropleth Maps (CM1 and CM2).
  mutCM1_Normalization <- reactive({
    #Defining inputs.
    mutCM1_Normalization_inTable <- mutCM1_dataSelector()$mutCM1
    mutTab_totSeq_perReg <- mutTab_seqSum()$mutTab_totSeq_perReg

    #Normalizing data.
    mutCM1_Normalized <- mutCM1_Normalization_inTable/mutTab_totSeq_perReg
    mutCM1_Normalized <- mutCM1_Normalized*100
    mutCM1_Normalized[is.nan(mutCM1_Normalized)] <- 0
    mutCM1_Normalized <- signif(mutCM1_Normalized, digits = 3)

    return(list(mutCM1 = mutCM1_Normalized))
  })
  
  mutCM2_Normalization <- reactive({
    #Defining inputs.
    mutCM2_Normalization_inTable <- mutCM2_dataSelector()$mutCM2
    mutTab_totSeq_perReg <- mutTab_seqSum()$mutTab_totSeq_perReg
    
    #Normalizing data.
    mutCM2_Normalized <- mutCM2_Normalization_inTable/mutTab_totSeq_perReg
    mutCM2_Normalized <- mutCM2_Normalized*100
    mutCM2_Normalized[is.nan(mutCM2_Normalized)] <- 0
    mutCM2_Normalized <- signif(mutCM2_Normalized, digits = 3)
    
    return(list(mutCM2 = mutCM2_Normalized))
  })
  
  #Defining the general color palette for the maps in the Mutations Tab.
  #The palette consist of ten colors each one defining an interval of
  #frequency (%). Colors in the palette are used to fill each region
  #of the country of interest according to the frequency (%) of a Mutation
  #of interest.
  mutCM_generalPalette <- reactive({
    #Defining inputs.
    mutCM_generalPalette_tabPal <- mutTheme
    mutCM_generalPalette_colorsNum <- 10
    
    #Producing the color palette.
    mutCM_Pal <- plotPalette(mutCM_generalPalette_tabPal,
                             colorsNum = mutCM_generalPalette_colorsNum)
    
    return(list(mutCM = mutCM_Pal))
  })
  
  #Defining the color palette for each of the Choropleth Maps (CM1 and CM2).
  mutCM1_Palette <- reactive({
    #Defining inputs.
    mutCM1_Palette_inTable <- mutCM1_Normalization()$mutCM1
    refPalette <- mutCM_generalPalette()$mutCM
    
    #Producing the color palette for each map.
    mutCM1_Pal <- sapply(mutCM1_Palette_inTable, mapPalette, refPalette)

    return(list(mutCM1 = mutCM1_Pal))
  })
  
  mutCM2_Palette <- reactive({
    #Defining inputs.
    mutCM2_Palette_inTable <- mutCM2_Normalization()$mutCM2
    refPalette <- mutCM_generalPalette()$mutCM
    
    #Producing the color palette for each map.
    mutCM2_Pal <- sapply(mutCM2_Palette_inTable, mapPalette, refPalette)
    
    return(list(mutCM2 = mutCM2_Pal))
  })
  
  #Generating the first Choropleth Map (CM1) and the corresponding legend.
  output$mutationsCM_1 <- renderLeaflet({
    #Defining inputs.
    mutCM1_inTable <- mutCM1_Normalization()$mutCM1
    mutCM1_mutSel <- input$mutationSel_CM1
    mutCMData <- geomDataSelector()$geomData
    mutCM1_Palette <- mutCM1_Palette()$mutCM1
    refPalette <- mutCM_generalPalette()$mutCM
    
    #Generating the first Choropleth Map (CM1).
    dataPlotter_ChoroMap(mutCM1_inTable,
                         mutCM1_mutSel,
                         mutCMData,
                         mutCM1_Palette,
                         refPalette)
  })
  
  #Generating the second Choropleth Map (CM2) and the corresponding legend.
  output$mutationsCM_2 <- renderLeaflet({
    #Defining inputs.
    mutCM2_inTable <- mutCM2_Normalization()$mutCM2
    mutCM2_mutSel <- input$mutationSel_CM2
    mutCMData <- geomDataSelector()$geomData
    mutCM2_Palette <- mutCM2_Palette()$mutCM2
    refPalette <- mutCM_generalPalette()$mutCM
    
    #Generating the second Choropleth Map (CM2).
    dataPlotter_ChoroMap(mutCM2_inTable,
                         mutCM2_mutSel,
                         mutCMData,
                         mutCM2_Palette,
                         refPalette)
  })
  
}
