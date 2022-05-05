#Define server -----
server <- function(input, output){
  #######COMMON DATA UPLOAD AND SUBSETTING#######
  #Reading the input tables for the selected country.
  #AGGIUNGO PATH E NOME FILE PER LA TABELLA DELLE VARIANTS
  countrySelector <- reactive({
    #Defining inputs.
    coutrySel <- countryConvertion_Table[input$country,1]
    
    var_baseName <- "#NOMEFILE"
    allLin_baseName <- "Epiweek."
    spLin_baseName <- "Epiweek.SpecialLin."
    mut_baseName <- "Epiweek.MutsSpike."
    
    exten <- ".csv"
    separ <- " "
    
    #Reading the input files.
    #var_countrySel <- read.table(paste0(var_path, var_baseName, coutrySel, exten),
                                      #sep = separ,
                                      #check.names = F)
    
    allLin_countrySel <- read.table(paste0(allLin_path, allLin_baseName, coutrySel, exten),
                                    sep = separ,
                                    check.names = F)
    
    spLin_countrySel <- read.table(paste0(spLin_path, spLin_baseName, coutrySel, exten),
                                        sep = separ,
                                        check.names = F)
    
    mut_countrySel <- read.table(paste0(mut_path, mut_baseName, coutrySel, exten),
                                 sep = separ,
                                 check.names = F)
    
    return(list(#var = var_countrySel,
                allLin = allLin_countrySel,
                spLin = spLin_countrySel,
                mut = mut_countrySel))
  })
  
  #Subsetting the input tables respect to the selected time lapse.
  weeksSelector <- reactive({
    #Defining inputs.
    startWeek <- input$weeksRange[1]
    endWeek <- input$weeksRange[2]
    
    #The selected time lapse MUST be >= 1 week otherwise the app rises
    #a warning.
    validate(need((endWeek-startWeek)>=1,
                  "Select a Weeks range"))
    
    #Subsetting the initial input tables according to the user-selected
    #time period.
    #var_weeksSel <- countrySelector()$var[,startWeek:endWeek]
    
    allLin_weeksSel <- countrySelector()$allLin[,startWeek:endWeek]
    
    spLin_weeksSel <- countrySelector()$spLin[,startWeek:endWeek]
    
    mut_weeksSel <- countrySelector()$mut[,startWeek:endWeek]
    
    return(list(#var = var_weeksSel,
                allLin = allLin_weeksSel,
                spLin = spLin_weeksSel,
                mut = mut_weeksSel))
  })
  
  #Calculating the total number of:
  #1. Sequences in the user-selected time lapse.
  #2. Per week sequences in the user-selected time lapse.
  #These values will be used to subset data by frequency (%) and
  #to normalize data.
  seqSum <- reactive({
    #Defining inputs.
    inTable <- weeksSelector()$allLin
    
    #Calculating the total number of sequences in the user-selected time
    #lapse of interest.
    totSeq <- sum(inTable)
    
    #Calculating the total number of sequences for each week in the
    #user-selected time lapse of interest
    totSeq_perWeek <- colSums(inTable)
    
    #The total number of genomes sequenced in the time period of interest MUST be >0
    #otherwise the app rises a warning.
    validate(need(totSeq>0,
                  "0 sequenced genomes in the selected weeks range"))
    
    return(list(totSeq = totSeq,
                totSeq_perWeek = totSeq_perWeek))
  })
  
  #######VARIANTS TAB#######
  #Generating the input table for the Viariants Tab.
  var_inTable <- reactive({
    
  })
  
  #Normalizing the input data for the variants barplot.
  var_Normalization <- reactive({
    
  })
  
  #Producing the barplot and the corresponding legend for the Variants
  #(as annotated by WHO) input table.
  output$variantsBP <- renderPlot({
    
  })
  
  #Generating the barplot displaying the number of genome sequences
  #available for each week of the selected time period.
  output$varTab_perWeekSeq <- renderPlot({
    
  })
  
  #Generating the drop down menu that allows to select a WHO Variant for
  #which produce a barplot displaying its composition (proportion of Sublineages).
  output$variant <- renderUI({
    
  })
  
  #Generating the barplot (and the corresponding legend) displaying the proportion
  #of Sublineages composing the user-selected Variant of interest.
  #CHIEDO SE DEVO NORMALIZZARE RISPETTO AL NUMERO GENOMI DI QUELLA VARIANTE
  output$lineagesBP_varOfInt <- renderPlot({
    
  })
  
  #######LINEAGES TAB#######
  #Subsetting the allLineages input table respect to the
  #minimum frequency (%) and the maximum number of displayed Lineages.
  allLin_Subsetting <- reactive({
    #Defining inputs.
    allLin_Subsetting_inTable <- weeksSelector()$allLin
    totSeq <- seqSum()$totSeq
    allLin_Subsetting_minFreq <- input$lineagesFreq
    allLin_Subsetting_maxViz <- 5
    
    #Subsetting data.
    allLin_Subsetted <- freqViz_Subsetter(allLin_Subsetting_inTable,
                                          totSeq,
                                          allLin_Subsetting_minFreq,
                                          allLin_Subsetting_maxViz)
    
    #The final input table MUST contain at least 1 Lineage.
    validate(need(nrow(allLin_Subsetted)>0,
                  paste("0 Lineages with a % of sequenced genomes higher than",
                        paste0(as.numeric(allLin_Subsetting_minFreq)*100,"%"),
                        "in the selected weeks range")))
    
    return(list(allLin = allLin_Subsetted))
  })
  
  #Normalizing the allLineages input table.
  allLin_Normalization <- reactive({
    #Defining inputs.
    allLin_Normalization_inTable <- allLin_Subsetting()$allLin
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    
    #Normalizing data.
    allLin_Normalized <- dataNormalizer(allLin_Normalization_inTable,
                                        totSeq_perWeek)
    
    return(list(allLin = allLin_Normalized))
  })
  
  #Producing the color palette for the plots in the Lineages Tab
  allLin_Palette <- reactive({
    #Defining inputs.
    allLin_Palette_inTable <- allLin_Subsetting()$allLin
    allLin_randomPal <- randomColors
    allLin_specialPal <- colVoc
    
    #Producing the color palette.
    allLin_Pal <- plotPalette(allLin_Palette_inTable,
                              allLin_randomPal,
                              allLin_specialPal)
    
    return(list(allLin = allLin_Pal))
  })
  
  #Generating a bar plot and the corresponding legend for the allLineages
  #input table.
  output$lineagesBP <- renderPlot({
    #Defining inputs.
    allLinBP_inTable <- as.matrix(allLin_Normalization()$allLin)
    allLinBP_Palette <- allLin_Palette()$allLin
    allLinBP_plotMain <- "Lineages"
    
    #Plotting
    dataPlotter_BP(allLinBP_inTable,
                   allLinBP_Palette,
                   allLinBP_plotMain)
  })
  
  #Generating the barplot displaying the number of genome sequences
  #available for each week of the selected time period.
  output$linTab_perWeekSeq <- renderPlot({
    #Defining inputs.
    allLin_perWeekSeq_inTable <- allLin_Normalization()$allLin
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    
    #Plotting.
    perWeekTotSeqPlotter_BP(allLin_perWeekSeq_inTable,
                            totSeq_perWeek)
  })
  
  #Generating a pie chart ad the corresponding legend for the allLineages
  #input table.
  output$lineagesPC <- renderPlot({
    #Defining inputs.
    allLinPC_inTable <- as.matrix(allLin_Subsetting()$allLin)
    allLinPC_Palette <- allLin_Palette()$allLin
    allLinPC_plotMain <- "Lineages"
    
    #Plotting.
    dataPlotter_PC(allLinPC_inTable,
                   allLinPC_Palette,
                   allLinPC_plotMain)
  })
  
  #######LINEAGES+ TAB#######
  #Subsetting the Lineages+ input table respect to the
  #minimum frequency (%) and  maximum number of displayed Lineages+.
  spLin_Subsetting <- reactive({
    #Defining inputs.
    spLin_Subsetting_inTable <- weeksSelector()$spLin
    totSeq <- seqSum()$totSeq
    spLin_Subsetting_minFreq <- input$spLineagesFreq
    spLin_Subsetting_maxViz <- 5
    
    #Subsetting data.
    spLin_Subsetted <- freqViz_Subsetter(spLin_Subsetting_inTable,
                                          totSeq,
                                          spLin_Subsetting_minFreq,
                                          spLin_Subsetting_maxViz)
    
    #The final input table MUST contain at least 1 Lineage+.
    validate(need(nrow(spLin_Subsetted)>0,
                  paste("0 Lineages+ with a % of sequenced genomes higher than",
                        paste0(as.numeric(spLin_Subsetting_minFreq)*100,"%"),
                        "in the selected weeks range")))
    
    return(list(spLin = spLin_Subsetted))
  })
  
  #Normalizing the Lineages+ input table.
  spLin_Normalization <- reactive({
    #Defining inputs.
    spLin_Normalization_inTable <- spLin_Subsetting()$spLin
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    
    #Normalizing data.
    spLin_Normalized <- dataNormalizer(spLin_Normalization_inTable,
                                        totSeq_perWeek)
    
    return(list(spLin = spLin_Normalized))
  })
  
  #Producing the color palette for the plots in the Lineages+ Tab.
  spLin_Palette <- reactive({
    #Defining inputs.
    spLin_Palette_inTable <- spLin_Subsetting()$spLin
    spLin_randomPal <- randomColors
    
    #Producing the color palette.
    spLin_Pal <- plotPalette(spLin_Palette_inTable,
                              spLin_randomPal)
    
    return(list(spLin = spLin_Pal))
  })
  
  #Generating a bar plot and the corresponding legend for the Lineages+
  #input table.
  output$spLineagesBP <- renderPlot({
    #Defining inputs.
    spLinBP_inTable <- as.matrix(spLin_Normalization()$spLin)
    spLinBP_Palette <- spLin_Palette()$spLin
    spLinBP_plotMain <- "Lineages+"
    
    #Plotting.
    dataPlotter_BP(spLinBP_inTable,
                   spLinBP_Palette,
                   spLinBP_plotMain)
  })
  
  #Generating the barplot displaying the number of genome sequences
  #available for each week of the selected time period.
  output$spLinTab_perWeekSeq <- renderPlot({
    #Defining inputs.
    spLin_perWeekSeq_inTable <- spLin_Normalization()$spLin
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    
    #Plotting.
    perWeekTotSeqPlotter_BP(spLin_perWeekSeq_inTable,
                            totSeq_perWeek)
  })
  
  #Generating a pie chart ad the corresponding legend for the Lineages+
  #input table.
  output$spLineagesPC <- renderPlot({
    #Defining inputs.
    spLinPC_inTable <- as.matrix(spLin_Subsetting()$spLin)
    spLinPC_Palette <- spLin_Palette()$spLin
    spLinPC_plotMain <- "Lineages+"
    
    #Plotting.
    dataPlotter_PC(spLinPC_inTable,
                   spLinPC_Palette,
                   spLinPC_plotMain)
  })
  
  #######MUTATIONS TAB#######
  #Subsetting the Mutations input table respect to the
  #maximum number of displayed Mutations.
  mut_Subsetting <- reactive({
    #Defining inputs.
    mut_Subsetting_inTable <- weeksSelector()$mut
    totSeq <- seqSum()$totSeq
    mut_Subsetting_maxViz <- 10
    
    #Subsetting data.
    mut_Subsetted <- dataViz_Subsetter(mut_Subsetting_inTable, 
                                       totSeq,
                                       mut_Subsetting_maxViz)
    
    #The final input table MUST contain at least 1 Mutation with at least
    #1 appearance in the user-selected time period of interest.
    validate(need(sum(mut_Subsetted)>0,
                  "0 Mutations of interest in the selected weeks range"))
    
    return(list(mut = mut_Subsetted))
  })
  
  #Normalizing the Mutations input table.
  mut_Normalization <- reactive({
    #Defining inputs.
    mut_Normalization_inTable <- mut_Subsetting()$mut
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    
    #Normalizing data.
    mut_Normalized <- dataNormalizer(mut_Normalization_inTable,
                                     totSeq_perWeek)
    
    return(list(mut = mut_Normalized))
  })
  
  #Producing the color palette for the plots in the Mutations Tab
  mut_Palette <- reactive({
    #Defining inputs.
    mut_Palette_inTable <- mut_Subsetting()$mut
    mut_randomPal <- mut_randomColors
    mut_specialPal <- colMoc
    
    #Producing the color palette.
    mut_Pal <- plotPalette(mut_Palette_inTable,
                           mut_randomPal,
                           mut_specialPal)
    
    return(list(mut = mut_Pal))
  })
  
  #Generating a bar plot and the corresponding legend for the Mutations
  #input table.
  output$mutationsBP <- renderPlot({
    #Defining inputs.
    mutBP_inTable <- as.matrix(mut_Normalization()$mut)
    mutBP_Palette <- mut_Palette()$mut
    mutBP_plotMain <- "Mutations"
    
    #Plotting
    dataPlotter_BP(mutBP_inTable,
                   mutBP_Palette,
                   mutBP_plotMain)
  })
  
  #Generating the barplot displaying the number of genome sequences
  #available for each week of the selected time period.
  output$mutTab_perWeekSeq <- renderPlot({
    #Defining inputs.
    mut_perWeekSeq_inTable <- mut_Normalization()$mut
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    
    #Plotting.
    perWeekTotSeqPlotter_BP(mut_perWeekSeq_inTable,
                            totSeq_perWeek)
  })
  
  #Generating a pie chart ad the corresponding legend for the Lineages+
  #input table.
  output$mutationsPC <- renderPlot({
    #Defining inputs.
    mutPC_inTable <- as.matrix(mut_Subsetting()$mut)
    mutPC_Palette <- mut_Palette()$mut
    mutPC_plotMain <- "Mutations"
    
    #Plotting.
    dataPlotter_PC(mutPC_inTable,
                   mutPC_Palette,
                   mutPC_plotMain)
  })
  
  #######SCATTERPLOTS TAB#######
  #Generating the drop down menu that allows to select a Lineage.
  output$lineage <- renderUI({
    #Defining inputs.
    linSel_inTable <- allLin_Subsetting()$allLin
    linSel_widgetName <- "lineageSel"
    linSel_widgetMain <- "Lineage"
    
    #Producing the drop down menu.
    dropdownM_widgetGenerator(linSel_inTable,
                              linSel_widgetName,
                              linSel_widgetMain,
                              input)
  })
  
  #Generating the drop down menu that allows to select a Mutation.
  output$mutation <- renderUI({
    #Defining inputs.
    mutSel_inTable <- mut_Subsetting()$mut
    mutSel_widgetName <- "mutationSel"
    mutSel_widgetMain <- "Mutation"
    
    #Producing the drop down menu.
    dropdownM_widgetGenerator(mutSel_inTable,
                              mutSel_widgetName,
                              mutSel_widgetMain,
                              input)
  })
  
  #Generating a scatter plot that allows to compare the number of occurrences
  #of a Lineage of interest vs all other Lineages (if present) and the
  #corresponding legend.
  output$lineagesSP <- renderPlot({
    #Defining inputs.
    allLinSP_inTable <- allLin_Subsetting()$allLin
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    allLinSP_selLin <- input$lineageSel
    allLinSP_Palette <- allLin_Palette()$allLin
    
    #Plotting.
    dataPlotter_SP(allLinSP_inTable,
                   totSeq_perWeek,
                   allLinSP_selLin,
                   allLinSP_Palette)
  })
  
  #Generating a scatter plot that allows to compare the number of occurrences
  #of a Mutation of interest vs the total number of sequenced genomes and the
  #corresponding legend.
  output$mutationsSP <- renderPlot({
    #Defining inputs.
    mutSP_inTable <- mut_Subsetting()$mut
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    mutSP_selMut <- input$mutationSel
    mutSP_Palette <- mut_Palette()$mut
    
    #Plotting.
    dataPlotter_SP(mutSP_inTable,
                   totSeq_perWeek,
                   mutSP_selMut,
                   mutSP_Palette)
  })
  
  #######LINEAGE+ VS LINEAGE TAB#######
  #Subsetting the input table for Lineages+ respect to minimum frequency (%).
  spLinVSLin_Subsetting <- reactive({
    #Defining inputs.
    spLinVSLin_Subsetting_inTable <- weeksSelector()$spLin
    totSeq <- seqSum()$totSeq
    spLinVSLin_Subsetting_minFreq <- input$spLineagesFreq
    
    #Subsetting data.
    spLinVSLin_Subsetted <- dataFreq_Subsetter(spLinVSLin_Subsetting_inTable,
                                               totSeq,
                                               spLinVSLin_Subsetting_minFreq)
    
    #The final input table MUST contain at least 1 Lineage+.
    validate(need(nrow(spLinVSLin_Subsetted)>0,
                  paste("0 Lineages+ with a % of sequenced genomes higher than",
                        paste0(as.numeric(spLinVSLin_Subsetting_minFreq)*100,"%"),
                        "in the selected weeks range")))
    
    return(list(spLinVSLin = spLinVSLin_Subsetted))
  })
  
  #Generating the drop down menu that allows to select a Lineage+ to be represented
  #in the Lineage+ VS Lineage barplot.
  output$specialLineage <- renderUI({
    #Defining inputs.
    spLinSel_inTable <- spLinVSLin_Subsetting()$spLinVSLin
    spLinSel_widgetName <- "spLineageSel"
    spLinSel_widgetMain <- "Lineage+"
    
    #Producing the drop down menu.
    dropdownM_widgetGenerator(spLinSel_inTable,
                              spLinSel_widgetName,
                              spLinSel_widgetMain,
                              input)
  })
  
  #Generating the input table for the Lineage+ VS Lineage barplot.
  spLinVSLin_inTableProduction <- reactive({
    #The final input table MUST contain at least 1 Lineage+.
    validate(need(nrow(spLinVSLin_Subsetting()$spLinVSLin)>0,
                  paste("0 Lineages+ with a % of sequenced genomes higher than",
                        paste0(as.numeric(input$spLineagesFreq)*100,"%"),
                        "in the selected weeks range")))
    
    #Defining inputs.
    spLinVSLin_inTableProduction_inTable <- weeksSelector()$allLin
    spLinVSLin_inTableProduction_selSpLin <- input$spLineageSel
    spLinVSLin_inTableProduction_convTable <- specialLin_to_Lineage
    
    #Producing the final table.
    spLinVSLin_inTable <- spLinVSLin_inTableProducer(spLinVSLin_inTableProduction_inTable,
                                                                          spLinVSLin_inTableProduction_selSpLin,
                                                                          spLinVSLin_inTableProduction_convTable)
    
    return(list(spLinVSLin = spLinVSLin_inTable))
  })
  
  #Normalizing the Lineage+ VS Lineage input table.
  spLinVSLin_Normalization <- reactive({
    #Defining inputs.
    spLinVSLin_Normalization_inTable <- spLinVSLin_inTableProduction()$spLinVSLin
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    
    #Normalizing data.
    spLinVSLin_Normalized <- dataNormalizer(spLinVSLin_Normalization_inTable,
                                            totSeq_perWeek)
    
    return(list(spLinVSLin = spLinVSLin_Normalized))
  })
  
  #Producing the color palette for the Lineage+ VS Lineage barplot.
  spLinVSLin_Palette <- reactive({
    #Defining inputs.
    spLinVSLin_Palette_inTable <- spLinVSLin_Normalization()$spLinVSLin
    spLinVSLin_randomPal <- randomColors
    spLinVSLin_specialPal <- colVoc
    
    #Producing the color palette.
    spLinVSLin_Pal <- plotPalette(spLinVSLin_Palette_inTable,
                                  spLinVSLin_randomPal,
                                  spLinVSLin_specialPal)
    
    return(list(spLinVSLin = spLinVSLin_Pal))
  })
  
  #Generating the barplot and the corresponding legend for the Lineage+ VS Lineage
  #analysis.
  output$spLvsLBP <- renderPlot({
    #Defining inputs.
    spLinVSLinBP_inTable <- as.matrix(spLinVSLin_Normalization()$spLinVSLin)
    spLinVSLinBP_Palette <- spLinVSLin_Palette()$spLinVSLin
    spLin_Sel <- input$spLineageSel
    parentLin_Sel <- specialLin_to_Lineage[spLin_Sel, 1]
    spLinVSLinBP_plotMain <- paste(spLin_Sel, "VS", parentLin_Sel)
    
    #Plotting.
    dataPlotter_BP(spLinVSLinBP_inTable,
                   spLinVSLinBP_Palette,
                   spLinVSLinBP_plotMain)
  })
  
  #Generating the barplot displaying the number of genome sequences available for
  #each week of the selected time period.
  output$spLvsL_perWeekSeq <- renderPlot({
    #Defining inputs.
    spLinVSLin_perWeekSeq_inTable <- spLinVSLin_Normalization()$spLinVSLin
    totSeq_perWeek <- seqSum()$totSeq_perWeek
    
    #Plotting.
    perWeekTotSeqPlotter_BP(spLinVSLin_perWeekSeq_inTable,
                            totSeq_perWeek)
  })
  
  #######MAPS TAB#######
  #Subsetting the input table for Lineages respect to minimum frequency (%).
  map_dataSubsetting <- reactive({
    #Defining inputs.
    map_dataSubsetting_inTable <- weeksSelector()$allLin
    totSeq <- seqSum()$totSeq
    map_dataSubsetting_minFreq <- input$lineagesFreq
    
    #Subsetting data.
    map_dataSubsetted <- dataFreq_Subsetter(map_dataSubsetting_inTable,
                                            totSeq,
                                            map_dataSubsetting_minFreq)
    
    #The final input table MUST contain at least 1 Lineage.
    validate(need(nrow(map_dataSubsetted)>0,
                  paste("0 Lineages with a % of sequenced genomes higher than",
                        paste0(as.numeric(map_dataSubsetting_minFreq)*100,"%"),
                        "in the selected weeks range")))
    
    return(list(mapData = map_dataSubsetted))
  })
  
  #Generating the drop down menu that allows to select a Lineage to represent inthe fist
  #the first choropleth map.
  output$lineageMap1 <- renderUI({
    #Defining inputs.
    linSel_Map1_inTable <- map_dataSubsetting()$mapData
    linSel_Map1_widgetName <- "lineageSel_Map1"
    linSel_Map1_widgetMain <- "Lineage (Map 1)"
    
    #Producing the drop down menu.
    dropdownM_widgetGenerator(linSel_Map1_inTable,
                              linSel_Map1_widgetName,
                              linSel_Map1_widgetMain,
                              input)
  })
  
  #Generating the drop down menu that allows to select a Lineage to represent inthe fist
  #the second choropleth map.
  output$lineageMap2 <- renderUI({
    #Defining inputs.
    linSel_Map2_inTable <- map_dataSubsetting()$mapData
    linSel_Map2_widgetName <- "lineageSel_Map2"
    linSel_Map2_widgetMain <- "Lineage (Map 2)"
    
    #Producing the drop down menu.
    dropdownM_widgetGenerator(linSel_Map2_inTable,
                              linSel_Map2_widgetName,
                              linSel_Map2_widgetMain,
                              input)
  })
  
  #Reading the input file containing the geometric data that allow to draw the map of the
  #user-selected country.
  map_geomDataSelector <- reactive({
    #Defining inputs.
    map_geomDataSelector_CountryISO <- countryConvertion_Table[input$country,2]
    
    #Opening the geometric data file that allows to draw the map of the
    #user-selected country.
    map_geomData <- gb_adm1(map_geomDataSelector_CountryISO)
    
    return(list(mapData = map_geomData))
  })
  
  #Reading the input tables for the selected Lineage and country.
  map_dataSelector <- reactive({
    #Defining inputs.
    map_dataSelector_CountrySel <- countryConvertion_Table[input$country,1]
    map1_dataSelector_LineageSel <- input$lineageSel_Map1
    map2_dataSelector_LineageSel <- input$lineageSel_Map2
    
    fileNameSep1 <- "_"
    fileNameSep2 <- "_regions"
    exten <- ".csv"
    separ <- "\t"
    
    #Reading the input data table.
    map1_Data <- read.table(paste0(map_path, map1_dataSelector_LineageSel, fileNameSep1, map_dataSelector_CountrySel, fileNameSep2, exten),
                            sep = separ,
                            row.names = 1,
                            header = T,
                            check.names = F,
                            quote = "")
    
    map2_Data <- read.table(paste0(map_path, map2_dataSelector_LineageSel, fileNameSep1, map_dataSelector_CountrySel, fileNameSep2, exten),
                            sep = separ,
                            row.names = 1,
                            header = T,
                            check.names = F,
                            quote = "")
    
    return(list(map1 = map1_Data,
                map2 = map2_Data))
  })
  
  #Ordering the subsetted data according to the order of the regions in the geometric data
  #table.
  map_dataOrdering <-reactive({
    #Defining inputs.
    map_dataOrdering_orderingPrinciple <- map_geomDataSelector()$mapData$shapeISO
    
    map1_dataOrdering_inTable <- map_dataSelector()$map1
    map1_dataOrdering_orderingCol <- map1_dataOrdering_inTable$ISO
    map1_dataOrder <- match(map_dataOrdering_orderingPrinciple, map1_dataOrdering_orderingCol)
    
    map2_dataOrdering_inTable <- map_dataSelector()$map2
    map2_dataOrdering_orderingCol <- map2_dataOrdering_inTable$ISO
    map2_dataOrder <- match(map_dataOrdering_orderingPrinciple, map2_dataOrdering_orderingCol)
    
    #Ordering the data.
    map1_dataOrdered <- map1_dataOrdering_inTable[map1_dataOrder,]
    map1_dataOrdered$ISO <- NULL
    
    map2_dataOrdered <- map2_dataOrdering_inTable[map2_dataOrder,]
    map2_dataOrdered$ISO <- NULL
    
    return(list(map1 = map1_dataOrdered,
                map2 = map2_dataOrdered))
  })
  
  #Subsetting the input tables respect to the selected time lapse.
  map_weeksSelector <- reactive({
    #Defining inputs.
    map1_weeksSelector_inTable <- map_dataOrdering()$map1
    map2_weeksSelector_inTable <- map_dataOrdering()$map2
    
    startWeek <- input$weeksRange[1]
    endWeek <- input$weeksRange[2]
    
    #The selected time lapse MUST be >= 1 week otherwise the app rises
    #a warning.
    validate(need((endWeek-startWeek)>=1,
                  "Select a Weeks range"))
    
    #Subsetting the initial input tables according to the user-selected
    #time period.
    map1_weeksSel <- map1_weeksSelector_inTable[,startWeek:endWeek]
    map2_weeksSel <- map2_weeksSelector_inTable[,startWeek:endWeek]
    
    return(list(map1 = map1_weeksSel,
                map2 = map2_weeksSel))
  })
  
  #Calculating the average frequency (%) of the user-selected Lineage at a regional level
  #in the Country of interest.
  map_averageFreq <- reactive({
    #Defining inputs.
    map1_averageFreq_inTable <- map_weeksSelector()$map1
    map2_averageFreq_inTable <- map_weeksSelector()$map2
    
    #Calculating the average frequency (%).
    map1_averageFreq <- apply(map1_averageFreq_inTable, 1, mean)
    map1_averageFreq <- map1_averageFreq*100
    map1_averageFreq <- signif(map1_averageFreq, digits = 3)
    
    map2_averageFreq <- apply(map2_averageFreq_inTable, 1, mean)
    map2_averageFreq <- map2_averageFreq*100
    map2_averageFreq <- signif(map2_averageFreq, digits = 3)
    
    return(list(map1 = map1_averageFreq,
                map2 = map2_averageFreq))
  })
  
  #Defining the color palette for each of the choropleth map.
  map_Palette <- reactive({
    #Defining inputs.
    map1_Palette_inTable <- map_averageFreq()$map1
    map2_Palette_inTable <- map_averageFreq()$map2
    refPalette <- map_referencePalette
    
    #Producing the color palette for each map.
    map1_Pal <- sapply(map1_Palette_inTable, mapPalette, refPalette)
    map2_Pal <- sapply(map2_Palette_inTable, mapPalette, refPalette)
    
    return(list(map1 = map1_Pal,
                map2 = map2_Pal))
  })
  
  #Generating the first choropleth map and the corresponding legend.
  output$lineagesMap_1 <- renderLeaflet({
    #Defining inputs.
    linMap1_inTable <- map_averageFreq()$map1
    linMap1_linSel <- input$lineageSel_Map1
    mapData <- map_geomDataSelector()$mapData
    linMap1_Palette <- map_Palette()$map1
    refPalette <- map_referencePalette
    
    #Generating the first choropleth map.
    dataPlotter_ChoroMap(linMap1_inTable,
                         linMap1_linSel,
                         mapData,
                         linMap1_Palette,
                         refPalette)
  })
  
  #Generating the second choropleth map and the corresponding legend.
  output$lineagesMap_2 <- renderLeaflet({
    #Defining inputs.
    linMap2_inTable <- map_averageFreq()$map2
    linMap2_linSel <- input$lineageSel_Map2
    mapData <- map_geomDataSelector()$mapData
    linMap2_Palette <- map_Palette()$map2
    refPalette <- map_referencePalette
    
    #Generating the second choropleth map.
    dataPlotter_ChoroMap(linMap2_inTable,
                         linMap2_linSel,
                         mapData,
                         linMap2_Palette,
                         refPalette)
  })

}