#Define UI -----
ui <- fluidPage(
  titlePanel("CorGAT-tracker"),
  
  #######OUTPUT REGION LAYOUT#######
  #Define the layout of the output region.
  #Each plot is generated in a different panel.
  tabsetPanel(
    #######README TAB#######
    tabPanel("README"), #RISCRIVERE IL MANUALE DELL'APP E RICOPIARE
    
    #######VARIANTS TAB#######
    #The Variants tab contains three different plots:
    #1. A barplot representing the frequency (%) of WHO Variants for each week of interest. (CHIEDO SE VOGLIO NOMALIZZARE QUESTI DATI)
    #2. A barplot representing the number of sequenced SARS-CoV-2 genomes available for
    #   each week of the time period of interest.
    #3. A barplot representing the composition (proportion of Lineages) of a WHO Variant
    #   of interest.
    tabPanel("Variants",
             plotOutput("variantsBP"),
             plotOutput("varTab_perWeekSeq"),
             plotOutput("lineagesBP_varOfInt")),
    
    #######LINEAGES TAB#######
    #The Lineages tab contains three different plots:
    #1. A barplot representing the frequency (%) of Lineages for each week of interest.
    #2. A barplot representing the number of sequenced SARS-CoV-2 genomes available for
    #   each week of the time period of interest.
    #3. A Pie Chart representing the proportion of sequenced genomes for Lineages in the
    #   time lapse of interest.
    #These plots are produced using the allLineage input table.
    tabPanel("Lineages",
             plotOutput("lineagesBP"),
             plotOutput("linTab_perWeekSeq"),
             plotOutput("lineagesPC")),
    
    #######LINEAGES+ TAB#######
    #The Lineages+ tab contains three different plots:
    #1. A barplot representing the frequency (%) of Lineages+ for each week of interest.
    #2. A barplot representing the number of sequenced SARS-CoV-2 genomes available for
    #   each week of the time period of interest.
    #3. A Pie Chart representing the proportion of sequenced genomes for Lineages+ in the
    #   time lapse of interest.
    #These plots are produced using the specialLineage input table.
    tabPanel("Lineages+",
             plotOutput("spLineagesBP"),
             plotOutput("spLinTab_perWeekSeq"),
             plotOutput("spLineagesPC")),
    
    #######MUTATIONS TAB#######
    #The Mutations tab contains three different plots:
    #1. A barplot representing the frequency (%) of Mutations for each week of interest.
    #2. A barplot representing the number of sequenced SARS-CoV-2 genomes available for
    #   each week of the time period of interest.
    #3. A Pie Chart representing the proportion of some Mutations in the time lapse
    #   of interest.
    #These plots are produced using the Mutations input table.
    tabPanel("Mutations",
             plotOutput("mutationsBP"),
             plotOutput("mutTab_perWeekSeq"),
             plotOutput("mutationsPC")),
    
    #######SCATTERPLOTS TAB#######
    #The Scatterplots tab contains two different plots:
    #1. A scatterplot for a user-selected Lineage from the allLineages input table.
    #2. A scatterplot for a user-selected Mutation from the Mutations input table.
    tabPanel("Scatterplots",
             plotOutput("lineagesSP"),
             plotOutput("mutationsSP")),
    
    #######LINEAGE+ VS LINEAGE TAB#######
    #The Lineage+ VS Lineage tab contains two different plots:
    #1. A barpot that allows the comparison between the frequency (%) of a Lineage+ 
    #   of interest and the "parental" Lineage it derives from.
    #2. A barplot that displays the total number of sequenced SARS-CoV-2 genomes
    #   available for each week of the time period of interest.
    tabPanel("Lineage+ VS Lineage",
             plotOutput("spLvsLBP"),
             plotOutput("spLvsL_perWeekSeq")),
    
    #######MAPS TAB#######
    #The Maps tab contains two differetn plots:
    #1. A choropleth map that represents the frequency (%) of a Lineage of choice
    #   divided by region for a user-selected Country in the time period of interest.
    #2. A chorophlet map that represents the frequency (%) of a second Lineage of
    #   choice divided by region for a user-selected Country in the time period of
    #   interest.
    tabPanel("Maps",
             fluidRow(
               column(6,
                      leafletOutput("lineagesMap_1")),
               column(6,
                      leafletOutput("lineagesMap_2"))))
  ),
  
  hr(),
  
  #######INPUT REGION LAYOUT#######
  #Define the layout of the input region.
  #Each interactive widget occupies a region of width=3 columns and is
  #accompanied by an help text briefly explaining its function.
  fluidRow(
    #######FIRST SET OF WIDGETS#######
    column(3,
           offset = 1,
           #######COUNTRY SELECTOR#######
           #Generates a drop down menu that allows to select a country to
           #analyze.
           #Default is Italy.
           selectInput("country",
                       "Country",
                       choices = countryList,
                       selected = "Italy"),
           helpText("Visualize data for the selected country"),
           
           br(),
           
           #######WEEKS RANGE SELECTOR#######
           #Generates a slider that allows to select a time lapse to analyze.
           #Time is calculated in weeks from a fixed date (2019-12-30).
           #Default is the whole period of time (from week 1 to maxWeek).
           sliderInput("weeksRange",
                       "Weeks range",
                       min = 1,
                       max = maxWeek,
                       value = c(1, maxWeek)),
           helpText("Time lapse of interest (number of weeks from a fixed date)"),
           
           br(),
           
           #######VARIANT SELECTOR (SUBLINEAGES BARPLOT)#######
           #Generates a drop down menu that allows to select a WHO Variant
           #for which analyze the composition (proportion of Sublineages).
           #Variants in the menu are selected among those that present sequenced
           #genomes in the user-select time period. The widget changes dynamically.
           #No default set.
           uiOutput("variant"),
           helpText("Produce a barplot of composition (proportion of lineages) for selected Variant"),
           
           #######LINEAGES MIN FREQUENCY (%) SELECTOR#######
           #Generates a radio buttons selection that allows to select the
           #minimum frequency (%) required to each Lineage to be represented
           #in the graphics.
           #Default is 1%.
           radioButtons("lineagesFreq",
                        "Min % of genomes (Lineages)",
                        choices = list("1%"=0.01,
                                       "25%"=0.25,
                                       "50%"=0.5,
                                       "75%"=0.75,
                                       "90%"=0.9),
                        selected = 0.01),
           helpText("Minimum frequency (%) required to display a Lineage")),
    
    #######SECOND SET OF WIDGETS#######
    column(3,
           offset = 1,
           #######LINEAGES+ MIN FREQUENCY (%) SELECTOR#######
           #Generates a radio buttons selection that allows to select the
           #minimum frequency (%) required to each Lineage+ to be represented
           #in the graphics.
           #Default is 1%.
           radioButtons("spLineagesFreq",
                        "Min % of genomes (Lineages+)",
                        choices = list("1%"=0.01,
                                       "2,5%"=0.025,
                                       "5%"=0.05,
                                       "10%"=0.1,
                                       "25%"=0.25),
                        selected = 0.01),
           helpText("Minimum frequency (%) required to display a Lineage+"),
           
           br(),
           
           #######LINEAGE SELECTOR (SCATTERPLOT)#######
           #Generates a drop down menu that allows to select a Lineage
           #for which produce a scatter plot.
           #Lineages in the menu are selected among those surviving
           #previous filters. The widget changes dynamically depending
           #on the selected time lapse and frequency (%) threshold.
           #No default set.
           uiOutput("lineage"),
           helpText("Produce a scatterplot for the selected Lineage"),
           
           br(),
           
           #######MUTATION SELECTOR (SCATTERPLOT)#######
           #Generates a drop down menu that allows to select a Mutation
           #for which produce a scatter plot.
           #Mutations in the menu are selected among those surviving
           #previous filters. The widget changes dynamically depending
           #on the selected time lapse.
           #No default set.
           uiOutput("mutation"),
           helpText("Produce a scatterplot for the selected Mutation")),
    
    #######THIRD SET OF WIDGETS#######
    column(3,
           #######LINEAGE+ SELECTOR (LINEAGE+ VS LINEAGE BARPLOT)####### 
           #Generates a drop down menu that allows to select the Lineage+ (and,
           #consequentially, its "parent" Lineage) for which produce the
           #corresponding Lineage+ VS Lineage barplot.
           #Lineages+ in the menu are selected among those surviving previous
           #filters. The widget changes dynamically depending on the selected
           #time lapse and the frequence (%) threshold.
           #No default set.
           uiOutput("specialLineage"),
           helpText("Produce a Lineage+ VS Lineage barplot for the selected Lineage+"),
           
           br(),
           
           #######LINEAGE SELECTOR (MAP 1)#######
           #Generates the drop down menu that allows to select the Lineage to be
           #represented in the first chorophlet map.
           #Lineages in the menu are selected among those surviving
           #previous filters. The widget changes dynamically depending
           #on the selected time lapse and frequency (%) threshold.
           #No default set.
           uiOutput("lineageMap1"),
           helpText("Produce a chorophlet Map for the selected Lineage"),
           
           br(),
           
           #######LINEAGE SELECTOR (MAP 2)#######
           #Generates the drop down menu that allows to select the Lineage to be
           #represented in the second chorophlet map.
           #Lineages in the menu are selected among those surviving
           #previous filters. The widget changes dynamically depending
           #on the selected time lapse and frequency (%) threshold.
           #No default set.
           uiOutput("lineageMap2"),
           helpText("Produce a chorophlet Map for the selected Lineage"))
  )
)