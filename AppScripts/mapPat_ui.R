#################################################################
#      DEFINING ALL THE ELEMENTS OF THE APP USER INTERFACE      #
#################################################################

#Define UI -----
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(".shiny-output-error-validation {
           color: blue;
           font-size: 75%;
           }"))),
  
  titlePanel("CorGAT-tracker"),
  
  #######OUTPUT REGION LAYOUT#######
  #Defining the layout of the output region.
  #Each group of plots is generated in a different panel.
  tabsetPanel(id = "allTabs",
    #######README TAB#######
    #The ReadMe Tab contains a series of guidelines on the use of the app.
    #RISCRIVERE IL MANUALE DELL'APP E RICOPIARE
    tabPanel("README"),
    
    #######GENERAL PRESENTATION OF THE TABS#######
    #Each Tab of the app consists of a similar series of plots that allow to efficiently
    #and easily describe the behavior of some objects/characteristics of interest
    #over a certain period of time.
    #Each Tab present plots that are common among all the Tabs and others that are specific
    #to it in order to grant more insight to some specific characteristics of the represented
    #object.
    #######VARIANTS TAB#######
    #Plots are produced using the input tables of Variants counts.
    #The complete list of plots in the Variants Tab consist of:
    #1. A Stacked Area Chart (SAC) representing the weekly changes in frequency (%) of Variants
    #   belonging to a user-selected category over a user-selected period of time.
    #2. A BarPlot (BP) representing the number of sequenced SARS-CoV-2 genomes available for each
    #   week of the time period of interest.
    #3. A BarPlot (BP) representing the composition (proportion of Lineages) of a user-selected
    #   Variant over a user-selected period of time.
    #4. A HeatMap (HM) representing the regional frequency (%) of a collection of Variants for
    #   a user-selected country in the time period of interest.
    #5. Two Choropleth Maps (CM) that represent the regional frequency (%) of two Variants of
    #   choice for a user-selected country in the time period of interest.
    tabPanel("Variants",
             plotOutput("variantsSAC",
                        height = "500px"),
             plotOutput("varTab_perWeekSeq",
                        height = "500px"),
             plotOutput("variantsBP_LinVar",
                        height = "500px"),
             div(style='height:500px;overflow-y: scroll;',
                 uiOutput("varHM_plotUI")),
             fluidRow(
               column(6,
                      leafletOutput("variantsCM_1",
                                    height = "500px")),
               column(6,
                      leafletOutput("variantsCM_2",
                                    height = "500px")))),
    
    #######LINEAGES TAB#######
    #Plots are produced using the input tables of Lineages counts.
    #The complete list of plots in the Lineages Tab consist of:
    #1. A Stacked Area Chart (SAC) representing the weekly changes in frequency (%) of a user-selected
    #   number of Lineages over a user-selected period of time.
    #2. A BarPlot (BP) representing the number of sequenced SARS-CoV-2 genomes available for each
    #   week of the time period of interest.
    #3. A ScatterPlot (SP) representing the weekly changes in the total number of sequenced genomes
    #   of a user-selected number Lineages over the selected time period.
    #4. A HeatMap (HM) representing the regional frequency (%) of a collection of Lineages for
    #   a user-selected country in the time period of interest.
    #5. Two Choropleth Maps (CM) that represent the regional frequency (%) of two Lineages of
    #   choice for a user-selected country in the time period of interest.
    tabPanel("Lineages",
             plotOutput("lineagesSAC",
                        height = "500px"),
             plotOutput("linTab_perWeekSeq",
                        height = "500px"),
#             plotOutput("lineagesSP",
#                        height = "500px"),
             div(style='height:500px;overflow-y: scroll;',
                 uiOutput("allLinHM_plotUI")),
             fluidRow(
               column(6,
                      leafletOutput("lineagesCM_1",
                                    height = "500px")),
               column(6,
                      leafletOutput("lineagesCM_2",
                                    height = "500px")))),
    
    #######MUTATIONS TAB#######
    #These plots are produced using the input tables of Mutations counts.
    #The complete list of plots in the Mutations Tab consist of:
    #1. Two BarPlots (BP) representing the changes in weekly frequency (%) of the genomes of a user-selected 
    #   Lineage presenting two different non-defining Mutations compared to the frequency of genomes of the same
    #   Lineage without the Mutations. The comparison is performed over a user-selected period of time.
    #2. A BarPlot (BP) representing the number of sequenced SARS-CoV-2 genomes available for the user-selected
    #   Lineage for each week of the time period of interest.
    #3. A HeatMap (HM) representing the regional frequency (%) of a collection of Mutations for
    #   the user-selected Lineage and country in the time period of interest.
    #4. Two Choropleth Maps (CM) that represent the regional frequency (%) of two non-defining Mutations of
    #   choice for the user-selected Lineage and country in the time period of interest.
    tabPanel("Mutations",
             plotOutput("mutationsBP_1",
                        height = "500px"),
             plotOutput("mutationsBP_2",
                        height = "500px"),
             plotOutput("mutTab_perWeekSeq",
                        height = "500px"),
             div(style='height:500px;overflow-y: scroll;',
                 uiOutput("mutHM_plotUI")),
             fluidRow(
               column(6,
                      leafletOutput("mutationsCM_1",
                                    height = "500px")),
               column(6,
                      leafletOutput("mutationsCM_2",
                                    height = "500px"))))
  ),
  
  hr(style = "border-top: 1px solid #A9A9A9;"),
  
  #######INPUT REGION LAYOUT#######
  #Define the layout of the input region.
  #Each interactive widget occupies a region of width 3 columns and is
  #accompanied by an help text briefly explaining its function.
  fluidRow(
    #######FIRST SET OF WIDGETS#######
    column(3,
           h4("General"),
           #######PATHOGEN SELECTOR#######
           #Generates a drop down menu that allows to select a pathogen to
           #analyze.
           #Default is "SARS-CoV-2".
           selectInput("pathogen",
                       "Pathogen",
                       choices = pathogenList,
                       selected = "SARS-CoV-2"),
           helpText("Visualize data for the selected pathogen"),
           
           br(),
           
           #######COUNTRY SELECTOR#######
           #Generates a drop down menu that allows to select a country to
           #analyze.
           #Countries in the menu are selected among those possessing data
           #for the pathogen of interest. The widget changes
           #dynamically depending on the user-selected pathogen.
           #Default is "Italy".
           uiOutput("countryAll"),
           helpText("Visualize data for the selected country"),
           
           br(),
           
           #######TIME RANGE SELECTOR#######
           #Generates a slider that allows to select a time lapse to analyze.
           #Time is calculated in weeks/months from a fixed date.
           #maxTime is defined as the number of columns available in the
           #input tables for the pathogen of interest. The widget changes dynamically.
           #Default is the last 20 weeks/months (from maxTime-20 to maxTime).
           uiOutput("timeRangeAll"),
           helpText("Time lapse of interest (in weeks/months from a fixed date)")),
           
    #######SECOND SET OF WIDGETS#######
    #Widgets dedicated to Variants are shown/hidden depending on the user-selected
    #pathogen of interest.
    column(3,
           conditionalPanel(
             condition = 'input.pathogen == "SARS-CoV-2"',
             h4("Variants"),
             #######CATEGORY SELECTOR#######
             #Generates a drop down menu that allows to select the category
             #of Variants to be graphically represented.
             #Default is "All".
             uiOutput("varCategory"),
             helpText("Visualize data for the selected category of Variants"),
             
             br(),
             
             #######VARIANT SELECTOR (VARIANT COMPOSITION BARPLOT)#######
             #Generates a drop down menu that allows to select a Variant
             #for which analyze the composition (proportion of Lineages).
             #Variants in the menu are selected among those that present sequenced
             #genomes in the user-select time period. The widget changes dynamically.
             #No default set.
             uiOutput("variant"),
             helpText("Analyze the composition (proportion of Lineages) for the selected Variant"),
           
             br(),
           
             #######VARIANT LINEAGES MIN FREQUENCY (%) SELECTOR#######
             #Generates a radio buttons selection that allows to select the
             #minimum frequency (%) required to each Lineage of a user selected
             #Variant to be represented in the plots
             #Default is 1%.
             uiOutput("varFrequency"),
             helpText("Minimum frequency (%) required to display a Lineage belonging to the selected Variant"),
             
             #######VARIANT SELECTOR (MAP1)#######
             #Generates the drop down menu that allows to select the Variant to be
             #represented in the first Choropleth Map (CM1).
             #Variants in the menu are selected among those surviving
             #previous filters. The widget changes dynamically depending
             #on the selected time lapse and category.
             #No default set.
             uiOutput("variantCM1"),
             helpText("Produce a Choropleth Map for the selected Variant"),
             
             br(),
             
             #######VARIANT SELECTOR (MAP2)#######
             #Generates the drop down menu that allows to select the Variant to be
             #represented in the second Choropleth Map (CM2).
             #Variants in the menu are selected among those surviving
             #previous filters. The widget changes dynamically depending
             #on the selected time lapse and category.
             #No default set.
             uiOutput("variantCM2"),
             helpText("Produce a Choropleth Map for the selected Variant"))),
    
    #######THIRD SET OF WIDGETS#######
    column(3,
           h4("Lineages"),
           #######LINEAGES MIN FREQUENCY (%) SELECTOR#######
           #Generates a radio buttons selection that allows to select the
           #minimum frequency (%) required to each Lineage to be represented
           #in the plots
           #Default is 1%.
           uiOutput("linFrequency"),
           helpText("Minimum frequency (%) required to display a Lineage"),
           
           br(),
           
           #######N. LINEAGES SELECTOR#######
           #Generates a drop down menu that allows to select a number n of
           #Lineages to be graphically represented.
           #Default is 5 Lineages.
           uiOutput("linNumber"),
           helpText("Maximum number of explicitly displayed Lineages"),
           
           #######LINEAGE SELECTOR (MAP 1)#######
           #Generates the drop down menu that allows to select the Lineage to be
           #represented in the first Choropleth Map (CM1).
           #Lineages in the menu are selected among those surviving
           #previous filters. The widget changes dynamically depending
           #on the selected time lapse and frequency (%) threshold.
           #No default set.
           uiOutput("lineageCM1"),
           helpText("Produce a Choropleth Map for the selected Lineage"),
           
           br(),
           
           #######LINEAGE SELECTOR (MAP 2)#######
           #Generates the drop down menu that allows to select the Lineage to be
           #represented in the second Choropleth Map (CM2).
           #Lineages in the menu are selected among those surviving
           #previous filters. The widget changes dynamically depending
           #on the selected time lapse and frequency (%) threshold.
           #No default set.
           uiOutput("lineageCM2"),
           helpText("Produce a Chorophlet Map for the selected Lineage")),
    
    #######FOURTH SET OF WIDGETS#######
    column(3,
           h4("Mutations"),
           #######LINEAGE SELECTOR (MUTATIONS TAB)#######
           #Generates the drop down menu that allows to select the Lineage which
           #non-defining mutations are analyzed in the Mutations Tab.
           #Lineages in the menu are selected among those surviving
           #previous filters in the Lineages Tab.
           #The widget changes dynamically depending on the selected time lapse
           #and frequency (%) threshold.
           #No default set.
           uiOutput("mutationsLineage"),
           helpText("Visualize Mutations data for the selected Lineage"),
           
           br(),
           
           #######MUTATION SELECTOR (MAP 1)#######
           #Generates the drop down menu that allows to select the Mutation to be
           #represented in the first Choropleth Map (CM1).
           #Mutations in the menu are selected among those surviving
           #previous filters. The widget changes dynamically depending
           #on the selected time lapse and Lineage and includes only Mutations with
           #a cumulative frequency (%) equal or higher to 1% in the time period of interest.
           #No default set.
           uiOutput("mutationCM1"),
           helpText("Produce a Chorophlet Map for the selected Mutation"),
           
           br(),
           
           #######MUTATION SELECTOR (MAP 2)#######
           #Generates the drop down menu that allows to select the Mutation to be
           #represented in the second Choropleth Map (CM2).
           #Mutations in the menu are selected among those surviving
           #previous filters. The widget changes dynamically depending
           #on the selected time lapse and Lineage and includes only Mutations with
           #a cumulative frequency (%) equal or higher to 1% in the time period of interest.
           #No default set.
           uiOutput("mutationCM2"),
           helpText("Produce a Chorophlet Map for the selected Mutation"))
  )
)
