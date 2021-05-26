## CorGAT-tracker

Welcome to **CorGAT-tracker** a [Shiny](https://shiny.rstudio.com/) based app that allows the visualization of the prevalence of SARS-CoV-2 lineages and mutations of concern (MOC) as annotated by [CorGAT](https://doi.org/10.1093/bioinformatics/btaa1047), in an interactive form.  
Several parameters can be adjusted by the user, including for example the minimum number of genomes required for a lineage to be included in the visualization (see below) and/or the time interval to consider (in weeks, see below).  
The tool produces 3 different types of plot, each contained in a different tab/panel:  
* ***Barplot panel***  
A barplot representing the total number of genomes sequenced on a weekly basis. Data are stratified by lineage, however only the top 5 most abundant lineages are represented. Other lineages are collapsed under *Others*.  
* ***Pie chart panel***  
A pie chart showing the cumulative (meaning not divided by week) prevalence of SARS-CoV-2 lineages in the interval of time selected by the user. Similar to the barplot only the top 5 most-abundant lineages are represented, while remaining lineages are collapsed under *Others*.  
* ***Scatterplot panel***  
A scatterplot that represents the number of genomic sequences associated with a user-selected lineage, collected at every time point (week, x axis). To facilitate the comparison, the total number of genomes not associated with the lineage is also reported.  Only lineages displayed in the barplot and/or pie chart panel can be selected, meaning that, for every time interval selected by the user, only the top 5 most prevalent individual lineages and the *Others* collapsed values can be represented.

### Control and customization of the plots

Users can interact with **CorGAT-tracker** by the means of the set of widgets displayed under the main plot area, including:  
* The ***Country*** drop down menu, which allows the user to select the country of origin of the data to be visualized. Default is Italy.  
* The ***Weeks range*** slider, which allows the selection of the interval of  time to be displayed. Intervals of time are computed in the form of non-overlapped windows of 7 days (or weeks if you prefer) starting from 2019-12-30 the reported date of isolation of the first SARS-CoV-2 genomic sequence. This widget defaults to the entire frame of time included in the latest version of the analysis (week 1 to current week).
* The ***Min number of genomes*** radio button, which enables the user to select a lower bound for the numerosity of sequenced genomes required to a lineage to be represented in the plots. Only lineages surpassing this minimum threshold will be represented. Please be aware, irrespective of the selection, **CorGAT-tracker** is configured to allow the visualization of a maximum of 5 lineages at every time-point. Only the 5 most numerous lineages will be shown by default, remaining lineages are collapsed under *Others*. Default threshold for this widget is 100.
* The ***Lineage*** drop down menu allows the user to select a lineage of interest to be used  in the scatter-plot. This widget is generated dynamically based on the current selection of the user. Please be aware that the lineages that are available for this type of plot might change and might not be consistent across different countries or between different intervals of time. No default value.

#### Please be aware that to present the user with a more meaningful and compact representation, CorGAT-tracker was designed explicitly to show only the five most numerous lineages for any selection performed by the user. The category *Others* is used to collapse all the lineages that do not pass the threshold on for being included in the visualization (see above, ***Min number of genomes***) and/or those lineages that do not rank among the 5 most abundant. If you are dissatisfied with this behaviour and/or would like to modify the default values, please feel free to contact us at *e.ferrandi@ibiom.cnr.it*.
