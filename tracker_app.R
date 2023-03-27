#######CorGAT-tracker App#######

#Upload packages and data -----
source("CorGAT-tracker_config.R")
source("CorGAT-tracker_functions.R")
source("CorGAT-tracker_ui.R")
source("CorGAT-tracker_server.R")

shinyApp(ui = ui, server = server)
