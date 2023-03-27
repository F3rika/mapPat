#######CorGAT-tracker App#######

#Upload packages and data -----
source("tracker_config.R")
source("tracker_functions.R")
source("tracker_ui.R")
source("tracker_server.R")

shinyApp(ui = ui, server = server)
