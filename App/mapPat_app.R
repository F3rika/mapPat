##############################
#      DEFINING THE APP      #
##############################

#Upload packages and data -----
source("mapPat_config.R")
source("mapPat_functions.R")
source("mapPat_ui.R")
source("mapPat_server.R")

shinyApp(ui = ui, server = server)
