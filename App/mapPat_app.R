##############################
#      DEFINING THE APP      #
##############################

#Upload packages and data -----
source("./Scripts/mapPat_config.R")
source("./Scripts/mapPat_functions.R")
source("./Scripts/mapPat_ui.R")
source("./Scripts/mapPat_server.R")

shinyApp(ui = ui, server = server)
