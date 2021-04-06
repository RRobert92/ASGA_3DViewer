################################################################################
# Shiny Global
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-03
################################################################################

# Global server settings  ------------------------------------------------------
options(shiny.maxRequestSize = 1024 * 1024^2)
options(shiny.host = "127.0.0.1")
options(shiny.port = 7878)
options(java.parameters = "-Xmx1024m")

# Title of the app -------------------------------------------------------------
source("bin/Utility/Library.R")

APP_TITLE <- "ASGA - 3D Viewer v0.1.0"
CC <- paste("© Copyright GPL V3.0 2021-",
            str_split(Sys.Date(), pattern = "-")[[1]][1],
            ", Robert Kiewisz",
            sep = ""
)

# Global CSS  ------------------------------------------------------------------
includeCSS("www/css/style.css")

JS_CODE <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"
JS_RESET <- "shinyjs.refresh = function() { history.go(0); }"

# Global HTML  -----------------------------------------------------------------
source("www/Home/index.R")
source("www/3D_Viewer/index.R")

# Global Utility  --------------------------------------------------------------
source("bin/Utility/Upload_Data.R")
source("bin/Utility/Load_Amira.R")
source("bin/Utility/Load_3D_Data.R")
source("bin/Utility/Define_Environment.R")
source("bin/Utility/3D_Generate.R")


# Global constant settings  ----------------------------------------------------
SHINY_IO <<- TRUE # Constant defining if app is running locally or online
START_UP <<- TRUE # Constant defining if app was stared freshly
VIEW_ALL <<- TRUE # Constant defining if app will show all MTs or just KMTs
Demo <<- FALSE

Non_KMT_Col <<- "#FFFFFF"
KMT_Col <<- "#CC1414"

WINDOW_HEIGHT <<- "640px"
Search_for_Data() # Scan environment and define set of variable for UI
