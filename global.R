################################################################################
# Shiny Global
#
# (c) 2021 Müller-Reichert Lab & Robert Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-03
################################################################################

# Global server settings  ------------------------------------------------------
options(shiny.maxRequestSize = 1024 * 1024^2)
# options(shiny.host = "127.0.0.1")
# options(shiny.port = 7878)
options(java.parameters = "-Xmx1024m")
options(encoding = "UTF-8")
options(rgl.useNULL = TRUE)

# Title of the app -------------------------------------------------------------
source("bin/Utility/Library.R")

APP_TITLE <- "ASGA - 3D Viewer v1.3.0"
CC <- paste0("© Copyright GPL V3.0 2021-",
             str_split(Sys.Date(), pattern = "-")[[1]][1],
             ", Robert Kiewisz & Müller-Reichert Lab",
             sep = ""
)

# Global CSS  ------------------------------------------------------------------
includeCSS("www/css/style.css")

JS_CODE <- "
shinyjs.browseURL = function(url) {
  window.location.assign(url,'_blank');
}
"

# Global HTML  -----------------------------------------------------------------
source("www/Home/index.R")
source("www/3D_Viewer/index.R")

# Global Utility  --------------------------------------------------------------
source("bin/Utility/Load_3D_Data.R")
source("bin/Utility/Define_Environment.R")
source("bin/Utility/3D_Generate.R")
source("bin/Utility/3D_Analysis_Settings.R")
source("bin/Utility/Color_Conversion.R")

# Global constant settings  ----------------------------------------------------
CASHING <- FALSE # Constant defining if the rgl widget should reused models and cash file

WINDOW_HEIGHT <- "640px" # Constant to define rgl_widget height
WINDOW_WIDTH <- NULL

Search_for_Data() # Scan environment and define set of variable for UI
