################################################################################
# Shiny UI-Home
#
# (c) 2021 Müller-Reichert Lab & Robert Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-17
################################################################################

# Shiny UI-Home  ---------------------------------------------------------------

homeUI <- function(id) {
    ns <- NS(id)

    tags$div(
            class = "splash-container",
            tags$div(
                    class = "splash",
                    tags$p(
                            align = "center",
                            tags$img(
                                    src = "https://github.com/RRobert92/ASGA_3DViewer/blob/main/img/ASGA_3D_logo.png?raw=true"
                            )
                    ),
                    tags$h1(
                            class = "splash-head",
                            "Spatial Graph 3D Viewer"
                    ),
                    tags$p(
                            class = "splash-subhead",
                            "ASGA open-source module for interactive visualization of microtubules."
                    ),
                    actionButton("DataViewer", "3D Data Viewer", class = "asga-button asga-button-primary"),
                    actionButton("Wiki", "Wiki", class = "asga-button asga-button-primary")
            ),
            tags$div(
                    class = "footer l-box is-center",
                    tags$p(CC)
            )
    )
}
