################################################################################
# Shiny UI -  Main UI module
#
# (c) 2021 Müller-Reichert Lab & Robert Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-03
################################################################################

# Shiny UI-Home  ---------------------------------------------------------------
fluidPage(
        includeCSS("www/css/style.css"),
        useShinyalert(),
        useShinyjs(),
        extendShinyjs(text = JS_CODE, functions = "browseURL"),
        # extendShinyjs(text = JS_RESET, functions = "refresh"),
        navbarPage(
                title = APP_TITLE,
                collapsible = TRUE,
                inverse = TRUE,
                position = "fixed-top",
                id = "innavbar",
                selected = "Home",
                # footer = footnoteUI("footnote"),
                tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.screen.width;
                                    dimension[1] = window.screen.height;
                                    Shiny.onInputChange("Home-dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.screen.width;
                                    dimension[1] = window.screen.height;
                                    Shiny.onInputChange("Home-dimension", dimension);
                                });
                            ')),
                tabPanel(
                        title = "Home",
                        value = "Home",
                        fluidRow(
                                homeUI("Home")
                        )
                ),
                tabPanel(
                        title = "3D Viewer",
                        value = "3D_Viewer",
                        Viewer_UI("Home")
                ),
                tabPanel(
                        title = "Wiki",
                        value = "Wiki"
                ),
                tabPanel(
                        title = "Cite Us",
                        value = "cite_us"
                )
        )
)
