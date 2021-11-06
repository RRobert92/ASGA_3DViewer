################################################################################
# Shiny UI - 3D viewer
#
# (c) 2021 Müller-Reichert Lab & Robert Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-03
################################################################################

# UI-3D viewer  ---------------------------------------------------------------
Viewer_UI <- function(id) {
  ns <- NS(id)
  navbarPage(
    title = APP_TITLE,
    collapsible = TRUE,
    inverse = TRUE,
    position = "fixed-top",
    id = "innavbar-3D",
    selected = "UploadData",

    # UI-3D viewer-Home  ------------------------------------------------------

    tabPanel(
      title = "Home",
      value = "Home"
    ),
    tabPanel(
      title = "Publication's",
      value = "3D_Data_Select",
      tags$div(
        class = "splash-container-3D",
        tags$div(
          class = "splash-3D",
          tags$h1(
            class = "splash-head-3D",
            "3D module"
          ),
          tags$p(
            class = "splash-subhead-3D",
            "Select data to visualized from one of our paper or load your own Amira .am files."
          ),
          tags$div(
            class = "splash-input-3D-row",
            tags$div(
              class = "splash-input-3D",
              "Select publication",
              if (Publication_No != 0) {
                lapply(1:Publication_No, function(i) {
                  tags$div(
                    class = "btn-default-3D",
                    actionButton(
                      inputId = ns(paste("3D_Viewer_Pub", i, sep = "_")),
                      label = str_replace_all(Publication_Name[i], "_", " "),
                      width = "100%"
                    )
                  )
                })
              }
            )
          )
        ),
        tags$div(
          class = "footer l-box is-center",
          tags$p(CC)
        )
      )
    ),
    tabPanel(
      title = "3D Viewer",
      value = "3D_Viewer",
      fluidRow(
        tags$div(
          class = "table-3D",
          column(
            2,
            offset = 0,
            tags$div(
              class = "Tools-3D",
              pickerInput(
                inputId = ns("DataSet_in_Pub"),
                label = "Select data set",
                choices = "Init",
                multiple = FALSE
              )
            ),
            tags$div(class = "h_line"),
            tags$div(
              class = "Tools-3D",
              pickerInput(
                inputId = ns("Analysis_in_DataSet"),
                label = "Select 3D View",
                choices = "KMTs",
                multiple = FALSE
              )
            ),
            tags$div(
              class = "Tools-3D",
              hidden(
                checkboxInput(
                  inputId = ns("Hidde_MTs"),
                  label = "Show non-KMTs (Longer calculation)",
                  value = FALSE
                )
              )
            ),
            tags$div(
              class = "Tools-3D",
              hidden(
                pickerInput(
                  inputId = ns("Select_fiber"),
                  label = "Select k-fibers",
                  choices = c("All"),
                  multiple = FALSE
                )
              )
            ),
            tags$div(
              class = "Tools-3D",
              hidden(
                checkboxInput(
                  inputId = ns("Show_Sister"),
                  label = "Show sister k-fiber",
                  value = FALSE
                )
              )
            ),
            tags$div(
              class = "Tools-3D",
              hidden(
                colourInput(ns("Non_KMT_Col"),
                  label = "Non-KMTs color",
                  value = "#FFFFFF"
                )
              )
            ),
            tags$div(
              class = "Tools-3D",
              hidden(
                colourInput(ns("KMT_Col"),
                  label = "KMTs color",
                  value = "#FF3C28"
                )
              )
            ),
            tags$div(
              class = "Tools-3D",
              hidden(
                pickerInput(
                  inputId = ns("Select_KMT_Analysis"),
                  label = "Select KMT analysis",
                  choices = c("NaN"),
                  multiple = FALSE
                )
              )
            ),
            tags$div(
              class = "Tools-3D",
              hidden(
                pickerInput(
                  inputId = ns("Select_SMT_Analysis"),
                  label = "Select MT-MT interaction analysis",
                  choices = c("NaN"),
                  multiple = FALSE
                )
              )
            ),
            tags$div(
              class = "splash-input-setting",
              actionBttn(
                inputId = ns("Refresh"),
                label = "Refresh",
                style = "material-flat",
                color = "primary"
              )
            )
          ),
          column(
            10,
            offset = 0,
            rglwidgetOutput(ns("wdg"), width = "100%", height = WINDOW_HEIGHT) %>% withSpinner(),
            rglwidgetOutput(ns("ScaleBare"), width = "100%", height = 200)
          )
        )
      )
    ),
    tabPanel(
      title = "Wiki",
      value = "Wiki"
    ),
    tabPanel(
      title = "About",
      value = "About"
    )
  )
}
