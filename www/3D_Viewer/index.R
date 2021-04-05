################################################################################
# Shiny UI - 3D viewer
#
# (c) 2021 Kiewisz
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
      title = "3D Module",
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
              lapply(1:Publication_No, function(i) {
                tags$div(
                  class = "btn-default-3D",
                  actionButton(
                    inputId = ns(paste("3D_Viewer_Pub", i, sep="_")),
                    label = Publication_Name[i],
                    width = "100%"
                    )
                )
              })
            ),
            tags$div(
              class = "splash-input-3D",
              tags$p(),
              fileInput(
                inputId = ns("file_3D"),
                label = "Load 3D model",
                multiple = FALSE,
                accept = ".am"
              )
            ),
            tags$div(
              class = "splash-input-3D",
              actionBttn(
                inputId = ns("3D_View_Demo"),
                label = "Demo",
                style = "material-flat",
                color = "primary"
              )
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
            pickerInput(
              inputId = ns("DataSet_in_Pub"), label = "Select data set :",
              choices = c("Init"),
              multiple = FALSE
            ),
            pickerInput(
              inputId = ns("Analysis_in_DataSet"), label = "Select 3D View :",
              choices = c("Init"),
              multiple = FALSE
            )
          ),
          column(
            10,
            offset = 0,
            rglwidgetOutput(ns("wdg"), width = "100%", height = WINDOW_HEIGHT) %>% withSpinner()
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
