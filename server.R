################################################################################
# Shiny Server - Main Server module
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
# TODO - Restructure code for higher scalability
#
# Author: Robert Kiewisz
# Created: 2021-04-03
################################################################################

function(input, output, session) {

  # Hide pages on start  -------------------------------------------------------
  hideTab(inputId = "innavbar", target = "3D_Viewer")

  # Wiki & about button actions ------------------------------------------------
  observeEvent(input$Wiki, {
    js$browseURL("https://rrobert92.github.io/ASGA/")
  })

  observeEvent(input$innavbar, {
    if (input$innavbar == "Home") {
      updateTabsetPanel(session, "innavbar", selected = "Home")
    }
    if (input$innavbar == "Wiki") {
      js$browseURL("https://rrobert92.github.io/ASGA/")
      updateTabsetPanel(session, "innavbar", selected = "Home")
    }
    if (input$innavbar == "About") {
      js$browseURL("https://rrobert92.github.io/ASGA/Cit/")
      updateTabsetPanel(session, "innavbar", selected = "Home")
    }
  })

  observeEvent(input$"innavbar-3D", {
    if (input$"innavbar-3D" == "Home") {
      hideTab(inputId = "innavbar", target = "3D_Data_Select")
      hideTab(inputId = "innavbar-3D", target = "3D_Data_Select")
      hideTab(inputId = "innavbar", target = "3D_Viewer")
      hideTab(inputId = "innavbar-3D", target = "3D_Viewer")

      rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
      source("global.R")

      updateTabsetPanel(session, "innavbar", selected = "Home")
    }
    if (input$"innavbar-3D" == "3D_Data_Select") {
      rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
      source("global.R")
    }

    if (input$"innavbar-3D" == "Wiki") {
      js$browseURL("https://rrobert92.github.io/ASGA/")
    }
    if (input$"innavbar-3D" == "About") {
      js$browseURL("https://rrobert92.github.io/ASGA/Cit/")
    }
  })

  # 3D_Viewer button actions  --------------------------------------------------
  observeEvent(input$DataViewer, {
    hideTab(inputId = "innavbar-3D", target = "3D_Viewer")
    showTab(inputId = "innavbar-3D", target = "3D_Data_Select")
    showTab(inputId = "innavbar", target = "3D_Viewer")

    updateTabsetPanel(session, "innavbar", selected = "3D_Viewer")
    updateTabsetPanel(session, "innavbar-3D", selected = "3D_Data_Select")
  })

  # 3D_Viewer module - load Pub Data -------------------------------------------
  lapply(1:Publication_No, function(i) {
    updateColourInput(session, "Home-Non_KMT_Col", value = "#FFFFFF")
    updateColourInput(session, "Home-KMT_Col", value = "#FF3C28")

    observeEvent(input[[paste("Home-3D_Viewer_Pub", i, sep = "_")]], {
      updatePickerInput(session, "Home-DataSet_in_Pub", choices = get(paste(Publication_Name[1], "Names", sep = "_")))
      updatePickerInput(session, "Home-Analysis_in_DataSet", choices = c("KMTs", "All MTs"), selected = "KMTs")

      showTab(inputId = "innavbar-3D", target = "3D_Viewer")
      updateTabsetPanel(session, "innavbar-3D", selected = "3D_Viewer")

      observeEvent(input$`Home-DataSet_in_Pub`, {
        updateColourInput(session, "Home-Non_KMT_Col", value = "#FFFFFF")
        updateColourInput(session, "Home-KMT_Col", value = "#FF3C28")
        updateCheckboxInput(session, "Home-Hidde_MTs", value = FALSE)

        hide("Home-Non_KMT_Col")
        show("Home-KMT_Col")

        show("Home-Hidde_MTs")

        show("Home-Select_KMT_Analysis")
        show("Home-Select_SMT_Analysis")

        show("Home-Select_fiber")

        # Loop to load and display for the first time the pub is selected
        lapply(1:get(paste(Publication_Name[i], "No", sep = "_")), function(j) {
          # Run first time when DataSet in Pub is changed
          if (input$`Home-DataSet_in_Pub` == get(paste(Publication_Name[i], "Names", sep = "_"))[j]) {
            `3D_Generate_RunUP`("Home", i, j)

            # Refresh rgl widget -------------------------------------------------------
            observeEvent(req(input$`Home-Refresh`), {
              `3D_Generate_Refresh`("Home",
                                    i, j,
                                    input$`Home-Analysis_in_DataSet`, input$`Home-Hidde_MTs`,
                                    input$`Home-Select_fiber`,
                                    input$`Home-Non_KMT_Col`, input$`Home-KMT_Col`,
                                    input$`Home-Select_KMT_Analysis`, input$`Home-Select_SMT_Analysis`)
            })
          }
        })
      })
    })
  })

  # Button triggering show/hide action
  observeEvent(input$`Home-Analysis_in_DataSet`, {
    if (input$"Home-Analysis_in_DataSet" == "All MTs") {
      show("Home-Non_KMT_Col")
      hide("Home-KMT_Col")

      hide("Home-Hidde_MTs")
      updateCheckboxInput(session, "Home-Hidde_MTs", value = FALSE)

      hide("Home-Select_fiber")
      updatePickerInput(session, "Home-Select_fiber", selected = "All")

      hide("Home-Select_KMT_Analysis")
      hide("Home-Select_SMT_Analysis")
    }

    if (input$`Home-Analysis_in_DataSet` == "KMTs") {
      show("Home-Hidde_MTs")

      show("Home-Select_KMT_Analysis")
      show("Home-Select_SMT_Analysis")

      show("Home-KMT_Col")
      hide("Home-Non_KMT_Col")

      observeEvent(input$`Home-Hidde_MTs`, {
        if (input$`Home-Hidde_MTs` == FALSE) {
          show("Home-Select_fiber")
          hide("Home-Non_KMT_Col")
        } else {
          hide("Home-Select_fiber")
          show("Home-Non_KMT_Col")
        }

        updatePickerInput(session, "Home-Select_fiber", selected = "All")
      })
    }
  })

  observeEvent(input$`Home-Hidde_MTs`, {
    updatePickerInput(session, "Home-Select_fiber", selected = "All")
  })
}
