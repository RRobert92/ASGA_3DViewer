################################################################################
# Shiny Server - Main Server module
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-03
################################################################################

function(input, output, session) {
  # Define Height of browser windows <- currently not working :( ---------------
  observe({
    if (START_UP == TRUE) {
      req(input$dimension)
      assign("WINDOW_HEIGHT",
        paste(as.numeric(input$dimension[2] - 51), "px", sep = ""),
        envir = .GlobalEnv
      )
    }
  })

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

  # 3D_Viewer module - loading data action -------------------------------------
  observeEvent(input$`Home-file_3D`, {
    callModule(Getfiles_3D, "Home")
    Sys.sleep(1)

    showTab(inputId = "innavbar-3D", target = "3D_Viewer")
    updateTabsetPanel(session, "innavbar-3D", selected = "3D_Viewer")
  })

  # 3D_Viewer module - load demo -----------------------------------------------
  observeEvent(input$`Home-3D_View`, {
    # Load Demo ----------------------------------------------------------------
    Load_Data("Demo", 1)
    callModule(Demo_Mode, "Home")

    showTab(inputId = "innavbar-3D", target = "3D_Viewer")
    updateTabsetPanel(session, "innavbar-3D", selected = "3D_Viewer")
  })

  # 3D_Viewer module - load Pub Data -------------------------------------------
  lapply(1:Publication_No, function(i) {
    observeEvent(input[[paste("Home-3D_Viewer_Pub", i, sep = "_")]], {
      # Update list of available Data sets -------------------------------------
      DataSet_Active_List <<- get(paste(Publication_Name[i], "Names", sep = "_"))
      updatePickerInput(session, "Home-DataSet_in_Pub", choices = DataSet_Active_List)

      # Update list of available analysis --------------------------------------
      Analysis_Active_List <<- get(paste(Publication_Name[i], "Analysis_Names", 1, sep = "_"))
      if (length(Analysis_Active_List) == 0) {
        Analysis_Active_List <- c("All MT", "KMTs")
      }
      updatePickerInput(session, "Home-Analysis_in_DataSet", choices = Analysis_Active_List)

      # Update Tabs ------------------------------------------------------------
      showTab(inputId = "innavbar-3D", target = "3D_Viewer")
      updateTabsetPanel(session, "innavbar-3D", selected = "3D_Viewer")
    })

    # Reload data set if user select different data
    observeEvent(input$`Home-DataSet_in_Pub`, {
      lapply(1:get(paste(Publication_Name[i], "No", sep = "_")), function(j) {
        if (input[[paste("Home-DataSet_in_Pub", sep = "_")]] == get(paste(Publication_Name[i], "Names", sep = "_"))[j]) {
          progressSweetAlert(
            session = session,
            id = "Load_3D",
            title = paste("Loading 3D data for ", Publication_Name[i], ", data set: ", get(paste(Publication_Name[i], "Names", sep = "_"))[j], sep = ""),
            display_pct = TRUE,
            value = 0
          )

          Load_Data(paste(getwd(), "/Data/", Publication_Name[i], "/Raw/", sep = ""), j)

          Analysis_Active_List <<- get(paste(Publication_Name[i], "Analysis_Names", 1, sep = "_"))

          if (length(Analysis_Active_List) == 0) {
            Analysis_Active_List <- c("All MT", "KMTs")
          }
          updatePickerInput(session, "Home-Analysis_in_DataSet", choices = Analysis_Active_List)

          Sys.sleep(0.1)
          closeSweetAlert(session = session)
        }
      })
    })
  })
}
