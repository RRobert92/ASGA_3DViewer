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
  # Define Height of browser windows <- currently not working :( ---------------)
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
      js$refresh();
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

  # 3D_Viewer module - load demo --------------------------------------
  observeEvent(input$`Home-3D_View`, {
    # Load Demo ----------------------------------------------------------------
    Load_Data("Demo")

    showTab(inputId = "innavbar-3D", target = "3D_Viewer")
    updateTabsetPanel(session, "innavbar-3D", selected = "3D_Viewer")
  })
  #callModule(Demo_Mode, "Home")

  # 3D_Viewer module - load Pub Data -------------------------------------------
  lapply(1:Publication_No, function(i) {
    observeEvent(input[[paste("Home-3D_Viewer_Pub", i, sep="_")]], {
      # Same action for each Button
        # Load data
        # Define setting
        # Show model
    })
  })
}
