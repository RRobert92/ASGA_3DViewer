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
      js$refresh()
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

  # 3D_Viewer module - load demo -----------------------------------------------
  observeEvent(input$`Home-3D_View_Demo`, {
    DEMO <<- TRUE
    updateCheckboxInput(session, "Home-Hidde_MTs", value = TRUE)

    updateColourInput(session, "Home-Non_KMT_Col", value = "#FFFFFF")
    updateColourInput(session, "Home-KMT_Col", value = "#E32626")

    show("Home-Non_KMT_Col")
    hide("Home-KMT_Col")

    hide("Home-Hidde_MTs")
    hide("Home-Select_KMT_Analysis")

    hide("Home-Select_KMT_Analysis")
    show("Home-Select_SMT_Analysis")

    # Load Demo ----------------------------------------------------------------
    Load_Data("Demo", 1)
    List_of_Kfibers()
    updatePickerInput(session, "Home-Select_fiber", choices = Column_List_Fiber)

    DataSet_Active_List <<- c("Demo")
    updatePickerInput(session, "Home-DataSet_in_Pub", choices = DataSet_Active_List)

    # Update list of available analysis --------------------------------------
    Analysis_Active_List <<- c("All MTs", "KMTs")
    updatePickerInput(session, "Home-Analysis_in_DataSet", choices = Analysis_Active_List)

    `3D_View_Set` <<- input[[paste("Home-Analysis_in_DataSet", sep = "_")]]
    callModule(`3D_Generate`, "Home")

    showTab(inputId = "innavbar-3D", target = "3D_Viewer")
    updateTabsetPanel(session, "innavbar-3D", selected = "3D_Viewer")

    Analysis_List("Demo", "Demo")
    updatePickerInput(session, "Home-Select_KMT_Analysis", choices = AVAILABLE_ANALYSIS_KMTs)
    updatePickerInput(session, "Home-Select_SMT_Analysis", choices = AVAILABLE_ANALYSIS_ALL)
  })

  # 3D_Viewer module - load Pub Data -------------------------------------------
  lapply(1:Publication_No, function(i) {
    DEMO <<- FALSE

    updateColourInput(session, "Home-Non_KMT_Col", value = "#FFFFFF")
    updateColourInput(session, "Home-KMT_Col", value = "#E32626")

    observeEvent(input[[paste("Home-3D_Viewer_Pub", i, sep = "_")]], {
      # Update list of available Data sets -------------------------------------
      DataSet_Active_List <<- get(paste(Publication_Name[i], "Names", sep = "_"))
      updatePickerInput(session, "Home-DataSet_in_Pub", choices = DataSet_Active_List)

      # Update list of available analysis --------------------------------------
      Analysis_Active_List <- c("All MTs", "KMTs")
      updatePickerInput(session, "Home-Analysis_in_DataSet", choices = Analysis_Active_List, selected = "All MTs")

      # Update Tabs ------------------------------------------------------------
      showTab(inputId = "innavbar-3D", target = "3D_Viewer")
      updateTabsetPanel(session, "innavbar-3D", selected = "3D_Viewer")

      # Reload data set if user select different data ----------------------------
      observeEvent(input$`Home-DataSet_in_Pub`, {
        updateColourInput(session, "Home-Non_KMT_Col", value = "#FFFFFF")
        updateColourInput(session, "Home-KMT_Col", value = "#E32626")
        updateCheckboxInput(session, "Home-Hidde_MTs", value = TRUE)

        show("Home-Non_KMT_Col")
        hide("Home-KMT_Col")

        hide("Home-Hidde_MTs")
        hide("Home-Select_KMT_Analysis")

        hide("Home-Select_KMT_Analysis")
        show("Home-Select_SMT_Analysis")

        lapply(1:get(paste(Publication_Name[i], "No", sep = "_")), function(j) {
          if (input[[paste("Home-DataSet_in_Pub", sep = "_")]] == get(paste(Publication_Name[i], "Names", sep = "_"))[j]) {
            Load_Data(paste(getwd(), "/Data/", Publication_Name[i], "/Raw/", sep = ""), j)
            List_of_Kfibers()
            updatePickerInput(session, "Home-Select_fiber", choices = Column_List_Fiber)

            Analysis_List(i, j)
            updatePickerInput(session, "Home-Select_KMT_Analysis", choices = AVAILABLE_ANALYSIS_KMTs)
            updatePickerInput(session, "Home-Select_SMT_Analysis", choices = AVAILABLE_ANALYSIS_ALL)

            `3D_View_Set` <<- "All MTs"
            callModule(`3D_Generate`, "Home")
          }
        })
      })
    })
  })

  observeEvent(input$`Home-Analysis_in_DataSet`, {
    if (input[[paste("Home-Analysis_in_DataSet", sep = "_")]] == "All MTs") {
      show("Home-Non_KMT_Col")
      hide("Home-KMT_Col")

      hide("Home-Select_KMT_Analysis")
      show("Home-Select_SMT_Analysis")

      Non_KMT_Col <<- input[["Home-Non_KMT_Col"]]
      `3D_View_Set` <<- input[[paste("Home-Analysis_in_DataSet", sep = "_")]]

      callModule(`3D_Generate`, "Home")
    }
    if (input[[paste("Home-Analysis_in_DataSet", sep = "_")]] == "KMTs") {
      show("Home-Hidde_MTs")
      show("Home-Select_KMT_Analysis")

      show("Home-Select_KMT_Analysis")
      show("Home-Select_SMT_Analysis")

      Non_KMT_Col <<- input[["Home-Non_KMT_Col"]]
      KMT_Col <<- input[["Home-KMT_Col"]]

      show("Home-Non_KMT_Col")
      show("Home-KMT_Col")

      observeEvent(input$`Home-Hidde_MTs`, {
        if(input$`Home-Hidde_MTs` == FALSE){
          show("Home-Select_fiber")
        } else {
          hide("Home-Select_fiber")
        }

        if (input$`Home-Hidde_MTs` == TRUE) {
          VIEW_ALL <<- TRUE
        } else {
          VIEW_ALL <<- FALSE
        }

        `3D_View_Set` <<- input$`Home-Analysis_in_DataSet`
        callModule(`3D_Generate`, "Home")
      })
    }
  })

  # Collect input about the color
  observeEvent(input$`Home-Non_KMT_Col`, {
    Non_KMT_Col <<- input$`Home-Non_KMT_Col`
  })
  observeEvent(input$`Home-KMT_Col`, {
    KMT_Col <<- input$`Home-KMT_Col`
  })

  # Refresh rgl widget
  observeEvent(input$`Home-Refresh`, {
    if (DEMO == TRUE) {
      callModule(`3D_Generate`, "Home")
    } else {
      callModule(`3D_Generate`, "Home")
    }
  })

  observeEvent(input$`Home-Select_SMT_Analysis`, {
    SMT_Analysis <<- input$`Home-Select_SMT_Analysis`

  })
  observeEvent(input$`Home-Select_KMT_Analysis`, {
    KMT_Analysis <<- input$`Home-Select_KMT_Analysis`
  })

  observeEvent(input$`Home-Select_fiber`, {
    print(input$`Home-Select_fiber`)
      List_of_Kfibers(input$`Home-Select_fiber`)
  })
}
