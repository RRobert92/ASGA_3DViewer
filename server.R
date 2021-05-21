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
      # js$refresh()
      updateTabsetPanel(session, "innavbar", selected = "Home")
    }
    if (input$"innavbar-3D" == "3D_Data_Select") {
      rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
      source("global.R")

      # Define Height of browser windows ---------------------------------------
      req(input$dimension)
      if (START_UP == TRUE) {
        assign("WINDOW_HEIGHT",
          paste(as.numeric(input$dimension[2] - 51), "px", sep = ""),
          envir = .GlobalEnv
        )
        assign("WINDOW_WIDTH",
          round(as.numeric(input$dimension[1]), 0),
          envir = .GlobalEnv
        )

        if (WINDOW_WIDTH < 800) {
          FONT_SIZE <<- 0.8
        } else if (WINDOW_WIDTH < 1400) {
          FONT_SIZE <<- 1
        } else {
          FONT_SIZE <<- 2
        }

        START_UP <<- FALSE
      }
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
    Pub_ID <<- 0
    Data_ID <<- 0

    updateCheckboxInput(session, "Home-Hidde_MTs", value = FALSE)

    updateColourInput(session, "Home-Non_KMT_Col", value = "#FFFFFF")
    updateColourInput(session, "Home-KMT_Col", value = "#FF3C28")

    show("Home-Non_KMT_Col")
    hide("Home-KMT_Col")

    hide("Home-Hidde_MTs")
    hide("Home-Select_KMT_Analysis")

    hide("Home-Select_KMT_Analysis")
    show("Home-Select_SMT_Analysis")

    # Load Demo ----------------------------------------------------------------
    Load_Data("Demo", 1, 1)
    List_of_Kfibers()
    updatePickerInput(session, "Home-Select_fiber", choices = Column_List_Fiber)

    DataSet_Active_List <<- c("Demo")
    updatePickerInput(session, "Home-DataSet_in_Pub", choices = DataSet_Active_List)

    # Update list of available analysis ----------------------------------------
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
    updateColourInput(session, "Home-KMT_Col", value = "#FF3C28")

    observeEvent(input[[paste("Home-3D_Viewer_Pub", i, sep = "_")]], {
      # Update list of available Data sets -------------------------------------
      DataSet_Active_List <<- get(paste(Publication_Name[i], "Names", sep = "_"))
      updatePickerInput(session, "Home-DataSet_in_Pub", choices = DataSet_Active_List)

      # Update list of available analysis --------------------------------------
      Analysis_Active_List <- c("KMTs", "All MTs")
      updatePickerInput(session, "Home-Analysis_in_DataSet", choices = Analysis_Active_List, selected = "KMTs")

      # Update Tabs ------------------------------------------------------------
      showTab(inputId = "innavbar-3D", target = "3D_Viewer")
      updateTabsetPanel(session, "innavbar-3D", selected = "3D_Viewer")

      # Reload data set if user select different data ----------------------------
      observeEvent(input$`Home-DataSet_in_Pub`, {
        updateColourInput(session, "Home-Non_KMT_Col", value = "#FFFFFF")
        updateColourInput(session, "Home-KMT_Col", value = "#FF3C28")
        updateCheckboxInput(session, "Home-Hidde_MTs", value = FALSE)

        hide("Home-Non_KMT_Col")
        show("Home-KMT_Col")

        hide("Home-Hidde_MTs")

        show("Home-Select_KMT_Analysis")
        show("Home-Select_SMT_Analysis")

        lapply(1:get(paste(Publication_Name[i], "No", sep = "_")), function(j) {
          if (input[[paste("Home-DataSet_in_Pub", sep = "_")]] == get(paste(Publication_Name[i], "Names", sep = "_"))[j]) {
            Pub_ID <<- i
            Data_ID <<- j
            Load_Data(paste(getwd(), "/Data/", Publication_Name[i], "/Raw/", sep = ""), i, j)
            List_of_Kfibers()
            updatePickerInput(session, "Home-Select_fiber", choices = Column_List_Fiber)

            Analysis_List(i, j)
            updatePickerInput(session, "Home-Select_KMT_Analysis", choices = AVAILABLE_ANALYSIS_KMTs)
            updatePickerInput(session, "Home-Select_SMT_Analysis", choices = AVAILABLE_ANALYSIS_ALL)

            `3D_View_Set` <<- "KMTs"
            callModule(`3D_Generate`, "Home")
          }
        })
      })
    })
  })

  # Responsive for each publications -------------------------------------------
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

      List_of_Kfibers()
      updatePickerInput(session, "Home-Select_fiber", choices = Column_List_Fiber)
      updatePickerInput(session, "Home-Select_fiber", selected = "All")

      Non_KMT_Col <<- input$`Home-Non_KMT_Col`
      `3D_View_Set` <<- input$`Home-Analysis_in_DataSet`

      callModule(`3D_Generate`, "Home")
    }

    if (input$`Home-Analysis_in_DataSet` == "KMTs") {
      show("Home-Hidde_MTs")

      show("Home-Select_KMT_Analysis")
      show("Home-Select_SMT_Analysis")

      Non_KMT_Col <<- input$`Home-Non_KMT_Col`
      KMT_Col <<- input$`Home-KMT_Col`

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

        if (input$`Home-Hidde_MTs` == TRUE) {
          VIEW_ALL <<- TRUE
          updatePickerInput(session, "Home-Select_fiber", selected = "All")
        } else {
          VIEW_ALL <<- FALSE
          updatePickerInput(session, "Home-Select_fiber", selected = "All")
        }

        `3D_View_Set` <<- input$`Home-Analysis_in_DataSet`
        callModule(`3D_Generate`, "Home")
      })
    }
  })

  # Collect input about the color ----------------------------------------------
  observeEvent(input$`Home-Non_KMT_Col`, {
    Non_KMT_Col <<- input$`Home-Non_KMT_Col`
  })
  observeEvent(input$`Home-KMT_Col`, {
    KMT_Col <<- input$`Home-KMT_Col`
  })

  # Refresh rgl widget ---------------------------------------------------------
  observeEvent(input$`Home-Refresh`, {
    req(input$`Home-Select_KMT_Analysis`)
    req(input$`Home-Select_SMT_Analysis`)

    if (DEMO == TRUE) {
      callModule(`3D_Generate`, "Home")
    } else {
      withProgress(message = "Loading data:", value = 1, {
        if (exists("Palette")) {
          rm(Palette, envir = .GlobalEnv)
        }
        if (input$`Home-Select_SMT_Analysis` == "NaN" &&
          input$`Home-Select_KMT_Analysis` != "NaN") {
          Collect_Analysis(input$`Home-Select_KMT_Analysis`, Pub_ID, Data_ID)
        } else if (input$`Home-Select_KMT_Analysis` == "NaN" &&
          input$`Home-Select_SMT_Analysis` != "NaN") {
          Collect_Analysis(input$`Home-Select_SMT_Analysis`, Pub_ID, Data_ID)
          Palette <<- tibble(c(
            KMT_Int,
            NON_KMT_Int
          ))
        }
        setProgress(100)
        Sys.sleep(0.1)
      })
      callModule(`3D_Generate`, "Home")
    }
  })

  # Collect info about KMT anaysis selection -----------------------------------
  observeEvent(input$`Home-Select_KMT_Analysis`, {
    SMT_Analysis <<- input$`Home-Select_SMT_Analysis`

    if (input$`Home-Select_KMT_Analysis` == "Length Distribution" ||
      input$`Home-Select_KMT_Analysis` == "Minus-ends Position" ||
      input$`Home-Select_KMT_Analysis` == "KMTs Curvature" ||
      input$`Home-Select_KMT_Analysis` == "No. of KMTs" ||
      input$`Home-Select_KMT_Analysis` == "No. of KMTs at a Pole") {
      KMT_Analysis <<- TRUE

      hide("Home-Non_KMT_Col")
      show("Home-KMT_Col")
    } else {
      KMT_Analysis <<- FALSE

      hide("Home-Non_KMT_Col")
      hide("Home-KMT_Col")
    }

    if (input$`Home-Select_KMT_Analysis` != "NaN" &&
      input$`Home-Select_SMT_Analysis` != "NaN") {
      updatePickerInput(session, "Home-Select_SMT_Analysis", selected = "NaN")

      hide("Home-Non_KMT_Col")
      show("Home-KMT_Col")
    }
  })

  # Collect info about MT-MT analysis selection --------------------------------
  observeEvent(input$`Home-Select_SMT_Analysis`, {
    if (input$`Home-Select_KMT_Analysis` != "NaN" &&
      input$`Home-Select_SMT_Analysis` != "NaN") {
      updatePickerInput(session, "Home-Select_KMT_Analysis", selected = "NaN")

      KMT_Analysis <<- NULL
      SMT_Analysis <<- input$`Home-Select_SMT_Analysis`
    }

    if (input$`Home-Select_KMT_Analysis` == "NaN") {
      hide("Home-Non_KMT_Col")
      hide("Home-KMT_Col")
    }
    SMT_Analysis <<- input$`Home-Select_SMT_Analysis`
  })

  # Collect info fiber selection -----------------------------------------------
  observeEvent(input$`Home-Select_fiber`, {
    List_of_Kfibers(input$`Home-Select_fiber`)
  })
}
