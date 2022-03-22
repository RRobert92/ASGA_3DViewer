################################################################################
# Shiny Server - Main Server module
#
# (c) 2021 Müller-Reichert Lab & Robert Kiewisz
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

      updateTabsetPanel(session, "innavbar", selected = "Home")
    }
    if (input$"innavbar-3D" == "3D_Data_Select") {
      updateTabsetPanel(session, "innavbar-3D", selected = "3D_Data_Select")
      showTab(inputId = "innavbar-3D", target = "3D_Data_Select")
      hideTab(inputId = "innavbar-3D", target = "3D_Viewer")
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

  sendSweetAlert(
    session = session,
    title = "Usage of a ASGA_3DViewer",
    text = "Due to the limitation of the server the app speed is highly dependent
    on your device. Please be patient during the rendering process.",
    "OK",
    type = "info"
  )

  # 3D_Viewer module - load Pub Data -------------------------------------------
  lapply(1:Publication_No, function(i) {
    observeEvent(input[[paste("Home-3D_Viewer_Pub", i, sep = "_")]], {
      updatePickerInput(session, "Home-DataSet_in_Pub",
        choices = get(paste(Publication_Name[i], "Names", sep = "_")),
        selected = get(paste(Publication_Name[i], "Names", sep = "_"))[1]
      )
      updatePickerInput(session, "Home-Analysis_in_DataSet",
        choices = c("KMTs", "All MTs"),
        selected = "KMTs"
      )

      showTab(inputId = "innavbar-3D", target = "3D_Viewer")
      updateTabsetPanel(session, "innavbar-3D", selected = "3D_Viewer")

      observeEvent(input$`Home-DataSet_in_Pub`, {
        updateCheckboxInput(session, "Home-Hidde_MTs", value = FALSE)

        hide("Home-Non_KMT_Col")
        show("Home-KMT_Col")

        show("Home-Hidde_MTs")

        show("Home-Select_KMT_Analysis")
        show("Home-Select_SMT_Analysis")

        show("Home-Select_fiber")

        # Run first time when Data Set in Pub is changed -----------------------
        lapply(1:get(paste(Publication_Name[i], "No", sep = "_")), function(j) {
          if (input[[paste("Home-DataSet_in_Pub", sep = "_")]] == get(paste(Publication_Name[i], "Names", sep = "_"))[j]) {
            `3D_Generate_RunUP`(
              "Home",
              i, j
            )
          }
        })
      })

      # Refresh rgl widget -------------------------------------------------
      observeEvent(input$`Home-Refresh`, ignoreNULL = TRUE, {
        lapply(1:get(paste(Publication_Name[i], "No", sep = "_")), function(j) {
          if (input[[paste("Home-DataSet_in_Pub", sep = "_")]] == get(paste(Publication_Name[i], "Names", sep = "_"))[j]) {
            `3D_Generate_Refresh`(
              "Home",
              i, which(input$`Home-DataSet_in_Pub` == get(paste(Publication_Name[i], "Names", sep = "_"))),
              input$`Home-Analysis_in_DataSet`,
              input$`Home-Hidde_MTs`,
              input$`Home-Select_fiber`,
              input$`Home-Non_KMT_Col`,
              input$`Home-KMT_Col`,
              input$`Home-Select_KMT_Analysis`,
              nput$`Home-Select_SMT_Analysis`,
              input$`Home-Show_Sister`
            )
          }
        })
      })
    })
  })

  # Button triggering show/hide action -----------------------------------------
  observeEvent(input$`Home-Analysis_in_DataSet`, {
    if (input$"Home-Analysis_in_DataSet" == "All MTs") {
      show("Home-Non_KMT_Col")
      show("Home-KMT_Col")

      hide("Home-Hidde_MTs")
      updateCheckboxInput(session, "Home-Hidde_MTs", value = FALSE)

      hide("Home-Select_fiber")
      updatePickerInput(session, "Home-Select_fiber", selected = "All")

      hide("Home-Select_KMT_Analysis")
      updatePickerInput(session, "Home-Select_KMT_Analysis",
        selected = "NaN"
      )

      hide("Home-Select_SMT_Analysis")
      updatePickerInput(session, "Home-Select_SMT_Analysis",
        selected = "NaN"
      )

      sendSweetAlert(
        session = session,
        title = "Warning!",
        text = "The full MT model is composed of thousands of MTs, rendering it may take time,
        and is highly related to your computer capability.
        If during rendering you will get disconnected from the server.
        Please, do not worry, try it on a faster PC or contact the admin for more information..",
        type = "warning"
      )
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

  # Collect info about KMT analysis selection -----------------------------------
  observeEvent(input$`Home-Select_KMT_Analysis`, {
    if (input$`Home-Select_KMT_Analysis` == "Length Distribution" ||
      input$`Home-Select_KMT_Analysis` == "Minus-ends Position" ||
      input$`Home-Select_KMT_Analysis` == "KMTs Curvature" ||
      input$`Home-Select_KMT_Analysis` == "No. of KMTs" ||
      input$`Home-Select_KMT_Analysis` == "No. of KMTs at a Pole") {
      hide("Home-Non_KMT_Col")
      show("Home-KMT_Col")
    }

    if (input$`Home-Select_KMT_Analysis` != "NaN" &&
      input$`Home-Select_SMT_Analysis` != "NaN") {
      updatePickerInput(session, "Home-Select_SMT_Analysis", selected = "NaN")
    }
  })

  # Collect info about MT-MT analysis selection --------------------------------
  observeEvent(input$`Home-Select_SMT_Analysis`, {
    if (input$`Home-Select_KMT_Analysis` != "NaN" &&
      input$`Home-Select_SMT_Analysis` != "NaN") {
      updatePickerInput(session, "Home-Select_KMT_Analysis", selected = "NaN")
    }

    if (input$`Home-Select_KMT_Analysis` == "NaN") {
      hide("Home-Non_KMT_Col")
      show("Home-KMT_Col")
    }
  })

  observeEvent(input$`Home-Select_KMT_Analysis`, {
    if (input$`Home-Select_KMT_Analysis` != "NaN") {
      hide("Home-Hidde_MTs")
      updateCheckboxInput(session, "Home-Hidde_MTs", value = FALSE)
    } else {
      show("Home-Hidde_MTs")
      updateCheckboxInput(session, "Home-Hidde_MTs", value = FALSE)
    }
  })

  observeEvent(input$`Home-Select_SMT_Analysis`, {
    if (input$`Home-Select_SMT_Analysis` != "NaN") {
      show("Home-Show_Sister")
      hide("Home-Hidde_MTs")
      updateCheckboxInput(session, "Home-Hidde_MTs", value = FALSE)
    } else {
      if (input$`Home-Select_fiber` == "All") {
        hide("Home-Show_Sister")
      }
      updateCheckboxInput(session, "Home-Hidde_MTs", value = FALSE)
    }
  })

  observeEvent(input$`Home-Select_fiber`, {
    if (input$`Home-Select_fiber` == "All") {
      hide("Home-Show_Sister")
      updateCheckboxInput(session, "Home-Show_Sister", value = FALSE)
    } else {
      show("Home-Show_Sister")
    }
  })
}
