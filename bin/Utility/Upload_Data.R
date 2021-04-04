################################################################################
# Shiny Server - Upload Data module
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-02
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

# Upload Data for main module --------------------------------------------------

Getfiles_3D <- function(input, output, session) {
  ## Upload 3D data for visualization module -------------------------------------
  observeEvent(input$file_3D, {
    infile <- input$file_3D

    progressSweetAlert(
      session = session,
      id = "Load_3D_Data",
      title = "Loading your 3D data",
      display_pct = TRUE,
      value = 0
    )

    Amira_df <<- readLines(input$file_3D$datapath[1])
    updateProgressBar(
      session = session,
      id = "Load_3D_Data",
      value = 25,
      title = paste("Loading your 3D data: Amira file no. 1")
    )

    Sys.sleep(0.1)
    Amira_df <<- as_tibble(Amira_df)
    names(Amira_df)[1] <<- "X1"

    tryCatch(
      {
        assign(paste("Data", "Nodes", 1, "3D", sep = "_"),
               Load_Amira_Nodes(),
               envir = .GlobalEnv
        )
        updateProgressBar(
          session = session,
          id = "Load_3D_Data",
          value = 50,
          title = paste("Loading your data: Node file no. 1")
        )

        Sys.sleep(0.1)

        assign(paste("Data", "Points", 1, "3D", sep = "_"),
               Load_Amira_Points(),
               envir = .GlobalEnv
        )
        updateProgressBar(
          session = session,
          id = "Load_3D_Data",
          value = 75,
          title = paste("Loading your data: Point file no. 1")
        )

        Sys.sleep(0.1)

        assign(paste("Data", "Segments", 1, "3D", sep = "_"),
               Load_Amira_Segments(),
               envir = .GlobalEnv
        )
      },
      error = function(e) {}
    )
    updateProgressBar(
      session = session,
      id = "Load_3D_Data",
      value = 100,
      title = paste("Loading your data: Segment file no. 1")
    )

    Sys.sleep(0.1)
    closeSweetAlert(session = session)
  })
}
