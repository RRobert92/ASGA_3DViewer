################################################################################
# Shiny Server - 3D view generator module
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# TODO - finally adapt functius to all previous restructures
#
# Author: Robert Kiewisz
# Created: 2021-04-06
################################################################################

`3D_Generate_RunUP` <- function(id, i, j) {
  moduleServer(
    id,
    function(input, output, session) {
      # Load data from i and j
      progressSweetAlert(
        session = session,
        id = "Load_3D",
        title = "Loading 3D data: Loading SpatialGraph...",
        display_pct = TRUE,
        value = 0
      )

      Data_Segments <- Load_Data_Segments(
        paste(getwd(), "/Data/", Publication_Name[i], "/Raw/", sep = ""),
        i,
        j
      )
      Data_Points <- Load_Data_Points(
        paste(getwd(), "/Data/", Publication_Name[i], "/Raw/", sep = ""),
        i,
        j
      )
      updateProgressBar(
        session = session,
        id = "Load_3D",
        title = "Loading 3D data: Loading parameaters...",
        value = 0
      )

      # Update all boxes for selections
      updatePickerInput(session, "Select_fiber", choices = List_of_Kfibers(Data_Segments))
      updatePickerInput(session, "Select_KMT_Analysis", choices = Analysis_List_KMTs(i, j))
      updatePickerInput(session, "Select_SMT_Analysis", choices = Analysis_List_All(i, j))

      # Collect data from all input
      df_Segments <- Data_Segments %>% filter_at(vars(starts_with("Pole")), any_vars(. >= 1))
      df_Segments <- df_Segments %>% select("Segment ID", starts_with("Pole"), "Point IDs")

      # Run rgl
      output$`wdg` <- renderRglwidget({
        open3d()
        rgl.bg(color = "black")

        for (k in 1:nrow(df_Segments)) {
          updateProgressBar(
            session = session,
            id = "Load_3D",
            title = "Loading 3D data: Loading KMTs...",
            value = (k / nrow(df_Segments)) * 100
          )

          MT <- as.numeric(unlist(strsplit(as.character(df_Segments[k, "Point IDs"]), split = ",")))
          MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

          if ("Color" %in% colnames(df_Segments)) {
            lines3d(MT, col = df_Segments[k, "Color"], alpha = 1)
          } else {
            lines3d(MT, col = input$`KMT_Col`, alpha = 1)
          }
        }

        scene <- scene3d()
        rgl.close()
        closeSweetAlert(session = session)
        rglwidget(scene, reuse = CASHING)
      })
    }
  )
}

`3D_Generate_Refresh` <- function(id, i, j) {
  moduleServer(
    id,
    function(input, output, session) {
      # Load data from i and j
      progressSweetAlert(
        session = session,
        id = "Load_3D",
        title = "Loading 3D data: Loading parameaters...",
        display_pct = TRUE,
        value = 0
      )

      Data_Segments <- Load_Data_Segments(
        paste(getwd(), "/Data/", Publication_Name[i], "/Raw/", sep = ""),
        i,
        j
      )
      Data_Points <- Load_Data_Points(
        paste(getwd(), "/Data/", Publication_Name[i], "/Raw/", sep = ""),
        i,
        j
      )

      # Collect all information from inputs
      updateProgressBar(
        session = session,
        id = "Load_3D",
        value = 50
      )

      WINDOW_HEIGHT <- paste(as.numeric(input$dimension[2] - 51), "px", sep = "")
      WINDOW_WIDTH <- round(as.numeric(input$dimension[1]), 0)

      if (WINDOW_WIDTH < 800) {
        FONT_SIZE <- 0.8
      } else if (WINDOW_WIDTH < 1400) {
        FONT_SIZE <- 1
      } else {
        FONT_SIZE <- 2
      }

      Data_to_Show <- input$`Analysis_in_DataSet`
      Show_All_MTs <- input$`Hidde_MTs`
      Fibers_to_Show <- input$`Select_fiber`
      Non_KMT_Col <- input$`Non_KMT_Col`
      KMT_Col <- input$`KMT_Col`
      KMT_Analysis <- input$`Select_KMT_Analysis`
      SMT_Analysis <- input$`Select_SMT_Analysis`

      updateProgressBar(
        session = session,
        id = "Load_3D",
        value = 75
      )
      # Laod data for KMTs
      df_Segments <- Data_Segments %>% filter_at(vars(starts_with("Pole")), any_vars(. >= 1))

      # If show all MT generate df for non-MT to be loaded at the end
      if(Show_All_MTs == TRUE){
      df_Segments_NoN_KMT <- Data_Segments %>% filter_at(vars(starts_with("Pole")), all_vars(. == 0))
      df_Segments_NoN_KMT <- df_Segments_NoN_KMT %>% select("Segment ID", "Point IDs")
      }
      df_Segments <- df_Segments %>% select("Segment ID", starts_with("Pole"), "Point IDs")

      if(Fibers_to_Show != "All"){
        df_Segments <- Data_Segments %>% filter_at(vars(starts_with(Fibers_to_Show)), any_vars(. >= 1))
        df_Segments <- df_Segments %>% select("Segment ID", starts_with(Fibers_to_Show), "Point IDs")
      }

      updateProgressBar(
        session = session,
        id = "Load_3D",
        value = 100
      )
      # Collect analysis
      if(KMT_Analysis != "NaN"){
        df_Data <- Collect_Analysis(KMT_Analysis, i, j)

        if(startsWith(KMT_Analysis, "KMT lattice interaction for")){
          df_Data <- Transform_Data(df_Data, df_Segments)
        }

        ACQ <- Legend_Setting_ACQ(KMT_Analysis)
        MIN_SLIDER <- Legend_Setting_MIN(KMT_Analysis)
        MAX_SLIDER <- Legend_Setting_MAX(KMT_Analysis, df_Data)
        UNIT <- Legend_Setting_UNIT(KMT_Analysis, MIN_SLIDER, MAX_SLIDER)
      }

      if(SMT_Analysis != "NaN"){
        df_Data <- Collect_Analysis(SMT_Analysis, i, j)

        ACQ <- Legend_Setting_ACQ(SMT_Analysis)
        MIN_SLIDER <- Legend_Setting_MIN(SMT_Analysis)
        MAX_SLIDER <- Legend_Setting_MAX(SMT_Analysis, df_Data)
        UNIT <- Legend_Setting_UNIT(SMT_Analysis, MIN_SLIDER, MAX_SLIDER)
      }
      closeSweetAlert(session = session)

      if(!exists("df_Data")){
          output$`wdg` <- renderRglwidget({
            open3d()
            rgl.bg(color = "black")

            progressSweetAlert(
              session = session,
              id = "Load_3D",
              title = "Loading 3D data: Loading KMTs...",
              display_pct = TRUE,
              value = 0
            )

            for (k in 1:nrow(df_Segments)) {
              updateProgressBar(
                session = session,
                id = "Load_3D",
                title = "Loading 3D data: Loading KMTs...",
                value = (k / nrow(df_Segments)) * 100
              )

              MT <- as.numeric(unlist(strsplit(as.character(df_Segments[k, "Point IDs"]), split = ",")))
              MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

              lines3d(MT, col = input$`KMT_Col`, alpha = 1)
            }

            if(Show_All_MTs == TRUE){
              closeSweetAlert(session = session)
              progressSweetAlert(
                session = session,
                id = "Load_3D",
                title = "Loading 3D data: Loading NoN-KMTs...",
                display_pct = TRUE,
                value = 0
              )
              for (k in 1:nrow(df_Segments_NoN_KMT)) {
                updateProgressBar(
                  session = session,
                  id = "Load_3D",
                  title = "Loading 3D data: Loading NoN-KMTs...",
                  value = (k / nrow(df_Segments_NoN_KMT)) * 100
                )

                MT <- as.numeric(unlist(strsplit(as.character(df_Segments_NoN_KMT[k, "Point IDs"]), split = ",")))
                MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

                lines3d(MT, col = input$`Non_KMT_Col`, alpha = 1)
              }
            }

            scene <- scene3d()
            rgl.close()
            closeSweetAlert(session = session)
            rglwidget(scene, reuse = CASHING)
          })
      } else {
        output$`wdg` <- renderRglwidget({
          open3d()
          rgl.bg(color = "black")

          progressSweetAlert(
            session = session,
            id = "Load_3D",
            title = "Loading 3D data: Loading KMTs...",
            display_pct = TRUE,
            value = 0
          )

          scene <- scene3d()
          rgl.close()
          closeSweetAlert(session = session)
          rglwidget(scene, reuse = CASHING)
        })
      }

      closeSweetAlert(session = session)
    }
  )
}
