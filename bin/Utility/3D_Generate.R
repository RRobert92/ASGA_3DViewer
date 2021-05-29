################################################################################
# Shiny Server - 3D view generator module
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# TODO - finally adapt functions to all previous restructures
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
        paste0(getwd(), "/Data/", Publication_Name[i], "/Raw/", sep = ""),
        i,
        j
      )
      Data_Points <- Load_Data_Points(
        paste0(getwd(), "/Data/", Publication_Name[i], "/Raw/", sep = ""),
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
      updatePickerInput(session, "Select_fiber", choices = List_of_Kfibers(Data_Segments), selected = "All")
      updatePickerInput(session, "Select_KMT_Analysis", choices = Analysis_List_KMTs(i, j), selected = "NaN")
      updatePickerInput(session, "Select_SMT_Analysis", choices = Analysis_List_All(i, j), selected = "NaN")

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

          lines3d(MT, col = input$`KMT_Col`, alpha = 1)
        }

        scene <- scene3d()
        rgl.close()
        closeSweetAlert(session = session)
        rglwidget(scene, reuse = CASHING)
      })
    }
  )
}

`3D_Generate_Refresh` <- function(id,
                                  i, j,
                                  Data_to_Show, Show_All_MTs,
                                  Fibers_to_Show,
                                  Non_KMT_Col, KMT_Col,
                                  KMT_Analysis, SMT_Analysis) {
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
        paste0(getwd(), "/Data/", Publication_Name[i], "/Raw/", sep = ""),
        i,
        j
      )
      Data_Points <- Load_Data_Points(
        paste0(getwd(), "/Data/", Publication_Name[i], "/Raw/", sep = ""),
        i,
        j
      )

      # Collect all information from inputs
      WINDOW_HEIGHT <- paste0(as.numeric(input$dimension[2] - 51), "px", sep = "")
      WINDOW_WIDTH <- round(as.numeric(input$dimension[1]), 0)

      if (WINDOW_WIDTH < 800) {
        FONT_SIZE <- 0.8
      } else if (WINDOW_WIDTH < 1400) {
        FONT_SIZE <- 1
      } else {
        FONT_SIZE <- 2
      }

      # Laod data for KMTs
      df_Segments <- Data_Segments %>% filter_at(vars(starts_with("Pole")), any_vars(. >= 1))

      # If show all MT generate df for non-MT to be loaded at the end
      if (Show_All_MTs == TRUE) {
        df_Segments_NoN_KMT <- Data_Segments %>% filter_at(vars(starts_with("Pole")), all_vars(. == 0))
        df_Segments_NoN_KMT <- df_Segments_NoN_KMT %>% select("Segment ID", "Point IDs")
      }
      df_Segments <- df_Segments %>% select("Segment ID", "Point IDs")

      updateProgressBar(
        session = session,
        id = "Load_3D",
        value = 100
      )

      if (Fibers_to_Show != "All") {
        df_Segments <- Data_Segments %>% filter_at(vars(starts_with(Fibers_to_Show)), any_vars(. >= 1))
        df_Segments <- df_Segments %>% select("Segment ID", "Point IDs")
      }

      # Collect analysis
      if (KMT_Analysis != "NaN") {
        df_Data <- Collect_Analysis(Data_Segments, KMT_Analysis, i, j)

        if (startsWith(KMT_Analysis, "KMT lattice interaction for")) {
          df_Data <- Transform_Data(df_Data, Data_Segments)
        }

        ACQ <- Legend_Setting_ACQ(KMT_Analysis)
        MIN_SLIDER <- Legend_Setting_MIN(KMT_Analysis)
        MAX_SLIDER <- Legend_Setting_MAX(KMT_Analysis, df_Data)
        UNIT <- Legend_Setting_UNIT(KMT_Analysis, MIN_SLIDER, MAX_SLIDER)

        if (startsWith(KMT_Analysis, "KMT minus-ends interaction for") ||
          startsWith(KMT_Analysis, "KMT lattice interaction for")) {
          Palette <- tibble(c("#8F8F8F", "#FF7A7A", "#FD7BFD", "#FDDC7B"))
        } else {
          Palette <- Creat_Palette(
            MIN_SLIDER,
            MAX_SLIDER,
            nrow(df_Segments),
            ACQ,
            KMT_Col
          )
        }

        if (startsWith(KMT_Analysis, "KMT minus-ends interaction for")) {
          # TODO assigin colors to df_Segments
          for (k in 1:nrow(df_Segments)) {
            df_Segments[k, 3:5] <- df_Data[
              which(as.numeric(df_Segments[k, 1]) == df_Data$KMT_ID),
              1:ncol(df_Data)
            ]
          }

        } else if (startsWith(KMT_Analysis, "KMT lattice interaction for")) {
          # TODO handle df_data if this is selected

        } else {
          for (k in 1:nrow(df_Segments)) {
            if (df_Segments[k, 1] %in% df_Data$`Segment ID`) {
              df_Segments[k, 3] <- df_Data[
                which(as.numeric(df_Segments[k, 1]) == df_Data$`Segment ID`),
                "Data"
              ]
              df_Segments[k, 4] <- Palette[
                which.min(abs(Palette$Range - as.numeric(df_Segments[k, 3]))),
                1
              ]
            } else {
              df_Segments[k, 3] <- 0
              df_Segments[k, 4] <- Palette[
                which.min(abs(Palette$Range - as.numeric(df_Segments[k, 3]))),
                1
              ]
            }
          }
        }

        if (nrow(df_Segments) >= 4) {
          names(df_Segments)[2:4] <- c("Point IDs", "Data", "Color")
        }
      }


      if (SMT_Analysis != "NaN") {
        df_Data <- Collect_Analysis(SMT_Analysis, i, j)

        ACQ <- Legend_Setting_ACQ(SMT_Analysis)
        MIN_SLIDER <- Legend_Setting_MIN(SMT_Analysis)
        MAX_SLIDER <- Legend_Setting_MAX(SMT_Analysis, df_Data)
        UNIT <- Legend_Setting_UNIT(SMT_Analysis, MIN_SLIDER, MAX_SLIDER)
        Palette <- tibble(c("#FD7BFD", "#8F8F8F"))
      }

      closeSweetAlert(session = session)


      if (!exists("df_Data")) {
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

          if (Show_All_MTs == TRUE) {
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

          for (k in 1:nrow(df_Segments)) {
            updateProgressBar(
              session = session,
              id = "Load_3D",
              title = "Loading 3D data: Loading KMTs with analysis...",
              value = (k / nrow(df_Segments)) * 100
            )

            # TODO if "POint IDs do not exisit then, search for IDs list in the Data_segment
            MT <- as.numeric(unlist(strsplit(as.character(df_Segments[k, "Point IDs"]), split = ",")))
            MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

            lines3d(MT, col = as.character(df_Segments[k, 4]), alpha = 1)
          }

          scene <- scene3d()
          rgl.close()
          closeSweetAlert(session = session)
          rglwidget(scene, reuse = CASHING)
        })

        output$`ScaleBare` <- renderRglwidget({
          if(exists("Palette")){
            open3d(windowRect = c(10, 10, WINDOW_WIDTH, 200))
            rgl.bg(color = "white")
            bgplot3d({
              plot.new()
              color.legend(0, -0.2, 1, 0.8,
                           rect.col = as.list(Palette[1])[[1]],
                           legend = UNIT, gradient = "x",
                           cex = FONT_SIZE
              )
            })
            scene <- scene3d()
            rgl.close()
            rglwidget(scene, reuse = CASHING)
          }
        })
      }

      closeSweetAlert(session = session)
    }
  )
}
