################################################################################
# Shiny Server - 3D view generator module
#
# (c) 2021 Müller-Reichert Lab & Robert Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-06
################################################################################

`3D_Generate_RunUP` <- function(id,
                                i, j) {
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
        paste0(getwd(), "/Data/", Publication_Name[i], "/Raw/Small/", sep = ""),
        i,
        j
      )
      Data_Points <- Load_Data_Points(
        paste0(getwd(), "/Data/", Publication_Name[i], "/Raw/Small/", sep = ""),
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
      updatePickerInput(session, "Select_KMT_Analysis", choices = Analysis_List_KMTs(i, j), selected = "NaN")
      updatePickerInput(session, "Select_SMT_Analysis", choices = Analysis_List_All(i, j), selected = "NaN")
      updateColourInput(session, "Non_KMT_Col", value = "#EBBA34")
      updateColourInput(session, "KMT_Col", value = "#FF3C28")
      updatePickerInput(session, "Select_fiber", choices = List_of_Kfibers(Data_Segments), selected = "All")

      # Collect data from all input
      df_Segments <- tibble(Collect_df_Segments(Data_Segments, "Pole", 1))

      updateProgressBar(
        session = session,
        id = "Load_3D",
        title = "Loading 3D data: Loading parameaters...",
        value = 100
      )
      closeSweetAlert(session = session)

      # Run rgl
      output$`wdg` <- renderRglwidget({
        progressSweetAlert(
          session = session,
          id = "Load_3D",
          title = "Loading 3D data: Loading KMTs...",
          display_pct = TRUE,
          value = 0
        )

        open3d()
        rgl.bg(color = "black")

        for (k in seq_len(nrow(df_Segments))) {
          updateProgressBar(
            session = session,
            id = "Load_3D",
            value = (k / nrow(df_Segments)) * 100
          )

          MT <- as.numeric(unlist(strsplit(as.character(df_Segments[k, "Point IDs"]), split = ",")))
          MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

          lines3d(MT, col = "#FF3C28", alpha = 1)
        }

        scene <- scene3d()
        rgl.close()
        closeSweetAlert(session = session)
        rglwidget(scene, reuse = CASHING)
      })

      output$`ScaleBare` <- renderRglwidget({
        open3d()
        rgl.bg(color = "black")
        scene <- scene3d()
        rgl.close()
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
                                  KMT_Analysis, SMT_Analysis,
                                  Show_Sister) {
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

      if (KMT_Analysis != "NaN") {
        Data_Segments <- Load_Data_Segments(
          paste0(getwd(), "/Data/", Publication_Name[i], "/Raw/Small/", sep = ""),
          i,
          j
        )
        Data_Points <- Load_Data_Points(
          paste0(getwd(), "/Data/", Publication_Name[i], "/Raw/Small/", sep = ""),
          i,
          j
        )
      } else if (SMT_Analysis != "NaN") {
        Data_Segments <- Load_Data_Segments(
          paste0(getwd(), "/Data/", Publication_Name[i], "/Raw/Small/", sep = ""),
          i,
          j
        )
        Data_Points <- Load_Data_Points(
          paste0(getwd(), "/Data/", Publication_Name[i], "/Raw/Small/", sep = ""),
          i,
          j
        )
      } else {
        Data_Segments <- Load_Data_Segments(
          paste0(getwd(), "/Data/", Publication_Name[i], "/Raw/Full/", sep = ""),
          i,
          j
        )
        Data_Points <- Load_Data_Points(
          paste0(getwd(), "/Data/", Publication_Name[i], "/Raw/Full/", sep = ""),
          i,
          j
        )
      }
      # Do not allow to load full stack
      if (SMT_Analysis != "NaN" && Fibers_to_Show == "All") {
        updatePickerInput(session, "Select_fiber", selected = List_of_Kfibers(Data_Segments)[2])
        Fibers_to_Show <- List_of_Kfibers(Data_Segments)[2]
      }

      # Collect all information from inputs
      WINDOW_WIDTH <- round(as.numeric(input$dimension[1]), 0)

      if (WINDOW_WIDTH < 500) {
        FONT_SIZE <- 0.5
      } else if (WINDOW_WIDTH < 650) {
        FONT_SIZE <- 0.6
      } else if (WINDOW_WIDTH < 800) {
        FONT_SIZE <- 0.8
      } else if (WINDOW_WIDTH < 1400) {
        FONT_SIZE <- 1
      } else {
        FONT_SIZE <- 2
      }

      # Laod data for KMTs
      df_Segments <- tibble(Collect_df_Segments(Data_Segments, "Pole", 1))

      # If show all MT generate df for non-MT to be loaded at the end
      if (Show_All_MTs == TRUE || Data_to_Show == "All MTs") {
        df_Segments_NoN_KMT <- tibble(Collect_df_Segments(Data_Segments, "Pole", 0))
      }

      if (Fibers_to_Show != "All") {
        if (Show_Sister == FALSE) {
          df_Segments <- tibble(Collect_df_Segments(Data_Segments, Fibers_to_Show, 1))
        } else {
          df_Fiber_Select <- str_split(Fibers_to_Show, "_")
          if (df_Fiber_Select[[1]][1] == "Pole1") {
            df_Fiber_Select[[1]][1] <- "Pole2"
          } else {
            df_Fiber_Select[[1]][1] <- "Pole1"
          }
          df_Fiber_Select <- paste(df_Fiber_Select[[1]][1], df_Fiber_Select[[1]][2], sep = "_")

          df_Segments_1 <- tibble(Collect_df_Segments(Data_Segments, Fibers_to_Show, 1))

          tryCatch(
            {
              df_Segments_2 <- tibble(Collect_df_Segments(Data_Segments, df_Fiber_Select, 1))
              df_Segments <- rbind(df_Segments_1, df_Segments_2)
            },
            error = function(e) {
              df_Segments <- df_Segments_1
            }
          )
        }
      }

      updateProgressBar(
        session = session,
        id = "Load_3D",
        value = 75
      )

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

        if (startsWith(KMT_Analysis, "KMT minus-ends interaction for")) {
          Palette <- tibble(c("#8F8F8F", "#FF7A7A", "#FD7BFD", "#FDDC7B"))
        } else if (startsWith(KMT_Analysis, "KMT lattice interaction for")) {
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
          for (k in seq_len(nrow(df_Segments))) {
            df_Segments[k, 3:5] <- df_Data[
              which(as.numeric(df_Segments[k, 1]) == df_Data$KMT_ID),
              seq_len(ncol(df_Data))
            ]
            if (df_Segments[k, 5] == "NaN") {
              df_Segments[k, 6] <- "#8F8F8F" # KMT without interaction
            } else if (df_Segments[k, 5] == "SMT") {
              df_Segments[k, 6] <- "#FF7A7A" # KMT with Non-KMT interaction
            } else if (df_Segments[k, 5] == "KMT") {
              df_Segments[k, 6] <- "#FD7BFD" # KMT - KMT interaction
            }
          }
          names(df_Segments)[6] <- "Color"

          df_Interactions <- tibble(df_Segments[, "Interaction_ID"])
          names(df_Interactions)[1] <- "Interaction_ID"
          df_Interactions <- df_Interactions[!(df_Interactions$Interaction_ID == "NaN"), ]

          if (exists("df_Interactions") && nrow(df_Interactions) > 0) {
            for (k in seq_len(nrow(df_Interactions))) {
              df_Interactions[k, 2] <- Data_Segments[
                as.numeric(df_Interactions[k, "Interaction_ID"]) + 1,
                "Point IDs"
              ]
              df_Interactions[k, 3] <- "#FDDC7B" # Non-KMT interaction with KMT
            }
            names(df_Interactions)[2:3] <- c("Point IDs", "Color")
          } else {
            rm(df_Interactions)
          }
        } else if (startsWith(KMT_Analysis, "KMT lattice interaction for")) {
          df_Data <- df_Data[(df_Data$KMT_ID %in% df_Segments$`Segment ID`), ]
          df_Segments <- df_Segments[!(df_Segments$`Segment ID` %in% df_Data$KMT_ID), ]
          df_Segments[, 3] <- "#8F8F8F" # KMT without interaction
          names(df_Segments)[2:3] <- c("Point IDs", "Color")

          df_Interactions <- tibble(df_Data[, c("Interactor_ID", "I_class")])
          names(df_Interactions)[1] <- "Interaction_ID"
          df_Interactions <- df_Interactions[!(df_Interactions$Interaction_ID == "NaN"), ]

          if (nrow(df_Data) > 0) {
            for (k in seq_len(nrow(df_Interactions))) {
              df_Interactions[k, 3] <- Data_Segments[
                as.numeric(df_Interactions[k, "Interaction_ID"]) + 1,
                "Point IDs"
              ]

              if (df_Data[k, 2] == "KMT") {
                df_Interactions[k, 4] <- "#FD7BFD" # KMT - KMT interaction
              } else {
                df_Interactions[k, 4] <- "#FDDC7B" # Non-KMT with KMT interaction
              }
            }
            names(df_Interactions)[3:4] <- c("Point IDs", "Color")

            df <- tibble("Segment ID" = df_Data$KMT_ID)

            for (k in seq_len(nrow(df))) {
              df[k, 2] <- Data_Segments[
                as.numeric(df[k, 1]) + 1,
                "Point IDs"
              ]
            }
            df[, 3] <- "#FF7A7A" # KMT with interaction
            names(df)[2:3] <- c("Point IDs", "Color")
            df_Segments <- rbind(df_Segments, df)
          } else {
            rm(df_Interactions)
          }
        } else {
          for (k in seq_len(nrow(df_Segments))) {
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
          names(df_Segments)[4] <- "Color"
        }
      }

      if (SMT_Analysis != "NaN") {
        df_Data <- Collect_Analysis(Data_Segments, SMT_Analysis, i, j)

        # ACQ <- Legend_Setting_ACQ(SMT_Analysis)
        MIN_SLIDER <- Legend_Setting_MIN(SMT_Analysis)
        MAX_SLIDER <- Legend_Setting_MAX(SMT_Analysis, df_Data)
        UNIT <- Legend_Setting_UNIT(SMT_Analysis, MIN_SLIDER, MAX_SLIDER)
        Palette <- tibble(c("#FF7A7A", "#FD7BFD", "#8F8F8F", "#FDDC7B"))

        df_Segments[, 3] <- "#FF7A7A" # KMT lattice
        names(df_Segments)[3] <- "Color"

        if (nrow(df_Data) > 0) {
          df_KMT <- df_Data[(df_Data$`Segment ID` %in% df_Segments$`Segment ID`), c("Segment ID", "S_1_Start", "S_1_Stop")]
          for (k in seq_len(nrow(df_KMT))) {
            df_KMT[k, 4] <- str_c(as.numeric(df_KMT[k, 2]):as.numeric(df_KMT[k, 3]),
              collapse = ","
            )
          }
          df_KMT[5] <- "#FD7BFD" # KMT interaction region
          names(df_KMT)[4:5] <- c("Point IDs", "Color")
          df_KMT <- df_KMT[, c("Point IDs", "Color")]

          df_Non_KMT <- df_Data[(df_Data$`Segment ID` %in% df_Segments$`Segment ID`), c("Segment ID", "Segments_ID_2", "S_2_Start", "S_2_Stop")]
          for (k in seq_len(nrow(df_Non_KMT))) {
            df_Non_KMT[k, 5] <- str_c(as.numeric(df_Non_KMT[k, 3]):as.numeric(df_Non_KMT[k, 4]),
              collapse = ","
            )
          }

          df_Non_KMT[6] <- "#FDDC7B" # Non-KMT interaction region
          names(df_Non_KMT)[5:6] <- c("Point IDs", "Color")
          df_Non_KMT_Lattices <- unique(df_Data[(df_Data$`Segment ID` %in% df_Segments$`Segment ID`), "Segments_ID_2"])
          names(df_Non_KMT_Lattices)[1] <- "Segment ID"
          df_Non_KMT_Lattices <- Data_Segments[Data_Segments$`Segment ID` %in% df_Non_KMT_Lattices$`Segment ID`, c("Segment ID", "Point IDs")]
          df_Non_KMT_Lattices[3] <- "#8F8F8F" # Non-KMT interaction region
          names(df_Non_KMT_Lattices)[3] <- "Color"

          df_Non_KMT <- df_Non_KMT[, c("Point IDs", "Color")]

          df_Interactions <- rbind(df_KMT, df_Non_KMT)
        } else {
          rm(df_Interactions)
        }
      }

      updateProgressBar(
        session = session,
        id = "Load_3D",
        value = 100
      )

      closeSweetAlert(session = session)

      if (!exists("df_Data")) {
        output$`wdg` <- renderRglwidget({
          open3d()
          rgl.bg(color = "black")

          if (nrow(df_Segments) > 0) {
            progressSweetAlert(
              session = session,
              id = "Load_3D",
              title = "Loading 3D data: Loading KMTs...",
              display_pct = TRUE,
              value = 0
            )

            for (k in seq_len(nrow(df_Segments))) {
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
          }

          if (Show_All_MTs == TRUE || Data_to_Show == "All MTs") {
            closeSweetAlert(session = session)
            progressSweetAlert(
              session = session,
              id = "Load_3D",
              title = "Loading 3D data: Loading NoN-KMTs...",
              display_pct = TRUE,
              value = 0
            )

            for (k in seq_len(nrow(df_Segments_NoN_KMT))) {
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

        output$`ScaleBare` <- renderRglwidget({
          if (KMT_Analysis == "NaN" || SMT_Analysis == "NaN") {
            open3d()
            rgl.bg(color = "black")
            scene <- scene3d()
            rgl.close()
            rglwidget(scene, reuse = CASHING)
          }
        })
      } else {
        output$`wdg` <- renderRglwidget({
          open3d()
          rgl.bg(color = "black")

          if (exists("df_Interactions")) {
            progressSweetAlert(
              session = session,
              id = "Load_3D",
              title = "Loading 3D data: Loading KMTs interaction...",
              display_pct = TRUE,
              value = 0
            )

            for (k in seq_len(nrow(df_Interactions))) {
              MT <- as.numeric(unlist(strsplit(as.character(df_Interactions[k, "Point IDs"]), split = ",")))
              MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

              lines3d(MT, col = df_Interactions[k, "Color"], alpha = 1)
            }
          }

          if (exists("df_Segments")) {
            closeSweetAlert(session = session)

            progressSweetAlert(
              session = session,
              id = "Load_3D",
              title = "Loading 3D data: Loading KMTs...",
              display_pct = TRUE,
              value = 0
            )

            for (k in seq_len(nrow(df_Segments))) {
              MT <- as.numeric(unlist(strsplit(as.character(df_Segments[k, "Point IDs"]), split = ",")))
              MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

              lines3d(MT, col = as.character(df_Segments[k, "Color"]), alpha = 1)
            }
          }

          if (exists("df_Non_KMT_Lattices")) {
            closeSweetAlert(session = session)
            progressSweetAlert(
              session = session,
              id = "Load_3D",
              title = "Loading 3D data: Loading non-KMTs...",
              display_pct = TRUE,
              value = 0
            )

            for (k in seq_len(nrow(df_Non_KMT_Lattices))) {
              MT <- as.numeric(unlist(strsplit(as.character(df_Non_KMT_Lattices[k, "Point IDs"]), split = ",")))
              MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

              lines3d(MT, col = as.character(df_Non_KMT_Lattices[k, "Color"]), alpha = 1)
            }
          }

          scene <- scene3d()
          rgl.close()
          closeSweetAlert(session = session)
          rglwidget(scene, reuse = CASHING)
        })

        output$`ScaleBare` <- renderRglwidget({
          if (KMT_Analysis != "NaN" || SMT_Analysis != "NaN") {
            open3d(windowRect = c(10, 10, WINDOW_WIDTH, 200))
            rgl.bg(color = "black")

            bgplot3d(bg.color = "black", {
              plot.new()
              color.legend(0, -0.2, 1, 0.8,
                rect.col = as.list(Palette[1])[[1]],
                legend = UNIT, gradient = "x",
                cex = FONT_SIZE, col = "white"
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
