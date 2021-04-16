################################################################################
# Shiny Server - 3D view generator module
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-06
################################################################################

`3D_Generate` <- function(input, output, session) {
  if (`3D_View_Set` == "All MTs") {
    output$`wdg` <- renderRglwidget({
      open3d()
      rgl.bg(color = "black", fogtype = "none")

      progressSweetAlert(
        session = session,
        id = "Load_3D",
        title = "Loading 3D data : ",
        display_pct = TRUE,
        value = 0
      )

      for (i in 1:nrow(Data_Segments)) {
        updateProgressBar(
          session = session,
          id = "Load_3D",
          value = (i / nrow(Data_Segments)) * 100
        )
        # Sys.sleep(0.1)
        MT <- as.numeric(unlist(strsplit(Data_Segments[i, "Point IDs"], split = ",")))
        MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

        lines3d(MT, col = Non_KMT_Col, alpha = 1)
      }

      scene <- scene3d()
      rgl.close()
      closeSweetAlert(session = session)
      rglwidget(scene, reuse = CASHING)
    })
  }
  if (`3D_View_Set` == "KMTs") {
    output$`wdg` <- renderRglwidget({
      open3d()
      rgl.bg(color = "black", fogtype = "none")

      if (input$`Select_fiber` != "All") {
        df_Segments <- Data_Segments %>% filter_at(vars(starts_with(input$`Select_fiber`)), any_vars(. >= 1))
        df_Segments <- df_Segments %>% select("Segment ID", starts_with(input$`Select_fiber`), "Point IDs")
      } else {
        df_Segments <- Data_Segments %>% filter_at(vars(starts_with("Pole")), any_vars(. >= 1))
        df_Segments <- df_Segments %>% select("Segment ID", starts_with("Pole"), "Point IDs")
      }

      if (!is.null(KMT_Analysis) && KMT_Analysis == TRUE && !is.null(Data) && "Segment ID" %in% colnames(Data)) {
        df_Data <- subset(Data, subset = `Segment ID` %in% df_Segments$`Segment ID`)
        df_Segments <- subset(df_Segments, subset = `Segment ID` %in% Data$`Segment ID`)

        Palette <- Creat_Palette(
          MIN_SLIDER,
          MAX_SLIDER,
          nrow(df_Segments),
          ACQ,
          KMT_Col
        )

        df_Data <- df_Data[order(df_Data$`Segment ID`), ]
        df_Segments <- cbind(df_Segments, df_Data["Data"])
        df_Segments <- tibble(
          df_Segments$`Segment ID`,
          df_Segments$`Point IDs`,
          df_Segments$`Data`
        )
        names(df_Segments)[1:3] <- c("Segment ID", "Point IDs", "Data")

        df_col <- tibble()
        for (i in 1:nrow(df_Segments)) {
          df_col[i, 1] <- Palette$Color[which.min(abs(Palette$Range - as.numeric(df_Segments[i, 3])))]
        }

        df_Segments <- cbind(df_Segments, df_col)
        names(df_Segments)[4] <- c("Color")
      }

      if (!is.null(KMT_Analysis) && KMT_Analysis == FALSE && !is.null(Data) && "KMT_ID" %in% colnames(Data) && "MT_type" %in% colnames(Data)) {
        # how to handle the data when user want to see interactions
        df_Data <- subset(Data, subset = `KMT_ID` %in% df_Segments$`Segment ID`)
        df_Segments <- subset(df_Segments, subset = `Segment ID` %in% Data$`KMT_ID`)
        df_Segments <- select(df_Segments, all_of(c("Segment ID")))

        df_Data <- df_Data %>% filter(Interaction_ID != "NaN")

        if (nrow(df_Data) > 0) {
          df_col <- tibble()
          for (i in 1:nrow(df_Segments)) {
            df_col[i, 1] <- df_Segments[i, "Segment ID"]

            if (df_Segments[i, "Segment ID"] %in% as.list(df_Data["KMT_ID"])[[1]]) {
              df_col[i, 2] <- "#FF7A7A" # KMT with inter == 1 / light red
            } else {
              df_col[i, 2] <- "#8F8F8F" # KMT without inter == 0 / gray
            }
          }

          for (i in 1:nrow(df_Data)) {
            if (df_Data[i, "Interaction_ID"] %in% as.list(df_Segments["Segment ID"])[[1]]) {
              Find_Col <- which(df_Segments$`Segment ID` == as.numeric(df_Data[i, "Interaction_ID"]))
              df_col[Find_Col, 2] <- "#FD7BFD" # KMT interacting == 2 / purple
            } else {
              Find_Col <- nrow(df_col) + 1
              df_col[Find_Col, 1] <- as.numeric(df_Data[i, "Interaction_ID"])
              df_col[Find_Col, 2] <- "#FDDC7B" # NoN-KMT interacting == 3 / light yellow
            }
          }

          for (i in 1:nrow(df_col)) {
            df_col[i, 3] <- Data_Segments[which(as.numeric(df_col[i, 1]) == Data_Segments$`Segment ID`), "Point IDs"]
          }

          df_Segments <- df_col
          names(df_Segments)[1:3] <- c("Segment ID", "Color", "Point IDs")
        } else {
          df_Segments[2] <- "#8F8F8F"
          for (i in 1:nrow(df_Segments)) {
            df_Segments[i, 3] <- Data_Segments[which(as.numeric(df_Segments[i, 1]) == Data_Segments$`Segment ID`), "Point IDs"]
          }
          names(df_Segments)[1:3] <- c("Segment ID", "Color", "Point IDs")
        }
      }

      if (!is.null(KMT_Analysis) && KMT_Analysis == FALSE && !is.null(Data) && "KMT_ID" %in% colnames(Data) && "I_class" %in% colnames(Data)) {
        df_Data <- subset(Data, subset = `KMT_ID` %in% df_Segments$`Segment ID`)
        df_Segments <- select(df_Segments, all_of(c("Segment ID", "Point IDs")))

        for (i in 1:nrow(df_Segments)) {
          if (df_Segments[i, "Segment ID"] %in% as.list(df_Data["Interactor_ID"])[[1]]) {
            df_Segments[i, 3] <- "#FF7A7A" # KMT-SMT lateral interaction == light red
          } else if (df_Segments[i, "Segment ID"] %in% as.list(df_Data["KMT_ID"])[[1]]) {
            df_Segments[i, 3] <- "#FD7BFD" # KMT-KMT lateral interaction == purple
          } else {
            df_Segments[i, 3] <- "#8F8F8F" # KMT-KMT lateral interaction == purple
          }
        }
        names(df_Segments)[3] <- "Color"
        df_Data <- df_Data %>%
          select(all_of(c("Interactor_ID", "I_class"))) %>%
          filter_at(vars(starts_with("I_class")), any_vars(. == "SMT"))

        if (nrow(df_Data) > 0) {
          for (i in 1:nrow(df_Data)) {
            df_Data[i, 3] <- Data_Segments[as.numeric(df_Data[i, 1] + 1), "Point IDs"]
          }
          df_Data <- tibble(
            df_Data[1],
            df_Data[3],
            df_Data[2]
          )
          df_Data[3] <- "#FDDC7B"
          names(df_Data)[1:3] <- c("Segment ID", "Point IDs", "Color")
          df_Segments <- rbind(df_Segments, df_Data)
        }
      }

      if (!is.null(KMT_Analysis) && KMT_Analysis == FALSE && !is.null(Data) && "Segments_ID_1" %in% colnames(Data)) {
        # Handle MT-MT interaction data
      }

      progressSweetAlert(
        session = session,
        id = "Load_3D",
        title = "Loading 3D data : ",
        display_pct = TRUE,
        value = 0
      )

      if (VIEW_ALL == TRUE) {
        for (i in 1:nrow(Data_Segments)) {
          updateProgressBar(
            session = session,
            id = "Load_3D",
            value = (i / nrow(Data_Segments)) * 100
          )

          MT <- as.numeric(unlist(strsplit(Data_Segments[i, "Point IDs"], split = ",")))
          MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

          if (Data_Segments[i, 1] %in% df_Segments[, 1]) {
            if ("Color" %in% colnames(df_Segments)) {
              lines3d(MT, col = df_Segments[i, "Color"], alpha = 1)
            } else {
              lines3d(MT, col = KMT_Col, alpha = 1)
            }
          } else {
            lines3d(MT, col = Non_KMT_Col, alpha = 1)
          }
        }
      } else {
        if (nrow(df_Segments) > 0 && input$`Select_fiber` == "All") {
          for (i in 1:nrow(df_Segments)) {
            updateProgressBar(
              session = session,
              id = "Load_3D",
              value = (i / nrow(df_Segments)) * 100
            )

            MT <- as.numeric(unlist(strsplit(as.character(df_Segments[i, "Point IDs"]), split = ",")))
            MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

            if ("Color" %in% colnames(df_Segments)) {
              lines3d(MT, col = df_Segments[i, "Color"], alpha = 1)
            } else {
              lines3d(MT, col = KMT_Col, alpha = 1)
            }
          }
        } else {
          for (i in 1:nrow(df_Segments)) {
            updateProgressBar(
              session = session,
              id = "Load_3D",
              value = (i / nrow(df_Segments)) * 100
            )

            MT <- as.numeric(unlist(strsplit(as.character(df_Segments[i, "Point IDs"]), split = ",")))
            MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]
            MT <- cylinder3d(MT / 10000, radius = 0.01)

            if ("Color" %in% colnames(df_Segments)) {
              shade3d(MT, col = df_Segments[i, "Color"], alpha = 1)
            } else {
              shade3d(MT, col = KMT_Col, alpha = 1)
            }
          }
        }
      }

      scene <- scene3d()
      rgl.close()
      closeSweetAlert(session = session)
      rglwidget(scene, reuse = CASHING)
    })
  }
}
