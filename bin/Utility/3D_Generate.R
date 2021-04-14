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
      rgl.light(
        diffuse = "gray75",
        specular = "gray75", viewpoint.rel = FALSE
      )
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

      if (!is.null(KMT_Analysis) && KMT_Analysis == TRUE && !is.null(Data)) {
        df_Data <- subset(Data, subset = `Segment ID` %in% df_Segments$`Segment ID`)
        df_Segments <- subset(df_Segments, subset = `Segment ID` %in% Data$`Segment ID`)

        Palette <- Creat_Palette(
          as.numeric(df_Data[(which.min(df_Data$Data)), "Data"]),
          as.numeric(df_Data[(which.max(df_Data$Data)), "Data"]),
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

      if (!is.null(KMT_Analysis) && KMT_Analysis == FALSE && !is.null(Data)) {
        # how to handle the data when user want to see interactions
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

            MT <- as.numeric(unlist(strsplit(df_Segments[i, "Point IDs"], split = ",")))
            MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

            if ("Color" %in% colnames(df_Segments)) {
              lines3d(MT, col = df_Segments[i, "Color"], alpha = 1)
            } else {
              lines3d(MT, col = KMT_Col, alpha = 1)
            }
          }
        } else {
          # rgl.light(
          #   theta = 0, phi = 0,
          #   ambient = "grey75",
          #   diffuse = "grey75"
          # )

          for (i in 1:nrow(df_Segments)) {
            updateProgressBar(
              session = session,
              id = "Load_3D",
              value = (i / nrow(df_Segments)) * 100
            )

            MT <- as.numeric(unlist(strsplit(df_Segments[i, "Point IDs"], split = ",")))
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
