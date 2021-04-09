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
      rgl.light(
        diffuse = "gray75",
        specular = "gray75", viewpoint.rel = FALSE
      )

      if (input$`Select_fiber` != "All") {
        df_Segments <- Data_Segments %>% filter_at(vars(starts_with(input$`Select_fiber`)), any_vars(. >= 1))
        df_Segments <- df_Segments %>% select("Segment ID", starts_with(input$`Select_fiber`), "Point IDs")
      } else {
        df_Segments <- Data_Segments %>% filter_at(vars(starts_with("Pole")), any_vars(. >= 1))
        df_Segments <- df_Segments %>% select("Segment ID", starts_with("Pole"), "Point IDs")
      }

      if(!is.null(KMT_Analysis) && !is.null(Data)){
        df_Data <- subset(Data, subset = `Segment ID` %in% df_Segments$`Segment ID`)
        df_Segments <- subset(df_Segments, subset = `Segment ID` %in% Data$`Segment ID`)

        Palette <- Creat_Palette(nrow(df_Segments), KMT_Col)

        df_Segments <- cbind(df_Segments, df_Data[2])
        df_Segments <- df_Segments[order(df_Segments$Data),]
        df_Segments <- cbind(df_Segments$`Segment ID`, df_Segments$`Point IDs`, df_Segments$Data, Palette[1])
        names(df_Segments)[1:4] <- c("Segment ID", "Point IDs", "Data", "Color")
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
            if("Color" %in% colnames(df_Segments)){
              lines3d(MT, col = df_Segments[i, "Color"], alpha = 1)
            } else {
              lines3d(MT, col = KMT_Col, alpha = 1)
            }
          } else {
            lines3d(MT, col = Non_KMT_Col, alpha = 1)
          }
        }
      } else {
        if (nrow(df_Segments) > 0) {
          for (i in 1:nrow(df_Segments)) {
            updateProgressBar(
              session = session,
              id = "Load_3D",
              value = (i / nrow(df_Segments)) * 100
            )

            MT <- as.numeric(unlist(strsplit(df_Segments[i, "Point IDs"], split = ",")))
            MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

            if("Color" %in% colnames(df_Segments)){
              lines3d(MT, col = df_Segments[i, "Color"], alpha = 1)
            } else {
              lines3d(MT, col = KMT_Col, alpha = 1)
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
