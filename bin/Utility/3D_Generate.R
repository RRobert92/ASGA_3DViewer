################################################################################
# Shiny Server - 3D view generator module
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-06
################################################################################

Demo_Mode <- function(input, output, session) {
  output$`wdg` <- renderRglwidget({
    open3d()
    rgl.bg(color = "black", fogtype = "none")
    rgl.light(
      diffuse = "gray75",
      specular = "gray75", viewpoint.rel = FALSE
    )

    for (i in 1:100) {
      MT <- as.numeric(unlist(strsplit(Data_Segments_1_Demo[i, "Point IDs"], split = ",")))
      MT <- Data_Points_1_Demo[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]
      # MT <- cylinder3d(MT/10000, radius=0.01)
      if (length(Data_Segments_1_Demo[i, 2:94][Data_Segments_1_Demo[i, 2:94] == TRUE]) == 1) {
        # shade3d(MT, col="red")
        lines3d(MT, col = KMT_Col)
      } else {
        # shade3d(MT, col="white")
        lines3d(MT, col = Non_KMT_Col, alpha = 1)
      }
    }
    scene <- scene3d()
    rgl.close()

    rglwidget(scene, reuse = FALSE)
  })
}

`3D_Generate` <- function(input, output, session) {

if(`3D_View_Set` == "All MTs"){
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
      updateProgressBar(session = session,
                        id = "Load_3D",
                        value = (i/nrow(Data_Segments))*100)
      #Sys.sleep(0.1)
      MT <- as.numeric(unlist(strsplit(Data_Segments[i, "Point IDs"], split = ",")))
      MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

      lines3d(MT, col = Non_KMT_Col, alpha = 1)
    }

    scene <- scene3d()
    rgl.close()
    closeSweetAlert(session = session)
    rglwidget(scene, reuse = FALSE)
  })
}
  if(`3D_View_Set` == "KMTs"){
    output$`wdg` <- renderRglwidget({
      open3d()
      rgl.bg(color = "black", fogtype = "none")
      rgl.light(
        diffuse = "gray75",
        specular = "gray75", viewpoint.rel = FALSE
      )

      df_Segments <- Data_Segments %>% filter_at(vars(starts_with("Pole")), any_vars(. >= 1))

      progressSweetAlert(
        session = session,
        id = "Load_3D",
        title = "Loading 3D data : ",
        display_pct = TRUE,
        value = 0
      )

      if(VIEW_ALL == TRUE){
        for (i in 1:nrow(Data_Segments)) {
          updateProgressBar(session = session,
                            id = "Load_3D",
                            value = (i/nrow(Data_Segments))*100)

          MT <- as.numeric(unlist(strsplit(Data_Segments[i, "Point IDs"], split = ",")))
          MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

          if(Data_Segments[i,1] %in% df_Segments[,1]){
            lines3d(MT, col = KMT_Col, alpha = 1)
          } else {
            lines3d(MT, col = Non_KMT_Col, alpha = 1)
          }
        }
      } else {
        for (i in 1:nrow(df_Segments)) {
          updateProgressBar(session = session,
                            id = "Load_3D",
                            value = (i/nrow(df_Segments))*100)

          MT <- as.numeric(unlist(strsplit(df_Segments[i, "Point IDs"], split = ",")))
          MT <- Data_Points[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]

          lines3d(MT, col = KMT_Col, alpha = 1)
        }
      }

      scene <- scene3d()
      rgl.close()
      closeSweetAlert(session = session)
      rglwidget(scene, reuse = FALSE)
    })
  }
}
