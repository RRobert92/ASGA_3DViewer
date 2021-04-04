Demo_Mode <- function(input, output, session) {
  observeEvent(input$`MT_NO`, {
    assign("MT_NO_IMPUT",
      as.numeric(input[["MT_NO"]]),
      envir = .GlobalEnv
    )

    output$`wdg` <- renderRglwidget({
      open3d()
      rgl.bg(color = "black", fogtype = "none")
      rgl.light(
        diffuse = "gray75",
        specular = "gray75", viewpoint.rel = FALSE
      )

      for (i in 1:MT_NO_IMPUT) {
        MT <- as.numeric(unlist(strsplit(Data_Segments_1_Demo[i, "Point IDs"], split = ",")))
        MT <- Data_Points_1_Demo[as.numeric(MT[which.min(MT)] + 1):as.numeric(MT[which.max(MT)] + 1), 2:4]
        # MT <- cylinder3d(MT/10000, radius=0.01)
        if (length(Data_Segments_1_Demo[i, 2:94][Data_Segments_1_Demo[i, 2:94] == TRUE]) == 1) {
          # shade3d(MT, col="red")
          lines3d(MT, col = "red")
        } else {
          # shade3d(MT, col="white")
          lines3d(MT, col = "white", alpha = 1)
        }
      }
      scene <- scene3d()
      rgl.close()

      rglwidget(scene, reuse = TRUE)
    })
  })
}
