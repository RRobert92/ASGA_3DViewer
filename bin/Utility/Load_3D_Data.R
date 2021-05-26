################################################################################
# Shiny Global - Load 3D data module
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-04
################################################################################

Load_Data_Points <- function(id, No_Pub, No_Data) {
    return(
      readRDS(
        paste(id, "/",
              get(paste(Publication_Name[No_Pub], "Names", sep = "_"))[No_Data],
              "_Points.RDS", sep = "")
        )
      )
}

Load_Data_Segments <- function(id, No_Pub, No_Data) {
  return(
    readRDS(
      paste(id, "/",
            get(paste(Publication_Name[No_Pub], "Names", sep = "_"))[No_Data],
            "_Segments.RDS", sep = "")
      )
    )
}

Load_Data_Nodes <- function(id, No_Pub, No_Data) {
  return(
    readRDS(
      paste(id, "/",
            get(paste(Publication_Name[No_Pub], "Names", sep = "_"))[No_Data],
            "_Nodes.RDS", sep = "")
    )
  )
}
