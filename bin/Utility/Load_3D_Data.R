################################################################################
# Shiny Global - Load 3D data module
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-04
################################################################################

Load_Data <- function(id, No_Data) {
  if (id == "Demo") {
    Data_Points_1_Demo <<- readRDS("demo/Data_Points_1.RDS")
    Data_Segments_1_Demo <<- readRDS("demo/Data_Segments_1.RDS")
  } else {
    Data_Points <<- readRDS(paste(paste(id, "/", Publication_Name_1_Names[No_Data], "_Points.RDS", sep = "")))
    Data_Nodes <<- readRDS(paste(paste(id, "/", Publication_Name_1_Names[No_Data], "_Nodes.RDS", sep = "")))
    Data_Segments <<- readRDS(paste(paste(id, "/", Publication_Name_1_Names[No_Data], "_Segments.RDS", sep = "")))
  }
}
