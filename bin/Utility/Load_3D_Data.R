################################################################################
# Shiny Global - Load 3D data module
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-04
################################################################################

Load_Data <- function(id){
  if(id == "Demo"){
    Data_Points_1_Demo <<- readRDS("demo/Data_Points_1")
    Data_Segments_1_Demo <<- readRDS("demo/Data_Segments_1")
  } else {

  }


}
