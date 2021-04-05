################################################################################
# Shiny Global - Load 3D data module
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-04
################################################################################

Load_Data <- function(id, No_Data){
  if(id == "Demo"){
    Data_Points_1_Demo <<- readRDS("demo/Data_Points_1.RDS")
    Data_Segments_1_Demo <<- readRDS("demo/Data_Segments_1.RDS")
  } else {
    Amira_df <<- as_tibble(readLines(paste(id, "/",list.files(id)[No_Data], sep = "")))
    names(Amira_df)[1] <<- "X1"


    Data_Points <<- Load_Amira_Points()
    Data_Nodes <<- Load_Amira_Nodes()
    Data_Segments <<- Load_Amira_Segments()

    rm(Amira_df, envir = .GlobalEnv)
  }
}
