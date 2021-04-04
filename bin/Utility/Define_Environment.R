################################################################################
# Shiny Global - Search Environment
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-03
################################################################################

Search_for_Data <- function(){
  Publication_Name <<- list.files("./Data")
  Publication_No <<- length(Publication_Name)
}
