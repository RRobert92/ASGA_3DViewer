################################################################################
# Shiny Global - Color conversion HEX to RGB and RGB to HEX
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-06
################################################################################

HexToRGB <- function(HEX) {
  HEX <- as.character(strsplit(HEX, split = "#")[[1]][2])

  if (str_length(HEX) == 6) {
    HEX <- strsplit(HEX, "")
    RGB <- tibble(
      R = as.integer(as.hexmode(paste(HEX[[1]][1], HEX[[1]][2], sep = ""))),
      G = as.integer(as.hexmode(paste(HEX[[1]][3], HEX[[1]][4], sep = ""))),
      B = as.integer(as.hexmode(paste(HEX[[1]][5], HEX[[1]][6], sep = "")))
    )
    return(RGB)
  } else {
    return(NULL)
  }
}

RGBtoHEX <- function(Col) {
  if (ncol(Col) == 3) {
    HEX <- tibble(
      R = as.character(as.hexmode(as.integer(Col[1]))),
      G = as.character(as.hexmode(as.integer(Col[2]))),
      B = as.character(as.hexmode(as.integer(Col[3])))
    )
    HEX <- paste("#", HEX[1], HEX[2], HEX[3], sep = "")
    return(HEX)
  } else {
    return(NULL)
  }
}
