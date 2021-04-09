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

      R <- as.character(as.hexmode(as.integer(Col[1])))
      if(nchar(R) != 2){
        R <- paste("0", R, sep = "")
      }

      G <- as.character(as.hexmode(as.integer(Col[2])))
      if(nchar(G) != 2){
        G <- paste("0", G, sep = "")
      }

      B <- as.character(as.hexmode(as.integer(Col[3])))
      if(nchar(B) != 2){
        B <- paste("0", B, sep = "")
      }

    HEX <- paste("#", R, G, B, sep = "")
    return(HEX)
  } else {
    return(NULL)
  }
}

Creat_Palette <- function(Range, HEX) {
  RGB <- HexToRGB(HEX)[1:Range, ]
  MAX_1 <- RGB[as.numeric(which.max(RGB[1, ]))]
  RGB <- subset(RGB, select = -c(as.numeric(which.max(RGB[1, ]))))

  MAX_2 <- RGB[as.numeric(which.max(RGB[1, ]))]
  RGB <- subset(RGB, select = -c(as.numeric(which.max(RGB[1, ]))))

  MAX_3 <- RGB[as.numeric(which.max(RGB[1, ]))]
  RGB <- subset(RGB, select = -c(as.numeric(which.max(RGB[1, ]))))

  if (MAX_1[1, 1] == MAX_2[1, 1] && MAX_1[1, 1] == MAX_3[1, 1]) {
    RGB <- data.frame(
      data.frame(round(seq(as.numeric(MAX_1[1, 1]), 255, length.out = Range), 0)),
      data.frame(round(seq(as.numeric(MAX_1[1, 1]), 255, length.out = Range), 0)),
      data.frame(round(seq(as.numeric(MAX_1[1, 1]), 255, length.out = Range), 0))
    )
    names(RGB)[1:3] <- c("R", "G", "B")
  } else if (MAX_1[1, 1] == MAX_2[1, 1]) {
    ismax <- as.logical(MAX_2[1, 1] > MAX_1[1, 1])
    if(ismax == TRUE){
    MAX_1[1:Range, 1] <- MAX_1[1, 1]
    MAX_1_Name <- colnames(MAX_1)
    MAX_2[1:Range, 1] <- MAX_2[1, 1]
    MAX_2_Name <- colnames(MAX_2)
    MAX_3[1:Range, 1] <- data.frame(round(seq(as.numeric(MAX_3[1, 1]), 255, length.out = Range), 0))
    MAX_3_Name <- colnames(MAX_3)
    } else {
      MAX_1[1:Range, 1] <- data.frame(round(seq(as.numeric(MAX_1[1, 1]), 255, length.out = Range), 0))
      MAX_1_Name <- colnames(MAX_1)
      MAX_2[1:Range, 1] <- data.frame(round(seq(as.numeric(MAX_2[1, 1]), 255, length.out = Range), 0))
      MAX_2_Name <- colnames(MAX_2)
      MAX_3[1:Range, 1] <- MAX_3[1, 1]
      MAX_3_Name <- colnames(MAX_3)
    }

  } else if (MAX_2[1, 1] == MAX_3[1, 1]) {
    ismax <- as.logical(MAX_2[1, 1] > MAX_1[1, 1])
    if(ismax == TRUE){
      MAX_1[1:Range, 1] <- data.frame(round(seq(as.numeric(MAX_2[1, 1]), 255, length.out = Range), 0))
      MAX_1_Name <- colnames(MAX_1)
      MAX_2[1:Range, 1] <- MAX_2[1, 1]
      MAX_2_Name <- colnames(MAX_2)
      MAX_3[1:Range, 1] <- MAX_3[1, 1]
      MAX_3_Name <- colnames(MAX_3)
    } else {
      MAX_1[1:Range, 1] <- MAX_1[1, 1]
      MAX_1_Name <- colnames(MAX_1)
      MAX_2[1:Range, 1] <- data.frame(round(seq(as.numeric(MAX_2[1, 1]), 255, length.out = Range), 0))
      MAX_2_Name <- colnames(MAX_2)
      MAX_3[1:Range, 1] <- data.frame(round(seq(as.numeric(MAX_3[1, 1]), 255, length.out = Range), 0))
      MAX_3_Name <- colnames(MAX_3)
    }

  } else if (MAX_1[1, 1] == MAX_3[1, 1]) {
    ismax <- as.logical(MAX_2[1, 1] > MAX_1[1, 1])
    if(ismax == TRUE){
    MAX_1[1:Range, 1] <- MAX_1[1, 1]
    MAX_1_Name <- colnames(MAX_1)
    MAX_2[1:Range, 1] <- data.frame(round(seq(as.numeric(MAX_2[1, 1]), 255, length.out = Range), 0))
    MAX_2_Name <- colnames(MAX_2)
    MAX_3[1:Range, 1] <- MAX_3[1, 1]
    MAX_3_Name <- colnames(MAX_3)

    } else {
      MAX_1[1:Range, 1] <- data.frame(round(seq(as.numeric(MAX_1[1, 1]), 255, length.out = Range), 0))
      MAX_1_Name <- colnames(MAX_1)
      MAX_2[1:Range, 1] <- MAX_2[1, 1]
      MAX_2_Name <- colnames(MAX_2)
      MAX_3[1:Range, 1] <- data.frame(round(seq(as.numeric(MAX_3[1, 1]), 255, length.out = Range), 0))
      MAX_3_Name <- colnames(MAX_3)
    }
  } else {
    MAX_1[1:Range, 1] <- MAX_1[1, 1]
    MAX_1_Name <- colnames(MAX_1)
    MAX_2[1:Range, 1] <- data.frame(round(seq(as.numeric(MAX_2[1, 1]), as.numeric(MAX_1[1, 1]), length.out = Range), 0))
    MAX_2_Name <- colnames(MAX_2)
    MAX_3[1:Range, 1] <- data.frame(round(seq(as.numeric(MAX_3[1, 1]), as.numeric(MAX_1[1, 1]), length.out = Range), 0))
    MAX_3_Name <- colnames(MAX_3)
  }

  RGB <- data.frame(
    get(paste("MAX_", which(c(MAX_1_Name, MAX_2_Name, MAX_3_Name) == "R"), sep = "")),
    get(paste("MAX_", which(c(MAX_1_Name, MAX_2_Name, MAX_3_Name) == "G"), sep = "")),
    get(paste("MAX_", which(c(MAX_1_Name, MAX_2_Name, MAX_3_Name) == "B"), sep = ""))
  )

  df_col <- data.frame()
  for (i in 1:nrow(RGB)) {
    df_col[i, 1] <- RGBtoHEX(RGB[i, 1:3])
  }
  names(df_col)[1] <- "Color"

  return(df_col)
}
