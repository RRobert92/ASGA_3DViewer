################################################################################
# Shiny Global - Search Environment
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-05
################################################################################

Search_for_Data <- function() {
  Publication_Name <<- list.files("./Data")
  Publication_No <<- length(Publication_Name)

  if (Publication_No != 0) {
    for (i in 1:Publication_No) {
      # List of data sets
      df <- str_split(list.files(paste("./Data/", list.files("./Data")[i], "/Raw", sep = "")), "_")
      df_df <- tibble()
      for (j in 1:length(df)) {
        df_df[j, 1] <- df[[j]][1]
        df_df[j, 2] <- df[[j]][2]
      }
      df_df <- unique(df_df)
      df <- c()
      for (j in 1:nrow(df_df)) {
        df <- c(df, paste(df_df[j, 1], df_df[j, 2], sep = "_"))
      }
      assign(paste(Publication_Name[i], "Names", sep = "_"),
        df,
        envir = .GlobalEnv
      )
      assign(paste(Publication_Name[i], "No", sep = "_"),
        length(get(paste(Publication_Name[i], "Names", sep = "_"))),
        envir = .GlobalEnv
      )
    }
  }
}

Analysis_List <- function(Pub, Data) {
  AVAILABLE_ANALYSIS_KMTs <<- c("NaN")
  AVAILABLE_ANALYSIS_ALL <<- c("NaN")

  if(Pub == "Demo" && Data == "Demo"){
    AVAILABLE_ANALYSIS_ALL <<- c(
      AVAILABLE_ANALYSIS_ALL,
      "Demo"
    )
    AVAILABLE_ANALYSIS_KMTs <<- c(
      AVAILABLE_ANALYSIS_KMTs,
      "Demo"
    )
  }

  if (length(list.files(paste("./Data/", list.files("./Data")[Pub], "/Analysis", sep = ""))) > 0) {
    # List of Analysis
    assign(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_"),
      list.files(paste("./Data/", list.files("./Data")[Pub], "/Analysis", sep = "")),
      envir = .GlobalEnv
    )
    if (length(get(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_"))) == 0) {
      assign(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_"),
        NULL,
        envir = .GlobalEnv
      )
    }

    if (!is.null(get(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_")))) {
      # List of analysis for all MTs
      if (sum(str_detect(get(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_")), "MT_Interaction") == TRUE) == 5) {
        AVAILABLE_ANALYSIS_ALL <<- c(
          AVAILABLE_ANALYSIS_ALL,
          "MT-MT interactions for 25nm",
          "MT-MT interactions for 30nm",
          "MT-MT interactions for 35nm",
          "MT-MT interactions for 45nm",
          "MT-MT interactions for 50nm"
        )
      }

      # List of analysis for KMTs
      if (sum(str_detect(get(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_")), "LD") == TRUE) != 0) {
        AVAILABLE_ANALYSIS_KMTs <<- c(
          AVAILABLE_ANALYSIS_KMTs,
          "Length Distribution"
        )
      }
      if (sum(str_detect(get(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_")), "KMT_Total_Curv") == TRUE) != 0) {
        AVAILABLE_ANALYSIS_KMTs <<- c(
          AVAILABLE_ANALYSIS_KMTs,
          "KMTs Curvature"
        )
      }
      if (sum(str_detect(get(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_")), "KMT_Total_Curv") == TRUE) != 0) {
        AVAILABLE_ANALYSIS_KMTs <<- c(
          AVAILABLE_ANALYSIS_KMTs,
          "KMTs Curvature"
        )
      }
      if (sum(str_detect(get(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_")), "K_Core_Area") == TRUE) != 0) {
        AVAILABLE_ANALYSIS_KMTs <<- c(
          AVAILABLE_ANALYSIS_KMTs,
          "No. of KMTs at a Pole"
        )
        if (sum(str_detect(get(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_")), "KMT_Pole") == TRUE) != 0) {
          AVAILABLE_ANALYSIS_KMTs <<- c(
            AVAILABLE_ANALYSIS_KMTs,
            "No. of KMTs"
          )
        }
      }
      if (sum(str_detect(get(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_")), "KMT_Minus_End") == TRUE) == 7) {
        AVAILABLE_ANALYSIS_KMTs <<- c(
          AVAILABLE_ANALYSIS_KMTs,
          "KMT minus-ends interaction for 25 nm",
          "KMT minus-ends interaction for 30 nm",
          "KMT minus-ends interaction for 35 nm",
          "KMT minus-ends interaction for 45 nm",
          "KMT minus-ends interaction for 50 nm",
          "KMT minus-ends interaction for 75 nm",
          "KMT minus-ends interaction for 100 nm"
        )
      }
      if (sum(str_detect(get(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_")), "KMTs_minus_seed") == TRUE) == 7) {
        AVAILABLE_ANALYSIS_KMTs <<- c(
          AVAILABLE_ANALYSIS_KMTs,
          "KMT lattice interaction for 25 nm",
          "KMT lattice interaction for 30 nm",
          "KMT lattice interaction for 35 nm",
          "KMT lattice interaction for 45 nm",
          "KMT lattice interaction for 50 nm",
          "KMT lattice interaction for 75 nm",
          "KMT lattice interaction for 100 nm"
        )
      }
    }
  }
}
