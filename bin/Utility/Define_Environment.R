################################################################################
# Shiny Global - Search Environment
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Define variables that are used in all sessions by the users
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
      if (length(df) > 0) {
        df_df <- tibble()
        for (j in 1:length(df)) {
          df_df[j, 1] <- df[[j]][1]
          df_df[j, 2] <- df[[j]][2]
        }

        df_df <- unique(df_df)
        df <- NULL
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
      } else {
        assign(paste(Publication_Name[i], "Names", sep = "_"),
          NULL,
          envir = .GlobalEnv
        )
        assign(paste(Publication_Name[i], "No", sep = "_"),
          0L,
          envir = .GlobalEnv
        )
      }
    }
  }
}

Analysis_List_All <- function(Pub, Data) {
  if (length(list.files(paste("./Data/", list.files("./Data")[Pub], "/Analysis", sep = ""))) > 0) {
    # List of Analysis
    assign(paste(Publication_Name[Pub], "Analysis_Names", sep = "_"),
      list.files(paste("./Data/", list.files("./Data")[Pub], "/Analysis", sep = "")),
      envir = .GlobalEnv
    )
    if (length(get(paste(Publication_Name[Pub], "Analysis_Names", sep = "_"))) == 0) {
      assign(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_"),
        NULL,
        envir = .GlobalEnv
      )
    }

    if (!is.null(get(paste(Publication_Name[Pub], "Analysis_Names", sep = "_")))) {
      if (sum(str_detect(
        get(paste(Publication_Name[Pub], "Analysis_Names", sep = "_")),
        paste("Data_", Data, "_MT_Interaction", sep = "")
      ) == TRUE) == 5) {
        return(
          c(
            "NaN",
            "MT-MT interactions for 25nm",
            "MT-MT interactions for 30nm",
            "MT-MT interactions for 35nm",
            "MT-MT interactions for 45nm",
            "MT-MT interactions for 50nm"
          )
        )
      } else {
        return(
          c(
            "NaN"
          )
        )
      }
    }
  }
}

Analysis_List_KMTs <- function(Pub, Data) {
  if (length(list.files(paste("./Data/", list.files("./Data")[Pub], "/Analysis", sep = ""))) > 0) {

    # List of Analysis
    assign(paste(Publication_Name[Pub], "Analysis_Names", sep = "_"),
      list.files(paste("./Data/", list.files("./Data")[Pub], "/Analysis", sep = "")),
      envir = .GlobalEnv
    )
    if (length(get(paste(Publication_Name[Pub], "Analysis_Names", sep = "_"))) == 0) {
      assign(paste(Publication_Name[Pub], "Analysis_Names", Data, sep = "_"),
        NULL,
        envir = .GlobalEnv
      )
    }

    df_list_of_analysis <- c("NaN")
    if (!is.null(get(paste(Publication_Name[Pub], "Analysis_Names", sep = "_")))) {
      # List of analysis for KMTs
      if (sum(str_detect(
        get(paste(Publication_Name[Pub], "Analysis_Names", sep = "_")),
        paste("Data_", Data, "_LD", sep = "")
      ) == TRUE) != 0) {
        df_list_of_analysis <- c(
          df_list_of_analysis,
          "Length Distribution",
          "Minus-ends Position"
        )
      }
      if (sum(str_detect(
        get(paste(Publication_Name[Pub], "Analysis_Names", sep = "_")),
        paste("Data_", Data, "_KMT_Total_Curv", sep = "")
      ) == TRUE) != 0) {
        df_list_of_analysis <- c(
          df_list_of_analysis,
          "KMTs Curvature"
        )
      }
      if (sum(str_detect(
        get(paste(Publication_Name[Pub], "Analysis_Names", sep = "_")),
        paste("Data_", Data, "_K_Core_Area", sep = "")
      ) == TRUE) != 0) {
        df_list_of_analysis <- c(
          df_list_of_analysis,
          "No. of KMTs at a Pole"
        )
      }
      if (sum(str_detect(
        get(paste(Publication_Name[Pub], "Analysis_Names", sep = "_")),
        paste("Data_", Data, "_KMT_Pole", sep = "")
      ) == TRUE) != 0) {
        df_list_of_analysis <- c(
          df_list_of_analysis,
          "No. of KMTs"
        )
      }
      if (sum(str_detect(
        get(paste(Publication_Name[Pub], "Analysis_Names", sep = "_")),
        paste("Data_", Data, "_KMT_Minus_End", sep = "")
      ) == TRUE) == 7) {
        df_list_of_analysis <- c(
          df_list_of_analysis,
          "KMT minus-ends interaction for 25nm",
          "KMT minus-ends interaction for 30nm",
          "KMT minus-ends interaction for 35nm",
          "KMT minus-ends interaction for 45nm",
          "KMT minus-ends interaction for 50nm",
          "KMT minus-ends interaction for 75nm",
          "KMT minus-ends interaction for 100nm"
        )
      }
      if (sum(str_detect(
        get(paste(Publication_Name[Pub], "Analysis_Names", sep = "_")),
        paste("Data_", Data, "_KMTs_minus_seed", sep = "")
      ) == TRUE) == 7) {
        df_list_of_analysis <- c(
          df_list_of_analysis,
          "KMT lattice interaction for 25nm",
          "KMT lattice interaction for 30nm",
          "KMT lattice interaction for 35nm",
          "KMT lattice interaction for 45nm",
          "KMT lattice interaction for 50nm",
          "KMT lattice interaction for 75nm",
          "KMT lattice interaction for 100nm"
        )
      }
      return(df_list_of_analysis)
    }
  } else {
    df_list_of_analysis <- c("NaN")
    return(df_list_of_analysis)
  }
}

List_of_Kfibers <- function(Data) {
  return(c(
    "All",
    colnames(Data[, grepl("Pole", colnames(Data))])
  ))
}
