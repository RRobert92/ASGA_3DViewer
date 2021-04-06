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

      # List of Analysis
      for (j in 1:get(paste(Publication_Name[i], "No", sep = "_"))) {
        # List of Analysis
        assign(paste(Publication_Name[i], "Analysis_Names", j, sep = "_"),
          list.files(paste("./Data/", list.files("./Data")[i], "/Analysis", sep = "")),
          envir = .GlobalEnv
        )
        if (length(get(paste(Publication_Name[i], "Analysis_Names", j, sep = "_"))) == 0) {
          assign(paste(Publication_Name[i], "Analysis_Names", j, sep = "_"),
            NULL,
            envir = .GlobalEnv
          )
        }

        tryCatch(
          {
            assign(paste(Publication_Name[i], "Analysis_No", j, sep = "_"),
              length(get(paste(Publication_Name[i], "Analysis_Names", sep = "_"))),
              envir = .GlobalEnv
            )
          },
          error = function(e) {
            assign(paste(Publication_Name[i], "Analysis_No", j, sep = "_"),
              as.numeric(0),
              envir = .GlobalEnv
            )
          }
        )
      }
    }
  }
}
