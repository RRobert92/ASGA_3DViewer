################################################################################
# Shiny Server - Load Amira Data module
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-09-22
# Reviewed: Robert Kiewisz 28/08/2020 (v0.32.1)
################################################################################

Load_Amira_Nodes <- function() {

  # Build Nodes data.table (ID, X,Y,Z) -------------------------------------------
  No_column <- Amira_df %>%
    filter(str_detect(X1, "VERTEX")) %>%
    filter(str_detect(X1, "@"))
  No_column <- No_column %>% separate(X1, c("V1", "V2", "V3", "V4", "V5", "V6"), sep = " ")

  No_column_Node <- No_column[6] %>% separate(V6, c("V1", "V2"), sep = "@")

  Nodes <- tibble(0:as.numeric(as.numeric(gsub("[^[:digit:]]", "", Amira_df %>%
                                                 filter(str_detect(X1, "VERTEX")) %>%
                                                 filter(str_detect(X1, "define")))) - 1))

  names(Nodes)[1] <- "Node ID"

  for (i in 1:nrow(No_column)) {
    Pattern <- as.vector(paste("@", No_column_Node[i, 2], sep = ""))

    Nodes_number <- which(Amira_df == Pattern)
    test_last <- length(which(Amira_df == as.vector(paste("@",
                                                          as.numeric(No_column_Node[i, 2]) + 1,
                                                          sep = ""
    ))))

    if (test_last == 1) {
      df <- Amira_df[as.numeric(Nodes_number + 1):as.numeric(which(Amira_df == as.vector(paste("@",
                                                                                               as.numeric(No_column_Node[i, 2]) + 1,
                                                                                               sep = ""
      ))) - 2), ]
    } else {
      df <- Amira_df[as.numeric(Nodes_number + 1):nrow(Amira_df), ]
    }

    id <- as.numeric(gsub("[^[:digit:]]", "", No_column[i, 3]))

    if (!is.na(id) && id == 3) {
      df <- as_tibble(df %>% separate(X1, c("X Coord", "Y Coord", "Z Coord"), sep = " ", extra = "drop"))
      df <- transform(df,
                      `X Coord` = as.numeric(`X Coord`),
                      `Y Coord` = as.numeric(`Y Coord`),
                      `Z Coord` = as.numeric(`Z Coord`)
      )

      names(df)[1:3] <- c("X Coord", "Y Coord", "Z Coord")
    } else {
      df <- as_tibble(df %>% separate(X1, "V1", sep = " ", extra = "drop"))
      df <- transform(df,
                      `V1` = as.numeric(`V1`)
      )

      names(df)[1] <- No_column[i, 4]
    }

    Nodes <- cbind(Nodes, df)
  }

  rm(id, Nodes_number, Pattern, No_column, No_column_Node, df, test_last)

  Nodes
}

Load_Amira_Points <- function() {
  # Build Points data.table (ID, X,Y,Z) -----------------------------------------
  No_column <- Amira_df %>%
    filter(str_detect(X1, "POINT")) %>%
    filter(str_detect(X1, "@"))
  No_column <- No_column %>% separate(X1, c("V1", "V2", "V3", "V4", "V5", "V6"), sep = " ")

  No_column_Points <- No_column[6] %>% separate(V6, c("V1", "V2"), sep = "@")

  Points <- tibble(0:as.numeric(as.numeric(gsub("[^[:digit:]]", "", Amira_df %>%
                                                  filter(str_detect(X1, "POINT")) %>%
                                                  filter(str_detect(X1, "define")))) - 1))

  names(Points)[1] <- "Point ID"

  for (i in 1:nrow(No_column)) {
    Pattern <- as.vector(paste("@", No_column_Points[i, 2], sep = ""))
    Points_number <- which(Amira_df == Pattern)

    last_column <- as.numeric(which(Amira_df == as.vector(paste("@",
                                                                as.numeric(No_column_Points[i, 2]) + 1,
                                                                sep = ""
    ))) - 2)

    if (last_column == nrow(Amira_df) || length(last_column) == 0) {
      last_column <- nrow(Amira_df) - 1
    }

    df <- Amira_df[as.numeric(Points_number + 1):last_column, ]

    id <- as.numeric(gsub("[^[:digit:]]", "", No_column[i, 3]))

    if (!is.na(id) && id == 3) {
      df <- as_tibble(df %>% separate(X1, c("X Coord", "Y Coord", "Z Coord"), sep = " ", extra = "drop"))
      df <- transform(df,
                      `X Coord` = as.numeric(`X Coord`),
                      `Y Coord` = as.numeric(`Y Coord`),
                      `Z Coord` = as.numeric(`Z Coord`)
      )

      names(df)[1:3] <- c("X Coord", "Y Coord", "Z Coord")
    } else {
      df <- as_tibble(df %>% separate(X1, "V1", sep = " ", extra = "drop"))
      df <- transform(df, `V1` = as.numeric(`V1`))

      names(df)[1] <- No_column[i, 4]
    }

    if(nrow(df) == as.numeric(gsub("[^[:digit:]]", "", Amira_df[6,1]))){
      Points <- cbind(Points, df)
    }
    if (as.numeric(nrow(df) + 1) == as.numeric(gsub("[^[:digit:]]", "", Amira_df[6,1]))){
      df[nrow(df)+1,] <- Amira_df[last_column + 1, ]
    }
  }

  rm(id, Points_number, Pattern, No_column, No_column_Points, df, last_column)

  Points
}

Load_Amira_Segments <- function() {
  # Build Segment data.table (ID, length, node #1, node #2, points) -----------------------------------------
  No_column <- Amira_df %>%
    filter(str_detect(X1, "EDGE")) %>%
    filter(str_detect(X1, "@"))
  No_column <- No_column %>% separate(X1, c("V1", "V2", "V3", "V4", "V5", "V6"), sep = " ")
  No_column <- No_column[order(No_column$V4),]

  No_column_Segments <- No_column[6] %>% separate(V6, c("V1", "V2"), sep = "@")

  Segments <- tibble(0:as.numeric(as.numeric(gsub("[^[:digit:]]", "", Amira_df %>%
                                                    filter(str_detect(X1, "EDGE")) %>%
                                                    filter(str_detect(X1, "define")))) - 1))

  names(Segments)[1] <- "Segment ID"

  for (i in 1:nrow(No_column)) {
    Pattern <- as.vector(paste("@", No_column_Segments[i, 2], sep = ""))
    Segments_number <- which(Amira_df == Pattern)

    last_column <- as.numeric(which(Amira_df == as.vector(paste("@",
                                                                as.numeric(No_column_Segments[i, 2]) + 1,
                                                                sep = ""
    ))) - 2)

    if (last_column == nrow(Amira_df) || length(last_column) == 0) {
      last_column <- nrow(Amira_df) - 1
    }

    df <- Amira_df[as.numeric(Segments_number + 1):last_column, ]

    if(nrow(df) != nrow(Segments)){
      df <- Amira_df[as.numeric(Segments_number + 1):nrow(Amira_df), ]
    }

    if(nrow(df) != nrow(Segments)){
      if(Amira_df[as.numeric(last_column + 2),1] == as.vector(paste("@", as.numeric(No_column_Segments[i, 2]) + 1, sep = ""))){
        df <- Amira_df[as.numeric(Segments_number + 1):as.numeric(last_column + 1), ]
      }
    }

    if(nrow(df) != nrow(Segments)) next

    id <- as.numeric(gsub("[^[:digit:]]", "", No_column[i, 3]))

    if (!is.na(id) && id == 2) {
      df <- as_tibble(df %>% separate(X1, c("V1", "V2"), sep = " ", extra = "drop"))
      df <- transform(df,
                      `V1` = as.numeric(`V1`),
                      `V2` = as.numeric(`V2`)
      )

      names(df)[1:2] <- c("Node ID #1", "Node ID #2")
    } else {
      df <- as_tibble(df %>% separate(X1, "V1", sep = " ", extra = "drop"))
      df <- transform(df,
                      `V1` = as.numeric(`V1`)
      )

      names(df)[1] <- No_column[i, 4]
    }

    Segments <- cbind(Segments, df)
  }

  df <- Segments$NumEdgePoints

  df_points <- tibble()
  j <- 1
  start_id <- 0

  for (end_id in df) {
    df_points[j, 1] <- paste(c(start_id:as.numeric((end_id - 1) + start_id)), collapse = ",")
    start_id <- end_id + start_id
    j <- j + 1
  }

  names(df_points)[1] <- "Point IDs"

  Segments$NumEdgePoints <- df_points

  df <- Segments %>% select(starts_with("Pole"))

  Segments <- cbind(
    "Segment ID" = Segments$`Segment ID`,
    df,
    "length" = Segments$length,
    "Node ID #1" = Segments$`Node ID #1`,
    "Node ID #2" = Segments$`Node ID #2`,
    "Point IDs" = Segments$NumEdgePoints
  )

  rm(id, Segments_number, Pattern, No_column, No_column_Segments, df, last_column, start_id, end_id, df_points)

  Segments
}
