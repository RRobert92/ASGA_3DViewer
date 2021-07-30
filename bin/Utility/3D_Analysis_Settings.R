################################################################################
# Shiny Server - Collect data and standardize output
#
# (c) 2021 Müller-Reichert Lab & Robert Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Collect_Analysis - return single output as a variable from the function
#
# New set of function that will define environment variable for rgl legend
#
# Author: Robert Kiewisz
# Created: 2021-04-07
################################################################################

Transform_Data <- function(Data, Data_Segments) {
    ID_List <- tibble()
    for (i in seq_len(nrow(Data))) {
        Name <- strsplit(as.character(Data[i, 1]), split = "_")
        No_KMT <- as.numeric(Name[[1]][3])
        Name <- paste(Name[[1]][1], Name[[1]][2], sep = "_")
        Name <- Data_Segments %>%
                select(all_of(c("Segment ID", Name))) %>%
                filter_at(vars(starts_with(Name)), any_vars(. >= 1))
        Name <- Name[No_KMT, 1]
        ID_List[i, 1] <- Name
    }

    df <- cbind(ID_List, select(Data, "Interactor_ID", "I_class"))
    names(df)[1] <- "KMT_ID"
    return(df)
}

Legend_Setting_ACQ <- function(Analysis) {
    if (Analysis == "Length Distribution" ||
            Analysis == "Minus-ends Position") {
        return(4)
    }
    if (Analysis == "KMTs Curvature") {
        return(3)
    }
    if (Analysis == "No. of KMTs" ||
            Analysis == "No. of KMTs at a Pole") {
        return(0)
    }
}

Legend_Setting_MIN <- function(Analysis) {
    if (Analysis == "Length Distribution" ||
            Analysis == "Minus-ends Position" ||
            Analysis == "No. of KMTs" ||
            Analysis == "No. of KMTs at a Pole") {
        return(0)
    }
    if (Analysis == "KMTs Curvature") {
        return(1)
    }
}

Legend_Setting_MAX <- function(Analysis, Data) {
    if (Analysis == "Length Distribution" ||
            Analysis == "Minus-ends Position" ||
            Analysis == "KMTs Curvature" ||
            Analysis == "No. of KMTs" ||
            Analysis == "No. of KMTs at a Pole") {
        return(as.numeric(Data[(which.max(Data$Data)), "Data"]))
    }
}

Legend_Setting_UNIT <- function(Analysis, MIN, MAX) {
    if (Analysis == "Length Distribution" ||
            Analysis == "Minus-ends Position") {
        return(paste(round(seq(MIN, MAX, length.out = 7), 1), "µm", sep = " "))
    }
    if (Analysis == "KMTs Curvature") {
        return(paste0(round(seq(MIN, MAX, length.out = 7), 2)))
    }
    if (Analysis == "No. of KMTs" ||
            Analysis == "No. of KMTs at a Pole") {
        return(paste(round(seq(MIN, MAX, length.out = 7), 0), "KMTs", sep = " "))
    }
    if (startsWith(Analysis, "KMT minus-ends interaction for")) {
        return(c("KMT without an interaction", "KMT with KMT interaction", "KMT with Non-KMT interaction", "non-KMT interacting with KMT"))
    }
    if (startsWith(Analysis, "KMT lattice interaction for")) {
        return(c("KMT without an interaction", "KMT with an interaction", "KMT with KMT interaction", "non-KMT interacting with KMT"))
    }
    if (startsWith(Analysis, "MT-MT interactions for")) {
        return(c("KMT lattice", "KMT Interaction Region", "MT lattice", "MT Interaction Region"))
    }
}

# Transform data for each selected analysis to create unified output
Collect_Analysis <- function(Data_Segments, Analysis, Pub_ID, Data_ID) {
    # Length Distribution --------------------------------------------------------
    if (Analysis == "Length Distribution") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_LD.xlsx"))
        Data <- select(Data, all_of(c("Fiber_Name", "length")))
        names(Data)[2] <- "Data"

        Fiber_List <- unique(Data$Fiber_Name)
        Segment_df <- data.frame()

        for (i in seq_along(Fiber_List)) {
            Data_df <- select(Data_Segments, all_of(c("Segment ID", Fiber_List[i])))
            Data_df <- Data_df %>% filter_at(vars(starts_with(Fiber_List[i])), any_vars(. >= 1))

            Segment_df <- rbind(Segment_df, Data_df[1])
        }
        Data <- cbind(Segment_df, Data[2])
        names(Data)[1:2] <- c("Segment ID", "Data")

        return(Data)
    }

    # KMTs minus-end position ----------------------------------------------------
    if (Analysis == "Minus-ends Position") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_LD.xlsx"))
        Data <- select(Data, all_of(c("Fiber_Name", "minus_dist_to_pole")))
        names(Data)[2] <- "Data"

        Fiber_List <- unique(Data$Fiber_Name)
        Segment_df <- data.frame()
        for (i in seq_along(Fiber_List)) {
            Data_df <- select(Data_Segments, all_of(c("Segment ID", Fiber_List[i])))
            Data_df <- Data_df %>% filter_at(vars(starts_with(Fiber_List[i])), any_vars(. >= 1))
            Segment_df <- rbind(Segment_df, Data_df[1])
        }
        Data <- cbind(Segment_df, Data[2])
        names(Data)[1:2] <- c("Segment ID", "Data")

        return(Data)
    }

    # KMTs Curvature -------------------------------------------------------------
    if (Analysis == "KMTs Curvature") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Total_Curv.xlsx"))
        Data <- select(Data, all_of(c("Segment_ID", "Curvature")))
        names(Data)[1:2] <- c("Segment ID", "Data")

        return(Data)
    }

    # No. of KMTs ----------------------------------------------------------------
    if (Analysis == "No. of KMTs") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_K_Core_Area.xlsx"))
        Data <- select(Data, all_of(c("Fiber_name", "KMT_no")))
        names(Data)[1] <- "Fiber_Name"

        Fiber_List <- unique(Data$Fiber_Name)
        Segment_df <- data.frame()
        KMT_df <- data.frame()
        fiber_df <- data.frame()

        for (i in seq_along(Fiber_List)) {
            Data_df <- select(Data_Segments, all_of(c("Segment ID", Fiber_List[i])))
            Data_df <- Data_df %>% filter_at(vars(starts_with(Fiber_List[i])), any_vars(. >= 1))
            Data_df[3] <- Data[i, 2]
            Data_df[4] <- Data[i, 1]

            Segment_df <- rbind(Segment_df, Data_df[1])
            KMT_df <- rbind(KMT_df, Data_df[3])
            fiber_df <- rbind(fiber_df, Data_df[4])
        }

        Data <- cbind(fiber_df, Segment_df, KMT_df)
        names(Data)[1:3] <- c("Fiber_Name", "Segment ID", "Data")

        return(Data)
    }

    # No. of KMTs at a Pole ------------------------------------------------------
    if (Analysis == "No. of KMTs at a Pole") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Pole.xlsx"))
        df_Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_K_Core_Area.xlsx"))
        df_Data <- select(df_Data, "Fiber_name")
        Data <- select(Data, "KMTs_at_the_Pole")
        Data <- cbind(df_Data, Data)
        names(Data)[1:2] <- c("Fiber_Name", "Data")

        Fiber_List <- unique(Data$Fiber_Name)
        Segment_df <- data.frame()
        KMT_df <- data.frame()
        fiber_df <- data.frame()

        for (i in seq_along(Fiber_List)) {
            Data_df <- select(Data_Segments, all_of(c("Segment ID", Fiber_List[i])))
            Data_df <- Data_df %>% filter_at(vars(starts_with(Fiber_List[i])), any_vars(. >= 1))
            Data_df[3] <- Data[i, 2]
            Data_df[4] <- Data[i, 1]

            Segment_df <- rbind(Segment_df, Data_df[1])
            KMT_df <- rbind(KMT_df, Data_df[3])
            fiber_df <- rbind(fiber_df, Data_df[4])
        }

        Data <- cbind(fiber_df, Segment_df, KMT_df)
        names(Data)[1:3] <- c("Fiber_Name", "Segment ID", "Data")

        return(Data)
    }

    # KMT minus-ends interaction -------------------------------------------------
    if (Analysis == "KMT minus-ends interaction for 25nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.025.xlsx"))
        Data <- select(Data, "KMT_ID", "Interaction_ID", "MT_type")

        return(Data)
    }
    if (Analysis == "KMT minus-ends interaction for 30nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.03.xlsx"))
        Data <- select(Data, "KMT_ID", "Interaction_ID", "MT_type")

        return(Data)
    }
    if (Analysis == "KMT minus-ends interaction for 35nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.035.xlsx"))
        Data <- select(Data, "KMT_ID", "Interaction_ID", "MT_type")

        return(Data)
    }
    if (Analysis == "KMT minus-ends interaction for 45nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.045.xlsx"))
        Data <- select(Data, "KMT_ID", "Interaction_ID", "MT_type")

        return(Data)
    }
    if (Analysis == "KMT minus-ends interaction for 50nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.05.xlsx"))
        Data <- select(Data, "KMT_ID", "Interaction_ID", "MT_type")

        return(Data)
    }
    if (Analysis == "KMT minus-ends interaction for 75nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.075.xlsx"))
        Data <- select(Data, "KMT_ID", "Interaction_ID", "MT_type")

        return(Data)
    }
    if (Analysis == "KMT minus-ends interaction for 100nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.1.xlsx"))
        Data <- select(Data, "KMT_ID", "Interaction_ID", "MT_type")

        return(Data)
    }

    # KMT lattice interaction ----------------------------------------------------
    if (Analysis == "KMT lattice interaction for 25nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.025.xlsx"))
        Data <- select(Data, "KMT_ID", "Interactor_ID", "I_class")

        return(Data)
    }
    if (Analysis == "KMT lattice interaction for 30nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.03.xlsx"))
        Data <- select(Data, "KMT_ID", "Interactor_ID", "I_class")

        return(Data)
    }
    if (Analysis == "KMT lattice interaction for 35nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.035.xlsx"))
        Data <- select(Data, "KMT_ID", "Interactor_ID", "I_class")

        return(Data)
    }
    if (Analysis == "KMT lattice interaction for 45nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.045.xlsx"))
        Data <- select(Data, "KMT_ID", "Interactor_ID", "I_class")

        return(Data)
    }
    if (Analysis == "KMT lattice interaction for 50nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.05.xlsx"))
        Data <- select(Data, "KMT_ID", "Interactor_ID", "I_class")

        return(Data)
    }
    if (Analysis == "KMT lattice interaction for 75nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.075.xlsx"))
        Data <- select(Data, "KMT_ID", "Interactor_ID", "I_class")

        return(Data)
    }
    if (Analysis == "KMT lattice interaction for 100nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.1.xlsx"))
        Data <- select(Data, "KMT_ID", "Interactor_ID", "I_class")

        return(Data)
    }

    # KMT lattice interaction ----------------------------------------------------
    if (Analysis == "MT-MT interactions for 25nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_MT_Interaction_0.025.xlsx"))
        Data <- select(Data, "Segments_ID_1", "Segments_ID_2", "S_1_Start", "S_1_Stop", "S_2_Start", "S_2_Stop")
        names(Data)[1] <- "Segment ID"

        return(Data)
    }
    if (Analysis == "MT-MT interactions for 30nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_MT_Interaction_0.03.xlsx"))
        Data <- select(Data, "Segments_ID_1", "Segments_ID_2", "S_1_Start", "S_1_Stop", "S_2_Start", "S_2_Stop")
        names(Data)[1] <- "Segment ID"

        return(Data)
    }
    if (Analysis == "MT-MT interactions for 35nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_MT_Interaction_0.035.xlsx"))
        Data <- select(Data, "Segments_ID_1", "Segments_ID_2", "S_1_Start", "S_1_Stop", "S_2_Start", "S_2_Stop")
        names(Data)[1] <- "Segment ID"

        return(Data)
    }
    if (Analysis == "MT-MT interactions for 45nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_MT_Interaction_0.045.xlsx"))
        Data <- select(Data, "Segments_ID_1", "Segments_ID_2", "S_1_Start", "S_1_Stop", "S_2_Start", "S_2_Stop")
        names(Data)[1] <- "Segment ID"

        return(Data)
    }
    if (Analysis == "MT-MT interactions for 50nm") {
        Data <- read_xlsx(paste0("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_MT_Interaction_0.05.xlsx"))
        Data <- select(Data, "Segments_ID_1", "Segments_ID_2", "S_1_Start", "S_1_Stop", "S_2_Start", "S_2_Stop")
        names(Data)[1] <- "Segment ID"

        return(Data)
    }
}
