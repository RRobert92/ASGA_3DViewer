################################################################################
# Shiny Server - Collect data and standardize output
#
# (c) 2021 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2021-04-07
################################################################################
Collect_Analysis <- function(Analysis, Pub_ID, Data_ID){

  # Nothing is selected --------------------------------------------------------
  if(Analysis == "NaN" || Analysis == "Demo"){
    Data <<- NULL
  }

  # Length Distribution --------------------------------------------------------
  if(Analysis == "Length Distribution"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_LD.xlsx", sep = ""))
    Data <<- select(Data, "Fiber_Name", "length")
    names(Data)[2] <<- "Data"

    Fiber_List <- unique(Data$Fiber_Name)
    Segment_df <- data.frame()

    for (i in 1:length(Fiber_List)){
      Data_df <- select(Data_Segments, c("Segment ID", Fiber_List[i]))
      Data_df <- Data_df %>% filter_at(vars(starts_with(Fiber_List[i])), any_vars(. >= 1))

      Segment_df <- rbind(Segment_df, Data_df[1])
    }
    Data <<- cbind(Segment_df, Data[2])
    names(Data)[1:2] <<- c("Segment ID", "Data")
  }

  # KMTs minus-end position ----------------------------------------------------
  if(Analysis == "Minus-ends Position"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_LD.xlsx", sep = ""))
    Data <<- select(Data, "Fiber_Name", "minus_dist_to_pole")
    names(Data)[2] <- "Data"

    Fiber_List <- unique(Data$Fiber_Name)
    Segment_df <- data.frame()
    for (i in 1:length(Fiber_List)){
      Data_df <- select(Data_Segments, c("Segment ID", Fiber_List[i]))
      Data_df <- Data_df %>% filter_at(vars(starts_with(Fiber_List[i])), any_vars(. >= 1))
      Segment_df <- rbind(Segment_df, Data_df[1])
    }
    Data <<- cbind(Segment_df, Data[2])
    names(Data)[1:2] <<- c("Segment ID", "Data")
  }

  # KMTs Curvature -------------------------------------------------------------
  if(Analysis == "KMTs Curvature"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Total_Curv.xlsx", sep = ""))
    Data <<- select(Data, "Segment_ID","Curvature")
    names(Data)[1:2] <<- c("Segment ID", "Data")
  }

  # No. of KMTs ----------------------------------------------------------------
  if(Analysis == "No. of KMTs"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_K_Core_Area.xlsx", sep = ""))
    Data <<- select(Data, "Fiber_name","KMT_no")
    names(Data)[1] <<- "Fiber_Name"

    Fiber_List <- unique(Data$Fiber_Name)
    Segment_df <- data.frame()
    KMT_df <- data.frame()
    fiber_df <- data.frame()

    for (i in 1:length(Fiber_List)){
      Data_df <- select(Data_Segments, c("Segment ID", Fiber_List[i]))
      Data_df <- Data_df %>% filter_at(vars(starts_with(Fiber_List[i])), any_vars(. >= 1))
      Data_df[3] <- Data[i,2]
      Data_df[4] <- Data[i,1]

      Segment_df <- rbind(Segment_df, Data_df[1])
      KMT_df <- rbind(KMT_df, Data_df[3])
      fiber_df <- rbind(fiber_df, Data_df[4])
    }

    Data <<- cbind(fiber_df, Segment_df, KMT_df)
    names(Data)[1:3] <<- c("Fiber_Name", "Segment ID", "Data")
  }

  # No. of KMTs at a Pole ------------------------------------------------------
  if(Analysis == "No. of KMTs at a Pole"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Pole.xlsx", sep = ""))
    df_Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_K_Core_Area.xlsx", sep = ""))
    df_Data <<- select(df_Data, "Fiber_name")
    Data <<- select(Data, "KMTs_at_the_Pole")
    Data <<- cbind(df_Data, Data)
    names(Data)[1:2] <<- c("Fiber_Name", "Data")

    Fiber_List <- unique(Data$Fiber_Name)
    Segment_df <- data.frame()
    KMT_df <- data.frame()
    fiber_df <- data.frame()

    for (i in 1:length(Fiber_List)){
      Data_df <- select(Data_Segments, c("Segment ID", Fiber_List[i]))
      Data_df <- Data_df %>% filter_at(vars(starts_with(Fiber_List[i])), any_vars(. >= 1))
      Data_df[3] <- Data[i,2]
      Data_df[4] <- Data[i,1]

      Segment_df <- rbind(Segment_df, Data_df[1])
      KMT_df <- rbind(KMT_df, Data_df[3])
      fiber_df <- rbind(fiber_df, Data_df[4])
    }

    Data <<- cbind(fiber_df, Segment_df, KMT_df)
    names(Data)[1:3] <<- c("Fiber_Name", "Segment ID", "Data")
  }

  # KMT minus-ends interaction -------------------------------------------------
  if(Analysis == "KMT minus-ends interaction for 25nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.025.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interaction_ID", "MT_type")
  }
  if(Analysis == "KMT minus-ends interaction for 30nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.03.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interaction_ID", "MT_type")
  }
  if(Analysis == "KMT minus-ends interaction for 35nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.035.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interaction_ID", "MT_type")
  }
  if(Analysis == "KMT minus-ends interaction for 45nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.045.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interaction_ID", "MT_type")
  }
  if(Analysis == "KMT minus-ends interaction for 50nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.05.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interaction_ID", "MT_type")
  }
  if(Analysis == "KMT minus-ends interaction for 75nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.075.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interaction_ID", "MT_type")
  }
  if(Analysis == "KMT minus-ends interaction for 100nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMT_Minus_End_0.1.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interaction_ID", "MT_type")
  }

  # KMT lattice interaction ----------------------------------------------------
  if(Analysis == "KMT lattice interaction for 25nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.025.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interactor_ID", "I_class")
  }
  if(Analysis == "KMT lattice interaction for 30nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.03.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interactor_ID", "I_class")
  }
  if(Analysis == "KMT lattice interaction for 35nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.035.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interactor_ID", "I_class")
  }
  if(Analysis == "KMT lattice interaction for 45nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.045.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interactor_ID", "I_class")
  }
  if(Analysis == "KMT lattice interaction for 50nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.05.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interactor_ID", "I_class")
  }
  if(Analysis == "KMT lattice interaction for 75nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.075.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interactor_ID", "I_class")
  }
  if(Analysis == "KMT lattice interaction for 100nm"){
    Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_KMTs_minus_seed_0.1.xlsx", sep = ""))
    Data <<- select(Data, "KMT_ID","Interactor_ID", "I_class")
  }

  # # KMT lattice interaction ----------------------------------------------------
  # if(Analysis == "MT-MT interactions for 25nm"){
  #   Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_MT_Interaction_0.025.xlsx", sep = ""))
  #   Data <<- select(Data, "Segments_ID_1","Segments_ID_2", "MT_type")
  #   names(Data)[1] <<- "Segment_ID"
  # }
  # if(Analysis == "MT-MT interactions for 30nm"){
  #   Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_MT_Interaction_0.03.xlsx", sep = ""))
  #   Data <<- select(Data, "KMT_ID","Interaction_ID", "MT_type")
  #   names(Data)[1] <<- "Segment_ID"
  # }
  # if(Analysis == "MT-MT interactions for 35nm"){
  #   Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_MT_Interaction_0.035.xlsx", sep = ""))
  #   Data <<- select(Data, "KMT_ID","Interaction_ID", "MT_type")
  #   names(Data)[1] <<- "Segment_ID"
  # }
  # if(Analysis == "MT-MT interactions for 45nm"){
  #   Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_MT_Interaction_0.045.xlsx", sep = ""))
  #   Data <<- select(Data, "KMT_ID","Interaction_ID", "MT_type")
  #   names(Data)[1] <<- "Segment_ID"
  # }
  # if(Analysis == "MT-MT interactions for 50nm"){
  #   Data <<- read_xlsx(paste("./Data/", Publication_Name[Pub_ID], "/Analysis/Data_", Data_ID, "_MT_Interaction_0.05.xlsx", sep = ""))
  #   Data <<- select(Data, "KMT_ID","Interaction_ID", "MT_type")
  #   names(Data)[1] <<- "Segment_ID"
  # }
}
