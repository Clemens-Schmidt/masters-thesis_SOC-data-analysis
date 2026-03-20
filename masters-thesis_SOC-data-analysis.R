##########################################################################################################################################
#### 0. PACKAGES AND PREPARATION
##########################################################################################################################################

## Install packages
install.packages("openxlsx2")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("purrr")
install.packages("patchwork")
install.packages("stringr")
install.packages("scico")
install.packages("ggpubr")
install.packages("nortest")
install.packages("rstatix")
install.packages("PMCMRplus")
install.packages("ez")

## Load packages
library(openxlsx2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(patchwork)
library(stringr)
library(scico)
library(ggpubr)
library(nortest)
library(rstatix)
library(PMCMRplus)
library(ez)

## Set the work directory
setwd("C:/Users/cleme/OneDrive/Dokumente/Uni Koblenz/Modul 16/UiB/Masters/02_R/masters-thesis_SOC-data-analysis")


##########################################################################################################################################
#### 1. READ EXCEL DATA
##########################################################################################################################################

## Read three sheets from the excel file and only write selected columns
sample_points <- wb_read("02_Data_Soil_Samples_Ecobudgets_CS_R-import.xlsx", sheet = 1) %>%
  select(plot_ID, sample_ID, one_soil_depth_north, one_soil_depth_east, one_soil_depth_south, one_soil_depth_west, ten_soil_depth_north, ten_soil_depth_east, ten_soil_depth_south, ten_soil_depth_west, nature_type_reevaluation, area_share_ten_pct, total_sample_depth_cm, black_horizon_depth_cm)

## Additionally only write black layers (NOTE: only black layer data is included from here on)
soil_layers <- wb_read("02_Data_Soil_Samples_Ecobudgets_CS_R-import.xlsx", sheet = 2) %>%
  filter(black_horizon == "y") %>%
  select(layer_ID, sample_ID, plot_ID, nature_type_reevaluation, black_horizon, core_area_cm2, layer_depth_top_cm, layer_depth_bottom_cm, part_of_pre_def_layer, part_of_pre_def_layer_txt, dry_roots_weight_g, dry_stones_weight_g, dry_soil_weight_g, first_loss_on_ignition_pct)

eco_points <- wb_read("02_Data_Soil_Samples_Ecobudgets_CS_R-import.xlsx", sheet = 3) %>%
  select(plot_ID, main_nature_type, one_soil_depth_north, one_soil_depth_east, one_soil_depth_south, one_soil_depth_west, ten_soil_depth_north, ten_soil_depth_east, ten_soil_depth_south, ten_soil_depth_west)


##########################################################################################################################################
#### 2. SOIL LAYERS: Calculate and write results back into "soil_layers"
##########################################################################################################################################

## Compute the SOC [g/cm3] and SOC [g/cm2] per Layer
soil_layers <- soil_layers %>%
  mutate(
    # Bulk density excluding dry_stones_weight_g because the mass of stones does not inherit organic C. Yet the volume includes the full core volume including the stones
    BD_g_cm3 = (dry_roots_weight_g + dry_soil_weight_g) / (core_area_cm2 * (layer_depth_bottom_cm - layer_depth_top_cm)),
    SOC_pct = first_loss_on_ignition_pct * 0.58,
    SOM_g_cm3 = first_loss_on_ignition_pct * BD_g_cm3 / 100,
    SOC_g_cm3 = SOM_g_cm3 * 1.724,
    SOM_g_cm2 = SOM_g_cm3 * (layer_depth_bottom_cm - layer_depth_top_cm),
    SOC_g_cm2 = SOC_g_cm3 * (layer_depth_bottom_cm - layer_depth_top_cm)
  ) 

## Compute the mean grouped by "nature_type_reevaluation" and assign the respective results to all rows
soil_layers <- soil_layers %>%
  group_by(part_of_pre_def_layer) %>%
  mutate(
    LOI_prelay_mean_pct = mean(first_loss_on_ignition_pct, na.rm = TRUE),
    LOI_prelay_median_pct = median(first_loss_on_ignition_pct, na.rm = TRUE),
    BD_prelay_mean_g_cm3 = mean(BD_g_cm3, na.rm = TRUE),
    BD_prelay_median_g_cm3 = median(BD_g_cm3, na.rm = TRUE),
    SOC_prelay_mean_g_cm3 = mean(SOC_g_cm3, na.rm = TRUE),
    SOC_prelay_median_g_cm3 = median(SOC_g_cm3, na.rm = TRUE),
  ) %>%
  ungroup()

soil_layers <- soil_layers %>%
  group_by(nature_type_reevaluation, part_of_pre_def_layer) %>%
  mutate(
    LOI_ntyp_prelay_mean_pct = mean(first_loss_on_ignition_pct, na.rm = TRUE),
    LOI_ntyp_prelay_median_pct = median(first_loss_on_ignition_pct, na.rm = TRUE),
    BD_ntyp_prelay_mean_g_cm3 = mean(BD_g_cm3, na.rm = TRUE),
    BD_ntyp_prelay_median_g_cm3 = median(BD_g_cm3, na.rm = TRUE),
    SOC_ntyp_prelay_mean_g_cm3 = mean(SOC_g_cm3, na.rm = TRUE),
    SOC_ntyp_prelay_median_g_cm3 = median(SOC_g_cm3, na.rm = TRUE),
  ) %>%
  ungroup()


##########################################################################################################################################
#### 3. SAMPLE POINTS: Calculate and write results back into "sample_points"
##########################################################################################################################################

## Compute the considerable depth (black layer depth or max 60 cm)
sample_points <- sample_points %>%
  mutate(
    considerable_black_depth_cm = pmin(
      60,
      coalesce(
        as.numeric(str_remove(black_horizon_depth_cm, "^>")),   # use O-horizon if it has a number
        as.numeric(str_remove(total_sample_depth_cm, "^>"))  # fallback
      )
    )
  )

## Compute the mean soil depth per sample point. ignore "NA", remove ">"-signs, treat everything deeper than 60 cm as 60 cm, sample core depth included
sample_points <- sample_points %>%
  mutate(
    sample_mean_depth_cm = rowMeans(
      cbind(
        across(
          c(
            "one_soil_depth_north", "one_soil_depth_east",
            "one_soil_depth_south", "one_soil_depth_west",
            "ten_soil_depth_north", "ten_soil_depth_south",
            "ten_soil_depth_east", "ten_soil_depth_west"
          ),
          ~ pmin(60, as.numeric(str_remove(.x, "^>")))
        ) |> as.matrix(),
        considerable_black_depth_cm
      ),
      na.rm = TRUE
    )
  )

## Define the case differentiation and compute needed and fully available layers as well as, if applicable, the accountable depth or additional depth per pre defined layer
sample_points <- sample_points %>%
  mutate(
    sample_depth_case = ifelse(sample_mean_depth_cm > considerable_black_depth_cm,
                               if_else(!is.na(black_horizon_depth_cm),
                                       "A1",  #av. layer SOC
                                       "A2"), #av. layer SOC + add. length * mean SOC
                               if_else(sample_mean_depth_cm == considerable_black_depth_cm,
                                       "B1",  # av. layer SOC
                                       "B2")  # av. layer SOC + acc. length * mean SOC
    ),
    depth_needed = case_when(
      sample_depth_case == "A1" ~ black_horizon_depth_cm,
      sample_depth_case == "A2" ~ sample_mean_depth_cm,
      sample_depth_case == "B1" ~ considerable_black_depth_cm,
      sample_depth_case == "B2" ~ sample_mean_depth_cm
    ),
    depth_full = case_when(
      sample_depth_case == "A1" ~ black_horizon_depth_cm,
      sample_depth_case == "A2" ~ considerable_black_depth_cm,
      sample_depth_case == "B1" ~ considerable_black_depth_cm,
      sample_depth_case == "B2" ~ sample_mean_depth_cm
    ),
    needed_layers = case_when(
      depth_needed <=  2 ~ 1,
      depth_needed <=  5 ~ 2,
      depth_needed <= 10 ~ 3,
      depth_needed <= 35 ~ 4,
      depth_needed <= 60 ~ 5
    ),
    lowest_full_available_layer = if_else(sample_depth_case == "B2",
                                          case_when(
                                            depth_full <  2 ~ 1,
                                            depth_full <  5 ~ 2,
                                            depth_full < 10 ~ 3,
                                            depth_full < 35 ~ 4,
                                            depth_full < 60 ~ 5
                                          ) - 1,
                                          case_when(
                                            depth_full <=  2 ~ 1,
                                            depth_full <=  5 ~ 2,
                                            depth_full <= 10 ~ 3,
                                            depth_full <= 35 ~ 4,
                                            depth_full <= 60 ~ 5
                                          )
  ),
  
    acc_depth_cm = case_when(
      sample_depth_case == "B2" & sample_mean_depth_cm < 2 ~ sample_mean_depth_cm - 0,
      sample_depth_case == "B2" & sample_mean_depth_cm < 5 ~ sample_mean_depth_cm - 2,
      sample_depth_case == "B2" & sample_mean_depth_cm < 10 ~ sample_mean_depth_cm - 5,
      sample_depth_case == "B2" & sample_mean_depth_cm < 35 ~ sample_mean_depth_cm - 10,
      sample_depth_case == "B2" & sample_mean_depth_cm < 60 ~ sample_mean_depth_cm - 35
    ),
    layer_limits = list(c(2, 5, 10, 35, 60)),
    
    add_depth_cm = if_else(
      sample_depth_case == "A2",
      pmax(sample_mean_depth_cm - considerable_black_depth_cm, 0),
      NA_real_
    ),

    layer_depths = pmap(
      list(layer_limits, add_depth_cm, considerable_black_depth_cm, sample_depth_case),
      function(limits, rem, start, case) {

        if (case != "A2") return(rep(NA_real_, 5))

        out <- numeric(length(limits))

        for (i in seq_along(limits)) {
          available <- max(limits[i] - start, 0)
          add <- min(rem, available)
          out[i] <- add
          rem <- rem - add
        }

        out
      }
    ),

    add_layer_1_depth_cm = map_dbl(layer_depths, 1),
    add_layer_2_depth_cm = map_dbl(layer_depths, 2),
    add_layer_3_depth_cm = map_dbl(layer_depths, 3),
    add_layer_4_depth_cm = map_dbl(layer_depths, 4),
    add_layer_5_depth_cm = map_dbl(layer_depths, 5)
  ) %>%
  select(-layer_limits, -layer_depths, -depth_needed, -depth_full)

## Compute/Left_join the SOC [g/cm2] per pre_def_layer of the respective sample
sample_points <- sample_points %>%
  left_join(
    soil_layers %>%
      group_by(sample_ID, part_of_pre_def_layer) %>%
      summarise(
        SOC_sample_g_cm2 = sum(SOC_g_cm2, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = part_of_pre_def_layer,
        values_from = SOC_sample_g_cm2,
        names_glue = "SOC_sample_prelay_{part_of_pre_def_layer}_g_cm2"
      ),   
    by = "sample_ID"
  )

## Left_join the SOC [g_cm3] per pre defined layer for the respective nature type
sample_points <- sample_points %>%
  left_join(
    soil_layers %>%
      group_by(nature_type_reevaluation, part_of_pre_def_layer) %>%
      summarise(SOC_ntyp_prelay_mean_g_cm3 = mean(SOC_ntyp_prelay_mean_g_cm3, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        id_cols = nature_type_reevaluation,
        names_from = part_of_pre_def_layer,
        values_from = SOC_ntyp_prelay_mean_g_cm3,
        names_glue = "SOC_mean_ntyp_prelay_{part_of_pre_def_layer}_g_cm3"
      ),
    by = "nature_type_reevaluation"
  )

## Compute the SOC stock per sample point [g/cm2] VARIANT 1: depth with respect to cases A1, A2, B1 and B2, combination of actuals and means
sample_points <- sample_points %>%
  rowwise() %>%
  mutate(
    SOC_stock_sample_var1_g_cm2 = case_when(
      # A1: sum all measured g/cm² layers
      sample_depth_case == "A1" ~
        sum(c_across(starts_with("SOC_sample_prelay_") &
                       ends_with("_g_cm2")), na.rm = TRUE),
      # A2: measured g/cm² + extended layers g/cm³ * depth
      sample_depth_case == "A2" ~ {
        # sum all g/cm² layers
        sum_gcm2 <- sum(c_across(starts_with("SOC_sample_prelay_") &
                                   ends_with("_g_cm2")), na.rm = TRUE)
        # sum the added g/cm³ * depth layers
        added_sum <- sum(c(
          SOC_mean_ntyp_prelay_1_g_cm3 * add_layer_1_depth_cm,
          SOC_mean_ntyp_prelay_2_g_cm3 * add_layer_2_depth_cm,
          SOC_mean_ntyp_prelay_3_g_cm3 * add_layer_3_depth_cm,
          SOC_mean_ntyp_prelay_4_g_cm3 * add_layer_4_depth_cm,
          SOC_mean_ntyp_prelay_5_g_cm3 * add_layer_5_depth_cm
        ), na.rm = TRUE)
        
        sum_gcm2 + added_sum
      },
      # B1: sum all measured g/cm² layers
      sample_depth_case == "B1" ~
        sum(c_across(starts_with("SOC_sample_prelay_") &
                       ends_with("_g_cm2")), na.rm = TRUE),
      
      # B2: sum lowest_full_available_layer g/cm² + acc_depth * needed_layer (one layer)
      sample_depth_case == "B2" ~ {
        # 1) Full g/cm2 layers (ignore if missing)
        full_cols <- paste0(
          "SOC_sample_prelay_", seq_len(lowest_full_available_layer), "_g_cm2"
        )
        full_sum <- sum(c_across(any_of(full_cols)), na.rm = TRUE)
        # 2) One g/cm3 layer from needed_layers (must be length 1)
        needed_col <- paste0("SOC_mean_ntyp_prelay_", needed_layers, "_g_cm3")
        needed_val <- if (needed_col %in% names(sample_points)) {
          c_across(all_of(needed_col))
        } else {
          NA_real_        # <-- prevents zero-length values
        }
        full_sum + acc_depth_cm * needed_val
      },
      
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

## At Sample-Point 496-S1-WET1, the bottom core could not be extracted -> for the bottom two layers, i take the SOC_mean_ntyp_prelay_[4, 5]_g_cm3
sample_points <- sample_points %>%
  mutate(
    SOC_stock_sample_var1_g_cm2 = if_else(
      sample_ID == "496-S1-WET1",
      SOC_sample_prelay_1_g_cm2 + SOC_sample_prelay_2_g_cm2 + SOC_sample_prelay_3_g_cm2 + (25 * SOC_mean_ntyp_prelay_4_g_cm3) + (25 * SOC_mean_ntyp_prelay_5_g_cm3),          
      SOC_stock_sample_var1_g_cm2   
    )
  )

## Compute the SOC stock per sample point [g/cm2] VARIANT 2: considerable depth per sample, but values: SOC_mean_ntyp_prelay_[1...5]_g_cm3
sample_points <- sample_points %>%
  mutate(
     depth_needed = case_when(
       sample_depth_case == "A1" ~ black_horizon_depth_cm,
       sample_depth_case == "A2" ~ sample_mean_depth_cm,
       sample_depth_case == "B1" ~ considerable_black_depth_cm,
       sample_depth_case == "B2" ~ sample_mean_depth_cm
       ),
    SOC_stock_sample_var2_g_cm2 = case_when(
      depth_needed <=  2 ~ depth_needed * SOC_mean_ntyp_prelay_1_g_cm3,
      depth_needed <=  5 ~ 2 * SOC_mean_ntyp_prelay_1_g_cm3 + (depth_needed -2) * SOC_mean_ntyp_prelay_2_g_cm3,
      depth_needed <= 10 ~ 2 * SOC_mean_ntyp_prelay_1_g_cm3 + 3 * SOC_mean_ntyp_prelay_2_g_cm3 + (depth_needed -5) * SOC_mean_ntyp_prelay_3_g_cm3,
      depth_needed <= 35 ~ 2 * SOC_mean_ntyp_prelay_1_g_cm3 + 3 * SOC_mean_ntyp_prelay_2_g_cm3 + 5 * SOC_mean_ntyp_prelay_3_g_cm3 + (depth_needed -10) * SOC_mean_ntyp_prelay_4_g_cm3,
      depth_needed <= 60 ~ 2 * SOC_mean_ntyp_prelay_1_g_cm3 + 3 * SOC_mean_ntyp_prelay_2_g_cm3 + 5 * SOC_mean_ntyp_prelay_3_g_cm3 + 25 * SOC_mean_ntyp_prelay_4_g_cm3 + (depth_needed -35) * SOC_mean_ntyp_prelay_5_g_cm3
    )
  ) %>%
  select(-depth_needed)

sample_points <- sample_points %>%
  mutate(
    SOC_stock_sample_var1_kg_m2 = SOC_stock_sample_var1_g_cm2 * 10,
    SOC_stock_area_share_var1_g_cm2 = SOC_stock_sample_var1_g_cm2 * area_share_ten_pct / 100,
    SOC_stock_area_share_var2_g_cm2 = SOC_stock_sample_var2_g_cm2 * area_share_ten_pct / 100
  )


##########################################################################################################################################
#### 4. ECO POINTS: add columns from "sample_points" and calculate values in "eco_points"
##########################################################################################################################################

## Merge selected columns from sample_points into eco_points
eco_points <- eco_points %>%
  rowwise() %>%
  mutate(
    this_plot = plot_ID,
    this_nature = main_nature_type,
    SOC_values = list(
      sample_points %>%
        filter(
          plot_ID == this_plot,
          nature_type_reevaluation == this_nature
        ) %>%
        slice_head(n = 1) %>%
        select(
          black_horizon_depth_cm,
          SOC_mean_ntyp_prelay_1_g_cm3,
          SOC_mean_ntyp_prelay_2_g_cm3,
          SOC_mean_ntyp_prelay_3_g_cm3,
          SOC_mean_ntyp_prelay_4_g_cm3,
          SOC_mean_ntyp_prelay_5_g_cm3
        ) %>%
        { if(nrow(.) == 0) tibble(
          black_horizon_depth_cm = NA_real_,
          SOC_mean_ntyp_prelay_1_g_cm3 = NA_real_,
          SOC_mean_ntyp_prelay_2_g_cm3 = NA_real_,
          SOC_mean_ntyp_prelay_3_g_cm3 = NA_real_,
          SOC_mean_ntyp_prelay_4_g_cm3 = NA_real_,
          SOC_mean_ntyp_prelay_5_g_cm3 = NA_real_
        ) else . }
    )
  ) %>%
  unnest_wider(SOC_values) %>%
  ungroup() %>%
  select(-this_plot, -this_nature)

## Compute the mean soil depth per Ecobudgets point. ignore "NA", remove ">"-signs, treat everything deeper than 60 cm as 60 cm and respect black horizon depth, sample core depth excluded, ignore nature types
eco_points <- eco_points %>%
  mutate(
    soil_depth_mean_ecop_var3_cm = rowMeans(
      across(
        all_of(c(
          "one_soil_depth_north", "one_soil_depth_east", 
          "one_soil_depth_south", "one_soil_depth_west", 
          "ten_soil_depth_north", "ten_soil_depth_south", 
          "ten_soil_depth_east", "ten_soil_depth_west"
        )),
        ~ as.numeric(str_remove(.x, "^>"))
      ),
      na.rm = TRUE
    ),
    soil_depth_mean_ecop_var3_cm = pmin(soil_depth_mean_ecop_var3_cm, 60, black_horizon_depth_cm, na.rm = TRUE)
  )

## Compute the mean soil depth per Ecobudgets point. ignore "NA", remove ">"-signs, treat everything deeper than 60 cm as 60 cm, sample core depth excluded, ignore nature types, ignore black horizon ends
eco_points <- eco_points %>%
  mutate(
    soil_depth_mean_ecop_var4_cm = rowMeans(
      across(
        all_of(c(
          "one_soil_depth_north", "one_soil_depth_east", 
          "one_soil_depth_south", "one_soil_depth_west", 
          "ten_soil_depth_north", "ten_soil_depth_south", 
          "ten_soil_depth_east", "ten_soil_depth_west"
        )),
        ~ as.numeric(str_remove(.x, "^>"))
      ),
      na.rm = TRUE
    ),
    soil_depth_mean_ecop_var4_cm = pmin(soil_depth_mean_ecop_var4_cm, 60)
  )

eco_points <- eco_points %>%
  mutate(
    SOC_stock_ecop_var3_g_cm2 = case_when(
      soil_depth_mean_ecop_var3_cm <=  2 ~ soil_depth_mean_ecop_var3_cm * SOC_mean_ntyp_prelay_1_g_cm3,
      soil_depth_mean_ecop_var3_cm <=  5 ~ 2 * SOC_mean_ntyp_prelay_1_g_cm3 + (soil_depth_mean_ecop_var3_cm -2) * SOC_mean_ntyp_prelay_2_g_cm3,
      soil_depth_mean_ecop_var3_cm <= 10 ~ 2 * SOC_mean_ntyp_prelay_1_g_cm3 + 3 * SOC_mean_ntyp_prelay_2_g_cm3 + (soil_depth_mean_ecop_var3_cm -5) * SOC_mean_ntyp_prelay_3_g_cm3,
      soil_depth_mean_ecop_var3_cm <= 35 ~ 2 * SOC_mean_ntyp_prelay_1_g_cm3 + 3 * SOC_mean_ntyp_prelay_2_g_cm3 + 5 * SOC_mean_ntyp_prelay_3_g_cm3 + (soil_depth_mean_ecop_var3_cm -10) * SOC_mean_ntyp_prelay_4_g_cm3,
      soil_depth_mean_ecop_var3_cm <= 60 ~ 2 * SOC_mean_ntyp_prelay_1_g_cm3 + 3 * SOC_mean_ntyp_prelay_2_g_cm3 + 5 * SOC_mean_ntyp_prelay_3_g_cm3 + 25 * SOC_mean_ntyp_prelay_4_g_cm3 + (soil_depth_mean_ecop_var3_cm -35) * SOC_mean_ntyp_prelay_5_g_cm3
    )
  )

eco_points <- eco_points %>%
  mutate(
    SOC_stock_ecop_var4_g_cm2 = case_when(
      soil_depth_mean_ecop_var4_cm <=  2 ~ soil_depth_mean_ecop_var4_cm * SOC_mean_ntyp_prelay_1_g_cm3,
      soil_depth_mean_ecop_var4_cm <=  5 ~ 2 * SOC_mean_ntyp_prelay_1_g_cm3 + (soil_depth_mean_ecop_var4_cm -2) * SOC_mean_ntyp_prelay_2_g_cm3,
      soil_depth_mean_ecop_var4_cm <= 10 ~ 2 * SOC_mean_ntyp_prelay_1_g_cm3 + 3 * SOC_mean_ntyp_prelay_2_g_cm3 + (soil_depth_mean_ecop_var4_cm -5) * SOC_mean_ntyp_prelay_3_g_cm3,
      soil_depth_mean_ecop_var4_cm <= 35 ~ 2 * SOC_mean_ntyp_prelay_1_g_cm3 + 3 * SOC_mean_ntyp_prelay_2_g_cm3 + 5 * SOC_mean_ntyp_prelay_3_g_cm3 + (soil_depth_mean_ecop_var4_cm -10) * SOC_mean_ntyp_prelay_4_g_cm3,
      soil_depth_mean_ecop_var4_cm <= 60 ~ 2 * SOC_mean_ntyp_prelay_1_g_cm3 + 3 * SOC_mean_ntyp_prelay_2_g_cm3 + 5 * SOC_mean_ntyp_prelay_3_g_cm3 + 25 * SOC_mean_ntyp_prelay_4_g_cm3 + (soil_depth_mean_ecop_var4_cm -35) * SOC_mean_ntyp_prelay_5_g_cm3
    )
  )

eco_points <- eco_points %>%
  left_join(
    sample_points %>%
      group_by(plot_ID) %>%
      summarise(
        SOC_stock_ecop_var1_g_cm2 = sum(SOC_stock_area_share_var1_g_cm2, na.rm = TRUE),
        SOC_stock_ecop_var2_g_cm2 = sum(SOC_stock_area_share_var2_g_cm2, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "plot_ID"
  )

eco_points <- eco_points %>%
  mutate(
    SOC_stock_ecop_var1_kg_m2 = SOC_stock_ecop_var1_g_cm2 * 10
  )

eco_points <- eco_points %>%
  left_join(
    eco_points %>%
      group_by(main_nature_type) %>%
      summarise(
        n_main_nature_type = n(),
        across(
          starts_with("SOC_stock_ecop_var"),
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            median = ~ median(.x, na.rm = TRUE)
          ),
          .names = "{.col}_{.fn}"
        ),
        .groups = "drop"
      ),
    by = "main_nature_type"
  )
    

##########################################################################################################################################
#### 5. PLOT RESULTS
##########################################################################################################################################

####
#### PLOT RESULT 1: LOI [%] and SOC [g/cm3] for all soil layers per pre defined layer depth

## Prepare plot data
soil_layers$part_of_pre_def_layer <- factor(
  soil_layers$part_of_pre_def_layer,
  levels = c("1", "2", "3", "4", "5"),
  labels = c("0-2", "2-5", "5-10", "10-35", "35-60")
)
counts_all_layers <- soil_layers %>%
  group_by(part_of_pre_def_layer) %>%
  summarise(n = n())
mean_LOI_all_layers <- soil_layers %>%
  group_by(part_of_pre_def_layer) %>%
  summarise(
    mean_SOM_pct = mean(first_loss_on_ignition_pct, na.rm = TRUE)
  )
median_LOI_all_layers <- soil_layers %>%
  group_by(part_of_pre_def_layer) %>%
  summarise(
    median_SOM_pct = median(first_loss_on_ignition_pct, na.rm = TRUE)
  )
mean_SOC_all_layers <- soil_layers %>%
  group_by(part_of_pre_def_layer) %>%
  summarise(
    mean_SOC_g_cm3 = mean(SOC_g_cm3, na.rm = TRUE)
  )
median_SOC_all_layers <- soil_layers %>%
  group_by(part_of_pre_def_layer) %>%
  summarise(
    median_SOC_g_cm3 = median(SOC_g_cm3, na.rm = TRUE)
  )

sl_groups <- length(levels(factor(sample_points$nature_type_reevaluation)))
sl_colors <- scico(n = sl_groups, palette = "turku", begin = 0.25, end = 0.95)

## Plot data for result 1: LOI [%] and SOC [g/cm3] for all soil layers per pre defined layer depth
plot_all_layers_SOM_pct <- ggplot(soil_layers, aes(x = part_of_pre_def_layer, 
                                     y = first_loss_on_ignition_pct)) +
  geom_boxplot(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", median.colour = "grey10", median.linewidth = 0.5, width = 0.6, outlier.shape = 5, outlier.size = 2.5, linewidth = 0.5) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.6,
    linetype = "dashed",
    linewidth = 0.25,
    color = "grey10"
  ) +
  geom_text(
    data = counts_all_layers,
    aes(x = part_of_pre_def_layer, label = paste0("n = ", n)),
    y = Inf, 
    vjust = -4.1,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = mean_LOI_all_layers,
    aes(x = part_of_pre_def_layer),
    label = sapply(mean_LOI_all_layers$mean_SOM_pct, 
                   function(x) bquote(bar(x) == .(sprintf("%.1f", x)))),
    parse = TRUE,
    y = Inf, 
    vjust = -2.3,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = median_LOI_all_layers,
    aes(x = part_of_pre_def_layer),
    label = sapply(median_LOI_all_layers$median_SOM_pct, function(x) bquote(tilde(x) == .(sprintf("%.1f", x)))),
    parse = TRUE,
    y = Inf,
    vjust = -0.5,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 14, family = "sans"),
        axis.title.y = element_text(size = 14, family = "sans"),
        axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
        axis.text.y  = element_text(size = 14, family = "sans", color = "black")
  ) +
  labs(x = "Layer depth [cm]", y = "SOM [mass-%]") +
  coord_cartesian(ylim = c(0, 100), clip = "off") +
  theme(
    plot.margin = margin(t = 75, r = 5, b = 5, l = 5) 
  ) +
  scale_fill_manual(values = sl_colors) +
  scale_color_manual(values = sl_colors)


plot_all_layers_SOC_g_cm3 <- ggplot(soil_layers, aes(x = part_of_pre_def_layer, 
                                       y = SOC_g_cm3)) +
  geom_boxplot(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", median.colour = "grey10", median.linewidth = 0.5, width = 0.6, outlier.shape = 5, outlier.size = 2.5, linewidth = 0.5) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.6,
    linetype = "dashed",
    linewidth = 0.25,
    color = "grey10"
  ) +
  geom_text(
    data = counts_all_layers,
    aes(x = part_of_pre_def_layer, label = paste0("n = ", n)),
    y = Inf, 
    vjust = -4.1,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = mean_SOC_all_layers,
    aes(x = part_of_pre_def_layer),
    label = sapply(mean_SOC_all_layers$mean_SOC_g_cm3, function(x) bquote(bar(x) == .(sprintf("%.2f", x)))),
    parse = TRUE,
    y = Inf, 
    vjust = -2.3,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = median_SOC_all_layers,
    aes(x = part_of_pre_def_layer),
    label = sapply(median_SOC_all_layers$median_SOC_g_cm3, function(x) bquote(tilde(x) == .(sprintf("%.2f", x)))),
    parse = TRUE,
    y = Inf,
    vjust = -0.5,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", 
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 14, family = "sans"),
        axis.title.y = element_text(size = 14, family = "sans"),
        axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
        axis.text.y  = element_text(size = 14, family = "sans", color = "black")
  ) +
  labs(x = "Layer depth [cm]", y = "SOC [g / cm²]") +
  coord_cartesian(ylim = c(0, 0.4), clip = "off") +
  theme(
    plot.margin = margin(t = 75, r = 5, b = 5, l = 5) 
  ) +
  scale_fill_manual(values = sl_colors) +
  scale_color_manual(values = sl_colors)


## PLOT SAVE
ggsave(
  "plot_all_layers_SOM_pct.png",
  plot_all_layers_SOM_pct,
  width = 160/25.4,
  height = 120/25.4,
  units = "in",
  dpi = 300
)

ggsave(
  "plot_all_layers_SOC_g_cm3.png",
  plot_all_layers_SOC_g_cm3,
  width = 160/25.4,
  height = 120/25.4,
  units = "in",
  dpi = 300
)


####
#### PLOT RESULT 2: SOC [g/cm3] per pre defined layer depth fro each nature type

## Prepare plot data
layers_urban <- soil_layers %>%
  filter(nature_type_reevaluation == "Urban")
counts_layers_urban <- layers_urban %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(n = n(), .groups = "drop")
mean_SOC_layers_urban <- layers_urban %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    mean_val = mean(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
mean_SOC_layers_urban <- mean_SOC_layers_urban %>%
  left_join(counts_layers_urban, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("bar(x) == '", sprintf("%.2f", mean_val), "'"),
                        NA))
median_SOC_layers_urban <- layers_urban %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    median_val = median(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
median_SOC_layers_urban <- median_SOC_layers_urban %>%
  left_join(counts_layers_urban, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("tilde(x) == '", sprintf("%.2f", median_val), "'"),
                        NA))


layers_deci <- soil_layers %>%
  filter(nature_type_reevaluation == "Deciduous Forest")
counts_layers_deci <- layers_deci %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(n = n(), .groups = "drop")
mean_SOC_layers_deci <- layers_deci %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    mean_val = mean(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
mean_SOC_layers_deci <- mean_SOC_layers_deci %>%
  left_join(counts_layers_deci, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("bar(x) == '", sprintf("%.2f", mean_val), "'"),
                        NA))
median_SOC_layers_deci <- layers_deci %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    median_val = median(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
median_SOC_layers_deci <- median_SOC_layers_deci %>%
  left_join(counts_layers_deci, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("tilde(x) == '", sprintf("%.2f", median_val), "'"),
                        NA))


layers_pine <- soil_layers %>%
  filter(nature_type_reevaluation == "Pine Forest")
counts_layers_pine <- layers_pine %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(n = n(), .groups = "drop")
mean_SOC_layers_pine <- layers_pine %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    mean_val = mean(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
mean_SOC_layers_pine <- mean_SOC_layers_pine %>%
  left_join(counts_layers_pine, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("bar(x) == '", sprintf("%.2f", mean_val), "'"),
                        NA))
median_SOC_layers_pine <- layers_pine %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    median_val = median(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
median_SOC_layers_pine <- median_SOC_layers_pine %>%
  left_join(counts_layers_pine, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("tilde(x) == '", sprintf("%.2f", median_val), "'"),
                        NA))


layers_spru <- soil_layers %>%
  filter(nature_type_reevaluation == "Spruce Forest")
counts_layers_spru <- layers_spru %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(n = n(), .groups = "drop")
mean_SOC_layers_spru <- layers_spru %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    mean_val = mean(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
mean_SOC_layers_spru <- mean_SOC_layers_spru %>%
  left_join(counts_layers_spru, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("bar(x) == '", sprintf("%.2f", mean_val), "'"),
                        NA))
median_SOC_layers_spru <- layers_spru %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    median_val = median(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
median_SOC_layers_spru <- median_SOC_layers_spru %>%
  left_join(counts_layers_spru, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("tilde(x) == '", sprintf("%.2f", median_val), "'"),
                        NA))


layers_shru <- soil_layers %>%
  filter(nature_type_reevaluation == "Shrubs")
counts_layers_shru <- layers_shru %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(n = n(), .groups = "drop")
mean_SOC_layers_shru <- layers_shru %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    mean_val = mean(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
mean_SOC_layers_shru <- mean_SOC_layers_shru %>%
  left_join(counts_layers_shru, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("bar(x) == '", sprintf("%.2f", mean_val), "'"),
                        NA))

median_SOC_layers_shru <- layers_shru %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    median_val = median(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
median_SOC_layers_shru <- median_SOC_layers_shru %>%
  left_join(counts_layers_shru, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("tilde(x) == '", sprintf("%.2f", median_val), "'"),
                        NA))

layers_wet <- soil_layers %>%
  filter(nature_type_reevaluation == "Wetland")
counts_layers_wet <- layers_wet %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(n = n(), .groups = "drop")
mean_SOC_layers_wet <- layers_wet %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    mean_val = mean(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
mean_SOC_layers_wet <- mean_SOC_layers_wet %>%
  left_join(counts_layers_wet, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("bar(x) == '", sprintf("%.2f", mean_val), "'"),
                        NA))
median_SOC_layers_wet <- layers_wet %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    median_val = median(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
median_SOC_layers_wet <- median_SOC_layers_wet %>%
  left_join(counts_layers_wet, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("tilde(x) == '", sprintf("%.2f", median_val), "'"),
                        NA))


layers_grass <- soil_layers %>%
  filter(nature_type_reevaluation == "Grassland")
counts_layers_grass <- layers_grass %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(n = n(), .groups = "drop")
mean_SOC_layers_grass <- layers_grass %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    mean_val = mean(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
mean_SOC_layers_grass <- mean_SOC_layers_grass %>%
  left_join(counts_layers_grass, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("bar(x) == '", sprintf("%.2f", mean_val), "'"),
                        NA))
median_SOC_layers_grass <- layers_grass %>%
  group_by(part_of_pre_def_layer, .drop = FALSE) %>%
  summarise(
    median_val = median(SOC_g_cm3, na.rm = TRUE), .groups = "drop"
  )
median_SOC_layers_grass <- median_SOC_layers_grass %>%
  left_join(counts_layers_grass, by = "part_of_pre_def_layer") %>%
  mutate(label = ifelse(n > 0,
                        paste0("tilde(x) == '", sprintf("%.2f", median_val), "'"),
                        NA))



## Plot data for result 2: SOC concentration [g/cm3] per pre defined layer depth for each nature type group
plot_SOC_layers_urban <- ggplot(layers_urban, aes(x = part_of_pre_def_layer, y = `SOC_g_cm3`)) +
  scale_x_discrete(drop = FALSE) +
  geom_boxplot(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.7,
    linetype = "dashed",
    linewidth = 0.2, 
    alpha = 0.5,
    color = "grey30"
  ) +
  stat_boxplot(
    geom = "errorbar",
    width = 0.4,
    linewidth = 0.3,
    color = "grey10",
  )+
  geom_jitter(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", width = 0.06, size = 3.5, shape = 21) + 
    geom_text(
      data = counts_layers_urban,
      aes(x = part_of_pre_def_layer, label = paste0("n = ", n)),
      y = Inf, 
      vjust = -4.1,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    geom_text(
      data = mean_SOC_layers_urban,
      aes(x = part_of_pre_def_layer,
      label = label),
      parse = TRUE,
      y = Inf, 
      vjust = -2.3,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    geom_text(
      data = median_SOC_layers_urban,
      aes(x = part_of_pre_def_layer,
      label = label),
      parse = TRUE,
      y = Inf,
      vjust = -0.5,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none", 
          axis.line = element_line(color = "black", linewidth = 0.6),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 14, family = "sans"),
          axis.title.y = element_text(size = 14, family = "sans"),
          axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
          axis.text.y  = element_text(size = 14, family = "sans", color = "black")
    ) +
    labs(x = "Layer depth [cm]\nBuilt Area", y = "SOC [g / cm³]") +
    coord_cartesian(ylim = c(0, 0.4), clip = "off") +
    theme(
      plot.margin = margin(t = 75, r = 5, b = 5, l = 5) 
    ) +
    scale_fill_manual(values = sl_colors) +
    scale_color_manual(values = sl_colors)


plot_SOC_layers_deci <- ggplot(layers_deci, aes(x = part_of_pre_def_layer, y = `SOC_g_cm3`)) +
  scale_x_discrete(drop = FALSE) +
  geom_boxplot(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.7,
    linetype = "dashed",
    linewidth = 0.2, 
    alpha = 0.5,
    color = "grey30"
  ) +
  stat_boxplot(
    geom = "errorbar",
    width = 0.4,
    linewidth = 0.3,
    color = "grey10",
  )+
  geom_jitter(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", width = 0.06, size = 3.5, shape = 21) + 
  geom_text(
    data = counts_layers_deci,
    aes(x = part_of_pre_def_layer, label = paste0("n = ", n)),
    y = Inf, 
    vjust = -4.1,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = mean_SOC_layers_deci,
    aes(x = part_of_pre_def_layer,
    label = label),
    parse = TRUE,
    y = Inf, 
    vjust = -2.3,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = median_SOC_layers_deci,
    aes(x = part_of_pre_def_layer,
    label = label),
    parse = TRUE,
    y = Inf,
    vjust = -0.5,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", 
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 14, family = "sans"),
        axis.title.y = element_text(size = 14, family = "sans"),
        axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
        axis.text.y  = element_text(size = 14, family = "sans", color = "black")
  ) +
  labs(x = "Layer depth [cm]\nDeciduous Forest", y = "SOC [g / cm³]") +
  coord_cartesian(ylim = c(0, 0.4), clip = "off") +
  theme(
    plot.margin = margin(t = 75, r = 5, b = 5, l = 5) 
  ) +
  scale_fill_manual(values = sl_colors) +
  scale_color_manual(values = sl_colors)


plot_SOC_layers_pine <- ggplot(layers_pine, aes(x = part_of_pre_def_layer, y = `SOC_g_cm3`)) +
  scale_x_discrete(drop = FALSE) +
  geom_boxplot(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.7,
    linetype = "dashed",
    linewidth = 0.2, 
    alpha = 0.5,
    color = "grey30"
  ) +
  stat_boxplot(
    geom = "errorbar",
    width = 0.4,
    linewidth = 0.3,
    color = "grey10",
  )+
  geom_jitter(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", width = 0.06, size = 3.5, shape = 21) + 
  geom_text(
    data = counts_layers_pine,
    aes(x = part_of_pre_def_layer, label = paste0("n = ", n)),
    y = Inf, 
    vjust = -4.1,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = mean_SOC_layers_pine,
    aes(x = part_of_pre_def_layer,
    label = label),
    parse = TRUE,
    y = Inf, 
    vjust = -2.3,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = median_SOC_layers_pine,
    aes(x = part_of_pre_def_layer,
    label = label),
    parse = TRUE,
    y = Inf,
    vjust = -0.5,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", 
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 14, family = "sans"),
        axis.title.y = element_text(size = 14, family = "sans"),
        axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
        axis.text.y  = element_text(size = 14, family = "sans", color = "black")
  ) +
  labs(x = "Layer depth [cm]\nPine Forest", y = "SOC [g / cm³]") +
  coord_cartesian(ylim = c(0, 0.4), clip = "off") +
  theme(
    plot.margin = margin(t = 75, r = 5, b = 5, l = 5) 
  ) +
  scale_fill_manual(values = sl_colors) +
  scale_color_manual(values = sl_colors)


plot_SOC_layers_spru <- ggplot(layers_spru, aes(x = part_of_pre_def_layer, y = `SOC_g_cm3`)) +
  scale_x_discrete(drop = FALSE) +
  geom_boxplot(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.7,
    linetype = "dashed",
    linewidth = 0.2, 
    alpha = 0.5,
    color = "grey30"
  ) +
  stat_boxplot(
    geom = "errorbar",
    width = 0.4,
    linewidth = 0.3,
    color = "grey10",
  )+
  geom_jitter(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", width = 0.06, size = 3.5, shape = 21) + 
  geom_text(
    data = counts_layers_spru,
    aes(x = part_of_pre_def_layer, label = paste0("n = ", n)),
    y = Inf, 
    vjust = -4.1,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = mean_SOC_layers_spru,
    aes(x = part_of_pre_def_layer,
    label = label),
    parse = TRUE,
    y = Inf, 
    vjust = -2.3,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = median_SOC_layers_spru,
    aes(x = part_of_pre_def_layer,
    label = label),
    parse = TRUE,
    y = Inf,
    vjust = -0.5,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", 
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 14, family = "sans"),
        axis.title.y = element_text(size = 14, family = "sans"),
        axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
        axis.text.y  = element_text(size = 14, family = "sans", color = "black")
  ) +
  labs(x = "Layer depth [cm]\nSpruce Forest", y = "SOC [g / cm³]") +
  coord_cartesian(ylim = c(0, 0.4), clip = "off") +
  theme(
    plot.margin = margin(t = 75, r = 5, b = 5, l = 5) 
  ) +
  scale_fill_manual(values = sl_colors) +
  scale_color_manual(values = sl_colors)


plot_SOC_layers_shru <- ggplot(layers_shru, aes(x = part_of_pre_def_layer, y = `SOC_g_cm3`)) +
  scale_x_discrete(drop = FALSE) +
  geom_boxplot(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.7,
    linetype = "dashed",
    linewidth = 0.2, 
    alpha = 0.5,
    color = "grey30"
  ) +
  stat_boxplot(
    geom = "errorbar",
    width = 0.4,
    linewidth = 0.3,
    color = "grey10",
  )+
  geom_jitter(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", width = 0.06, size = 3.5, shape = 21) + 
  geom_text(
    data = counts_layers_shru,
    aes(x = part_of_pre_def_layer, label = paste0("n = ", n)),
    y = Inf, 
    vjust = -4.1,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = mean_SOC_layers_shru,
    aes(x = part_of_pre_def_layer,
    label = label),
    parse = TRUE,
    y = Inf, 
    vjust = -2.3,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = median_SOC_layers_shru,
    aes(x = part_of_pre_def_layer,
    label = label),
    parse = TRUE,
    y = Inf,
    vjust = -0.5,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", 
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 14, family = "sans"),
        axis.title.y = element_text(size = 14, family = "sans"),
        axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
        axis.text.y  = element_text(size = 14, family = "sans", color = "black")
  ) +
  labs(x = "Layer depth [cm]\nShrubs", y = "SOC [g / cm³]") +
  coord_cartesian(ylim = c(0, 0.4), clip = "off") +
  theme(
    plot.margin = margin(t = 75, r = 5, b = 5, l = 5) 
  ) +
  scale_fill_manual(values = sl_colors) +
  scale_color_manual(values = sl_colors)


plot_SOC_layers_grass <- ggplot(layers_grass, aes(x = part_of_pre_def_layer, y = `SOC_g_cm3`)) +
  scale_x_discrete(drop = FALSE) +
  geom_boxplot(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.7,
    linetype = "dashed",
    linewidth = 0.2, 
    alpha = 0.5,
    color = "grey30"
  ) +
  stat_boxplot(
    geom = "errorbar",
    width = 0.4,
    linewidth = 0.3,
    color = "grey10",
  )+
  geom_jitter(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", width = 0.06, size = 3.5, shape = 21) + 
  geom_text(
    data = counts_layers_grass,
    aes(x = part_of_pre_def_layer, label = paste0("n = ", n)),
    y = Inf, 
    vjust = -4.1,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = mean_SOC_layers_grass,
    aes(x = part_of_pre_def_layer,
    label = label),
    parse = TRUE,
    y = Inf, 
    vjust = -2.3,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = median_SOC_layers_grass,
    aes(x = part_of_pre_def_layer,
    label = label),
    parse = TRUE,
    y = Inf,
    vjust = -0.5,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", 
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 14, family = "sans"),
        axis.title.y = element_text(size = 14, family = "sans"),
        axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
        axis.text.y  = element_text(size = 14, family = "sans", color = "black")
  ) +
  labs(x = "Layer depth [cm]\nGrassland", y = "SOC [g / cm³]") +
  coord_cartesian(ylim = c(0, 0.4), clip = "off") +
  theme(
    plot.margin = margin(t = 75, r = 5, b = 5, l = 5) 
  ) +
  scale_fill_manual(values = sl_colors) +
  scale_color_manual(values = sl_colors)


plot_SOC_layers_wet <- ggplot(layers_wet, aes(x = part_of_pre_def_layer, y = `SOC_g_cm3`)) +
  scale_x_discrete(drop = FALSE) +
  geom_boxplot(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.7,
    linetype = "dashed",
    linewidth = 0.2, 
    alpha = 0.5,
    color = "grey30"
  ) +
  stat_boxplot(
    geom = "errorbar",
    width = 0.4,
    linewidth = 0.3,
    color = "grey10",
  )+
  geom_jitter(aes(fill = factor(part_of_pre_def_layer)), color = "grey10", width = 0.06, size = 3.5, shape = 21) + 
  geom_text(
    data = counts_layers_wet,
    aes(x = part_of_pre_def_layer, label = paste0("n = ", n)),
    y = Inf, 
    vjust = -4.1,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = mean_SOC_layers_wet,
    aes(x = part_of_pre_def_layer,
    label = label),
    parse = TRUE,
    y = Inf, 
    vjust = -2.3,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = median_SOC_layers_wet,
    aes(x = part_of_pre_def_layer,
    label = label),
    parse = TRUE,
    y = Inf,
    vjust = -0.5,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", 
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 14, family = "sans"),
        axis.title.y = element_text(size = 14, family = "sans"),
        axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
        axis.text.y  = element_text(size = 14, family = "sans", color = "black")
  ) +
  labs(x = "Layer depth [cm]\nWetland", y = "SOC [g / cm³]") +
  coord_cartesian(ylim = c(0, 0.4), clip = "off") +
  theme(
    plot.margin = margin(t = 75, r = 5, b = 5, l = 5) 
  ) +
  scale_fill_manual(values = sl_colors) +
  scale_color_manual(values = sl_colors)

# Merge single plots to two combined plots
plot_SOC_layers_ntyp_1v2 <- (((plot_SOC_layers_wet | plot_SOC_layers_deci) + plot_layout(widths = c(1, 1))) / 
                            (plot_SOC_layers_spru | plot_SOC_layers_pine)  + plot_layout(widths = c(1, 1)))


plot_SOC_layers_ntyp_2v2 <- (((plot_SOC_layers_shru | plot_SOC_layers_grass) + plot_layout(widths = c(1, 1))) /
                            (plot_SOC_layers_urban | plot_spacer()) + plot_layout(widths = c(1.5, 0.5)))

ggsave(
  "plot_SOC_layers_ntyp_1v2.png",
  plot = plot_SOC_layers_ntyp_1v2,
  width = 10, 
  height = 10, 
  dpi = 300
)

ggsave(
  "plot_SOC_layers_ntyp_2v2.png",
  plot = plot_SOC_layers_ntyp_2v2,
  width = 10, 
  height = 10, 
  dpi = 300
)


####
#### PLOT RESULT 3: SOC stock [kg/m2] per soil sample

## Prepare plot data
sample_points$nature_type_reevaluation[
  sample_points$nature_type_reevaluation == "Urban"
] <- "Built Area"

sample_points$nature_type_reevaluation <- factor(
  sample_points$nature_type_reevaluation,
  levels = c("Wetland", "Deciduous Forest", "Spruce Forest", "Pine Forest", "Shrubs", "Grassland", "Built Area")
)

nt_groups <- length(levels(sample_points$nature_type_reevaluation))
batlow_bamako <- scico(n = nt_groups, palette = "bamako")

counts_all_samples <- sample_points %>%
  group_by(nature_type_reevaluation) %>%
  summarise(n = n())
mean_SOC_all_samples_kg_m2 <- sample_points %>%
  group_by(nature_type_reevaluation) %>%
  summarise(
    mean_val = mean(SOC_stock_sample_var1_kg_m2, na.rm = TRUE)
  )
median_SOC_all_samples_kg_m2 <- sample_points %>%
  group_by(nature_type_reevaluation) %>%
  summarise(
    median_val = median(SOC_stock_sample_var1_kg_m2, na.rm = TRUE)
  )

## Plot data for result 3: SOC stock per unit area [kg/m²] per nature type group
plot_SOC_all_samples_kg_m2 <- ggplot(sample_points, aes(x = nature_type_reevaluation, y = SOC_stock_sample_var1_kg_m2)) +
  geom_boxplot(aes(fill = factor(nature_type_reevaluation)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.3) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.7,
    linetype = "dashed",
    linewidth = 0.2, 
    alpha = 0.5,
    color = "grey30"
  ) +
  stat_boxplot(
    geom = "errorbar",
    width = 0.4,
    linewidth = 0.3,
    color = "grey10",
  )+
  geom_jitter(aes(fill = factor(nature_type_reevaluation)), color = "grey10", width = 0.06, size = 2.5, shape = 21) + 
   geom_text(
    data = counts_all_samples,
    aes(x = nature_type_reevaluation, label = paste0("n = ", n)),
    y = Inf, 
    vjust = -4.1,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = mean_SOC_all_samples_kg_m2,
    aes(x = nature_type_reevaluation),
    label = sapply(mean_SOC_all_samples_kg_m2$mean_val, function(x) bquote(bar(x) == .(sprintf("%.1f", x)))),
    parse = TRUE,
    y = Inf, 
    vjust = -2.3,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  geom_text(
    data = median_SOC_all_samples_kg_m2,
    aes(x = nature_type_reevaluation),
    label = sapply(median_SOC_all_samples_kg_m2$median_val, function(x) bquote(tilde(x) == .(sprintf("%.1f", x)))),
    parse = TRUE,
    y = Inf,
    vjust = -0.5,
    inherit.aes = FALSE,
    color = "black",
    size = 4.3
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        axis.line = element_line(color = "black", linewidth = 0.6),
        axis.ticks = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 14, family = "sans"),
        axis.title.y = element_text(size = 14, family = "sans"),
        axis.text.x  = element_text(size = 14, family = "sans", color = "black", angle = 45, hjust = 1),
        axis.text.y  = element_text(size = 14, family = "sans", color = "black")
  ) +
  labs(x = paste0("Nature type group"), y = "SOC stock [kg/m²]") +  # 
  coord_cartesian(ylim = c(0, 210), clip = "off") +
  theme(plot.margin = margin(t = 65, r = 5, b = 5, l = 5)) +
  scale_fill_manual(values = batlow_bamako) +
  scale_color_manual(values = batlow_bamako)


  ## PLOT SAVE
  ggsave(
    "plot_SOC_all_samples_kg_m2.png",
    plot_SOC_all_samples_kg_m2,
    width = 160/25.4,
    height = 160/25.4,
    units = "in",
    dpi = 300
  )

  
  
  ####
  #### PLOT RESULT 4: SOC stock per unit area [kg/m2] per Ecobudgets site following calculation variant 1 displayed for each dominating nature type
  
  ## Prepare plot data
  eco_points$main_nature_type[
    eco_points$main_nature_type == "Urban"
  ] <- "Built Area"
  
  eco_points$main_nature_type <- factor(
    eco_points$main_nature_type,
    levels = c("Wetland", "Deciduous Forest", "Spruce Forest", "Pine Forest", "Shrubs", "Grassland", "Built Area")
  )
  
  mnt_groups <- length(levels(eco_points$main_nature_type))
  batlow_bamako2 <- scico(n = mnt_groups, palette = "bamako")
  
  counts_ecop_var1 <- eco_points %>%
    group_by(main_nature_type) %>%
    summarise(n = n())
  
  n_eco_points <- length(eco_points$plot_ID)
  
  mean_SOC_ecop_var1_kg_m2 <- eco_points %>%
    group_by(main_nature_type) %>%
    summarise(
      mean_val = mean(SOC_stock_ecop_var1_kg_m2, na.rm = TRUE)
    )
  
  median_SOC_ecop_var1_kg_m2 <- eco_points %>%
    group_by(main_nature_type) %>%
    summarise(
      median_val = median(SOC_stock_ecop_var1_kg_m2, na.rm = TRUE)
    )
  
  
  
  ## Plot data for result 4: SOC stock per unit area [kg/m2] per Ecobudgets site following calculation variant 1 displayed for each dominating nature type
  plot_SOC_all_ecop_kg_m2 <- ggplot(eco_points, aes(x = main_nature_type, y = SOC_stock_ecop_var1_kg_m2)) +
    geom_boxplot(aes(fill = factor(main_nature_type)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.3) +
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.7,
      linetype = "dashed",
      linewidth = 0.2, 
      alpha = 0.5,
      color = "grey30"
    ) +
    stat_boxplot(
      geom = "errorbar",
      width = 0.4,
      linewidth = 0.3,
      color = "grey10",
    )+
    geom_jitter(aes(fill = factor(main_nature_type)), color = "grey10", width = 0.06, size = 2.5, shape = 21) + 
    geom_text(
      data = counts_ecop_var1,
      aes(x = main_nature_type, label = paste0("n = ", n)),
      y = Inf, 
      vjust = -4.1,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    geom_text(
      data = mean_SOC_ecop_var1_kg_m2,
      aes(x = main_nature_type),
      label = sapply(mean_SOC_ecop_var1_kg_m2$mean_val, function(x) bquote(bar(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf, 
      vjust = -2.3,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    geom_text(
      data = median_SOC_ecop_var1_kg_m2,
      aes(x = main_nature_type),
      label = sapply(median_SOC_ecop_var1_kg_m2$median_val, function(x) bquote(tilde(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf,
      vjust = -0.5,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          axis.line = element_line(color = "black", linewidth = 0.6),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 14, family = "sans"),
          axis.title.y = element_text(size = 14, family = "sans"),
          axis.text.x  = element_text(size = 14, family = "sans", color = "black", angle = 45, hjust = 1),
          axis.text.y  = element_text(size = 14, family = "sans", color = "black")
    ) +
    labs(x = paste0("Dominant nature type group of 10x10 m square"), y = "SOC stock / Var 1 [kg/m²]") +  # 
    coord_cartesian(ylim = c(0, 210), clip = "off") +
    theme(plot.margin = margin(t = 65, r = 5, b = 5, l = 5)) +
    scale_fill_manual(values = batlow_bamako2) +
    scale_color_manual(values = batlow_bamako2)

  
  ## PLOT SAVE
  ggsave(
    "plot_SOC_all_ecop_kg_m2.png",
    plot_SOC_all_ecop_kg_m2,
    width = 160/25.4,
    height = 160/25.4,
    units = "in",
    dpi = 300
  )
  
  
  ####
  #### PLOT RESULT 5: SOC [g/cm2] per Ecobudgets point per dominating nature type in 4 different calculation Variants
  
  ## Prepare plot data
  eco_points_long_raw <- eco_points %>%
    pivot_longer(
      cols = tidyselect::matches("^SOC_stock_ecop_var.*_g_cm2$"),
      names_to = "SOC_stock_variant",
      values_to = "SOC_stock_ecop_g_cm2"
    )%>%
    mutate(
      SOC_stock_variant = SOC_stock_variant %>%
        str_remove("^SOC_stock_ecop_") %>% 
        str_remove("_g_cm2$") 
    )
  
  eco_points_long_mean <- eco_points %>%
    pivot_longer(
      cols = tidyselect::matches("^SOC_stock_ecop_var.*_g_cm2_mean$"),
      names_to = "SOC_stock_variant",
      values_to = "SOC_stock_ecop_mean_g_cm2"
    ) %>%
    mutate(
      SOC_stock_variant = SOC_stock_variant %>%
        str_remove("^SOC_stock_ecop_") %>% 
        str_remove("_g_cm2_mean$") 
    ) %>%
    select(plot_ID, SOC_stock_variant, SOC_stock_ecop_mean_g_cm2)
  
  eco_points_long_median <- eco_points %>%
    pivot_longer(
      cols = tidyselect::matches("^SOC_stock_ecop_var.*_g_cm2_median$"),
      names_to = "SOC_stock_variant",
      values_to = "SOC_stock_ecop_median_g_cm2"
    ) %>%
    mutate(
      SOC_stock_variant = SOC_stock_variant %>%
        str_remove("^SOC_stock_ecop_") %>% 
        str_remove("_g_cm2_median$") 
    ) %>%
    select(plot_ID, SOC_stock_variant, SOC_stock_ecop_median_g_cm2)
  
  eco_points_long <- eco_points_long_raw %>%
    left_join(eco_points_long_mean, by = c("plot_ID", "SOC_stock_variant")) %>%
    left_join(eco_points_long_median, by = c("plot_ID", "SOC_stock_variant"))%>%
    select(plot_ID, main_nature_type, n_main_nature_type, SOC_stock_variant, SOC_stock_ecop_g_cm2, SOC_stock_ecop_mean_g_cm2, SOC_stock_ecop_median_g_cm2)
  
  eco_points_long$SOC_stock_variant <- factor(
    eco_points_long$SOC_stock_variant,
    levels = c("var1", "var2", "var3", "var4"),   # original values
    labels = c("Var 1", "Var 2", "Var 3", "Var 4")  # x-axis labels
  )
  
  eco_points_long <- eco_points_long %>%
    mutate(
      SOC_stock_ecop_kg_m2 = SOC_stock_ecop_g_cm2 * 10,
      SOC_stock_ecop_mean_kg_m2 = SOC_stock_ecop_mean_g_cm2 * 10,
      SOC_stock_ecop_median_kg_m2 = SOC_stock_ecop_median_g_cm2 * 10
    )
  
  eco_points_deci <- eco_points_long %>%
    filter(main_nature_type == "Deciduous Forest")
  eco_points_pine <- eco_points_long %>%
    filter(main_nature_type == "Pine Forest")
  eco_points_spru <- eco_points_long %>%
    filter(main_nature_type == "Spruce Forest")
  eco_points_shru <- eco_points_long %>%
    filter(main_nature_type == "Shrubs")
  eco_points_grass <- eco_points_long %>%
    filter(main_nature_type == "Grassland")
  eco_points_wet <- eco_points_long %>%
    filter(main_nature_type == "Wetland")
  eco_points_urban <- eco_points_long %>%
    filter(main_nature_type == "Built Area")
  
  n_main_nature_type_deci <- eco_points_deci$n_main_nature_type[1]
  n_main_nature_type_pine <- eco_points_pine$n_main_nature_type[1]
  n_main_nature_type_spru <- eco_points_spru$n_main_nature_type[1]
  n_main_nature_type_shru <- eco_points_shru$n_main_nature_type[1]
  n_main_nature_type_grass <- eco_points_grass$n_main_nature_type[1]
  n_main_nature_type_wet <- eco_points_wet$n_main_nature_type[1]
  n_main_nature_type_urban <- eco_points_urban$n_main_nature_type[1]
  
  var_groups <- length(levels(eco_points_long$SOC_stock_variant))
  batlow_batlow <- scico(n = var_groups, palette = "batlow", begin = 0.15, end = 0.7)
  
  ## Plot data for result 6: SOC stock per unit area [g/cm2] per Ecobudgets point per dominating nature type group in 4 different calculation Variants
  plot_SOC_ecop_deci <- ggplot(eco_points_deci, aes(x = SOC_stock_variant, y = SOC_stock_ecop_kg_m2)) +
    geom_boxplot(aes(fill = factor(SOC_stock_variant)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.7,
      linetype = "dashed",
      linewidth = 0.2, 
      alpha = 0.5,
      color = "grey30"
    ) +
    stat_boxplot(
      geom = "errorbar",
      width = 0.4,
      linewidth = 0.3,
      color = "grey10",
    )+
    geom_jitter(aes(fill = factor(SOC_stock_variant)), color = "grey10", width = 0.06, size = 2.5, shape = 21) + 
    geom_text(
      data = eco_points_deci,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_deci$SOC_stock_ecop_mean_kg_m2, function(x) bquote(bar(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf, 
      vjust = -2.3,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    geom_text(
      data = eco_points_deci,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_deci$SOC_stock_ecop_median_kg_m2, function(x) bquote(tilde(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf,
      vjust = -0.5,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          axis.line = element_line(color = "black", linewidth = 0.6),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 14, family = "sans"),
          axis.title.y = element_text(size = 14, family = "sans"),
          axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
          axis.text.y  = element_text(size = 14, family = "sans", color = "black")
    ) +
    labs(x = paste0("Deciduous Forest (n = ", n_main_nature_type_deci, ")"), y = "SOC stock [kg/m²]") +
    coord_cartesian(ylim = c(0, 170), clip = "off") +
    theme(plot.margin = margin(t = 65, r = 5, b = 5, l = 5)) +
    scale_fill_manual(values = batlow_batlow) +
    scale_color_manual(values = batlow_batlow)
  
  
  plot_SOC_ecop_pine <- ggplot(eco_points_pine, aes(x = SOC_stock_variant, y = SOC_stock_ecop_kg_m2)) +
    geom_boxplot(aes(fill = factor(SOC_stock_variant)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.7,
      linetype = "dashed",
      linewidth = 0.2, 
      alpha = 0.5,
      color = "grey30"
    ) +
    stat_boxplot(
      geom = "errorbar",
      width = 0.4,
      linewidth = 0.3,
      color = "grey10",
    )+
    geom_jitter(aes(fill = factor(SOC_stock_variant)), color = "grey10", width = 0.06, size = 2.5, shape = 21) + 
    geom_text(
      data = eco_points_pine,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_pine$SOC_stock_ecop_mean_kg_m2, function(x) bquote(bar(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf, 
      vjust = -2.3,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    geom_text(
      data = eco_points_pine,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_pine$SOC_stock_ecop_median_kg_m2, function(x) bquote(tilde(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf,
      vjust = -0.5,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          axis.line = element_line(color = "black", linewidth = 0.6),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 14, family = "sans"),
          axis.title.y = element_text(size = 14, family = "sans"),
          axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
          axis.text.y  = element_text(size = 14, family = "sans", color = "black")
    ) +
    labs(x = paste0("Pine Forest (n = ", n_main_nature_type_pine, ")"), y = "SOC stock [kg/m²]") +
    coord_cartesian(ylim = c(0, 170), clip = "off") +
    theme(plot.margin = margin(t = 65, r = 5, b = 5, l = 5)) +
    scale_fill_manual(values = batlow_batlow) +
    scale_color_manual(values = batlow_batlow)
  
  
  plot_SOC_ecop_spru <- ggplot(eco_points_spru, aes(x = SOC_stock_variant, y = SOC_stock_ecop_kg_m2)) +
    geom_boxplot(aes(fill = factor(SOC_stock_variant)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.7,
      linetype = "dashed",
      linewidth = 0.2, 
      alpha = 0.5,
      color = "grey30"
    ) +
    stat_boxplot(
      geom = "errorbar",
      width = 0.4,
      linewidth = 0.3,
      color = "grey10",
    )+
    geom_jitter(aes(fill = factor(SOC_stock_variant)), color = "grey10", width = 0.06, size = 2.5, shape = 21) + 
    geom_text(
      data = eco_points_spru,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_spru$SOC_stock_ecop_mean_kg_m2, function(x) bquote(bar(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf, 
      vjust = -2.3,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    geom_text(
      data = eco_points_spru,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_spru$SOC_stock_ecop_median_kg_m2, function(x) bquote(tilde(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf,
      vjust = -0.5,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          axis.line = element_line(color = "black", linewidth = 0.6),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 14, family = "sans"),
          axis.title.y = element_text(size = 14, family = "sans"),
          axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
          axis.text.y  = element_text(size = 14, family = "sans", color = "black")
    ) +
    labs(x = paste0("Spruce Forest (n = ", n_main_nature_type_spru, ")"), y = "SOC stock [kg/m²]") +
    coord_cartesian(ylim = c(0, 170), clip = "off") +
    theme(plot.margin = margin(t = 65, r = 5, b = 5, l = 5)) +
    scale_fill_manual(values = batlow_batlow) +
    scale_color_manual(values = batlow_batlow)
  
  
  plot_SOC_ecop_shru <- ggplot(eco_points_shru, aes(x = SOC_stock_variant, y = SOC_stock_ecop_kg_m2)) +
    geom_boxplot(aes(fill = factor(SOC_stock_variant)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.7,
      linetype = "dashed",
      linewidth = 0.2, 
      alpha = 0.5,
      color = "grey30"
    ) +
    stat_boxplot(
      geom = "errorbar",
      width = 0.4,
      linewidth = 0.3,
      color = "grey10",
    )+
    geom_jitter(aes(fill = factor(SOC_stock_variant)), color = "grey10", width = 0.06, size = 2.5, shape = 21) + 
    geom_text(
      data = eco_points_shru,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_shru$SOC_stock_ecop_mean_kg_m2, function(x) bquote(bar(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf, 
      vjust = -2.3,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    geom_text(
      data = eco_points_shru,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_shru$SOC_stock_ecop_median_kg_m2, function(x) bquote(tilde(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf,
      vjust = -0.5,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          axis.line = element_line(color = "black", linewidth = 0.6),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 14, family = "sans"),
          axis.title.y = element_text(size = 14, family = "sans"),
          axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
          axis.text.y  = element_text(size = 14, family = "sans", color = "black")
    ) +
    labs(x = paste0("Shrubs (n = ", n_main_nature_type_shru, ")"), y = "SOC stock [kg/m²]") +
    coord_cartesian(ylim = c(0, 170), clip = "off") +
    theme(plot.margin = margin(t = 65, r = 5, b = 5, l = 5)) +
    scale_fill_manual(values = batlow_batlow) +
    scale_color_manual(values = batlow_batlow)
  
  
  plot_SOC_ecop_grass <- ggplot(eco_points_grass, aes(x = SOC_stock_variant, y = SOC_stock_ecop_kg_m2)) +
    geom_boxplot(aes(fill = factor(SOC_stock_variant)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.7,
      linetype = "dashed",
      linewidth = 0.2, 
      alpha = 0.5,
      color = "grey30"
    ) +
    stat_boxplot(
      geom = "errorbar",
      width = 0.4,
      linewidth = 0.3,
      color = "grey10",
    )+
    geom_jitter(aes(fill = factor(SOC_stock_variant)), color = "grey10", width = 0.06, size = 2.5, shape = 21) + 
    geom_text(
      data = eco_points_grass,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_grass$SOC_stock_ecop_mean_kg_m2, function(x) bquote(bar(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf, 
      vjust = -2.3,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    geom_text(
      data = eco_points_grass,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_grass$SOC_stock_ecop_median_kg_m2, function(x) bquote(tilde(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf,
      vjust = -0.5,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          axis.line = element_line(color = "black", linewidth = 0.6),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 14, family = "sans"),
          axis.title.y = element_text(size = 14, family = "sans"),
          axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
          axis.text.y  = element_text(size = 14, family = "sans", color = "black")
    ) +
    labs(x = paste0("Grassland (n = ", n_main_nature_type_grass, ")"), y = "SOC stock [kg/m²]") +
    coord_cartesian(ylim = c(0, 170), clip = "off") +
    theme(plot.margin = margin(t = 65, r = 5, b = 5, l = 5)) +
    scale_fill_manual(values = batlow_batlow) +
    scale_color_manual(values = batlow_batlow)
  
  
  plot_SOC_ecop_wet <- ggplot(eco_points_wet, aes(x = SOC_stock_variant, y = SOC_stock_ecop_kg_m2)) +
    geom_boxplot(aes(fill = factor(SOC_stock_variant)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.7,
      linetype = "dashed",
      linewidth = 0.2, 
      alpha = 0.5,
      color = "grey30"
    ) +
    stat_boxplot(
      geom = "errorbar",
      width = 0.4,
      linewidth = 0.3,
      color = "grey10",
    )+
    geom_jitter(aes(fill = factor(SOC_stock_variant)), color = "grey10", width = 0.06, size = 2.5, shape = 21) + 
    geom_text(
      data = eco_points_wet,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_wet$SOC_stock_ecop_mean_kg_m2, function(x) bquote(bar(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf, 
      vjust = -2.3,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    geom_text(
      data = eco_points_wet,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_wet$SOC_stock_ecop_median_kg_m2, function(x) bquote(tilde(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf,
      vjust = -0.5,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          axis.line = element_line(color = "black", linewidth = 0.6),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 14, family = "sans"),
          axis.title.y = element_text(size = 14, family = "sans"),
          axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
          axis.text.y  = element_text(size = 14, family = "sans", color = "black")
    ) +
    labs(x = paste0("Wetland (n = ", n_main_nature_type_wet, ")"), y = "SOC stock [kg/m²]") +
    coord_cartesian(ylim = c(0, 170), clip = "off") +
    theme(plot.margin = margin(t = 65, r = 5, b = 5, l = 5)) +
    scale_fill_manual(values = batlow_batlow) +
    scale_color_manual(values = batlow_batlow)
  
  
  plot_SOC_ecop_urban <- ggplot(eco_points_urban, aes(x = SOC_stock_variant, y = SOC_stock_ecop_kg_m2)) +
    geom_boxplot(aes(fill = factor(SOC_stock_variant)), color = "grey10", median.colour = "grey30",  width = 0.7, outlier.shape = NA, linewidth = 0.3, alpha = 0.5) +
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.7,
      linetype = "dashed",
      linewidth = 0.2, 
      alpha = 0.5,
      color = "grey30"
    ) +
    stat_boxplot(
      geom = "errorbar",
      width = 0.4,
      linewidth = 0.3,
      color = "grey10",
    )+
    geom_jitter(aes(fill = factor(SOC_stock_variant)), color = "grey10", width = 0.06, size = 2.5, shape = 21) + 
    geom_text(
      data = eco_points_urban,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_urban$SOC_stock_ecop_mean_kg_m2, function(x) bquote(bar(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf, 
      vjust = -2.3,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    geom_text(
      data = eco_points_urban,
      aes(x = SOC_stock_variant),
      label = sapply(eco_points_urban$SOC_stock_ecop_median_kg_m2, function(x) bquote(tilde(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf,
      vjust = -0.5,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          axis.line = element_line(color = "black", linewidth = 0.6),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 14, family = "sans"),
          axis.title.y = element_text(size = 14, family = "sans"),
          axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
          axis.text.y  = element_text(size = 14, family = "sans", color = "black")
    ) +
    labs(x = paste0("Built Area (n = ", n_main_nature_type_urban, ")"), y = "SOC stock [kg/m²]") +
    coord_cartesian(ylim = c(0, 170), clip = "off") +
    theme(plot.margin = margin(t = 65, r = 5, b = 5, l = 5)) +
    scale_fill_manual(values = batlow_batlow) +
    scale_color_manual(values = batlow_batlow)
  
  
  plot_SOC_ecop_all_1v2 <- (((plot_SOC_ecop_wet | plot_SOC_ecop_deci) + plot_layout(widths = c(1, 1))) / 
                              (plot_SOC_ecop_spru | plot_SOC_ecop_pine)  + plot_layout(widths = c(1, 1)))
  
  
  plot_SOC_ecop_all_2v2 <- (((plot_SOC_ecop_shru | plot_SOC_ecop_grass) + plot_layout(widths = c(1, 1))) /
                              (plot_SOC_ecop_urban | plot_spacer()) + plot_layout(widths = c(1.5, 0.5)))
  
  ggsave(
    "plot_SOC_ecop_all_1v2.png",
    plot = plot_SOC_ecop_all_1v2,
    width = 8, 
    height = 9, 
    dpi = 300
  )
  
  ggsave(
    "plot_SOC_ecop_all_2v2.png",
    plot = plot_SOC_ecop_all_2v2,
    width = 8, 
    height = 9, 
    dpi = 300
  )
  
  
  
  ####
  #### PLOT RESULT 6: SOC stock per unit area [kg/m2] per Ecobudgets point in 4 different calculation Variants
  
  ## Prepare plot data
  counts_all_var <- eco_points_long %>%
    group_by(SOC_stock_variant) %>%
    summarise(n = n())
  
  n_eco_points <- length(eco_points$plot_ID)
  
  mean_SOC_all_var_kg_m2 <- eco_points_long %>%
    group_by(SOC_stock_variant) %>%
    summarise(
      mean_val = mean(SOC_stock_ecop_kg_m2, na.rm = TRUE)
    )
  
  median_SOC_all_var_kg_m2 <- eco_points_long %>%
    group_by(SOC_stock_variant) %>%
    summarise(
      median_val = median(SOC_stock_ecop_kg_m2, na.rm = TRUE)
    )
  
  ## Plot data for result 5: SOC [kg/m2] per Ecobudgets point in 4 different calculation Variants
  plot_SOC_all_var_kg_m2 <- ggplot(eco_points_long, aes(x = SOC_stock_variant, y = SOC_stock_ecop_kg_m2)) +
    geom_boxplot(aes(fill = factor(SOC_stock_variant)), color = "grey10",  width = 0.6, outlier.shape = 5, outlier.size = 2.5, linewidth = 0.5) +
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.6,
      linetype = "dashed",
      linewidth = 0.2, 
      color = "grey10"
    ) +
    geom_segment(            # If needed according to result in CODE CHAPTER 6.
      aes(x = 1, xend = 4, y = 105, yend = 105),
      arrow = arrow(length = unit(0.25, "cm"), ends = "both", type = "closed"),
      linewidth = 0.4
    ) +
    annotate(
      "text",
      x = 2.5,
      y = 112,
      label = "r = 0.70",    # Value needs to be added according to results in CODE CHAPTER 6.
      size = 4
    ) +
    geom_segment(            # If needed according to result in CODE CHAPTER 6.
      aes(x = 2, xend = 4, y = 78, yend = 78),
      arrow = arrow(length = unit(0.25, "cm"), ends = "both", type = "closed"),
      linewidth = 0.4
    ) +
    annotate(
      "text",
      x = 2.5,
      y = 85,
      label = "r = 0.80",    # Value needs to be added according to results in CODE CHAPTER 6.
      size = 4
    ) +
    geom_segment(            # If needed according to result in CODE CHAPTER 6.
      aes(x = 2, xend = 3, y = 58, yend = 58),
      arrow = arrow(length = unit(0.25, "cm"), ends = "both", type = "closed"),
      linewidth = 0.4,
      linetype = "longdash"
    ) +
    annotate(
      "text",
      x = 2.5,
      y = 65,
      label = "r = 0.44",    # Value needs to be added according to results in CODE CHAPTER 6.
      size = 4
    ) +
    geom_text(
      data = mean_SOC_all_var_kg_m2,
      aes(x = SOC_stock_variant),
      label = sapply(mean_SOC_all_var_kg_m2$mean_val, function(x) bquote(bar(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf, 
      vjust = -2.3,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    geom_text(
      data = median_SOC_all_var_kg_m2,
      aes(x = SOC_stock_variant),
      label = sapply(median_SOC_all_var_kg_m2$median_val, function(x) bquote(tilde(x) == .(sprintf("%.1f", x)))),
      parse = TRUE,
      y = Inf,
      vjust = -0.5,
      inherit.aes = FALSE,
      color = "black",
      size = 4.3
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          axis.line = element_line(color = "black", linewidth = 0.6),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),
          axis.title.x = element_text(size = 14, family = "sans"),
          axis.title.y = element_text(size = 14, family = "sans"),
          axis.text.x  = element_text(size = 14, family = "sans", color = "black"),
          axis.text.y  = element_text(size = 14, family = "sans", color = "black")
    ) +
    labs(x = paste0("Ecobudgets points (n = ", n_eco_points, ")"), y = "SOC stock [kg/m²]") +
    coord_cartesian(ylim = c(0, 170), clip = "off") +
    theme(plot.margin = margin(t = 65, r = 5, b = 5, l = 5)) +
    scale_fill_manual(values = batlow_batlow) +
    scale_color_manual(values = batlow_batlow)
  
  ## PLOT SAVE
  ggsave(
    "plot_SOC_all_var_kg_m2.png",
    plot_SOC_all_var_kg_m2,
    width = 160/25.4,
    height = 120/25.4,
    units = "in",
    dpi = 300
  )
  
  
  
  ##########################################################################################################################################
  #### 6. STATISTICAL TESTING OF VARIANTS - rmANOVA assumptions -> FRIEDMAN + POST-HOC (Eisinga + Bonferroni) + EFFECT SIZE (Cohen) 
  ##########################################################################################################################################
  
  ####
  #### DATA PREPARATION
  
  statdata_long <- eco_points_long %>%
    select(plot_ID, SOC_stock_variant, SOC_stock_ecop_kg_m2) %>%  
    mutate(
      plot_ID = factor(plot_ID, levels = unique(plot_ID)),  
      SOC_stock_variant = factor(SOC_stock_variant, levels = c("Var 1", "Var 2", "Var 3", "Var 4"))  
    ) %>%
    arrange(plot_ID, SOC_stock_variant) 
  
  ## NOT MANDATORY INFO
  
  statdata_long %>%
    group_by(SOC_stock_variant) %>%
    summarize(M = mean(SOC_stock_ecop_kg_m2),
              SD = sd(SOC_stock_ecop_kg_m2)) %>%
    as.data.frame()
  
  boxplot(statdata_long$SOC_stock_ecop_kg_m2~statdata_long$SOC_stock_variant)
  
  
  ####
  #### NORMALITY CHECK for rmANOVA
  
  ggqqplot(statdata_long, "SOC_stock_ecop_kg_m2", facet.by = "SOC_stock_variant")
  
  normality <- statdata_long %>%
    group_by(SOC_stock_variant) %>%
    summarise(
      sw_p = shapiro.test(SOC_stock_ecop_kg_m2)$p.value,
      ad_p = ad.test(SOC_stock_ecop_kg_m2)$p.value,
      .groups = "drop"
    )
  
  
  ## Mauchlys test of sphericity
  
  ez_model <- ezANOVA(
    data = statdata_long,
    dv = SOC_stock_ecop_kg_m2,
    wid = plot_ID,
    within = SOC_stock_variant,
    detailed = TRUE
  )
  
  ez_model$Mauchly
  
  
  ####
  #### FRIEDMAN TEST (because normal distribution is not the case)
  
  friedman_result <- friedman.test(statdata_long$SOC_stock_ecop_kg_m2, statdata_long$SOC_stock_variant, statdata_long$plot_ID)
  
  ## Effect size
  friedmann_effect_size <- friedman_effsize(statdata_long, SOC_stock_ecop_kg_m2 ~ SOC_stock_variant | plot_ID)
  
  
  ####
  #### POST HOC TEST - Exact-p-value-method with Bonferroni correction
  
  posthoc_exact_bonf_result <- frdAllPairsExactTest(statdata_long$SOC_stock_ecop_kg_m2, statdata_long$SOC_stock_variant, statdata_long$plot_ID, 
                                                    p.adjust.method = "bonferroni")
  
  
  ## POST HOC TEST for EFFECZ SIZE - Exact-p-value-method without correction
  
  posthoc_exact_none_result <- frdAllPairsExactTest(statdata_long$SOC_stock_ecop_kg_m2, statdata_long$SOC_stock_variant, statdata_long$plot_ID, 
                                                    p.adjust.method = "none")
  
  z13 <- qnorm(posthoc_exact_none_result$p.value[2]/2)
  z14 <- qnorm(posthoc_exact_none_result$p.value[3]/2)
  z23 <- qnorm(posthoc_exact_none_result$p.value[5]/2)
  z24 <- qnorm(posthoc_exact_none_result$p.value[6]/2)
  
  n <- rstatix::friedman_test(data = statdata_long, formula = SOC_stock_ecop_kg_m2 ~ SOC_stock_variant | plot_ID)$n
  
  r13 <- z13/sqrt(n)
  r14 <- z14/sqrt(n)
  r23 <- z23/sqrt(n)
  r24 <- z24/sqrt(n)
  

  ##########################################################################################################################################
  #### 7. DATA EXPORT 
  ##########################################################################################################################################
  
  
  ## create a new file and Write updated soil_layers to new sheets, while keeping the origianl sheets
  wb <- wb_load("02_Data_Soil_Samples_Ecobudgets_CS_R-import.xlsx")
  wb <- wb %>%
    wb_add_worksheet(sheet = "soil_layers_analysis") %>%
    wb_add_data(sheet = "soil_layers_analysis", x = soil_layers)
  wb <- wb %>%
    wb_add_worksheet(sheet = "sample_points_analysis") %>%
    wb_add_data(sheet = "sample_points_analysis", x = sample_points)
  wb <- wb %>%
    wb_add_worksheet(sheet = "eco_points_analysis") %>%
    wb_add_data(sheet = "eco_points_analysis", x = eco_points)

  ## save the new file into the work directory with the given name
  wb_save(wb, "Soil_Samples_Ecobudgets_analysis_R-export.xlsx", overwrite = TRUE)


