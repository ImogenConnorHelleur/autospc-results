################################################################################
# This script applies various approaches to (re)calculation of control limits  #
# to NHS England A&E data. This is achieved through the following steps:       #
# 1. Derive and clean time series data from A&E data files produced by the     #
#   nhsAEscraper package. This optional step can be skipped (load the result)  #                                                 #
# 2. Apply the following four approaches to each resulting weekly/monthly      #
#   timeseries:                                                                #
#     a) The stable shift algorithm - with noRegrets and overhangingReversions #
#     b) "Naive1" - limits derived from the whole timeseries                   #
#     c) "Naive2" - limits derived from baseline and extended                  #
#     d) "Naive3" - limits recalculated at every shift rule break, subject to  #
#       the minimum number of points required to form limits                   #
#     e) "Naive3b" - limits recalculated at every shift rule break, with no    #
#       minimum number of points to form limits
# 3. The results are then combined and saved, in raw and summarised form.      #
################################################################################

if(FALSE) { # Use timestamp and SHA in filenames?
  # TRUE - use if not committing outputs to git
  timestamp <- gsub(":","-",paste(strsplit(x = toString(Sys.time()),split = " ")[[1]], collapse = "-"))
  commit <- stringr::str_sub(system("git rev-parse HEAD", intern=TRUE), 1, 8)
  filename_suffix <- paste0("_",
                            timestamp,
                            "_",
                            commit)
} else {
  # FALSE - use if committing outputs to git
  filename_suffix <- ""
}

library(tidyverse)
library(scales)

#devtools::install_github("HorridTom/autospc", ref = "naive3-argument-103b")
library(autospc)

source("R/get_perf_series_df.R")
source("R/results_functions.R")

AE_Data <- readRDS("data/AE_Data.RDS")
AE_Data_Scot <- readRDS("data/AE_Data_Scot.RDS")

#change Nat_Code to make it distinct from Lothian Reg_Code (S)
AE_Data <- AE_Data %>%
  mutate(Nat_Code = if_else(Nat_Code == "S", "Sc", Nat_Code))

AE_Data_Scot <- AE_Data_Scot %>%
  mutate(Nat_Code = if_else(Nat_Code == "S", "Sc", Nat_Code))

if(FALSE) { # SLOW
  perf_series_df <- make_perf_series_data_frame(data = AE_Data,
                                                data_scot = AE_Data_Scot,
                                                measureArg = "All",
                                                onlyProvsReportingArg = TRUE)
  
  perf_series_df_filename <- paste0("perf_series_df",
                                    filename_suffix,
                                    ".rds")
  
  saveRDS(perf_series_df,
          file.path("data",
                    perf_series_df_filename))
  
} else {
  perf_series_df <- readRDS(file.path("data",
                                      "perf_series_df.rds"))
  
}

if(TRUE) {
  source("R/perf_series_cleaning.R")
  
  perf_series_df <- clean_perf_series_df(perf_series_df,
                                         mid_range_min = 0.01,
                                         mid_range_width = 0.5)
  
  perf_series_df_clean_filename <- paste0("perf_series_df_clean",
                                    filename_suffix,
                                    ".rds")
  
  saveRDS(perf_series_df,
          file.path("data",
                    perf_series_df_clean_filename))
}

if(FALSE) { # Use only first 10 codes
  perf_series_df <- perf_series_df %>%
    filter(Code %in% (dplyr::distinct(perf_series_df, Code) %>%
                        head(10) %>%
                        pull(Code)))
}

##Monthly C algorithm###############################################################################

#initialise blank results table
limits_table_output_Monthly_C_algorithm <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_volume_from_perf_series_df(data = perf_series_df,
                                           Code_arg = c,
                                           weeklyOrMonthly_arg = "Monthly",
                                           measure_arg = "All",
                                           onlyProvsReporting_arg = TRUE,
                                           periodMin = 24,
                                           noRecals = FALSE)
  
  limits_table_output_Monthly_C_algorithm <- bind_rows(limits_table_output_Monthly_C_algorithm, df_out)
  
}

saveRDS(limits_table_output_Monthly_C_algorithm,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Monthly_C_algorithm",
                         filename_suffix,".rds")))

##Weekly C algorithm###############################################################################

#initialise blank results table
limits_table_output_Weekly_C_algorithm <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_volume_from_perf_series_df(data = perf_series_df,
                                           Code_arg = c,
                                           weeklyOrMonthly_arg = "Weekly",
                                           measure_arg = "All",
                                           onlyProvsReporting_arg = TRUE,
                                           periodMin = 21,
                                           noRecals = FALSE)
  
  limits_table_output_Weekly_C_algorithm <- bind_rows(limits_table_output_Weekly_C_algorithm, df_out)
  
}

saveRDS(limits_table_output_Weekly_C_algorithm,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Weekly_C_algorithm",
                         filename_suffix,".rds")))

##Monthly P algorithm###############################################################################

#initialise blank results table
limits_table_output_Monthly_P_algorithm <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){

  print(c)

  df_out <- get_peformance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Monthly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 24,
                                               noRecals = FALSE)

  limits_table_output_Monthly_P_algorithm <- bind_rows(limits_table_output_Monthly_P_algorithm, df_out)

  # tryCatch({
  #   df_out <- get_peformance_from_perf_series_df(data = perf_series_df,
  #                                                Code_arg = c,
  #                                                weeklyOrMonthly_arg = "Monthly",
  #                                                measure_arg = "All",
  #                                                onlyProvsReporting_arg = TRUE,
  #                                                periodMin = 24,
  #                                                noRecals = FALSE)
  #
  #   limits_table_output_Monthly_test_P_20230306 <- bind_rows(limits_table_output_Monthly_test_P_20230306, df_out)
  #   err <- FALSE
  # }, warning = function(w) {
  #   err <- FALSE
  # }, error = function(e) {
  #   err <- TRUE
  # })


}
saveRDS(limits_table_output_Monthly_P_algorithm,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Monthly_P_algorithm",
                         filename_suffix,".rds")))

##Weekly P algorithm###############################################################################

#initialise blank results table
limits_table_output_Weekly_P_algorithm <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_peformance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Weekly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 21,
                                               noRecals = FALSE)
  
  limits_table_output_Weekly_P_algorithm <- bind_rows(limits_table_output_Weekly_P_algorithm, df_out)
  
}
saveRDS(limits_table_output_Weekly_P_algorithm,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Weekly_P_algorithm",
                         filename_suffix,".rds")))



################################################################################
################################################################################
##Monthly C naive 1###############################################################################

#initialise blank results table
limits_table_output_Monthly_C_naive1 <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_volume_from_perf_series_df_naive1(data = perf_series_df,
                                                  Code_arg = c,
                                                  weeklyOrMonthly_arg = "Monthly",
                                                  measure_arg = "All",
                                                  onlyProvsReporting_arg = TRUE,
                                                  periodMin = 24,
                                                  noRecals = FALSE)
  
  limits_table_output_Monthly_C_naive1 <- bind_rows(limits_table_output_Monthly_C_naive1, df_out)
  
}

saveRDS(limits_table_output_Monthly_C_naive1,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Monthly_C_naive1",
                         filename_suffix,".rds")))

##Weekly C naive1###############################################################################

#initialise blank results table
limits_table_output_Weekly_C_naive1 <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_volume_from_perf_series_df_naive1(data = perf_series_df,
                                                  Code_arg = c,
                                                  weeklyOrMonthly_arg = "Weekly",
                                                  measure_arg = "All",
                                                  onlyProvsReporting_arg = TRUE,
                                                  periodMin = 21,
                                                  noRecals = FALSE)
  
  limits_table_output_Weekly_C_naive1 <- bind_rows(limits_table_output_Weekly_C_naive1, df_out)
  
}

saveRDS(limits_table_output_Weekly_C_naive1,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Weekly_C_naive1",
                         filename_suffix,".rds")))

##Monthly P naive1###############################################################################

#initialise blank results table
limits_table_output_Monthly_P_naive1 <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_peformance_from_perf_series_df_naive1(data = perf_series_df,
                                                      Code_arg = c,
                                                      weeklyOrMonthly_arg = "Monthly",
                                                      measure_arg = "All",
                                                      onlyProvsReporting_arg = TRUE,
                                                      periodMin = 24,
                                                      noRecals = FALSE)
  
  limits_table_output_Monthly_P_naive1 <- bind_rows(limits_table_output_Monthly_P_naive1, df_out)
  
}
saveRDS(limits_table_output_Monthly_P_naive1,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Monthly_P_naive1",
                         filename_suffix,".rds")))

##Weekly P naive1###############################################################################

#initialise blank results table
limits_table_output_Weekly_P_naive1 <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_peformance_from_perf_series_df_naive1(data = perf_series_df,
                                                      Code_arg = c,
                                                      weeklyOrMonthly_arg = "Weekly",
                                                      measure_arg = "All",
                                                      onlyProvsReporting_arg = TRUE,
                                                      periodMin = 21,
                                                      noRecals = FALSE)
  
  limits_table_output_Weekly_P_naive1 <- bind_rows(limits_table_output_Weekly_P_naive1, df_out)
  
}
saveRDS(limits_table_output_Weekly_P_naive1,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Weekly_P_naive1",
                         filename_suffix,".rds")))



################################################################################
################################################################################
##Monthly C naive2###############################################################################

#initialise blank results table
limits_table_output_Monthly_C_naive2 <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_volume_from_perf_series_df(data = perf_series_df,
                                           Code_arg = c,
                                           weeklyOrMonthly_arg = "Monthly",
                                           measure_arg = "All",
                                           onlyProvsReporting_arg = TRUE,
                                           periodMin = 24,
                                           noRecals = TRUE)
  
  limits_table_output_Monthly_C_naive2 <- bind_rows(limits_table_output_Monthly_C_naive2, df_out)
  
}

saveRDS(limits_table_output_Monthly_C_naive2,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Monthly_C_naive2",
                         filename_suffix,".rds")))

##Weekly C naive2###############################################################################

#initialise blank results table
limits_table_output_Weekly_C_naive2 <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_volume_from_perf_series_df(data = perf_series_df,
                                           Code_arg = c,
                                           weeklyOrMonthly_arg = "Weekly",
                                           measure_arg = "All",
                                           onlyProvsReporting_arg = TRUE,
                                           periodMin = 21,
                                           noRecals = TRUE)
  
  limits_table_output_Weekly_C_naive2 <- bind_rows(limits_table_output_Weekly_C_naive2, df_out)
  
}

saveRDS(limits_table_output_Weekly_C_naive2,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Weekly_C_naive2",
                         filename_suffix,".rds")))

##Monthly P naive2###############################################################################

#initialise blank results table
limits_table_output_Monthly_P_naive2 <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_peformance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Monthly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 24,
                                               noRecals = TRUE)
  
  limits_table_output_Monthly_P_naive2 <- bind_rows(limits_table_output_Monthly_P_naive2, df_out)
  
  
}
saveRDS(limits_table_output_Monthly_P_naive2,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Monthly_P_naive2",
                         filename_suffix,".rds")))

##Weekly P naive2###############################################################################

#initialise blank results table
limits_table_output_Weekly_P_naive2 <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_peformance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Weekly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 21,
                                               noRecals = TRUE)
  
  limits_table_output_Weekly_P_naive2 <- bind_rows(limits_table_output_Weekly_P_naive2, df_out)
  
}
saveRDS(limits_table_output_Weekly_P_naive2,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Weekly_P_naive2",
                         filename_suffix,".rds")))

################################################################################
##Monthly C naive3###############################################################################

#initialise blank results table
limits_table_output_Monthly_C_naive3 <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_volume_from_perf_series_df(data = perf_series_df,
                                           Code_arg = c,
                                           weeklyOrMonthly_arg = "Monthly",
                                           measure_arg = "All",
                                           onlyProvsReporting_arg = TRUE,
                                           periodMin = 24,
                                           noRecals = FALSE,
                                           recalc_every_shift = TRUE)
  
  limits_table_output_Monthly_C_naive3 <- bind_rows(limits_table_output_Monthly_C_naive3, df_out)
  
}

saveRDS(limits_table_output_Monthly_C_naive3,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Monthly_C_naive3",
                         filename_suffix,".rds")))

##Weekly C naive3###############################################################################

#initialise blank results table
limits_table_output_Weekly_C_naive3 <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_volume_from_perf_series_df(data = perf_series_df,
                                           Code_arg = c,
                                           weeklyOrMonthly_arg = "Weekly",
                                           measure_arg = "All",
                                           onlyProvsReporting_arg = TRUE,
                                           periodMin = 21,
                                           noRecals = FALSE, 
                                           recalc_every_shift = TRUE)
  
  limits_table_output_Weekly_C_naive3 <- bind_rows(limits_table_output_Weekly_C_naive3, df_out)
  
}

saveRDS(limits_table_output_Weekly_C_naive3,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Weekly_C_naive3",
                         filename_suffix,".rds")))

##Monthly P naive3###############################################################################

#initialise blank results table
limits_table_output_Monthly_P_naive3 <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_peformance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Monthly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 24,
                                               noRecals = FALSE,
                                               recalc_every_shift = TRUE)
  
  limits_table_output_Monthly_P_naive3 <- bind_rows(limits_table_output_Monthly_P_naive3, df_out)
  
  
}
saveRDS(limits_table_output_Monthly_P_naive3,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Monthly_P_naive3",
                         filename_suffix,".rds")))

##Weekly P naive3###############################################################################

#initialise blank results table
limits_table_output_Weekly_P_naive3 <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_peformance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Weekly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 21,
                                               noRecals = FALSE,
                                               recalc_every_shift = TRUE)
  
  limits_table_output_Weekly_P_naive3 <- bind_rows(limits_table_output_Weekly_P_naive3, df_out)
  
}
saveRDS(limits_table_output_Weekly_P_naive3,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Weekly_P_naive3",
                         filename_suffix,".rds")))



################################################################################
##Monthly C naive3b###############################################################################

#initialise blank results table
limits_table_output_Monthly_C_naive3b <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_volume_from_perf_series_df(data = perf_series_df,
                                           Code_arg = c,
                                           weeklyOrMonthly_arg = "Monthly",
                                           measure_arg = "All",
                                           onlyProvsReporting_arg = TRUE,
                                           periodMin = 8,
                                           noRecals = FALSE,
                                           recalc_every_shift = TRUE,
                                           noPeriodMin = FALSE)
  
  limits_table_output_Monthly_C_naive3b <- bind_rows(limits_table_output_Monthly_C_naive3b, df_out)
  
}

saveRDS(limits_table_output_Monthly_C_naive3b,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Monthly_C_naive3b",
                         filename_suffix,".rds")))

##Weekly C naive3b###############################################################################

#initialise blank results table
limits_table_output_Weekly_C_naive3b <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_volume_from_perf_series_df(data = perf_series_df,
                                           Code_arg = c,
                                           weeklyOrMonthly_arg = "Weekly",
                                           measure_arg = "All",
                                           onlyProvsReporting_arg = TRUE,
                                           periodMin = 8,
                                           noRecals = FALSE, 
                                           recalc_every_shift = TRUE,
                                           noPeriodMin = FALSE)
  
  limits_table_output_Weekly_C_naive3b <- bind_rows(limits_table_output_Weekly_C_naive3b, df_out)
  
}

saveRDS(limits_table_output_Weekly_C_naive3b,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Weekly_C_naive3b",
                         filename_suffix,".rds")))

##Monthly P naive3b###############################################################################

#initialise blank results table
limits_table_output_Monthly_P_naive3b <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_peformance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Monthly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 8,
                                               noRecals = FALSE,
                                               recalc_every_shift = TRUE,
                                               noPeriodMin = FALSE)
  
  limits_table_output_Monthly_P_naive3b <- bind_rows(limits_table_output_Monthly_P_naive3b, df_out)
  
  
}
saveRDS(limits_table_output_Monthly_P_naive3b,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Monthly_P_naive3b",
                         filename_suffix,".rds")))

##Weekly P naive3b###############################################################################

#initialise blank results table
limits_table_output_Weekly_P_naive3b <- data.frame()
codes <- dplyr::distinct(perf_series_df, Code)
#codes_short <- head(codes,14)

for(c in codes$Code){
  
  print(c)
  
  df_out <- get_peformance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Weekly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 8,
                                               noRecals = FALSE,
                                               recalc_every_shift = TRUE,
                                               noPeriodMin = FALSE)
  
  limits_table_output_Weekly_P_naive3b <- bind_rows(limits_table_output_Weekly_P_naive3b, df_out)
  
}
saveRDS(limits_table_output_Weekly_P_naive3b,
        file.path("data",
                  "outputs",
                  paste0("limits_table_output_Weekly_P_naive3b",
                         filename_suffix,".rds")))




################################################################################

################################################################################
################################################################################
limits_table_output_C_algorithm <- bind_rows(limits_table_output_Monthly_C_algorithm,
                                             limits_table_output_Weekly_C_algorithm)

limits_table_output_C_naive1 <- bind_rows(limits_table_output_Monthly_C_naive1,
                                          limits_table_output_Weekly_C_naive1)

limits_table_output_C_naive2 <- bind_rows(limits_table_output_Monthly_C_naive2,
                                          limits_table_output_Weekly_C_naive2)

limits_table_output_C_naive3 <- bind_rows(limits_table_output_Monthly_C_naive3,
                                          limits_table_output_Weekly_C_naive3)

limits_table_output_C_naive3b <- bind_rows(limits_table_output_Monthly_C_naive3b,
                                          limits_table_output_Weekly_C_naive3b)

limits_table_output_P_algorithm <- bind_rows(limits_table_output_Monthly_P_algorithm,
                                             limits_table_output_Weekly_P_algorithm)

limits_table_output_P_naive1 <- bind_rows(limits_table_output_Monthly_P_naive1,
                                          limits_table_output_Weekly_P_naive1)

limits_table_output_P_naive2 <- bind_rows(limits_table_output_Monthly_P_naive2,
                                          limits_table_output_Weekly_P_naive2)

limits_table_output_P_naive3 <- bind_rows(limits_table_output_Monthly_P_naive3,
                                          limits_table_output_Weekly_P_naive3)

limits_table_output_P_naive3b <- bind_rows(limits_table_output_Monthly_P_naive3b,
                                          limits_table_output_Weekly_P_naive3b)

################################################################################
results_summary_list_C_algoritm <- get_results_summary(limits_table_output_C_algorithm)
results_summary_list_C_naive1 <- get_results_summary(limits_table_output_C_naive1)
results_summary_list_C_naive2 <- get_results_summary(limits_table_output_C_naive2)
results_summary_list_C_naive3 <- get_results_summary(limits_table_output_C_naive3)
results_summary_list_C_naive3b <- get_results_summary(limits_table_output_C_naive3b)

results_summary_list_P_algoritm <- get_results_summary(limits_table_output_P_algorithm)
results_summary_list_P_naive1 <- get_results_summary(limits_table_output_P_naive1)
results_summary_list_P_naive2 <- get_results_summary(limits_table_output_P_naive2)
results_summary_list_P_naive3 <- get_results_summary(limits_table_output_P_naive3)
results_summary_list_P_naive3b <- get_results_summary(limits_table_output_P_naive3b)

################################################################################
results_summary_list_C_algoritm_w <- get_results_summary(limits_table_output_Weekly_C_algorithm)
results_summary_list_C_naive1_w <- get_results_summary(limits_table_output_Weekly_C_naive1)
results_summary_list_C_naive2_w <- get_results_summary(limits_table_output_Weekly_C_naive2)
results_summary_list_C_naive3_w <- get_results_summary(limits_table_output_Weekly_C_naive3)
results_summary_list_C_naive3b_w <- get_results_summary(limits_table_output_Weekly_C_naive3b)

results_summary_list_P_algoritm_w <- get_results_summary(limits_table_output_Weekly_P_algorithm)
results_summary_list_P_naive1_w <- get_results_summary(limits_table_output_Weekly_P_naive1)
results_summary_list_P_naive2_w <- get_results_summary(limits_table_output_Weekly_P_naive2)
results_summary_list_P_naive3_w <- get_results_summary(limits_table_output_Weekly_P_naive3)
results_summary_list_P_naive3b_w <- get_results_summary(limits_table_output_Weekly_P_naive3b)

results_summary_list_C_algoritm_m <- get_results_summary(limits_table_output_Monthly_C_algorithm)
results_summary_list_C_naive1_m <- get_results_summary(limits_table_output_Monthly_C_naive1)
results_summary_list_C_naive2_m <- get_results_summary(limits_table_output_Monthly_C_naive2)
results_summary_list_C_naive3_m <- get_results_summary(limits_table_output_Monthly_C_naive3)
results_summary_list_C_naive3b_m <- get_results_summary(limits_table_output_Monthly_C_naive3b)

results_summary_list_P_algoritm_m <- get_results_summary(limits_table_output_Monthly_P_algorithm)
results_summary_list_P_naive1_m <- get_results_summary(limits_table_output_Monthly_P_naive1)
results_summary_list_P_naive2_m <- get_results_summary(limits_table_output_Monthly_P_naive2)
results_summary_list_P_naive3_m <- get_results_summary(limits_table_output_Monthly_P_naive3)
results_summary_list_P_naive3b_m <- get_results_summary(limits_table_output_Monthly_P_naive3b)
################################################################################

full_results_C_algorithm <- results_summary_list_C_algoritm[[1]] %>%
  mutate(results_set = "C algorithm")
full_results_C_naive1 <- results_summary_list_C_naive1[[1]] %>%
  mutate(results_set = "C naive1")
full_results_C_naive2 <- results_summary_list_C_naive2[[1]] %>%
  mutate(results_set = "C naive2")
full_results_C_naive3 <- results_summary_list_C_naive3[[1]] %>%
  mutate(results_set = "C naive3")
full_results_C_naive3b <- results_summary_list_C_naive3b[[1]] %>%
  mutate(results_set = "C naive3b")

full_results_P_algorithm <- results_summary_list_P_algoritm[[1]] %>%
  mutate(results_set = "P algorithm")
full_results_P_naive1 <- results_summary_list_P_naive1[[1]] %>%
  mutate(results_set = "P naive1")
full_results_P_naive2 <- results_summary_list_P_naive2[[1]] %>%
  mutate(results_set = "P naive2")
full_results_P_naive3 <- results_summary_list_P_naive3[[1]] %>%
  mutate(results_set = "P naive3")
full_results_P_naive3b <- results_summary_list_P_naive3b[[1]] %>%
  mutate(results_set = "P naive3b")

summary_results_C_algorithm <- results_summary_list_C_algoritm[[2]] %>%
  mutate(results_set = "C algorithm")
summary_results_C_naive1 <- results_summary_list_C_naive1[[2]] %>%
  mutate(results_set = "C naive1")
summary_results_C_naive2 <- results_summary_list_C_naive2[[2]] %>%
  mutate(results_set = "C naive2")
summary_results_C_naive3 <- results_summary_list_C_naive3[[2]] %>%
  mutate(results_set = "C naive3")
summary_results_C_naive3b <- results_summary_list_C_naive3b[[2]] %>%
  mutate(results_set = "C naive3b")

summary_results_P_algorithm <- results_summary_list_P_algoritm[[2]] %>%
  mutate(results_set = "P algorithm")
summary_results_P_naive1 <- results_summary_list_P_naive1[[2]] %>%
  mutate(results_set = "P naive1")
summary_results_P_naive2 <- results_summary_list_P_naive2[[2]] %>%
  mutate(results_set = "P naive2")
summary_results_P_naive3 <- results_summary_list_P_naive3[[2]] %>%
  mutate(results_set = "P naive3")
summary_results_P_naive3b <- results_summary_list_P_naive3b[[2]] %>%
  mutate(results_set = "P naive3b")

################################################################################
summary_results_C_algorithm_w <- results_summary_list_C_algoritm_w[[2]] %>%
  mutate(results_set = "C algorithm weekly")
summary_results_C_naive1_w <- results_summary_list_C_naive1_w[[2]] %>%
  mutate(results_set = "C naive1 weekly")
summary_results_C_naive2_w <- results_summary_list_C_naive2_w[[2]] %>%
  mutate(results_set = "C naive2 weekly")
summary_results_C_naive3_w <- results_summary_list_C_naive3_w[[2]] %>%
  mutate(results_set = "C naive3 weekly")
summary_results_C_naive3b_w <- results_summary_list_C_naive3b_w[[2]] %>%
  mutate(results_set = "C naive3b weekly")

summary_results_P_algorithm_w <- results_summary_list_P_algoritm_w[[2]] %>%
  mutate(results_set = "P algorithm weekly")
summary_results_P_naive1_w <- results_summary_list_P_naive1_w[[2]] %>%
  mutate(results_set = "P naive1 weekly")
summary_results_P_naive2_w <- results_summary_list_P_naive2_w[[2]] %>%
  mutate(results_set = "P naive2 weekly")
summary_results_P_naive3_w <- results_summary_list_P_naive3_w[[2]] %>%
  mutate(results_set = "P naive3 weekly")
summary_results_P_naive3b_w <- results_summary_list_P_naive3b_w[[2]] %>%
  mutate(results_set = "P naive3b weekly")

summary_results_C_algorithm_m <- results_summary_list_C_algoritm_m[[2]] %>%
  mutate(results_set = "C algorithm monthly")
summary_results_C_naive1_m <- results_summary_list_C_naive1_m[[2]] %>%
  mutate(results_set = "C naive1 monthly")
summary_results_C_naive2_m <- results_summary_list_C_naive2_m[[2]] %>%
  mutate(results_set = "C naive2 monthly")
summary_results_C_naive3_m <- results_summary_list_C_naive3_m[[2]] %>%
  mutate(results_set = "C naive3 monthly")
summary_results_C_naive3b_m <- results_summary_list_C_naive3b_m[[2]] %>%
  mutate(results_set = "C naive3b monthly")

summary_results_P_algorithm_m <- results_summary_list_P_algoritm_m[[2]] %>%
  mutate(results_set = "P algorithm monthly")
summary_results_P_naive1_m <- results_summary_list_P_naive1_m[[2]] %>%
  mutate(results_set = "P naive1 monthly")
summary_results_P_naive2_m <- results_summary_list_P_naive2_m[[2]] %>%
  mutate(results_set = "P naive2 monthly")
summary_results_P_naive3_m <- results_summary_list_P_naive3_m[[2]] %>%
  mutate(results_set = "P naive3 monthly")
summary_results_P_naive3b_m <- results_summary_list_P_naive3b_m[[2]] %>%
  mutate(results_set = "P naive3b monthly")
################################################################################

summary_results_all <- bind_rows(summary_results_C_algorithm,
                                 summary_results_C_naive1,
                                 summary_results_C_naive2,
                                 summary_results_C_naive3,
                                 summary_results_C_naive3b,
                                 summary_results_P_algorithm,
                                 summary_results_P_naive1,
                                 summary_results_P_naive2,
                                 summary_results_P_naive3,
                                 summary_results_P_naive3b) %>%
  select(results_set, num_charts, everything())

summary_results_all_wm <- bind_rows(summary_results_C_algorithm_w,
                                    summary_results_C_naive1_w,
                                    summary_results_C_naive2_w,
                                    summary_results_C_naive3_w,
                                    summary_results_C_naive3b_w,
                                    summary_results_P_algorithm_w,
                                    summary_results_P_naive1_w,
                                    summary_results_P_naive2_w,
                                    summary_results_P_naive3_w,
                                    summary_results_P_naive3b_w,
                                    summary_results_C_algorithm_m,
                                    summary_results_C_naive1_m,
                                    summary_results_C_naive2_m,
                                    summary_results_C_naive3_m,
                                    summary_results_C_naive3b_m,
                                    summary_results_P_algorithm_m,
                                    summary_results_P_naive1_m,
                                    summary_results_P_naive2_m,
                                    summary_results_P_naive3_m,
                                    summary_results_P_naive3b_m) %>%
  select(results_set, num_charts, everything())

full_results_all <- bind_rows(full_results_C_algorithm,
                              full_results_C_naive1,
                              full_results_C_naive2,
                              full_results_C_naive3,
                              full_results_C_naive3b,
                              full_results_P_algorithm,
                              full_results_P_naive1,
                              full_results_P_naive2,
                              full_results_P_naive3,
                              full_results_P_naive3b) %>%
  select(results_set, everything())



saveRDS(full_results_all,
        file.path("data",
                  "outputs",
                  "full_results",
                  paste0("full_results_all",
                         filename_suffix,".rds")))

writexl::write_xlsx(full_results_all,
                    file.path("data",
                              "outputs",
                              "full_results",
                              paste0("full_results_all",
                                     filename_suffix,".xlsx")))

saveRDS(summary_results_all,
        file.path("data",
                  "outputs",
                  "full_results",
                  paste0("summary_results_all",
                         filename_suffix,".rds")))

writexl::write_xlsx(summary_results_all,
                    file.path("data",
                              "outputs",
                              "full_results",
                              paste0("summary_results_all",
                                     filename_suffix,".xlsx")))

saveRDS(summary_results_all_wm,
        file.path("data",
                  "outputs",
                  "full_results",
                  paste0("summary_results_all_wm",
                         filename_suffix,".rds")))

writexl::write_xlsx(summary_results_all_wm,
                    file.path("data",
                              "outputs",
                              "full_results",
                              paste0("summary_results_all_wm",
                                     filename_suffix,".xlsx")))
