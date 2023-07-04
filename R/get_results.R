library(tidyverse)
library(dplyr)
library(fpCompare)
library(ggplot2)
library(lubridate)
library(magrittr)
library(qicharts2)
library(rlang)
library(scales)

devtools::install_github("HorridTom/autospc", ref = "naive3-argument-103")
library(autospc)

source("R/get_perf_series_df.R")
source("R/results_functions.R")

AE_Data <- readRDS("data/AE_Data.RDS")
AE_Data_Scot <- readRDS("data/AE_Data_Scot.RDS")

#ONLY COMMENTED OUT BECAUSE IT'S SLOW!
# perf_series_df <- make_perf_series_data_frame(data = AE_Data,
#                                               data_scot = AE_Data_Scot,
#                                               measureArg = "All",
#                                               onlyProvsReportingArg = TRUE)

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

saveRDS(limits_table_output_Monthly_C_algorithm, "R/Results/outputs_20230618/limits_table_output_Monthly_C_algorithm.rds")

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

saveRDS(limits_table_output_Weekly_C_algorithm, "R/Results/outputs_20230618/limits_table_output_Weekly_C_algorithm.rds")

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
saveRDS(limits_table_output_Monthly_P_algorithm, "R/Results/outputs_20230618/limits_table_output_Monthly_P_algorithm.rds")

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
saveRDS(limits_table_output_Weekly_P_algorithm, "R/Results/outputs_20230618/limits_table_output_Weekly_P_algorithm.rds")



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

saveRDS(limits_table_output_Monthly_C_naive1, "R/Results/outputs_20230618/limits_table_output_Monthly_C_naive1.rds")

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

saveRDS(limits_table_output_Weekly_C_naive1, "R/Results/outputs_20230618/limits_table_output_Weekly_C_naive1.rds")

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
saveRDS(limits_table_output_Monthly_P_naive1, "R/Results/outputs_20230618/limits_table_output_Monthly_P_naive1.rds")

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
saveRDS(limits_table_output_Weekly_P_naive1, "R/Results/outputs_20230618/limits_table_output_Weekly_P_naive1.rds")



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

saveRDS(limits_table_output_Monthly_C_naive2, "R/Results/outputs_20230618/limits_table_output_Monthly_C_naive2.rds")

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

saveRDS(limits_table_output_Weekly_C_naive2, "R/Results/outputs_20230618/limits_table_output_Weekly_C_naive2.rds")

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
saveRDS(limits_table_output_Monthly_P_naive2, "R/Results/outputs_20230618/limits_table_output_Monthly_P_naive2.rds")

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
saveRDS(limits_table_output_Weekly_P_naive2, "R/Results/outputs_20230618/limits_table_output_Weekly_P_naive2.rds")

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
                                           development_recalc_at_every_break = TRUE)
  
  limits_table_output_Monthly_C_naive3 <- bind_rows(limits_table_output_Monthly_C_naive3, df_out)
  
}

saveRDS(limits_table_output_Monthly_C_naive3, "R/Results/outputs_20230618/limits_table_output_Monthly_C_naive3.rds")

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
                                           development_recalc_at_every_break = TRUE)
  
  limits_table_output_Weekly_C_naive3 <- bind_rows(limits_table_output_Weekly_C_naive3, df_out)
  
}

saveRDS(limits_table_output_Weekly_C_naive3, "R/Results/outputs_20230618/limits_table_output_Weekly_C_naive3.rds")

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
                                               development_recalc_at_every_break = TRUE)
  
  limits_table_output_Monthly_P_naive3 <- bind_rows(limits_table_output_Monthly_P_naive3, df_out)
  
  
}
saveRDS(limits_table_output_Monthly_P_naive3, "R/Results/outputs_20230618/limits_table_output_Monthly_P_naive3.rds")

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
                                               development_recalc_at_every_break = TRUE)
  
  limits_table_output_Weekly_P_naive3 <- bind_rows(limits_table_output_Weekly_P_naive3, df_out)
  
}
saveRDS(limits_table_output_Weekly_P_naive3, "R/Results/outputs_20230618/limits_table_output_Weekly_P_naive3.rds")




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

limits_table_output_P_algorithm <- bind_rows(limits_table_output_Monthly_P_algorithm,
                                             limits_table_output_Weekly_P_algorithm)

limits_table_output_P_naive1 <- bind_rows(limits_table_output_Monthly_P_naive1,
                                          limits_table_output_Weekly_P_naive1)

limits_table_output_P_naive2 <- bind_rows(limits_table_output_Monthly_P_naive2,
                                          limits_table_output_Weekly_P_naive2)

limits_table_output_P_naive3 <- bind_rows(limits_table_output_Monthly_P_naive3,
                                          limits_table_output_Weekly_P_naive3)

################################################################################
results_summary_list_C_algoritm <- get_results_summary(limits_table_output_C_algorithm)
results_summary_list_C_naive1 <- get_results_summary(limits_table_output_C_naive1)
results_summary_list_C_naive2 <- get_results_summary(limits_table_output_C_naive2)
results_summary_list_C_naive3 <- get_results_summary(limits_table_output_C_naive3)

results_summary_list_P_algoritm <- get_results_summary(limits_table_output_P_algorithm)
results_summary_list_P_naive1 <- get_results_summary(limits_table_output_P_naive1)
results_summary_list_P_naive2 <- get_results_summary(limits_table_output_P_naive2)
results_summary_list_P_naive3 <- get_results_summary(limits_table_output_P_naive3)

full_results_C_algorithm <- results_summary_list_C_algoritm[[1]] %>%
  mutate(results_set = "C algorithm")
full_results_C_naive1 <- results_summary_list_C_naive1[[1]] %>%
  mutate(results_set = "C naive1")
full_results_C_naive2 <- results_summary_list_C_naive2[[1]] %>%
  mutate(results_set = "C naive2")
full_results_C_naive3 <- results_summary_list_C_naive3[[1]] %>%
  mutate(results_set = "C naive3")

full_results_P_algorithm <- results_summary_list_P_algoritm[[1]] %>%
  mutate(results_set = "P algorithm")
full_results_P_naive1 <- results_summary_list_P_naive1[[1]] %>%
  mutate(results_set = "P naive1")
full_results_P_naive2 <- results_summary_list_P_naive2[[1]] %>%
  mutate(results_set = "P naive2")
full_results_P_naive3 <- results_summary_list_P_naive3[[1]] %>%
  mutate(results_set = "P naive3")

summary_results_C_algorithm <- results_summary_list_C_algoritm[[2]] %>%
  mutate(results_set = "C algorithm")
summary_results_C_naive1 <- results_summary_list_C_naive1[[2]] %>%
  mutate(results_set = "C naive1")
summary_results_C_naive2 <- results_summary_list_C_naive2[[2]] %>%
  mutate(results_set = "C naive2")
summary_results_C_naive3 <- results_summary_list_C_naive3[[2]] %>%
  mutate(results_set = "C naive3")

summary_results_P_algorithm <- results_summary_list_P_algoritm[[2]] %>%
  mutate(results_set = "P algorithm")
summary_results_P_naive1 <- results_summary_list_P_naive1[[2]] %>%
  mutate(results_set = "P naive1")
summary_results_P_naive2 <- results_summary_list_P_naive2[[2]] %>%
  mutate(results_set = "P naive2")
summary_results_P_naive3 <- results_summary_list_P_naive3[[2]] %>%
  mutate(results_set = "P naive3")

summary_results_all <- bind_rows(summary_results_C_algorithm,
                                 summary_results_C_naive1,
                                 summary_results_C_naive2,
                                 summary_results_C_naive3,
                                 summary_results_P_algorithm,
                                 summary_results_P_naive1,
                                 summary_results_P_naive2,
                                 summary_results_P_naive3) %>%
  select(results_set, num_charts, everything())

full_results_all <- bind_rows(full_results_C_algorithm,
                              full_results_C_naive1,
                              full_results_C_naive2,
                              full_results_C_naive3,
                              full_results_P_algorithm,
                              full_results_P_naive1,
                              full_results_P_naive2,
                              full_results_P_naive3) %>%
  select(results_set, everything())

 
writexl::write_xlsx(full_results_all, "R/Results/outputs_20230618/full_results/full_results_all3.xlsx")
writexl::write_xlsx(summary_results_all, "R/Results/outputs_20230618/full_results/summary_results_all.xlsx")
