################################################################################
plot_volume_from_perf_series_df <- function(data = perf_series_df,
                                            Code_arg = "RYJ",
                                            #level_arg = "Provider",
                                            weeklyOrMonthly_arg = "Monthly",
                                            measure_arg = "All",
                                            onlyProvsReporting_arg = TRUE,
                                            
                                            periodMin = 21,
                                            noRecals = FALSE,
                                            development_recalc_at_every_break = FALSE){
  
  data <- data %>%
    dplyr::filter(Code == Code_arg,
                  #level == level_arg,
                  weeklyOrMonthly == weeklyOrMonthly_arg,
                  measure == measure_arg,
                  onlyProvsReporting == onlyProvsReporting_arg)
  
  #dataLength <- nrow(data)

  plot_auto_SPC(data,
                x = Month_Start,
                y = daily_ave_att,
                chartType = "C'",
                periodMin = periodMin,#dataLength,
                plotChart = TRUE,
                noRecals = noRecals,
                title = paste(Code_arg, weeklyOrMonthly_arg),
                development_recalc_at_every_break = development_recalc_at_every_break)
}

################################################################################
get_volume_from_perf_series_df <- function(data = perf_series_df,
                                            Code_arg = "RYJ",
                                            weeklyOrMonthly_arg = "Monthly",
                                            measure_arg = "All",
                                            onlyProvsReporting_arg = TRUE,
                                            
                                            periodMin = 21,
                                            noRecals = FALSE,
                                           development_recalc_at_every_break = FALSE){
  
  data <- data %>%
    dplyr::filter(Code == Code_arg,
                  weeklyOrMonthly == weeklyOrMonthly_arg,
                  measure == measure_arg,
                  onlyProvsReporting == onlyProvsReporting_arg)
  
  #dataLength <- nrow(data)
  
  if(nrow(data) >= periodMin * 2){
    data_out <- plot_auto_SPC(data,
                              x = Month_Start,
                              y = daily_ave_att,
                              chartType = "C'",
                              periodMin = periodMin,#dataLength,
                              plotChart = FALSE,
                              noRecals = noRecals,
                              title = paste(Code_arg, weeklyOrMonthly_arg, measure_arg, onlyProvsReporting_arg),
                              development_recalc_at_every_break = development_recalc_at_every_break)
    
    data_out <- data_out %>%
      mutate(Code = Code_arg,
             weeklyOrMonthly = weeklyOrMonthly_arg,
             measure = measure_arg,
             onlyProvsReporting = onlyProvsReporting_arg
      )
  }else{
    
    print("not enough points")
    data_out <- data.frame()
  }
  
}

################################################################################
plot_volume_from_perf_series_df_naive1 <- function(data = perf_series_df,
                                            Code_arg = "RYJ",
                                            #level_arg = "Provider",
                                            weeklyOrMonthly_arg = "Monthly",
                                            measure_arg = "All",
                                            onlyProvsReporting_arg = TRUE,
                                            
                                            periodMin = 21,
                                            noRecals = FALSE,
                                            development_recalc_at_every_break = FALSE){
  
  data <- data %>%
    dplyr::filter(Code == Code_arg,
                  #level == level_arg,
                  weeklyOrMonthly == weeklyOrMonthly_arg,
                  measure == measure_arg,
                  onlyProvsReporting == onlyProvsReporting_arg)
  
  dataLength <- nrow(data)
  
  plot_auto_SPC(data,
                x = Month_Start,
                y = daily_ave_att,
                chartType = "C'",
                periodMin = dataLength,
                plotChart = TRUE,
                noRecals = noRecals,
                title = paste(Code_arg, weeklyOrMonthly_arg),
                development_recalc_at_every_break = development_recalc_at_every_break)
}

################################################################################
get_volume_from_perf_series_df_naive1 <- function(data = perf_series_df,
                                                  Code_arg = "RYJ",
                                                  weeklyOrMonthly_arg = "Monthly",
                                                  measure_arg = "All",
                                                  onlyProvsReporting_arg = TRUE,
                                                  
                                                  periodMin = 21,
                                                  noRecals = FALSE,
                                                  development_recalc_at_every_break = FALSE){
  
  data <- data %>%
    dplyr::filter(Code == Code_arg,
                  weeklyOrMonthly == weeklyOrMonthly_arg,
                  measure == measure_arg,
                  onlyProvsReporting == onlyProvsReporting_arg)
  
  dataLength <- nrow(data)
  
  if(nrow(data) >= periodMin * 2){
    data_out <- plot_auto_SPC(data,
                              x = Month_Start,
                              y = daily_ave_att,
                              chartType = "C'",
                              periodMin = dataLength,
                              plotChart = FALSE,
                              noRecals = TRUE,
                              title = paste(Code_arg, weeklyOrMonthly_arg, measure_arg, onlyProvsReporting_arg),
                              development_recalc_at_every_break = development_recalc_at_every_break)
    
    data_out <- data_out %>%
      mutate(Code = Code_arg,
             weeklyOrMonthly = weeklyOrMonthly_arg,
             measure = measure_arg,
             onlyProvsReporting = onlyProvsReporting_arg
      )
  }else{
    
    print("not enough points")
    data_out <- data.frame()
  }
  
}





################################################################################
plot_performance_from_perf_series_df <- function(data = perf_series_df,
                                                 Code_arg = "RYJ",
                                                 weeklyOrMonthly_arg = "Monthly",
                                                 measure_arg = "All",
                                                 onlyProvsReporting_arg = TRUE,
                                                 
                                                 periodMin = 21,
                                                 noRecals = FALSE,
                                                 development_recalc_at_every_break = FALSE){
  
  data <- data %>%
    dplyr::filter(Code == Code_arg,
                  weeklyOrMonthly == weeklyOrMonthly_arg,
                  measure == measure_arg,
                  onlyProvsReporting == onlyProvsReporting_arg)
  
  #dataLength <- nrow(data)
  
  if(!(sum(is.na(data$Within_4h)) == nrow(data))) {
    plot_auto_SPC(data,
                  x = Month_Start,
                  y = Within_4h,
                  n = Total_Att,
                  chartType = "P'",
                  periodMin = periodMin,#dataLength,
                  plotChart = TRUE,
                  noRecals = noRecals,
                  title = paste(Code_arg, weeklyOrMonthly_arg, "P"),
                  development_recalc_at_every_break = development_recalc_at_every_break)
  } else {
    print("Performance NA")
    data_out <- data.frame()
  }
}

################################################################################
get_peformance_from_perf_series_df <- function(data = perf_series_df,
                                               Code_arg = "RYJ",
                                               weeklyOrMonthly_arg = "Monthly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               
                                               periodMin = 21,
                                               noRecals = FALSE,
                                               development_recalc_at_every_break = FALSE){
  
  data <- data %>%
    dplyr::filter(Code == Code_arg,
                  weeklyOrMonthly == weeklyOrMonthly_arg,
                  measure == measure_arg,
                  onlyProvsReporting == onlyProvsReporting_arg)
  
  #dataLength <- nrow(data)
  
  if(nrow(data) >= periodMin * 2){
    if(!(sum(is.na(data$Within_4h)) == nrow(data))) {
      data_out <- plot_auto_SPC(data,
                                x = Month_Start,
                                y = Within_4h,
                                n = Total_Att,
                                chartType = "P'",
                                periodMin = periodMin, #dataLength,
                                plotChart = FALSE,
                                noRecals = noRecals,
                                title = paste(Code_arg, weeklyOrMonthly_arg, measure_arg, onlyProvsReporting_arg),
                                development_recalc_at_every_break = development_recalc_at_every_break)
      
      data_out <- data_out %>%
        mutate(Code = Code_arg,
               weeklyOrMonthly = weeklyOrMonthly_arg,
               measure = measure_arg,
               onlyProvsReporting = onlyProvsReporting_arg
        )
    } else {
      print("Performance NA")
      data_out <- data.frame()
    }
  } else {
    
    print("not enough points")
    data_out <- data.frame()
  }
  
}

################################################################################
plot_performance_from_perf_series_df_naive1 <- function(data = perf_series_df,
                                                Code_arg = "RYJ",
                                                weeklyOrMonthly_arg = "Monthly",
                                                measure_arg = "All",
                                                onlyProvsReporting_arg = TRUE,
                                                
                                                periodMin = 21,
                                                noRecals = FALSE,
                                                development_recalc_at_every_break = FALSE){
  
  data <- data %>%
    dplyr::filter(Code == Code_arg,
                  weeklyOrMonthly == weeklyOrMonthly_arg,
                  measure == measure_arg,
                  onlyProvsReporting == onlyProvsReporting_arg)
  
  dataLength <- nrow(data)
  
  plot_auto_SPC(data,
                x = Month_Start,
                y = Within_4h,
                n = Total_Att,
                chartType = "P'",
                periodMin = dataLength,
                plotChart = TRUE,
                noRecals = noRecals,
                title = paste(Code_arg, weeklyOrMonthly_arg, "P"),
                development_recalc_at_every_break = development_recalc_at_every_break)
}

################################################################################
get_peformance_from_perf_series_df_naive1 <- function(data = perf_series_df,
                                                      Code_arg = "RYJ",
                                                      weeklyOrMonthly_arg = "Monthly",
                                                      measure_arg = "All",
                                                      onlyProvsReporting_arg = TRUE,
                                                      
                                                      periodMin = 21,
                                                      noRecals = FALSE,
                                                      development_recalc_at_every_break = FALSE){
  
  data <- data %>%
    dplyr::filter(Code == Code_arg,
                  weeklyOrMonthly == weeklyOrMonthly_arg,
                  measure == measure_arg,
                  onlyProvsReporting == onlyProvsReporting_arg)
  
  dataLength <- nrow(data)
  
  if(nrow(data) >= periodMin * 2){
    data_out <- plot_auto_SPC(data,
                              x = Month_Start,
                              y = Within_4h,
                              n = Total_Att,
                              chartType = "P'",
                              periodMin = dataLength,
                              plotChart = FALSE,
                              noRecals = noRecals,
                              title = paste(Code_arg, weeklyOrMonthly_arg, measure_arg, onlyProvsReporting_arg),
                              development_recalc_at_every_break = development_recalc_at_every_break)
    
    data_out <- data_out %>%
      mutate(Code = Code_arg,
             weeklyOrMonthly = weeklyOrMonthly_arg,
             measure = measure_arg,
             onlyProvsReporting = onlyProvsReporting_arg
      )
  }else{
    
    print("not enough points")
    data_out <- data.frame()
  }
  
}

################################################################################
get_results_summary <- function(limits_data = limits_table_output_C_charts){
  
  limits_data <- limits_data %>%
    group_by(Code, weeklyOrMonthly, measure, onlyProvsReporting) %>%
    arrange(Code, weeklyOrMonthly, measure, onlyProvsReporting) %>%
    mutate(startOfRule2 = ifelse((rule2 == TRUE & rule2 != dplyr::lag(rule2)) | (rule2 == TRUE & dplyr::row_number() == 1), TRUE, FALSE))
  
  num_points <- limits_data %>%
    count()
  
  num_recalcs <- limits_data %>%
    summarise(num_recalcs = sum(breakPoint, na.rm = TRUE))
  
  num_rule1_points <- limits_data %>%
    summarise(num_rule1_points = sum(rule1, na.rm = TRUE))
  
  num_rule2_points <- limits_data %>%
    summarise(num_rule2_points = sum(rule2, na.rm = TRUE))
  
  num_rule2_breaks <- limits_data %>%
    summarise(num_rule2_breaks = sum(startOfRule2, na.rm = TRUE))
  
  num_points_per_period <- limits_data %>%
    # group_by(Code, weeklyOrMonthly, measure, onlyProvsReporting, plotPeriod) %>%
    group_by(Code, weeklyOrMonthly, measure, onlyProvsReporting, cl) %>%
    count() %>%
    group_by(Code, weeklyOrMonthly, measure, onlyProvsReporting) %>%
    summarise(mean_num_points_per_period = mean(n))
    
  results_data <- num_points %>%
    left_join(num_recalcs, by = c("Code", "weeklyOrMonthly", "measure", "onlyProvsReporting")) %>%
    left_join(num_rule1_points, by = c("Code", "weeklyOrMonthly", "measure", "onlyProvsReporting")) %>%    
    left_join(num_rule2_points, by = c("Code", "weeklyOrMonthly", "measure", "onlyProvsReporting")) %>%
    left_join(num_rule2_breaks, by = c("Code", "weeklyOrMonthly", "measure", "onlyProvsReporting")) %>%
    left_join(num_points_per_period , by = c("Code", "weeklyOrMonthly", "measure", "onlyProvsReporting")) %>%
    ungroup() %>%
    mutate(minimumPeriod = if_else(weeklyOrMonthly == "Monthly", 24, 21)) %>%
    mutate(max_num_recalcs_possible = floor(n / minimumPeriod) - 1) %>%
    mutate(num_actual_recals_by_potential_recals = num_recalcs / max_num_recalcs_possible)
  
  summary_data_num_charts <- results_data %>%
    count()
  
  summary_data <- results_data %>%
    summarise(min_num_points = quantile(n)[1],
              lower_quartile_num_points = quantile(n)[2],
              median_num_points = quantile(n)[3],
              upper_quartile_num_points = quantile(n)[4],
              max_num_points = quantile(n)[5],
              mean_num_points = mean(n),
              sd_num_points = sd(n),
              
              min_num_recalcs = quantile(num_recalcs)[1],
              lower_quartile_num_recalcs = quantile(num_recalcs)[2],
              median_num_recalcs = quantile(num_recalcs)[3],
              upper_quartile_num_recalcs = quantile(num_recalcs)[4],
              max_num_recalcs = quantile(num_recalcs)[5],
              mean_num_recalcs = mean(num_recalcs),
              sd_num_recalcs = sd(num_recalcs),
              
              min_num_rule1_points = quantile(num_rule1_points)[1],
              lower_quartile_num_rule1_points = quantile(num_rule1_points)[2],
              median_num_rule1_points = quantile(num_rule1_points)[3],
              upper_quartile_num_rule1_points = quantile(num_rule1_points)[4],
              max_num_rule1_points = quantile(num_rule1_points)[5],
              mean_num_rule1_points = mean(num_rule1_points),
              sd_num_rule1_points = sd(num_rule1_points),
              
              min_num_rule2_points = quantile(num_rule2_points)[1],
              lower_quartile_num_rule2_points = quantile(num_rule2_points)[2],
              median_num_rule2_points = quantile(num_rule2_points)[3],
              upper_quartile_num_rule2_points = quantile(num_rule2_points)[4],
              max_num_rule2_points = quantile(num_rule2_points)[5],
              mean_num_rule2_points = mean(num_rule2_points),
              sd_num_rule2_points = sd(num_rule2_points),
              
              min_num_rule2_breaks = quantile(num_rule2_breaks)[1],
              lower_quartile_num_rule2_breaks = quantile(num_rule2_breaks)[2],
              median_num_rule2_breaks = quantile(num_rule2_breaks)[3],
              upper_quartile_num_rule2_breaks = quantile(num_rule2_breaks)[4],
              max_num_rule2_breaks = quantile(num_rule2_breaks)[5],
              mean_num_rule2_breaks = mean(num_rule2_breaks),
              sd_num_rule2_breaks = sd(num_rule2_breaks),
              
              min_num_actual_recals_by_potential_recals = quantile(num_actual_recals_by_potential_recals)[1],
              lower_quartile_num_actual_recals_by_potential_recals = quantile(num_actual_recals_by_potential_recals)[2],
              median_num_actual_recals_by_potential_recals = quantile(num_actual_recals_by_potential_recals)[3],
              upper_quartile_num_actual_recals_by_potential_recals = quantile(num_actual_recals_by_potential_recals)[4],
              max_num_actual_recals_by_potential_recals = quantile(num_actual_recals_by_potential_recals)[5],
              mean_num_actual_recals_by_potential_recals = mean(num_actual_recals_by_potential_recals),
              sd_num_actual_recals_by_potential_recals = sd(num_actual_recals_by_potential_recals),
              
              min_mean_num_points_per_period = quantile(mean_num_points_per_period)[1],
              lower_quartile_mean_num_points_per_period = quantile(mean_num_points_per_period)[2],
              median_mean_num_points_per_period = quantile(mean_num_points_per_period)[3],
              upper_quartile_mean_num_points_per_period = quantile(mean_num_points_per_period)[4],
              max_mean_num_points_per_period = quantile(mean_num_points_per_period)[5],
              mean_mean_num_points_per_period = mean(mean_num_points_per_period),
              sd_mean_num_points_per_period = sd(mean_num_points_per_period)
    ) %>%
    mutate(num_charts = summary_data_num_charts$n)
  
  list(results = results_data, summary = summary_data)
  
}

