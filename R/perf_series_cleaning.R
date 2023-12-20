# Function to return the distance between the two quantiles of v that are
# mid_range_width apart and centred on the median. For mid_range_width = 0.5
# this is the IQR.
mid_range <- function(v,
                      mid_range_width) {
  
  v <- v[!is.na(v)]
  
  if(length(v) == 0) {
    return(NA_real_)
  }
  
  if(abs(mid_range_width) >= 1 | abs(mid_range_width) <= 0.01) {
    stop("mid_range_width must be strictly between 0.01 and 1")
  }
  
  upper_quantile <- quantile(v,
                             0.5 + mid_range_width/2)
  lower_quantile <- quantile(v,
                             0.5 - mid_range_width/2)
  
  mid_range_result <- upper_quantile - lower_quantile
  names(mid_range_result) <- NULL
  
  return(mid_range_result)
  
}

# Remove series with fewer than 24 points, and suppress (by setting to NA)
# performance series that do not exhibit enough variation - defined to be
# the mid range of a specified width at least some minimum value.
# Defaults to IQR at least 1%.
clean_perf_series_df <- function(df,
                                 mid_range_min = 0.01,
                                 mid_range_width = 0.5) {
  
  exclusion_results <- df %>% 
    group_by(Code, weeklyOrMonthly) %>% 
    summarise(num_rows = n(),
              mid_range = mid_range(v = Performance,
                                    mid_range_width = mid_range_width)) %>% 
    mutate(mid_range_ok = case_when(
      is.na(mid_range) ~ FALSE,
      mid_range >= mid_range_min ~ TRUE,
      TRUE ~ FALSE),
      n_ok = if_else(num_rows >= 48,
                     TRUE,
                     FALSE),
      include = mid_range_ok & n_ok)
  
  n_problems <- exclusion_results %>% 
    filter(!n_ok) %>% 
    nrow()
  
  mid_range_problems <- exclusion_results %>% 
    filter(!mid_range_ok) %>% 
    nrow()
  
  df_out <- df %>%
    left_join(exclusion_results %>% 
                select(Code,
                       weeklyOrMonthly,
                       mid_range_ok,
                       n_ok),
              by = c("Code" = "Code",
                     "weeklyOrMonthly" = "weeklyOrMonthly")) %>% 
    filter(n_ok) %>% 
    mutate(Performance = if_else(mid_range_ok,
                                 Performance,
                                 NA_real_)) %>% 
    select(-mid_range_ok,
           -n_ok)
  
  print(paste0(n_problems, " Code-TimeGranularity pairings removed for insufficient data"))
  print(paste0(mid_range_problems, " Performance series removed for insufficient variation"))
  
  return(df_out)
}