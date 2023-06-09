library(tidyverse)
library(dplyr)
library(qicharts2)
library(ggplot2)
library(scales)
library(zoo)
library(lubridate)
library(wktmo)

make_perf_series <- function(df, code = "RQM", measure = "All", level, 
                             weeklyOrMonthly = "Monthly", onlyProvsReporting = F) {
  
  df <- regional_analysis(df, level, onlyProvsReporting)
  df <- filter(df, Code == code)
  
  # Make new variables
  df <- df %>% mutate(Att_Typ1_NotBr = Att_Typ1 - Att_Typ1_Br,
                      Att_Typ2_NotBr = Att_Typ2 - Att_Typ2_Br,
                      Att_Typ3_NotBr = Att_Typ3 - Att_Typ3_Br,
                      Att_All_NotBr = Att_All - Att_All_Br,
                      E_Adm_Not4hBr_D = E_Adm_All_ED - E_Adm_4hBr_D) 
  
  
  if(df[1,"Nat_Code"] == "Sc"){
    perf_series <- df %>%
      select(Code, Month_Start, Name, Nat_Code,
             Within_4h = Att_All_NotBr, Greater_4h = Att_All_Br, Total_Att = Att_All,Total_Adm = E_Adm_All_ED)
    if(weeklyOrMonthly != "Weekly"){
      perf_series <- weekly_to_monthly(perf_series)
    }
  }else{
    perf_series <- switch(measure,
                          All = df %>%
                            select(Code, Month_Start, Name, Nat_Code,
                                   Within_4h = Att_All_NotBr, Greater_4h = Att_All_Br, Total_Att = Att_All,
                                   Total_Adm = E_Adm_All_ED),
                          Typ1 = df %>%
                            select(Code, Month_Start, Name, Nat_Code,
                                   Within_4h = Att_Typ1_NotBr, Greater_4h = Att_Typ1_Br, Total_Att = Att_Typ1,
                                   Total_Adm = E_Adm_Typ1)#,
                          # Adm = df %>%
                          #   select(Code, Month_Start, Name, Nat_Code,
                          #          Within_4h = E_Adm_Not4hBr_D, Greater_4h = E_Adm_4hBr_D, Total = E_Adm_All_ED,
                          #          E_Adm_Typ1, E_Adm_All_ED)
    )
  }
  
  perf_series %>% mutate(Performance = Within_4h / Total_Att) %>%
    mutate(Month_Start = as.Date(Month_Start, tz = 'Europe/London')) %>%
    #two new cols: 
    #if National code - i.e. Scotland $ weekly, return number of days - i.e. 7
    #else return number of days in a months
    arrange(Month_Start) %>% mutate(days_in_period = ifelse(Nat_Code == "Sc" & weeklyOrMonthly == "Weekly",7, days_in_month(df$Month_Start))) %>%
    mutate(daily_ave_att = Total_Att/days_in_period) %>%
    mutate(daily_ave_adm = Total_Adm/days_in_period)
  
}


regional_analysis <- function(df, level, onlyProvsReporting){
  
  if(level == "National"){
    Name <- "Country"
    Code <- "Nat_Code"
  }else if(level == "Regional"){
    Name <- "Region"
    Code <- "Reg_Code"
  }else{
    Name <- "Prov_Name"
    Code <- "Prov_Code"
  }
  
  Name <- as.name(Name)
  Code <- as.name(Code)
  
  df <- df %>%
    mutate(Name = !!Name, Code = !!Code)
  
  if(onlyProvsReporting == T & (level == "National" | level == "Regional")){
    provs_not_reporting <- df %>% filter(is.na(Att_All_Br)) %>% distinct(Prov_Code) %>% pull(Prov_Code)
    df <- df %>%
      filter(!(Prov_Code %in% provs_not_reporting)) 
  }
  
  dfReg <- df %>%
    group_by(Name, Code, Month_Start) %>%
    summarise(Att_Typ1 = sum(Att_Typ1), Att_Typ2 = sum(Att_Typ2),
              Att_Typ3 = sum(Att_Typ3), Att_All = sum(Att_All), Att_Typ1_Br = sum(Att_Typ1_Br),
              Att_Typ2_Br = sum(Att_Typ2_Br), Att_Typ3_Br = sum(Att_Typ3_Br), Att_All_Br = sum(Att_All_Br),
              Perf_Typ1 = (Att_Typ1 - Att_Typ1_Br)/Att_Typ1, Perf_All = (Att_All - Att_All_Br)/Att_All,
              E_Adm_Typ1 = sum(E_Adm_Typ1), E_Adm_Typ2 = sum(E_Adm_Typ2), E_Adm_Typ34 = sum(E_Adm_Typ34),
              E_Adm_All_ED = sum(E_Adm_All_ED), E_Adm_Not_ED = sum(E_Adm_Not_ED), E_Adm_All = sum(E_Adm_All),
              E_Adm_4hBr_D = sum(E_Adm_4hBr_D), E_Adm_12hBr_D = sum(E_Adm_12hBr_D),
              Nat_Code = dplyr::first(Nat_Code)) %>%
    ungroup()
  
}


#Reg_codes are made up so London=Lo, Midlands=Mi, North=No, South=So.  Make sure these aren't the same as Scottish Reg_Codes!
clean_region_col <- function(df){
  
  df <- df %>%
    mutate(Region = ifelse(str_detect(Region, coll("london",ignore_case = T)), "London", 
                           ifelse(str_detect(Region, coll("midlands",ignore_case = T)),"Midlands", 
                                  ifelse(str_detect(Region, coll("north",ignore_case = T)),"North of England", 
                                         "South of England"))),
           Reg_Code = ifelse(Region == "London", "Lo", 
                             ifelse(Region == "Midlands", "Mi", 
                                    ifelse(Region == "North of England", "No", "So"))), 
           Country = "England", Nat_Code = "E")
}

#function to out Scottish data into NHS England format to be used in the app
standardise_data <- function(df){
  
  df <- df %>%
    select(-c(Att_8hr_Br, Perf_8hr, Att_12hr_Br, Perf_12hr)) %>%
    rename(Region = Board_Name, Reg_Code = Board_Code, Att_All_Br = Att_4hr_Br, Perf_All = Perf_4hr, Month_Start = Week_End) %>%
    mutate(Country = "Scotland", Nat_Code = "Sc") %>%
    mutate(Att_Typ1 = NA, Att_Typ2 = NA, Att_Typ3 = NA, Att_Typ1_Br = NA, Att_Typ2_Br = NA, Att_Typ3_Br = NA,
           Perf_Typ1 = NA, E_Adm_Typ1 = NA, E_Adm_Typ2 = NA, E_Adm_Typ34 = NA, E_Adm_All_ED = NA, 
           E_Adm_Not_ED = NA, E_Adm_All = NA, E_Adm_4hBr_D = NA, E_Adm_12hBr_D = NA) %>%
    mutate(Region = gsub("NHS ","", Region))
  
}

weekly_to_monthly <- function(df){
  
  df <- mutate(df, Month_Start = as.Date(Month_Start))
  df <- df %>% arrange(Month_Start)
  datStart <- as.character(df$Month_Start[1] - weeks(1))  ###because dates are given as week_end
  
  Within_4h_df <- weekToMonth(df$Within_4h, datStart = datStart, wkMethod = "startDat", format = "%Y-%m-%d")
  Within_4h <- Within_4h_df$value
  Greater_4h_df <- weekToMonth(df$Greater_4h, datStart = datStart, wkMethod = "startDat", format = "%Y-%m-%d")
  Greater_4h <- Greater_4h_df$value
  TotalAtt_df <- weekToMonth(df$Total_Att, datStart = datStart, wkMethod = "startDat", format = "%Y-%m-%d")
  Total_Att <- TotalAtt_df$value
  TotalAdm_df <- weekToMonth(df$Total_Adm, datStart = datStart, wkMethod = "startDat", format = "%Y-%m-%d")
  Total_Adm <- TotalAdm_df$value
  Month_Start <- TotalAtt_df$yearMonth
  
  dfMonthly <- data.frame(Month_Start, Within_4h, Greater_4h, Total_Att, Total_Adm)
  dfMonthly <- dfMonthly %>%
    mutate(Code = df$Code[nrow(df)], Name = df$Name[nrow(df)], Nat_Code = df$Nat_Code[nrow(df)]) %>%
    filter(row_number() != 1 & row_number() != nrow(dfMonthly)) %>% 
    mutate(Month_Start = as.yearmon(Month_Start, tz = 'Europe/London'))
  
}

plot_performance <- function(df, code = "RBZ", date.col = 'Month_Start',
                             start.date = "2015-07-01", end.date = lubridate::today(),
                             brk.date = NULL, max_lower_y_scale = 60,
                             measure = "All", plot.chart = TRUE,
                             pr_name = NULL, x_title = "Month",
                             r1_col = "orange", r2_col = "steelblue3",
                             level = level, weeklyOrMonthly = "Monthly",
                             onlyProvsReporting = onlyProvsReporting) { 
  
  cht_title = "Percentage A&E attendances\nwith time in department < 4h"
  
  # df <- make_perf_series(df = df, code = code, measure = measure, level = level, weeklyOrMonthly = weeklyOrMonthly,
  #                        onlyProvsReporting = onlyProvsReporting)
  
  #It gets confused if the filter value is the same as the column name
  levelFilter <- level
  weeklyOrMonthlyFilter <- weeklyOrMonthly
  measureFilter <- measure
  onlyProvsReportingFilter <- onlyProvsReporting
  
  df <- df %>%
    filter(Code == code & level == level & weeklyOrMonthly == weeklyOrMonthlyFilter & measure == measureFilter & onlyProvsReporting == onlyProvsReportingFilter)
  
  df <- filter(df, !(is.na(Within_4h)))
  
  # if no pr_name passed, lookup full name of provider
  if (is.null(pr_name)) {pr_name <- df %>% top_n(1, wt = !!date.col) %>% pull(Name)}
  
  # convert arguments to dates
  st.dt <- as.Date(start.date, tz = "Europe/London")
  ed.dt <- as.Date(end.date, tz = "Europe/London")
  q.st.dt <- as.Date(zoo::as.yearqtr(st.dt, format="%Y-%m-%d"))
  q.ed.dt <- as.Date(zoo::as.yearqtr(ed.dt, format="%Y-%m-%d"), frac = 1) + 1
  cht_axis_breaks <- seq(q.st.dt, q.ed.dt, "quarters")
  
  # restrict to the period specified
  df <- df %>% filter(Month_Start >= st.dt, Month_Start <= ed.dt)
  if(nrow(df)==0) {stop("No data for provider period specified")}
  
  if (is.null(brk.date)) {
    pct <- qicharts2::qic(Month_Start, Within_4h, n = Total_Att, data = df, chart = 'pp', multiply = 100)
    pct$data$x <- as.Date(pct$data$x, tz = 'Europe/London')
    cht_data <- add_rule_breaks(pct$data)
    pct <- ggplot(cht_data, aes(x,y, label = x))
  } else {
    br.dt <- as.Date(brk.date)
    # locate break row
    v <- df$Month_Start
    br.row <- which(v == max(v[v < br.dt]))
    
    pct <- qicharts2::qic(Month_Start, Within_4h, n = Total_Att, data = df, chart = 'pp', multiply = 100,
                          freeze = br.row)
    pct$data$x <- as.Date(pct$data$x, tz = 'Europe/London')
    cht_data <- add_rule_breaks(pct$data)
    pct <- ggplot(cht_data, aes(x,y))
  }
  
  # chart y limit
  ylimlow <- min(min(pct$data$y, na.rm = TRUE),min(pct$data$lcl, na.rm = TRUE), max_lower_y_scale)
  # data for target line
  cutoff <- data.frame(yintercept=95, cutoff=factor(95))
  
  #currently only England differentiates by type
  if(df$Nat_Code[1] == "E"){
    typeTitle <- ifelse(measure == "Typ1", "\nType 1 departments only", "\nAll department types")
  }else{
    typeTitle <- ""
  }
  
  if(level == "National"){
    levelTitle <- "Country:"
  }else if(level == "Regional"){
    levelTitle <- ifelse(df$Nat_Code[1] == "E", "Region:","Board:")
  }else{
    levelTitle <- ""
  }
  
  if(onlyProvsReporting == T & (level == "National" | level == "Regional")){
    reportingTitle <- "\nIncludes only providers that are still reporting"
  }else if(onlyProvsReporting == F & (level == "National" | level == "Regional")){
    reportingTitle <- "\nIncludes providers that are no longer reporting"
  }else{
    reportingTitle <- ""
  }
  
  if(plot.chart == TRUE) {
    format_control_chart(pct, r1_col = r1_col, r2_col = r2_col) +
      geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff, colour = '#00BB00', linetype = 1) +
      scale_x_date(labels = date_format("%Y-%m"), breaks = cht_axis_breaks,
                   limits = c(q.st.dt, q.ed.dt)) +
      annotate("text", ed.dt - 90, 95, vjust = -2, label = "95% Target", colour = '#00BB00') +
      ggtitle(cht_title, subtitle = paste0(levelTitle, pr_name, typeTitle, reportingTitle)) +
      labs(x= x_title, y="Percentage within 4 hours",
           caption = "*Shewhart chart rules apply (see Understanding the Analysis tab for more detail) \nRule 1: Any month outside the control limits \nRule 2: Eight or more consecutive months all above, or all below, the centre line", size = 10) +
      ylim(ylimlow,100) +
      geom_text(aes(label=ifelse(x==max(x), format(x, '%b-%y'),'')),hjust=-0.05, vjust= 2)
    
  } else {df}
}


plot_volume <- function(df, code = "RBZ", date.col = 'Month_Start',
                        start.date = "2015-07-01", end.date = lubridate::today(),
                        brk.date = NULL, max_lower_y_scale = 60,
                        measure = "All", plot.chart = TRUE,
                        pr_name = NULL, x_title = "Month",
                        r1_col = "orange", r2_col = "steelblue3", level = "Provider", 
                        weeklyOrMonthly = "Monthly",
                        onlyProvsReporting = onlyProvsReporting,
                        attOrAdm = "Attendances") { 
  
  if(attOrAdm == "Attendances"){
    cht_title <- ifelse(weeklyOrMonthly == "Weekly", "Average daily A&E attendances per week", "Average daily A&E attendances per month")
  }else{
    cht_title <- ifelse(weeklyOrMonthly == "Weekly", "Average daily admissions from A&E per week", "Average daily admissions from A&E per month")
  }
  
  
  df_original <- df
  # df <- make_perf_series(df = df_original, code = code, measure = measure, level = level,
  #                        weeklyOrMonthly = weeklyOrMonthly, onlyProvsReporting = onlyProvsReporting)
  # df_all <- make_perf_series(df = df_original, code = code, measure = measure, level = level, 
  #                        weeklyOrMonthly = weeklyOrMonthly, onlyProvsReporting = FALSE)
  
  #It gets confused if the filter value is the same as the column name
  levelFilter <- level
  weeklyOrMonthlyFilter <- weeklyOrMonthly
  measureFilter <- measure
  onlyProvsReportingFilter <- onlyProvsReporting
  
  df <- df_original %>%
    filter(Code == code & level == level & weeklyOrMonthly == weeklyOrMonthlyFilter & measure == measureFilter & onlyProvsReporting == onlyProvsReportingFilter)
  
  df_all <- df_original %>%
    filter(Code == code & level == level & weeklyOrMonthly == weeklyOrMonthlyFilter & measure == measureFilter & onlyProvsReporting == FALSE)
  
  
  # if no pr_name passed, lookup full name of provider
  if (is.null(pr_name)) {pr_name <- df %>% top_n(1, wt = Performance) %>% pull(Name)}
  
  # convert arguments to dates and round to nearest quarter
  st.dt <- as.Date(start.date, tz = "Europe/London")
  ed.dt <- as.Date(end.date, tz = "Europe/London")
  q.st.dt <- as.Date(zoo::as.yearqtr(st.dt, format="%Y-%m-%d"))
  q.ed.dt <- as.Date(zoo::as.yearqtr(ed.dt, format="%Y-%m-%d"), frac = 1) + 1
  cht_axis_breaks <- seq(q.st.dt, q.ed.dt, "quarters")
  
  # restrict to the period specified
  df <- df %>% filter(Month_Start >= st.dt, Month_Start <= ed.dt)
  if(nrow(df)==0) {stop("No data for provider period specified")}
  
  if (is.null(brk.date)) {
    #Total is replaced with the new col "daily_ave"
    if(attOrAdm == "Attendances"){
      pct <- qicharts2::qic(Month_Start, daily_ave_att, n = rep(1, nrow(df)), data = df, chart = 'up')
    }else{
      pct <- qicharts2::qic(Month_Start, daily_ave_adm, n = rep(1, nrow(df)), data = df, chart = 'up')
    }
    
    pct$data$x <- as.Date(pct$data$x, tz = 'Europe/London')
    cht_data <- add_rule_breaks(pct$data)
    pct <- ggplot(cht_data, aes(x,y))
  } else {
    br.dt <- as.Date(brk.date)
    # locate break row
    v <- df$Month_Start
    br.row <- which(v == max(v[v < br.dt]))
    
    #Total is replaced with the new col "daily_ave"
    pct <- ifelse(attOrAdm == "Attendances",
                  qicharts2::qic(Month_Start, daily_ave_att, n = rep(1, nrow(df)), data = df, chart = 'up', freeze = br.row),
                  qicharts2::qic(Month_Start, daily_ave_adm, n = rep(1, nrow(df)), data = df, chart = 'up', freeze = br.row))
    pct$data$x <- as.Date(pct$data$x, tz = 'Europe/London')
    cht_data <- add_rule_breaks(pct$data)
    pct <- ggplot(cht_data, aes(x,y))
    
  }
  
  # chart y limit
  ylimlow <- 0
  ylimhigh <- ifelse(attOrAdm == "Attendances",
                     ceiling(max(df_all$daily_ave_att)*1.1),
                     ceiling(max(df_all$daily_ave_adm)*1.1))
  
  cutoff <- data.frame(yintercept=95, cutoff=factor(95))
  
  # for subtitle
  if(df$Nat_Code[1] == "E"){
    typeTitle <- ifelse(measure == "Typ1", "\nType 1 departments only", "\nAll department types")
  }else{
    typeTitle <- ""
  }
  
  if(level == "National"){
    levelTitle <- "Country:"
  }else if(level == "Regional"){
    levelTitle <- ifelse(df$Nat_Code[1] == "E", "Region:","Board:")
  }else{
    levelTitle <- ""
  }
  
  if(onlyProvsReporting == T & (level == "National" | level == "Regional")){
    reportingTitle <- "\nIncludes only providers that are still reporting"
  }else if(onlyProvsReporting == F & (level == "National" | level == "Regional")){
    reportingTitle <- "\nIncludes providers that are no longer reporting"
  }else{
    reportingTitle <- ""
  }
  
  if(plot.chart == TRUE) {
    format_control_chart(pct, r1_col = r1_col, r2_col = r2_col) +
      scale_x_date(labels = date_format("%Y-%m"), breaks = cht_axis_breaks,
                   limits = c(q.st.dt, q.ed.dt)) +
      ggtitle(cht_title, subtitle = paste0(levelTitle, pr_name, typeTitle, reportingTitle)) +
      labs(x= x_title, y=ifelse(attOrAdm == "Attendances","Average daily attendances", "Average daily admissions"),
           caption = "*Shewhart chart rules apply (see Understanding the Analysis tab for more detail) \nRule 1: Any month outside the control limits \nRule 2: Eight or more consecutive months all above, or all below, the centre line",
           size = 10) +
      scale_y_continuous(limits = c(ylimlow, ylimhigh),
                         breaks = breaks_pretty(),
                         labels = number_format(accuracy = 1, big.mark = ",")
      )
    
  } else {df}
}

format_control_chart <- function(cht, r1_col, r2_col) {
  point_colours <- c("Rule 1" = r1_col, "Rule 2" = r2_col, "None" = "black")
  cht + 
    geom_line(colour = "black", size = .5) + 
    geom_line(aes(x,cl), size = 0.75) +
    geom_line(aes(x,ucl), size = 0.75, linetype = 2) +
    geom_line(aes(x,lcl), size = 0.75, linetype = 2) +
    geom_point(aes(colour = highlight), size = 2) +
    scale_color_manual("Rule triggered*", values = point_colours) + 
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey80"),
          panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.0, size = 14),
          axis.text.y = element_text(size = 14), axis.title = element_text(size = 14),
          plot.title = element_text(size = 20, hjust = 0),
          plot.subtitle = element_text(size = 16, face = "italic"),
          axis.line = element_line(colour = "grey60"),
          plot.caption = element_text(size = 10, hjust = 0.5))
}

#function to get table of perf series
make_perf_series_data_frame <- function(data = AE_Data,
                                        data_scot = AE_Data_Scot,
                                        measureArg = "All",#"Typ1"
                                        onlyProvsReportingArg = TRUE
){
  
  #get lists of providers, regions and counties
  provs <- unique(data$Prov_Code)
  regs <- unique(data$Reg_Code)
  nats <- unique(data$Nat_Code)
  
  provs_scot <- unique(data_scot$Prov_Code)
  regs_scot <- unique(data_scot$Reg_Code)
  nats_scot <- unique(data_scot$Nat_Code)
  
  #initialise empty dataframes
  perf_series_df_provs_monthly_All_F <- data.frame()
  perf_series_df_regs_monthly_All_F <- data.frame()
  perf_series_df_nats_monthly_All_F <- data.frame()
  
  perf_series_df_scot_provs_weekly_All_F <- data.frame()
  perf_series_df_scot_regs_weekly_All_F <- data.frame()
  perf_series_df_scot_nats_weekly_All_F <- data.frame()
  
  #loops through all providers, monthly aggregation
  for(i in provs){
    df <- make_perf_series(data, code = i, measure = measureArg, level = "Provider",
                           weeklyOrMonthly = "Monthly", onlyProvsReporting = onlyProvsReportingArg)
    
    perf_series_df_provs_monthly_All_F <- bind_rows(perf_series_df_provs_monthly_All_F, df)
    
    print(paste(i, "monthly"))
  }
  
  perf_series_df_provs_monthly_All_F <- perf_series_df_provs_monthly_All_F %>%
    mutate(level = "Provider",
           weeklyOrMonthly = "Monthly",
           measure = measureArg,
           onlyProvsReporting = onlyProvsReportingArg
    )
  
  #loops through all regions, monthly aggregation
  for(i in regs){
    df <- make_perf_series(data, code = i, measure = measureArg, level = "Regional",
                           weeklyOrMonthly = "Monthly", onlyProvsReporting = onlyProvsReportingArg)
    
    perf_series_df_regs_monthly_All_F <- bind_rows(perf_series_df_regs_monthly_All_F, df)
    
    print(paste(i, "monthly"))
  }
  
  perf_series_df_regs_monthly_All_F <- perf_series_df_regs_monthly_All_F %>%
    mutate(level = "Regional",
           weeklyOrMonthly = "Monthly",
           measure = measureArg,
           onlyProvsReporting = onlyProvsReportingArg
    )
  
  #loops through countries, monthly aggregation
  for(i in nats){
    df <- make_perf_series(data, code = i, measure = measureArg, level = "National",
                           weeklyOrMonthly = "Monthly", onlyProvsReporting = onlyProvsReportingArg)
    
    perf_series_df_nats_monthly_All_F <- bind_rows(perf_series_df_nats_monthly_All_F, df)
    
    print(paste(i, "monthly"))
  }
  
  perf_series_df_nats_monthly_All_F <- perf_series_df_nats_monthly_All_F %>%
    mutate(level = "National",
           weeklyOrMonthly = "Monthly",
           measure = measureArg,
           onlyProvsReporting = onlyProvsReportingArg
    )
  
  #loops through Scottish providers, weekly aggregation
  for(i in provs_scot){
    df <- make_perf_series(data, code = i, measure = measureArg, level = "Provider",
                           weeklyOrMonthly = "Weekly", onlyProvsReporting = onlyProvsReportingArg)
    
    perf_series_df_scot_provs_weekly_All_F <- bind_rows(perf_series_df_scot_provs_weekly_All_F, df)
    
    print(paste(i, "weekly"))
  }
  
  perf_series_df_scot_provs_weekly_All_F <- perf_series_df_scot_provs_weekly_All_F %>%
    mutate(level = "Provider",
           weeklyOrMonthly = "Weekly",
           measure = measureArg,
           onlyProvsReporting = onlyProvsReportingArg
    )
  
  #loops through Scottish boards (regions), weekly aggregation
  for(i in regs_scot){
    df <- make_perf_series(data, code = i, measure = measureArg, level = "Regional",
                           weeklyOrMonthly = "Weekly", onlyProvsReporting = onlyProvsReportingArg)
    
    perf_series_df_scot_regs_weekly_All_F <- bind_rows(perf_series_df_scot_regs_weekly_All_F, df)
    
    print(paste(i, "weekly"))
  }
  
  perf_series_df_scot_regs_weekly_All_F <- perf_series_df_scot_regs_weekly_All_F %>%
    mutate(level = "Regional",
           weeklyOrMonthly = "Weekly",
           measure = measureArg,
           onlyProvsReporting = onlyProvsReportingArg
    )
  
  #loops through Scotland, weekly aggregation
  for(i in nats_scot){
    df <- make_perf_series(data, code = i, measure = measureArg, level = "National",
                           weeklyOrMonthly = "Weekly", onlyProvsReporting = onlyProvsReportingArg)
    
    perf_series_df_scot_nats_weekly_All_F <- bind_rows(perf_series_df_scot_nats_weekly_All_F, df)
    
    print(paste(i, "weekly"))
  }
  
  perf_series_df_scot_nats_weekly_All_F <- perf_series_df_scot_nats_weekly_All_F %>%
    mutate(level = "National",
           weeklyOrMonthly = "Weekly",
           measure = measureArg,
           onlyProvsReporting = onlyProvsReportingArg
    )
  
  #all outputs bound together to load into database
  perf_series_df <- bind_rows(perf_series_df_provs_monthly_All_F,
                              perf_series_df_regs_monthly_All_F,
                              perf_series_df_nats_monthly_All_F,
                              perf_series_df_scot_provs_weekly_All_F,
                              perf_series_df_scot_regs_weekly_All_F,
                              perf_series_df_scot_nats_weekly_All_F)
  
  #edit column types to be compatible with database
  perf_series_df <- perf_series_df %>%
    mutate(onlyProvsReporting = as.character(onlyProvsReporting))
}

# # perf_series_df_all_FALSE <- make_perf_series_data_frame(data = AE_Data,
# #                                                         data_scot = AE_Data_Scot,
# #                                                         measureArg = "All",
# #                                                         onlyProvsReportingArg = FALSE)
# # 
# # perf_series_df_Typ1_FALSE <- make_perf_series_data_frame(data = AE_Data,
# #                                                          data_scot = AE_Data_Scot,
# #                                                          measureArg = "Typ1",
# #                                                          onlyProvsReportingArg = FALSE)
# 
# perf_series_df_all_TRUE <- make_perf_series_data_frame(data = AE_Data,
#                                                        data_scot = AE_Data_Scot,
#                                                        measureArg = "All",
#                                                        onlyProvsReportingArg = TRUE)
# 
# # perf_series_df_Typ1_TRUE <- make_perf_series_data_frame(data = AE_Data,
# #                                                         data_scot = AE_Data_Scot,
# #                                                         measureArg = "Typ1",
# #                                                         onlyProvsReportingArg = TRUE)
# 
# 
# # perf_series_df <- bind_rows(perf_series_df_all_FALSE,
# #                             perf_series_df_Typ1_FALSE,
# #                             perf_series_df_all_TRUE,
# #                             perf_series_df_Typ1_TRUE)