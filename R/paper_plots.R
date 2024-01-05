library(tidyverse)
library(autospc)
library(ggh4x)

save_plots <- TRUE
hospital_name <- "Hospital A"

perf_series_df <- readRDS("data/perf_series_df.rds")

G513H_data <- filter(perf_series_df, Code == "G513H", weeklyOrMonthly == "Monthly")
G513H_data_weekly <- filter(perf_series_df, Code == "G513H", weeklyOrMonthly == "Weekly")

# Simulate data for figure 1

set.seed(1234L)

simulated_data <- tibble(x = seq.Date(from = as.Date("2023-01-01"),
                                      by = 7L,
                                      length.out = 44L),
                         y = rpois(44L, 200L)) %>% 
  mutate(y = if_else(row_number() %in% c(35:38, 41), y + 25, y),
         y = if_else(row_number() %in% c(42), y, y))


# Plot charts

pA <- plot_auto_SPC(df = head(simulated_data, 21),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Example C-Chart",
                    subtitle = "Simulated Data",
                    plotChart = TRUE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x_break = 28,
                    x_pad_end = as.Date("2023-12-01"),
                    extend_limits_to = as.Date("2023-09-01"),
                    override_y_lim = 60,
                    includeAnnotations = FALSE)


pB <- plot_auto_SPC(df = head(simulated_data, 26),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Example C-Chart",
                    subtitle = "Simulated Data",
                    plotChart = TRUE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x_break = 28,
                    x_pad_end = as.Date("2023-12-01"),
                    extend_limits_to = as.Date("2023-10-08"),
                    override_y_lim = 60,
                    includeAnnotations = FALSE)


pC <- plot_auto_SPC(df = head(simulated_data, 44),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Example C-Chart",
                    subtitle = "Simulated Data",
                    plotChart = TRUE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x_break = 28,
                    x_pad_end = as.Date("2023-12-01"),
                    extend_limits_to = as.Date("2023-12-01"),
                    override_y_lim = 60,
                    includeAnnotations = FALSE)


p1 <- plot_auto_SPC(df = head(G513H_data, 21),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = TRUE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    override_y_lim = 300,
                    includeAnnotations = FALSE)

p2 <- plot_auto_SPC(df = head(G513H_data, 30),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = TRUE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-06-01"),
                    override_y_lim = 300,
                    includeAnnotations = FALSE)


p3 <- plot_auto_SPC(df = head(G513H_data, 54),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = TRUE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    noRecals = TRUE,
                    override_y_lim = 300,
                    includeAnnotations = FALSE)

p4 <- plot_auto_SPC(df = head(G513H_data, 54),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = TRUE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    noRecals = FALSE,
                    override_y_lim = 300,
                    includeAnnotations = FALSE)


p5 <- plot_auto_SPC(df = head(G513H_data, 74),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = TRUE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    noRecals = FALSE,
                    override_y_lim = 300,
                    includeAnnotations = FALSE)

p6 <- plot_auto_SPC(df = head(G513H_data, 97),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 0,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = TRUE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    noRecals = FALSE,
                    override_y_lim = 300,
                    includeAnnotations = FALSE,
                    development_recalc_at_every_break = TRUE)

p7 <- plot_auto_SPC(df = head(G513H_data, 97),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = TRUE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    noRecals = FALSE,
                    override_y_lim = 300,
                    includeAnnotations = FALSE)

p_algorithm <- plot_auto_SPC(df = G513H_data,
              periodMin = 21,
              runRuleLength = 8,
              maxNoOfExclusions = 3,
              highlightExclusions = TRUE,
              title = "Average daily A&E attendances per month",
              subtitle = hospital_name,
              plotChart = TRUE,
              writeTable = FALSE,
              noRegrets = TRUE,
              chartType = "C'",
              x = Month_Start,
              y = daily_ave_att,
              x_break = 210,
              x_pad_end = as.Date("2023-03-01"),
              override_y_lim = 300)

p_naive1 <- plot_auto_SPC(df = G513H_data,
                          periodMin = nrow(G513H_data),
                          runRuleLength = 8,
                          maxNoOfExclusions = 3,
                          highlightExclusions = TRUE,
                          title = "Average daily A&E attendances per month",
                          subtitle = paste0(hospital_name, "\nNaive Approach 1"),
                          plotChart = TRUE,
                          writeTable = FALSE,
                          noRegrets = TRUE,
                          chartType = "C'",
                          x = Month_Start,
                          y = daily_ave_att,
                          x_break = 210,
                          x_pad_end = as.Date("2023-03-01"),
                          noRecals = FALSE,
                          override_y_lim = 300,
                          includeAnnotations = FALSE)


p_naive2 <- plot_auto_SPC(df = G513H_data,
                          periodMin = 21,
                          runRuleLength = 8,
                          maxNoOfExclusions = 3,
                          highlightExclusions = TRUE,
                          title = "Average daily A&E attendances per month",
                          subtitle = paste0(hospital_name, "\nNaive Approach 2"),
                          plotChart = TRUE,
                          writeTable = FALSE,
                          noRegrets = TRUE,
                          chartType = "C'",
                          x = Month_Start,
                          y = daily_ave_att,
                          x_break = 210,
                          x_pad_end = as.Date("2023-03-01"),
                          noRecals = TRUE,
                          override_y_lim = 300,
                          includeAnnotations = FALSE)


p_naive3 <- plot_auto_SPC(df = G513H_data,
                          periodMin = 21,
                          runRuleLength = 8,
                          maxNoOfExclusions = 3,
                          highlightExclusions = TRUE,
                          title = "Average daily A&E attendances per month",
                          subtitle = paste0(hospital_name, "\nNaive Approach 3"),
                          plotChart = TRUE,
                          writeTable = FALSE,
                          noRegrets = TRUE,
                          chartType = "C'",
                          x = Month_Start,
                          y = daily_ave_att,
                          x_break = 210,
                          x_pad_end = as.Date("2023-03-01"),
                          noRecals = FALSE,
                          override_y_lim = 300,
                          includeAnnotations = FALSE,
                          development_recalc_at_every_break = TRUE)




dataA <- plot_auto_SPC(df = head(simulated_data, 21),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Example C-Chart",
                    subtitle = "Simulated Data",
                    plotChart = FALSE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x_break = 28,
                    x_pad_end = as.Date("2023-12-01"),
                    extend_limits_to = as.Date("2023-09-01"),
                    override_y_lim = 60,
                    includeAnnotations = FALSE)

dataB <- plot_auto_SPC(df = head(simulated_data, 26),
                       periodMin = 21,
                       runRuleLength = 8,
                       maxNoOfExclusions = 3,
                       highlightExclusions = TRUE,
                       title = "Example C-Chart",
                       subtitle = "Simulated Data",
                       plotChart = FALSE,
                       writeTable = FALSE,
                       noRegrets = TRUE,
                       chartType = "C'",
                       x_break = 28,
                       x_pad_end = as.Date("2023-12-01"),
                       extend_limits_to = as.Date("2023-10-08"),
                       override_y_lim = 60,
                       includeAnnotations = FALSE)

dataC <- plot_auto_SPC(df = head(simulated_data, 44),
                       periodMin = 21,
                       runRuleLength = 8,
                       maxNoOfExclusions = 3,
                       highlightExclusions = TRUE,
                       title = "Example C-Chart",
                       subtitle = "Simulated Data",
                       plotChart = FALSE,
                       writeTable = FALSE,
                       noRegrets = TRUE,
                       chartType = "C'",
                       x_break = 28,
                       x_pad_end = as.Date("2023-12-01"),
                       extend_limits_to = as.Date("2023-12-01"),
                       override_y_lim = 60,
                       includeAnnotations = FALSE)

data1 <- plot_auto_SPC(df = head(G513H_data, 21),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = FALSE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    extend_limits_to = as.Date("2018-06-01"),
                    override_y_lim = 300,
                    includeAnnotations = FALSE)

data2 <- plot_auto_SPC(df = head(G513H_data, 30),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = FALSE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    extend_limits_to = as.Date("2018-06-01"),
                    override_y_lim = 300,
                    includeAnnotations = FALSE)


data3 <- plot_auto_SPC(df = head(G513H_data, 54),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = FALSE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    extend_limits_to = as.Date("2020-06-01"),
                    noRecals = TRUE,
                    override_y_lim = 300,
                    includeAnnotations = FALSE)

data4 <- plot_auto_SPC(df = head(G513H_data, 54),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = FALSE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    extend_limits_to = as.Date("2022-01-01"),
                    noRecals = FALSE,
                    override_y_lim = 300,
                    includeAnnotations = FALSE)


data5 <- plot_auto_SPC(df = head(G513H_data, 74),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = FALSE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    extend_limits_to = as.Date("2022-01-01"),
                    noRecals = FALSE,
                    override_y_lim = 300,
                    includeAnnotations = FALSE)

data6 <- plot_auto_SPC(df = head(G513H_data, 97),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 0,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = FALSE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    noRecals = FALSE,
                    override_y_lim = 300,
                    includeAnnotations = FALSE,
                    development_recalc_at_every_break = TRUE)


data7 <- plot_auto_SPC(df = head(G513H_data, 97),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = hospital_name,
                    plotChart = FALSE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    noRecals = FALSE,
                    override_y_lim = 300,
                    includeAnnotations = FALSE)

data_algorithm <- plot_auto_SPC(df = G513H_data,
                             periodMin = 21,
                             runRuleLength = 8,
                             maxNoOfExclusions = 3,
                             highlightExclusions = TRUE,
                             title = "Average daily A&E attendances per month",
                             subtitle = hospital_name,
                             plotChart = FALSE,
                             writeTable = FALSE,
                             noRegrets = TRUE,
                             chartType = "C'",
                             x = Month_Start,
                             y = daily_ave_att,
                             x_break = 210,
                             x_pad_end = as.Date("2023-03-01"),
                             override_y_lim = 300)

data_naive1 <- plot_auto_SPC(df = G513H_data,
                          periodMin = nrow(G513H_data),
                          runRuleLength = 8,
                          maxNoOfExclusions = 3,
                          highlightExclusions = TRUE,
                          title = "Average daily A&E attendances per month",
                          subtitle = paste0(hospital_name, "\nNaive Approach 1"),
                          plotChart = FALSE,
                          writeTable = FALSE,
                          noRegrets = TRUE,
                          chartType = "C'",
                          x = Month_Start,
                          y = daily_ave_att,
                          x_break = 210,
                          x_pad_end = as.Date("2023-03-01"),
                          noRecals = FALSE,
                          override_y_lim = 300,
                          includeAnnotations = FALSE)


data_naive2 <- plot_auto_SPC(df = G513H_data,
                          periodMin = 21,
                          runRuleLength = 8,
                          maxNoOfExclusions = 3,
                          highlightExclusions = TRUE,
                          title = "Average daily A&E attendances per month",
                          subtitle = paste0(hospital_name, "\nNaive Approach 2"),
                          plotChart = FALSE,
                          writeTable = FALSE,
                          noRegrets = TRUE,
                          chartType = "C'",
                          x = Month_Start,
                          y = daily_ave_att,
                          x_break = 210,
                          x_pad_end = as.Date("2023-03-01"),
                          noRecals = TRUE,
                          override_y_lim = 300,
                          includeAnnotations = FALSE)


data_naive3 <- plot_auto_SPC(df = G513H_data,
                          periodMin = 21,
                          runRuleLength = 8,
                          maxNoOfExclusions = 3,
                          highlightExclusions = TRUE,
                          title = "Average daily A&E attendances per month",
                          subtitle = paste0(hospital_name, "\nNaive Approach 2"),
                          plotChart = FALSE,
                          writeTable = FALSE,
                          noRegrets = TRUE,
                          chartType = "C'",
                          x = Month_Start,
                          y = daily_ave_att,
                          x_break = 210,
                          x_pad_end = as.Date("2023-03-01"),
                          noRecals = FALSE,
                          override_y_lim = 300,
                          includeAnnotations = FALSE,
                          development_recalc_at_every_break = TRUE)



################################################################################
plot_figure_1 <- function(){
  
  panel_captions <- c("(a) Control limits formed using baseline data (calculation period), and extended (display period)",
                      "(b) Additional data added to chart",
                      "(c) Further data added to chart")
  
  data1 <- dataA %>%
    mutate(step = panel_captions[1])
  
  data2 <- dataB %>%
    mutate(step = panel_captions[2])
  
  data3 <- dataC %>%
    mutate(step = panel_captions[3])
  
  data <- bind_rows(data1, data2, data3)
  
  data$step <- factor(data$step, levels = c(panel_captions[1],
                                            panel_captions[2],
                                            panel_captions[3]))
  
  strip <- strip_themed(background_x = elem_list_rect(fill = c("#CACC90", "#CACC90", "#CACC90")))
  
  plot <-  ggplot2::ggplot(data, 
                           ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(colour = "black",
                       linewidth = 0.5) +
    ggplot2::geom_point(colour = "black", size = 2) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = "#CACC9080"),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1,
                                                       vjust = 1.0,
                                                       size = 14),
                   axis.text.y = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 14),
                   plot.title = ggplot2::element_text(size = 20, hjust = 0),
                   plot.subtitle = ggplot2::element_text(size = 16, face = "italic"),
                   axis.line = ggplot2::element_line(colour = "#CACC9060"),
                   plot.caption = ggplot2::element_text(size = 10, hjust = 0.5)) +
    ggplot2::ggtitle("Example Control Chart (C-Chart)", subtitle = "Simulated A&E Attendance Data") +
    ggplot2::labs(x = "Week",
                  y = "Number of attendances",
                  size = 10) +
    ggplot2::scale_y_continuous(limits = c(150, 250),
      breaks = scales::breaks_pretty(),
      labels = scales::number_format(accuracy = 1,
                                     big.mark = ",")) +
    ggplot2::scale_x_date(
      breaks = scales::date_breaks(width = "4 weeks")) +
    facet_wrap2(vars(step),
                strip = strip,
                ncol = 1L)
  
  plot <- autospc:::format_SPC(cht = plot, 
                               df = data, 
                               r1_col = "orange", 
                               r2_col = "steelblue3", ymin, ymax)
  plot
  
}


################################################################################
plot_steps_facet <- function(all_steps = TRUE){
  
  panel_captions <- c("(a) Baseline established and display limits extended",
                      "(b) Shift rule (rule 2) break identified",
                      "(c) Control limits are recalculated",
                      "(d) Shift rule (rule 2) break identified",
                      "(e) Candidate limits: shift back towards original",
                      "(f) Candidate limits rejected")
  
  data1 <- data2 %>%
    mutate(step = panel_captions[1])
  
  data2 <- data3 %>%
    mutate(step = panel_captions[2])
  
  data3 <- data4 %>%
    mutate(step = panel_captions[3])
  
  data4 <- data5 %>%
    mutate(step = panel_captions[4])
  
  data5 <- data6 %>%
    mutate(step = panel_captions[5])
  
  data6 <- data7 %>%
    mutate(step = panel_captions[6])
  
  data <- bind_rows(data1, data2, data3, data4)
  
  plot_title <- "Re-establishing control limits after a shift"
  
  if(all_steps) {
    data <- bind_rows(data, data5, data6)
    plot_title <- "Algorithm steps"
  }
  
  data$step <- factor(data$step, levels = panel_captions)
  
  strip <- strip_themed(background_x = elem_list_rect(fill = c("#CACC90", "#F4EBBE", "#CACC90",
                                                               "#CACC90", "#F4EBBE", "#CACC90")))
  
  plot <-  ggplot2::ggplot(data, 
                           ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(colour = "black",
                       linewidth = 0.5) +
    ggplot2::geom_point(colour = "black", size = 2) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = "#CACC9080"),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1.0, size = 14),
                   axis.text.y = ggplot2::element_text(size = 14), axis.title = ggplot2::element_text(size = 14),
                   plot.title = ggplot2::element_text(size = 20, hjust = 0),
                   plot.subtitle = ggplot2::element_text(size = 16, face = "italic"),
                   axis.line = ggplot2::element_line(colour = "#CACC9060"),
                   plot.caption = ggplot2::element_text(size = 10, hjust = 0.5)) +
    ggplot2::ggtitle(plot_title,
                     subtitle = hospital_name) +
    ggplot2::labs(x = "Month",
                  y = "Average daily attendances per month",
                  size = 10) +
    ggplot2::scale_y_continuous(#limits = c(ylimlow, ylimhigh),
                                breaks = scales::breaks_pretty(),
                                labels = scales::number_format(accuracy = 1,
                                                               big.mark = ",")) +
    facet_wrap2(vars(step),
                strip = strip,
                ncol = 2L)#+
    #theme(strip.background =element_rect(fill=c("red", "green", "orange", "yellow", "blue", "purple")))
  
  plot <- autospc:::format_SPC(cht = plot, 
                     df = data, 
                     r1_col = "orange", 
                     r2_col = "steelblue3", ymin, ymax)
  plot
  
}

################################################################################
plot_approaches_facet <- function(){
  
  panel_captions <- c("(a) Algorithm",
                      "(b) Whole period as calculation period",
                      "(c) Baseline calculation period extended to end",
                      "(d) Recalculation at every shift rule (rule 2) break")
  
  data_algorithm <- data_algorithm %>%
    mutate(step = panel_captions[1])
  
  data_naive1 <- data_naive1 %>%
    mutate(step = panel_captions[2])
  
  data_naive2 <- data_naive2 %>%
    mutate(step = panel_captions[3])
  
  data_naive3 <- data_naive3 %>%
    mutate(step = panel_captions[4])
  
  data <- bind_rows(data_algorithm, data_naive1, data_naive2, data_naive3)
  
  data$step <- factor(data$step, levels = panel_captions)
  
  strip <- strip_themed(background_x = elem_list_rect(fill = c("#CACC90", "#F4EBBE", "#F4EBBE",
                                                               "#F4EBBE")))
  plot <-  ggplot2::ggplot(data, 
                           ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(colour = "black",
                       linewidth = 0.5) +
    ggplot2::geom_point(colour = "black", size = 2) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = "#CACC9080"),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1.0, size = 14),
                   axis.text.y = ggplot2::element_text(size = 14), axis.title = ggplot2::element_text(size = 14),
                   plot.title = ggplot2::element_text(size = 20, hjust = 0),
                   plot.subtitle = ggplot2::element_text(size = 16, face = "italic"),
                   axis.line = ggplot2::element_line(colour = "#CACC9060"),
                   plot.caption = ggplot2::element_text(size = 10, hjust = 0.5)) +
    ggplot2::ggtitle("Approaches to Partitioning Control Chart Limits",
                     subtitle = hospital_name) +
    ggplot2::labs(x = "Month",
                  y = "Average daily attendances per month",
                  size = 10) +
    ggplot2::scale_y_continuous(#limits = c(ylimlow, ylimhigh),
      breaks = scales::breaks_pretty(),
      labels = scales::number_format(accuracy = 1,
                                     big.mark = ",")) +
    facet_wrap2(vars(step), strip = strip)
  
  plot <- autospc:::format_SPC(cht = plot, 
                     df = data, 
                     r1_col = "orange", 
                     r2_col = "steelblue3", ymin, ymax)
  plot
  
}

p_fig1 <- plot_figure_1()
p_fig2 <- plot_steps_facet(all_steps = FALSE)
p_steps <- plot_steps_facet()
p_approaches <- plot_approaches_facet()

if(save_plots) {
  
  ggsave(file.path("plots", "pA.png"), plot = pA)
  ggsave(file.path("plots", "pB.png"), plot = pB)
  ggsave(file.path("plots", "pC.png"), plot = pC)
  ggsave(file.path("plots", "p1.png"), plot = p1)
  ggsave(file.path("plots", "p2.png"), plot = p2)
  ggsave(file.path("plots", "p3.png"), plot = p3)
  ggsave(file.path("plots", "p4.png"), plot = p4)
  ggsave(file.path("plots", "p5.png"), plot = p5)
  ggsave(file.path("plots", "p6.png"), plot = p6)
  ggsave(file.path("plots", "p7.png"), plot = p7)
  ggsave(file.path("plots", "p_naive1.png"), plot = p_naive1)
  ggsave(file.path("plots", "p_naive2.png"), plot = p_naive2)
  ggsave(file.path("plots", "p_naive3.png"), plot = p_naive3)
  ggsave(file.path("plots", "p_algorithm.png"), plot = p_algorithm)
  ggsave(file.path("plots", "p_fig1.png"), plot = p_fig1,
         width = 10, height = 10, units = "in")
  ggsave(file.path("plots", "p_fig2.png"), plot = p_fig2,
         width = 10, height = 7, units = "in")
  ggsave(file.path("plots", "p_steps_fig5.png"), plot = p_steps,
         width = 12, height = 7, units = "in")
  ggsave(file.path("plots", "p_approaches_fig3.png"), plot = p_approaches,
         width = 10, height = 7, units = "in")
  
}
