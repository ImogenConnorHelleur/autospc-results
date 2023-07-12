library(tidyverse)
library(autospc)
library(ggh4x)

perf_series_df <- readRDS("data/perf_series_df.rds")

G513H_data <- filter(perf_series_df, Code == "G513H", weeklyOrMonthly == "Monthly")
G513H_data_weekly <- filter(perf_series_df, Code == "G513H", weeklyOrMonthly == "Weekly")



p1 <- plot_auto_SPC(df = head(G513H_data, 21),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = "Royal Hospital For Children Glasgow",
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
                    subtitle = "Royal Hospital For Children Glasgow",
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


p3 <- plot_auto_SPC(df = head(G513H_data, 54),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = "Royal Hospital For Children Glasgow",
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
                    subtitle = "Royal Hospital For Children Glasgow",
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
                    subtitle = "Royal Hospital For Children Glasgow",
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
                    subtitle = "Royal Hospital For Children Glasgow",
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
                    subtitle = "Royal Hospital For Children Glasgow",
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
              subtitle = "Royal Hospital For Children Glasgow",
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
                          subtitle = "Royal Hospital For Children Glasgow \nNaive Approach 1",
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
                          subtitle = "Royal Hospital For Children Glasgow \nNaive Approach 2",
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
                          subtitle = "Royal Hospital For Children Glasgow \nNaive Approach 3",
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



data1 <- plot_auto_SPC(df = head(G513H_data, 21),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = "Royal Hospital For Children Glasgow",
                    plotChart = FALSE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    override_y_lim = 300,
                    includeAnnotations = FALSE)

data2 <- plot_auto_SPC(df = head(G513H_data, 30),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = "Royal Hospital For Children Glasgow",
                    plotChart = FALSE,
                    writeTable = FALSE,
                    noRegrets = TRUE,
                    chartType = "C'",
                    x = Month_Start,
                    y = daily_ave_att,
                    x_break = 210,
                    x_pad_end = as.Date("2023-03-01"),
                    override_y_lim = 300,
                    includeAnnotations = FALSE)


data3 <- plot_auto_SPC(df = head(G513H_data, 54),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = "Royal Hospital For Children Glasgow",
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

data4 <- plot_auto_SPC(df = head(G513H_data, 54),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = "Royal Hospital For Children Glasgow",
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


data5 <- plot_auto_SPC(df = head(G513H_data, 74),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 3,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = "Royal Hospital For Children Glasgow",
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

data6 <- plot_auto_SPC(df = head(G513H_data, 97),
                    periodMin = 21,
                    runRuleLength = 8,
                    maxNoOfExclusions = 0,
                    highlightExclusions = TRUE,
                    title = "Average daily A&E attendances per month",
                    subtitle = "Royal Hospital For Children Glasgow",
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
                    subtitle = "Royal Hospital For Children Glasgow",
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
                             subtitle = "Royal Hospital For Children Glasgow",
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
                          subtitle = "Royal Hospital For Children Glasgow \nNaive Approach 1",
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
                          subtitle = "Royal Hospital For Children Glasgow \nNaive Approach 2",
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
                          subtitle = "Royal Hospital For Children Glasgow \nNaive Approach 2",
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
plot_steps_facet <- function(){
  
  data1 <- data2 %>%
    mutate(step = "Baseline established and display limits extended")
  
  data2 <- data3 %>%
    mutate(step = "Rule 2 break identified*")
  
  data3 <- data4 %>%
    mutate(step = "Control limits are recalculated")
  
  data4 <- data5 %>%
    mutate(step = "Next rule 2 break identified")
  
  data5 <- data6 %>%
    mutate(step = "Candidate limits show rule 2 break back to original*")
  
  data6 <- data7 %>%
    mutate(step = "Candidate limits rejected")
  
  data <- bind_rows(data1, data2, data3, data4, data5, data6)
  
  data$step <- factor(data$step, levels = c("Baseline established and display limits extended",
                                            "Rule 2 break identified*",
                                            "Control limits are recalculated",
                                            "Next rule 2 break identified",
                                            "Candidate limits show rule 2 break back to original*",
                                            "Candidate limits rejected"))
  
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
    ggplot2::ggtitle("Algorithm steps", subtitle = "Royal Hospital For Children Glasgow") +
    ggplot2::labs(x = "Month",
                  y = "Average daily attendances per month",
                  caption = "*these visualisations of intermediate steps are not possible outputs from the algorithm. \nThey have just been shown here for illustration purposes.",
                  size = 10) +
    ggplot2::scale_y_continuous(#limits = c(ylimlow, ylimhigh),
                                breaks = scales::breaks_pretty(),
                                labels = scales::number_format(accuracy = 1,
                                                               big.mark = ",")) +
    facet_wrap2(vars(step), strip = strip)#+
    #theme(strip.background =element_rect(fill=c("red", "green", "orange", "yellow", "blue", "purple")))
  
  plot <- autospc::format_SPC(cht = plot, 
                     df = data, 
                     r1_col = "orange", 
                     r2_col = "steelblue3", ymin, ymax)
  plot
  
}

################################################################################
plot_approaches_facet <- function(){
  
  data_algorithm <- data_algorithm %>%
    mutate(step = "Algorithm")
  
  data_naive1 <- data_naive1 %>%
    mutate(step = "Naive 1: whole period as calculation period")
  
  data_naive2 <- data_naive2 %>%
    mutate(step = "Naive 2: first period extended to end")
  
  data_naive3 <- data_naive3 %>%
    mutate(step = "Naive 3: recalculation at every rule 2 break")
  
  data <- bind_rows(data_algorithm, data_naive1, data_naive2, data_naive3)
  
  data$step <- factor(data$step, levels = c("Algorithm",
                                            "Naive 1: whole period as calculation period",
                                            "Naive 2: first period extended to end",
                                            "Naive 3: recalculation at every rule 2 break"))
  
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
    ggplot2::ggtitle("Approaches", subtitle = "Royal Hospital For Children Glasgow") +
    ggplot2::labs(x = "Month",
                  y = "Average daily attendances per month",
                  size = 10) +
    ggplot2::scale_y_continuous(#limits = c(ylimlow, ylimhigh),
      breaks = scales::breaks_pretty(),
      labels = scales::number_format(accuracy = 1,
                                     big.mark = ",")) +
    facet_wrap2(vars(step), strip = strip)
  
  plot <- autospc::format_SPC(cht = plot, 
                     df = data, 
                     r1_col = "orange", 
                     r2_col = "steelblue3", ymin, ymax)
  plot
  
}