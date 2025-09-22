###############################################################################
require(tidyverse)
require(lattice)
require(magrittr)
require(autospc)

source(file.path("R", "results_functions.R"))

c_algorithm_title = paste("\nSupplementary Appendix A: Number of Attendances\n",
                          " - Stable Shift Algorithm")

c_algorithm_tx = paste("\n",
                       "This document contains C-prime charts for time series",
                       "of number of attendances at each Accident and\n",
                       "Emergency department in England and Scotland,",
                       "excluding those with insufficient data. Centre lines\n",
                       "and control limits were re-established using the",
                       "Stable Shift Algorithm, as described in the main",
                       "article.")

p_algorithm_title = paste("\nSupplementary Appendix B: Four-Hour Performance\n",
                          " - Stable Shift Algorithm")

p_algorithm_tx = paste("\n",
                       "This document contains P-prime charts for time series",
                       "of four-hour performance at each Accident and\n",
                       "Emergency department in England and Scotland,",
                       "excluding those with insufficient data. Centre lines\n",
                       "and control limits were re-established using the",
                       "Stable Shift Algorithm, as described in the main\n",
                       "article. Note that since p-prime limits depend upon",
                       "the size of the denominator, the width of control\n",
                       "limits on these charts varies with the number of",
                       "attendances")


perf_series_df <- readRDS(file.path("data", "perf_series_df_clean.rds"))

codes <- dplyr::distinct(perf_series_df, Code)
n_codes <- nrow(codes)

set.seed(1862315L)
code_mapping <- tibble::tibble(Code = codes$Code,
                               pseudo_code = sample(1:n_codes,
                                                    replace=FALSE)
                               )

codes <- code_mapping %>% 
  arrange(pseudo_code) %>% 
  select(Code)

#codes <- head(codes,3)

get_pseudo_code <- function(c, code_mapping) {
  pc <- code_mapping %>% 
    dplyr::filter(Code == c) %>% 
    dplyr::pull(pseudo_code)

  if(length(pc) != 1L) {stop("Code existence or uniqueness error")}
  
  return(pc)
}

pdf(paste("C_algorithm", ".pdf", sep = ""),
    width = 10,
    height = 5)

ggplot() + 
  annotate("text",
           x = c(1,0), 
           y = c(28,19),
           size=c(8,5),
           hjust = c(0, 0),
           label = c(c_algorithm_title,
                     c_algorithm_tx)) + 
  xlim(0,10) +
  ylim(0,30) +
  theme_void()

for(c in codes$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Monthly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 24,
                                          pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Monthly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 24,
                                          pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

for(c in codes$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Weekly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 21,
                                          pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Weekly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 21,
                                          pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

dev.off()


###############################################################################
require(lattice)
codes_p <- perf_series_df %>% 
  dplyr::filter(mid_range_ok) %>% 
  dplyr::distinct(Code) 

#codes_p <- head(codes_p,3)

codes_p <- codes_p %>%
  left_join(code_mapping,
            by = c("Code" = "Code")) %>% 
  arrange(pseudo_code) %>% 
  select(Code)


pdf(paste("P_algorithm", ".pdf", sep = ""),
    width = 10,
    height = 5)

ggplot() + 
  annotate("text",
           x = c(1,0), 
           y = c(28,19),
           size=c(8,5),
           hjust = c(0, 0),
           label = c(p_algorithm_title,
                     p_algorithm_tx)) + 
  xlim(0,10) +
  ylim(0,30) +
  theme_void()


for(c in codes_p$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Monthly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 24,
                                               pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Monthly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 24,
                                               pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

for(c in codes_p$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Weekly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 21,
                                               pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Weekly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 21,
                                               pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

dev.off()

###############################################################################

pdf(paste("C_naive1", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_volume_from_perf_series_df_naive1(data = perf_series_df,
                                                 Code_arg = c,
                                                 weeklyOrMonthly_arg = "Monthly",
                                                 measure_arg = "All",
                                                 onlyProvsReporting_arg = TRUE,
                                                 periodMin = 24,
                                                 pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df_naive1(data = perf_series_df,
                                                 Code_arg = c,
                                                 weeklyOrMonthly_arg = "Monthly",
                                                 measure_arg = "All",
                                                 onlyProvsReporting_arg = TRUE,
                                                 periodMin = 24,
                                                 pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

for(c in codes$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_volume_from_perf_series_df_naive1(data = perf_series_df,
                                                 Code_arg = c,
                                                 weeklyOrMonthly_arg = "Weekly",
                                                 measure_arg = "All",
                                                 onlyProvsReporting_arg = TRUE,
                                                 periodMin = 21,
                                                 pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df_naive1(data = perf_series_df,
                                                 Code_arg = c,
                                                 weeklyOrMonthly_arg = "Weekly",
                                                 measure_arg = "All",
                                                 onlyProvsReporting_arg = TRUE,
                                                 periodMin = 21,
                                                 pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}
dev.off()


###############################################################################

pdf(paste("P_naive1", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes_p$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_performance_from_perf_series_df_naive1(data = perf_series_df,
                                                      Code_arg = c,
                                                      weeklyOrMonthly_arg = "Monthly",
                                                      measure_arg = "All",
                                                      onlyProvsReporting_arg = TRUE,
                                                      periodMin = 24,
                                                      pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df_naive1(data = perf_series_df,
                                                      Code_arg = c,
                                                      weeklyOrMonthly_arg = "Monthly",
                                                      measure_arg = "All",
                                                      onlyProvsReporting_arg = TRUE,
                                                      periodMin = 24,
                                                      pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

for(c in codes_p$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_performance_from_perf_series_df_naive1(data = perf_series_df,
                                                      Code_arg = c,
                                                      weeklyOrMonthly_arg = "Weekly",
                                                      measure_arg = "All",
                                                      onlyProvsReporting_arg = TRUE,
                                                      periodMin = 21,
                                                      pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df_naive1(data = perf_series_df,
                                                      Code_arg = c,
                                                      weeklyOrMonthly_arg = "Weekly",
                                                      measure_arg = "All",
                                                      onlyProvsReporting_arg = TRUE,
                                                      periodMin = 21,
                                                      pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

dev.off()


###############################################################################

pdf(paste("C_naive2", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Monthly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 24,
                                          noRecals = TRUE,
                                          pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Monthly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 24,
                                          noRecals = TRUE,
                                          pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

for(c in codes$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Weekly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 21,
                                          noRecals = TRUE,
                                          pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Weekly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 21,
                                          noRecals = TRUE,
                                          pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}
dev.off()


###############################################################################

pdf(paste("P_naive2", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes_p$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Monthly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 24,
                                               noRecals = TRUE,
                                               pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Monthly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 24,
                                               noRecals = TRUE,
                                               pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}
for(c in codes_p$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Weekly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 21,
                                               noRecals = TRUE,
                                               pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Weekly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 21,
                                               noRecals = TRUE,
                                               pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

dev.off()


###############################################################################

pdf(paste("C_naive3", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Monthly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 24,
                                          recalEveryShift = TRUE,
                                          pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Monthly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 24,
                                          recalEveryShift = TRUE,
                                          pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

for(c in codes$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Weekly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 21,
                                          recalEveryShift = TRUE,
                                          pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Weekly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 21,
                                          recalEveryShift = TRUE,
                                          pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}
dev.off()


###############################################################################
require(lattice)

pdf(paste("P_naive3", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes_p$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Monthly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 24,
                                               recalEveryShift = TRUE,
                                               pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Monthly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 24,
                                               recalEveryShift = TRUE,
                                               pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

for(c in codes_p$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Weekly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 21,
                                               recalEveryShift = TRUE,
                                               pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Weekly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 21,
                                               recalEveryShift = TRUE,
                                               pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

dev.off()

###############################################################################

###############################################################################

pdf(paste("C_naive3b", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Monthly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 8,
                                          baseline = 24,
                                          recalEveryShift = TRUE,
                                          noPeriodMin = FALSE,
                                          pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Monthly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 8,
                                          baseline = 24,
                                          recalEveryShift = TRUE,
                                          noPeriodMin = FALSE,
                                          pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

for(c in codes$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Weekly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 8,
                                          baseline = 21,
                                          recalEveryShift = TRUE,
                                          noPeriodMin = FALSE,
                                          pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df(data = perf_series_df,
                                          Code_arg = c,
                                          weeklyOrMonthly_arg = "Weekly",
                                          measure_arg = "All",
                                          onlyProvsReporting_arg = TRUE,
                                          periodMin = 8,
                                          baseline = 21,
                                          recalEveryShift = TRUE,
                                          noPeriodMin = FALSE,
                                          pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}
dev.off()


###############################################################################


###############################################################################
require(lattice)

pdf(paste("P_naive3b", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes_p$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Monthly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 8,
                                               baseline = 24,
                                               recalEveryShift = TRUE,
                                               noPeriodMin = FALSE,
                                               pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Monthly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 8,
                                               baseline = 24,
                                               recalEveryShift = TRUE,
                                               noPeriodMin = FALSE,
                                               pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

for(c in codes_p$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Weekly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 8,
                                               baseline = 21,
                                               recalEveryShift = TRUE,
                                               noPeriodMin = FALSE,
                                               pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(data = perf_series_df,
                                               Code_arg = c,
                                               weeklyOrMonthly_arg = "Weekly",
                                               measure_arg = "All",
                                               onlyProvsReporting_arg = TRUE,
                                               periodMin = 8,
                                               baseline = 21,
                                               recalEveryShift = TRUE,
                                               noPeriodMin = FALSE,
                                               pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}

dev.off()

###############################################################################



remove_pdf_pages_with_no_text <- function(pdf_path) {
  doc_txt <- pdftools::pdf_text(pdf = pdf_path)
  
  pages_with_no_text <- which(doc_txt == "")
  
  if(length(pages_with_no_text) == 0) {
    return("All pages have text, none removed.")
  }
  
  staplr::remove_pages(rmpages = pages_with_no_text,
                       input_filepath = pdf_path,
                       output_filepath = pdf_path,
                       overwrite = TRUE)
  
  return(paste(pages_with_no_text, collapse = ", "))
}

fileNames <- Sys.glob('*.pdf')
lapply(fileNames,
       remove_pdf_pages_with_no_text)

###############################################################################






