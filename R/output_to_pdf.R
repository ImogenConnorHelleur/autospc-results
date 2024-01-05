###############################################################################
require(lattice)
require(magrittr)

source(file.path("R", "results_functions.R"))
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

for(c in codes$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_volume_from_perf_series_df(Code_arg = c, pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df(Code_arg = c, pseudo_code = pc))
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

for(c in codes_p$Code) {
  pc <- get_pseudo_code(c, code_mapping = code_mapping)
  print(pc)
  tryCatch({
    print(plot_performance_from_perf_series_df(Code_arg = c, pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(Code_arg = c, pseudo_code = pc))
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
    print(plot_volume_from_perf_series_df_naive1(Code_arg = c, pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df_naive1(Code_arg = c, pseudo_code = pc))
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
    print(plot_performance_from_perf_series_df_naive1(Code_arg = c, pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df_naive1(Code_arg = c, pseudo_code = pc))
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
    print(plot_volume_from_perf_series_df(Code_arg = c, noRecals = TRUE, pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df(Code_arg = c, noRecals = TRUE, pseudo_code = pc))
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
    print(plot_performance_from_perf_series_df(Code_arg = c, noRecals = TRUE, pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(Code_arg = c, noRecals = TRUE, pseudo_code = pc))
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
    print(plot_volume_from_perf_series_df(Code_arg = c, development_recalc_at_every_break = TRUE, pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df(Code_arg = c, development_recalc_at_every_break = TRUE, pseudo_code = pc))
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
    print(plot_performance_from_perf_series_df(Code_arg = c, development_recalc_at_every_break = TRUE, pseudo_code = pc))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(Code_arg = c, development_recalc_at_every_break = TRUE, pseudo_code = pc))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", pc))
  })
  
}
dev.off()
