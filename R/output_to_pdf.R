###############################################################################
require(lattice)
require(magrittr)
codes <- dplyr::distinct(perf_series_df, Code)
#codes <- head(codes,3)

pdf(paste("C_algorithm", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes$Code) {
print(c)
    tryCatch({
      print(plot_volume_from_perf_series_df(Code_arg = c))
      err <- FALSE
    }, warning = function(w) {
      print(plot_volume_from_perf_series_df(Code_arg = c))
      err <- FALSE
    }, error = function(e) {
      err <- TRUE
      plot.new()
      text(x=.5, y=.5, paste("Too few points for", c))
    })

}
dev.off()


###############################################################################
require(lattice)
codes <- perf_series_df %>% 
  dplyr::filter(mid_range_ok) %>% 
  dplyr::distinct(Code) 
#codes <- head(codes,3)

pdf(paste("P_algorithm", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes$Code) {
  print(c)
  tryCatch({
    print(plot_performance_from_perf_series_df(Code_arg = c))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(Code_arg = c))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", c))
  })
  
}
dev.off()

###############################################################################
require(lattice)
codes <- dplyr::distinct(perf_series_df, Code)
#codes <- head(codes,3)

pdf(paste("C_naive1", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes$Code) {
  print(c)
  tryCatch({
    print(plot_volume_from_perf_series_df_naive1(Code_arg = c))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df_naive1(Code_arg = c))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", c))
  })
  
}
dev.off()


###############################################################################
require(lattice)
codes <- perf_series_df %>% 
  dplyr::filter(mid_range_ok) %>% 
  dplyr::distinct(Code) 
#codes <- head(codes,3)

pdf(paste("P_naive1", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes$Code) {
  print(c)
  tryCatch({
    print(plot_performance_from_perf_series_df_naive1(Code_arg = c))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df_naive1(Code_arg = c))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", c))
  })
  
}
dev.off()


###############################################################################
require(lattice)
codes <- dplyr::distinct(perf_series_df, Code)
#codes <- head(codes,3)

pdf(paste("C_naive2", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes$Code) {
  print(c)
  tryCatch({
    print(plot_volume_from_perf_series_df(Code_arg = c, noRecals = TRUE))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df(Code_arg = c, noRecals = TRUE))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", c))
  })
  
}
dev.off()


###############################################################################
require(lattice)
codes <- perf_series_df %>% 
  dplyr::filter(mid_range_ok) %>% 
  dplyr::distinct(Code) 
#codes <- head(codes,3)

pdf(paste("P_naive2", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes$Code) {
  print(c)
  tryCatch({
    print(plot_performance_from_perf_series_df(Code_arg = c, noRecals = TRUE))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(Code_arg = c, noRecals = TRUE))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", c))
  })
  
}
dev.off()


###############################################################################
require(lattice)
codes <- dplyr::distinct(perf_series_df, Code)
#codes <- head(codes,3)

pdf(paste("C_naive3", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes$Code) {
  print(c)
  tryCatch({
    print(plot_volume_from_perf_series_df(Code_arg = c, development_recalc_at_every_break = TRUE))
    err <- FALSE
  }, warning = function(w) {
    print(plot_volume_from_perf_series_df(Code_arg = c, development_recalc_at_every_break = TRUE))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", c))
  })
  
}
dev.off()


###############################################################################
require(lattice)
codes <- perf_series_df %>% 
  dplyr::filter(mid_range_ok) %>% 
  dplyr::distinct(Code) 
#codes <- head(codes,3)

pdf(paste("P_naive3", ".pdf", sep = ""),
    width = 10,
    height = 5)

for(c in codes$Code) {
  print(c)
  tryCatch({
    print(plot_performance_from_perf_series_df(Code_arg = c, development_recalc_at_every_break = TRUE))
    err <- FALSE
  }, warning = function(w) {
    print(plot_performance_from_perf_series_df(Code_arg = c, development_recalc_at_every_break = TRUE))
    err <- FALSE
  }, error = function(e) {
    err <- TRUE
    plot.new()
    text(x=.5, y=.5, paste("Too few points for", c))
  })
  
}
dev.off()
