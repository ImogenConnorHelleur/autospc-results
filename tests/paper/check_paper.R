# Check that this repository still reproduces the results in
#
#   Woodcock T, O'Connor I, Bell D. The Stable Shift Algorithm.
#   BMJ Qual Saf 2026;35:201-212. doi:10.1136/bmjqs-2025-019263
#
# The analysis scripts are run unchanged in a temporary copy of the repository,
# so nothing in the repository is overwritten. Their outputs are then compared
# with:
#   - the stored outputs in data/ and data/outputs/ (must be identical);
#   - every case study number in the text of the paper;
#   - the facts stated in the captions of figures 1 to 3 (Hospital A);
#   - the figure files in plots/ that went into the paper (pixel by pixel);
#   - the supplementary PDFs of charts, C_algorithm.pdf and P_algorithm.pdf;
#   - the plot data saved in tests/paper/reference/, if it exists.
#
# Usage, from the repository root (with the renv library active):
#
#   Rscript tests/paper/check_paper.R
#   Rscript tests/paper/check_paper.R --stages=numbers,figures
#   Rscript tests/paper/check_paper.R --update-reference
#
# Stages (all run by default; the full run takes a few hours):
#   raw      rebuild data/perf_series_df.rds from data/AE_Data*.RDS (slow)
#   results  run R/get_results.R (all approaches, monthly and weekly)
#   numbers  check the numbers in the paper text; uses the results stage's
#            output if it ran, otherwise the stored full_results_all.rds
#   fig6     render R/results_analysis.Rmd and compare Figure 6
#   figures  run R/paper_plots.R; compare figures 1-3 and the supplementary
#            figure, and check the facts in the captions of figures 1-3
#   pdfs     run R/output_to_pdf.R and compare the PDFs of charts
#
# --update-reference saves this run's plot data, and a record of its figures
# and how they differ from plots/, to tests/paper/reference/, for future runs
# to compare with. Use it only once the figures have been checked against the
# paper: all figure checks pass, or the images of the differences have been
# looked at and the differences accepted (for example, text drawn with
# slightly different fonts). The figure record is specific to the computer
# it was made on.
#
# Output: a report in tests/paper/output/<date-time>/ (report.md and
# report.csv), with logs of each stage and images showing any differences
# between figures. The script stops with an error if any check fails. A
# figure that differs from plots/ only in the way recorded as accepted in
# tests/paper/reference/ gives a warning, not an error.
#
# Not covered: figure 4 (flow chart, drawn by hand), figure 5 and online
# supplemental file 2 (simulations, not in this repository), supplemental
# file 3 (detailed flow chart), and supplemental file 1 apart from the
# simulated example chart in plots/p_supp_fig1_r.tiff. See README.md.

expected_autospc_version <- "0.0.0.9031"

# Settings ---------------------------------------------------------------------

all_stages <- c("raw", "results", "numbers", "fig6", "figures", "pdfs")
args <- commandArgs(trailingOnly = TRUE)
stage_arg <- grep("^--stages=", args, value = TRUE)
stages <- if (length(stage_arg) == 1) {
  strsplit(sub("^--stages=", "", stage_arg), ",")[[1]]
} else {
  all_stages
}
stopifnot(all(stages %in% all_stages))
update_reference <- "--update-reference" %in% args

repo <- normalizePath(getwd())
if (!file.exists(file.path(repo, "R", "get_results.R"))) {
  stop("Run this script from the root of the autospc-results repository.")
}
check_dir <- file.path(repo, "tests", "paper")
reference_dir <- file.path(check_dir, "reference")
out_dir <- file.path(check_dir, "output", format(Sys.time(), "%Y-%m-%d_%H%M%S"))
work <- file.path(out_dir, "work")
dir.create(work, recursive = TRUE)

suppressPackageStartupMessages(library(dplyr))

# Results of the checks --------------------------------------------------------

checks <- data.frame(section = character(), check = character(),
                     status = character(), detail = character())

record <- function(section, check, status, detail = "") {
  stopifnot(status %in% c("PASS", "WARN", "FAIL", "NOTE"))
  checks[nrow(checks) + 1, ] <<- list(section, check, status, detail)
  cat(sprintf("[%s] %s: %s%s\n", status, section, check,
              if (nzchar(detail)) paste0(" (", detail, ")") else ""))
  invisible(status)
}

# Record PASS if actual equals expected (to within tolerance), FAIL otherwise
check_value <- function(section, check, actual, expected, tolerance = 0) {
  ok <- length(actual) == length(expected) &&
    all(abs(actual - expected) <= tolerance + 1e-9)
  record(section, check, if (ok) "PASS" else "FAIL",
         sprintf("paper: %s; code: %s",
                 paste(expected, collapse = ", "),
                 paste(signif(actual, 6), collapse = ", ")))
}

check_true <- function(section, check, condition, detail = "") {
  record(section, check, if (isTRUE(condition)) "PASS" else "FAIL", detail)
}

# Compare an object with a stored .rds file
check_identical_rds <- function(section, new_object, stored_path) {
  stored <- readRDS(stored_path)
  name <- sub(paste0(repo, "/"), "", stored_path, fixed = TRUE)
  if (identical(new_object, stored)) {
    record(section, name, "PASS", "identical")
  } else {
    difference <- all.equal(stored, new_object)
    detail <- paste(head(difference, 3), collapse = "; ")
    if ("breakPoint" %in% names(stored) && "Code" %in% names(stored)) {
      changed <- full_join(
        stored %>% group_by(Code) %>%
          summarise(a = paste(which(breakPoint %in% TRUE), collapse = " ")),
        new_object %>% group_by(Code) %>%
          summarise(b = paste(which(breakPoint %in% TRUE), collapse = " ")),
        by = "Code") %>%
        filter(is.na(a) | is.na(b) | a != b)
      detail <- paste0(nrow(changed), " series with different re-establishments; ",
                       detail)
    }
    record(section, name, "FAIL", detail)
  }
}

# Running the analysis scripts -------------------------------------------------

# Run R code in a new R session, in the working copy, with this session's
# library paths. Output goes to a log file in the output folder.
run_in_copy <- function(stage, code) {
  script <- file.path(out_dir, paste0(stage, ".R"))
  log <- file.path(out_dir, paste0(stage, ".log"))
  writeLines(c(
    sprintf('stopifnot(as.character(packageVersion("autospc")) == "%s")',
            expected_autospc_version),
    code), script)
  cat(sprintf("Running stage '%s' (log: %s) ...\n", stage, log))
  started <- Sys.time()
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--no-init-file", shQuote(script)),
    stdout = log, stderr = log,
    env = paste0("R_LIBS=", paste(.libPaths(), collapse = .Platform$path.sep))
  )
  minutes <- round(as.numeric(difftime(Sys.time(), started, units = "mins")), 1)
  record("run", stage, if (status == 0) "PASS" else "FAIL",
         sprintf("%s minutes; exit status %s", minutes, status))
  status == 0
}

# Make the working copy: the scripts and the data, and an empty plots folder
for (d in c("R", "data")) {
  file.copy(file.path(repo, d), work, recursive = TRUE)
}
dir.create(file.path(work, "plots"))
old_wd <- setwd(work)

record("setup", "autospc version",
       if (as.character(packageVersion("autospc")) == expected_autospc_version)
         "PASS" else "FAIL",
       as.character(packageVersion("autospc")))
if (requireNamespace("renv", quietly = TRUE)) {
  in_sync <- tryCatch(
    isTRUE(renv::status(project = repo, dev = FALSE)$synchronized),
    error = function(e) NA)
  record("setup", "renv library matches renv.lock",
         if (isTRUE(in_sync)) "PASS" else "FAIL",
         if (is.na(in_sync)) "renv::status() failed" else "")
}
record("setup", "R version", "NOTE",
       paste0(R.version.string, "; renv.lock: ",
              jsonlite::fromJSON(file.path(repo, "renv.lock"))$R$Version))

# Stage raw: perf_series_df from the A&E data -----------------------------------

if ("raw" %in% stages) {
  ok <- run_in_copy("raw", c(
    'suppressPackageStartupMessages({library(tidyverse); library(autospc)})',
    'source("R/get_perf_series_df.R")',
    # As at the start of R/get_results.R
    'AE_Data <- readRDS("data/AE_Data.RDS")',
    'AE_Data_Scot <- readRDS("data/AE_Data_Scot.RDS")',
    'AE_Data <- AE_Data %>% mutate(Nat_Code = if_else(Nat_Code == "S", "Sc", Nat_Code))',
    'AE_Data_Scot <- AE_Data_Scot %>% mutate(Nat_Code = if_else(Nat_Code == "S", "Sc", Nat_Code))',
    'perf_series_df <- make_perf_series_data_frame(data = AE_Data, data_scot = AE_Data_Scot,',
    '  measureArg = "All", onlyProvsReportingArg = TRUE)',
    'saveRDS(perf_series_df, "data/perf_series_df_rebuilt.rds")'
  ))
  if (ok) {
    check_identical_rds("raw data",
                        readRDS(file.path(work, "data", "perf_series_df_rebuilt.rds")),
                        file.path(repo, "data", "perf_series_df.rds"))
  }
}

# Stage results: R/get_results.R ------------------------------------------------

results_ok <- FALSE
if ("results" %in% stages) {
  results_ok <- run_in_copy("results", 'source("R/get_results.R")')
  if (results_ok) {
    check_identical_rds("results",
                        readRDS(file.path(work, "data", "perf_series_df_clean.rds")),
                        file.path(repo, "data", "perf_series_df_clean.rds"))
    stored <- list.files(file.path(repo, "data", "outputs"), pattern = "\\.rds$",
                         recursive = TRUE)
    for (f in stored) {
      check_identical_rds("results",
                          readRDS(file.path(work, "data", "outputs", f)),
                          file.path(repo, "data", "outputs", f))
    }
  }
}

# Stage numbers: the numbers in the text of the paper ----------------------------

if ("numbers" %in% stages) {
  results_file <- if (results_ok) {
    file.path(work, "data", "outputs", "full_results", "full_results_all.rds")
  } else {
    file.path(repo, "data", "outputs", "full_results", "full_results_all.rds")
  }
  record("numbers", "source of results", "NOTE",
         if (results_ok) "this run of R/get_results.R" else "stored full_results_all.rds")

  # As in R/results_analysis.Rmd, which excludes naive1 and naive2
  res <- readRDS(results_file) %>%
    tidyr::separate_wider_delim(results_set, delim = " ",
                                names = c("chart_type", "approach")) %>%
    filter(approach %in% c("algorithm", "naive3", "naive3b")) %>%
    mutate(num_recalcs_by_points = num_recalcs / n)
  ssa <- filter(res, approach == "algorithm")

  # Median (lower quartile, upper quartile), as in the Rmd's summary tables
  mq <- function(wm, approach_name, column) {
    v <- res[[column]][res$weeklyOrMonthly == wm & res$approach == approach_name]
    unname(c(median(v), quantile(v, 0.25), quantile(v, 0.75)))
  }

  s <- "numbers"
  # Abstract and Results, 'Case study'
  check_value(s, "557 time series", nrow(ssa), 557)
  check_value(s, "465 monthly and 92 weekly",
              c(sum(ssa$weeklyOrMonthly == "Monthly"),
                sum(ssa$weeklyOrMonthly == "Weekly")), c(465, 92))
  check_value(s, "316 attendance volume series and 241 performance series",
              c(sum(ssa$chart_type == "C"), sum(ssa$chart_type == "P")),
              c(316, 241))
  check_true(s, "the same series for SSA, RSR and RSR8",
             all(table(res$approach) == 557) &&
               n_distinct(res %>% select(chart_type, Code, weeklyOrMonthly, n)) == 557)
  monthly_n <- ssa$n[ssa$weeklyOrMonthly == "Monthly"]
  check_value(s, "monthly length between 48 and 97",
              range(monthly_n), c(48, 97))
  check_value(s, "monthly length median 95, IQR 95-95",
              mq("Monthly", "algorithm", "n"), c(95, 95, 95))
  weekly_n <- ssa$n[ssa$weeklyOrMonthly == "Weekly"]
  check_true(s, "all weekly length 428, except two of length 316",
             sum(weekly_n == 428) == length(weekly_n) - 2 &&
               sum(weekly_n == 316) == 2,
             paste(names(table(weekly_n)), table(weekly_n), sep = ": ",
                   collapse = "; "))
  # Methods: series with n < 2 nmin excluded
  check_true(s, "no series shorter than 2 nmin (48 monthly, 42 weekly)",
             min(monthly_n) >= 48 && min(weekly_n) >= 42)

  # Results, re-establishments (figure 6b)
  check_value(s, "re-establishments, monthly, SSA: 1 (1-2)",
              mq("Monthly", "algorithm", "num_recalcs"), c(1, 1, 2))
  check_value(s, "re-establishments, monthly, RSR: 2 (1-2)",
              mq("Monthly", "naive3", "num_recalcs"), c(2, 1, 2))
  check_value(s, "re-establishments, weekly, SSA: 10 (8-11)",
              mq("Weekly", "algorithm", "num_recalcs"), c(10, 8, 11))
  check_value(s, "re-establishments, weekly, RSR: 13.5 (11-15)",
              mq("Weekly", "naive3", "num_recalcs"), c(13.5, 11, 15))
  for (wm in c("Monthly", "Weekly")) {
    check_true(s, paste0("RSR8 re-establishes most often (median), ", tolower(wm)),
               mq(wm, "naive3b", "num_recalcs")[1] >
                 max(mq(wm, "algorithm", "num_recalcs")[1],
                     mq(wm, "naive3", "num_recalcs")[1]))
    # 'The same pattern held ... relative to the length of each series (6c)'
    check_true(s, paste0("re-establishments per point lower for SSA than RSR (median), ",
                         tolower(wm)),
               mq(wm, "algorithm", "num_recalcs_by_points")[1] <=
                 mq(wm, "naive3", "num_recalcs_by_points")[1],
               sprintf("SSA %.4f, RSR %.4f",
                       mq(wm, "algorithm", "num_recalcs_by_points")[1],
                       mq(wm, "naive3", "num_recalcs_by_points")[1]))
  }

  # Results, shift rule breaks (figure 6d): 'median and IQRs similar'
  for (wm in c("Monthly", "Weekly")) {
    record(s, paste0("shift rule breaks, ", tolower(wm),
                     " (text: similar for SSA and RSR)"), "NOTE",
           sprintf("SSA %s; RSR %s",
                   paste(mq(wm, "algorithm", "num_rule2_breaks"), collapse = ", "),
                   paste(mq(wm, "naive3", "num_rule2_breaks"), collapse = ", ")))
  }

  # Results: 'a median of 36 points (25-46) outside the control limits using
  # SSA, and similarly 35 points (25-47) using RSR; the corresponding figures
  # for weekly series were 88.5 (51.8-129) compared with 72.5 (35.8-110.8)
  # (figure 6f)'. These are the numbers of points within shift rule breaks
  # (figure 6e); the text calls them points outside the control limits.
  check_value(s, "points quoted, monthly, SSA: 36 (25-46)",
              mq("Monthly", "algorithm", "num_rule2_points"), c(36, 25, 46))
  check_value(s, "points quoted, monthly, RSR: 35 (25-47)",
              mq("Monthly", "naive3", "num_rule2_points"), c(35, 25, 47))
  check_value(s, "points quoted, weekly, SSA: 88.5 (51.8-129)",
              mq("Weekly", "algorithm", "num_rule2_points"), c(88.5, 51.8, 129),
              tolerance = 0.05)
  check_value(s, "points quoted, weekly, RSR: 72.5 (35.8-110.8)",
              mq("Weekly", "naive3", "num_rule2_points"), c(72.5, 35.8, 110.8),
              tolerance = 0.05)
  record(s, "points quoted are points within shift rule breaks (figure 6e)", "NOTE",
         paste0("the text calls them points outside the control limits (figure 6f), ",
                "which are, monthly SSA ",
                paste(mq("Monthly", "algorithm", "num_rule1_points"), collapse = ", "),
                ", RSR ", paste(mq("Monthly", "naive3", "num_rule1_points"), collapse = ", "),
                "; weekly SSA ",
                paste(mq("Weekly", "algorithm", "num_rule1_points"), collapse = ", "),
                ", RSR ", paste(mq("Weekly", "naive3", "num_rule1_points"), collapse = ", ")))

  # Discussion: 'on average re-established control limits 10 times, compared
  # with 13.5 times ... over one-quarter of occasions'
  weekly_medians <- c(mq("Weekly", "algorithm", "num_recalcs")[1],
                      mq("Weekly", "naive3", "num_recalcs")[1])
  check_value(s, "weekly: 10 times compared with 13.5", weekly_medians, c(10, 13.5))
  check_true(s, "over one-quarter of occasions",
             (weekly_medians[2] - weekly_medians[1]) / weekly_medians[2] > 0.25,
             sprintf("%.1f%%", 100 * (weekly_medians[2] - weekly_medians[1]) /
                       weekly_medians[2]))
  record(s, "figure 6c caption", "NOTE",
         paste0("caption: re-establishments as a proportion of the maximum ",
                "possible; plotted: re-establishments per point (num_recalcs / n)"))
  record(s, "software versions in Methods", "NOTE",
         paste0("paper: R 4.4.1, ggplot2 3.5.1; this run: R ",
                getRversion(), ", ggplot2 ", packageVersion("ggplot2")))
  write.csv(res, file.path(out_dir, "per_series_results.csv"), row.names = FALSE)
}

# Comparing figures ---------------------------------------------------------------

# Read an uncompressed TIFF, as written by ggsave(), into a raw vector of pixel
# values. Returns NULL for a TIFF this simple reader cannot handle.
read_tiff <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  if (!identical(bytes[1:4], as.raw(c(0x49, 0x49, 0x2a, 0x00)))) return(NULL)
  u16 <- function(at) readBin(bytes[at + 1:2], "integer", size = 2, signed = FALSE,
                              endian = "little")
  u32 <- function(at) readBin(bytes[at + 1:4], "integer", size = 4, endian = "little")
  ifd <- u32(4)
  tags <- list()
  for (i in seq_len(u16(ifd))) {
    e <- ifd + 2 + 12 * (i - 1)
    tag <- u16(e); type <- u16(e + 2); count <- u32(e + 4)
    size <- c(1, 1, 2, 4)[match(type, c(1, 2, 3, 4))]
    if (is.na(size)) next
    at <- if (count * size <= 4) e + 8 else u32(e + 8)
    tags[[as.character(tag)]] <- vapply(
      seq_len(count) - 1,
      function(k) if (size == 2) u16(at + 2 * k) else if (size == 4) u32(at + 4 * k)
                  else as.integer(bytes[at + k + 1]),
      numeric(1))
  }
  if (!all(c("256", "257", "273", "279") %in% names(tags))) return(NULL)
  if (!is.null(tags[["259"]]) && tags[["259"]] != 1) return(NULL)  # compressed
  pixels <- unlist(Map(function(offset, n) bytes[offset + seq_len(n)],
                       tags[["273"]], tags[["279"]]))
  list(width = tags[["256"]], height = tags[["257"]],
       channels = if (is.null(tags[["277"]])) 1 else tags[["277"]],
       pixels = pixels)
}

# Compare a figure made in this run with the one in plots/ that went into the
# paper. If they differ, writes an image of the differences (differing pixels
# in red over a faded copy of the figure in plots/).
#
# Text is drawn with the computer's fonts, so a figure made on a different
# computer, or with different versions of the graphics packages, can differ
# slightly from plots/ even when everything plotted is the same. Once the
# differences have been looked at and accepted, --update-reference records
# them in tests/paper/reference/figures.csv, with a checksum of this run's
# figure. Later runs then pass if they make exactly that figure again, and if
# its difference from plots/ is the one accepted.
figure_reference_file <- file.path(reference_dir, "figures.csv")
figure_reference <- if (file.exists(figure_reference_file)) {
  read.csv(figure_reference_file, stringsAsFactors = FALSE)
} else {
  data.frame(file = character(), pixel_md5 = character(),
             percent_differing_from_plots = numeric(), accepted = character())
}
new_figure_reference <- figure_reference

pixel_md5 <- function(pixels) {
  tmp <- tempfile()
  writeBin(pixels, tmp)
  on.exit(unlink(tmp))
  unname(tools::md5sum(tmp))
}

compare_figure <- function(label, file) {
  new_path <- file.path(work, "plots", file)
  old_path <- file.path(repo, "plots", file)
  check <- paste0(label, " (plots/", file, ")")
  if (!file.exists(old_path)) {
    return(record("figures", check, "FAIL", "plots/ has no such file to compare with"))
  }
  if (!file.exists(new_path)) {
    return(record("figures", check, "FAIL", "not made by this run"))
  }
  old <- read_tiff(old_path); new <- read_tiff(new_path)
  if (is.null(old) || is.null(new)) {
    same <- identical(readBin(old_path, "raw", file.size(old_path)),
                      readBin(new_path, "raw", file.size(new_path)))
    return(record("figures", check, if (same) "PASS" else "FAIL",
                  "compared as files; TIFF not readable here"))
  }
  if (old$width != new$width || old$height != new$height ||
      old$channels != new$channels) {
    return(record("figures", check, "FAIL",
                  sprintf("size %dx%d (%d channels) in plots/, %dx%d (%d) now",
                          old$width, old$height, old$channels,
                          new$width, new$height, new$channels)))
  }
  md5 <- pixel_md5(new$pixels)
  if (identical(old$pixels, new$pixels)) {
    percent <- 0
  } else {
    ch <- old$channels
    a <- matrix(as.integer(old$pixels), nrow = ch)
    b <- matrix(as.integer(new$pixels), nrow = ch)
    differs <- colSums(a != b) > 0
    percent <- round(100 * mean(differs), 3)
    grey <- colMeans(a[seq_len(min(ch, 3)), , drop = FALSE]) / 255
    if (ch == 4) grey <- 1 - (1 - grey) * a[4, ] / 255  # transparent shows white
    faded <- 1 - (1 - grey) * 0.3
    red <- ifelse(differs, 1, faded)
    green_blue <- ifelse(differs, 0, faded)
    img <- array(c(matrix(red, nrow = old$height, byrow = TRUE),
                   matrix(green_blue, nrow = old$height, byrow = TRUE),
                   matrix(green_blue, nrow = old$height, byrow = TRUE)),
                 dim = c(old$height, old$width, 3))
    diff_file <- file.path(out_dir, paste0("differences_", sub("\\.tiff$", ".png", file)))
    ragg::agg_png(diff_file, width = old$width, height = old$height)
    grid::grid.raster(img)
    grDevices::dev.off()
  }
  detail <- if (percent == 0) "pixel for pixel identical" else
    sprintf("%.3f%% of pixels differ; see differences_%s", percent,
            sub("\\.tiff$", ".png", file))
  ref <- figure_reference[figure_reference$file == file, ]
  if (update_reference) {
    new_figure_reference <<- rbind(
      new_figure_reference[new_figure_reference$file != file, ],
      data.frame(file = file, pixel_md5 = md5,
                 percent_differing_from_plots = percent,
                 accepted = format(Sys.Date())))
    record("figures", check, if (percent == 0) "PASS" else "WARN",
           paste0(detail, "; recorded as accepted in tests/paper/reference"))
  } else if (percent == 0) {
    record("figures", check, "PASS", detail)
  } else if (nrow(ref) == 1 && ref$pixel_md5 == md5 &&
             abs(ref$percent_differing_from_plots - percent) < 1e-6) {
    # Differs from plots/, but in the way accepted: a warning, not a failure
    record("figures", check, "WARN",
           paste0(detail, "; same figure as accepted on ", ref$accepted))
  } else {
    record("figures", check, "FAIL",
           paste0(detail, if (nrow(ref) == 1)
             paste0("; not the figure accepted on ", ref$accepted) else ""))
  }
}

check_plot_data <- function(name, current) {
  saveRDS(current, file.path(out_dir, paste0("plot_data_", name, ".rds")))
  ref <- file.path(reference_dir, paste0("plot_data_", name, ".rds"))
  if (update_reference) {
    dir.create(reference_dir, showWarnings = FALSE, recursive = TRUE)
    saveRDS(current, ref)
    record("plot data", name, "NOTE", "reference updated")
  } else if (file.exists(ref)) {
    same <- isTRUE(all.equal(readRDS(ref), current))
    record("plot data", name, if (same) "PASS" else "FAIL",
           if (same) "same as tests/paper/reference" else
             paste(head(all.equal(readRDS(ref), current), 3), collapse = "; "))
  } else {
    record("plot data", name, "NOTE",
           "no reference yet; see --update-reference")
  }
}

# Stage fig6: R/results_analysis.Rmd ----------------------------------------------

if ("fig6" %in% stages) {
  ok <- run_in_copy("fig6", c(
    'e <- new.env()',
    'rmarkdown::render("R/results_analysis.Rmd", envir = e, run_pandoc = FALSE,',
    '                  quiet = TRUE)',
    'panels <- mget(c("num_points_hist", "num_recalcs_viol", "prop_recalcs_viol",',
    '                 "r2_brks_viol", "r2_pts_viol", "r1_pts_viol"), envir = e)',
    'saveRDS(lapply(panels, function(p)',
    '  lapply(ggplot2::ggplot_build(p)$data, as.data.frame)), "plot_data_fig6.rds")'
  ))
  if (ok) {
    compare_figure("Figure 6", "combined_results_wide_viol_fig6.tiff")
    check_plot_data("figure6", readRDS(file.path(work, "plot_data_fig6.rds")))
  }
}

# Stage figures: R/paper_plots.R ---------------------------------------------------

if ("figures" %in% stages) {
  ok <- run_in_copy("figures", c(
    'code <- readLines("R/paper_plots.R")',
    'at <- grep("^save_plots <- FALSE", code)',
    'stopifnot(length(at) == 1)',
    'code[at] <- "save_plots <- TRUE"',
    # R/paper_plots.R calls autospc:::format_SPC(..., ymin, ymax), passing two
    # objects that the script does not create. In autospc 0.0.0.9031 these
    # arguments are matched by position to point_size and line_width_sf, so
    # they are set here to autospc's defaults for those arguments.
    'ymin <- 2',
    'ymax <- 1',
    'eval(parse(text = code), envir = globalenv())',
    'tables <- mget(c("dataA", "dataB", "dataC", paste0("data", 1:7),',
    '                 "data_algorithm", "data_naive3", "data_naive3b"))',
    'saveRDS(tables, "paper_plots_tables.rds")',
    'saveRDS(lapply(list(supplementary_figure = p_fig1, figure1 = p_fig2,',
    '                    figure2 = p_steps, figure3 = p_approaches),',
    '  function(p) lapply(ggplot2::ggplot_build(p)$data, as.data.frame)),',
    '  "plot_data_figures.rds")'
  ))
  record("figures", "R/paper_plots.R needs objects ymin and ymax", "NOTE",
         paste0("not created by the script; set to 2 and 1, autospc's default ",
                "point_size and line_width_sf, to which format_SPC() matches them"))
  if (ok) {
    compare_figure("Figure 1", "p_fig1_r.tiff")
    compare_figure("Figure 2", "p_steps_fig2_r.tiff")
    compare_figure("Figure 3", "p_approaches_fig3_r.tiff")
    compare_figure("Supplementary figure (simulated C chart)", "p_supp_fig1_r.tiff")
    pd <- readRDS(file.path(work, "plot_data_figures.rds"))
    for (nm in names(pd)) check_plot_data(nm, pd[[nm]])

    # Facts stated in the captions of figures 1 to 3 and in the Discussion
    tb <- readRDS(file.path(work, "paper_plots_tables.rds"))
    s <- "captions"
    month <- function(d) format(as.Date(d), "%Y-%m")
    calc_periods <- function(t) {
      t %>% mutate(row = row_number(),
                   period = cumsum(row == 1 | breakPoint %in% TRUE)) %>%
        filter(periodType == "calculation") %>%
        group_by(period) %>%
        summarise(from = month(min(x)), to = month(max(x)), n = n(),
                  .groups = "drop")
    }
    # The shift rule breaks, as runs of consecutive rule 2 points
    rule2_runs <- function(t) {
      t %>% filter(!is.na(y)) %>%
        mutate(run = cumsum(rule2 %in% TRUE & !(lag(rule2, default = FALSE) %in% TRUE))) %>%
        filter(rule2 %in% TRUE) %>%
        group_by(run) %>%
        summarise(from = month(min(x)), to = month(max(x)), .groups = "drop")
    }
    breaks_at <- function(t) month(t$x[t$breakPoint %in% TRUE])

    # Figure 1: (a) data2, (b) data3, (c) data4, (d) data5
    d <- tb$data5
    check_value(s, "Figure 1: Hospital A data March 2015 to April 2021",
                c(month(min(d$x[!is.na(d$y)])) == "2015-03",
                  month(max(d$x[!is.na(d$y)])) == "2021-04"), c(1, 1))
    d <- tb$data2
    check_true(s, "Figure 1a: attendances to August 2017",
               month(max(d$x[!is.na(d$y)])) == "2017-08")
    cp <- calc_periods(d)
    check_true(s, "Figure 1a: calculation period of 21 months to November 2016",
               nrow(cp) == 1 && cp$n == 21 && cp$to == "2016-11",
               paste(cp$from, cp$to, cp$n))
    d <- tb$data3
    check_true(s, "Figure 1b: attendances to August 2019",
               month(max(d$x[!is.na(d$y)])) == "2019-08")
    r <- rule2_runs(d)
    check_true(s, "Figure 1b: shift rule break from August 2017 to August 2019",
               nrow(r) == 1 && r$from == "2017-08" && r$to == "2019-08",
               paste(r$from, r$to, collapse = "; "))
    d <- tb$data4
    cp <- calc_periods(d)
    check_true(s, "Figure 1c: re-established from August 2017; calculation period August 2017 to April 2019",
               identical(breaks_at(d), "2017-08") && nrow(cp) == 2 &&
                 cp$from[2] == "2017-08" && cp$to[2] == "2019-04",
               paste(cp$from, cp$to, collapse = "; "))
    d <- tb$data5
    r <- rule2_runs(d)
    check_true(s, "Figure 1d: shift rule break from February 2020 to April 2021",
               nrow(r) >= 1 && tail(r$from, 1) == "2020-02" && tail(r$to, 1) == "2021-04",
               paste(r$from, r$to, collapse = "; "))
    check_true(s, "Figure 1d: limits not re-established (insufficient data)",
               identical(breaks_at(d), "2017-08"),
               paste(breaks_at(d), collapse = ", "))

    # Figure 2: (a)-(d) as figure 1, (e) data6 (every shift), (f) data7 (SSA)
    check_true(s, "Figure 2a: baseline from the first 21 points",
               calc_periods(tb$data2)$n[1] == 21)
    check_true(s, "Figure 2e: candidate limits at the start of the February 2020 shift",
               "2020-02" %in% breaks_at(tb$data6),
               paste(breaks_at(tb$data6), collapse = ", "))
    check_true(s, "Figure 2f: candidate rejected; only the August 2017 re-establishment",
               identical(breaks_at(tb$data7), "2017-08"),
               paste(breaks_at(tb$data7), collapse = ", "))

    # Figure 3 and Discussion
    check_true(s, "Figure 3c / Discussion: RSR8 re-establishes in November 2018",
               "2018-11" %in% breaks_at(tb$data_naive3b),
               paste(breaks_at(tb$data_naive3b), collapse = ", "))
    d <- tb$data_algorithm %>% filter(format(as.Date(x), "%Y") %in% c("2018", "2019"))
    check_true(s, "Discussion: SSA, no shift rule break in 2018 and 2019",
               !any(d$rule2 %in% TRUE))
    check_true(s, "Discussion: SSA, stable in 2018 and 2019 except November 2019",
               identical(month(d$x[d$rule1 %in% TRUE]), "2019-11"),
               paste(month(d$x[d$rule1 %in% TRUE]), collapse = ", "))
    check_true(s, "Figure 3c: RSR8 has more re-establishments than RSR, than SSA",
               length(breaks_at(tb$data_naive3b)) > length(breaks_at(tb$data_naive3)) &&
                 length(breaks_at(tb$data_naive3)) >= length(breaks_at(tb$data_algorithm)),
               sprintf("SSA %d, RSR %d, RSR8 %d", length(breaks_at(tb$data_algorithm)),
                       length(breaks_at(tb$data_naive3)),
                       length(breaks_at(tb$data_naive3b))))
  }
}

# Stage pdfs: R/output_to_pdf.R ------------------------------------------------------

# The PDFs differ only in their creation and modification dates if the
# charts are the same
pdf_without_dates <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  text <- rawToChar(bytes[bytes != as.raw(0)])
  gsub("/(CreationDate|ModDate) \\(D:[^)]*\\)", "", text, useBytes = TRUE)
}

if ("pdfs" %in% stages) {
  # print.eval = TRUE: the title page of each PDF is a ggplot at the top level
  # of the script, which is drawn only if top-level values are printed, as
  # when the script is run in RStudio
  ok <- run_in_copy("pdfs", 'source("R/output_to_pdf.R", print.eval = TRUE)')
  if (ok) {
    pdfs <- c("C_algorithm.pdf" = "Supplemental file 4 (attendances, SSA)",
              "P_algorithm.pdf" = "Supplemental file 5 (performance, SSA)")
    others <- setdiff(list.files(work, pattern = "\\.pdf$"), names(pdfs))
    pdfs <- c(pdfs, setNames(rep("not in the paper", length(others)), others))
    for (f in names(pdfs)) {
      check <- paste0(f, ": ", pdfs[[f]])
      old <- file.path(repo, f); new <- file.path(work, f)
      if (!file.exists(old)) {
        record("pdfs", check, "NOTE", "no stored copy to compare with")
        next
      }
      if (identical(pdf_without_dates(old), pdf_without_dates(new))) {
        record("pdfs", check, "PASS", "identical apart from dates")
        next
      }
      # R/output_to_pdf.R ends by removing pages with no text from some PDFs
      # with staplr, which rewrites the file with its own metadata. So
      # compare what is on the pages: their text, and the pages drawn as
      # images.
      pages_old <- pdftools::pdf_info(old)$pages
      pages_new <- pdftools::pdf_info(new)$pages
      text_old <- pdftools::pdf_text(old); text_new <- pdftools::pdf_text(new)
      same_pages <- pages_old == pages_new
      differing_text <- if (same_pages) which(text_old != text_new) else integer()
      differing_image <- integer()
      if (same_pages && length(differing_text) == 0) {
        for (page in seq_len(pages_old)) {
          img_old <- pdftools::pdf_render_page(old, page = page, dpi = 72)
          img_new <- pdftools::pdf_render_page(new, page = page, dpi = 72)
          if (!identical(img_old, img_new)) differing_image <- c(differing_image, page)
        }
      }
      if (same_pages && length(differing_text) == 0 && length(differing_image) == 0) {
        record("pdfs", check, "PASS",
               sprintf("same %d pages: text and page images identical", pages_old))
        next
      }
      # Pages that say a chart could not be drawn, by pseudonymised code
      too_few <- function(text) {
        sort(as.integer(sub(".*Too few points for ([0-9]+).*", "\\1",
                            grep("Too few points for", text, value = TRUE))))
      }
      record("pdfs", check, if (f %in% names(pdfs)[1:2]) "FAIL" else "NOTE",
             sprintf(paste0("%d pages stored, %d now; text differs on %s pages; ",
                            "page images differ on %s pages; 'Too few points' pages ",
                            "for codes [%s] stored, [%s] now"),
                     pages_old, pages_new,
                     if (same_pages) length(differing_text) else "?",
                     if (same_pages && length(differing_text) == 0)
                       length(differing_image) else "?",
                     paste(too_few(text_old), collapse = " "),
                     paste(too_few(text_new), collapse = " ")))
    }
  }
}

# Report -----------------------------------------------------------------------------

setwd(old_wd)
if (update_reference && nrow(new_figure_reference) > 0) {
  dir.create(reference_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(new_figure_reference, figure_reference_file, row.names = FALSE)
}
write.csv(checks, file.path(out_dir, "report.csv"), row.names = FALSE)
counts <- table(factor(checks$status, levels = c("PASS", "WARN", "FAIL", "NOTE")))
report <- c(
  "# Paper checks",
  "",
  sprintf("Run %s, stages: %s.", format(Sys.time(), "%Y-%m-%d %H:%M"),
          paste(stages, collapse = ", ")),
  sprintf("autospc %s, R %s.", packageVersion("autospc"), getRversion()),
  "",
  sprintf("**%d passed, %d warnings, %d failed, %d notes.**", counts[["PASS"]],
          counts[["WARN"]], counts[["FAIL"]], counts[["NOTE"]]),
  "",
  "| Section | Check | Status | Detail |",
  "|---|---|---|---|",
  sprintf("| %s | %s | %s | %s |", checks$section, checks$check, checks$status,
          gsub("|", "/", checks$detail, fixed = TRUE))
)
writeLines(report, file.path(out_dir, "report.md"))
# Keep the figures and the supplementary PDFs made by this run; remove the rest
# of the working copy
invisible(file.rename(file.path(work, "plots"), file.path(out_dir, "plots")))
for (f in c("C_algorithm.pdf", "P_algorithm.pdf")) {
  if (file.exists(file.path(work, f))) {
    file.rename(file.path(work, f), file.path(out_dir, "plots", f))
  }
}
unlink(work, recursive = TRUE)
cat(sprintf("\n%d passed, %d warnings, %d failed, %d notes. Report: %s\n",
            counts[["PASS"]], counts[["WARN"]], counts[["FAIL"]], counts[["NOTE"]],
            file.path(out_dir, "report.md")))
if (counts[["WARN"]] > 0) {
  warning("Some figures differ from plots/ in a way accepted earlier; see the report.",
          call. = FALSE)
}
if (counts[["FAIL"]] > 0) stop("Some checks failed; see the report.")
