# autospc-results

Code and data for the case study in:

> Woodcock T, O'Connor I, Bell D. The Stable Shift Algorithm. *BMJ Qual Saf*
> 2026;35:201–212. doi:10.1136/bmjqs-2025-019263

The case study applies the Stable Shift Algorithm (SSA) and other ways of re-establishing control
limits to 557 time series of A&E attendances and 4-hour performance, for providers in England
and Scotland, March 2015 to April 2023.

This repository is a record of how the paper's results were made. It is not maintained for
other use, and it should not be changed in ways that alter its results. `tests/paper/` checks
that it still reproduces the paper (see below).

## Software

R 4.5.1, with the packages in `renv.lock`, including autospc v0.0.0.9031 (GitHub tag
`v0.0.0.9031`, `d885f75`). Run `renv::restore()` before running anything. Later versions of
autospc give slightly different results; see "Later versions of autospc" below.

## How the results are made

Run the scripts from the repository root (except the R Markdown file), in this order:

| Step | Script | Makes | In the paper |
|---|---|---|---|
| 1 | `R/get_results.R` | `data/perf_series_df_clean.rds`, `data/outputs/*.rds` and `.xlsx` | all case study results |
| 2 | `R/results_analysis.Rmd` (knit from `R/`) | summary tables; `plots/combined_results_wide_viol_fig6.*` and other versions of figure 6 | Results text, figure 6 |
| 3 | `R/paper_plots.R`, with `save_plots <- TRUE` | `plots/p_fig1_r.*`, `p_steps_fig2_r.*`, `p_approaches_fig3_r.*`, `p_supp_fig1_r.*` and others | figures 1–3, supplementary example chart |
| 4 | `R/output_to_pdf.R` | `C_*.pdf`, `P_*.pdf` in the repository root | `C_algorithm.pdf` and `P_algorithm.pdf` are online supplemental files 4 and 5 |

Steps 1 and 4 take about 20 and 45 minutes. Step 1 overwrites the stored outputs in
`data/outputs/`.

### Step 1: `R/get_results.R`

1. **Time series.** The series come from `data/AE_Data.RDS` and `data/AE_Data_Scot.RDS`, made
   with the nhsAEscraper package from the NHS spreadsheets in `data-raw/sitreps/` (not under
   version control). `make_perf_series_data_frame()` (`R/get_perf_series_df.R`) turns them into
   one row per organisation, week or month, in `data/perf_series_df.rds`. This step is inside
   `if(FALSE)` because it is slow (about 20 minutes); the script loads the stored file instead.
2. **Inclusion.** `clean_perf_series_df()` (`R/perf_series_cleaning.R`) removes series with
   fewer than 48 points, and sets to `NA` the performance (or attendance) values of series whose
   interquartile range is less than 0.01, that is, series that almost always take the same value.
   The result is saved as `data/perf_series_df_clean.rds`.
3. **Re-establishing limits.** For each organisation, and for monthly and weekly data, the
   script calls `get_volume_from_perf_series_df()` (C′ charts of average daily attendances) or
   `get_peformance_from_perf_series_df()` (P′ charts of attendances within 4 hours), both in
   `R/results_functions.R`. These call `autospc::plot_auto_SPC()` with the arguments for each
   approach. There is one block of code per approach, chart type and time unit, 20 in all, each
   saving `data/outputs/limits_table_output_<Monthly|Weekly>_<C|P>_<approach>.rds`.
4. **Summaries.** `get_results_summary()` counts, for each series, the points, re-establishments
   (`num_recalcs`), points outside the limits (`num_rule1_points`), points in shift rule breaks
   (`num_rule2_points`) and shift rule breaks (`num_rule2_breaks`). The results are saved in
   `data/outputs/full_results/`: `full_results_all` (one row per series and approach), and
   `summary_results_all` and `summary_results_all_wm` (summaries by approach, and by approach and
   time unit).

### Approaches

The scripts use their original names; the paper uses the names in the second column.

| In the code | In the paper | Arguments |
|---|---|---|
| `algorithm` | SSA | `periodMin` 24 (monthly) or 21 (weekly) |
| `naive3` | RSR | as SSA, with `recalEveryShift = TRUE` |
| `naive3b` | RSR8 | `periodMin = 8`, `baseline` 24 or 21, `recalEveryShift = TRUE` |
| `naive1` | not used | the whole series as one calculation period |
| `naive2` | not used | baseline limits extended to the end (`noRecals = TRUE`) |

All use `runRuleLength = 8` and `maxNoOfExclusions = 0`. `R/results_analysis.Rmd` leaves out
`naive1` and `naive2`.

### Figures

`R/paper_plots.R` draws its charts for "Hospital A" (organisation G513H, monthly) and for
simulated data. File names ending `_r` follow the figure numbering of the revised manuscript;
the object names in the script follow the original submission:

| File in `plots/` | Object in `R/paper_plots.R` | In the paper |
|---|---|---|
| `p_fig1_r` | `p_fig2` | Figure 1 |
| `p_steps_fig2_r` | `p_steps` | Figure 2 |
| `p_approaches_fig3_r` | `p_approaches` | Figure 3 |
| `p_supp_fig1_r` | `p_fig1` | supplementary example chart (simulated data) |
| `combined_results_wide_viol_fig6` | made in `R/results_analysis.Rmd` | Figure 6 |

Things to know:

- `R/paper_plots.R` passes `ymin` and `ymax` to `autospc:::format_SPC()` but does not create
  them. Set `ymin <- 2` and `ymax <- 1` before running it: with autospc v0.0.0.9031 they are
  matched to `point_size` and `line_width_sf`, and these are autospc's defaults. Figures 1–3 in
  `plots/` were made on 1 July 2025 with an earlier, pre-release autospc.
- In `R/output_to_pdf.R`, each PDF's title page is drawn only when top-level values are printed,
  as when the script is run in RStudio. With `source()`, use `print.eval = TRUE`. The script
  ends by removing pages with no text from each PDF, using staplr, which needs Java.
- Figure 6c plots re-establishments per point (`num_recalcs / n`). The caption describes a
  proportion of the maximum possible number of re-establishments, which is
  `num_actual_recals_by_potential_recals` in `full_results_all`.
- `plots/` and `*.pdf` are in `.gitignore`. The figures and PDFs used in the paper are kept only
  in the working folder.

## Checking that the paper is reproduced

```
Rscript tests/paper/check_paper.R
```

runs all the steps above in a temporary copy of the repository (about 1.5 hours) and checks the
results against the stored outputs, the numbers in the paper, the figures and the supplementary
PDFs. See `tests/paper/README.md`.

## Later versions of autospc

Run with later versions of autospc, the results differ slightly from the paper (checked with
autospc 0.2.0.9001, October 2026). The medians and IQRs in figure 6 move by at most 4 points,
and no comparison between SSA and RSR changes. The main reasons:

- autospc now rounds C and P counts that are not whole numbers. Most of the C′ values (daily
  average attendances) are not whole numbers, nor are some derived P′ counts.
- A point on the centre line no longer ends a run (autospc #273).
- After a calculation period with no variation, limits are not re-established (autospc #310, a
  bug introduced in #300). This affects providers RW4, Y02147 and Y03082.

## Other files

- `R/perf_series_df.rds`: an older copy of `data/perf_series_df.rds`, with the same contents.
- `R/Results/outputs_20230618/`: empty.
- `R/*.html`: earlier renderings of `R/results_analysis.Rmd`.
- `plots/2025-07-01-bkp/`, `plots/2025-09-19-bkp/`, `plots/prev_pdfs/`: earlier versions of
  the figures.
