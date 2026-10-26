# Checks against the BMJ Quality & Safety paper

`check_paper.R` checks that this repository still reproduces what the paper reports:

> Woodcock T, O'Connor I, Bell D. The Stable Shift Algorithm. *BMJ Qual Saf*
> 2026;35:201–212. doi:10.1136/bmjqs-2025-019263

## Running it

From the repository root, in R with the project's renv library active
(`renv::restore()` first if `renv::status()` says the library is out of sync):

```r
system("Rscript tests/paper/check_paper.R")
```

or, in a terminal, `Rscript tests/paper/check_paper.R`. A full run takes about one and a half hours.
To run some stages only:

```
Rscript tests/paper/check_paper.R --stages=numbers,figures
```

The analysis scripts are run unchanged in a temporary copy of the repository, so nothing in
the repository is overwritten. The report is written to `tests/paper/output/<date-time>/`
(`report.md`, `report.csv`), with a log of each stage, the figures and supplementary PDFs
made by the run, and, for any figure that differs from `plots/`, an image of the differences
(differing pixels in red). The script ends with an error if any check fails. A figure that
differs from `plots/` only in the way recorded as accepted (see below) gives a warning, not an
error.

## Stages

| Stage | Runs | Checks | Time |
|---|---|---|---|
| `raw` | `make_perf_series_data_frame()` on `data/AE_Data*.RDS` | identical to `data/perf_series_df.rds` | 20 min |
| `results` | `R/get_results.R` | `data/perf_series_df_clean.rds` and every `.rds` in `data/outputs/` identical | 25 min |
| `numbers` | — | every case study number in the text (Abstract, Methods, Results, Discussion) | seconds |
| `fig6` | `R/results_analysis.Rmd` | Figure 6 against `plots/combined_results_wide_viol_fig6.tiff` | 1 min |
| `figures` | `R/paper_plots.R` | Figures 1–3 and the supplementary figure against `plots/`; the facts in the captions of figures 1–3 and the Discussion (Hospital A) | 1 min |
| `pdfs` | `R/output_to_pdf.R` | `C_algorithm.pdf` and `P_algorithm.pdf` (online supplemental files 4 and 5): identical apart from their dates, or the same text and page images; the other PDFs are reported but not in the paper | 45 min |

`numbers` uses the results of the `results` stage if it ran in the same run, and otherwise the
stored `data/outputs/full_results/full_results_all.rds`.

## Figures and the reference

The figures are compared pixel by pixel with the TIFF files in `plots/`. Text is drawn with the
computer's fonts, so a figure made on another computer, or with other versions of the graphics
packages, can differ slightly even when everything plotted is the same. Look at the images of
the differences. If the differences are acceptable, run once with `--update-reference`:

```
Rscript tests/paper/check_paper.R --stages=fig6,figures --update-reference
```

This saves to `tests/paper/reference/`:

- the data behind every layer of every figure (`plot_data_*.rds`), which later runs must match
  exactly, on any computer;
- `figures.csv`: a checksum of each figure this run made, and how much it differed from
  `plots/`. Later runs on the same computer give a warning, not an error, if they make
  exactly that figure again; any other difference is an error.

Commit `tests/paper/reference/` so that the checks are locked to it. `tests/paper/output/` is
ignored by git.

## Things to know

- `plots/` and `*.pdf` are in `.gitignore`, so the figures and supplementary PDFs that went into
  the paper are not under version control. The checks compare against the copies in the working
  folder.
- `R/paper_plots.R` passes two objects, `ymin` and `ymax`, to `autospc:::format_SPC()`. The
  script does not create them. In autospc 0.0.0.9031 they are matched by position to
  `point_size` and `line_width_sf`. The check sets them to autospc's defaults for those
  arguments, 2 and 1. Figures 1–3 in `plots/` are dated 1 July 2025, so they were made with an
  earlier, pre-release autospc.
- The title page of each PDF made by `R/output_to_pdf.R` is a ggplot at the top level of the
  script. It is drawn only if top-level values are printed, as when the script is run in
  RStudio, so the check sources the script with `print.eval = TRUE`.
- The paper's Results text gives the numbers of points within shift rule breaks (figure 6e) as
  numbers of points outside the control limits (figure 6f). The check tests the numbers as
  printed against the points within shift rule breaks, and notes the figure 6f values. The
  report also notes that the caption of figure 6c describes a proportion of the maximum number
  of re-establishments, while the plot shows re-establishments per point. Methods gives R 4.4.1
  and ggplot2 3.5.1; `renv.lock` has R 4.5.1 and ggplot2 4.0.0.

## Not covered

Figure 4 (flow chart, drawn by hand), figure 5 and online supplemental file 2 (simulations,
whose code is not in this repository), and online supplemental file 3 (detailed flow chart).
The supplementary figure checked here (`plots/p_supp_fig1_r.tiff`, simulated data) is
presumably in online supplemental file 1; the rest of that file is not checked.
