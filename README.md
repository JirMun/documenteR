# documenteR

Collect, document and export the outputs of an R analysis.

Instead of scattering `ggsave()`, `write.csv()` and `saveRDS()` calls through a
script, you **mark** each output as you produce it and describe it while you
still remember what it is. One call at the end writes a versioned,
self-describing folder: the files, a README per output type, a manifest with
checksums, a variable codebook, a typeset PDF catalogue of every plot, a copy
of the code, and a log of everything that happened.

## Installation

```r
# install.packages("remotes")
remotes::install_github("JirMun/documenteR")
```

---

# Tutorial

Ten minutes, copy-pasteable, no files of your own needed — it uses the
built-in `cars` dataset and writes to a temporary folder. Work through it once
and you will know everything you need for daily use.

```r
library(documenteR)

# Everything in this tutorial is written here; nothing touches your project.
outdir <- file.path(tempdir(), "tutorial")
```

## 1. Start collecting

```r
init_outputs()
#> ✔ Output collection initialised as `list_outputs`.
```

This creates an object called `list_outputs` in your workspace. It is the
basket you drop outputs into. Call it once, near the top of a script.

## 2. Append your first output

```r
data_cars <- cars

append_data(data_cars,
            desc   = "Speed and stopping distance of 1920s cars",
            source = c(datasets = "R built-in dataset"),
            labels = c(speed = "Speed (mph)", dist = "Stopping distance (ft)"))
#> ✔ Appended "data_cars" to data.
```

Three things to notice:

- You did **not** give a file path or a file name. `documenteR` derives those
  from the object name at export time.
- You did not have to reassign anything — no `list_outputs <- append_data(...)`.
- The documentation is optional but this is the moment to write it. `desc`,
  `source` and `labels` all end up in files that travel with the data.

## 3. See what you have

Just print the collection:

```r
list_outputs
#> ── documenteR output collection ──────────────────────────────────────────────
#>   id     name      title                          size           formats
#> data (1)
#>   01     data_cars                                50 x 2         .csv
#>
#> ✔ All 1 output carry some documentation.
#> formats  data .csv  plots .png  tables .csv  stats .csv
#> ℹ `summary()` for the full inventory; `save_outputs()` to export.
```

The `id` column is the number the file will actually get (`01_data_cars.csv`),
so this doubles as a preview of the export.

## 4. Add a plot

Any plotting system works. Base R plots cannot be captured as objects, so you
pass a **function of no arguments** that draws one:

```r
plt_speed <- function() {
  plot(cars$speed, cars$dist, pch = 19, col = "steelblue",
       xlab = "Speed (mph)", ylab = "Stopping distance (ft)")
  abline(lm(dist ~ speed, data = cars), col = "firebrick", lwd = 2)
}

append_plot(plt_speed,
            desc = "Stopping distance rises faster than speed",
            vars = c(x = "Speed (mph)", y = "Stopping distance (ft)"),
            type = "m_wide")
#> ✔ Appended "plt_speed" to plots.
```

A `ggplot2` object would be passed directly instead:

```r
plt_speed <- ggplot2::ggplot(cars, ggplot2::aes(speed, dist)) +
  ggplot2::geom_point()
append_plot(plt_speed, type = "m_wide")
```

`type = "m_wide"` picks a size preset — 16 × 12.35 cm, sized for an A4 page
with 2.5 cm margins. See [Plot sizes](#plot-sizes).

## 5. Add a table and a number

```r
tab_by_speed <- aggregate(dist ~ speed > 15, data = cars, FUN = mean)
names(tab_by_speed) <- c("fast", "mean_dist")
append_table(tab_by_speed, desc = "Mean stopping distance by speed group")
#> ✔ Appended "tab_by_speed" to tables.

stat_mean_speed <- mean(cars$speed)
append_stats(stat_mean_speed,
             type       = "float",
             stat_round = 1,
             formula    = "sum(speed)/n",
             vars       = c(speed = "Speed of each car", n = "Number of cars"),
             text       = "Cars travelled at * mph on average.")
#> ✔ Appended "stat_mean_speed" to stats.
```

`append_table()` is for presentation tables, `append_data()` for the
analysis-ready dataset others will reuse — data additionally gets a codebook.

`append_stats()` is for a single number you quote in a report. The `*` in
`text` marks where the number goes; you get the finished sentence back in the
export, which means the figures in your report can always be traced to the
code that produced them.

## 6. Review before exporting

```r
list_outputs
#> ── documenteR output collection ──────────────────────────────────────────────
#>   id     name            title                    size           formats
#> data (1)
#>   01     data_cars                                50 x 2         .csv
#> plots (1)
#>   01     plt_speed                                16 x 12.35 cm  .png
#> tables (1)
#>   01     tab_by_speed                             2 x 2          .csv
#> stats (1)
#>   01     stat_mean_speed                          15.4
#>
#> ✔ All 4 outputs carry some documentation.
```

For the full inventory, and to find what still needs documenting, use
`summary()`:

```r
summary(list_outputs)
#> type   file            title                         size          formats doc
#> data   01_data_cars                                  50 x 2        .csv    ✔·✔
#> plots  01_plt_speed                                  16 x 12.35 cm .png    ✔✔·
#> tables 01_tab_by_speed                               2 x 2         .csv    ✔··
#> stats  01_stats                                      15.4          .csv    ·✔·
#> doc columns: ✔/· for description, variables, source
```

It is an ordinary data frame underneath, so you can ask it questions:

```r
s <- summary(list_outputs)
s$name[!s$has_desc]      # which outputs still need a description?
#> [1] "stat_mean_speed"
```

To see everything you recorded about one output, print it:

```r
list_outputs$plots$plt_speed
#> ── plot plt_speed ────────────────────────────────────────────────────────────
#> title      plt_speed
#> desc       Stopping distance rises faster than speed
#> vars       1. x: Speed (mph)
#>            2. y: Stopping distance (ft)
#> type       m_wide
#> created    2026-09-10 17:39:02
```

## 7. Check the plot size

Before a full export, see whether a plot actually suits the size you chose.
This writes it at exactly the export geometry and opens it:

```r
preview_plot(plt_speed, type = "m_wide")
```

## 8. See what would be written

```r
save_outputs(outdir, list_outputs, dry_run = TRUE)
#> ── Dry run: nothing will be written ──
#> Target: '.../tutorial/0_1'
#> ✔ data: 2 files
#> • 01_data_cars.csv
#> • 00_codebook.csv
#> ✔ plots: 2 files
#> • 01_plt_speed.png
#> • 00_01_catalogue.pdf
#> ✔ tables: 1 file
#> • 01_tab_by_speed.csv
#> ✔ stats: 1 file
#> • 01_stats.csv
```

## 9. Export

```r
res <- save_outputs(outdir, list_outputs,
                    project_title = "Cars tutorial",
                    version_notes = "First upload")
#> ── Exporting to '.../tutorial/0_1' ──
#> version name Naši hopálci - Děda Zajíc, Prezident Zájínek a Popelka - s
#> oblibou slaví Den veteránů zaječích válek.
#> → Saving data (1)
#> → Saving plots (1)
#> → Saving tables (1)
#> → Saving stats (1)
#> ✔ Version 0_1: 7 files written to '.../tutorial/0_1'
```

Yes, each version gets a silly name. It turns out to be genuinely useful when
someone asks which version a figure came from.

## 10. What you got

```r
list.files(res$path, recursive = TRUE)
#>  [1] "00_DOC.txt"                    "00_LOG.txt"
#>  [3] "00_MANIFEST.csv"               "00_MANIFEST.json"
#>  [5] "01_output/00_README.txt"       "01_output/01_list_outputs.rds"
#>  [7] "02_data/00_codebook.csv"       "02_data/00_README.txt"
#>  [9] "02_data/01_data_cars.csv"      "03_plots/00_01_catalogue.pdf"
#> [11] "03_plots/00_README.txt"        "03_plots/01_plt_speed.png"
#> [13] "04_tables/00_README.txt"       "04_tables/01_tab_by_speed.csv"
#> [15] "05_stats/00_README.txt"        "05_stats/01_stats.csv"
```

Worth opening, in this order:

| File | What it is |
|------|------------|
| `03_plots/00_01_catalogue.pdf` | one page per plot, at true size, with its documentation typeset above it |
| `02_data/00_codebook.csv` | every variable in every dataset: class, missingness, distinct values, range |
| `02_data/00_README.txt` | the documentation you wrote, as plain text next to the files |
| `00_MANIFEST.csv` | every file with its source object, size and MD5 checksum |
| `00_DOC.txt` | version, notes and full session info |
| `00_LOG.txt` | every message and warning raised during the export |

And the statistic came back as a finished sentence:

```r
read.csv(file.path(res$path, "05_stats", "01_stats.csv"))[, c("stat", "formula_text", "text_fill")]
#>   stat                          formula_text                              text_fill
#> 1 15.4 sum(Speed of each car)/Number of cars Cars travelled at 15.4 mph on average.
```

## 11. Change something and export again

Re-run your analysis, then export again. You get a new version; nothing is
overwritten:

```r
res2 <- save_outputs(outdir, list_outputs, version_notes = "Added a table")
#> ✔ Version 0_2: 7 files written to '.../tutorial/0_2'

dr_versions(outdir)[, c("version", "notes")]
#>   version         notes
#> 1     0_1  First upload
#> 2     0_2 Added a table
```

Use `new_version = TRUE` for a major bump (`0_2` → `1_0`) when results change
substantively, and `update_latest_version = TRUE` to overwrite the last
version in place when you are only fixing a typo.

## 12. Confirm an export is intact

After copying an export to a shared drive or sending it on:

```r
dr_verify(outdir)
#> ✔ All 7 files in '0_2' match the manifest.
```

**That is the whole workflow:** `init_outputs()` → `append_*()` as you work →
`save_outputs()`. Everything below is detail you can look up when you need it.

---

# In a real script

This is the pattern worth copying. `setup_code()` will write you a starter
script laid out exactly this way:

```r
# 04_code/01_01_cars.R

library(documenteR)

this_file <- "01_01_cars"
init_outputs()

# ---- analysis -------------------------------------------------------------

data_cars <- cars
append_data(data_cars, desc = "...", source = c(CZSO = "..."))

plt_speed <- function() plot(cars)
append_plot(plt_speed, desc = "...", type = "m_wide")

# ---- export ---------------------------------------------------------------

output_dir <- file.path("05_analyzy", "01_data", this_file)

# dr_is_rerun() is TRUE only while dr_check_code() re-runs this script as a
# reproducibility check, so the check does not itself write an export.
if (!dr_is_rerun()) {
  save_outputs(
    output_dir    = output_dir,
    output_list   = list_outputs,
    project_title = "Cars",
    version_notes = "First upload",
    code_location = file.path("04_code", paste0(this_file, ".R"))
  )
}
```

```r
setup_code("01_01_cars", project_title = "Cars")
```

---

# Reference

## Documenting well

Every `append_*()` takes the same documentation arguments. None are required,
but each one answers a question someone will eventually ask:

| Argument | Answers |
|----------|---------|
| `title` | What is this, in a few words? Defaults to the object name. |
| `desc` | What does it show, and what should the reader notice? |
| `vars` | What is each variable? `c(x = "Speed (mph)")` |
| `source` | Where did the data come from? `c(CZSO = "Census 2021")` |
| `notes` | Caveats, to-dos, anything you would say out loud. |
| `formula` | For statistics: how was the number computed? |
| `text` | For statistics: the sentence it appears in, with `*` as placeholder. |
| `section`, `page` | For statistics: where in the report it is used. |

`vars` and `source` accept named vectors; the names become the labels in the
README, the codebook and the catalogue.

## Plot sizes

`type` selects a preset from `gpars$sizes$type`. The defaults suit an A4 page
with 2.5 cm margins, so 16 cm is the usable text width:

| Preset | Size (cm) | | Preset | Size (cm) |
|--------|-----------|-|--------|-----------|
| `s` | 8 × 8 | | `m_wide` | 16 × 12.35 |
| `s_wide` | 16 × 8 | | `m_tall` | 12.35 × 24.7 |
| `s_tall` | 8 × 16 | | `l` | 24.7 × 16 |
| `m` | 12.35 × 12.35 | | `l_landscape` | 16 × 24.7 |

`H`, `W` and `units` override a preset. Add your own:

```r
list_outputs$gpars$sizes$type$banner <- list(units = "cm", H = 6, W = 24)
```

Plots are written at their exact physical size — `documenteR` opens the
graphics device itself rather than going through `ggsave()`, so a plot
declared 16 × 12.35 cm is exactly that in every format, and in the catalogue.

## Which plots are supported

`ggplot2` (including `patchwork` compositions), `lattice`/`trellis`, `grid`
grobs and `gtable`s, recorded base plots from `recordPlot()`, and functions of
no arguments that draw with base graphics. To add another system, define a
method for the `dr_draw()` generic:

```r
dr_draw.my_class <- function(x, ...) {
  my_render_function(x)
  invisible(x)
}
```

## File formats

```r
dr_formats()
```

Data and tables: `.csv`, `.csv2`, `.tsv`, `.xlsx`, `.rds`, `.parquet`,
`.json`. Plots: `.png`, `.jpg`, `.tiff`, `.svg`, `.pdf`, `.eps`. Set them per
output or globally:

```r
append_data(data_cars, format = c(".csv", ".xlsx"))
append_plot(plt_speed, device = c(".png", ".svg"))

list_outputs$gpars$formats$plots <- c(".png", ".svg")
```

`.csv2` writes semicolon-separated values with a comma decimal mark, which is
what Czech and German Excel expects. For UTF-8 accented characters to open
correctly in Excel, set `csv_bom = TRUE`.

## Configuring defaults

Everything adjustable lives in `gpars`. `dr_gpars()` merges your overrides
into the defaults recursively, so you name only what you change:

```r
init_outputs(gpars = dr_gpars(
  formats   = list(plots = c(".png", ".svg")),
  csv_bom   = TRUE,
  stat_round = 2,
  catalogue = dr_catalogue_pars(paper = "a3", orientation = "landscape")
))
```

Or afterwards: `list_outputs$gpars$readme_w <- 90`.

See `?dr_gpars` for every parameter and `?dr_catalogue_pars` for the
catalogue's appearance.

## Subfolders

```r
append_plot(plt_cz, subfolder = "maps")
append_plot(plt_regions, subfolder = "maps")
```

Subfolders are numbered automatically after the root-level files, so a folder
listing always sorts in the order things were appended:

```
03_plots/
├── 01_plt_speed.png
├── 02_maps/
│   ├── 02_01_plt_cz.png
│   └── 02_02_plt_regions.png
└── 00_01_catalogue.pdf
```

## Reproducibility

`dr_check_code()` runs your scripts in a clean R subprocess and reports which
ones fail — it catches the script that only works because of an object left
over in your workspace:

```r
dr_check_code("04_code/01_01_cars.R")
```

`save_outputs(rerun_code = TRUE)` runs that check first and refuses to export
if it fails.

## Inspecting past exports

```r
dr_versions(output_dir)   # every version, with its notes and name
dr_manifest(output_dir)   # every file, its object and its checksum
dr_verify(output_dir)     # re-check the checksums
dr_log(output_dir)        # what the last export recorded
```

Each takes an optional `version = "0_3"`; they default to the most recent.

An old collection can be read back and re-exported:

```r
old <- readRDS(file.path(path, "01_output", "01_list_outputs.rds"))
out <- as_outputs(old)
```

## When something goes wrong

Messages and warnings from other packages are captured into `00_LOG.txt`
rather than printed, so the progress report stays readable; warnings are
summarised at the end. A single output that cannot be written is recorded as
`failed` in the manifest and **the export continues** — one broken plot does
not cost you the other forty files. To find out what happened:

```r
dr_log(output_dir)
res$manifest[res$manifest$status != "ok", c("name", "message")]
```

For a chattier run: `options(documenteR.verbose = TRUE)`. For silence:
`options(documenteR.quiet = TRUE)`. See `?"documenteR-options"`.

## Function reference

| | |
|-|-|
| **Collect** | `init_outputs()`, `append_data()`, `append_plot()`, `append_table()`, `append_stats()`, `append_from_script()`, `remove_outputs()` |
| **Inspect** | `print()`, `summary()`, `preview_plot()`, `save_outputs(dry_run = TRUE)` |
| **Export** | `save_outputs()`, `folder_setup()`, `get_output_README()`, `dr_catalogue()`, `dr_codebook()` |
| **Review** | `dr_versions()`, `dr_manifest()`, `dr_verify()`, `dr_log()` |
| **Configure** | `dr_gpars()`, `dr_catalogue_pars()`, `dr_formats()`, `dr_folders()` |
| **Extend** | `dr_draw()`, `dr_save_plot()`, `dr_can_draw()` |
| **Scaffold** | `setup_code()`, `dr_check_code()`, `dr_is_rerun()` |

---

# Migrating from the standalone `documenteR.R` script

The API is deliberately close to the original, and existing scripts mostly
work unchanged. The differences worth knowing:

| Then | Now |
|------|-----|
| `source("documenteR.R")` | `library(documenteR)` |
| `list_outputs` is a nested list | a `documenteR_outputs` environment that still responds to `$`, `names()` and `length()` |
| `gpars$ggplot` | `gpars$sizes` (`gpars$ggplot` is still read from a saved collection) |
| `test_save()` | `preview_plot()` |
| plots had to be `ggplot` objects | any plotting system — see `dr_draw()` |
| CSVs carried a stray `...1` index column | fixed: `row.names = FALSE` |
| `stat_round` rounded to the nearest `1.001` | fixed: it is the number of decimal places |
| a statistic with a `formula` but no `vars` errored | fixed |
| catalogue pages had different sizes, distorting plots | fixed: one page size, true-size plots |
| `rerun_code = TRUE` re-ran the code *instead of* exporting | it is now a pre-flight check; the export follows |

An old `list_outputs.rds` reads back with `as_outputs()`. See `NEWS.md` for
the complete list.

## Licence

MIT © PAQ Research
