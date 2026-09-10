# documenteR

<!-- badges: start -->
<!-- badges: end -->

Collect, document and export the outputs of an R analysis.

Instead of scattering `ggsave()`, `write.csv()` and `saveRDS()` calls through
a script, you *mark* each output as you produce it and describe it while you
still remember what it is. At the end, one call writes a versioned,
self-describing folder: the files, a README per output type, a manifest with
checksums, a variable codebook, a typeset PDF catalogue of every plot, a copy
of the code, and a log of everything that happened.

## Installation

```r
# install.packages("remotes")
remotes::install_github("JirMun/documenteR")
```

## The workflow

```r
library(documenteR)

init_outputs()          # creates `list_outputs`

# --- as you work -----------------------------------------------------------

data_cars <- cars
append_data(data_cars,
            desc   = "Speed and stopping distance of 1920s cars",
            source = c(datasets = "R built-in"),
            labels = c(speed = "Speed (mph)", dist = "Distance (ft)"),
            format = c(".csv", ".rds"))

plt_cars <- ggplot2::ggplot(cars, ggplot2::aes(speed, dist)) +
  ggplot2::geom_point()
append_plot(plt_cars,
            desc = "Braking distance rises faster than speed",
            vars = c(x = "Speed", y = "Distance"),
            type = "m_wide")

stat_mean <- mean(cars$speed)
append_stats(stat_mean,
             type       = "float",
             stat_round = 1,
             formula    = "sum(speed)/n",
             vars       = c(speed = "Speed", n = "Number of cars"),
             text       = "Cars travelled at * mph on average.")

# --- once, at the end ------------------------------------------------------

save_outputs(
  output_dir    = "05_analyzy/01_cars",
  output_list   = list_outputs,
  project_title = "Cars",
  version_notes = "First upload",
  code_location = "04_code/01_cars.R"
)
```

## What an export looks like

```
05_analyzy/01_cars/
└── 0_1/
    ├── 00_DOC.txt         version number, name, notes, session info
    ├── 00_LOG.txt         every message and warning raised during the export
    ├── 00_MANIFEST.csv    one row per file: object, documentation, size, MD5
    ├── 00_MANIFEST.json
    ├── 01_output/         the collection object + a copy of the code
    ├── 02_data/           datasets + 00_codebook.csv + 00_README.txt
    ├── 03_plots/          plots + 00_01_catalogue.pdf + 00_README.txt
    ├── 04_tables/
    └── 05_stats/          01_stats.csv
```

Folders are only created for output types that actually contain something.

## What it does for you

**Any plotting system.** `append_plot()` takes `ggplot2` objects (including
`patchwork` compositions), `lattice`/`trellis` objects, `grid` grobs and
`gtable`s, recorded base plots, and plain drawing functions:

```r
plt_hist <- function() hist(cars$speed, col = "steelblue")
append_plot(plt_hist, type = "s_wide")
```

To support anything else, define a `dr_draw()` method for it.

**Plots at their true size.** The package opens the graphics device itself
rather than going through `ggsave()`, so a plot declared as 16 × 12.35 cm is
exactly that — in the standalone file *and* in the catalogue. Nothing is ever
stretched; a plot too large for a catalogue page is scaled down
proportionally, and the page footer says by how much.

**A readable PDF catalogue.** One page per plot, its documentation typeset
above it, all pages the same size, with a contents page.

**A quiet console and a complete log.** Messages and warnings from other
packages are captured into `00_LOG.txt` instead of burying the progress
report; warnings are then summarised in a few lines. A single output that
cannot be written is recorded as `failed` in the manifest and the export
carries on.

**A codebook.** Appended data gets `00_codebook.csv`: one row per variable,
with its class, missingness, distinct values and a compact summary. Variable
labels come from `labels =` or from a column's `label` attribute, so
`haven`-read survey data documents itself.

**Auditable versions.**

```r
dr_versions(output_dir)   # what was exported when, with which notes
dr_manifest(output_dir)   # every file, its object and its checksum
dr_verify(output_dir)     # re-check the checksums after a copy or transfer
dr_log(output_dir)        # what the last export recorded
```

## Useful things to know

**Preview a plot at its export size** before committing to a full run:

```r
preview_plot(plt_cars, type = "m_wide")
```

**See what an export would write**, without writing it:

```r
save_outputs(output_dir, list_outputs, dry_run = TRUE)
```

**Change the defaults** in `gpars`:

```r
init_outputs(gpars = dr_gpars(
  formats   = list(plots = c(".png", ".svg")),
  csv_bom   = TRUE,                          # so Excel opens UTF-8 correctly
  catalogue = dr_catalogue_pars(paper = "a3", orientation = "landscape")
))

# or afterwards
list_outputs$gpars$formats$plots <- ".svg"
list_outputs$gpars$sizes$type$my_size <- list(units = "cm", H = 15, W = 20)
```

**Group outputs into subfolders**, which are numbered automatically:

```r
append_plot(plt_map, subfolder = "maps")
```

**Check reproducibility** by running the scripts in a clean R session:

```r
dr_check_code("04_code/01_cars.R")
```

Guard the export at the end of a script so the check does not re-export:

```r
if (!dr_is_rerun()) save_outputs(output_dir, list_outputs)
```

**Start a new script** pre-wired for this workflow:

```r
setup_code("01_02_analysis", project_title = "My project")
```

## Migrating from the standalone `documenteR.R` script

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
| `rerun_code = TRUE` re-ran the code *instead of* exporting | it is now a pre-flight check; the export follows |

An old `list_outputs.rds` can be read back with `as_outputs()`.

## Licence

MIT © PAQ Research
