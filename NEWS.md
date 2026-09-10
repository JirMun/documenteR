# documenteR 0.9.0

First release as an R package. Previously a standalone script
(`04_code/documenteR.R`), which remains in place for existing analyses.

## Bug fixes carried over from the script

* `save_stats()` no longer fails with ``` `..2 (right)` must be a vector, not
  `NULL` ``` when a statistic has a `formula` but no documented `vars`. The
  formula-to-text substitution is now done by tokenising the formula rather
  than by a constructed `case_when()`, so it also handles `vars` passed as a
  list, unnamed `vars`, labels containing regex metacharacters, and names that
  are prefixes of one another.

* Exported CSV files no longer carry a phantom `...1` index column.
  `utils::write.csv()` writes row names by default as an unnamed first
  column; all delimited output is now written with `row.names = FALSE`.

* `stat_round` now means the number of decimal places. It was passed to
  `scales::comma()` as `accuracy = 1 + 10^(-stat_round)`, which rounded to
  the nearest 1.001 — so a proportion of `0.5` was formatted as `"0"`.

* The PDF plot catalogue no longer distorts plots. Every page previously had
  a different size (plot height plus header), so viewers rescaled each page
  by a different factor, and stacking with `ggpubr::ggarrange()` stretched
  the plot panel itself. See *The catalogue* below.

* `dr_version_name()` (the verbal version name) can no longer loop forever
  once the vocabulary is exhausted, and its uniqueness check no longer
  miscounts the available combinations.

* Object titles no longer come out quoted. `deparse(substitute(object_name))`
  was applied after `object_name` had been assigned, yielding `"\"my_plot\""`.

* Long or non-ASCII object names and subfolders no longer produce unopenable
  files: names are transliterated and sanitised, and paths that exceed the
  Windows limit are warned about before they are written.

## Plots work with any graphics system

* `append_plot()` accepts `ggplot2` objects (including `patchwork` and
  `ggarrange` compositions), `lattice`/`trellis` objects, `grid` grobs and
  `gtable`s, recorded base plots, and functions of no arguments that draw
  with base graphics. Extend it by defining a method for the new `dr_draw()`
  generic.

* Plots are written by opening a graphics device at the requested physical
  size and drawing on it (`dr_save_plot()`), not via `ggsave()`. A plot
  declared as 16 × 12.35 cm is exactly that size in every format, and the
  catalogue uses the same geometry as the standalone file.

* New formats: `.pdf`, `.eps` and `.tiff` for plots; `.csv2`, `.tsv`,
  `.parquet` and `.json` for data and tables. `ragg` and `svglite` are used
  when installed.

* `test_save()` is now `preview_plot()`, and can preview an
  already-appended plot by name.

## The catalogue

Rebuilt from scratch, with `grid` rather than stacked ggplot objects:

* One device, one page size for the whole document.
* Each plot drawn at its true physical size, centred, with its aspect ratio
  preserved. A plot too large for the page is scaled down proportionally and
  the footer records the percentage.
* Documentation typeset above the plot as a two-column definition list, with
  wrapping and hanging indents, and trimmed from the bottom if it would
  crowd out the plot.
* A contents page, page numbers and a project/version footer.
* Configurable via `dr_catalogue_pars()`: paper size, orientation, margins,
  font sizes, header share.
* `pdftools` and `ggpubr` are no longer needed.

## Quiet console, complete log

* Messages and warnings raised by other packages during `save_outputs()` are
  captured into `00_LOG.txt` rather than printed, and summarised in a few
  readable lines at the end. Stray `stdout` is captured too.
* An output that cannot be written is recorded as `failed` in the manifest
  and the export continues, instead of aborting partway through.
* Progress is reported with `cli`, with a progress bar per output type.
* `dr_log()` reads back the log of a saved version.

## New

* `00_MANIFEST.csv` / `.json`: one row per written file, with the object it
  came from, its documentation, its size and an MD5 checksum.
  `dr_manifest()` reads it; `dr_verify()` re-checks the checksums, which is
  worth doing after copying an export across a network share.
* `00_codebook.csv`: a variable-level codebook for all appended data, from
  the new `dr_codebook()`. Variable labels are taken from `labels =` or from
  a column's `label` attribute. This completes the `get_data_codebook()`
  placeholder left in the script.
* `dr_versions()` lists every exported version with its notes and name.
* `save_outputs(dry_run = TRUE)` reports exactly what would be written.
* `dr_check_code()` runs the analysis scripts in a clean R subprocess as a
  reproducibility check. `rerun_code = TRUE` now runs this check *before*
  exporting rather than instead of it, and `dr_is_rerun()` lets a script skip
  its own export during the check.
* `remove_outputs()` drops appended entries by name or type.
* `print()` and `summary()` methods for a collection.
* `dr_gpars()`, `dr_formats()`, `dr_folders()`, `dr_output_types()` make the
  defaults discoverable and overridable without editing nested lists by hand.
* Package options `documenteR.quiet`, `documenteR.verbose`,
  `documenteR.version_names` and `documenteR.max_path`.

## Internals

* An output collection is now an environment with class
  `documenteR_outputs`. `append_*()` adds to it in place, so the ergonomics
  of the original are kept without `eval(parse(text = ...))` and `<<-` —
  which in a package would have written to the package namespace rather than
  the global environment. It still behaves like the nested list it replaces:
  `x$plots`, `x$gpars$formats$plots <- ".svg"` and `length(x$data)` all work.
* `as_outputs()` converts a collection saved by the original script,
  including mapping `gpars$ggplot` to `gpars$sizes`.
* The tidyverse dependency is gone. `dplyr`, `tibble`, `tidyr`, `stringr`,
  `purrr`, `readr`, `scales`, `ggpubr` and `pdftools` are no longer needed;
  the package imports only `cli`, `rlang` and base R. Every file format and
  plotting system is an optional dependency, checked at the point of use with
  an actionable error. This also removes a class of breakage — the
  `case_when()` failure above was a consequence of a dplyr upgrade.
* File numbering is derived explicitly rather than through a chain of joins
  and mutates, and rows are ordered so that writing order, README order and
  a folder listing all agree.
* Text output is written through explicitly UTF-8 connections, so Czech
  characters survive on Windows and Linux alike. `gpars$csv_bom` adds a
  byte-order mark for Excel.
* Tests: around 200 assertions covering the fixes above, all four plotting
  paths, versioning, the manifest, the log and the catalogue.
