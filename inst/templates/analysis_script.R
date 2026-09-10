# {{SCRIPT_FILE}} -------------------------------------------------------------
# Created by documenteR::setup_code()

# Setup -----------------------------------------------------------------------

library(documenteR)
{{SOURCES}}

this_file <- "{{SCRIPT_FILE}}"

# Start collecting outputs. This creates `list_outputs`, which the append_*()
# calls below add to.
init_outputs()


# Body ------------------------------------------------------------------------

# Mark outputs as you produce them. Document them here, while you still
# remember what they are.

# data_example <- ...
# append_data(data_example,
#             desc   = "What this dataset is",
#             source = c(CZSO = "Census 2021"))

# plt_example <- ggplot2::ggplot(cars, ggplot2::aes(speed, dist)) +
#   ggplot2::geom_point()
# append_plot(plt_example,
#             desc = "What this plot shows",
#             vars = c(x = "Speed", y = "Braking distance"),
#             type = "m_wide")

# tab_example <- ...
# append_table(tab_example, desc = "What this table reports")

# stat_example <- mean(cars$speed)
# append_stats(stat_example,
#              type       = "float",
#              stat_round = 1,
#              formula    = "sum(speed)/n",
#              vars       = c(speed = "Speed", n = "Number of cars"),
#              text       = "Cars travelled at * mph on average.")


# Export ----------------------------------------------------------------------

output_dir <- {{OUTPUT_DIR}}

code_locations <- c(
  # Every script this analysis needs, in run order.
  file.path("04_code", paste0(this_file, ".R"))
)

# dr_is_rerun() is TRUE only when dr_check_code() is re-running this script as
# a reproducibility check, so the check does not itself write an export.
if (!dr_is_rerun()) {
  save_outputs(
    output_dir    = output_dir,
    output_list   = list_outputs,
    project_title = "{{PROJECT_TITLE}}",
    version_notes = "First upload",
    code_location = code_locations
  )
}
