library(targets)
library(tarchetypes)
library(crew)

list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()

# Set target-specific options such as packages.
tar_option_set(
  error = "continue",
  workspace_on_error = TRUE,
  format = "qs",
  controller = crew_controller_local(worker = 2)
)

# End this file with a list of target objects.
list(

  # # Import your file from custom (shared) location, and preprocess them
  # tar_target(
  #   db_raw_path,
  #   file.path(get_input_data_path("db_raw.csv")),
  #   format = "file"
  # ),
  #
  # # Use {qs} in {targets} to save space and time in save/load objects
  # tar_target(db_raw, import_data(db_raw_path), format = "qs"),
  # tar_target(db, preprocess(db_raw), format = "qs"),
  #
  #
  # # Call your custom functions as needed.
  # tar_target(relevantResult, relevant_computation(db), format = "qs"),
  #
  # # compile yor report
  # tar_render(report, here::here("reports/report.Rmd")),
  #
  #
  # # Decide what to share with other, and do it in a standard RDS format
  # tar_target(
  #   objectToShare,
  #   list(
  #     relevant_result = relevantResult
  #   )
  # ),
  # tar_target(
  #   shareOutput,
  #   share_objects(objectToShare),
  #   format = "file",
  #   pattern = map(objectToShare)
  # )
)
