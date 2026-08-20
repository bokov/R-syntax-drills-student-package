args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: Rscript tools/build_student_assets.R /path/to/R-syntax-drills")
}

authoring_root <- normalizePath(args[[1]], mustWork = TRUE)
student_root <- normalizePath(".", mustWork = TRUE)
out_dir <- file.path(student_root, "inst", "tutorials", "drills")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_dir <- normalizePath(out_dir, mustWork = TRUE)

source(file.path(authoring_root, "R", "question_manifest.R"))
source(file.path(authoring_root, "R", "assignment_storage.R"))
source(file.path(authoring_root, "R", "player_builder.R"))

old_wd <- setwd(authoring_root)
on.exit(setwd(old_wd), add = TRUE)

# This build step needs only the curriculum shape, not the Sheet ID or webhook.
APP_CONFIG <- list(
  queue_size = 10L,
  topic_priority = c("vector_creation", "vector_indexing")
)

build_player_assets(
  root = ".",
  config = APP_CONFIG,
  pool_output = file.path(out_dir, "runtime_question_pool.Rmd"),
  manifest_output = file.path(out_dir, "question_manifest.csv")
)

message("Copied generated student-safe pool and manifest into ", out_dir, ".")
