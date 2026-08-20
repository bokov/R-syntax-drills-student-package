args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: Rscript tools/build_student_assets.R /path/to/R-syntax-drills")
}

authoring_root <- normalizePath(args[[1]], mustWork = TRUE)
student_root <- normalizePath(".", mustWork = TRUE)
source_dir <- file.path(authoring_root, "student-assets")
out_dir <- file.path(student_root, "inst", "tutorials", "drills")

pool_source <- file.path(source_dir, "runtime_question_pool.Rmd")
manifest_source <- file.path(source_dir, "question_manifest.csv")
if (!all(file.exists(c(pool_source, manifest_source)))) {
  stop("Authoring checkout does not contain the published student-assets bundle.")
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.copy(
  pool_source,
  file.path(out_dir, "runtime_question_pool.Rmd"),
  overwrite = TRUE
)) {
  stop("Could not copy runtime_question_pool.Rmd.")
}

# PR1 shipped before bank_version was added to the published manifest. Keep the
# bundled file diff-small; Drillr computes the same fingerprint from the six
# canonical columns and accepts downloaded manifests with bank_version present.
manifest <- read.csv(manifest_source, stringsAsFactors = FALSE, na.strings = "")
manifest$bank_version <- NULL
write.csv(
  manifest,
  file.path(out_dir, "question_manifest.csv"),
  row.names = FALSE,
  na = ""
)

message("Copied the published student-safe pool and manifest into ", out_dir, ".")
