.drillr_default_bank_manifest_url <- paste0(
  "https://raw.githubusercontent.com/bokov/R-syntax-drills/main/",
  "student-assets/question_manifest.csv"
)

.drillr_default_bank_pool_url <- paste0(
  "https://raw.githubusercontent.com/bokov/R-syntax-drills/main/",
  "student-assets/runtime_question_pool.Rmd"
)

.drillr_bank_env <- c(
  manifest = "DRILLR_BANK_MANIFEST_PATH",
  pool = "DRILLR_BANK_POOL_PATH"
)

.drillr_bank_prepared_env <- "DRILLR_BANK_PREPARED"

drillr_bank_cache_root <- function(create = TRUE) {
  path <- file.path(tools::R_user_dir("drillr", "cache"), "runtime-bank")
  if (isTRUE(create) && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

drillr_read_bank_manifest <- function(path) {
  if (!file.exists(path)) stop("Question manifest does not exist: ", path)
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "")
}

drillr_manifests_equal <- function(old, new) {
  isTRUE(all.equal(old, new, check.attributes = FALSE))
}

drillr_pool_item_labels <- function(path) {
  if (!file.exists(path)) stop("Runtime question pool does not exist: ", path)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  pattern <- "^```\\{r[[:space:]]+([^,}[:space:]]+).*exercise[[:space:]]*=[[:space:]]*TRUE"
  matches <- regexec(pattern, lines, perl = TRUE)
  pieces <- regmatches(lines, matches)
  pieces <- pieces[lengths(pieces) > 1L]
  if (!length(pieces)) return(character())
  vapply(pieces, function(x) x[[2]], character(1))
}

drillr_bank_mismatch <- function(manifest, pool_path) {
  required <- c("item_label", "event", "points")
  missing <- setdiff(required, names(manifest))
  if (length(missing)) {
    stop(
      "Question manifest is missing required column(s): ",
      paste(missing, collapse = ", "), "."
    )
  }

  manifest_labels <- as.character(manifest$item_label[
    manifest$event == "exercise_result" & manifest$points > 0
  ])
  pool_labels <- drillr_pool_item_labels(pool_path)

  if (anyDuplicated(manifest_labels)) {
    stop("Question manifest contains duplicate scored item_label values.")
  }
  if (anyDuplicated(pool_labels)) {
    stop("Runtime question pool contains duplicate exercise labels.")
  }

  list(
    manifest_only = setdiff(manifest_labels, pool_labels),
    pool_only = setdiff(pool_labels, manifest_labels),
    usable = intersect(manifest_labels, pool_labels)
  )
}

drillr_bank_warning <- function(mismatch) {
  if (!length(mismatch$manifest_only) && !length(mismatch$pool_only)) return("")

  manifest_only <- if (length(mismatch$manifest_only)) {
    paste(mismatch$manifest_only, collapse = ", ")
  } else {
    "none"
  }
  pool_only <- if (length(mismatch$pool_only)) {
    paste(mismatch$pool_only, collapse = ", ")
  } else {
    "none"
  }

  paste0(
    "Drillr content warning - please copy and paste this entire message into a Teams message ",
    "to your course instructor. The question manifest and drill file disagree. ",
    "Manifest-only item_label(s): ", manifest_only, ". ",
    "Rmd-only item_label(s): ", pool_only, ". ",
    "Drillr will keep going using only item_label(s) present in both files."
  )
}

drillr_bank_from_pair <- function(
  manifest_path,
  pool_path,
  source,
  updated = FALSE,
  notice = ""
) {
  manifest <- drillr_read_bank_manifest(manifest_path)
  mismatch <- drillr_bank_mismatch(manifest, pool_path)

  list(
    manifest_path = normalizePath(manifest_path, mustWork = TRUE),
    pool_path = normalizePath(pool_path, mustWork = TRUE),
    manifest = manifest[manifest$item_label %in% mismatch$usable, , drop = FALSE],
    mismatch = mismatch,
    warning = drillr_bank_warning(mismatch),
    notice = as.character(notice),
    source = source,
    updated = isTRUE(updated)
  )
}

drillr_bundled_bank <- function() {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  if (!nzchar(tutorial)) stop("Could not locate the installed Drillr tutorial.")
  drillr_bank_from_pair(
    file.path(tutorial, "question_manifest.csv"),
    file.path(tutorial, "runtime_question_pool.Rmd"),
    source = "bundled"
  )
}

drillr_cached_bank <- function(cache_root = drillr_bank_cache_root()) {
  dir <- file.path(cache_root, "current")
  manifest_path <- file.path(dir, "question_manifest.csv")
  pool_path <- file.path(dir, "runtime_question_pool.Rmd")
  if (!all(file.exists(c(manifest_path, pool_path)))) return(NULL)

  tryCatch(
    drillr_bank_from_pair(manifest_path, pool_path, source = "cache"),
    error = function(e) NULL
  )
}

drillr_active_bank_from_env <- function() {
  manifest_path <- Sys.getenv(.drillr_bank_env[["manifest"]], unset = "")
  pool_path <- Sys.getenv(.drillr_bank_env[["pool"]], unset = "")
  if (!nzchar(manifest_path) || !nzchar(pool_path)) return(NULL)

  tryCatch(
    drillr_bank_from_pair(manifest_path, pool_path, source = "environment"),
    error = function(e) NULL
  )
}

drillr_activate_bank <- function(bank, prepared = FALSE) {
  stopifnot(
    is.list(bank),
    nzchar(bank$manifest_path),
    nzchar(bank$pool_path)
  )
  do.call(
    Sys.setenv,
    stats::setNames(
      list(bank$manifest_path, bank$pool_path),
      unname(.drillr_bank_env)
    )
  )
  if (isTRUE(prepared)) {
    Sys.setenv(DRILLR_BANK_PREPARED = "1")
  } else {
    Sys.unsetenv(.drillr_bank_prepared_env)
  }
  invisible(bank)
}

drillr_download_asset <- function(url, path, timeout_sec = 15) {
  response <- httr2::request(url) |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_perform()
  bytes <- httr2::resp_body_raw(response)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(bytes, con)
  invisible(path)
}

drillr_install_bank_pair <- function(
  manifest_path,
  pool_path,
  cache_root = drillr_bank_cache_root()
) {
  # Validate before replacing anything. ID mismatches are intentionally nonfatal;
  # malformed/unreadable files are not installed.
  drillr_bank_from_pair(manifest_path, pool_path, source = "incoming")

  dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
  incoming <- tempfile("incoming-", tmpdir = cache_root)
  dir.create(incoming)
  on.exit(unlink(incoming, recursive = TRUE), add = TRUE)

  if (!file.copy(manifest_path, file.path(incoming, "question_manifest.csv"))) {
    stop("Could not stage the updated question manifest.")
  }
  if (!file.copy(pool_path, file.path(incoming, "runtime_question_pool.Rmd"))) {
    stop("Could not stage the updated runtime question pool.")
  }

  current <- file.path(cache_root, "current")
  backup <- tempfile("previous-", tmpdir = cache_root)
  had_current <- dir.exists(current)

  if (had_current && !file.rename(current, backup)) {
    stop("Could not move the previous cached drill files aside.")
  }

  installed <- file.rename(incoming, current)
  if (!installed) {
    if (had_current && dir.exists(backup)) file.rename(backup, current)
    stop("Could not install the updated drill files.")
  }

  if (dir.exists(backup)) unlink(backup, recursive = TRUE)
  drillr_bank_from_pair(current |> file.path("question_manifest.csv"),
                        current |> file.path("runtime_question_pool.Rmd"),
                        source = "cache",
                        updated = TRUE)
}

drillr_refresh_bank <- function(
  force = FALSE,
  cache_root = drillr_bank_cache_root(),
  manifest_url = getOption(
    "drillr.bank_manifest_url",
    .drillr_default_bank_manifest_url
  ),
  pool_url = getOption(
    "drillr.bank_pool_url",
    .drillr_default_bank_pool_url
  ),
  downloader = drillr_download_asset
) {
  bundled <- drillr_bundled_bank()
  cached <- drillr_cached_bank(cache_root)
  current <- cached %||% bundled

  incoming <- tempfile("drillr-bank-")
  dir.create(incoming)
  on.exit(unlink(incoming, recursive = TRUE), add = TRUE)

  remote_manifest_path <- file.path(incoming, "question_manifest.csv")
  manifest_result <- tryCatch({
    downloader(manifest_url, remote_manifest_path, timeout_sec = 15)
    NULL
  }, error = function(e) e)

  if (inherits(manifest_result, "error")) {
    current$notice <- paste(
      "Drillr could not check GitHub for updated questions and is using its current local copy:",
      conditionMessage(manifest_result)
    )
    current$updated <- FALSE
    return(current)
  }

  remote_manifest <- drillr_read_bank_manifest(remote_manifest_path)
  local_manifest <- tryCatch(
    drillr_read_bank_manifest(current$manifest_path),
    error = function(e) NULL
  )

  changed <- isTRUE(force) ||
    is.null(local_manifest) ||
    !drillr_manifests_equal(local_manifest, remote_manifest)

  if (!changed) {
    current$updated <- FALSE
    current$notice <- ""
    return(current)
  }

  remote_pool_path <- file.path(incoming, "runtime_question_pool.Rmd")
  pool_result <- tryCatch({
    downloader(pool_url, remote_pool_path, timeout_sec = 30)
    NULL
  }, error = function(e) e)

  if (inherits(pool_result, "error")) {
    current$notice <- paste(
      "Drillr found an updated question manifest but could not download the matching drill file; ",
      "it is using its current local copy:",
      conditionMessage(pool_result)
    )
    current$updated <- FALSE
    return(current)
  }

  drillr_install_bank_pair(
    remote_manifest_path,
    remote_pool_path,
    cache_root = cache_root
  )
}

drillr_runtime_bank <- function(force = FALSE, prelaunch = FALSE) {
  if (
    !isTRUE(prelaunch) &&
    identical(Sys.getenv(.drillr_bank_prepared_env, unset = ""), "1")
  ) {
    Sys.unsetenv(.drillr_bank_prepared_env)
    active <- drillr_active_bank_from_env()
    if (!is.null(active)) return(active)
  }

  bank <- drillr_refresh_bank(force = force)
  drillr_activate_bank(bank, prepared = prelaunch)
  bank
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
