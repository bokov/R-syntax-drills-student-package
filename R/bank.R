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

# Bank cache paths and readers ------------------------------------------------

#' Locate the runtime question-bank cache
#'
#' Returns Drillr's per-user cache directory for downloaded runtime-bank files,
#' optionally creating it before use.
#'
#' @param create If `TRUE`, create the cache directory recursively when needed.
#' @return A length-one character path to the runtime-bank cache root.
#' @details Called directly by `drillr_cached_bank()`,
#'   `drillr_install_bank_pair()`, and `drillr_refresh_bank()` through default
#'   arguments. It has no within-repo function dependencies.
drillr_bank_cache_root <- function(create = TRUE) {
  path <- file.path(tools::R_user_dir("drillr", "cache"), "runtime-bank")
  if (isTRUE(create) && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

#' Read a runtime question manifest
#'
#' Loads a manifest CSV from disk using the package's expected string and
#' missing-value conventions, failing immediately when the file is absent.
#'
#' @param path Path to a question-manifest CSV file.
#' @return A data frame containing the manifest rows and columns.
#' @details Called directly by `drillr_bank_from_pair()` and
#'   `drillr_refresh_bank()`. It has no within-repo function dependencies.
drillr_read_bank_manifest <- function(path) {
  if (!file.exists(path)) stop("Question manifest does not exist: ", path)
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "")
}

#' Compare two manifests by their parsed content
#'
#' Tests whether two manifest data frames are equal while ignoring attributes,
#' which determines whether the runtime question pool also needs downloading.
#'
#' @param old Previously available manifest data frame.
#' @param new Candidate replacement manifest data frame.
#' @return A single logical value indicating content equality.
#' @details Called directly by `drillr_refresh_bank()`. It has no within-repo
#'   function dependencies.
drillr_manifests_equal <- function(old, new) {
  isTRUE(all.equal(old, new, check.attributes = FALSE))
}

# Bank validation -------------------------------------------------------------

#' Extract scored exercise labels from a runtime question pool
#'
#' Reads an Rmd question pool and extracts chunk labels for exercise chunks so
#' the package can compare the executable pool with the manifest's scored IDs.
#'
#' @param path Path to a runtime question-pool Rmd file.
#' @return A character vector of exercise chunk labels, possibly empty.
#' @details Called directly by `drillr_bank_mismatch()`. It has no within-repo
#'   function dependencies.
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

#' Compare manifest IDs with runtime-pool exercise IDs
#'
#' Validates the manifest columns and duplicate IDs, then identifies scored
#' labels present only in the manifest, only in the Rmd pool, or in both. The
#' intersection is the usable question set when the two files disagree.
#'
#' @param manifest Parsed question-manifest data frame.
#' @param pool_path Path to the matching runtime question-pool Rmd file.
#' @return A list with `manifest_only`, `pool_only`, and `usable` character
#'   vectors.
#' @details Called directly by `drillr_bank_from_pair()`. Depends on
#'   `drillr_pool_item_labels()`.
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

#' Build the user-facing warning for a bank mismatch
#'
#' Converts manifest/pool ID differences into the warning displayed by the
#' tutorial while allowing Drillr to continue with the intersection of IDs.
#'
#' @param mismatch List returned by `drillr_bank_mismatch()`.
#' @return An empty string when there is no mismatch; otherwise a warning
#'   message describing manifest-only and Rmd-only item labels.
#' @details Called directly by `drillr_bank_from_pair()`. It has no within-repo
#'   function dependencies.
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

#' Construct a validated runtime-bank object from two files
#'
#' Reads and reconciles a manifest/pool pair, filters the manifest to usable
#' item labels, and packages the paths, content, mismatch information, source,
#' and update metadata consumed by the tutorial launcher and runtime.
#'
#' @param manifest_path Path to the question-manifest CSV.
#' @param pool_path Path to the runtime question-pool Rmd.
#' @param source Character label describing where the pair came from.
#' @param updated Whether this pair was newly installed during the current
#'   refresh.
#' @param notice Optional user-facing informational message.
#' @return A runtime-bank list containing normalized paths, filtered `manifest`,
#'   `mismatch`, `warning`, `notice`, `source`, and `updated`.
#' @details Called by `drillr_bundled_bank()`, `drillr_cached_bank()`,
#'   `drillr_active_bank_from_env()`, and `drillr_install_bank_pair()`, and
#'   directly by `test-bank-cache.R`. Depends on `drillr_read_bank_manifest()`,
#'   `drillr_bank_mismatch()`, and `drillr_bank_warning()`.
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

# Bank sources and activation -------------------------------------------------

#' Load the bank bundled with the installed package
#'
#' Locates the tutorial's packaged manifest and runtime pool and turns them into
#' a validated bank object used as the offline/default fallback.
#'
#' @return A validated runtime-bank list with `source = "bundled"`.
#' @details Called directly by `drillr_refresh_bank()` and
#'   `test-bank-cache.R`. Depends on `drillr_bank_from_pair()`.
drillr_bundled_bank <- function() {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  if (!nzchar(tutorial)) stop("Could not locate the installed Drillr tutorial.")
  drillr_bank_from_pair(
    file.path(tutorial, "question_manifest.csv"),
    file.path(tutorial, "runtime_question_pool.Rmd"),
    source = "bundled"
  )
}

#' Load the currently cached bank
#'
#' Reads the installed cache pair when both expected files exist and are valid;
#' invalid or incomplete cache state is treated as unavailable rather than
#' preventing Drillr from falling back to the bundled bank.
#'
#' @param cache_root Runtime-bank cache root.
#' @return A validated bank with `source = "cache"`, or `NULL` when no usable
#'   cache pair exists.
#' @details Called directly by `drillr_refresh_bank()`. Depends on
#'   `drillr_bank_cache_root()` through its default and
#'   `drillr_bank_from_pair()`.
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

#' Recover an already activated bank from environment variables
#'
#' Reconstructs the manifest/pool pair passed from a prelaunch process to the
#' tutorial process, returning `NULL` if either path is absent or unusable.
#'
#' @return A validated bank with `source = "environment"`, or `NULL` when the
#'   activated pair cannot be recovered.
#' @details Called directly by `drillr_runtime_bank()`. Depends on
#'   `drillr_bank_from_pair()` and the `.drillr_bank_env` variable names.
drillr_active_bank_from_env <- function() {
  manifest_path <- Sys.getenv(.drillr_bank_env[["manifest"]], unset = "")
  pool_path <- Sys.getenv(.drillr_bank_env[["pool"]], unset = "")
  if (!nzchar(manifest_path) || !nzchar(pool_path)) return(NULL)

  tryCatch(
    drillr_bank_from_pair(manifest_path, pool_path, source = "environment"),
    error = function(e) NULL
  )
}

#' Activate a runtime bank for the tutorial process
#'
#' Writes the selected manifest and pool paths to environment variables and
#' records whether the bank has been prepared for a subsequent tutorial launch.
#'
#' @param bank Validated runtime-bank list containing non-empty `manifest_path`
#'   and `pool_path` values.
#' @param prepared If `TRUE`, mark the environment as carrying a prelaunch bank
#'   that should be consumed once by the tutorial runtime.
#' @return Invisibly, `bank`.
#' @details Called directly by `drillr_runtime_bank()`. It uses the
#'   `.drillr_bank_env` and `.drillr_bank_prepared_env` constants but has no
#'   within-repo function dependencies.
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

# Downloading and cache installation -----------------------------------------

#' Download one runtime-bank asset
#'
#' Retrieves an asset over HTTP and writes the response bytes to the requested
#' path. This is the default downloader injected into bank refreshes.
#'
#' @param url Asset URL.
#' @param path Destination file path.
#' @param timeout_sec Request timeout in seconds.
#' @return Invisibly, `path` after the response body has been written.
#' @details Used as the default `downloader` by `drillr_refresh_bank()`. It has
#'   no within-repo function dependencies.
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

#' Install a validated manifest/pool pair into the cache
#'
#' Validates incoming files, stages copies together, swaps them into the
#' `current` cache directory with rollback on failure, and returns the installed
#' pair as an updated runtime-bank object.
#'
#' @param manifest_path Path to the incoming question-manifest CSV.
#' @param pool_path Path to the incoming runtime question-pool Rmd.
#' @param cache_root Runtime-bank cache root.
#' @return A validated cached bank with `updated = TRUE`.
#' @details Called directly by `drillr_refresh_bank()`. Depends on
#'   `drillr_bank_cache_root()` through its default and
#'   `drillr_bank_from_pair()` for both pre-install validation and the returned
#'   cache object.
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

#' Refresh the locally available runtime bank from GitHub
#'
#' Chooses the cached or bundled bank as the current fallback, downloads the
#' remote manifest, and downloads/installs the matching pool only when the
#' manifest changed or a forced refresh was requested. Network failures return
#' the current local bank with a notice instead of preventing tutorial launch.
#'
#' @param force If `TRUE`, treat the bank as changed even when the downloaded
#'   manifest matches the local manifest.
#' @param cache_root Runtime-bank cache root.
#' @param manifest_url URL for the published question manifest.
#' @param pool_url URL for the published runtime question pool.
#' @param downloader Function accepting `url`, `path`, and `timeout_sec`; tests
#'   inject local-copy implementations to exercise refresh behavior offline.
#' @return A validated runtime-bank list representing the current bundled,
#'   cached, or newly installed pair, with `notice`/`updated` reflecting the
#'   refresh outcome.
#' @details Called directly by `drillr_runtime_bank()` and by
#'   `test-bank-cache.R`. Depends on `drillr_bundled_bank()`,
#'   `drillr_cached_bank()`, `%||%`, `drillr_read_bank_manifest()`,
#'   `drillr_manifests_equal()`, `drillr_download_asset()` through its default,
#'   and `drillr_install_bank_pair()`.
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

#' Select and activate the runtime bank for a Drillr process
#'
#' Reuses a once-prepared bank passed through environment variables when the
#' tutorial process starts; otherwise refreshes the local bank and activates the
#' selected pair for the current or subsequent process.
#'
#' @param force Passed to `drillr_refresh_bank()` to force a remote pool refresh.
#' @param prelaunch If `TRUE`, mark the selected bank as prepared so the launched
#'   tutorial process can consume the activated paths once.
#' @return The selected validated runtime-bank list.
#' @details Called by exported `drills()` before `learnr::run_tutorial()` and by
#'   the setup chunk of `inst/tutorials/drills/drills.Rmd`. Depends on
#'   `drillr_active_bank_from_env()`, `drillr_refresh_bank()`, and
#'   `drillr_activate_bank()`.
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

# Utility helpers -------------------------------------------------------------

#' Substitute a fallback for a null or empty value
#'
#' Provides the package-internal null-coalescing operation used to select a
#' cached bank when available. Equivalent definitions also appear in other
#' package source files, so the final namespace binding is intentionally
#' interchangeable with those copies.
#'
#' @param x Value to return unless it is `NULL` or length zero.
#' @param y Fallback value.
#' @return `y` when `x` is `NULL` or empty; otherwise `x`.
#' @details Used directly by `drillr_refresh_bank()` in this file.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
