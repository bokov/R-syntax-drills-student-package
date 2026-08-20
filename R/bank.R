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
  pool = "DRILLR_BANK_POOL_PATH",
  version = "DRILLR_BANK_VERSION"
)

.drillr_bank_version_columns <- c(
  "item_label",
  "event",
  "topic",
  "points",
  "starter_question",
  "question_hash"
)

drillr_bank_cache_root <- function(create = TRUE) {
  path <- file.path(tools::R_user_dir("drillr", "cache"), "bank")
  if (isTRUE(create) && !dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

drillr_manifest_bank_version <- function(manifest) {
  missing <- setdiff(.drillr_bank_version_columns, names(manifest))
  if (length(missing)) {
    stop(
      "Question manifest is missing required bank-version column(s): ",
      paste(missing, collapse = ", "), "."
    )
  }

  runtime <- manifest[
    manifest$event == "exercise_result" & manifest$points > 0,
    .drillr_bank_version_columns,
    drop = FALSE
  ]
  if (!nrow(runtime)) {
    stop("Question manifest contains no scored runtime exercises.")
  }
  if (anyDuplicated(runtime$item_label)) {
    stop("Question manifest contains duplicate scored item_label values.")
  }

  runtime <- runtime[order(runtime$item_label), , drop = FALSE]
  canonical <- data.frame(
    item_label = enc2utf8(as.character(runtime$item_label)),
    event = enc2utf8(as.character(runtime$event)),
    topic = enc2utf8(as.character(runtime$topic)),
    points = sprintf("%.15g", as.numeric(runtime$points)),
    starter_question = ifelse(runtime$starter_question %in% TRUE, "1", "0"),
    question_hash = enc2utf8(as.character(runtime$question_hash)),
    stringsAsFactors = FALSE
  )

  path <- tempfile("drillr-bank-version-")
  on.exit(unlink(path), add = TRUE)
  write.table(
    canonical,
    file = path,
    sep = "\t",
    quote = TRUE,
    row.names = FALSE,
    col.names = TRUE,
    na = "",
    eol = "\n",
    fileEncoding = "UTF-8"
  )

  computed <- paste0("md5-", unname(tools::md5sum(path)))
  if ("bank_version" %in% names(manifest)) {
    declared <- trimws(as.character(manifest$bank_version))
    declared <- unique(declared[!is.na(declared) & nzchar(declared)])
    if (length(declared) != 1L) {
      stop("Question manifest must contain exactly one non-empty bank_version.")
    }
    if (!identical(declared[[1]], computed)) {
      stop("Question manifest bank_version does not match its scored runtime content.")
    }
  }

  computed
}

drillr_read_bank_manifest <- function(path) {
  if (!file.exists(path)) stop("Question manifest does not exist: ", path)
  manifest <- read.csv(path, stringsAsFactors = FALSE, na.strings = "")
  version <- drillr_manifest_bank_version(manifest)
  attr(manifest, "bank_version") <- version
  manifest
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

drillr_validate_bank_pair <- function(manifest_path, pool_path, metadata = NULL) {
  manifest <- drillr_read_bank_manifest(manifest_path)
  expected <- as.character(manifest$item_label[
    manifest$event == "exercise_result" & manifest$points > 0
  ])
  actual <- drillr_pool_item_labels(pool_path)

  if (anyDuplicated(actual)) {
    stop("Runtime question pool contains duplicate exercise labels.")
  }
  if (!setequal(expected, actual) || length(expected) != length(actual)) {
    stop("Runtime question pool and question manifest do not contain the same scored items.")
  }

  if (!is.null(metadata)) {
    manifest_md5 <- unname(tools::md5sum(manifest_path))
    pool_md5 <- unname(tools::md5sum(pool_path))
    if (!identical(as.character(metadata$manifest_md5), manifest_md5)) {
      stop("Cached question manifest checksum does not match its metadata.")
    }
    if (!identical(as.character(metadata$pool_md5), pool_md5)) {
      stop("Cached runtime question pool checksum does not match its metadata.")
    }
    if (!identical(as.character(metadata$bank_version), attr(manifest, "bank_version"))) {
      stop("Cached bank version does not match its metadata.")
    }
  }

  list(
    manifest_path = normalizePath(manifest_path, mustWork = TRUE),
    pool_path = normalizePath(pool_path, mustWork = TRUE),
    bank_version = attr(manifest, "bank_version")
  )
}

drillr_bundled_bank <- function() {
  tutorial <- system.file("tutorials", "drills", package = "drillr")
  if (!nzchar(tutorial)) stop("Could not locate the installed Drillr tutorial.")
  bank <- drillr_validate_bank_pair(
    file.path(tutorial, "question_manifest.csv"),
    file.path(tutorial, "runtime_question_pool.Rmd")
  )
  bank$source <- "bundled"
  bank$updated <- FALSE
  bank$update_error <- ""
  bank
}

drillr_cache_bank_dir <- function(bank_version, cache_root = drillr_bank_cache_root()) {
  safe <- gsub("[^A-Za-z0-9._-]", "_", as.character(bank_version)[[1]])
  file.path(cache_root, safe)
}

drillr_cached_bank <- function(bank_version, cache_root = drillr_bank_cache_root()) {
  if (is.null(bank_version) || !nzchar(as.character(bank_version)[[1]])) return(NULL)
  dir <- drillr_cache_bank_dir(bank_version, cache_root)
  manifest_path <- file.path(dir, "question_manifest.csv")
  pool_path <- file.path(dir, "runtime_question_pool.Rmd")
  metadata_path <- file.path(dir, "metadata.rds")
  if (!all(file.exists(c(manifest_path, pool_path, metadata_path)))) return(NULL)

  metadata <- tryCatch(readRDS(metadata_path), error = function(e) NULL)
  if (is.null(metadata)) return(NULL)
  bank <- tryCatch(
    drillr_validate_bank_pair(manifest_path, pool_path, metadata),
    error = function(e) NULL
  )
  if (is.null(bank) || !identical(bank$bank_version, as.character(bank_version)[[1]])) {
    return(NULL)
  }
  bank$source <- "cache"
  bank$updated <- FALSE
  bank$update_error <- ""
  bank$cached_package_version <- as.character(metadata$package_version %||% "")
  bank
}

drillr_current_cache_bank <- function(cache_root = drillr_bank_cache_root()) {
  pointer <- file.path(cache_root, "current_version.txt")
  if (!file.exists(pointer)) return(NULL)
  version <- trimws(readLines(pointer, n = 1L, warn = FALSE))
  if (!length(version) || !nzchar(version[[1]])) return(NULL)
  drillr_cached_bank(version[[1]], cache_root)
}

drillr_active_bank_from_env <- function() {
  manifest_path <- Sys.getenv(.drillr_bank_env[["manifest"]], unset = "")
  pool_path <- Sys.getenv(.drillr_bank_env[["pool"]], unset = "")
  version <- Sys.getenv(.drillr_bank_env[["version"]], unset = "")
  if (!nzchar(manifest_path) || !nzchar(pool_path) || !nzchar(version)) return(NULL)
  bank <- tryCatch(
    drillr_validate_bank_pair(manifest_path, pool_path),
    error = function(e) NULL
  )
  if (is.null(bank) || !identical(bank$bank_version, version)) return(NULL)
  bank$source <- "environment"
  bank$updated <- FALSE
  bank$update_error <- ""
  bank
}

drillr_activate_bank <- function(bank) {
  stopifnot(
    is.list(bank),
    nzchar(bank$manifest_path),
    nzchar(bank$pool_path),
    nzchar(bank$bank_version)
  )
  do.call(
    Sys.setenv,
    stats::setNames(
      list(bank$manifest_path, bank$pool_path, bank$bank_version),
      unname(.drillr_bank_env)
    )
  )
  invisible(bank)
}

drillr_active_bank_is_set <- function() {
  !is.null(drillr_active_bank_from_env())
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

drillr_write_cache_pointer <- function(bank_version, cache_root) {
  dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("current-version-", tmpdir = cache_root)
  on.exit(unlink(tmp), add = TRUE)
  writeLines(bank_version, tmp, useBytes = TRUE)
  target <- file.path(cache_root, "current_version.txt")
  if (file.exists(target)) unlink(target)
  if (!file.rename(tmp, target)) stop("Could not update the Drillr bank cache pointer.")
  invisible(target)
}

drillr_install_cached_bank <- function(manifest_path, pool_path, cache_root) {
  validated <- drillr_validate_bank_pair(manifest_path, pool_path)
  target_dir <- drillr_cache_bank_dir(validated$bank_version, cache_root)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  target_manifest <- file.path(target_dir, "question_manifest.csv")
  target_pool <- file.path(target_dir, "runtime_question_pool.Rmd")
  if (!file.copy(manifest_path, target_manifest, overwrite = TRUE)) {
    stop("Could not cache the updated question manifest.")
  }
  if (!file.copy(pool_path, target_pool, overwrite = TRUE)) {
    stop("Could not cache the updated runtime question pool.")
  }

  metadata <- list(
    bank_version = validated$bank_version,
    manifest_md5 = unname(tools::md5sum(target_manifest)),
    pool_md5 = unname(tools::md5sum(target_pool)),
    cached_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    package_version = as.character(utils::packageVersion("drillr"))
  )
  saveRDS(metadata, file.path(target_dir, "metadata.rds"))
  drillr_write_cache_pointer(validated$bank_version, cache_root)

  bank <- drillr_validate_bank_pair(target_manifest, target_pool, metadata)
  bank$source <- "cache"
  bank$updated <- TRUE
  bank$update_error <- ""
  bank
}

drillr_runtime_bank <- function(cache_root = drillr_bank_cache_root()) {
  active <- drillr_active_bank_from_env()
  if (!is.null(active)) return(active)

  bundled <- drillr_bundled_bank()
  cached <- drillr_current_cache_bank(cache_root)
  package_version <- as.character(utils::packageVersion("drillr"))
  if (
    !is.null(cached) &&
    (
      identical(cached$bank_version, bundled$bank_version) ||
      identical(cached$cached_package_version, package_version)
    )
  ) {
    drillr_activate_bank(cached)
    return(cached)
  }

  drillr_activate_bank(bundled)
  bundled
}

drillr_resolve_bank_version <- function(
  expected_version,
  cache_root = drillr_bank_cache_root(),
  manifest_url = getOption("drillr.bank_manifest_url", .drillr_default_bank_manifest_url),
  pool_url = getOption("drillr.bank_pool_url", .drillr_default_bank_pool_url),
  downloader = drillr_download_asset
) {
  expected_version <- trimws(as.character(expected_version %||% ""))
  if (!nzchar(expected_version)) {
    stop("The service did not identify the required drill-bank version.")
  }

  active <- drillr_active_bank_from_env()
  if (!is.null(active) && identical(active$bank_version, expected_version)) return(active)

  cached <- drillr_cached_bank(expected_version, cache_root)
  if (!is.null(cached)) {
    drillr_write_cache_pointer(expected_version, cache_root)
    drillr_activate_bank(cached)
    return(cached)
  }

  bundled <- drillr_bundled_bank()
  if (identical(bundled$bank_version, expected_version)) {
    pointer <- file.path(cache_root, "current_version.txt")
    if (file.exists(pointer)) unlink(pointer)
    drillr_activate_bank(bundled)
    return(bundled)
  }

  incoming <- tempfile("drillr-bank-")
  dir.create(incoming)
  on.exit(unlink(incoming, recursive = TRUE), add = TRUE)

  manifest_one <- file.path(incoming, "manifest-one.csv")
  pool <- file.path(incoming, "runtime_question_pool.Rmd")
  manifest_two <- file.path(incoming, "manifest-two.csv")

  downloader(manifest_url, manifest_one, timeout_sec = 15)
  first <- drillr_read_bank_manifest(manifest_one)
  if (!identical(attr(first, "bank_version"), expected_version)) {
    stop(
      "The published drill bundle does not match the version required by the grading service."
    )
  }

  downloader(pool_url, pool, timeout_sec = 30)
  downloader(manifest_url, manifest_two, timeout_sec = 15)
  if (!identical(
    unname(tools::md5sum(manifest_one)),
    unname(tools::md5sum(manifest_two))
  )) {
    stop("The published drill bank changed while it was downloading; try again.")
  }

  second <- drillr_read_bank_manifest(manifest_two)
  if (!identical(attr(second, "bank_version"), expected_version)) {
    stop(
      "The published drill bundle changed before the download completed; try again."
    )
  }

  bank <- drillr_install_cached_bank(manifest_two, pool, cache_root)
  if (!identical(bank$bank_version, expected_version)) {
    stop("Downloaded drill-bank version did not match the service requirement.")
  }
  drillr_activate_bank(bank)
  bank
}

drillr_update_for_service <- function(body) {
  expected_version <- as.character(body$current_bank_version %||% "")
  tryCatch({
    bank <- drillr_resolve_bank_version(expected_version)
    drillr_invalidate_prerendered_html()
    list(
      ok = TRUE,
      bank = bank,
      message = paste(
        "New drill content was downloaded.",
        "Close and reopen Drillr to use the updated questions."
      )
    )
  }, error = function(e) {
    list(
      ok = FALSE,
      bank = NULL,
      message = paste(
        "Drillr needs a different question bank, but the update could not be downloaded:",
        conditionMessage(e)
      )
    )
  })
}

drillr_invalidate_prerendered_html <- function() {
  candidates <- unique(c(
    file.path(getwd(), "drills.html"),
    file.path(system.file("tutorials", "drills", package = "drillr"), "drills.html")
  ))
  existing <- candidates[file.exists(candidates)]
  removed <- character()
  for (path in existing) {
    if (isTRUE(suppressWarnings(unlink(path) == 0L)) && !file.exists(path)) {
      removed <- c(removed, path)
    }
  }
  invisible(removed)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
