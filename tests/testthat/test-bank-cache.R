test_that("bundled bank loads as a usable local pair", {
  bank <- drillr:::drillr_bundled_bank()

  expect_identical(bank$source, "bundled")
  expect_true(file.exists(bank$manifest_path))
  expect_true(file.exists(bank$pool_path))
  expect_gt(length(bank$mismatch$usable), 0)
})

test_that("unchanged manifest does not download the runtime pool", {
  bundled <- drillr:::drillr_bundled_bank()
  cache_root <- tempfile("bank-cache-")
  dir.create(cache_root)
  requested <- character()

  downloader <- function(url, path, timeout_sec) {
    requested <<- c(requested, url)
    source <- switch(
      url,
      manifest = bundled$manifest_path,
      pool = bundled$pool_path,
      stop("unexpected URL")
    )
    if (!file.copy(source, path, overwrite = TRUE)) stop("copy failed")
    invisible(path)
  }

  bank <- drillr:::drillr_refresh_bank(
    cache_root = cache_root,
    manifest_url = "manifest",
    pool_url = "pool",
    downloader = downloader
  )

  expect_identical(requested, "manifest")
  expect_false(bank$updated)
  expect_identical(bank$source, "bundled")
})

test_that("changed manifest downloads and installs a matching runtime pool", {
  bundled <- drillr:::drillr_bundled_bank()
  manifest <- read.csv(
    bundled$manifest_path,
    stringsAsFactors = FALSE,
    na.strings = ""
  )
  manifest$release <- 1L

  source_dir <- tempfile("bank-source-")
  cache_root <- tempfile("bank-cache-")
  dir.create(source_dir)
  dir.create(cache_root)
  manifest_source <- file.path(source_dir, "question_manifest.csv")
  pool_source <- file.path(source_dir, "runtime_question_pool.Rmd")
  write.csv(manifest, manifest_source, row.names = FALSE, na = "")
  file.copy(bundled$pool_path, pool_source)

  requested <- character()
  downloader <- function(url, path, timeout_sec) {
    requested <<- c(requested, url)
    source <- if (identical(url, "manifest")) manifest_source else pool_source
    if (!file.copy(source, path, overwrite = TRUE)) stop("copy failed")
    invisible(path)
  }

  bank <- drillr:::drillr_refresh_bank(
    cache_root = cache_root,
    manifest_url = "manifest",
    pool_url = "pool",
    downloader = downloader
  )

  expect_identical(requested, c("manifest", "pool"))
  expect_true(bank$updated)
  expect_identical(bank$source, "cache")
  expect_true("release" %in% names(bank$manifest))
  expect_true(file.exists(file.path(cache_root, "current", "question_manifest.csv")))
  expect_true(file.exists(file.path(cache_root, "current", "runtime_question_pool.Rmd")))
})

test_that("forced refresh downloads both files even when manifest is unchanged", {
  bundled <- drillr:::drillr_bundled_bank()
  cache_root <- tempfile("bank-cache-")
  dir.create(cache_root)
  requested <- character()

  downloader <- function(url, path, timeout_sec) {
    requested <<- c(requested, url)
    source <- if (identical(url, "manifest")) bundled$manifest_path else bundled$pool_path
    if (!file.copy(source, path, overwrite = TRUE)) stop("copy failed")
    invisible(path)
  }

  bank <- drillr:::drillr_refresh_bank(
    force = TRUE,
    cache_root = cache_root,
    manifest_url = "manifest",
    pool_url = "pool",
    downloader = downloader
  )

  expect_identical(requested, c("manifest", "pool"))
  expect_true(bank$updated)
  expect_identical(bank$source, "cache")
})

test_that("manifest and Rmd mismatches are warned about and reduced to their intersection", {
  dir <- tempfile("bank-pair-")
  dir.create(dir)
  manifest_path <- file.path(dir, "question_manifest.csv")
  pool_path <- file.path(dir, "runtime_question_pool.Rmd")

  manifest <- data.frame(
    item_label = c("q1", "manifest_only"),
    event = c("exercise_result", "exercise_result"),
    topic = c("vectors", "vectors"),
    points = c(1, 1),
    starter_question = c(FALSE, FALSE),
    release = c(1L, 1L),
    stringsAsFactors = FALSE
  )
  write.csv(manifest, manifest_path, row.names = FALSE)
  writeLines(c(
    "```{r q1, exercise=TRUE}",
    "```",
    "```{r rmd_only, exercise=TRUE}",
    "```"
  ), pool_path)

  bank <- drillr:::drillr_bank_from_pair(
    manifest_path,
    pool_path,
    source = "test"
  )

  expect_identical(bank$manifest$item_label, "q1")
  expect_identical(bank$mismatch$manifest_only, "manifest_only")
  expect_identical(bank$mismatch$pool_only, "rmd_only")
  expect_match(bank$warning, "copy and paste this entire message into a Teams message", fixed = TRUE)
})
