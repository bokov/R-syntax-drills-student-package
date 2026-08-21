test_that("bundled bank has a deterministic runtime fingerprint", {
  bank <- drillr:::drillr_bundled_bank()
  expect_match(bank$bank_version, "^md5-[0-9a-f]{32}$")

  manifest <- drillr:::drillr_read_bank_manifest(bank$manifest_path)
  expect_identical(
    drillr:::drillr_manifest_bank_version(manifest[nrow(manifest):1, , drop = FALSE]),
    bank$bank_version
  )
})

test_that("a service-requested bank is validated and cached as one pair", {
  bundled <- drillr:::drillr_bundled_bank()
  manifest <- read.csv(bundled$manifest_path, stringsAsFactors = FALSE, na.strings = "")
  manifest$question_hash[[1]] <- paste0(manifest$question_hash[[1]], "x")
  manifest$bank_version <- NULL
  expected <- drillr:::drillr_manifest_bank_version(manifest)
  manifest$bank_version <- expected

  source_dir <- tempfile("bank-source-")
  cache_root <- tempfile("bank-cache-")
  dir.create(source_dir)
  dir.create(cache_root)
  manifest_source <- file.path(source_dir, "question_manifest.csv")
  pool_source <- file.path(source_dir, "runtime_question_pool.Rmd")
  write.csv(manifest, manifest_source, row.names = FALSE, na = "")
  file.copy(bundled$pool_path, pool_source)

  old_env <- Sys.getenv(c(
    "DRILLR_BANK_MANIFEST_PATH",
    "DRILLR_BANK_POOL_PATH",
    "DRILLR_BANK_VERSION"
  ), unset = NA_character_)
  on.exit({
    Sys.unsetenv(names(old_env))
    keep <- !is.na(old_env)
    if (any(keep)) do.call(Sys.setenv, as.list(old_env[keep]))
  }, add = TRUE)
  Sys.unsetenv(names(old_env))

  downloader <- function(url, path, timeout_sec) {
    source <- if (identical(url, "manifest")) manifest_source else pool_source
    if (!file.copy(source, path, overwrite = TRUE)) stop("copy failed")
    invisible(path)
  }

  bank <- drillr:::drillr_resolve_bank_version(
    expected,
    cache_root = cache_root,
    manifest_url = "manifest",
    pool_url = "pool",
    downloader = downloader
  )

  expect_identical(bank$bank_version, expected)
  expect_identical(bank$source, "cache")
  expect_true(bank$updated)
  expect_true(file.exists(file.path(cache_root, "current_version.txt")))
  expect_identical(
    drillr:::drillr_cached_bank(expected, cache_root)$bank_version,
    expected
  )
})

test_that("downloaded content must match the service-requested version", {
  bundled <- drillr:::drillr_bundled_bank()
  cache_root <- tempfile("bank-cache-")
  dir.create(cache_root)

  downloader <- function(url, path, timeout_sec) {
    source <- if (identical(url, "manifest")) bundled$manifest_path else bundled$pool_path
    file.copy(source, path, overwrite = TRUE)
    invisible(path)
  }

  expect_error(
    drillr:::drillr_resolve_bank_version(
      "md5-00000000000000000000000000000000",
      cache_root = cache_root,
      manifest_url = "manifest",
      pool_url = "pool",
      downloader = downloader
    ),
    "does not match the version required"
  )
})

test_that("corrupted cached files are not reused", {
  bundled <- drillr:::drillr_bundled_bank()
  cache_root <- tempfile("bank-cache-")
  dir.create(cache_root)
  bank <- drillr:::drillr_install_cached_bank(
    bundled$manifest_path,
    bundled$pool_path,
    cache_root
  )

  cat("\ncorruption\n", file = bank$pool_path, append = TRUE)
  expect_null(drillr:::drillr_cached_bank(bank$bank_version, cache_root))
})
