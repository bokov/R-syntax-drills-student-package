# drillr

`drillr` runs the R syntax drills locally in RStudio while using the same central assignment service and adaptive queue as the hosted tutorial.

## Install and run

```r
pak::pak("bokov/R-syntax-drills-student-package")
drillr::drills()
```

After installation, the tutorial is also discoverable through RStudio's Tutorials pane because it is packaged under `inst/tutorials/drills/`.

The tutorial remembers the last student ID and optional name on the local computer. Exercise answers are not normally persisted between tutorial sessions. If a response cannot reach the grading service, Drillr temporarily stores the unsent event in a local outbox and retries it later with the same request ID. Use **Forget saved identity** on a shared computer; note that unsent outbox entries are separate from the remembered identity and remain until they are delivered or become obsolete.

## Student-safe package boundary

The package contains a frozen fallback runtime question pool and manifest plus the local client/UI assets. Current question content and the checker support required to grade it are published as student-safe generated assets in `bokov/R-syntax-drills`; the package does not contain the canonical authoring bank, instructor gradebook code, Google Sheet administration code, or Apps Script source.

The package talks to the Apps Script web-app endpoint and to the public student-safe release files in `bokov/R-syntax-drills`. It never contains or directly accesses the instructor Google Sheet ID.

## Question-bank updates

`drills.Rmd` is the stable local-player shell. The question content is the generated pair:

- `runtime_question_pool.Rmd`
- `question_manifest.csv`

The pair has a deterministic bank fingerprint. If the service requires a different bank, Drillr downloads the published student-safe pair, verifies that the pair matches the exact version requested by the service, and stores it under Drillr's standard per-user cache directory. It does **not** overwrite files inside the installed R package.

After a new bank is downloaded, close and reopen Drillr so learnr can render the new question pool. Package updates are therefore needed for changes to the Drillr engine or stable shell, not for routine question-bank releases.

The package's bundled pool and manifest are a frozen, self-contained bootstrap/fallback. They are validated for internal consistency by package tests but are intentionally not kept synchronized with the current authoring bank.

## Service configuration

The package ships with the production Apps Script `/exec` endpoint, course ID, queue size, and baseline curriculum used by the central service. Students do not need to configure a Google Sheet or Apps Script project. The package never contains the underlying Google Sheet ID.

For development/testing only, the endpoint and client-side curriculum settings can be overridden with `options(drillr.webhook_url = "...")`, `options(drillr.course_id = "...")`, `options(drillr.queue_size = ...)`, and `options(drillr.topic_priority = ...)`. `DRILLR_WEBHOOK_URL` is also accepted as an endpoint override. The public bank URLs can be overridden with `options(drillr.bank_manifest_url = "...")` and `options(drillr.bank_pool_url = "...")`.
