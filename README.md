# drillr

`drillr` runs the R syntax drills locally in RStudio while using the same central assignment service and adaptive queue as the hosted tutorial.

## Install and run

```r
pak::pak("bokov/R-syntax-drills-student-package")
drillr::drills()
```

After installation, the tutorial is also discoverable through RStudio's Tutorials pane because it is packaged under `inst/tutorials/drills/`.

The tutorial remembers only the last student ID and optional name on the local computer. Exercise answers are not persisted between tutorial sessions. Use **Forget saved identity** on a shared computer.

## Student-safe package boundary

The package contains only the generated runtime question pool, its manifest, the check code required to grade those questions, and client/UI assets. It does not contain the canonical authoring bank, instructor gradebook code, Google Sheet administration code, or Apps Script source.

The package talks only to the Apps Script web-app endpoint. It never contains or directly accesses the instructor Google Sheet ID.

## Updating the bundled question bank

For now, question-bank releases are intentionally manual. In `R-syntax-drills`, build and validate the current runtime player. Then copy these two generated files together into `inst/tutorials/drills/` in this repository:

- `runtime_question_pool.Rmd`
- `question_manifest.csv`

They are one release unit: server assignments are checked against the local manifest's labels, topics, points, and question hashes.

`tools/build_student_assets.R` performs the same copy/build operation when both repositories are available locally.

## Service configuration

The package ships with the production Apps Script `/exec` endpoint, course ID, queue size, and baseline curriculum used by the central service. Students do not need to configure a Google Sheet or Apps Script project. The package never contains the underlying Google Sheet ID.

For development/testing only, the endpoint and client-side curriculum settings can be overridden with `options(drillr.webhook_url = "...")`, `options(drillr.course_id = "...")`, `options(drillr.queue_size = ...)`, and `options(drillr.topic_priority = ...)`. `DRILLR_WEBHOOK_URL` is also accepted as an endpoint override.

## PR 1 scope

This first package PR intentionally keeps networking behavior equivalent to the working hosted client. Durable offline retries and the package/question-bank compatibility handshake are reserved for the follow-up reliability PR.
