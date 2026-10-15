set.seed(20260512)

.app_skip <- function() {
  for (package in c("shiny", "bslib", "DT", "ggplot2"))
    testthat::skip_if_not_installed(package)
}

.app_snapshot <- function(...) {
  .shield_app_collect(c(list(N = 2L, n_rep = 2L, horizon = 20L), list(...)),
    .shield_app_default_graph())
}

test_that("app constructs a runnable, accessible UI without launching", {
  .app_skip()
  expect_s3_class(shield_app(launch = FALSE), "shiny.appobj")
  html <- as.character(.shield_app_ui())
  expect_match(html, "aria-live=\"polite\"", fixed = TRUE)
  expect_match(html, "chain-description", fixed = TRUE)
  expect_match(html, "Paired disruptions-disabled intervention", fixed = TRUE)
  expect_match(html, "Methods and help", fixed = TRUE)
  expect_error(shield_app(launch = NA), "TRUE or FALSE")
})

test_that("configuration validates design, workload and reconstructs parameters", {
  snapshot <- .app_snapshot(design = "policies", policies = c("P0", "P3"))
  config <- .shield_app_configure(snapshot)
  expect_s3_class(config$parameters, "shield_parameters")
  expect_identical(config$arms$seed, c(20260512L, 20260513L))
  expect_equal(config$bootstrap_resamples, 10000)
  expect_equal(config$parameters$graph$exposure_weight, rep(0, 15))
  expect_equal(config$parameters$h, c(0, 0, 0))
  snapshot$settings$policies <- "P0"
  expect_error(.shield_app_configure(snapshot), "at least two")
  snapshot$settings$policies <- c("P0", "P0")
  expect_error(.shield_app_configure(snapshot), "distinct")
  snapshot$settings$policies <- c("P0", "P3")
  snapshot$settings$N <- 60L
  snapshot$settings$n_rep <- 100L
  snapshot$settings$horizon <- 240L
  expect_error(.shield_app_configure(snapshot), "workload")
  snapshot <- .app_snapshot(slot = 2L, max_hold = 3L)
  expect_error(.shield_app_configure(snapshot), "multiple")
  snapshot <- .app_snapshot()
  snapshot$settings$seed <- .Machine$integer.max
  expect_error(.shield_app_configure(snapshot), "Seed")
  snapshot$settings$N <- NA_real_
  expect_error(.shield_app_configure(snapshot), "Batch size")
})

test_that("graph edits preserve topology and synchronize fleet by leg", {
  graph <- .shield_app_default_graph()
  changed <- .shield_app_edit_graph(graph, 1L, "vehicle_fraction", ".5")
  expect_equal(changed$vehicle_fraction[1:3], rep(.5, 3))
  expect_equal(changed$vehicle_fraction[4:15], rep(1, 12))
  changed <- .shield_app_edit_graph(graph, 1L, "travel_min", "3")
  expect_equal(changed$travel_min, c(3, rep(1, 14)))
  expect_error(.shield_app_edit_graph(graph, 1L, "from", "Collection"), "Only route")
  expect_error(.shield_app_edit_graph(graph, 1L, "travel_min", "0"), "domains")
  expect_error(.shield_app_edit_graph(graph, 1L, "exposure_weight", "2"), "domains")
  expect_error(.shield_app_edit_graph(graph, 1L, "travel_min", "oops"), "finite")
  expect_identical(graph, .shield_app_default_graph())
})

test_that("independent analysis uses distinct schedules and explicit FDR family", {
  config <- .shield_app_configure(.app_snapshot(design = "policies",
    policies = c("P0", "P1", "P3")))
  config$bootstrap_resamples <- 20L
  old_seed <- .Random.seed
  bundle <- .shield_app_run(config)
  expect_identical(.Random.seed, old_seed)
  expect_length(bundle$runs, 3L)
  expect_false(identical(bundle$runs[[1]]$replicate_seed,
                         bundle$runs[[2]]$replicate_seed))
  expect_equal(nrow(bundle$comparisons), 3L)
  expect_equal(bundle$comparisons$test, rep("Mann-Whitney U", 3))
  expect_equal(bundle$comparisons$p_bh_log10,
    adjust_fdr(bundle$comparisons$p_raw_log10))
  expect_equal(nrow(bundle$omnibus), 1L)
  expect_equal(bundle$runs[[1]], simulate_shield(config$parameters,
    policy = "P0", n_rep = 2L, seed = config$arms$seed[1]))
})

test_that("paired app design preserves exact intervention identities", {
  snapshot <- .app_snapshot(design = "intervention", policy = "P0", q = 1,
    disruption_risk = 1)
  snapshot$graph$exposure_weight <- 1
  config <- .shield_app_configure(snapshot)
  config$bootstrap_resamples <- 20L
  expect_equal(config$arms$seed, rep(20260512, 2))
  expect_identical(config$arms$disruptions_enabled, c(TRUE, FALSE))
  bundle <- .shield_app_run(config)
  expect_identical(bundle$runs[[1]]$replicate_seed,
                   bundle$runs[[2]]$replicate_seed)
  expect_equal(bundle$comparisons, compare_intervention(bundle$runs[[1]],
    bundle$runs[[2]], B = 20L))
  expect_equal(bundle$runs[[1]]$viable_stage2, c(0, 0))
  expect_equal(bundle$runs[[2]]$viable_stage2, c(1, 1))
  expect_equal(bundle$runs[[1]]$n_disruption_loss, c(2, 2))
  expect_equal(bundle$comparisons$mean_difference, 1)
  expect_true(bundle$comparisons$point_interval)
  expect_equal(nrow(bundle$omnibus), 0L)
})

test_that("replay export reconstructs the complete simulated batch results", {
  bundle <- .shield_app_run(.shield_app_configure(.app_snapshot()))
  path <- tempfile(fileext = ".R")
  on.exit(unlink(path), add = TRUE)
  .shield_app_replay(bundle, path)
  code <- parse(path)
  environment <- new.env(parent = globalenv())
  # The final expression persists a file; evaluate the replay computation only.
  for (expression in code[-length(code)]) eval(expression, environment)
  expect_equal(environment$runs, bundle$runs)
  expect_equal(environment$summaries, lapply(bundle$runs, summarize_shield))
})

test_that("server reports invalid input without presenting completed results", {
  .app_skip()
  shiny::testServer(.shield_app_server, {
    session$setInputs(N = 2L, n_rep = 2L, horizon = 20L, run = 1L)
    expect_false(is.null(state$bundle))
    session$setInputs(N = 60L, n_rep = 100L, horizon = 240L, run = 2L)
    expect_null(state$bundle)
    expect_match(state$error, "workload")
    expect_match(output$status$html, "Action needed")
  })
})

test_that("server graphs, outcomes and exports describe the accepted run", {
  .app_skip()
  shiny::testServer(.shield_app_server, {
    session$setInputs(N = 2L, n_rep = 2L, horizon = 20L)
    session$setInputs(graph_cell_edit = list(row = 1L, col = 4L, value = "2"))
    expect_equal(state$graph$travel_min[1], 2)
    session$setInputs(run = 1L)
    expect_null(state$error)
    expect_match(state$message, "Completed 2 batch")
    expect_equal(nrow(state$bundle$summary), 16L)
    expect_false(is.null(output$primary))
    expect_false(is.null(output$viability_plot))
    expect_false(is.null(output$resource_plot))
    expect_false(is.null(output$time_plot))
    bundle_path <- output$download_bundle
    expect_equal(readRDS(bundle_path), state$bundle)
    expect_equal(nrow(utils::read.csv(output$download_raw)), 2L)
    expect_equal(nrow(utils::read.csv(output$download_summary)), 16L)
    expect_equal(nrow(utils::read.csv(output$download_graph)), 15L)
    expect_true(length(parse(output$download_replay)) > 0)
    expect_true(file.size(output$download_png) > 1000)
    expect_true(file.size(output$download_pdf) > 1000)
    session$setInputs(horizon = 21L)
    expect_true(changed())
    expect_match(output$status$html, "Inputs changed")
    expect_equal(state$bundle$config$settings$horizon, 20L)
  })
})

test_that("server runs real independent and paired comparisons", {
  .app_skip()
  shiny::testServer(.shield_app_server, {
    session$setInputs(N = 2L, n_rep = 2L, horizon = 20L,
      design = "policies", policies = c("P0", "P3"), run = 1L)
    expect_null(state$error)
    expect_equal(state$bundle$provenance$bootstrap_resamples, 10000)
    expect_equal(state$bundle$comparisons$test, "Mann-Whitney U")
    expect_equal(nrow(utils::read.csv(output$download_comparisons)), 1L)
    session$setInputs(design = "intervention", policy = "P3", run = 2L)
    expect_null(state$error)
    expect_equal(state$bundle$comparisons$n_pairs, 2L)
    expect_equal(state$bundle$config$arms$seed, rep(20260512, 2))
    expect_match(output$comparison_note$html, "Paired contrast")
  })
})

test_that("graph upload and invalid cell edits have transactional behavior", {
  .app_skip()
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  graph <- .shield_app_default_graph("constrained")
  utils::write.csv(graph, path, row.names = FALSE)
  shiny::testServer(.shield_app_server, {
    session$setInputs(N = 2L)
    session$setInputs(graph_upload = data.frame(name = "graph.csv",
      datapath = path, size = file.size(path)))
    expect_equal(state$graph, graph)
    session$setInputs(graph_cell_edit = list(row = 1L, col = 4L, value = "-1"))
    expect_equal(state$graph, graph)
    expect_match(state$error, "domains")
    session$setInputs(graph_upload = data.frame(name = "large.csv",
      datapath = path, size = 70000))
    expect_match(state$error, "64 KB")
    expect_equal(state$graph, graph)
  })
})

test_that("missing arrivals remain missing in diagnostics and empty-time state", {
  .app_skip()
  shiny::testServer(.shield_app_server, {
    session$setInputs(N = 2L, n_rep = 2L, horizon = 1L, run = 1L)
    expect_null(state$error)
    rows <- state$bundle$summary
    time_row <- rows[rows$endpoint == "t_stage2_med", ]
    expect_equal(time_row$missing, 2L)
    expect_true(is.na(time_row$median))
    expect_equal(rows$median[rows$endpoint == "viable_stage2"], 0)
    expect_error(output$time_plot, "No defined Stage 2")
  })
})

test_that("PDF export recovers from an unusable Cairo runtime and cleans devices", {
  testthat::skip_if_not_installed("ggplot2")
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = c(2, 1, 3)),
    ggplot2::aes(x, y)) + ggplot2::geom_point()
  original_devices <- grDevices::dev.list()
  original_active <- grDevices::dev.cur()
  directory <- tempfile("shieldR-pdf-test-")
  dir.create(directory)
  on.exit({
    for (device in rev(setdiff(grDevices::dev.list(), original_devices)))
      grDevices::dev.off(device)
    if (original_active %in% grDevices::dev.list())
      grDevices::dev.set(original_active)
    unlink(directory, recursive = TRUE)
  }, add = TRUE)
  # Represent an existing caller-owned graphics device. A failed export must
  # neither close it nor draw its plot on that device.
  grDevices::pdf(file.path(directory, "existing.pdf"))
  caller_device <- grDevices::dev.cur()
  before <- grDevices::dev.list()
  for (failure in c("warning_after_open", "error", "no_device", "no_file")) {
    attempted <- FALSE
    attempted_path <- NULL
    cairo <- function(filename, ...) {
      attempted <<- TRUE
      attempted_path <<- filename
      if (failure == "warning_after_open") {
        grDevices::pdf(filename, ...)
        warning("failed to load cairo DLL", call. = FALSE)
      } else if (failure == "error") {
        stop("unable to load shared object", call. = FALSE)
      } else if (failure == "no_file") {
        grDevices::pdf(NULL, ...)
      }
      invisible(NULL)
    }
    destination <- file.path(directory, paste0(failure, ".pdf"))
    expect_silent(.shield_app_save_pdf(destination, plot,
      cairo_device = cairo, cairo_available = TRUE))
    expect_true(attempted)
    expect_gt(file.size(destination), 1000)
    bytes <- readBin(destination, "raw", n = file.size(destination))
    expect_identical(bytes[1:5], charToRaw("%PDF-"))
    expect_identical(tail(bytes, 6L), charToRaw("%%EOF\n"))
    expect_identical(grDevices::dev.list(), before)
    expect_identical(grDevices::dev.cur(), caller_device)
    expect_false(file.exists(attempted_path))
  }
})
