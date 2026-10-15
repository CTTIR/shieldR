.shield_app_server <- function(input, output, session) {
  state <- shiny::reactiveValues(graph = .shield_app_default_graph(),
    bundle = NULL, snapshot = NULL, message = NULL, error = NULL)

  shiny::observeEvent(input$preset, {
    state$graph <- .shield_app_default_graph(input$preset)
    shiny::updateNumericInput(session, "horizon",
      value = if (input$preset == "constrained") 12 else 60)
    state$message <- "Teaching graph loaded. Review assumptions before running."
    state$error <- NULL
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$graph_upload, {
    tryCatch({
      upload <- input$graph_upload
      if (!is.null(upload$size) && upload$size > 65536)
        stop("Graph CSV must be at most 64 KB.", call. = FALSE)
      candidate <- utils::read.csv(upload$datapath, stringsAsFactors = FALSE,
                                   check.names = FALSE, nrows = 16L)
      shield_parameters(candidate)
      state$graph <- candidate
      state$message <- "Graph CSV imported and validated."
      state$error <- NULL
    }, error = function(e) state$error <- conditionMessage(e))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$graph_cell_edit, {
    tryCatch({
      edit <- input$graph_cell_edit
      columns <- .shield_app_graph_columns()
      if (length(edit$col) != 1L || edit$col < 0 || edit$col >= length(columns))
        stop("Choose a numeric graph cell.", call. = FALSE)
      state$graph <- .shield_app_edit_graph(state$graph, edit$row,
        columns[edit$col + 1L], edit$value)
      state$message <- "Graph updated and validated."
      state$error <- NULL
    }, error = function(e) state$error <- conditionMessage(e))
  }, ignoreInit = TRUE)

  output$graph <- DT::renderDT({
    DT::datatable(state$graph, rownames = FALSE, selection = "none",
      editable = list(target = "cell", disable = list(columns = 0:3)),
      options = list(pageLength = 15, dom = "t", scrollX = TRUE))
  }, server = FALSE)

  snapshot <- shiny::reactive(.shield_app_collect(input, state$graph))
  changed <- shiny::reactive(!is.null(state$bundle) &&
                              !identical(snapshot(), state$snapshot))
  shiny::observeEvent(input$run, {
    state$error <- NULL
    state$message <- NULL
    state$bundle <- NULL
    state$snapshot <- NULL
    tryCatch({
      current <- snapshot()
      config <- .shield_app_configure(current)
      bundle <- shiny::withProgress(message = "Running synthetic experiment",
        value = 0, {
          .shield_app_run(config, progress = function(value, detail) {
            shiny::setProgress(value = value, detail = detail)
          })
        })
      state$bundle <- bundle
      state$snapshot <- current
      state$message <- sprintf("Completed %d batch replications across %d arm(s).",
        config$settings$n_rep * nrow(config$arms), nrow(config$arms))
    }, error = function(e) state$error <- conditionMessage(e))
  })

  output$status <- shiny::renderUI({
    shiny::div(class = "shield-status",
      if (!is.null(state$error)) shiny::p(shiny::strong("Action needed: "), state$error),
      if (!is.null(state$message)) shiny::p(state$message),
      if (changed()) shiny::p(shiny::strong("Inputs changed. "),
        "Results and experiment downloads still describe the last completed run. Run again to apply current settings."),
      if (is.null(state$bundle) && is.null(state$error)) shiny::p(
        "Choose assumptions and select Run experiment to populate outcomes and exports."))
  })

  output$outcome_note <- shiny::renderUI({
    if (is.null(state$bundle)) return(shiny::p("Run an experiment to see batch outcomes."))
    s <- state$bundle$config$settings
    shiny::p(sprintf("Last completed experiment: %d items per batch; %d replications per arm; %d-minute horizon. Primary endpoint: arrival viable at Stage 2.",
      s$N, s$n_rep, s$horizon))
  })
  output$comparison_note <- shiny::renderUI({
    if (is.null(state$bundle)) return(shiny::p("Run an experiment to inspect comparisons."))
    design <- state$bundle$config$settings$design
    if (design == "single") return(shiny::p(
      "This is a single-policy experiment. Choose independent policy arms or a paired intervention to estimate a contrast."))
    if (design == "intervention") return(shiny::p(
      "Paired contrast: no disruptions minus disruptions, on the viability-proportion scale. The mean paired difference and 95% interval use 10,000 joint bootstrap resamples. This total intervention effect includes downstream and horizon effects."))
    shiny::p("Independent arms use distinct seeds. Positive Cliff's delta and median difference favour arm_x. Mann-Whitney U compares distributions. All displayed Stage 2 pairwise contrasts form one Benjamini-Hochberg FDR family. Intervals use 10,000 bootstrap resamples; point intervals are flagged. An omnibus test is shown for three or more arms.")
  })

  render_table <- function(get_data) DT::renderDT({
    shiny::req(state$bundle)
    DT::datatable(get_data(), rownames = FALSE, options = list(
      pageLength = 10, scrollX = TRUE, lengthMenu = c(10, 25, 50)))
  })
  output$primary <- render_table(function() {
    x <- state$bundle$summary
    x[x$endpoint %in% c("viable_stage1", "viable_stage2", "viable_stage3"), ]
  })
  output$diagnostics <- render_table(function() {
    x <- state$bundle$summary
    x[!x$endpoint %in% c("viable_stage1", "viable_stage2", "viable_stage3"), ]
  })
  output$comparisons <- render_table(function() state$bundle$comparisons)
  output$omnibus <- render_table(function() state$bundle$omnibus)
  output$raw <- render_table(function() .shield_app_raw(state$bundle))

  output$viability_plot <- shiny::renderPlot({
    shiny::req(state$bundle)
    .shield_app_plot(state$bundle, "viability")
  }, alt = "Viability-to-arrival proportions by arm and service level, with batch points, median and interquartile range.")
  output$resource_plot <- shiny::renderPlot({
    shiny::req(state$bundle)
    .shield_app_plot(state$bundle, "resources")
  }, alt = "Resource utilization distributions for the five legs and selected arms.")
  output$time_plot <- shiny::renderPlot({
    shiny::req(state$bundle)
    p <- .shield_app_plot(state$bundle, "time")
    shiny::validate(shiny::need(nrow(p$data) > 0,
      "No defined Stage 2 or Stage 3 arrival-time medians in this experiment. Inspect viability and the horizon."))
    p
  }, alt = "Distribution of defined batch median arrival times at Stages 2 and 3.")
  output$provenance <- shiny::renderPrint({
    shiny::req(state$bundle)
    p <- state$bundle$provenance
    print(p[setdiff(names(p), "session_info")])
    cat("\nArm seed schedule:\n")
    print(state$bundle$config$arms, row.names = FALSE)
  })

  filename <- function(suffix) paste0("shieldR_", suffix)
  output$download_graph <- shiny::downloadHandler(
    filename = function() filename("graph.csv"),
    content = function(file) utils::write.csv(state$graph, file, row.names = FALSE))
  output$download_bundle <- shiny::downloadHandler(
    filename = function() filename("experiment.rds"),
    content = function(file) {
      shiny::req(state$bundle)
      saveRDS(state$bundle, file)
    })
  output$download_replay <- shiny::downloadHandler(
    filename = function() filename("replay.R"),
    content = function(file) {
      shiny::req(state$bundle)
      .shield_app_replay(state$bundle, file)
    })
  for (kind in c("raw", "summary", "comparisons")) local({
    target <- kind
    output[[paste0("download_", target)]] <- shiny::downloadHandler(
      filename = function() filename(paste0(target, ".csv")),
      content = function(file) {
        shiny::req(state$bundle)
        data <- if (target == "raw") .shield_app_raw(state$bundle) else state$bundle[[target]]
        utils::write.csv(data, file, row.names = FALSE)
      })
  })
  for (extension in c("png", "pdf")) local({
    ext <- extension
    output[[paste0("download_", ext)]] <- shiny::downloadHandler(
      filename = function() filename(paste0("viability.", ext)),
      content = function(file) {
        shiny::req(state$bundle)
        plot <- .shield_app_plot(state$bundle, "viability")
        if (ext == "pdf") {
          .shield_app_save_pdf(file, plot)
        } else {
          ggplot2::ggsave(file, plot, device = "png", width = 12,
            height = 4.5, units = "in", dpi = 600)
        }
      })
  })
}

# Capability flags describe the R build, not whether Cairo's runtime libraries
# can actually load. Render in isolation before publishing a download file.
.shield_app_save_pdf <- function(file, plot, width = 12, height = 4.5,
    cairo_device = grDevices::cairo_pdf, cairo_available = capabilities("cairo")) {
  temporary <- tempfile("shieldR-pdf-", fileext = ".pdf")
  on.exit(unlink(temporary), add = TRUE)

  valid_pdf <- function(path) {
    size <- file.size(path)
    if (is.na(size) || size < 12) return(FALSE)
    connection <- file(path, open = "rb")
    on.exit(close(connection), add = TRUE)
    header <- readBin(connection, "raw", n = 5L)
    seek(connection, where = max(0, size - 64), origin = "start")
    trailer <- readBin(connection, "raw", n = 64L)
    eof <- charToRaw("%%EOF")
    identical(header, charToRaw("%PDF-")) &&
      any(vapply(seq_len(length(trailer) - length(eof) + 1L), function(i) {
        identical(trailer[i + seq_along(eof) - 1L], eof)
      }, logical(1)))
  }

  attempt <- function(device) {
    previous_devices <- grDevices::dev.list()
    previous_active <- grDevices::dev.cur()
    on.exit({
      added <- setdiff(grDevices::dev.list(), previous_devices)
      for (id in rev(added)) {
        try(grDevices::dev.off(which = id), silent = TRUE)
      }
      if (previous_active %in% grDevices::dev.list()) {
        grDevices::dev.set(previous_active)
      }
    }, add = TRUE)
    unlink(temporary)
    withCallingHandlers({
      device(temporary, width = width, height = height, onefile = TRUE)
      active <- grDevices::dev.cur()
      if (!active %in% setdiff(grDevices::dev.list(), previous_devices))
        stop("The PDF device did not open.", call. = FALSE)
      print(plot)
      grDevices::dev.off(which = active)
      if (!valid_pdf(temporary))
        stop("The PDF device did not produce a complete PDF.", call. = FALSE)
    }, warning = function(warning) {
      stop(conditionMessage(warning), call. = FALSE)
    })
    TRUE
  }

  devices <- if (isTRUE(cairo_available)) list(cairo_device, grDevices::pdf)
             else list(grDevices::pdf)
  failures <- character()
  for (device in devices) {
    result <- tryCatch(attempt(device), error = function(error) {
      conditionMessage(error)
    })
    if (isTRUE(result)) {
      if (!file.copy(temporary, file, overwrite = TRUE))
        stop("Could not save the completed PDF download.", call. = FALSE)
      return(invisible(file))
    }
    failures <- c(failures, result)
  }
  stop("Could not render a PDF download: ", paste(failures, collapse = "; "),
       call. = FALSE)
}
