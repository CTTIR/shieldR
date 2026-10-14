#' Explore synthetic logistics scenarios in Shiny
#'
#' A local scenario workbench with an editable five-leg graph, independent
#' policy comparisons and paired disruptions-disabled interventions. Teaching
#' presets have zero item-condition hazards and zero exposure. Every edited
#' scenario is validated by [shield_parameters()] before simulation.
#'
#' The primary endpoint is live arrival at Stage 2 by the administrative
#' horizon. Arrival-time summaries condition on observed arrivals. The coupled
#' policy is a heuristic, and neither the app nor its examples establish
#' application-specific validity. Simulations use [simulate_shield()];
#' comparisons use the documented package analysis functions, with 10,000
#' bootstrap resamples. The interactive workload is bounded; use scripts for
#' full study runs. Independent policy arms use distinct master seeds; paired
#' intervention arms use identical seeds and replication counts.
#'
#' Requires the optional packages `shiny`, `bslib`, `DT` and `ggplot2`.
#' The app uses local system fonts and makes no external data requests.
#' Uploads and results remain in the running R process unless downloaded.
#' Use `host = "127.0.0.1"` (the Shiny default) for local-only access.
#'
#' @param launch Logical. If `TRUE`, launch the app. If `FALSE`, return an app
#'   object for hosting or testing without starting a server.
#' @param ... Passed to [shiny::runApp()] when `launch = TRUE`.
#' @return A `shiny.appobj`, invisibly when launched.
#' @export
#' @examples
#' if (interactive()) {
#'   shield_app()
#' }
shield_app <- function(launch = TRUE, ...) {
  if (!is.logical(launch) || length(launch) != 1L || is.na(launch))
    stop("launch must be TRUE or FALSE", call. = FALSE)
  for (pkg in c("shiny", "bslib", "DT", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Install optional package '", pkg, "' to use shield_app().",
           call. = FALSE)
  }
  app <- shiny::shinyApp(ui = .shield_app_ui(), server = .shield_app_server)
  if (launch) {
    shiny::runApp(app, ...)
    return(invisible(app))
  }
  app
}

.shield_app_graph_columns <- function() {
  c("leg", "from", "to", "route", "travel_min", "exposure_weight",
    "vehicle_fraction")
}

.shield_app_default_graph <- function(kind = "balanced") {
  example_scenario(kind)$graph[.shield_app_graph_columns()]
}

.shield_app_defaults <- function() {
  list(design = "single", policy = "P3", policies = c("P0", "P3"),
       disruption_model = "static", N = 12L, horizon = 60L, n_rep = 8L,
       seed = 20260512L, slot = 1L, max_hold = 2L, reassess = 1L,
       misclass = 0, t_load = 1L, t_unload = 1L,
       h1 = 0, h2 = 0, h3 = 0, esc32 = 0, esc21 = 0,
       disruptions_enabled = FALSE, disruption_risk = 0, q = 0, att_delay = 0L,
       lambda = 0, atk_window = 1L, beta = 1)
}

.shield_app_collect <- function(input, graph) {
  settings <- .shield_app_defaults()
  for (nm in names(settings)) {
    if (!is.null(input[[nm]])) settings[[nm]] <- input[[nm]]
  }
  list(settings = settings, graph = graph)
}

.shield_app_configure <- function(snapshot) {
  s <- snapshot$settings
  bounded_integer <- function(value, label, low, high) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value != floor(value) || value < low || value > high)
      stop(label, " must be a whole number between ", low, " and ", high,
           ".", call. = FALSE)
  }
  bounded_integer(s$N, "Batch size", 1, 60)
  bounded_integer(s$horizon, "Horizon", 1, 240)
  bounded_integer(s$n_rep, "Replications", 2, 100)
  bounded_integer(s$seed, "Seed", 0, .Machine$integer.max - 1000)
  bounded_integer(s$slot, "Dispatch interval", 1, 20)
  bounded_integer(s$max_hold, "Maximum hold", 0, 20)
  if (s$max_hold %% s$slot != 0)
    stop("Maximum hold must be a multiple of the dispatch interval.", call. = FALSE)
  if (!s$design %in% c("single", "policies", "intervention"))
    stop("Choose a supported comparison design.", call. = FALSE)
  policies <- if (s$design == "policies") s$policies else s$policy
  if (!is.character(policies) || anyNA(policies) ||
      !length(policies) || anyDuplicated(policies) ||
      any(!policies %in% paste0("P", 0:3)))
    stop("Select valid, distinct policies.", call. = FALSE)
  if (s$design == "policies" && length(policies) < 2L)
    stop("Independent comparisons require at least two policies.", call. = FALSE)
  if (s$design != "policies" && length(policies) != 1L)
    stop("This design requires exactly one policy.", call. = FALSE)
  if (!identical(s$disruption_model, "static") && !identical(s$disruption_model, "adaptive"))
    stop("Choose static or adaptive disruption_model response.", call. = FALSE)
  n_arms <- if (s$design == "intervention") 2L else length(policies)
  effort <- s$N * s$horizon * s$n_rep * n_arms
  if (effort > 250000)
    stop("Interactive workload exceeds 250,000 item-minute steps. Reduce ",
         "batch size, horizon, replications or policy count; use an R script ",
         "for larger experiments.", call. = FALSE)
  args <- list(graph = snapshot$graph, N = s$N, horizon = s$horizon,
    slot = s$slot, departure_holds = seq(0, s$max_hold, by = s$slot),
    h = c(s$h1, s$h2, s$h3), esc32 = s$esc32, esc21 = s$esc21,
    reassess = s$reassess, misclass = s$misclass, t_load = s$t_load,
    t_unload = s$t_unload, disruption_risk = c(low = s$disruption_risk, high = s$disruption_risk),
    disruption_risk_level = "low", q = s$q, att_delay = s$att_delay,
    lambda = s$lambda, atk_window = s$atk_window, beta = s$beta,
    disruptions_enabled = if (s$design == "intervention") TRUE else s$disruptions_enabled)
  parameters <- do.call(shield_parameters, args)
  arms <- data.frame(arm = policies, policy = policies,
    disruption_model = s$disruption_model, seed = s$seed + seq_along(policies) - 1L,
    disruptions_enabled = parameters$disruptions_enabled, stringsAsFactors = FALSE)
  if (s$design == "intervention") {
    arms <- arms[rep(1, 2), ]
    arms$arm <- paste0(policies, c(" / disruptions", " / no disruptions"))
    arms$disruptions_enabled <- c(TRUE, FALSE)
  }
  list(parameters = parameters, arguments = args, settings = s,
       arms = arms, workload = effort, bootstrap_resamples = 10000L)
}

.shield_app_edit_graph <- function(graph, row, column, value) {
  if (length(row) != 1L || !is.numeric(row) || is.na(row) ||
      row != floor(row) || row < 1 || row > nrow(graph) ||
      !column %in% c("travel_min", "exposure_weight", "vehicle_fraction"))
    stop("Only route durations, exposure and vehicle fractions can be edited.",
         call. = FALSE)
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || !is.finite(value))
    stop("Enter one finite numeric value.", call. = FALSE)
  candidate <- graph
  if (column == "vehicle_fraction") {
    candidate[candidate$leg == candidate$leg[row], column] <- value
  } else candidate[row, column] <- value
  shield_parameters(candidate)
  candidate
}

.shield_app_analyze <- function(runs, config, B = 10000L) {
  summary <- do.call(rbind, lapply(names(runs), function(arm) {
    cbind(arm = arm, summarize_shield(runs[[arm]]), stringsAsFactors = FALSE)
  }))
  comparisons <- omnibus <- data.frame()
  if (config$settings$design == "intervention") {
    comparisons <- compare_intervention(runs[[1L]], runs[[2L]], B = B,
                                         seed = config$settings$seed)
  } else if (config$settings$design == "policies") {
    pairs <- utils::combn(seq_along(runs), 2L)
    comparisons <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(i) {
      a <- pairs[1L, i]; b <- pairs[2L, i]
      compare_policies(runs[[a]]$viable_stage2, runs[[b]]$viable_stage2,
        names(runs)[a], names(runs)[b], family = "interactive_viable_stage2",
        B = B, seed = config$settings$seed + i - 1L)
    }))
    comparisons$p_bh_log10 <- adjust_fdr(comparisons$p_raw_log10)
    comparisons$p_bh <- 10^comparisons$p_bh_log10
    if (length(runs) >= 3L) {
      omnibus <- compare_multiple(
        unlist(lapply(runs, function(x) x$viable_stage2), use.names = FALSE),
        rep(names(runs), lengths(lapply(runs, function(x) x$viable_stage2))),
        family = "interactive_viable_stage2")
    }
  }
  rownames(summary) <- NULL
  list(summary = summary, comparisons = comparisons, omnibus = omnibus)
}

.shield_app_run <- function(config, progress = function(value, detail) NULL) {
  runs <- vector("list", nrow(config$arms))
  names(runs) <- config$arms$arm
  for (i in seq_along(runs)) {
    a <- config$arms[i, ]
    progress((i - 1) / (length(runs) + 1), paste("Simulating", a$arm))
    runs[[i]] <- simulate_shield(config$parameters, policy = a$policy,
      disruption_model = a$disruption_model, n_rep = config$settings$n_rep, seed = a$seed,
      disruptions_enabled = a$disruptions_enabled)
  }
  progress(length(runs) / (length(runs) + 1), "Summarizing and bootstrapping")
  analysis <- .shield_app_analyze(runs, config, B = config$bootstrap_resamples)
  progress(1, "Complete")
  c(list(schema_version = 1L, config = config, runs = runs,
    provenance = list(created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      shieldR_version = as.character(utils::packageVersion("shieldR")),
      R_version = R.version.string,
      rng_kind = c("L'Ecuyer-CMRG", "Inversion", "Rejection"),
      endpoint = "Live arrival at Stage 2 by the administrative horizon",
      bootstrap_resamples = config$bootstrap_resamples,
      family = if (config$settings$design == "policies")
        "All selected pairwise policy contrasts for viable_stage2" else NA_character_,
      session_info = utils::sessionInfo())), analysis)
}

.shield_app_raw <- function(bundle) {
  do.call(rbind, lapply(names(bundle$runs), function(arm) {
    cbind(arm = arm, as.data.frame(bundle$runs[[arm]]), stringsAsFactors = FALSE)
  }))
}

.shield_app_replay <- function(bundle, file) {
  config <- bundle$config
  lines <- c("# Reproduce a shieldR interactive experiment.",
    "# Synthetic model outputs do not establish application-specific validity.",
    "library(shieldR)", "set.seed(20260512)",
    paste0("# Original shieldR version: ", bundle$provenance$shieldR_version),
    "arguments <-", utils::capture.output(dput(config$arguments)),
    "arms <-", utils::capture.output(dput(config$arms)),
    paste0("n_rep <- ", config$settings$n_rep, "L"),
    "parameters <- do.call(shield_parameters, arguments)",
    "runs <- lapply(seq_len(nrow(arms)), function(i) {",
    "  simulate_shield(parameters, policy = arms$policy[i],",
    "    disruption_model = arms$disruption_model[i], n_rep = n_rep, seed = arms$seed[i],",
    "    disruptions_enabled = arms$disruptions_enabled[i])", "})",
    "names(runs) <- arms$arm", "summaries <- lapply(runs, summarize_shield)",
    "saveRDS(list(parameters = parameters, arms = arms, runs = runs,",
    "  summaries = summaries, session_info = sessionInfo()),",
    "  file = 'shieldR_replayed.rds')")
  writeLines(lines, file)
  invisible(file)
}

.shield_app_plot <- function(bundle, kind = c("viability", "resources", "time")) {
  kind <- match.arg(kind)
  endpoints <- switch(kind, viability = paste0("viable_stage", 1:3),
    resources = paste0("util_L", 1:5), time = c("t_stage2_med", "t_stage3_med"))
  labels <- switch(kind,
    viability = c("Stage 1 arrival viable", "Stage 2 arrival viable (primary)",
                 "Stage 3 arrival viable"),
    resources = paste("Leg", 1:5),
    time = c("Stage 2: observed batch medians", "Stage 3: observed batch medians"))
  raw <- .shield_app_raw(bundle)
  d <- do.call(rbind, lapply(seq_along(endpoints), function(i) {
    data.frame(arm = raw$arm, value = raw[[endpoints[i]]],
               endpoint = labels[i], rep = raw$rep)
  }))
  d <- d[is.finite(d$value), ]
  d$endpoint <- factor(d$endpoint, levels = labels)
  d$arm <- factor(d$arm, levels = names(bundle$runs))
  # Deterministic horizontal offsets avoid changing the caller's RNG state.
  d$x <- as.integer(d$arm) + ((d$rep %% 11) - 5) / 55
  arm <- value <- x <- NULL
  p <- ggplot2::ggplot(d, ggplot2::aes(x = arm, y = value)) +
    ggplot2::geom_boxplot(width = .5, outlier.shape = NA, fill = "#e5e7eb",
                         colour = "#202020", na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(x = x), shape = 21, size = 1.5,
                        fill = "#ffffff", colour = "#454545", alpha = .65) +
    ggplot2::facet_wrap(~endpoint, nrow = 1, drop = FALSE,
      scales = if (kind == "time") "free_y" else "fixed") +
    ggplot2::labs(x = NULL, y = if (kind == "time") "Minutes" else
      if (kind == "resources") "Resource utilization" else "Proportion",
      caption = "Points: independent batch replications within each arm. Boxes: median and IQR.") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 25, hjust = 1),
      plot.caption = ggplot2::element_text(hjust = 0))
  if (kind == "viability") p <- p + ggplot2::scale_y_continuous(
    limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%"))
  p
}
