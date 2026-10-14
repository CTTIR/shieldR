with_shield_rng <- function(seed, code) {
  check_scalar(seed, "seed", 0, integer = TRUE)
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(seed)
  force(code)
}

#' Simulate independent batch replications
#'
#' Each row represents one batch. All items enter at time zero. The engine
#' advances in one-minute intervals and preserves the original event ordering.
#' P0 uses initial true class and a fixed route; P1 uses noisy reassessment;
#' P2 updates route beliefs; P3 couples reassessment, route, destination and
#' departure holds. P3 knows the specified disruption_model response model and uses an
#' entropy-regularized heuristic. It does not solve an equilibrium.
#'
#' The master seed creates a per-replication seed schedule. Item, policy
#' and disruption streams are separated using L'Ecuyer-CMRG. Item draws are
#' indexed by item and minute. The caller's RNG kind and seed are restored.
#' Equal seeds and equal replication counts support an disruptions-disabled pair.
#' Use distinct seeds for independent policy arms.
#'
#' @param parameters Object created by [shield_parameters()], optionally
#'   configured with [with_decision_controls()] for a separate experiment.
#' @param policy One of P0, P1, P2 or P3.
#' @param disruption_model Either `static` or `adaptive`.
#' @param n_rep Positive integer replication count. The package default is
#'   deliberately small; SHIELD publication runs require 10,000 replications.
#' @param seed Integer seed; default 20260512.
#' @param disruptions_enabled Optional logical override of the scenario setting.
#' @return A `shield_run` data frame with replication identifiers and seeds,
#'   viability-to-arrival proportions at Stages 1--3, conditional arrival-time
#'   medians, disruptions, dispatches, holds, route counts and resource utilization.
#'   Scenario parameters, software version and RNG kind are stored as attributes.
#'   For the legacy mode the primary endpoint is `viable_stage2`. The opt-in
#'   [with_item_model()] extension adds overall `viable_30d` from damage,
#'   `viable_24h`, cause-specific loss and servicing-access endpoints. Its risk
#'   continues after arrival; it does not establish application-specific validity.
#'   Viable nonarrivals at the transport horizon are
#'   endpoint failures; missing arrival times remain `NA`.
#' @export
simulate_shield <- function(parameters, policy = c("P0", "P1", "P2", "P3"),
    disruption_model = c("static", "adaptive"), n_rep = 10L, seed = 20260512L,
    disruptions_enabled = parameters$disruptions_enabled) {
  if (!inherits(parameters, "shield_parameters"))
    stop("parameters must be created by shield_parameters()", call. = FALSE)
  policy <- match.arg(policy)
  disruption_model <- match.arg(disruption_model)
  check_scalar(n_rep, "n_rep", 1, integer = TRUE)
  if (!is.logical(disruptions_enabled) || length(disruptions_enabled) != 1L || is.na(disruptions_enabled))
    stop("disruptions_enabled must be TRUE or FALSE", call. = FALSE)
  p <- parameters
  p$n_rep <- as.integer(n_rep)
  p$disruptions_enabled <- disruptions_enabled
  result <- with_shield_rng(seed, run_scenario_full(policy, disruption_model, p, seed))
  attr(result, "parameters") <- p
  attr(result, "shieldR_version") <- as.character(utils::packageVersion("shieldR"))
  attr(result, "rng_kind") <- c("L'Ecuyer-CMRG", "Inversion", "Rejection")
  class(result) <- c("shield_run", "data.frame")
  result
}

#' Summarize batch outcomes
#'
#' @param x A `shield_run` returned by [simulate_shield()].
#' @return Data frame of endpoint names, valid and missing replication counts,
#'   median, first quartile and third quartile. Arrival-time summaries are
#'   across defined batch medians, not pooled item times.
#' @export
summarize_shield <- function(x) {
  if (!inherits(x, "shield_run")) stop("x must be a shield_run", call. = FALSE)
  fields <- c("viable_stage1", "viable_stage2", "viable_stage3", "t_stage2_med", "t_stage3_med",
    paste0("util_L", 1:5), "n_dispatch", "n_disruption", "n_disruption_loss", "n_pending",
    "hold_mean", "hold_rate")
  if (!is.null(attr(x, "parameters")$item_model)) fields <- c(fields,
    "viable_30d", "viable_24h", "loss_degradation", "loss_other", "loss_second_hit",
    "stabilization_access", "never_stabilization", "loss_before_stabilization",
    "t_stabilization_med", "t_processing_med", "processing_utilization", "exposure_proxy")
  do.call(rbind, lapply(fields, function(nm) {
    v <- x[[nm]]
    good <- v[is.finite(v)]
    q <- if (length(good)) stats::quantile(good, c(.25, .5, .75), names = FALSE)
         else rep(NA_real_, 3)
    data.frame(endpoint = nm, n = length(good), missing = length(v) - length(good),
      median = q[2], q1 = q[1], q3 = q[3])
  }))
}
