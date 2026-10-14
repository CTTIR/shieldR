#' Construct an explicit five-leg scenario
#'
#' Neutral defaults switch off physiological and disruption hazards. They are
#' teaching settings, not the parameters of the SHIELD manuscript.
#' The topology is fixed: Source, Collection, parallel Transfer nodes, parallel Stage 1 nodes,
#' Stage 2 and Stage 3. Every leg has routes A, B and C. Time is in whole minutes.
#' A live arrival at Stage 2 ends physiological hazard. Stage 3 is terminal.
#'
#' @param graph Data frame with exactly the columns `leg`, `from`, `to`,
#'   `route`, `travel_min`, `exposure_weight`, `vehicle_fraction`. One row per
#'   leg L1--L5 and route A--C. See [example_scenario()] for the schema.
#' @param N Positive integer cohort size.
#' @param horizon Positive integer administrative horizon in minutes.
#' @param slot Positive integer dispatch interval in minutes.
#' @param departure_holds Increasing nonnegative whole-minute holds, starting
#'   at zero; each must be a multiple of `slot`.
#' @param h Three finite nonnegative instantaneous mortality rates for classes
#'   1, 2 and 3, before multiplication by `hazard_band`.
#' @param esc32,esc21 Nonnegative class escalation rates before scaling.
#' @param mix Three integer class counts summing to `N`; `NULL` uses a
#'   rounded 1:2:2 mix with the residual allocated to class 3.
#' @param reassess Positive integer observation interval in minutes.
#' @param misclass Probability of an adjacent-class observation error.
#' @param t_load,t_unload Positive integer loading and unloading durations.
#' @param disruption_risk Named vector of probabilities with names `low` and `high`.
#' @param disruption_risk_level Selected named disruption_risk level.
#' @param p_cap Maximum disruption probability, in (0, 1].
#' @param q Conditional disruption fatality probability and security-loss weight.
#' @param att_delay Nonnegative integer nonfatal disruption delay in minutes.
#' @param lambda Nonnegative finite response parameter.
#' @param atk_window Positive integer nominal departure-history window.
#' @param beta Nonnegative finite policy softmax parameter.
#' @param beta_prior Two strictly positive Beta prior shapes.
#' @param eps_time Positive route-time tie-break weight for P2.
#' @param disruptions_enabled Logical; whether to simulate disruption outcomes.
#' @param hazard_band Positive multiplier of mortality and escalation rates.
#' @return A `shield_parameters` list. Treat derived fields as read-only and
#'   reconstruct through this function when changing a scenario.
#' @export
shield_parameters <- function(graph, N = 20L, horizon = 60L, slot = 1L,
    departure_holds = 0:2, h = c(0, 0, 0), esc32 = 0, esc21 = 0,
    mix = NULL, reassess = 1L, misclass = 0, t_load = 1L, t_unload = 1L,
    disruption_risk = c(low = 0, high = 0), disruption_risk_level = c("low", "high"),
    p_cap = 1, q = 0, att_delay = 0L, lambda = 0, atk_window = 1L,
    beta = 1, beta_prior = c(1, 1), eps_time = 1e-4,
    disruptions_enabled = FALSE, hazard_band = 1) {
  disruption_risk_level <- match.arg(disruption_risk_level)
  for (name in c("N", "horizon", "slot", "reassess", "t_load", "t_unload",
                 "atk_window")) check_scalar(get(name), name, 1, integer = TRUE)
  check_scalar(att_delay, "att_delay", 0, integer = TRUE)
  for (name in c("lambda", "beta", "esc32", "esc21"))
    check_scalar(get(name), name, 0)
  for (name in c("hazard_band", "eps_time")) {
    check_scalar(get(name), name, 0)
    if (get(name) == 0) stop(name, " must be positive", call. = FALSE)
  }
  check_probability(misclass, "misclass", 1L)
  check_probability(q, "q", 1L)
  check_probability(p_cap, "p_cap", 1L)
  if (p_cap == 0) stop("p_cap must be positive", call. = FALSE)
  check_probability(disruption_risk, "disruption_risk", 2L)
  if (!identical(names(disruption_risk), c("low", "high")))
    stop("disruption_risk must have names low and high, in that order", call. = FALSE)
  if (!is.logical(disruptions_enabled) || length(disruptions_enabled) != 1L ||
      is.na(disruptions_enabled)) stop("disruptions_enabled must be TRUE or FALSE", call. = FALSE)
  if (!is.numeric(h) || length(h) != 3L || any(!is.finite(h) | h < 0))
    stop("h must contain three finite nonnegative rates", call. = FALSE)
  if (any(!is.finite(c(h, esc32, esc21) * hazard_band)))
    stop("scaled hazards must be finite", call. = FALSE)
  if (!is.numeric(beta_prior) || length(beta_prior) != 2L ||
      any(!is.finite(beta_prior) | beta_prior <= 0))
    stop("beta_prior must contain two finite positive shapes", call. = FALSE)
  if (!is.numeric(departure_holds) || !length(departure_holds) ||
      any(!is.finite(departure_holds)) || departure_holds[1L] != 0 ||
      any(departure_holds < 0 | departure_holds > .Machine$integer.max |
          departure_holds != floor(departure_holds) | departure_holds %% slot != 0) ||
      any(diff(departure_holds) <= 0))
    stop("departure_holds must start at zero and increase in multiples of slot", call. = FALSE)
  N <- as.integer(N)
  if (is.null(mix)) {
    mix <- round(N * c(1, 2, 2) / 5)
    mix[3L] <- N - mix[1L] - mix[2L]
  }
  if (!is.numeric(mix) || length(mix) != 3L ||
      any(!is.finite(mix) | mix < 0 | mix != floor(mix)) || sum(mix) != N)
    stop("mix must contain three nonnegative integer counts summing to N", call. = FALSE)
  graph <- validate_graph(graph)
  p <- list(seed0 = 20260512L, N = N, horizon = as.integer(horizon),
    slot = as.integer(slot), departure_holds = as.integer(departure_holds),
    mix = as.integer(mix), h = hazard_band * h, esc32 = hazard_band * esc32,
    esc21 = hazard_band * esc21, hazard_band = hazard_band,
    reassess = as.integer(reassess), misclass = misclass,
    t_load = as.integer(t_load), t_unload = as.integer(t_unload),
    R = 3L, routes = c("A", "B", "C"), disruption_risk_level = disruption_risk_level,
    disruption_risk = disruption_risk, p_cap = p_cap, q = q, att_delay = as.integer(att_delay),
    lambda = lambda, atk_window = as.integer(atk_window), beta = beta,
    beta_prior = beta_prior, eps_time = eps_time, n_rep = 1L,
    disruptions_enabled = disruptions_enabled, graph = graph)
  p$leg_time <- matrix(graph$travel_min, nrow = 5L, byrow = TRUE,
                       dimnames = list(paste0("L", 1:5), p$routes))
  p$leg_w <- matrix(graph$exposure_weight, nrow = 5L, byrow = TRUE,
                    dimnames = list(paste0("L", 1:5), p$routes))
  p$n_leg <- 5L
  p$leg_origins <- list(L1 = 1L, L2 = 2L, L3 = 3:4, L4 = 5:6, L5 = 7L)
  p$leg_dests <- list(L1 = 2L, L2 = 3:4, L3 = 5:6, L4 = 7L, L5 = 8L)
  p$node_leg <- c(1L, 2L, 3L, 3L, 4L, 4L, 5L, 0L)
  vf <- vapply(split(graph$vehicle_fraction, graph$leg_i), unique, numeric(1))
  if (any(ceiling(N * vf) > .Machine$integer.max))
    stop("requested fleet exceeds integer capacity", call. = FALSE)
  p$V <- pmax(2L, as.integer(ceiling(N * vf)))
  p$pools <- make_leg_pools(p$V, p$leg_origins)
  node_capacity <- numeric(length(NODE))
  for (pool in p$pools) for (origin in pool$origins)
    node_capacity[origin] <- node_capacity[origin] + pool$vehicles / length(pool$origins)
  cyc <- 2 * rowMeans(p$leg_time) + p$t_load + p$t_unload
  p$kappa_node <- numeric(length(NODE))
  for (node in seq_along(NODE)) {
    leg <- p$node_leg[node]
    if (leg > 0L && node_capacity[node] > 0)
      p$kappa_node[node] <- cyc[leg] / node_capacity[node]
  }
  p$node_capacity <- node_capacity
  p$hazard_stop_node <- NODE[["Stage2"]]
  p$terminal_node <- NODE[["Stage3"]]
  class(p) <- c("shield_parameters", "list")
  p
}

validate_graph <- function(graph) {
  required <- c("leg", "from", "to", "route", "travel_min",
                "exposure_weight", "vehicle_fraction")
  if (!is.data.frame(graph) || !identical(names(graph), required) || nrow(graph) != 15L)
    stop("graph must be a data frame with the documented columns and 15 rows", call. = FALSE)
  if (anyNA(graph)) stop("graph cannot contain missing values", call. = FALSE)
  for (nm in c("travel_min", "exposure_weight", "vehicle_fraction"))
    if (!is.numeric(graph[[nm]]) || any(!is.finite(graph[[nm]])))
      stop(nm, " must be finite numeric data", call. = FALSE)
  if (any(graph$travel_min <= 0 | graph$travel_min != floor(graph$travel_min) |
          graph$travel_min > .Machine$integer.max) ||
      any(graph$exposure_weight < 0 | graph$exposure_weight > 1) ||
      any(graph$vehicle_fraction <= 0))
    stop("graph numeric values are outside the documented domains", call. = FALSE)
  graph$leg_i <- match(graph$leg, paste0("L", 1:5))
  graph$route_i <- match(graph$route, c("A", "B", "C"))
  keys <- paste(graph$leg_i, graph$route_i, sep = ":")
  expected <- as.vector(outer(1:5, 1:3, paste, sep = ":"))
  if (anyNA(graph[c("leg_i", "route_i")]) || anyDuplicated(keys) || !setequal(keys, expected))
    stop("graph must contain every leg-route combination exactly once", call. = FALSE)
  origins <- c("Source", "Collection", "Transfer-A/B", "Stage1-a/b", "Stage2")
  destinations <- c("Collection", "Transfer-A/B", "Stage1-a/b", "Stage2", "Stage3")
  for (leg in 1:5) {
    d <- graph[graph$leg_i == leg, ]
    if (any(d$from != origins[leg]) || any(d$to != destinations[leg]) ||
        length(unique(d$vehicle_fraction)) != 1L)
      stop("inconsistent fixed topology or vehicle fraction for L", leg, call. = FALSE)
  }
  graph[order(graph$leg_i, graph$route_i), ]
}

check_scalar <- function(x, name, minimum, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < minimum ||
      (integer && (x != floor(x) || x > .Machine$integer.max)))
    stop(name, " must be a finite ", if (integer) "integer " else "number ",
         ">= ", minimum, call. = FALSE)
  invisible(x)
}

check_probability <- function(x, name, n) {
  if (!is.numeric(x) || length(x) != n || any(!is.finite(x) | x < 0 | x > 1))
    stop(name, " must contain ", n, " probabilities in [0, 1]", call. = FALSE)
}

#' Neutral teaching scenarios
#'
#' Both examples use equal abstract travel durations and zero exposure and
#' physiological hazards. They do not contain the controlled study inputs.
#' `constrained` reduces vehicle availability and shortens the horizon to
#' illustrate congestion and administrative censoring.
#' @param kind Either `balanced` or `constrained`.
#' @param N Positive integer cohort size.
#' @return A [shield_parameters()] object.
#' @export
#' @examples
#' set.seed(20260512)
#' scenario <- example_scenario(N = 4L)
#' result <- simulate_shield(scenario, n_rep = 2L)
#' summarize_shield(result)
example_scenario <- function(kind = c("balanced", "constrained"), N = 6L) {
  kind <- match.arg(kind)
  graph <- data.frame(leg = rep(paste0("L", 1:5), each = 3),
    from = rep(c("Source", "Collection", "Transfer-A/B", "Stage1-a/b", "Stage2"), each = 3),
    to = rep(c("Collection", "Transfer-A/B", "Stage1-a/b", "Stage2", "Stage3"), each = 3),
    route = rep(c("A", "B", "C"), 5), travel_min = rep(1, 15),
    exposure_weight = rep(0, 15),
    vehicle_fraction = rep(if (kind == "balanced") 1 else 0.05, 15))
  shield_parameters(graph, N = N, horizon = if (kind == "balanced") 60L else 12L)
}
