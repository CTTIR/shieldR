#' Configure matched item-information and anticipation controls
#'
#' Creates a new parameter object for a separately identified experiment.
#' All controlled policies use the first noisy observation rather than giving
#' fixed policies the true initial class. Updated policies use the latest
#' scheduled observation; initial_observed policies retain the first observation.
#' Item deterioration and observation draws continue in both conditions.
#'
#' For P3, reactive planning uses current risk estimates without forecasting
#' changes from committed departures. Candidate actions, loss weights, queue
#' handling and randomization stay unchanged. known_model preserves the original
#' specified-response forecast; it is not an equilibrium solution or a model of
#' unspecified disruption mechanisms. Other policies do not use this forecast switch.
#'
#' Uncontrolled parameter objects preserve the legacy P0--P3 behavior exactly.
#' These controls do not add servicing effects, post-arrival loss or a
#' real-time item system. Existing transport reservations remain committed.
#'
#' @param parameters A [shield_parameters()] object.
#' @param item Either updated or initial_observed.
#' @param anticipation Either known_model or reactive.
#' @return A new shield_parameters object with recorded decision_controls.
#' @export
#' @examples
#' set.seed(20260512)
#' p <- with_decision_controls(example_scenario(N = 2L),
#'   item = "initial_observed", anticipation = "reactive")
#' simulate_shield(p, policy = "P3", n_rep = 1L)
with_decision_controls <- function(parameters,
    item = c("updated", "initial_observed"),
    anticipation = c("known_model", "reactive")) {
  if (!inherits(parameters, "shield_parameters"))
    stop("parameters must be created by shield_parameters()", call. = FALSE)
  item <- match.arg(item)
  anticipation <- match.arg(anticipation)
  parameters$decision_controls <- list(schema = 1L, item = item,
    anticipation = anticipation, initial_information = "observed")
  parameters
}

prepare_decision_state_full <- function(state, params) {
  control <- params$decision_controls
  if (is.null(control)) return(state)
  if (!identical(control$schema, 1L) ||
      !control$item %in% c("updated", "initial_observed") ||
      !control$anticipation %in% c("known_model", "reactive"))
    stop("invalid decision controls; use with_decision_controls()", call. = FALSE)
  if (control$item == "initial_observed") {
    if (is.null(state$obs_initial) || length(state$obs_initial) != params$N)
      stop("first observed classes must be recorded before controlled decisions",
           call. = FALSE)
    state$obs <- state$obs_initial
  }
  state
}
