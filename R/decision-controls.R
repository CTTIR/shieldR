#' Configure matched clinical-information and anticipation controls
#'
#' Creates a new parameter object for a separately identified experiment.
#' All controlled policies use the first noisy observation rather than giving
#' fixed policies the true initial class. Updated policies use the latest
#' scheduled observation; initial_observed policies retain the first observation.
#' Patient deterioration and observation draws continue in both conditions.
#'
#' For P3, reactive planning uses current risk estimates without forecasting
#' changes from committed departures. Candidate actions, loss weights, queue
#' handling and randomization stay unchanged. known_model preserves the original
#' specified-response forecast; it is not an equilibrium solution or a model of
#' unknown adversary intent. Other policies do not use this forecast switch.
#'
#' Uncontrolled parameter objects preserve the legacy P0--P3 behavior exactly.
#' These controls do not add treatment effects, post-arrival mortality or a
#' real-time clinical system. Existing transport reservations remain committed.
#'
#' @param parameters A [shield_parameters()] object.
#' @param clinical Either updated or initial_observed.
#' @param anticipation Either known_model or reactive.
#' @return A new shield_parameters object with recorded decision_controls.
#' @export
#' @examples
#' set.seed(20260512)
#' p <- with_decision_controls(example_scenario(N = 2L),
#'   clinical = "initial_observed", anticipation = "reactive")
#' simulate_shield(p, policy = "P3", n_rep = 1L)
with_decision_controls <- function(parameters,
    clinical = c("updated", "initial_observed"),
    anticipation = c("known_model", "reactive")) {
  if (!inherits(parameters, "shield_parameters"))
    stop("parameters must be created by shield_parameters()", call. = FALSE)
  clinical <- match.arg(clinical)
  anticipation <- match.arg(anticipation)
  parameters$decision_controls <- list(schema = 1L, clinical = clinical,
    anticipation = anticipation, initial_information = "observed")
  parameters
}

prepare_decision_state_full <- function(state, params) {
  control <- params$decision_controls
  if (is.null(control)) return(state)
  if (!identical(control$schema, 1L) ||
      !control$clinical %in% c("updated", "initial_observed") ||
      !control$anticipation %in% c("known_model", "reactive"))
    stop("invalid decision controls; use with_decision_controls()", call. = FALSE)
  if (control$clinical == "initial_observed") {
    if (is.null(state$obs_initial) || length(state$obs_initial) != params$N)
      stop("first observed classes must be recorded before controlled decisions",
           call. = FALSE)
    state$obs <- state$obs_initial
  }
  state
}
