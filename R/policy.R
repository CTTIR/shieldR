# Five-stage network simulation model.

select_patient_full <- function(state, nodes,
                                basis = c("initial", "observed")) {
  basis <- match.arg(basis)
  waiting <- which(state$alive & state$status == 0L & state$node %in% nodes)
  if (!length(waiting)) return(NA_integer_)
  priority <- if (basis == "initial") state$cls0[waiting] else state$obs[waiting]
  waiting[order(priority, waiting)][1L]
}

destination_load <- function(demand, dests, params) {
  cap <- params$node_capacity[dests]
  cap[cap <= 0] <- Inf
  demand[dests] / cap
}

decide_transport_full <- function(policy = c("P0", "P1", "P2", "P3"),
                                  state, theta, leg, origins, params,
                                  demand, time = 0L, disruption_model = "static",
                                  history = NULL, scheduled = NULL,
                                  decision_u = NULL) {
  policy <- match.arg(policy)
  control <- params$decision_controls
  state <- prepare_decision_state_full(state, params)
  basis <- if (!is.null(control)) "observed" else
    if (policy %in% c("P0", "P2")) "initial" else "observed"
  patient <- select_patient_full(state, origins, basis)
  if (is.na(patient)) return(NULL)

  dests <- params$leg_dests[[leg]]
  disruption_risk <- theta_hat_full(theta)[leg, ]
  travel <- params$leg_time[leg, ]

  if (policy == "P3") {
    urgency <- urgency_index_full(state, 0, params, observed = TRUE)[patient]
    queue_wait <- params$kappa_node[dests] * demand[dests]
    choices <- expand.grid(route = seq_len(params$R), dest_i = seq_along(dests),
                           KEEP.OUT.ATTRS = FALSE)
    if (is.null(history))
      history <- data.frame(time = integer(0), route = integer(0))
    if (is.null(scheduled))
      scheduled <- data.frame(time = numeric(0), route = integer(0))
    reactive <- !is.null(control) && control$anticipation == "reactive"
    current_alloc <- if (!reactive) disruption_model_response_full(history, params, disruption_model)
    loss_by_hold <- vapply(params$departure_holds, function(hold) {
      if (reactive) {
        projected_disruption_risk <- pmin(params$p_cap, disruption_risk)
      } else {
      depart_at <- time + params$t_load + hold
      prior <- scheduled[scheduled$time < depart_at, c("time", "route"),
                         drop = FALSE]
      projected_history <- if (nrow(prior))
        rbind(history, prior[order(prior$time), , drop = FALSE]) else history
      projected_alloc <- disruption_model_response_full(projected_history, params,
                                                 disruption_model)
      # Preserve the reference arithmetic in its original range. Extremely
      # concentrated allocations need log ratios because both linear terms
      # can round to zero even when their ratio is meaningful.
      if (any(current_alloc == 0)) {
        current_log <- disruption_model_response_full(history, params, disruption_model, log_scale = TRUE)
        projected_log <- disruption_model_response_full(projected_history, params, disruption_model,
                                                log_scale = TRUE)
        projected_disruption_risk <- exp(pmin(log(params$p_cap),
          log(disruption_risk) + projected_log - current_log))
      } else {
        projected_disruption_risk <- pmin(params$p_cap,
          disruption_risk * projected_alloc / current_alloc)
      }
      }
      urgency * (hold + travel[choices$route] +
                 queue_wait[choices$dest_i]) +
        params$q * projected_disruption_risk[choices$route]
    }, numeric(nrow(choices)))
    if (is.null(dim(loss_by_hold)))
      loss_by_hold <- matrix(loss_by_hold, nrow = nrow(choices))
    best_hold <- max.col(-loss_by_hold, ties.method = "first")
    loss <- loss_by_hold[cbind(seq_len(nrow(choices)), best_hold)]
    log_weight <- -params$beta * loss
    probability <- exp(log_weight - max(log_weight))
    probability <- probability / sum(probability)
    if (is.null(decision_u)) decision_u <- stats::runif(1)
    selected <- which(decision_u <= cumsum(probability))[1L]
    return(list(patient = patient,
                route = as.integer(choices$route[selected]),
                dest = as.integer(dests[choices$dest_i[selected]]),
                hold = as.integer(params$departure_holds[best_hold[selected]])))
  }

  dest <- dests[which.min(destination_load(demand, dests, params))]
  route <- switch(policy,
    P0 = 1L,
    P1 = 1L,
    P2 = as.integer(which.min(params$q * disruption_risk +
                              params$eps_time * travel)))
  list(patient = patient, route = route, dest = as.integer(dest), hold = 0L)
}
