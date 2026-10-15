# Five-stage network simulation model.

# Initialise the per-leg route beliefs
# @param params full_params() list
# @return list(a, b) of 5 x 3 matrices
init_theta_full <- function(params) {
  list(a = matrix(params$beta_prior[1], params$n_leg, params$R),
       b = matrix(params$beta_prior[2], params$n_leg, params$R))
}

# Posterior mean disruption probability per (leg, route)
# @param theta belief list
# @return 5 x 3 matrix
theta_hat_full <- function(theta) theta$a / (theta$a + theta$b)

# Conjugate update after one observed traversal
# @param theta belief list
# @param leg,route integer indices
# @param disrupted logical
# @return updated belief list
update_theta_full <- function(theta, leg, route, disrupted) {
  if (disrupted) theta$a[leg, route] <- theta$a[leg, route] + 1
  else          theta$b[leg, route] <- theta$b[leg, route] + 1
  theta
}

# Disruption model allocation over the routes of one leg
#
# @param hist data.frame with time and route columns, or a legacy integer
#   vector of route indices, oldest first
# @param params parameter list
# @param mode "static" or "adaptive"
# @return numeric vector of length R summing to 1
disruption_model_response_full <- function(hist, params, mode = c("adaptive", "static"),
                                   log_scale = FALSE) {
  mode <- match.arg(mode)
  R <- params$R
  uniform <- if (log_scale) rep(-log(R), R) else rep(1 / R, R)
  if (mode == "static") return(uniform)
  w <- params$atk_window
  if (is.data.frame(hist)) {
    routes <- hist$route
    times <- hist$time
  } else {
    routes <- as.integer(hist)
    times <- seq_along(routes)
  }
  n <- length(routes)
  if (n < w) return(uniform)
  first <- n - w + 1L
  ## Departures at the same minute are simultaneous. If the trailing-window
  ## boundary cuts such a batch, include the entire tied batch so resource
  ## enumeration cannot change the disruption_model's allocation.
  while (first > 1L && times[first - 1L] == times[first]) first <- first - 1L
  recent <- routes[first:n]
  f <- tabulate(recent, nbins = R) / length(recent)
  logits <- params$lambda * f
  if (log_scale) {
    shifted <- logits - max(logits)
    return(shifted - log(sum(exp(shifted))))
  }
  e <- if (max(logits) < 700) exp(logits) else exp(logits - max(logits))
  e / sum(e)
}

# Disruption probability of one outbound traversal
# @param leg,route integer indices
# @param alloc disruption_model allocation on that leg
# @param base_disruption_risk scalar
# @param params parameter list
# @return probability in [0, p_cap]
disruption_prob_full <- function(leg, route, alloc, base_disruption_risk, params) {
  min(params$p_cap,
      base_disruption_risk * params$leg_w[leg, route] * params$R * alloc[route])
}
