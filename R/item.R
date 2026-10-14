# Five-stage network simulation model.

# Initialise the batch at the source node
# @param params full_params() list
# @return item state list
init_items_full <- function(params) {
  cls <- as.integer(rep(1:3, times = params$mix))
  n <- params$N
  list(
    cls    = cls,
    cls0   = cls,
    obs    = cls,                      # first observation is exact at t = 0
    viable  = rep(TRUE, n),
    node   = rep(1L, n),               # everybody starts at the Source
    status = rep(0L, n),               # 0 wait, 1 in transport, 2 done, 3 failed
    t_eta  = rep(NA_real_, n),
    t_arrival = rep(NA_real_, n),      # physical arrival, before unloading
    dest   = rep(NA_integer_, n),      # node the current transport delivers to
    ## first arrival time at each stage (NA = never reached)
    t_stage1 = rep(NA_real_, n),
    t_stage2 = rep(NA_real_, n),
    t_stage3 = rep(NA_real_, n)
  )
}

# Instantaneous viability-loss rate U_i(t)
# @param state item state
# @param t current minute (unused; hazards are time-homogeneous per class)
# @param params parameter list
# @param observed use the controller-observed class (TRUE) or the truth
# @return numeric vector >= 0
urgency_index_full <- function(state, t, params, observed = TRUE) {
  cl <- if (observed) state$obs else state$cls
  u <- params$h[cl]
  u[!state$viable] <- 0
  u[state$status >= 2L] <- 0
  u[!is.na(state$t_stage2)] <- 0         # hazard stops on physical Stage-2 arrival
  unname(u)
}

# Advance every item by one minute
#
# Hazard and escalation apply to live items who are waiting or in
# transport and have not yet reached Stage 2. Loss is drawn first, then
# escalation among that minute's viable_items.
# @param state item state
# @param dt must be 1
# @param params parameter list
# @return updated state
update_item_state_full <- function(state, dt = 1, params,
                                      u_loss = NULL, u_escalate = NULL) {
  if (dt != 1) stop("item module is specified for dt = 1 minute")
  at_risk <- state$viable & state$status <= 1L & is.na(state$t_stage2)
  n_act <- sum(at_risk)
  if (n_act == 0L) return(state)

  idx <- which(at_risk)
  if (is.null(u_loss)) u_loss <- stats::runif(params$N)
  if (length(u_loss) != params$N) stop("u_loss must have length N")
  p_loss <- -expm1(-params$h[state$cls[idx]] * dt)
  failed <- idx[u_loss[idx] < p_loss]
  if (length(failed)) {
    state$viable[failed]  <- FALSE
    state$status[failed] <- 3L
    state$t_eta[failed]  <- NA_real_
    state$t_arrival[failed] <- NA_real_
    state$dest[failed]   <- NA_integer_
  }
  act2 <- state$viable & state$status <= 1L & is.na(state$t_stage2)
  n2 <- sum(act2)
  if (n2) {
    idx2 <- which(act2)
    c2 <- state$cls[idx2]
    if (is.null(u_escalate)) u_escalate <- stats::runif(params$N)
    if (length(u_escalate) != params$N)
      stop("u_escalate must have length N")
    u2 <- u_escalate[idx2]
    p32 <- -expm1(-params$esc32 * dt)
    p21 <- -expm1(-params$esc21 * dt)
    hit <- (c2 == 3L & u2 < p32) | (c2 == 2L & u2 < p21)
    if (any(hit)) state$cls[idx2[hit]] <- c2[hit] - 1L
  }
  state
}

# Controller reassessment with misclassification
#
# Observation model: with probability `params$misclass` the observed class
# is a neighbouring class. T1 and T3 can only be misread inwards (towards
# T2); T2 splits evenly between T1 and T3.
# @param state item state
# @param params parameter list
# @return updated state
reassess_items_full <- function(state, params, u_error = NULL,
                                   u_shift = NULL) {
  keep <- state$viable & state$status <= 1L & is.na(state$t_stage2)
  n <- sum(keep)
  if (!n) return(state)
  idx <- which(keep)
  truth <- state$cls[idx]
  obs <- truth
  if (is.null(u_error)) u_error <- stats::runif(params$N)
  if (is.null(u_shift)) u_shift <- stats::runif(params$N)
  if (length(u_error) != params$N || length(u_shift) != params$N)
    stop("reassessment uniforms must have length N")
  err <- u_error[idx] < params$misclass
  if (any(err)) {
    e <- which(err)
    tt <- truth[e]
    shifted <- ifelse(tt == 1L, 2L,
               ifelse(tt == 3L, 2L,
                      ifelse(u_shift[idx[e]] < 0.5, 1L, 3L)))
    obs[e] <- shifted
  }
  state$obs[idx] <- obs
  state
}
